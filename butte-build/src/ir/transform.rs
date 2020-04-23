use crate::{ast::types as ast, ir::types as ir};
use anyhow::{anyhow, Result};
use heck::SnakeCase;
use itertools::Itertools;
use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque},
    convert::{TryFrom, TryInto},
};

#[derive(Default, Clone, Debug)]
pub struct Builder<'a> {
    current_namespace: Option<ast::Namespace<'a>>,
    types: HashMap<ir::QualifiedIdent<'a>, CustomTypeStatus<'a>>,
    nodes: BTreeMap<usize, Vec<ir::Node<'a>>>,
    root_types: HashSet<ir::QualifiedIdent<'a>>,
}

#[derive(Clone, Debug)]
struct IndexedElement<'a>(usize, ast::Element<'a>);

#[derive(Clone, Debug, PartialEq)]
enum CustomTypeStatus<'a> {
    TableDeclared,
    Defined(ir::CustomType<'a>),
}

// Utility struct to help build and stack namespaces
#[derive(Clone, Debug, PartialEq)]
struct NamespaceBuf<'a> {
    ident: ir::QualifiedIdent<'a>,
    namespaces: BTreeMap<ir::Ident<'a>, NamespaceBuf<'a>>,
    nodes: Vec<ir::Node<'a>>,
}

impl<'a> NamespaceBuf<'a> {
    fn new(ident: ir::QualifiedIdent<'a>) -> Self {
        Self {
            ident,
            namespaces: BTreeMap::new(),
            nodes: Vec::new(),
        }
    }

    pub fn depth(&self) -> usize {
        self.ident.parts.len()
    }

    fn into_namespace(self) -> ir::Namespace<'a> {
        let ident = self.ident;
        let nodes = self
            .namespaces
            .into_iter()
            .map(|(_, v)| ir::Node::Namespace(v.into_namespace()))
            .chain(self.nodes)
            .collect();
        ir::Namespace { ident, nodes }
    }

    fn into_node(self) -> ir::Node<'a> {
        ir::Node::Namespace(self.into_namespace())
    }
}

impl<'a> Builder<'a> {
    fn new_element(
        &mut self,
        &IndexedElement(pos, ref element): &IndexedElement<'a>,
    ) -> Result<bool> {
        match element {
            ast::Element::Namespace(n) => self.new_namespace(n),
            ast::Element::Table(t) => self.new_table(t, pos),
            ast::Element::Struct(s) => self.new_struct(s, pos),
            ast::Element::Enum(e) => self.new_enum(e, pos),
            ast::Element::Union(u) => self.new_union(u, pos),
            ast::Element::Rpc(r) => self.new_rpc(r, pos),
            ast::Element::Root(r) => self.new_root(r, pos),
            ast::Element::FileExtension(..) => unimplemented!(),
            ast::Element::FileIdentifier(..) => unimplemented!(),
            ast::Element::Attribute(..) => unimplemented!(),
            ast::Element::Object(..) => unimplemented!(),
        }
    }

    fn new_namespace(&mut self, ns: &ast::Namespace<'a>) -> Result<bool> {
        self.current_namespace = Some(ns.clone());
        Ok(true)
    }

    fn new_table(&mut self, t: &ast::Table<'a>, pos: usize) -> Result<bool> {
        // Has this type been referenced before?
        // Let's find out and update the status.
        let ident = &self.make_fully_qualified_ident(ir::Ident::from(t.id));

        if let Some(CustomTypeStatus::Defined(..)) = self.types.get(ident) {
            return Err(anyhow!("type already defined: {}", ident));
        };

        // We consider the table declared even if all its fields haven't been
        // properly resolved, because tables may be recursive.
        self.types
            .insert(ident.clone(), CustomTypeStatus::TableDeclared);

        let mut fields = Vec::new();

        // Make sure all references to custom types are resolved
        for f in &t.fields {
            let ty = self.try_type(&f.ty);
            if let Some(ty) = ty {
                let required = match &f.metadata {
                    Some(m) => match m.values.get(&ast::Ident::from("required")) {
                        Some(None) => true,
                        _ => false,
                    },
                    None => false,
                };

                // If it's a union, push the enum type field first
                if let Some(enum_ty_ident) = ty.make_union_enum_type_companion() {
                    let ty = self
                        .find_custom_type(enum_ty_ident.clone())
                        .expect("Union enum type companion should be defined now");
                    let ident = ir::Ident::from(format!("{}_type", &f.id.raw.to_snake_case()));
                    let default_value = None;
                    let metadata = ir::FieldMetadata::builder().required(required).build();
                    let doc = f.doc.clone();
                    fields.push(ir::Field {
                        ident,
                        ty,
                        default_value,
                        metadata,
                        doc,
                    });
                }
                let ident = ir::Ident::from(&f.id);
                // Use `Some` explicit or implicit default value or `None`
                let default_value = f.default_value.clone().or_else(|| (&f.ty).try_into().ok());
                let metadata = ir::FieldMetadata::builder().required(required).build();
                let doc = f.doc.clone();

                fields.push(ir::Field {
                    ident,
                    ty,
                    default_value,
                    metadata,
                    doc,
                });
            } else {
                // not satisfied yet, so we can't build the fields
                // we will try after having seen all other types.
                return Ok(false);
            }
        }

        let ident = ident.clone();
        let root_type = false; // Set as non-root by default
        let doc = t.doc.clone();

        self.types.insert(
            ident.clone(),
            CustomTypeStatus::Defined(ir::CustomType::Table),
        );

        self.nodes.insert(
            pos,
            vec![ir::Node::Table(ir::Table {
                ident,
                fields,
                root_type,
                doc,
            })],
        );

        Ok(true)
    }

    fn new_struct(&mut self, t: &ast::Struct<'a>, pos: usize) -> Result<bool> {
        let ident = &self.make_fully_qualified_ident(ir::Ident::from(t.id));

        if let Some(CustomTypeStatus::Defined(..)) = self.types.get(ident) {
            return Err(anyhow!("type already defined: {}", ident));
        };

        // Structs go straight to `defined` state; they can't be referred to
        // otherise, because they may be invalid.

        let mut fields = Vec::new();

        // Make sure all references to custom types are resolved
        for f in &t.fields {
            let ty = self.try_type(&f.ty);
            if let Some(ty) = ty {
                if !ty.is_scalar() {
                    return Err(anyhow!(
                        "struct contains illegal member: {}\nStructs can only contain scalar types, structs and fixed-size arrays", ident)
                    );
                }

                let ident = ir::Ident::from(&f.id);
                let default_value = f.default_value.clone();
                let metadata = ir::FieldMetadata::default();
                let doc = f.doc.clone();

                fields.push(ir::Field {
                    ident,
                    ty,
                    default_value,
                    metadata,
                    doc,
                });
            } else {
                // not satisfied yet, so we can't build the fields
                // we will try after having seen all other types.
                return Ok(false);
            }
        }

        let doc = t.doc.clone();

        self.types.insert(
            ident.clone(),
            CustomTypeStatus::Defined(ir::CustomType::Struct {
                fields: fields.clone(),
            }),
        );

        let s = ir::Struct {
            ident: ident.clone(),
            fields,
            doc,
        };

        self.nodes.insert(pos, vec![ir::Node::Struct(s)]);

        Ok(true)
    }

    fn new_enum(&mut self, e: &ast::Enum<'a>, pos: usize) -> Result<bool> {
        let ident = &self.make_fully_qualified_ident(ir::Ident::from(e.id));

        if let Some(CustomTypeStatus::Defined(..)) = self.types.get(ident) {
            return Err(anyhow!("type already defined: {}", ident));
        };

        let base_type = ir::EnumBaseType::try_from(&e.base_type)
            .map_err(|_| anyhow!("enum has bad base type: {}", ident))?;
        let variants: Vec<_> = e
            .variants
            .iter()
            .map(|v| ir::EnumVariant {
                ident: ir::Ident::from(v.id),
                value: v.value,
                doc: v.doc.clone(),
            })
            .collect();

        let doc = e.doc.clone();

        self.types.insert(
            ident.clone(),
            CustomTypeStatus::Defined(ir::CustomType::Enum {
                variants: variants.clone(),
                base_type,
            }),
        );

        let s = ir::Enum {
            ident: ident.clone(),
            base_type,
            variants,
            doc,
        };

        self.nodes.insert(pos, vec![ir::Node::Enum(s)]);

        Ok(true)
    }

    fn new_union(&mut self, u: &ast::Union<'a>, pos: usize) -> Result<bool> {
        let ident = &self.make_fully_qualified_ident(ir::Ident::from(u.id));

        if let Some(CustomTypeStatus::Defined(..)) = self.types.get(ident) {
            return Err(anyhow!("type already defined: {}", ident));
        };

        let mut variants = Vec::new();

        // Make sure all references to custom types are resolved
        for f in &u.variants {
            let ty = self.try_type(&ast::Type::Ident(ast::QualifiedIdent::from(vec![f.id])));
            if let Some(ty) = ty {
                let id = &f.id;
                variants.push(ir::UnionVariant {
                    ty,
                    ident: ir::Ident::from(id),
                    doc: f.doc.clone(),
                });
            } else {
                // not satisfied yet, so we can't build the fields
                // we will try after having seen all other types.
                return Ok(false);
            }
        }

        let doc = u.doc.clone();

        // We generate an enum for this union which describes the different variants
        let e = self.generate_enum_for_union(u)?;

        self.types.insert(
            ident.clone(),
            CustomTypeStatus::Defined(ir::CustomType::Union {
                enum_ident: e.ident.clone(),
                variants: variants.clone(),
            }),
        );

        let s = ir::Union {
            ident: ident.clone(),
            enum_ident: e.ident.clone(),
            variants,
            doc,
        };

        self.nodes
            .insert(pos, vec![ir::Node::Enum(e), ir::Node::Union(s)]);

        Ok(true)
    }

    fn generate_enum_for_union(&mut self, u: &ast::Union<'a>) -> Result<ir::Enum<'a>> {
        let enum_ident = format!("{}Type", u.id.raw);
        let ident = &self.make_fully_qualified_ident_relative(ir::QualifiedIdent::from(vec![
            ir::Ident::from("butte_gen"),
            ir::Ident::from(enum_ident),
        ]));
        if let Some(CustomTypeStatus::Defined(..)) = self.types.get(ident) {
            return Err(anyhow!("type already defined: {}", ident));
        };

        // There's a max of 255 variants in a union in flatbuffers
        // See https://github.com/google/flatbuffers/issues/4209
        let base_type = ir::EnumBaseType::UByte;
        let variants: Vec<_> = std::iter::once(ir::EnumVariant {
            ident: ir::Ident::from("None"),
            value: Some(0.into()),
            doc: vec![].into(),
        })
        .chain(u.variants.iter().map(|v| ir::EnumVariant {
            ident: v.id.into(),
            value: None,
            doc: v.doc.clone(),
        }))
        .collect();

        let doc = u.doc.clone();

        self.types.insert(
            ident.clone(),
            CustomTypeStatus::Defined(ir::CustomType::Enum {
                variants: variants.clone(),
                base_type,
            }),
        );

        Ok(ir::Enum {
            ident: ident.clone(),
            base_type,
            variants,
            doc,
        })
    }

    fn new_root(&mut self, root: &ast::Root<'a>, _: usize) -> Result<bool> {
        let ident = self.make_fully_qualified_ident(ir::Ident::from(&root.typename));

        self.new_root_ident(&ident)
    }

    fn new_root_ident(&mut self, ident: &ir::QualifiedIdent<'a>) -> Result<bool> {
        if let Some(&CustomTypeStatus::Defined(ref def)) = self.types.get(&ident) {
            if def == &ir::CustomType::Table {
                self.root_types.insert(ident.clone());
                Ok(true)
            } else {
                Err(anyhow!("Only tables can be root types: {}", ident))
            }
        } else {
            Ok(false)
        }
    }

    fn new_rpc(&mut self, rpc: &ast::Rpc<'a>, pos: usize) -> Result<bool> {
        let ident = &self.make_fully_qualified_ident(ir::Ident::from(&rpc.id));

        let mut methods = Vec::new();

        for m in &rpc.methods {
            let ident = ir::Ident::from(&m.id);
            let snake_ident = ir::Ident::from(m.id.as_ref().to_snake_case());

            let request_type = self.make_fully_qualified_from_ast_qualified_ident(&m.request_type);
            let response_type =
                self.make_fully_qualified_from_ast_qualified_ident(&m.response_type);

            if !self.new_root_ident(&request_type)? || !self.new_root_ident(&response_type)? {
                // Request & Response types have not been defined yet, push back to queue
                return Ok(false);
            }
            let streaming = match &m.metadata {
                Some(m) => match m.values.get(&ast::Ident::from("streaming")) {
                    Some(Some(ast::Single::String("client"))) => ir::RpcStreaming::Client,
                    Some(Some(ast::Single::String("server"))) => ir::RpcStreaming::Server,
                    Some(Some(ast::Single::String("bidi"))) => ir::RpcStreaming::Bidi,
                    None => ir::RpcStreaming::None,
                    Some(Some(s)) => return Err(anyhow!("Unknown streaming type: {:?}", s)),
                    Some(None) => {
                        return Err(anyhow!(
                        "Missing streaming type: possible values are `client`, `server` and `bidi`"
                    ))
                    }
                },
                None => ir::RpcStreaming::None,
            };
            let metadata = ir::RpcMethodMetadata { streaming };
            methods.push(ir::RpcMethod {
                ident,
                snake_ident,
                request_type,
                response_type,
                metadata,
                doc: m.doc.clone(),
            });
        }

        let s = ir::Rpc {
            ident: ident.clone(),
            methods,
            doc: rpc.doc.clone(),
        };

        self.nodes.insert(pos, vec![ir::Node::Rpc(s)]);

        Ok(true)
    }

    fn current_namespace(&self) -> Option<&ast::Namespace<'a>> {
        self.current_namespace.as_ref()
    }

    fn make_fully_qualified_ident(&self, ident: ir::Ident<'a>) -> ir::QualifiedIdent<'a> {
        let mut fqi = self
            .current_namespace()
            .map(|ns| ir::QualifiedIdent::from(&ns.ident))
            .unwrap_or_default();
        fqi.parts.push(ident);
        fqi
    }

    fn make_fully_qualified_ident_relative(
        &self,
        ident: ir::QualifiedIdent<'a>,
    ) -> ir::QualifiedIdent<'a> {
        let mut fqi = self
            .current_namespace()
            .map(|ns| ir::QualifiedIdent::from(&ns.ident))
            .unwrap_or_default();
        fqi.parts.extend(ident.parts);
        fqi
    }

    fn make_fully_qualified_from_ast_qualified_ident(
        &self,
        qualified_ident: &ast::QualifiedIdent<'a>,
    ) -> ir::QualifiedIdent<'a> {
        if qualified_ident.parts.len() == 1 {
            // We need to prefix the namespace
            self.make_fully_qualified_ident(ir::Ident::from(qualified_ident.parts[0]))
        } else {
            ir::QualifiedIdent::from(qualified_ident.clone())
        }
    }

    fn try_type(&self, ty: &ast::Type<'a>) -> Option<ir::Type<'a>> {
        Some(match ty {
            ast::Type::Bool => ir::Type::Bool,
            ast::Type::Byte => ir::Type::Byte,
            ast::Type::UByte => ir::Type::UByte,
            ast::Type::Short => ir::Type::Short,
            ast::Type::UShort => ir::Type::UShort,
            ast::Type::Int => ir::Type::Int,
            ast::Type::UInt => ir::Type::UInt,
            ast::Type::Float => ir::Type::Float,
            ast::Type::Long => ir::Type::Long,
            ast::Type::ULong => ir::Type::ULong,
            ast::Type::Double => ir::Type::Double,
            ast::Type::Int8 => ir::Type::Int8,
            ast::Type::UInt8 => ir::Type::UInt8,
            ast::Type::Int16 => ir::Type::Int16,
            ast::Type::UInt16 => ir::Type::UInt16,
            ast::Type::Int32 => ir::Type::Int32,
            ast::Type::UInt32 => ir::Type::UInt32,
            ast::Type::Int64 => ir::Type::Int64,
            ast::Type::UInt64 => ir::Type::UInt64,
            ast::Type::Float32 => ir::Type::Float32,
            ast::Type::Float64 => ir::Type::Float64,
            ast::Type::String => ir::Type::String,
            ast::Type::Array(ty) => {
                if let Some(inner) = self.try_type(ty) {
                    ir::Type::Array(Box::new(inner))
                } else {
                    return None; // unsatisfied inner type
                }
            }
            ast::Type::Ident(qualified_ident) => {
                let ident = self.make_fully_qualified_from_ast_qualified_ident(&qualified_ident);

                return self.find_custom_type(ident);
            }
        })
    }

    fn find_custom_type(
        &self,
        fully_qualified_ident: ir::QualifiedIdent<'a>,
    ) -> Option<ir::Type<'a>> {
        let ident = fully_qualified_ident;
        let ty = match self.types.get(&ident) {
            Some(CustomTypeStatus::TableDeclared) => ir::CustomType::Table,
            Some(CustomTypeStatus::Defined(ty)) => ty.clone(),
            _ => return None, // Type unsatisfied
        };

        Some(ir::Type::Custom(ir::CustomTypeRef { ident, ty }))
    }

    pub fn build(schema: ast::Schema<'a>) -> Result<ir::Root<'a>> {
        let mut context = Builder::default();

        // Keep track of the definition order,
        // mostly to build IR deterministically
        let mut input: VecDeque<_> = schema
            .elements
            .into_iter()
            .enumerate()
            .map(|(i, el)| IndexedElement(i, el))
            .collect();

        // Loop until all types can be defined
        // or stop if we make no progress
        loop {
            let mut unsatisfied = VecDeque::new();
            let nodes_len = context.nodes.len();
            while let Some(element) = input.pop_front() {
                if !context.new_element(&element)? {
                    unsatisfied.push_back(element);
                }
            }
            if unsatisfied.is_empty() {
                break; // everything is satisfied
            } else {
                // Didn't we progress?
                if context.nodes.len() == nodes_len {
                    let unsatisfied_elements = unsatisfied
                        .into_iter()
                        .map(|IndexedElement(_, element)| element)
                        .collect::<Vec<_>>();
                    return Err(anyhow!("Unable to type schema: the following definitions referenced items not found: {:?}", unsatisfied_elements));
                    // TODO proper reporting
                }
            }
            input = std::mem::take(&mut unsatisfied);
        }

        let nodes = context.nodes;
        let root_types = &context.root_types;

        // mark tables that are root types
        let nodes: Vec<_> = nodes
            .into_iter()
            .map(|(_, v)| v)
            .flatten()
            .map(|mut v| {
                if let ir::Node::Table(ref mut t) = &mut v {
                    if root_types.contains(&t.ident) {
                        t.root_type = true;
                    }
                };
                v
            })
            .collect();

        let mut elements: HashMap<_, _> = nodes
            .into_iter()
            .map(|n| (n.namespace(), n))
            .into_group_map()
            .into_iter()
            .collect();

        let namespaces_idents: BTreeSet<_> = elements.keys().cloned().filter_map(|id| id).collect();

        let mut namespaces: Vec<_> = namespaces_idents
            .into_iter()
            .map(NamespaceBuf::new)
            .collect();

        // Add children to namespace
        for ns in namespaces.iter_mut() {
            if let Some(children) = elements.remove(&Some(ns.ident.clone())) {
                ns.nodes = children;
            }
        }

        // Build top-level with namespaces and elements without namespaces
        let mut nodes = BTreeMap::new();
        // stack namespaces
        for ns in namespaces.into_iter() {
            let mut parent = &mut nodes;

            // Handle intermediate levels
            for i in 1..ns.depth() {
                parent = &mut parent
                    .entry(ns.ident.parts[i - 1].clone())
                    .or_insert_with(|| {
                        NamespaceBuf::new(ir::QualifiedIdent::from(ns.ident.parts[..i].to_vec()))
                    })
                    .namespaces;
            }
            // Add leaf
            if let Some(n) = parent.get_mut(ns.ident.simple()) {
                n.nodes = ns.nodes;
            } else {
                parent.insert(ns.ident.simple().clone(), ns);
            }
        }

        let mut nodes: Vec<_> = nodes.into_iter().map(|(_, v)| v.into_node()).collect();

        // Are there elements without namespace?
        if let Some(rest) = elements.remove(&None) {
            nodes.extend(rest);
        }

        Ok(ir::Root { nodes })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::types as ir;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_table_simple() {
        let input = "\
// Not a doc
table HelloReply {
    message:string;
}
";
        let (_, schema) = crate::parser::schema_decl(input).unwrap();
        let actual = Builder::build(schema).unwrap();
        let expected = ir::Root {
            nodes: vec![ir::Node::Table(
                ir::Table::builder()
                    .ident(ir::QualifiedIdent::from("HelloReply"))
                    .fields(vec![ir::Field::builder()
                        .ident(ir::Ident::from("message"))
                        .ty(ir::Type::String)
                        .build()])
                    .build(),
            )],
        };

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_table_root() {
        let input = "\
table HelloReply {
    /// Doc
    message:string;
}

root_type HelloReply;
";
        let (_, schema) = crate::parser::schema_decl(input).unwrap();
        let actual = Builder::build(schema).unwrap();
        let expected = ir::Root {
            nodes: vec![ir::Node::Table(
                ir::Table::builder()
                    .ident(ir::QualifiedIdent::from("HelloReply"))
                    .fields(vec![ir::Field::builder()
                        .ident(ir::Ident::from("message"))
                        .ty(ir::Type::String)
                        .doc(vec![" Doc"].into())
                        .build()])
                    .root_type(true)
                    .build(),
            )],
        };

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_table_in_namespace() {
        let input = "\
namespace foo.bar;

table HelloReply {
    message:string;
}
";
        let (_, schema) = crate::parser::schema_decl(input).unwrap();
        let actual = Builder::build(schema).unwrap();
        let expected = ir::Root {
            nodes: vec![ir::Node::Namespace(
                ir::Namespace::builder()
                    .ident(ir::QualifiedIdent::parse_str("foo"))
                    .nodes(vec![ir::Node::Namespace(
                        ir::Namespace::builder()
                            .ident(ir::QualifiedIdent::parse_str("foo.bar"))
                            .nodes(vec![ir::Node::Table(
                                ir::Table::builder()
                                    .ident(ir::QualifiedIdent::parse_str("foo.bar.HelloReply"))
                                    .fields(vec![ir::Field::builder()
                                        .ident(ir::Ident::from("message"))
                                        .ty(ir::Type::String)
                                        .build()])
                                    .build(),
                            )])
                            .build(),
                    )])
                    .build(),
            )],
        };

        assert_eq!(actual, expected);
    }

    #[test]
    fn namespace_stacking() {
        let input = "\
namespace foo.bar;

table HelloReply {
    message:string;
}

namespace foo.baz;

table GoodbyeReply {
    message:string;
}
";
        let (_, schema) = crate::parser::schema_decl(input).unwrap();
        let actual = Builder::build(schema).unwrap();
        let expected = ir::Root {
            nodes: vec![ir::Node::Namespace(
                ir::Namespace::builder()
                    .ident(ir::QualifiedIdent::parse_str("foo"))
                    .nodes(vec![
                        ir::Node::Namespace(
                            ir::Namespace::builder()
                                .ident(ir::QualifiedIdent::parse_str("foo.bar"))
                                .nodes(vec![ir::Node::Table(
                                    ir::Table::builder()
                                        .ident(ir::QualifiedIdent::parse_str("foo.bar.HelloReply"))
                                        .fields(vec![ir::Field::builder()
                                            .ident(ir::Ident::from("message"))
                                            .ty(ir::Type::String)
                                            .build()])
                                        .build(),
                                )])
                                .build(),
                        ),
                        ir::Node::Namespace(
                            ir::Namespace::builder()
                                .ident(ir::QualifiedIdent::parse_str("foo.baz"))
                                .nodes(vec![ir::Node::Table(
                                    ir::Table::builder()
                                        .ident(ir::QualifiedIdent::parse_str(
                                            "foo.baz.GoodbyeReply",
                                        ))
                                        .fields(vec![ir::Field::builder()
                                            .ident(ir::Ident::from("message"))
                                            .ty(ir::Type::String)
                                            .build()])
                                        .build(),
                                )])
                                .build(),
                        ),
                    ])
                    .build(),
            )],
        };

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_table_simple_dependency() {
        let input = "\
table HelloMsg {
    val: string;
}

table HelloReply {
    message: HelloMsg;
}
";
        let (_, schema) = crate::parser::schema_decl(input).unwrap();
        let actual = Builder::build(schema).unwrap();
        let expected = ir::Root {
            nodes: vec![
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::QualifiedIdent::from("HelloMsg"))
                        .fields(vec![ir::Field::builder()
                            .ident(ir::Ident::from("val"))
                            .ty(ir::Type::String)
                            .build()])
                        .build(),
                ),
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::QualifiedIdent::from("HelloReply"))
                        .fields(vec![ir::Field::builder()
                            .ident(ir::Ident::from("message"))
                            .ty(ir::Type::Custom(
                                ir::CustomTypeRef::builder()
                                    .ident(ir::QualifiedIdent::from("HelloMsg"))
                                    .ty(ir::CustomType::Table)
                                    .build(),
                            ))
                            .build()])
                        .build(),
                ),
            ],
        };

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_table_required_field() {
        let input = "\
table HelloMsg {
    message: string (required);
}
";
        let (_, schema) = crate::parser::schema_decl(input).unwrap();
        let actual = Builder::build(schema).unwrap();
        let expected = ir::Root {
            nodes: vec![ir::Node::Table(
                ir::Table::builder()
                    .ident(ir::QualifiedIdent::from("HelloMsg"))
                    .fields(vec![ir::Field::builder()
                        .ident(ir::Ident::from("message"))
                        .ty(ir::Type::String)
                        .metadata(ir::FieldMetadata::builder().required(true).build())
                        .build()])
                    .build(),
            )],
        };

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_table_depends_on_next() {
        let input = "\
table HelloReply {
    message: HelloMsg;
}

table HelloMsg {
    val: string;
}
";
        let (_, schema) = crate::parser::schema_decl(input).unwrap();
        let actual = Builder::build(schema).unwrap();
        let expected = ir::Root {
            nodes: vec![
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::QualifiedIdent::from("HelloReply"))
                        .fields(vec![ir::Field::builder()
                            .ident(ir::Ident::from("message"))
                            .ty(ir::Type::Custom(
                                ir::CustomTypeRef::builder()
                                    .ident(ir::QualifiedIdent::from("HelloMsg"))
                                    .ty(ir::CustomType::Table)
                                    .build(),
                            ))
                            .build()])
                        .build(),
                ),
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::QualifiedIdent::from("HelloMsg"))
                        .fields(vec![ir::Field::builder()
                            .ident(ir::Ident::from("val"))
                            .ty(ir::Type::String)
                            .build()])
                        .build(),
                ),
            ],
        };

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_table_depends_on_next_with_root_type_first() {
        let input = "\
root_type HelloReply;

table HelloReply {
    message: HelloMsg;
}

table HelloMsg {
    val: string;
}
";
        let (_, schema) = crate::parser::schema_decl(input).unwrap();
        let actual = Builder::build(schema).unwrap();
        let expected = ir::Root {
            nodes: vec![
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::QualifiedIdent::from("HelloReply"))
                        .fields(vec![ir::Field::builder()
                            .ident(ir::Ident::from("message"))
                            .ty(ir::Type::Custom(
                                ir::CustomTypeRef::builder()
                                    .ident(ir::QualifiedIdent::from("HelloMsg"))
                                    .ty(ir::CustomType::Table)
                                    .build(),
                            ))
                            .build()])
                        .root_type(true)
                        .build(),
                ),
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::QualifiedIdent::from("HelloMsg"))
                        .fields(vec![ir::Field::builder()
                            .ident(ir::Ident::from("val"))
                            .ty(ir::Type::String)
                            .build()])
                        .build(),
                ),
            ],
        };

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_table_recursive() {
        let input = "\
table HelloReply {
    message: HelloReply;
}
";
        let (_, schema) = crate::parser::schema_decl(input).unwrap();
        let actual = Builder::build(schema).unwrap();
        let expected = ir::Root {
            nodes: vec![ir::Node::Table(
                ir::Table::builder()
                    .ident(ir::QualifiedIdent::from("HelloReply"))
                    .fields(vec![ir::Field::builder()
                        .ident(ir::Ident::from("message"))
                        .ty(ir::Type::Custom(
                            ir::CustomTypeRef::builder()
                                .ident(ir::QualifiedIdent::from("HelloReply"))
                                .ty(ir::CustomType::Table)
                                .build(),
                        ))
                        .build()])
                    .build(),
            )],
        };

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_struct_single() {
        let input = "\
struct Vector3 {
    x: float32;
    y: float32;
    z: float32;
} 
";
        let (_, schema) = crate::parser::schema_decl(input).unwrap();
        let actual = Builder::build(schema).unwrap();
        let expected = ir::Root {
            nodes: vec![ir::Node::Struct(
                ir::Struct::builder()
                    .ident(ir::QualifiedIdent::from("Vector3"))
                    .fields(vec![
                        ir::Field::builder()
                            .ident(ir::Ident::from("x"))
                            .ty(ir::Type::Float32)
                            .build(),
                        ir::Field::builder()
                            .ident(ir::Ident::from("y"))
                            .ty(ir::Type::Float32)
                            .build(),
                        ir::Field::builder()
                            .ident(ir::Ident::from("z"))
                            .ty(ir::Type::Float32)
                            .build(),
                    ])
                    .build(),
            )],
        };

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_struct_in_struct() {
        let input = "\
struct Vector2 {
    x: float32;
    y: float32;
}
struct Vector3 {
    xy: Vector2;
    z: float32;
} 
";
        let (_, schema) = crate::parser::schema_decl(input).unwrap();
        let actual = Builder::build(schema).unwrap();
        let expected = ir::Root {
            nodes: vec![
                ir::Node::Struct(
                    ir::Struct::builder()
                        .ident(ir::QualifiedIdent::from("Vector2"))
                        .fields(vec![
                            ir::Field::builder()
                                .ident(ir::Ident::from("x"))
                                .ty(ir::Type::Float32)
                                .build(),
                            ir::Field::builder()
                                .ident(ir::Ident::from("y"))
                                .ty(ir::Type::Float32)
                                .build(),
                        ])
                        .build(),
                ),
                ir::Node::Struct(
                    ir::Struct::builder()
                        .ident(ir::QualifiedIdent::from("Vector3"))
                        .fields(vec![
                            ir::Field::builder()
                                .ident(ir::Ident::from("xy"))
                                .ty(ir::Type::Custom(
                                    ir::CustomTypeRef::builder()
                                        .ident(ir::QualifiedIdent::from("Vector2"))
                                        .ty(ir::CustomType::Struct {
                                            fields: vec![
                                                ir::Field::builder()
                                                    .ident(ir::Ident::from("x"))
                                                    .ty(ir::Type::Float32)
                                                    .build(),
                                                ir::Field::builder()
                                                    .ident(ir::Ident::from("y"))
                                                    .ty(ir::Type::Float32)
                                                    .build(),
                                            ],
                                        })
                                        .build(),
                                ))
                                .build(),
                            ir::Field::builder()
                                .ident(ir::Ident::from("z"))
                                .ty(ir::Type::Float32)
                                .build(),
                        ])
                        .build(),
                ),
            ],
        };

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_enum_single() {
        let input = "enum MyEnum : ubyte { Foo, Bar }";
        let (_, schema) = crate::parser::schema_decl(input).unwrap();
        let actual = Builder::build(schema).unwrap();
        let expected = ir::Root {
            nodes: vec![ir::Node::Enum(
                ir::Enum::builder()
                    .ident(ir::QualifiedIdent::from("MyEnum"))
                    .base_type(ir::EnumBaseType::UByte)
                    .variants(vec![
                        ir::EnumVariant::builder()
                            .ident(ir::Ident::from("Foo"))
                            .build(),
                        ir::EnumVariant::builder()
                            .ident(ir::Ident::from("Bar"))
                            .build(),
                    ])
                    .build(),
            )],
        };

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_union_single() {
        let input = "\
        table A {
            message: string;
        }
        table B {
            message: [ubyte];
        }
        union AorB {
            A,
            B
        }
        ";
        let (_, schema) = crate::parser::schema_decl(input).unwrap();
        let actual = Builder::build(schema).unwrap();
        let expected = ir::Root {
            nodes: vec![
                ir::Node::Namespace(
                    ir::Namespace::builder()
                        .ident(ir::QualifiedIdent::parse_str("butte_gen"))
                        .nodes(vec![ir::Node::Enum(
                            ir::Enum::builder()
                                .ident(ir::QualifiedIdent::parse_str("butte_gen.AorBType"))
                                .base_type(ir::EnumBaseType::UByte)
                                .variants(vec![
                                    ir::EnumVariant::builder()
                                        .ident(ir::Ident::from("None"))
                                        .value(Some(0.into()))
                                        .build(),
                                    ir::EnumVariant::builder()
                                        .ident(ir::Ident::from("A"))
                                        .build(),
                                    ir::EnumVariant::builder()
                                        .ident(ir::Ident::from("B"))
                                        .build(),
                                ])
                                .build(),
                        )])
                        .build(),
                ),
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::QualifiedIdent::from("A"))
                        .fields(vec![ir::Field::builder()
                            .ident(ir::Ident::from("message"))
                            .ty(ir::Type::String)
                            .build()])
                        .build(),
                ),
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::QualifiedIdent::from("B"))
                        .fields(vec![ir::Field::builder()
                            .ident(ir::Ident::from("message"))
                            .ty(ir::Type::Array(Box::new(ir::Type::UByte)))
                            .build()])
                        .build(),
                ),
                ir::Node::Union(
                    ir::Union::builder()
                        .ident(ir::QualifiedIdent::from("AorB"))
                        .enum_ident(ir::QualifiedIdent::parse_str("butte_gen.AorBType"))
                        .variants(vec![
                            ir::UnionVariant::builder()
                                .ident(ir::Ident::from("A"))
                                .ty(ir::Type::Custom(
                                    ir::CustomTypeRef::builder()
                                        .ident(ir::QualifiedIdent::from("A"))
                                        .ty(ir::CustomType::Table)
                                        .build(),
                                ))
                                .build(),
                            ir::UnionVariant::builder()
                                .ident(ir::Ident::from("B"))
                                .ty(ir::Type::Custom(
                                    ir::CustomTypeRef::builder()
                                        .ident(ir::QualifiedIdent::from("B"))
                                        .ty(ir::CustomType::Table)
                                        .build(),
                                ))
                                .build(),
                        ])
                        .build(),
                ),
            ],
        };

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_table_using_union() {
        let input = "\
        table A {
            /// A nice message
            message: string;
        }
        table B {
            /// A nicer message
            message: [ubyte];
        }
        union AorB {
            /// Multiple
            /// lines
            /// of comments.
            A,
            /// Comments
            B
        }
        table Z {
            a_or_b: AorB;
        }
        ";
        let (_, schema) = crate::parser::schema_decl(input).unwrap();
        let actual = Builder::build(schema).unwrap();
        let expected = ir::Root {
            nodes: vec![
                ir::Node::Namespace(
                    ir::Namespace::builder()
                        .ident(ir::QualifiedIdent::parse_str("butte_gen"))
                        .nodes(vec![ir::Node::Enum(
                            ir::Enum::builder()
                                .ident(ir::QualifiedIdent::parse_str("butte_gen.AorBType"))
                                .base_type(ir::EnumBaseType::UByte)
                                .variants(vec![
                                    ir::EnumVariant::builder()
                                        .ident(ir::Ident::from("None"))
                                        .value(Some(0.into()))
                                        .build(),
                                    ir::EnumVariant::builder()
                                        .ident(ir::Ident::from("A"))
                                        .doc(vec![" Multiple", " lines", " of comments."].into())
                                        .build(),
                                    ir::EnumVariant::builder()
                                        .ident(ir::Ident::from("B"))
                                        .doc(vec![" Comments"].into())
                                        .build(),
                                ])
                                .build(),
                        )])
                        .build(),
                ),
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::QualifiedIdent::from("A"))
                        .fields(vec![ir::Field::builder()
                            .ident(ir::Ident::from("message"))
                            .ty(ir::Type::String)
                            .doc(vec![" A nice message"].into())
                            .build()])
                        .build(),
                ),
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::QualifiedIdent::from("B"))
                        .fields(vec![ir::Field::builder()
                            .ident(ir::Ident::from("message"))
                            .ty(ir::Type::Array(Box::new(ir::Type::UByte)))
                            .doc(vec![" A nicer message"].into())
                            .build()])
                        .build(),
                ),
                ir::Node::Union(
                    ir::Union::builder()
                        .ident(ir::QualifiedIdent::from("AorB"))
                        .enum_ident(ir::QualifiedIdent::parse_str("butte_gen.AorBType"))
                        .variants(vec![
                            ir::UnionVariant::builder()
                                .ident(ir::Ident::from("A"))
                                .ty(ir::Type::Custom(
                                    ir::CustomTypeRef::builder()
                                        .ident(ir::QualifiedIdent::from("A"))
                                        .ty(ir::CustomType::Table)
                                        .build(),
                                ))
                                .doc(vec![" Multiple", " lines", " of comments."].into())
                                .build(),
                            ir::UnionVariant::builder()
                                .ident(ir::Ident::from("B"))
                                .ty(ir::Type::Custom(
                                    ir::CustomTypeRef::builder()
                                        .ident(ir::QualifiedIdent::from("B"))
                                        .ty(ir::CustomType::Table)
                                        .build(),
                                ))
                                .doc(vec![" Comments"].into())
                                .build(),
                        ])
                        .build(),
                ),
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::QualifiedIdent::from("Z"))
                        .fields(vec![
                            // Synthetic union enum type!
                            ir::Field::builder()
                                .ident(ir::Ident::from("a_or_b_type"))
                                .ty(ir::Type::Custom(
                                    ir::CustomTypeRef::builder()
                                        .ident(ir::QualifiedIdent::parse_str("butte_gen.AorBType"))
                                        .ty(ir::CustomType::Enum {
                                            variants: vec![
                                                ir::EnumVariant::builder()
                                                    .ident(ir::Ident::from("None"))
                                                    .value(Some(0.into()))
                                                    .build(),
                                                ir::EnumVariant::builder()
                                                    .ident(ir::Ident::from("A"))
                                                    .doc(
                                                        vec![
                                                            " Multiple",
                                                            " lines",
                                                            " of comments.",
                                                        ]
                                                        .into(),
                                                    )
                                                    .build(),
                                                ir::EnumVariant::builder()
                                                    .ident(ir::Ident::from("B"))
                                                    .doc(vec![" Comments"].into())
                                                    .build(),
                                            ],
                                            base_type: ir::EnumBaseType::UByte,
                                        })
                                        .build(),
                                ))
                                .build(),
                            ir::Field::builder()
                                .ident(ir::Ident::from("a_or_b"))
                                .ty(ir::Type::Custom(
                                    ir::CustomTypeRef::builder()
                                        .ident(ir::QualifiedIdent::from("AorB"))
                                        .ty(ir::CustomType::Union {
                                            enum_ident: ir::QualifiedIdent::parse_str(
                                                "butte_gen.AorBType",
                                            ),
                                            variants: vec![
                                                ir::UnionVariant {
                                                    ty: ir::Type::Custom(
                                                        ir::CustomTypeRef::builder()
                                                            .ident(ir::QualifiedIdent::from("A"))
                                                            .ty(ir::CustomType::Table)
                                                            .build(),
                                                    ),
                                                    ident: ir::Ident::from("A"),
                                                    doc: vec![
                                                        " Multiple",
                                                        " lines",
                                                        " of comments.",
                                                    ]
                                                    .into(),
                                                },
                                                ir::UnionVariant {
                                                    ty: ir::Type::Custom(
                                                        ir::CustomTypeRef::builder()
                                                            .ident(ir::QualifiedIdent::from("B"))
                                                            .ty(ir::CustomType::Table)
                                                            .build(),
                                                    ),
                                                    ident: ir::Ident::from("B"),
                                                    doc: vec![" Comments"].into(),
                                                },
                                            ],
                                        })
                                        .build(),
                                ))
                                .build(),
                        ])
                        .build(),
                ),
            ],
        };

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_rpc_method() {
        let input = "\
namespace foo.bar;
table HelloMsg {
    val: string;
}

table HelloReply {
    message: HelloMsg;
}

rpc_service Greeter {
    SayHello(foo.bar.HelloMsg) : foo.bar.HelloReply;
}
  
";
        let (_, schema) = crate::parser::schema_decl(input).unwrap();
        let actual = Builder::build(schema).unwrap();
        let expected = ir::Root {
            nodes: vec![ir::Node::Namespace(
                ir::Namespace::builder()
                    .ident(ir::QualifiedIdent::parse_str("foo"))
                    .nodes(vec![ir::Node::Namespace(
                        ir::Namespace::builder()
                            .ident(ir::QualifiedIdent::parse_str("foo.bar"))
                            .nodes(vec![
                                ir::Node::Table(
                                    ir::Table::builder()
                                        .ident(ir::QualifiedIdent::parse_str("foo.bar.HelloMsg"))
                                        .root_type(true)
                                        .fields(vec![ir::Field::builder()
                                            .ident(ir::Ident::from("val"))
                                            .ty(ir::Type::String)
                                            .build()])
                                        .build(),
                                ),
                                ir::Node::Table(
                                    ir::Table::builder()
                                        .ident(ir::QualifiedIdent::parse_str("foo.bar.HelloReply"))
                                        .root_type(true)
                                        .fields(vec![ir::Field::builder()
                                            .ident(ir::Ident::from("message"))
                                            .ty(ir::Type::Custom(
                                                ir::CustomTypeRef::builder()
                                                    .ident(ir::QualifiedIdent::parse_str(
                                                        "foo.bar.HelloMsg",
                                                    ))
                                                    .ty(ir::CustomType::Table)
                                                    .build(),
                                            ))
                                            .build()])
                                        .build(),
                                ),
                                ir::Node::Rpc(
                                    ir::Rpc::builder()
                                        .ident(ir::QualifiedIdent::parse_str("foo.bar.Greeter"))
                                        .methods(vec![ir::RpcMethod::builder()
                                            .ident("SayHello".into())
                                            .snake_ident("say_hello".into())
                                            .request_type(ir::QualifiedIdent::parse_str(
                                                "foo.bar.HelloMsg",
                                            ))
                                            .response_type(ir::QualifiedIdent::parse_str(
                                                "foo.bar.HelloReply",
                                            ))
                                            .build()])
                                        .build(),
                                ),
                            ])
                            .build(),
                    )])
                    .build(),
            )],
        };

        assert_eq!(actual, expected);
    }
}
