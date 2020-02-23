use crate::{ast::types as ast, ir::types as ir};
use anyhow::{anyhow, Result};
use heck::SnakeCase;
use itertools::Itertools;
use std::{
    collections::{BTreeMap, HashMap, HashSet, VecDeque},
    convert::TryFrom,
};

#[derive(Default)]
pub struct Builder<'a> {
    current_namespace: Option<ast::Namespace<'a>>,
    types: HashMap<ir::DottedIdent<'a>, CustomTypeStatus<'a>>,
    nodes: BTreeMap<usize, Vec<ir::Node<'a>>>,
    root_types: HashSet<ir::DottedIdent<'a>>,
}

struct ElementInput<'a> {
    el: ast::Element<'a>,
    pos: usize,
}

#[derive(Clone, Debug, PartialEq)]
enum CustomTypeStatus<'a> {
    TableDeclared,
    Defined(ir::CustomType<'a>),
}

impl<'a> Builder<'a> {
    fn new_element(&mut self, element: &ElementInput<'a>) -> Result<bool> {
        let pos = element.pos;
        Ok(match &element.el {
            ast::Element::Namespace(n) => self.new_namespace(n, pos)?,
            ast::Element::Table(t) => self.new_table(t, pos)?,
            ast::Element::Struct(s) => self.new_struct(s, pos)?,
            ast::Element::Enum(e) => self.new_enum(e, pos)?,
            ast::Element::Union(u) => self.new_union(u, pos)?,
            ast::Element::Rpc(r) => self.new_rpc(r, pos)?,
            ast::Element::Root(r) => self.new_root(r, pos)?,
            ast::Element::FileExtension(..) => unimplemented!(),
            ast::Element::FileIdentifier(..) => unimplemented!(),
            ast::Element::Attribute(..) => unimplemented!(),
            ast::Element::Object(..) => unimplemented!(),
        })
    }
    fn new_namespace(&mut self, ns: &ast::Namespace<'a>, pos: usize) -> Result<bool> {
        self.current_namespace = Some(ns.clone());
        self.nodes.insert(
            pos,
            vec![ir::Node::Namespace(
                ir::Namespace::builder()
                    .ident(ir::DottedIdent::from(ns.ident.clone()))
                    .build(),
            )],
        );

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
                // If it's a union, push the enum type field first
                if let Some(enum_ty_ident) = ty.make_union_enum_type_companion() {
                    let ty = self
                        .find_custom_type(enum_ty_ident.clone())
                        .expect("Union enum type companion should be defined now");
                    let ident = ir::Ident::from(format!("{}_type", &f.id.raw.to_snake_case()));
                    let default_value = None;
                    let doc = f.doc.clone();
                    fields.push(ir::Field {
                        ident,
                        ty,
                        default_value,
                        doc,
                    });
                }
                let ident = ir::Ident::from(&f.id);
                let default_value = f.default_value.clone();
                let doc = f.doc.clone();

                fields.push(ir::Field {
                    ident,
                    ty,
                    default_value,
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
                let doc = f.doc.clone();

                fields.push(ir::Field {
                    ident,
                    ty,
                    default_value,
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
        let values: Vec<_> = e
            .values
            .iter()
            .map(|v| ir::EnumVal {
                ident: ir::Ident::from(v.id),
                value: v.value,
            })
            .collect();

        let doc = e.doc.clone();

        self.types.insert(
            ident.clone(),
            CustomTypeStatus::Defined(ir::CustomType::Enum {
                values: values.clone(),
                base_type,
            }),
        );

        let s = ir::Enum {
            ident: ident.clone(),
            base_type,
            values,
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
        for f in &u.values {
            let ty = self.try_type(&ast::Type::Ident(ast::DottedIdent::from(vec![f.id])));
            if let Some(ty) = ty {
                let id = &f.id;
                variants.push(ir::UnionVariant {
                    ty,
                    ident: ir::Ident::from(id),
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
        let ident = &self.make_fully_qualified_ident(ir::Ident::from(enum_ident));

        if let Some(CustomTypeStatus::Defined(..)) = self.types.get(ident) {
            return Err(anyhow!("type already defined: {}", ident));
        };

        // There's a max of 255 variants in a union in flatbuffers
        // See https://github.com/google/flatbuffers/issues/4209
        let base_type = ir::EnumBaseType::UByte;
        let values: Vec<_> = std::iter::once(ir::EnumVal {
            ident: ir::Ident::from("None"),
            value: Some(0),
        })
        .chain(u.values.iter().map(|v| ir::EnumVal {
            ident: ir::Ident::from(v.id),
            value: None,
        }))
        .collect();

        let doc = u.doc.clone();

        self.types.insert(
            ident.clone(),
            CustomTypeStatus::Defined(ir::CustomType::Enum {
                values: values.clone(),
                base_type,
            }),
        );

        Ok(ir::Enum {
            ident: ident.clone(),
            base_type,
            values,
            doc,
        })
    }

    fn new_root(&mut self, root: &ast::Root<'a>, _: usize) -> Result<bool> {
        let ident = self.make_fully_qualified_ident(ir::Ident::from(&root.typename));

        self.new_root_ident(&ident)
    }

    fn new_root_ident(&mut self, ident: &ir::DottedIdent<'a>) -> Result<bool> {
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

            let request_type = self.make_fully_qualified_from_ast_dotted_ident(&m.request_type);
            let response_type = self.make_fully_qualified_from_ast_dotted_ident(&m.response_type);

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

    fn make_fully_qualified_ident(&self, ident: ir::Ident<'a>) -> ir::DottedIdent<'a> {
        let mut fqi = self
            .current_namespace()
            .map(|ns| ir::DottedIdent::from(&ns.ident))
            .unwrap_or_default();
        fqi.parts.push(ident);
        fqi
    }

    fn make_fully_qualified_from_ast_dotted_ident(
        &self,
        dotted_ident: &ast::DottedIdent<'a>,
    ) -> ir::DottedIdent<'a> {
        if dotted_ident.parts.len() == 1 {
            // We need to prefix the namespace
            self.make_fully_qualified_ident(ir::Ident::from(dotted_ident.parts[0]))
        } else {
            ir::DottedIdent::from(dotted_ident.clone())
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
            ast::Type::Ident(dotted_ident) => {
                let ident = self.make_fully_qualified_from_ast_dotted_ident(&dotted_ident);

                return self.find_custom_type(ident);
            }
        })
    }

    fn find_custom_type(&self, fully_qualified_ident: ir::DottedIdent<'a>) -> Option<ir::Type<'a>> {
        let ident = fully_qualified_ident;

        let ty = match self.types.get(&ident) {
            Some(CustomTypeStatus::TableDeclared) => ir::CustomType::Table,
            Some(CustomTypeStatus::Defined(ty)) => ty.clone(),
            _ => return None, // Type unsatisfied
        };

        Some(ir::Type::Custom(ir::CustomTypeRef { ident, ty }))
    }

    pub fn build(schema: ast::Schema<'a>) -> Result<ir::Root<'a>> {
        let mut context = Builder {
            current_namespace: None,
            types: HashMap::new(),
            nodes: BTreeMap::new(),
            root_types: HashSet::new(),
        };

        // Keep track of the definition order,
        // mostly to build IR deterministically
        let mut input: VecDeque<_> = schema
            .elements
            .into_iter()
            .enumerate()
            .map(|(i, el)| ElementInput { el, pos: i })
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
                    return Err(anyhow!("Unable to type schema")); // TODO proper reporting
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

        // handle namespaces
        let (namespaces, rest): (Vec<_>, Vec<_>) =
            nodes.into_iter().partition(ir::Node::is_namespace);

        let mut namespaces: Vec<_> = namespaces
            .into_iter()
            .map(|ns| ns.into_namespace().expect("should be a namespace"))
            .collect();

        let mut elements: HashMap<_, _> = rest
            .into_iter()
            .map(|n| (n.namespace(), n))
            .into_group_map()
            .into_iter()
            .collect();

        for ns in namespaces.iter_mut() {
            if let Some(children) = elements.remove(&Some(ns.ident.clone())) {
                ns.nodes = children;
            }
        }

        // Build top-level with namespaces and elements without namespaces
        let mut nodes = Vec::new();
        // stack namespaces
        for mut ns in namespaces.into_iter() {
            let ns_depth = ns.ident.parts.len();
            for i in (1..ns_depth).rev() {
                let mut parent = ir::Namespace::builder()
                    .ident(ir::DottedIdent::from(ns.ident.parts[..i].to_vec()))
                    .build();
                parent.nodes.push(ir::Node::Namespace(ns));
                ns = parent;
            }
            nodes.push(ir::Node::Namespace(ns));
        }

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
table HelloReply {
    message:string;
}
";
        let (_, schema) = crate::parser::schema_decl(input).unwrap();
        let actual = Builder::build(schema).unwrap();
        let expected = ir::Root {
            nodes: vec![ir::Node::Table(
                ir::Table::builder()
                    .ident(ir::DottedIdent::from("HelloReply"))
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
    message:string;
}

root_type HelloReply;
";
        let (_, schema) = crate::parser::schema_decl(input).unwrap();
        let actual = Builder::build(schema).unwrap();
        let expected = ir::Root {
            nodes: vec![ir::Node::Table(
                ir::Table::builder()
                    .ident(ir::DottedIdent::from("HelloReply"))
                    .fields(vec![ir::Field::builder()
                        .ident(ir::Ident::from("message"))
                        .ty(ir::Type::String)
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
                    .ident(ir::DottedIdent::parse_str("foo"))
                    .nodes(vec![ir::Node::Namespace(
                        ir::Namespace::builder()
                            .ident(ir::DottedIdent::parse_str("foo.bar"))
                            .nodes(vec![ir::Node::Table(
                                ir::Table::builder()
                                    .ident(ir::DottedIdent::parse_str("foo.bar.HelloReply"))
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
                        .ident(ir::DottedIdent::from("HelloMsg"))
                        .fields(vec![ir::Field::builder()
                            .ident(ir::Ident::from("val"))
                            .ty(ir::Type::String)
                            .build()])
                        .build(),
                ),
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::DottedIdent::from("HelloReply"))
                        .fields(vec![ir::Field::builder()
                            .ident(ir::Ident::from("message"))
                            .ty(ir::Type::Custom(
                                ir::CustomTypeRef::builder()
                                    .ident(ir::DottedIdent::from("HelloMsg"))
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
                        .ident(ir::DottedIdent::from("HelloReply"))
                        .fields(vec![ir::Field::builder()
                            .ident(ir::Ident::from("message"))
                            .ty(ir::Type::Custom(
                                ir::CustomTypeRef::builder()
                                    .ident(ir::DottedIdent::from("HelloMsg"))
                                    .ty(ir::CustomType::Table)
                                    .build(),
                            ))
                            .build()])
                        .build(),
                ),
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::DottedIdent::from("HelloMsg"))
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
                        .ident(ir::DottedIdent::from("HelloReply"))
                        .fields(vec![ir::Field::builder()
                            .ident(ir::Ident::from("message"))
                            .ty(ir::Type::Custom(
                                ir::CustomTypeRef::builder()
                                    .ident(ir::DottedIdent::from("HelloMsg"))
                                    .ty(ir::CustomType::Table)
                                    .build(),
                            ))
                            .build()])
                        .root_type(true)
                        .build(),
                ),
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::DottedIdent::from("HelloMsg"))
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
                    .ident(ir::DottedIdent::from("HelloReply"))
                    .fields(vec![ir::Field::builder()
                        .ident(ir::Ident::from("message"))
                        .ty(ir::Type::Custom(
                            ir::CustomTypeRef::builder()
                                .ident(ir::DottedIdent::from("HelloReply"))
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
                    .ident(ir::DottedIdent::from("Vector3"))
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
                        .ident(ir::DottedIdent::from("Vector2"))
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
                        .ident(ir::DottedIdent::from("Vector3"))
                        .fields(vec![
                            ir::Field::builder()
                                .ident(ir::Ident::from("xy"))
                                .ty(ir::Type::Custom(
                                    ir::CustomTypeRef::builder()
                                        .ident(ir::DottedIdent::from("Vector2"))
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
                    .ident(ir::DottedIdent::from("MyEnum"))
                    .base_type(ir::EnumBaseType::UByte)
                    .values(vec![
                        ir::EnumVal::builder().ident(ir::Ident::from("Foo")).build(),
                        ir::EnumVal::builder().ident(ir::Ident::from("Bar")).build(),
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
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::DottedIdent::from("A"))
                        .fields(vec![ir::Field::builder()
                            .ident(ir::Ident::from("message"))
                            .ty(ir::Type::String)
                            .build()])
                        .build(),
                ),
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::DottedIdent::from("B"))
                        .fields(vec![ir::Field::builder()
                            .ident(ir::Ident::from("message"))
                            .ty(ir::Type::Array(Box::new(ir::Type::UByte)))
                            .build()])
                        .build(),
                ),
                ir::Node::Enum(
                    ir::Enum::builder()
                        .ident(ir::DottedIdent::from("AorBType"))
                        .base_type(ir::EnumBaseType::UByte)
                        .values(vec![
                            ir::EnumVal::builder()
                                .ident(ir::Ident::from("None"))
                                .value(Some(0))
                                .build(),
                            ir::EnumVal::builder().ident(ir::Ident::from("A")).build(),
                            ir::EnumVal::builder().ident(ir::Ident::from("B")).build(),
                        ])
                        .build(),
                ),
                ir::Node::Union(
                    ir::Union::builder()
                        .ident(ir::DottedIdent::from("AorB"))
                        .enum_ident(ir::DottedIdent::from("AorBType"))
                        .variants(vec![
                            ir::UnionVariant::builder()
                                .ident(ir::Ident::from("A"))
                                .ty(ir::Type::Custom(
                                    ir::CustomTypeRef::builder()
                                        .ident(ir::DottedIdent::from("A"))
                                        .ty(ir::CustomType::Table)
                                        .build(),
                                ))
                                .build(),
                            ir::UnionVariant::builder()
                                .ident(ir::Ident::from("B"))
                                .ty(ir::Type::Custom(
                                    ir::CustomTypeRef::builder()
                                        .ident(ir::DottedIdent::from("B"))
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
            message: string;
        }
        table B {
            message: [ubyte];
        }
        union AorB {
            A,
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
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::DottedIdent::from("A"))
                        .fields(vec![ir::Field::builder()
                            .ident(ir::Ident::from("message"))
                            .ty(ir::Type::String)
                            .build()])
                        .build(),
                ),
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::DottedIdent::from("B"))
                        .fields(vec![ir::Field::builder()
                            .ident(ir::Ident::from("message"))
                            .ty(ir::Type::Array(Box::new(ir::Type::UByte)))
                            .build()])
                        .build(),
                ),
                ir::Node::Enum(
                    ir::Enum::builder()
                        .ident(ir::DottedIdent::from("AorBType"))
                        .base_type(ir::EnumBaseType::UByte)
                        .values(vec![
                            ir::EnumVal::builder()
                                .ident(ir::Ident::from("None"))
                                .value(Some(0))
                                .build(),
                            ir::EnumVal::builder().ident(ir::Ident::from("A")).build(),
                            ir::EnumVal::builder().ident(ir::Ident::from("B")).build(),
                        ])
                        .build(),
                ),
                ir::Node::Union(
                    ir::Union::builder()
                        .ident(ir::DottedIdent::from("AorB"))
                        .enum_ident(ir::DottedIdent::from("AorBType"))
                        .variants(vec![
                            ir::UnionVariant::builder()
                                .ident(ir::Ident::from("A"))
                                .ty(ir::Type::Custom(
                                    ir::CustomTypeRef::builder()
                                        .ident(ir::DottedIdent::from("A"))
                                        .ty(ir::CustomType::Table)
                                        .build(),
                                ))
                                .build(),
                            ir::UnionVariant::builder()
                                .ident(ir::Ident::from("B"))
                                .ty(ir::Type::Custom(
                                    ir::CustomTypeRef::builder()
                                        .ident(ir::DottedIdent::from("B"))
                                        .ty(ir::CustomType::Table)
                                        .build(),
                                ))
                                .build(),
                        ])
                        .build(),
                ),
                ir::Node::Table(
                    ir::Table::builder()
                        .ident(ir::DottedIdent::from("Z"))
                        .fields(vec![
                            // Synthetic union enum type!
                            ir::Field::builder()
                                .ident(ir::Ident::from("a_or_b_type"))
                                .ty(ir::Type::Custom(
                                    ir::CustomTypeRef::builder()
                                        .ident(ir::DottedIdent::from("AorBType"))
                                        .ty(ir::CustomType::Enum {
                                            values: vec![
                                                ir::EnumVal::builder()
                                                    .ident(ir::Ident::from("None"))
                                                    .value(Some(0))
                                                    .build(),
                                                ir::EnumVal::builder()
                                                    .ident(ir::Ident::from("A"))
                                                    .build(),
                                                ir::EnumVal::builder()
                                                    .ident(ir::Ident::from("B"))
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
                                        .ident(ir::DottedIdent::from("AorB"))
                                        .ty(ir::CustomType::Union {
                                            enum_ident: ir::DottedIdent::from("AorBType"),
                                            variants: vec![
                                                ir::UnionVariant {
                                                    ty: ir::Type::Custom(
                                                        ir::CustomTypeRef::builder()
                                                            .ident(ir::DottedIdent::from("A"))
                                                            .ty(ir::CustomType::Table)
                                                            .build(),
                                                    ),
                                                    ident: ir::Ident::from("A"),
                                                },
                                                ir::UnionVariant {
                                                    ty: ir::Type::Custom(
                                                        ir::CustomTypeRef::builder()
                                                            .ident(ir::DottedIdent::from("B"))
                                                            .ty(ir::CustomType::Table)
                                                            .build(),
                                                    ),
                                                    ident: ir::Ident::from("B"),
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
                    .ident(ir::DottedIdent::parse_str("foo"))
                    .nodes(vec![ir::Node::Namespace(
                        ir::Namespace::builder()
                            .ident(ir::DottedIdent::parse_str("foo.bar"))
                            .nodes(vec![
                                ir::Node::Table(
                                    ir::Table::builder()
                                        .ident(ir::DottedIdent::parse_str("foo.bar.HelloMsg"))
                                        .root_type(true)
                                        .fields(vec![ir::Field::builder()
                                            .ident(ir::Ident::from("val"))
                                            .ty(ir::Type::String)
                                            .build()])
                                        .build(),
                                ),
                                ir::Node::Table(
                                    ir::Table::builder()
                                        .ident(ir::DottedIdent::parse_str("foo.bar.HelloReply"))
                                        .root_type(true)
                                        .fields(vec![ir::Field::builder()
                                            .ident(ir::Ident::from("message"))
                                            .ty(ir::Type::Custom(
                                                ir::CustomTypeRef::builder()
                                                    .ident(ir::DottedIdent::parse_str(
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
                                        .ident(ir::DottedIdent::parse_str("foo.bar.Greeter"))
                                        .methods(vec![ir::RpcMethod::builder()
                                            .ident("SayHello")
                                            .snake_ident("say_hello")
                                            .request_type(ir::DottedIdent::parse_str(
                                                "foo.bar.HelloMsg",
                                            ))
                                            .response_type(ir::DottedIdent::parse_str(
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
