use crate::{ir::types::*, parse::types::*};
use anyhow::{anyhow, Result};
use heck::SnakeCase;
use itertools::Itertools;
use std::{
    collections::{BTreeMap, HashMap, VecDeque},
    convert::TryFrom,
};

#[derive(Default)]
pub struct IrBuilder<'a> {
    current_namespace: Option<Namespace<'a>>,
    types: HashMap<IrDottedIdent<'a>, IrCustomTypeStatus<'a>>,
    nodes: BTreeMap<usize, Vec<IrNode<'a>>>,
}

struct ElementInput<'a> {
    el: Element<'a>,
    pos: usize,
}

enum IrCustomTypeStatus<'a> {
    TableDeclared,
    Defined(IrCustomType<'a>),
}

impl<'a> IrBuilder<'a> {
    fn new_element(&mut self, element: &ElementInput<'a>) -> Result<bool> {
        let pos = element.pos;
        Ok(match &element.el {
            Element::Namespace(n) => self.new_namespace(n, pos)?,
            Element::Table(t) => self.new_table(t, pos)?,
            Element::Struct(s) => self.new_struct(s, pos)?,
            Element::Enum(e) => self.new_enum(e, pos)?,
            Element::Union(u) => self.new_union(u, pos)?,
            Element::Rpc(r) => self.new_rpc(r, pos)?,
            Element::Root(..) => true, // Skip root types for now
            Element::FileExtension(..) => unimplemented!(),
            Element::FileIdentifier(..) => unimplemented!(),
            Element::Attribute(..) => unimplemented!(),
            Element::Object(..) => unimplemented!(),
        })
    }
    fn new_namespace(&mut self, ns: &Namespace<'a>, pos: usize) -> Result<bool> {
        self.current_namespace = Some(ns.clone());
        self.nodes.insert(
            pos,
            vec![IrNode::Namespace(
                IrNamespace::builder()
                    .ident(IrDottedIdent::from(ns.ident.clone()))
                    .build(),
            )],
        );

        Ok(true)
    }

    fn new_table(&mut self, t: &Table<'a>, pos: usize) -> Result<bool> {
        // Has this type been referenced before?
        // Let's find out and update the status.
        let ident = &self.make_fully_qualified_ident(IrIdent::from(t.id));

        if let Some(IrCustomTypeStatus::Defined(..)) = self.types.get(ident) {
            return Err(anyhow!("type already defined: {}", ident));
        };

        // We consider the table declared even if all its fields haven't been
        // properly resolved, because tables may be recursive.
        self.types
            .insert(ident.clone(), IrCustomTypeStatus::TableDeclared);

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
                    let ident = IrIdent::from(format!("{}_type", &f.id.raw.to_snake_case()));
                    let default_value = None;
                    let metadata = IrMetadata::default();
                    let doc = f.doc.clone();
                    fields.push(IrField {
                        ident,
                        ty,
                        default_value,
                        metadata,
                        doc,
                    });
                }
                let ident = IrIdent::from(&f.id);
                let default_value = f.default_value.clone();
                let metadata = IrMetadata::default();
                let doc = f.doc.clone();

                fields.push(IrField {
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
        let metadata = IrMetadata::default();
        let doc = t.doc.clone();

        self.types.insert(
            ident.clone(),
            IrCustomTypeStatus::Defined(IrCustomType::Table),
        );

        self.nodes.insert(
            pos,
            vec![IrNode::Table(IrTable {
                ident,
                fields,
                metadata,
                doc,
            })],
        );

        Ok(true)
    }

    fn new_struct(&mut self, t: &Struct<'a>, pos: usize) -> Result<bool> {
        let ident = &self.make_fully_qualified_ident(IrIdent::from(t.id));

        if let Some(IrCustomTypeStatus::Defined(..)) = self.types.get(ident) {
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

                let ident = IrIdent::from(&f.id);
                let default_value = f.default_value.clone();
                let metadata = IrMetadata::default();
                let doc = f.doc.clone();

                fields.push(IrField {
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

        let metadata = IrMetadata::default();
        let doc = t.doc.clone();

        self.types.insert(
            ident.clone(),
            IrCustomTypeStatus::Defined(IrCustomType::Struct {
                fields: fields.clone(),
            }),
        );

        let s = IrStruct {
            ident: ident.clone(),
            fields,
            metadata,
            doc,
        };

        self.nodes.insert(pos, vec![IrNode::Struct(s)]);

        Ok(true)
    }

    fn new_enum(&mut self, e: &Enum<'a>, pos: usize) -> Result<bool> {
        let ident = &self.make_fully_qualified_ident(IrIdent::from(e.id));

        if let Some(IrCustomTypeStatus::Defined(..)) = self.types.get(ident) {
            return Err(anyhow!("type already defined: {}", ident));
        };

        let base_type = IrEnumBaseType::try_from(&e.base_type)
            .map_err(|_| anyhow!("enum has bad base type: {}", ident))?;
        let values: Vec<_> = e
            .values
            .iter()
            .map(|v| IrEnumVal {
                ident: IrIdent::from(v.id),
                value: v.value,
            })
            .collect();

        let metadata = IrMetadata::default();
        let doc = e.doc.clone();

        self.types.insert(
            ident.clone(),
            IrCustomTypeStatus::Defined(IrCustomType::Enum {
                values: values.clone(),
                base_type,
            }),
        );

        let s = IrEnum {
            ident: ident.clone(),
            base_type,
            values,
            metadata,
            doc,
        };

        self.nodes.insert(pos, vec![IrNode::Enum(s)]);

        Ok(true)
    }

    fn new_union(&mut self, u: &Union<'a>, pos: usize) -> Result<bool> {
        let ident = &self.make_fully_qualified_ident(IrIdent::from(u.id));

        if let Some(IrCustomTypeStatus::Defined(..)) = self.types.get(ident) {
            return Err(anyhow!("type already defined: {}", ident));
        };

        let mut variants = Vec::new();

        // Make sure all references to custom types are resolved
        for f in &u.values {
            let ty = self.try_type(&Type::Ident(DottedIdent::from(vec![f.id])));
            if let Some(ty) = ty {
                let id = &f.id;
                variants.push(IrUnionVariant {
                    ty,
                    ident: IrIdent::from(id),
                });
            } else {
                // not satisfied yet, so we can't build the fields
                // we will try after having seen all other types.
                return Ok(false);
            }
        }

        let metadata = IrMetadata::default();
        let doc = u.doc.clone();

        // We generate an enum for this union which describes the different variants
        let e = self.generate_enum_for_union(u)?;

        self.types.insert(
            ident.clone(),
            IrCustomTypeStatus::Defined(IrCustomType::Union {
                enum_ident: e.ident.clone(),
                variants: variants.clone(),
            }),
        );

        let s = IrUnion {
            ident: ident.clone(),
            enum_ident: e.ident.clone(),
            variants,
            metadata,
            doc,
        };

        self.nodes
            .insert(pos, vec![IrNode::Enum(e), IrNode::Union(s)]);

        Ok(true)
    }

    fn generate_enum_for_union(&mut self, u: &Union<'a>) -> Result<IrEnum<'a>> {
        let enum_ident = format!("{}Type", u.id.raw);
        let ident = &self.make_fully_qualified_ident(IrIdent::from(enum_ident));

        if let Some(IrCustomTypeStatus::Defined(..)) = self.types.get(ident) {
            return Err(anyhow!("type already defined: {}", ident));
        };

        // There's a max of 255 variants in a union in flatbuffers
        // See https://github.com/google/flatbuffers/issues/4209
        let base_type = IrEnumBaseType::UByte;
        let values: Vec<_> = std::iter::once(IrEnumVal {
            ident: IrIdent::from("None"),
            value: Some(0),
        })
        .chain(u.values.iter().map(|v| IrEnumVal {
            ident: IrIdent::from(v.id),
            value: None,
        }))
        .collect();

        let metadata = IrMetadata::default();
        let doc = u.doc.clone();

        self.types.insert(
            ident.clone(),
            IrCustomTypeStatus::Defined(IrCustomType::Enum {
                values: values.clone(),
                base_type,
            }),
        );

        Ok(IrEnum {
            ident: ident.clone(),
            base_type,
            values,
            metadata,
            doc,
        })
    }

    fn new_rpc(&mut self, rpc: &Rpc<'a>, pos: usize) -> Result<bool> {
        let ident = &self.make_fully_qualified_ident(IrIdent::from(&rpc.id));

        let mut methods = Vec::new();

        for m in &rpc.methods {
            let ident = IrIdent::from(&m.id);
            let request_type = IrDottedIdent::from(&m.request_type);
            let response_type = IrDottedIdent::from(&m.response_type);
            let streaming = match &m.metadata {
                Some(m) => match m.values.get(&Ident::from("streaming")) {
                    Some(Some(Single::String("client"))) => IrRpcStreaming::Client,
                    Some(Some(Single::String("server"))) => IrRpcStreaming::Server,
                    Some(Some(Single::String("bidi"))) => IrRpcStreaming::Bidi,
                    None => IrRpcStreaming::None,
                    Some(Some(s)) => return Err(anyhow!("Unknown streaming type: {:?}", s)),
                    Some(None) => {
                        return Err(anyhow!(
                        "Missing streaming type: possible values are `client`, `server` and `bidi`"
                    ))
                    }
                },
                None => IrRpcStreaming::None,
            };
            let metadata = IrRpcMethodMetadata { streaming };
            methods.push(IrRpcMethod {
                ident,
                request_type,
                response_type,
                metadata,
                doc: m.doc.clone(),
            });
        }

        let s = IrRpc {
            ident: ident.clone(),
            methods,
            doc: rpc.doc.clone(),
        };

        self.nodes.insert(pos, vec![IrNode::Rpc(s)]);

        Ok(true)
    }

    fn current_namespace(&self) -> Option<&Namespace<'a>> {
        self.current_namespace.as_ref()
    }

    fn make_fully_qualified_ident(&self, ident: IrIdent<'a>) -> IrDottedIdent<'a> {
        let mut fqi = self
            .current_namespace()
            .map(|ns| IrDottedIdent::from(&ns.ident))
            .unwrap_or_default();
        fqi.parts.push(ident);
        fqi
    }

    fn try_type(&self, ty: &Type<'a>) -> Option<IrType<'a>> {
        Some(match ty {
            Type::Bool => IrType::Bool,
            Type::Byte => IrType::Byte,
            Type::UByte => IrType::UByte,
            Type::Short => IrType::Short,
            Type::UShort => IrType::UShort,
            Type::Int => IrType::Int,
            Type::UInt => IrType::UInt,
            Type::Float => IrType::Float,
            Type::Long => IrType::Long,
            Type::ULong => IrType::ULong,
            Type::Double => IrType::Double,
            Type::Int8 => IrType::Int8,
            Type::UInt8 => IrType::UInt8,
            Type::Int16 => IrType::Int16,
            Type::UInt16 => IrType::UInt16,
            Type::Int32 => IrType::Int32,
            Type::UInt32 => IrType::UInt32,
            Type::Int64 => IrType::Int64,
            Type::UInt64 => IrType::UInt64,
            Type::Float32 => IrType::Float32,
            Type::Float64 => IrType::Float64,
            Type::String => IrType::String,
            Type::Array(ty) => {
                if let Some(inner) = self.try_type(ty) {
                    IrType::Array(Box::new(inner))
                } else {
                    return None; // unsatisfied inner type
                }
            }
            Type::Ident(dotted_ident) => {
                let ident = if dotted_ident.parts.len() == 1 {
                    // We need to prefix the namespace
                    self.make_fully_qualified_ident(IrIdent::from(dotted_ident.parts[0]))
                } else {
                    IrDottedIdent::from(dotted_ident)
                };

                return self.find_custom_type(ident);
            }
        })
    }

    fn find_custom_type(&self, fully_qualified_ident: IrDottedIdent<'a>) -> Option<IrType<'a>> {
        let ident = fully_qualified_ident;

        let ty = match self.types.get(&ident) {
            Some(IrCustomTypeStatus::TableDeclared) => IrCustomType::Table,
            Some(IrCustomTypeStatus::Defined(ty)) => ty.clone(),
            _ => return None, // Type unsatisfied
        };

        Some(IrType::Custom(IrCustomTypeRef { ident, ty }))
    }

    pub fn build(schema: Schema<'a>) -> Result<IrRoot<'a>> {
        let mut context = IrBuilder {
            current_namespace: None,
            types: HashMap::new(),
            nodes: BTreeMap::new(),
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

        // handle namespaces
        let (namespaces, rest): (Vec<_>, Vec<_>) = context
            .nodes
            .into_iter()
            .map(|(_, v)| v)
            .flatten()
            .partition(IrNode::is_namespace);

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
                let mut parent = IrNamespace::builder()
                    .ident(IrDottedIdent::from(ns.ident.parts[..i].to_vec()))
                    .build();
                parent.nodes.push(IrNode::Namespace(ns));
                ns = parent;
            }
            nodes.push(IrNode::Namespace(ns));
        }

        // Are there elements without namespace?
        if let Some(rest) = elements.remove(&None) {
            nodes.extend(rest);
        }

        Ok(IrRoot { nodes })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_table_simple() {
        let input = "\
table HelloReply {
    message:string;
}
";
        let (_, schema) = crate::parser::schema_decl(input).unwrap();
        let actual = IrBuilder::build(schema).unwrap();
        let expected = IrRoot {
            nodes: vec![IrNode::Table(
                IrTable::builder()
                    .ident(IrDottedIdent::from("HelloReply"))
                    .fields(vec![IrField::builder()
                        .ident(IrIdent::from("message"))
                        .ty(IrType::String)
                        .build()])
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
        let actual = IrBuilder::build(schema).unwrap();
        let expected = IrRoot {
            nodes: vec![IrNode::Namespace(
                IrNamespace::builder()
                    .ident(IrDottedIdent::parse_str("foo"))
                    .nodes(vec![IrNode::Namespace(
                        IrNamespace::builder()
                            .ident(IrDottedIdent::parse_str("foo.bar"))
                            .nodes(vec![IrNode::Table(
                                IrTable::builder()
                                    .ident(IrDottedIdent::parse_str("foo.bar.HelloReply"))
                                    .fields(vec![IrField::builder()
                                        .ident(IrIdent::from("message"))
                                        .ty(IrType::String)
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
        let actual = IrBuilder::build(schema).unwrap();
        let expected = IrRoot {
            nodes: vec![
                IrNode::Table(
                    IrTable::builder()
                        .ident(IrDottedIdent::from("HelloMsg"))
                        .fields(vec![IrField::builder()
                            .ident(IrIdent::from("val"))
                            .ty(IrType::String)
                            .build()])
                        .build(),
                ),
                IrNode::Table(
                    IrTable::builder()
                        .ident(IrDottedIdent::from("HelloReply"))
                        .fields(vec![IrField::builder()
                            .ident(IrIdent::from("message"))
                            .ty(IrType::Custom(
                                IrCustomTypeRef::builder()
                                    .ident(IrDottedIdent::from("HelloMsg"))
                                    .ty(IrCustomType::Table)
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
        let actual = IrBuilder::build(schema).unwrap();
        let expected = IrRoot {
            nodes: vec![
                IrNode::Table(
                    IrTable::builder()
                        .ident(IrDottedIdent::from("HelloReply"))
                        .fields(vec![IrField::builder()
                            .ident(IrIdent::from("message"))
                            .ty(IrType::Custom(
                                IrCustomTypeRef::builder()
                                    .ident(IrDottedIdent::from("HelloMsg"))
                                    .ty(IrCustomType::Table)
                                    .build(),
                            ))
                            .build()])
                        .build(),
                ),
                IrNode::Table(
                    IrTable::builder()
                        .ident(IrDottedIdent::from("HelloMsg"))
                        .fields(vec![IrField::builder()
                            .ident(IrIdent::from("val"))
                            .ty(IrType::String)
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
        let actual = IrBuilder::build(schema).unwrap();
        let expected = IrRoot {
            nodes: vec![IrNode::Table(
                IrTable::builder()
                    .ident(IrDottedIdent::from("HelloReply"))
                    .fields(vec![IrField::builder()
                        .ident(IrIdent::from("message"))
                        .ty(IrType::Custom(
                            IrCustomTypeRef::builder()
                                .ident(IrDottedIdent::from("HelloReply"))
                                .ty(IrCustomType::Table)
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
        let actual = IrBuilder::build(schema).unwrap();
        let expected = IrRoot {
            nodes: vec![IrNode::Struct(
                IrStruct::builder()
                    .ident(IrDottedIdent::from("Vector3"))
                    .fields(vec![
                        IrField::builder()
                            .ident(IrIdent::from("x"))
                            .ty(IrType::Float32)
                            .build(),
                        IrField::builder()
                            .ident(IrIdent::from("y"))
                            .ty(IrType::Float32)
                            .build(),
                        IrField::builder()
                            .ident(IrIdent::from("z"))
                            .ty(IrType::Float32)
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
        let actual = IrBuilder::build(schema).unwrap();
        let expected = IrRoot {
            nodes: vec![
                IrNode::Struct(
                    IrStruct::builder()
                        .ident(IrDottedIdent::from("Vector2"))
                        .fields(vec![
                            IrField::builder()
                                .ident(IrIdent::from("x"))
                                .ty(IrType::Float32)
                                .build(),
                            IrField::builder()
                                .ident(IrIdent::from("y"))
                                .ty(IrType::Float32)
                                .build(),
                        ])
                        .build(),
                ),
                IrNode::Struct(
                    IrStruct::builder()
                        .ident(IrDottedIdent::from("Vector3"))
                        .fields(vec![
                            IrField::builder()
                                .ident(IrIdent::from("xy"))
                                .ty(IrType::Custom(
                                    IrCustomTypeRef::builder()
                                        .ident(IrDottedIdent::from("Vector2"))
                                        .ty(IrCustomType::Struct {
                                            fields: vec![
                                                IrField::builder()
                                                    .ident(IrIdent::from("x"))
                                                    .ty(IrType::Float32)
                                                    .build(),
                                                IrField::builder()
                                                    .ident(IrIdent::from("y"))
                                                    .ty(IrType::Float32)
                                                    .build(),
                                            ],
                                        })
                                        .build(),
                                ))
                                .build(),
                            IrField::builder()
                                .ident(IrIdent::from("z"))
                                .ty(IrType::Float32)
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
        let actual = IrBuilder::build(schema).unwrap();
        let expected = IrRoot {
            nodes: vec![IrNode::Enum(
                IrEnum::builder()
                    .ident(IrDottedIdent::from("MyEnum"))
                    .base_type(IrEnumBaseType::UByte)
                    .values(vec![
                        IrEnumVal::builder().ident(IrIdent::from("Foo")).build(),
                        IrEnumVal::builder().ident(IrIdent::from("Bar")).build(),
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
        let actual = IrBuilder::build(schema).unwrap();
        let expected = IrRoot {
            nodes: vec![
                IrNode::Table(
                    IrTable::builder()
                        .ident(IrDottedIdent::from("A"))
                        .fields(vec![IrField::builder()
                            .ident(IrIdent::from("message"))
                            .ty(IrType::String)
                            .build()])
                        .build(),
                ),
                IrNode::Table(
                    IrTable::builder()
                        .ident(IrDottedIdent::from("B"))
                        .fields(vec![IrField::builder()
                            .ident(IrIdent::from("message"))
                            .ty(IrType::Array(Box::new(IrType::UByte)))
                            .build()])
                        .build(),
                ),
                IrNode::Enum(
                    IrEnum::builder()
                        .ident(IrDottedIdent::from("AorBType"))
                        .base_type(IrEnumBaseType::UByte)
                        .values(vec![
                            IrEnumVal::builder()
                                .ident(IrIdent::from("None"))
                                .value(Some(0))
                                .build(),
                            IrEnumVal::builder().ident(IrIdent::from("A")).build(),
                            IrEnumVal::builder().ident(IrIdent::from("B")).build(),
                        ])
                        .build(),
                ),
                IrNode::Union(
                    IrUnion::builder()
                        .ident(IrDottedIdent::from("AorB"))
                        .enum_ident(IrDottedIdent::from("AorBType"))
                        .variants(vec![
                            IrUnionVariant::builder()
                                .ident(IrIdent::from("A"))
                                .ty(IrType::Custom(
                                    IrCustomTypeRef::builder()
                                        .ident(IrDottedIdent::from("A"))
                                        .ty(IrCustomType::Table)
                                        .build(),
                                ))
                                .build(),
                            IrUnionVariant::builder()
                                .ident(IrIdent::from("B"))
                                .ty(IrType::Custom(
                                    IrCustomTypeRef::builder()
                                        .ident(IrDottedIdent::from("B"))
                                        .ty(IrCustomType::Table)
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
        let actual = IrBuilder::build(schema).unwrap();
        let expected = IrRoot {
            nodes: vec![
                IrNode::Table(
                    IrTable::builder()
                        .ident(IrDottedIdent::from("A"))
                        .fields(vec![IrField::builder()
                            .ident(IrIdent::from("message"))
                            .ty(IrType::String)
                            .build()])
                        .build(),
                ),
                IrNode::Table(
                    IrTable::builder()
                        .ident(IrDottedIdent::from("B"))
                        .fields(vec![IrField::builder()
                            .ident(IrIdent::from("message"))
                            .ty(IrType::Array(Box::new(IrType::UByte)))
                            .build()])
                        .build(),
                ),
                IrNode::Enum(
                    IrEnum::builder()
                        .ident(IrDottedIdent::from("AorBType"))
                        .base_type(IrEnumBaseType::UByte)
                        .values(vec![
                            IrEnumVal::builder()
                                .ident(IrIdent::from("None"))
                                .value(Some(0))
                                .build(),
                            IrEnumVal::builder().ident(IrIdent::from("A")).build(),
                            IrEnumVal::builder().ident(IrIdent::from("B")).build(),
                        ])
                        .build(),
                ),
                IrNode::Union(
                    IrUnion::builder()
                        .ident(IrDottedIdent::from("AorB"))
                        .enum_ident(IrDottedIdent::from("AorBType"))
                        .variants(vec![
                            IrUnionVariant::builder()
                                .ident(IrIdent::from("A"))
                                .ty(IrType::Custom(
                                    IrCustomTypeRef::builder()
                                        .ident(IrDottedIdent::from("A"))
                                        .ty(IrCustomType::Table)
                                        .build(),
                                ))
                                .build(),
                            IrUnionVariant::builder()
                                .ident(IrIdent::from("B"))
                                .ty(IrType::Custom(
                                    IrCustomTypeRef::builder()
                                        .ident(IrDottedIdent::from("B"))
                                        .ty(IrCustomType::Table)
                                        .build(),
                                ))
                                .build(),
                        ])
                        .build(),
                ),
                IrNode::Table(
                    IrTable::builder()
                        .ident(IrDottedIdent::from("Z"))
                        .fields(vec![
                            // Synthetic union enum type!
                            IrField::builder()
                                .ident(IrIdent::from("a_or_b_type"))
                                .ty(IrType::Custom(
                                    IrCustomTypeRef::builder()
                                        .ident(IrDottedIdent::from("AorBType"))
                                        .ty(IrCustomType::Enum {
                                            values: vec![
                                                IrEnumVal::builder()
                                                    .ident(IrIdent::from("None"))
                                                    .value(Some(0))
                                                    .build(),
                                                IrEnumVal::builder()
                                                    .ident(IrIdent::from("A"))
                                                    .build(),
                                                IrEnumVal::builder()
                                                    .ident(IrIdent::from("B"))
                                                    .build(),
                                            ],
                                            base_type: IrEnumBaseType::UByte,
                                        })
                                        .build(),
                                ))
                                .build(),
                            IrField::builder()
                                .ident(IrIdent::from("a_or_b"))
                                .ty(IrType::Custom(
                                    IrCustomTypeRef::builder()
                                        .ident(IrDottedIdent::from("AorB"))
                                        .ty(IrCustomType::Union {
                                            enum_ident: IrDottedIdent::from("AorBType"),
                                            variants: vec![
                                                IrUnionVariant {
                                                    ty: IrType::Custom(
                                                        IrCustomTypeRef::builder()
                                                            .ident(IrDottedIdent::from("A"))
                                                            .ty(IrCustomType::Table)
                                                            .build(),
                                                    ),
                                                    ident: IrIdent::from("A"),
                                                },
                                                IrUnionVariant {
                                                    ty: IrType::Custom(
                                                        IrCustomTypeRef::builder()
                                                            .ident(IrDottedIdent::from("B"))
                                                            .ty(IrCustomType::Table)
                                                            .build(),
                                                    ),
                                                    ident: IrIdent::from("B"),
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
}
