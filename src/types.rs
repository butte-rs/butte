//! The types representing the parts of a flatbuffer schema

use std::collections::HashMap;
use typed_builder::TypedBuilder;

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Schema<'a> {
    #[builder(default)]
    pub(crate) includes: Vec<Include<'a>>,
    #[builder(default)]
    pub(crate) elements: Vec<Element<'a>>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Include<'a>(pub(crate) &'a str);

#[derive(Debug, Clone, PartialEq)]
pub enum Element<'a> {
    Namespace(Namespace<'a>),
    ProductType(ProductType<'a>), // type_decl in the grammar
    Enum(Enum<'a>),
    Root(Root<'a>),
    FileExtension(FileExtension<'a>),
    FileIdentifier(FileIdentifier<'a>),
    Attribute(Attribute<'a>),
    Rpc(Rpc<'a>),
    Object(Object<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Root<'a>(pub(crate) Ident<'a>);

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub struct FileExtension<'a>(pub(crate) &'a str);

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub struct FileIdentifier<'a>(pub(crate) &'a str);

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Namespace<'a>(pub(crate) Vec<Ident<'a>>);

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Attribute<'a>(pub(crate) Ident<'a>);

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct ProductType<'a> {
    pub(crate) kind: ProductKind,
    pub(crate) name: Ident<'a>,
    pub(crate) fields: Vec<Field<'a>>, // one or more

    #[builder(default)]
    pub(crate) metadata: Option<Metadata<'a>>,
}

pub fn table<'a>(name: Ident<'a>, fields: Vec<Field<'a>>) -> ProductType<'a> {
    ProductType::builder()
        .kind(ProductKind::Table)
        .name(name)
        .fields(fields)
        .build()
}

pub fn struct_<'a>(name: Ident<'a>, fields: Vec<Field<'a>>) -> ProductType<'a> {
    ProductType::builder()
        .kind(ProductKind::Struct)
        .name(name)
        .fields(fields)
        .build()
}

#[derive(Debug, Copy, Clone, PartialEq, Hash)]
pub enum ProductKind {
    Table,
    Struct,
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Enum<'a> {
    pub(crate) kind: EnumKind<'a>,
    pub(crate) name: Ident<'a>,

    pub(crate) values: Vec<EnumVal<'a>>,

    #[builder(default)]
    pub(crate) metadata: Option<Metadata<'a>>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum EnumKind<'a> {
    Enum(Type<'a>),
    Union,
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Field<'a> {
    pub(crate) name: Ident<'a>,
    pub(crate) ty: Type<'a>,

    #[builder(default)]
    pub(crate) scalar: Option<Scalar>,

    #[builder(default)]
    pub(crate) metadata: Option<Metadata<'a>>,
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Rpc<'a> {
    pub(crate) name: Ident<'a>,
    pub(crate) methods: Vec<RpcMethod<'a>>,
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct RpcMethod<'a> {
    pub(crate) name: Ident<'a>,
    pub(crate) request_type: Ident<'a>,
    pub(crate) response_type: Ident<'a>,

    #[builder(default)]
    pub(crate) metadata: Option<Metadata<'a>>,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum Type<'a> {
    Bool,
    Byte,
    UByte,
    Short,
    UShort,
    Int,
    UInt,
    Float,
    Long,
    ULong,
    Double,
    Int8,
    UInt8,
    Int16,
    UInt16,
    Int32,
    UInt32,
    Int64,
    UInt64,
    Float32,
    Float64,
    String,
    Array(Box<Type<'a>>),
    Ident(Ident<'a>),
}

pub type IntegerConstant = i64;
pub type FloatingConstant = f64;
pub type BooleanConstant = bool;

#[derive(Debug, Clone, PartialEq, Hash, Eq, TypedBuilder)]
pub struct EnumVal<'a> {
    pub(crate) name: Ident<'a>,

    #[builder(default)]
    pub(crate) value: Option<IntegerConstant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Metadata<'a>(pub(crate) HashMap<Ident<'a>, Option<SingleValue<'a>>>);

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Scalar {
    Integer(IntegerConstant),
    Float(FloatingConstant),
    Boolean(BooleanConstant),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Object<'a>(pub(crate) HashMap<Ident<'a>, Value<'a>>);

#[derive(Debug, Clone, PartialEq)]
pub enum SingleValue<'a> {
    Scalar(Scalar),
    StringConstant(&'a str),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    SingleValue(SingleValue<'a>),
    Object(Object<'a>),
    List(Vec<Value<'a>>),
}

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub struct Ident<'a>(pub(crate) &'a str);
