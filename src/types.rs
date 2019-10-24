//! The types representing the parts of a flatbuffer schema

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Schema {
    pub(crate) includes: Vec<Include>, // zero or more
    pub(crate) body: Vec<Element>,     // zero or more
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Include(pub(crate) StringConstant);

#[derive(Debug, Clone, PartialEq)]
pub enum Element {
    Namespace(Namespace),
    ProductType(ProductType), // type_decl in the grammar
    Enum(Enum),
    Root(Root),
    FileExtension(FileExtension),
    FileIdentifier(FileIdentifier),
    Attribute(Attribute),
    Rpc(Rpc),
    Object(Object),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Root(pub(crate) Ident);

pub type FileExtension = String;
pub type FileIdentifier = String;

#[derive(Debug, Clone, PartialEq)]
pub struct Namespace(pub(crate) Vec<Ident>);

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Attribute(pub(crate) Ident);

#[derive(Debug, Clone, PartialEq)]
pub struct ProductType {
    pub(crate) kind: ProductKind,
    pub(crate) name: Ident,
    pub(crate) fields: Vec<Field>, // one or more
    pub(crate) metadata: Option<Metadata>,
}

#[derive(Debug, Copy, Clone, PartialEq, Hash)]
pub enum ProductKind {
    Table,
    Struct,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub(crate) kind: EnumKind,
    pub(crate) metadata: Option<Metadata>,
    pub(crate) values: Vec<EnumVal>, // zero or more?
    pub(crate) ident: Ident,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum EnumKind {
    Enum(Type),
    Union,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub(crate) name: Ident,
    pub(crate) ty: Type,
    pub(crate) scalar: Option<Scalar>,
    pub(crate) metadata: Option<Metadata>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Rpc {
    pub(crate) name: Ident,
    pub(crate) methods: Vec<RpcMethod>, // one or more
}

#[derive(Debug, Clone, PartialEq)]
pub struct RpcMethod {
    pub(crate) name: Ident,
    pub(crate) request_type: Ident,
    pub(crate) response_type: Ident,
    pub(crate) metadata: Option<Metadata>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Type {
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
    Array(Box<Type>),
    Ident(Ident),
}

pub type IntegerConstant = i64;
pub type FloatingConstant = f64;
pub type StringConstant = String;

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct EnumVal {
    pub(crate) name: Ident,
    pub(crate) value: Option<IntegerConstant>,
}

pub type Metadata = HashMap<Ident, Option<SingleValue>>;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Scalar {
    Integer(IntegerConstant),
    Float(FloatingConstant),
}

pub type Object = HashMap<Ident, Value>;

#[derive(Debug, Clone, PartialEq)]
pub enum SingleValue {
    Scalar(Scalar),
    StringConstant(StringConstant),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    SingleValue(SingleValue),
    Object(Object),
    Values(Vec<Value>),
}

pub type Ident = String;

#[cfg(test)]
mod tests {}
