//! The types representing the parts of a flatbuffer schema

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Schema {
    Includes(Vec<Include>), // zero or more
    Body(Vec<Body>),        // zero or more
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Include(pub(crate) StringConstant);

#[derive(Debug, Clone, PartialEq)]
pub enum Body {
    Namespace(Namespace),
    ProductType(ProductType), // type_decl in the grammar
    Enum(Enum),
    Root(Ident),
    FileExtension(StringConstant),
    FileIdentifier(StringConstant),
    Attribute(Attribute),
    Rpc(Rpc),
    Object(Object),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Namespace(Vec<Ident>);

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Attribute(Ident);

#[derive(Debug, Clone, PartialEq)]
pub struct ProductType {
    kind: ProductKind,
    name: Ident,
    fields: Vec<Field>, // one or more
    metadata: Metadata,
}

#[derive(Debug, Copy, Clone, PartialEq, Hash)]
pub enum ProductKind {
    Table,
    Struct,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    kind: EnumKind,
    metadata: Metadata,
    values: Vec<EnumVal>, // zero or more?
    ident: Ident,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum EnumKind {
    Enum(Type),
    Union,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    name: Ident,
    ty: Type,
    scalar: Option<Scalar>,
    metadata: Metadata,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Rpc {
    name: Ident,
    methods: Vec<RpcMethod>, // one or more
}

#[derive(Debug, Clone, PartialEq)]
pub struct RpcMethod {
    name: Ident,
    request_type: Ident,
    response_type: Ident,
    metadata: Metadata,
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
    Array(Vec<Box<Type>>),
    Ident(Ident),
}

pub type IntegerConstant = i64;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct FloatingConstant(f64);

pub type StringConstant = String;

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct EnumVal {
    name: Ident,
    value: Option<IntegerConstant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Metadata(HashMap<Ident, Option<SingleValue>>);

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Scalar {
    Integer(IntegerConstant),
    Float(FloatingConstant),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Object(HashMap<Ident, Value>);

#[derive(Debug, Clone, PartialEq)]
pub enum SingleValue {
    Scalar(Scalar),
    StringConstant(StringConstant),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    SingleValue(SingleValue),
    Object(Object),
    Values(Vec<Box<Value>>),
}

pub type Ident = String;

#[cfg(test)]
mod tests {}
