//! Types representing the parts of a flatbuffer schema
use derive_more::From;
use std::{collections::HashMap, iter::FromIterator, path::Path};
use typed_builder::TypedBuilder;

/// A Flatbuffer schema
#[derive(Debug, Clone, PartialEq, From, TypedBuilder)]
pub struct Schema<'a> {
    /// Included flatbuffer files
    #[builder(default)]
    pub includes: Vec<Include<'a>>,

    /// The body of the schema file
    #[builder(default)]
    pub elements: Vec<Element<'a>>,
}

/// A single include
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, From, TypedBuilder)]
pub struct Include<'a> {
    pub path: &'a Path,
    pub stem: &'a str,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// A single schema file element
#[derive(Debug, Clone, PartialEq, From)]
pub enum Element<'a> {
    Namespace(Namespace<'a>),
    Table(Table<'a>),
    Struct(Struct<'a>),
    Enum(Enum<'a>),
    Union(Union<'a>),
    Root(Root<'a>),
    FileExtension(FileExtension<'a>),
    FileIdentifier(FileIdentifier<'a>),
    Attribute(Attribute<'a>),
    Rpc(Rpc<'a>),
    Object(Object<'a>),
}

/// The root type of the schema file; there can be only one
#[derive(Debug, Clone, PartialEq, From, TypedBuilder)]
pub struct Root<'a> {
    pub typename: Ident<'a>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// The extension to use when creating flatbuffers binary files
#[derive(Debug, Clone, PartialEq, Hash, Eq, From, TypedBuilder)]
pub struct FileExtension<'a> {
    pub ext: &'a str,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// A magic number for using flatbuffers as a file format
#[derive(Debug, Clone, PartialEq, Hash, Eq, From, TypedBuilder)]
pub struct FileIdentifier<'a> {
    pub id: [char; 4],

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// The namespace in which the schema body resides
#[derive(Debug, Clone, PartialEq, Eq, From, TypedBuilder)]
pub struct Namespace<'a> {
    pub parts: Vec<Ident<'a>>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// Declares an attribute to be used as metadata wherever metadata is valid
#[derive(Debug, Clone, PartialEq, Hash, Eq, From, TypedBuilder)]
pub struct Attribute<'a> {
    pub attr: Ident<'a>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// Struct type
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Struct<'a> {
    pub id: Ident<'a>,
    pub fields: Vec<Field<'a>>, // one or more

    #[builder(default)]
    pub metadata: Option<Metadata<'a>>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// Table type
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Table<'a> {
    pub id: Ident<'a>,
    pub fields: Vec<Field<'a>>, // one or more

    #[builder(default)]
    pub metadata: Option<Metadata<'a>>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// Type representing enums
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Enum<'a> {
    pub id: Ident<'a>,
    pub values: Vec<EnumVal<'a>>,
    pub base_type: Type<'a>,

    #[builder(default)]
    pub metadata: Option<Metadata<'a>>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// Type representing unions
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Union<'a> {
    pub id: Ident<'a>,
    pub values: Vec<EnumVal<'a>>,

    #[builder(default)]
    pub metadata: Option<Metadata<'a>>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// A field of a struct or table
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Field<'a> {
    pub id: Ident<'a>,
    pub ty: Type<'a>,

    #[builder(default)]
    pub scalar: Option<Scalar>,

    #[builder(default)]
    pub metadata: Option<Metadata<'a>>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// An RPC service
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Rpc<'a> {
    pub id: Ident<'a>,
    pub methods: Vec<RpcMethod<'a>>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// A method in an RPC service
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct RpcMethod<'a> {
    pub id: Ident<'a>,
    pub request_type: Ident<'a>,
    pub response_type: Ident<'a>,

    #[builder(default)]
    pub metadata: Option<Metadata<'a>>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// Flatbuffer scalar, array types and user-defined types
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

impl Type<'_> {
    pub fn is_scalar(&self) -> bool {
        // If it's a string, array type, or type name (UDT) it's not a scalar.
        // Otherwise it is.
        match self {
            Type::String | Type::Array(_) | Type::Ident(_) => false,
            _ => true,
        }
    }
}

impl<'a> From<[Type<'a>; 1]> for Type<'a> {
    fn from(array: [Type<'a>; 1]) -> Self {
        Type::Array(Box::new(array[0].clone()))
    }
}

/// Integer constant type
pub type IntegerConstant = i64;

/// Floating point constant type
pub type FloatingConstant = f64;

/// Boolean constant type
pub type BooleanConstant = bool;

/// Type for enum/union values
#[derive(Debug, Clone, PartialEq, Hash, Eq, From, TypedBuilder)]
pub struct EnumVal<'a> {
    pub id: Ident<'a>,

    #[builder(default)]
    pub value: Option<IntegerConstant>,
}

impl<'a> From<Ident<'a>> for EnumVal<'a> {
    fn from(id: Ident<'a>) -> Self {
        Self::builder().id(id).build()
    }
}

/// Key-value pair metadata
#[derive(Debug, Clone, PartialEq, From, TypedBuilder)]
pub struct Metadata<'a> {
    #[builder(default)]
    pub values: HashMap<Ident<'a>, Option<Single<'a>>>,
}

impl<'a> From<Vec<(Ident<'a>, Option<Single<'a>>)>> for Metadata<'a> {
    fn from(values: Vec<(Ident<'a>, Option<Single<'a>>)>) -> Self {
        Self::builder().values(HashMap::from_iter(values)).build()
    }
}

/// Integer, float, or boolean constants
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, From)]
pub enum Scalar {
    Integer(IntegerConstant),
    Float(FloatingConstant),
    Boolean(BooleanConstant),
}

/// JSON blob-like
#[derive(Debug, Clone, PartialEq, From, TypedBuilder)]
pub struct Object<'a> {
    #[builder(default)]
    pub values: HashMap<Ident<'a>, Value<'a>>,
}

impl<'a> From<Vec<(Ident<'a>, Value<'a>)>> for Object<'a> {
    fn from(values: Vec<(Ident<'a>, Value<'a>)>) -> Self {
        Self::builder().values(HashMap::from_iter(values)).build()
    }
}

/// A `Scalar` or string literal
#[derive(Debug, Clone, PartialEq, PartialOrd, From)]
pub enum Single<'a> {
    Scalar(Scalar),
    StringConstant(&'a str),
}

impl From<IntegerConstant> for Single<'_> {
    fn from(value: IntegerConstant) -> Self {
        Self::from(Scalar::from(value))
    }
}

impl From<FloatingConstant> for Single<'_> {
    fn from(value: FloatingConstant) -> Self {
        Self::from(Scalar::from(value))
    }
}

impl From<BooleanConstant> for Single<'_> {
    fn from(value: BooleanConstant) -> Self {
        Self::from(Scalar::from(value))
    }
}
/// Strings, integers, bools, objects, and lists thereof.
#[derive(Debug, Clone, PartialEq, From)]
pub enum Value<'a> {
    Single(Single<'a>),
    Object(Object<'a>),
    List(Vec<Value<'a>>),
}

impl<'a> From<Vec<(Ident<'a>, Value<'a>)>> for Value<'a> {
    fn from(values: Vec<(Ident<'a>, Value<'a>)>) -> Self {
        Value::Object(Object::from(values))
    }
}

/// An identifier
#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq, From, TypedBuilder)]
pub struct Ident<'a> {
    pub raw: &'a str,
}

// TODO: Use derive_more::AsRef when 0.15.1 is released
impl AsRef<str> for Ident<'_> {
    fn as_ref(&self) -> &str {
        self.raw
    }
}

/// A documentation comment
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Default, From, TypedBuilder)]
pub struct Comment<'a> {
    #[builder(default)]
    pub lines: Vec<&'a str>,
}

/// The root type of the file, there can be only one. This is different from
/// the Root type resulting from a parse. This type is an enum that contains
/// the actual type object.
#[derive(Debug, Clone, PartialEq, From)]
pub enum RootType<'a> {
    Table(Table<'a>),
    Struct(Struct<'a>),
}

/// A file
#[derive(Debug, Clone, PartialEq, From, TypedBuilder)]
pub struct File<'a> {
    pub schema: Schema<'a>,

    pub path: &'a Path,

    #[builder(default)]
    pub root_type: Option<RootType<'a>>,

    #[builder(default)]
    pub file_identifier: Option<FileIdentifier<'a>>,

    #[builder(default)]
    pub file_extension: Option<FileExtension<'a>>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_scalar() {
        assert!(Type::Float32.is_scalar());
        assert!(Type::UInt16.is_scalar());
        assert!(!Type::String.is_scalar());
        assert!(!Type::Ident("foobar".into()).is_scalar());
        assert!(!Type::Array(Box::new(Type::Byte)).is_scalar());
    }

    #[test]
    fn test_enum_val_from_ident() {
        let result: EnumVal = Ident::from("foo").into();
        let expected = EnumVal::builder().id(Ident::from("foo")).build();
        assert_eq!(result, expected);
    }
}
