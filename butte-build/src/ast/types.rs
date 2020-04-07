//! Types representing the parts of a flatbuffer schema
use derive_more::{AsRef, From};
use std::{collections::HashMap, iter::FromIterator, path::Path};
use typed_builder::TypedBuilder;

/// A Flatbuffer schema.
#[derive(Debug, Clone, PartialEq, From, TypedBuilder)]
pub struct Schema<'a> {
    /// A collection of included flatbuffer files.
    #[builder(default)]
    pub includes: Vec<Include<'a>>,

    /// A collection of `Element`s that make up the body of the schema.
    #[builder(default)]
    pub elements: Vec<Element<'a>>,
}

/// A single include.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, From, TypedBuilder)]
pub struct Include<'a> {
    /// The path to the included file.
    pub path: &'a Path,

    /// The file stem of `path`.
    pub stem: &'a str,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// A single schema element.
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

impl Element<'_> {
    /// Check whether an element is a namespace.
    pub fn is_namespace(&self) -> bool {
        self.namespace().is_some()
    }

    /// Return the underlying `Namespace` object from the element if `self` is a `Namespace`.
    pub fn namespace(&self) -> Option<&Namespace<'_>> {
        match self {
            Element::Namespace(ns) => Some(ns),
            _ => None,
        }
    }
}

#[cfg(test)]
mod element_impl_tests {
    use super::*;
    use crate::namespace;

    #[test]
    fn test_is_namespace_with_namespace() {
        let ns = Element::from(namespace!(a::b::c));
        assert!(ns.is_namespace());
    }

    #[test]
    fn test_namespace_with_namespace() {
        let ns = Element::from(namespace!(foo::bar));
        assert_eq!(
            ns.namespace(),
            Some(&Namespace::from((
                vec!["foo".into(), "bar".into()].into(),
                Comment::builder().build(),
            )))
        );
    }
}

/// The root type of the schema file.
#[derive(Debug, Clone, PartialEq, From, TypedBuilder)]
pub struct Root<'a> {
    pub typename: Ident<'a>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// The extension to use when creating flatbuffers binary files.
#[derive(Debug, Clone, PartialEq, Hash, Eq, From, TypedBuilder)]
pub struct FileExtension<'a> {
    pub ext: &'a str,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// A magic number for using flatbuffers as a file format.
#[derive(Debug, Clone, PartialEq, Hash, Eq, From, TypedBuilder)]
pub struct FileIdentifier<'a> {
    pub id: [u8; 4],

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// A namespace in which one or more schema elements resides.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, TypedBuilder)]
pub struct Namespace<'a> {
    pub ident: QualifiedIdent<'a>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// Declares an attribute to be used as metadata wherever metadata is valid.
#[derive(Debug, Clone, PartialEq, Hash, Eq, From, TypedBuilder)]
pub struct Attribute<'a> {
    pub attr: Ident<'a>,
}

/// Struct type. Structs are product types where fields are always required.
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Struct<'a> {
    pub id: Ident<'a>,
    pub fields: Vec<Field<'a>>,

    #[builder(default)]
    pub metadata: Option<Metadata<'a>>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// Table type. Tables are product types where fields are optional unless indicated otherwise.
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Table<'a> {
    pub id: Ident<'a>,
    pub fields: Vec<Field<'a>>, // one or more

    #[builder(default)]
    pub metadata: Option<Metadata<'a>>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// Enum type.
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Enum<'a> {
    pub id: Ident<'a>,
    pub variants: Vec<EnumVariant<'a>>,
    pub base_type: Type<'a>,

    #[builder(default)]
    pub metadata: Option<Metadata<'a>>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// Union type.
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Union<'a> {
    pub id: Ident<'a>,
    pub variants: Vec<EnumVariant<'a>>,

    #[builder(default)]
    pub metadata: Option<Metadata<'a>>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// A field of a `Struct` or `Table`.
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Field<'a> {
    pub id: Ident<'a>,
    pub ty: Type<'a>,

    #[builder(default)]
    pub default_value: Option<DefaultValue<'a>>,

    #[builder(default)]
    pub metadata: Option<Metadata<'a>>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// An RPC service.
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Rpc<'a> {
    pub id: Ident<'a>,
    pub methods: Vec<RpcMethod<'a>>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// A method in an RPC service.
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct RpcMethod<'a> {
    /// The name of the method.
    pub id: Ident<'a>,

    /// The request type of the method.
    pub request_type: QualifiedIdent<'a>,

    /// The response type of the method.
    pub response_type: QualifiedIdent<'a>,

    /// Method metadata.
    #[builder(default)]
    pub metadata: Option<Metadata<'a>>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// Scalar, array, and user-defined types.
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
    Ident(QualifiedIdent<'a>),
}

impl Type<'_> {
    /// Check whether a `Type` is scalar.
    pub fn is_scalar(&self) -> bool {
        // If it's a string or array type it's not a scalar.
        //
        // TODO: enums are scalars, tables are not -- what about structs?
        // we might need more context to determine if the type is a scalar
        // to catch errors early
        match self {
            Type::String | Type::Array(_) => false,
            _ => true,
        }
    }
}

impl<'a> From<[Type<'a>; 1]> for Type<'a> {
    /// Convert an array of size 1 to a `Type::Array`.
    fn from(array: [Type<'a>; 1]) -> Self {
        Self::Array(Box::new(array[0].clone()))
    }
}

pub type IntegerConstant = i128;

/// Floating point constant type.
pub type FloatingConstant = f64;

/// Boolean constant type.
pub type BooleanConstant = bool;

/// Type for `Enum`/`Union` values.
#[derive(Debug, Clone, PartialEq, Hash, Eq, From, TypedBuilder)]
pub struct EnumVariant<'a> {
    /// The name of the enum value.
    pub id: Ident<'a>,

    /// An optional enum value.
    #[builder(default)]
    pub value: Option<IntegerConstant>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// Key-value pair metadata.
#[derive(Debug, Clone, PartialEq, From, TypedBuilder)]
pub struct Metadata<'a> {
    #[builder(default)]
    pub values: HashMap<Ident<'a>, Option<Single<'a>>>,
}

impl<'a> From<Vec<(Ident<'a>, Option<Single<'a>>)>> for Metadata<'a> {
    /// Convert a `Vec` of `Ident`/`Value` pairs to a `Value`.
    fn from(values: Vec<(Ident<'a>, Option<Single<'a>>)>) -> Self {
        Self::builder().values(HashMap::from_iter(values)).build()
    }
}

/// Integer, float, or boolean constants.
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, From)]
pub enum Scalar {
    Integer(IntegerConstant),
    Float(FloatingConstant),
    Boolean(BooleanConstant),
}

impl From<u64> for Scalar {
    fn from(value: u64) -> Self {
        Self::from(IntegerConstant::from(value))
    }
}

impl From<i32> for Scalar {
    fn from(value: i32) -> Self {
        Self::from(IntegerConstant::from(value))
    }
}

/// JSON-like values.
#[derive(Debug, Clone, PartialEq, From, TypedBuilder)]
pub struct Object<'a> {
    #[builder(default)]
    pub values: HashMap<Ident<'a>, Value<'a>>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

impl<'a> From<Vec<(Ident<'a>, Value<'a>)>> for Object<'a> {
    /// Convert a `Vec` of `Ident`/`Value` pairs to a `Value`.
    fn from(values: Vec<(Ident<'a>, Value<'a>)>) -> Self {
        Self::builder().values(HashMap::from_iter(values)).build()
    }
}

/// Default field value: A `Scalar` or an `Ident` referring to an enum variant
/// on the current field enum type
#[derive(Debug, Clone, PartialEq, From)]
pub enum DefaultValue<'a> {
    Scalar(Scalar),
    Ident(Ident<'a>),
}

impl From<IntegerConstant> for DefaultValue<'_> {
    fn from(value: IntegerConstant) -> Self {
        Scalar::from(value).into()
    }
}

impl From<u64> for DefaultValue<'_> {
    fn from(value: u64) -> Self {
        Scalar::from(IntegerConstant::from(value)).into()
    }
}

impl From<i32> for DefaultValue<'_> {
    fn from(value: i32) -> Self {
        Scalar::from(IntegerConstant::from(value)).into()
    }
}

impl From<FloatingConstant> for DefaultValue<'_> {
    fn from(value: FloatingConstant) -> Self {
        Scalar::from(value).into()
    }
}

impl From<BooleanConstant> for DefaultValue<'_> {
    fn from(value: BooleanConstant) -> Self {
        Scalar::from(value).into()
    }
}

impl<'a> From<&'a str> for DefaultValue<'a> {
    fn from(value: &'a str) -> Self {
        Ident::from(value).into()
    }
}

/// A `Scalar` or string literal
#[derive(Debug, Clone, PartialEq, PartialOrd, From)]
pub enum Single<'a> {
    Scalar(Scalar),
    String(&'a str),
}

impl From<IntegerConstant> for Single<'_> {
    fn from(value: IntegerConstant) -> Self {
        Scalar::from(value).into()
    }
}

impl From<u64> for Single<'_> {
    fn from(value: u64) -> Self {
        Scalar::from(IntegerConstant::from(value)).into()
    }
}

impl From<i32> for Single<'_> {
    fn from(value: i32) -> Self {
        Scalar::from(IntegerConstant::from(value)).into()
    }
}

impl From<FloatingConstant> for Single<'_> {
    fn from(value: FloatingConstant) -> Self {
        Scalar::from(value).into()
    }
}

impl From<BooleanConstant> for Single<'_> {
    fn from(value: BooleanConstant) -> Self {
        Scalar::from(value).into()
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
        Object::from(values).into()
    }
}

/// An identifier
#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq, From, AsRef, TypedBuilder)]
pub struct Ident<'a> {
    pub raw: &'a str,
}

/// An identifier composed of `Ident`s separated by dots.
#[derive(Debug, Clone, PartialEq, Hash, Eq, From, TypedBuilder)]
pub struct QualifiedIdent<'a> {
    pub parts: Vec<Ident<'a>>,
}

/// A documentation comment.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Default, From, TypedBuilder)]
pub struct Comment<'a> {
    #[builder(default)]
    pub lines: Vec<&'a str>,
}

/// The root type of the file. This type is different from the [`Root`](crate::types::Root) type resulting from a parse.
/// This type is an enum that contains the actual type object.
#[derive(Debug, Clone, PartialEq, From)]
pub enum RootType<'a> {
    Table(Table<'a>),
    Struct(Struct<'a>),
}

/// A file containing a flatbuffer schema.
#[derive(Debug, Clone, PartialEq, From, TypedBuilder)]
pub struct File<'a> {
    /// The flatbuffer schema.
    pub schema: Schema<'a>,

    /// The path to the file.
    pub path: &'a Path,

    /// A list of root types declared in the file.
    #[builder(default)]
    pub root_type: Vec<RootType<'a>>,

    /// An optional file identifier. See [`FileIdentifier`](crate::types::Identifier).
    #[builder(default)]
    pub file_identifier: Option<FileIdentifier<'a>>,

    /// An optional file extension. See [`FileExtension`](crate::types::FileExtension).
    #[builder(default)]
    pub file_extension: Option<FileExtension<'a>>,
}

#[cfg(test)]
mod type_tests {
    use super::*;

    #[test]
    fn test_is_scalar() {
        assert!(Type::Float32.is_scalar());
        assert!(Type::UInt16.is_scalar());
        assert!(!Type::String.is_scalar());
        assert!(!Type::Array(Box::new(Type::Byte)).is_scalar());
    }
}
