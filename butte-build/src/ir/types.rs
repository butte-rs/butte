//! IR types.
//!
//! Intermediate representations for `butte-build` where types are semantically organized (e.g by
//! namespace) and resolved so that type dependencies are known.
//!
//! IR types can also implement `ToTokens` because they contain all the required information to be
//! transformed into code.

use crate::ast::types as ast;
use derive_more::{AsRef, From};
use std::{borrow::Cow, fmt::Display};
use typed_builder::TypedBuilder;

/// A single IR node.
#[derive(Debug, Clone, PartialEq)]
pub enum Node<'a> {
    Namespace(Namespace<'a>),
    Table(Table<'a>),
    Struct(Struct<'a>),
    Enum(Enum<'a>),
    Union(Union<'a>),
    Rpc(Rpc<'a>),
}

impl<'a> Node<'a> {
    pub fn ident(&self) -> &QualifiedIdent<'a> {
        match self {
            Node::Namespace(ns) => &ns.ident,
            Node::Table(t) => &t.ident,
            Node::Struct(s) => &s.ident,
            Node::Enum(e) => &e.ident,
            Node::Union(u) => &u.ident,
            Node::Rpc(r) => &r.ident,
        }
    }

    pub fn namespace(&self) -> Option<QualifiedIdent<'a>> {
        self.ident().namespace()
    }

    pub fn is_namespace(&self) -> bool {
        self.as_namespace().is_some()
    }

    pub fn as_namespace(&self) -> Option<&Namespace<'a>> {
        match self {
            Node::Namespace(ns) => Some(ns),
            _ => None,
        }
    }

    pub fn as_namespace_mut(&mut self) -> Option<&mut Namespace<'a>> {
        match self {
            Node::Namespace(ns) => Some(ns),
            _ => None,
        }
    }

    pub fn into_namespace(self) -> Option<Namespace<'a>> {
        match self {
            Node::Namespace(ns) => Some(ns),
            _ => None,
        }
    }
}

/// The root of the intermediate representation.
///
/// *Not* to be confused with a flatbuffers `root_type`
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Root<'a> {
    /// Top-level IR nodes
    pub nodes: Vec<Node<'a>>,
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Namespace<'a> {
    // The namespace's fully-qualified name
    pub ident: QualifiedIdent<'a>,

    /// IR namespace nodes
    #[builder(default)]
    pub nodes: Vec<Node<'a>>,
}

impl Namespace<'_> {
    pub fn depth(&self) -> usize {
        self.nodes.len()
    }
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Table<'a> {
    /// The table's ident
    pub ident: QualifiedIdent<'a>,
    #[builder(default)]
    pub fields: Vec<Field<'a>>,
    #[builder(default)]
    pub root_type: bool,
    #[builder(default)]
    pub doc: ast::Comment<'a>,
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Struct<'a> {
    /// The table's ident
    pub ident: QualifiedIdent<'a>,
    #[builder(default)]
    pub fields: Vec<Field<'a>>,

    #[builder(default)]
    pub doc: ast::Comment<'a>,
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Field<'a> {
    /// The fields's ident
    pub ident: Ident<'a>,
    pub ty: Type<'a>,
    #[builder(default)]
    pub default_value: Option<ast::DefaultValue<'a>>,
    #[builder(default)]
    pub metadata: FieldMetadata,

    #[builder(default)]
    pub doc: ast::Comment<'a>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, TypedBuilder)]
pub struct FieldMetadata {
    #[builder(default)]
    pub required: bool,
}

#[derive(Debug, Clone, PartialEq)]
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
    Custom(CustomTypeRef<'a>),
}

impl<'a> Type<'a> {
    pub fn is_scalar(&self) -> bool {
        match self {
            // Apparently there are fixed sized arrays
            // which are considered scalar if their contents
            // is scalar

            // However they're not currently supported by butte's parser
            Type::String | Type::Array(..) => false,
            Type::Custom(CustomTypeRef { ty, .. }) => match ty {
                // Apparently only C++ impl supports union in structs
                // TODO check if unions containing only structs are considered scalar?
                CustomType::Table | CustomType::Union { .. } => false,
                CustomType::Struct { .. } | CustomType::Enum { .. } => true,
            },
            _ => true,
        }
    }

    pub fn is_union(&self) -> bool {
        matches!(
            self,
            Type::Custom(CustomTypeRef {
                ty: CustomType::Union { .. },
                ..
            })
        )
    }

    pub fn is_enum(&self) -> bool {
        matches!(
            self,
            Type::Custom(CustomTypeRef {
                ty: CustomType::Enum { .. },
                ..
            })
        )
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(..))
    }

    // Is it a complex type that will trigger Clippy warnings
    pub fn is_complex(&self) -> bool {
        self.is_array()
    }

    // For unions, we generate an enum type "companion" that's just a simple
    // ubyte based enum with all the union variants, + None (default 0)
    //
    // This method lets us generate the companion type name from the union.
    pub fn make_union_enum_type_companion(&self) -> Option<&QualifiedIdent<'a>> {
        match self {
            Type::Custom(CustomTypeRef {
                ty: CustomType::Union { ref enum_ident, .. },
                ..
            }) => Some(enum_ident),
            _ => None,
        }
    }
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bool => write!(f, "bool"),
            Type::Byte => write!(f, "i8"),
            Type::UByte => write!(f, "u8"),
            Type::Short => write!(f, "i16"),
            Type::UShort => write!(f, "u16"),
            Type::Int => write!(f, "i32"),
            Type::UInt => write!(f, "u32"),
            Type::Float => write!(f, "f32"),
            Type::Long => write!(f, "i64"),
            Type::ULong => write!(f, "u64"),
            Type::Double => write!(f, "f64"),
            Type::Int8 => write!(f, "i8"),
            Type::UInt8 => write!(f, "u8"),
            Type::Int16 => write!(f, "i16"),
            Type::UInt16 => write!(f, "u16"),
            Type::Int32 => write!(f, "i32"),
            Type::UInt32 => write!(f, "u32"),
            Type::Int64 => write!(f, "i64"),
            Type::UInt64 => write!(f, "u64"),
            Type::Float32 => write!(f, "f32"),
            Type::Float64 => write!(f, "f64"),
            Type::String => write!(f, "&str"),
            Type::Array(component) => write!(f, "[{}]", component),
            Type::Custom(CustomTypeRef { ident, .. }) => write!(f, "{}", ident),
        }
    }
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct CustomTypeRef<'a> {
    pub ident: QualifiedIdent<'a>,
    pub ty: CustomType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CustomType<'a> {
    Table,
    Struct {
        fields: Vec<Field<'a>>,
    },
    Enum {
        variants: Vec<EnumVariant<'a>>,
        base_type: EnumBaseType,
    },
    Union {
        enum_ident: QualifiedIdent<'a>,
        variants: Vec<UnionVariant<'a>>,
    },
}

impl<'a> CustomType<'a> {
    pub fn is_scalar(&self) -> bool {
        matches!(self, CustomType::Struct { .. } | CustomType::Enum { .. })
    }
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Enum<'a> {
    pub ident: QualifiedIdent<'a>,
    pub variants: Vec<EnumVariant<'a>>,
    pub base_type: EnumBaseType,

    #[builder(default)]
    pub doc: ast::Comment<'a>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum EnumBaseType {
    Byte,
    UByte,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    Int8,
    UInt8,
    Int16,
    UInt16,
    Int32,
    UInt32,
    Int64,
    UInt64,
}

impl<'a> std::convert::TryFrom<&ast::Type<'a>> for EnumBaseType {
    type Error = ();
    fn try_from(ty: &ast::Type<'a>) -> Result<Self, Self::Error> {
        Ok(match ty {
            ast::Type::Byte => EnumBaseType::Byte,
            ast::Type::UByte => EnumBaseType::UByte,
            ast::Type::Short => EnumBaseType::Short,
            ast::Type::UShort => EnumBaseType::UShort,
            ast::Type::Int => EnumBaseType::Int,
            ast::Type::UInt => EnumBaseType::UInt,
            ast::Type::Long => EnumBaseType::Long,
            ast::Type::ULong => EnumBaseType::ULong,
            ast::Type::Int8 => EnumBaseType::Int8,
            ast::Type::UInt8 => EnumBaseType::UInt8,
            ast::Type::Int16 => EnumBaseType::Int16,
            ast::Type::UInt16 => EnumBaseType::UInt16,
            ast::Type::Int32 => EnumBaseType::Int32,
            ast::Type::UInt32 => EnumBaseType::UInt32,
            ast::Type::Int64 => EnumBaseType::Int64,
            ast::Type::UInt64 => EnumBaseType::UInt64,
            _ => return Err(()),
        })
    }
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Union<'a> {
    pub ident: QualifiedIdent<'a>,
    // Ident of the enum "companion"
    pub enum_ident: QualifiedIdent<'a>,
    pub variants: Vec<UnionVariant<'a>>,

    #[builder(default)]
    pub doc: ast::Comment<'a>,
}

/// An RPC service.
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Rpc<'a> {
    pub ident: QualifiedIdent<'a>,
    pub methods: Vec<RpcMethod<'a>>,

    #[builder(default)]
    pub doc: ast::Comment<'a>,
}

/// A method in an RPC service.
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct RpcMethod<'a> {
    /// The name of the method.
    pub ident: Ident<'a>,

    /// The snake name of the method.
    pub snake_ident: Ident<'a>,

    /// The request type of the method.
    pub request_type: QualifiedIdent<'a>,

    /// The response type of the method.
    pub response_type: QualifiedIdent<'a>,

    /// Method metadata.
    #[builder(default)]
    pub metadata: RpcMethodMetadata,

    #[builder(default)]
    pub doc: ast::Comment<'a>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, TypedBuilder)]
pub struct RpcMethodMetadata {
    pub streaming: RpcStreaming,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RpcStreaming {
    None,
    Client,
    Server,
    Bidi,
}

impl Default for RpcStreaming {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RootType<'a> {
    _phantom: &'a (),
}

/// Type for `Enum` values.
#[derive(Debug, Clone, PartialEq, Hash, Eq, From, TypedBuilder)]
pub struct EnumVariant<'a> {
    /// The name of the enum value.
    pub ident: Ident<'a>,

    /// An optional enum value.
    #[builder(default)]
    pub value: Option<ast::IntegerConstant>,

    #[builder(default)]
    pub doc: ast::Comment<'a>,
}

/// Type for `Union` variants.
#[derive(Debug, Clone, PartialEq, From, TypedBuilder)]
pub struct UnionVariant<'a> {
    /// The type of the variant
    pub ty: Type<'a>,
    /// The ident of the variant, matching the companion enum's
    pub ident: Ident<'a>,

    #[builder(default)]
    pub doc: ast::Comment<'a>,
}

/// An identifier
///
/// Can be either taken from a schema, or generated synthetically,
/// in which case an owned version of the ident is used.
#[derive(Debug, Clone, PartialEq, Hash, Eq, PartialOrd, Ord, AsRef)]
pub struct Ident<'a> {
    pub raw: Cow<'a, str>,
}

impl<'a> Ident<'a> {
    // Utility function to fix identifiers that may be invalid,
    // e.g that contain Rust keywords and would generate invalid code
    pub fn fix_identifier(ident: Cow<'_, str>) -> Cow<'_, str> {
        if let Some(substitute) = super::keywords::substitute_keyword(ident.as_ref()) {
            Cow::Borrowed(substitute)
        } else {
            ident
        }
    }

    // Purposefully create an ident named after a keyword
    // e.g to use `crate` or `super` for a type path
    fn keyword(keyword: &str) -> Ident<'_> {
        Ident {
            raw: Cow::Borrowed(keyword),
        }
    }
}

impl<'a> From<ast::Ident<'a>> for Ident<'a> {
    fn from(ident: ast::Ident<'a>) -> Self {
        Ident::from(Cow::Borrowed(ident.raw))
    }
}

impl<'a> From<&ast::Ident<'a>> for Ident<'a> {
    fn from(ident: &ast::Ident<'a>) -> Self {
        Ident::from(Cow::Borrowed(ident.raw))
    }
}
impl<'a> From<&'a str> for Ident<'a> {
    fn from(s: &'a str) -> Self {
        Ident::from(Cow::Borrowed(s))
    }
}

impl<'a> From<String> for Ident<'a> {
    fn from(s: String) -> Self {
        Ident::from(Cow::Owned(s))
    }
}

impl<'a> From<Cow<'a, str>> for Ident<'a> {
    fn from(raw: Cow<'a, str>) -> Self {
        let raw = Ident::fix_identifier(raw);
        Self { raw }
    }
}

impl<'a> Display for Ident<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.raw.as_ref())
    }
}

/// An identifier composed of `Ident`s separated by dots.
#[derive(Default, Debug, Clone, PartialEq, Hash, Eq, PartialOrd, Ord, From)]
pub struct QualifiedIdent<'a> {
    pub parts: Vec<Ident<'a>>,
}

// Clippy wants `is_empty` if there's `len` but it makes no sense here since we never have an
// empty `QualifiedIdent`.
#[allow(clippy::len_without_is_empty)]
impl<'a> QualifiedIdent<'a> {
    pub fn len(&self) -> usize {
        self.parts.len()
    }

    pub fn namespace(&self) -> Option<QualifiedIdent<'a>> {
        if self.parts.len() > 1 {
            Some(Self {
                parts: self.parts[..self.parts.len() - 1].to_vec(),
            })
        } else {
            None
        }
    }

    pub fn relative(&self, namespace: Option<&QualifiedIdent<'a>>) -> QualifiedIdent<'a> {
        if let Some(ns) = namespace {
            let common_prefix_len = self
                .parts
                .iter()
                .zip(ns.parts.iter())
                .take_while(|(a, b)| a == b)
                .count();

            let parts = std::iter::repeat(Ident::keyword("super"))
                .take(ns.len() - common_prefix_len)
                .chain(self.parts[common_prefix_len..].to_vec())
                .collect();
            QualifiedIdent { parts }
        } else {
            self.clone()
        }
    }

    pub fn simple(&self) -> &Ident<'a> {
        &self.parts[self.parts.len() - 1]
    }

    // Assumes correctly formed ident.
    // Keywords are allowed
    // Use only for tests
    #[cfg(test)]
    pub fn parse_str(s: &'a str) -> Self {
        QualifiedIdent {
            parts: s
                .split('.')
                .map(|p| Ident {
                    raw: Cow::Borrowed(p),
                })
                .collect(),
        }
    }
}

impl<'a> From<&'a str> for QualifiedIdent<'a> {
    fn from(s: &'a str) -> Self {
        Self {
            parts: vec![s.into()],
        }
    }
}

impl<'a> From<ast::QualifiedIdent<'a>> for QualifiedIdent<'a> {
    fn from(ident: ast::QualifiedIdent<'a>) -> Self {
        Self {
            parts: ident.parts.into_iter().map(Into::into).collect(),
        }
    }
}

impl<'a> From<&ast::QualifiedIdent<'a>> for QualifiedIdent<'a> {
    fn from(ident: &ast::QualifiedIdent<'a>) -> Self {
        Self {
            parts: ident.parts.iter().map(Into::into).collect(),
        }
    }
}

impl<'a> Display for QualifiedIdent<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use itertools::Itertools;
        write!(f, "{}", self.parts.iter().join("."))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_relative_ident_from_same_namespace() {
        let id = QualifiedIdent::parse_str("foo.bar.Baz");
        let ns = id.namespace(); // ns = "foo.bar"
        let expected = QualifiedIdent::parse_str("Baz");
        let actual = id.relative(ns.as_ref());

        assert_eq!(expected, actual);
    }

    #[test]
    pub fn test_relative_ident_from_root_namespace() {
        let id = QualifiedIdent::parse_str("foo.bar.Baz");
        let ns = None; // "root" namespace
        let expected = id.clone(); // absolute id is unchanged
        let actual = id.relative(ns.as_ref());

        assert_eq!(expected, actual);
    }

    #[test]
    pub fn test_relative_ident_from_parent_namespace() {
        let id = QualifiedIdent::parse_str("foo.bar.Baz");
        let ns = Some(QualifiedIdent::parse_str("foo"));
        let expected = QualifiedIdent::parse_str("bar.Baz");
        let actual = id.relative(ns.as_ref());

        assert_eq!(expected, actual);
    }

    #[test]
    pub fn test_relative_ident_from_child_namespace() {
        let id = QualifiedIdent::parse_str("foo.bar.Baz");
        let ns = Some(QualifiedIdent::parse_str("foo.bar.bim"));
        let expected = QualifiedIdent::parse_str("super.Baz");
        let actual = id.relative(ns.as_ref());

        assert_eq!(expected, actual);
    }

    #[test]
    pub fn test_relative_ident_from_sibling_namespace() {
        let id = QualifiedIdent::parse_str("foo.bar.bam.Baz");
        let ns = Some(QualifiedIdent::parse_str("foo.bar.bim"));
        let expected = QualifiedIdent::parse_str("super.bam.Baz");
        let actual = id.relative(ns.as_ref());

        assert_eq!(expected, actual);
    }
}
