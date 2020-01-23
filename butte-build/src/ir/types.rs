//! IR types
//!
//! Intermediate representations for `butte-build` where types
//! are semantically organized (e.g by namespace) and resolved
//! (so that type dependencies are known)
//!
//! IR types can also implement `ToTokens` because they contain
//! all the required information to be transformed into code.

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
}

impl<'a> Node<'a> {
    pub fn ident(&self) -> &DottedIdent<'a> {
        match self {
            Node::Namespace(ns) => &ns.ident,
            Node::Table(t) => &t.ident,
            Node::Struct(s) => &s.ident,
            Node::Enum(e) => &e.ident,
            Node::Union(u) => &u.ident,
        }
    }

    pub fn namespace(&self) -> Option<DottedIdent<'a>> {
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

/// The root of the intermediate representation
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
    pub ident: DottedIdent<'a>,

    /// IR namespace nodes
    #[builder(default)]
    pub nodes: Vec<Node<'a>>,
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Table<'a> {
    /// The table's ident
    pub ident: DottedIdent<'a>,
    #[builder(default)]
    pub fields: Vec<Field<'a>>,
    #[builder(default)]
    pub doc: ast::Comment<'a>,
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Struct<'a> {
    /// The table's ident
    pub ident: DottedIdent<'a>,
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
    pub doc: ast::Comment<'a>,
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
        match self {
            Type::Custom(CustomTypeRef { ty, .. }) => match ty {
                CustomType::Union { .. } => true,
                _ => false,
            },
            _ => false,
        }
    }

    // For unions, we generate an enum type "companion" that's just a simple
    // ubyte based enum with all the union variants, + None (default 0)
    //
    // This method lets us generate the companion type name from the union.
    pub fn make_union_enum_type_companion(&self) -> Option<&DottedIdent<'a>> {
        match self {
            Type::Custom(CustomTypeRef { ty, .. }) => match ty {
                CustomType::Union { ref enum_ident, .. } => Some(enum_ident),
                _ => None,
            },
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
    pub ident: DottedIdent<'a>,
    pub ty: CustomType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CustomType<'a> {
    Table,
    Struct {
        fields: Vec<Field<'a>>,
    },
    Enum {
        values: Vec<EnumVal<'a>>,
        base_type: EnumBaseType,
    },
    Union {
        enum_ident: DottedIdent<'a>,
        variants: Vec<UnionVariant<'a>>,
    },
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct Enum<'a> {
    pub ident: DottedIdent<'a>,
    pub values: Vec<EnumVal<'a>>,
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
    pub ident: DottedIdent<'a>,
    // Ident of the enum "companion"
    pub enum_ident: DottedIdent<'a>,

    pub variants: Vec<UnionVariant<'a>>,

    #[builder(default)]
    pub doc: ast::Comment<'a>,
}

/// Type for `Enum` values.
#[derive(Debug, Clone, PartialEq, Hash, Eq, From, TypedBuilder)]
pub struct EnumVal<'a> {
    /// The name of the enum value.
    pub ident: Ident<'a>,

    /// An optional enum value.
    #[builder(default)]
    pub value: Option<ast::IntegerConstant>,
}

/// Type for `Union` variants.
#[derive(Debug, Clone, PartialEq, From, TypedBuilder)]
pub struct UnionVariant<'a> {
    /// The type of the variant
    pub ty: Type<'a>,
    /// The ident of the variant, matching the companion enum's
    pub ident: Ident<'a>,
}

/// An identifier
///
/// Can be either taken from a schema, or generated synthetically,
/// in which case an owned version of the ident is used.
#[derive(Debug, Clone, PartialEq, Hash, Eq, AsRef, From)]
pub struct Ident<'a> {
    pub raw: Cow<'a, str>,
}

impl<'a> From<ast::Ident<'a>> for Ident<'a> {
    fn from(ident: ast::Ident<'a>) -> Self {
        Self {
            raw: Cow::Borrowed(ident.raw),
        }
    }
}

impl<'a> From<&ast::Ident<'a>> for Ident<'a> {
    fn from(ident: &ast::Ident<'a>) -> Self {
        Self {
            raw: Cow::Borrowed(ident.raw),
        }
    }
}
impl<'a> From<&'a str> for Ident<'a> {
    fn from(s: &'a str) -> Self {
        Self {
            raw: Cow::Borrowed(s),
        }
    }
}

impl<'a> From<String> for Ident<'a> {
    fn from(s: String) -> Self {
        Self { raw: Cow::Owned(s) }
    }
}

impl<'a> Display for Ident<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.raw.as_ref())
    }
}

/// An identifier composed of `Ident`s separated by dots.
#[derive(Default, Debug, Clone, PartialEq, Hash, Eq, From)]
pub struct DottedIdent<'a> {
    pub parts: Vec<Ident<'a>>,
}

impl<'a> DottedIdent<'a> {
    pub fn namespace(&self) -> Option<DottedIdent<'a>> {
        if self.parts.len() > 1 {
            Some(Self {
                parts: self.parts[..self.parts.len() - 1].to_vec(),
            })
        } else {
            None
        }
    }

    pub fn simple(&self) -> &Ident<'a> {
        &self.parts[self.parts.len() - 1]
    }

    // Assumes correctly formed ident
    pub fn parse_str(s: &'a str) -> Self {
        DottedIdent {
            parts: s.split('.').map(Ident::from).collect(),
        }
    }
}

impl<'a> From<&'a str> for DottedIdent<'a> {
    fn from(s: &'a str) -> Self {
        Self {
            parts: vec![s.into()],
        }
    }
}

impl<'a> From<ast::DottedIdent<'a>> for DottedIdent<'a> {
    fn from(ident: ast::DottedIdent<'a>) -> Self {
        Self {
            parts: ident.parts.into_iter().map(Into::into).collect(),
        }
    }
}

impl<'a> From<&ast::DottedIdent<'a>> for DottedIdent<'a> {
    fn from(ident: &ast::DottedIdent<'a>) -> Self {
        Self {
            parts: ident.parts.iter().map(Into::into).collect(),
        }
    }
}

impl<'a> Display for DottedIdent<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use itertools::Itertools;
        write!(f, "{}", self.parts.iter().join("."))
    }
}
