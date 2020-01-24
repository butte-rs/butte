//! IR types
//!
//! Intermediate representations for `butte-build` where types
//! are semantically organized (e.g by namespace) and resolved
//! (so that type dependencies are known)
//!
//! IR types can also implement `ToTokens` because they contain
//! all the required information to be transformed into code.

use crate::parse::types::*;
use derive_more::{AsRef, From};
use std::{borrow::Cow, fmt::Display};
use typed_builder::TypedBuilder;

/// A single IR node.
#[derive(Debug, Clone, PartialEq)]
pub enum IrNode<'a> {
    Namespace(IrNamespace<'a>),
    Table(IrTable<'a>),
    Struct(IrStruct<'a>),
    Enum(IrEnum<'a>),
    Union(IrUnion<'a>),
    Rpc(IrRpc<'a>),
    RootType(IrRootType<'a>),
}

impl<'a> IrNode<'a> {
    pub fn ident(&self) -> &IrDottedIdent<'a> {
        match self {
            IrNode::Namespace(ns) => &ns.ident,
            IrNode::Table(t) => &t.ident,
            IrNode::Struct(s) => &s.ident,
            IrNode::Enum(e) => &e.ident,
            IrNode::Union(u) => &u.ident,
            IrNode::Rpc(r) => &r.ident,
            IrNode::RootType(..) => unimplemented!(),
        }
    }

    pub fn namespace(&self) -> Option<IrDottedIdent<'a>> {
        self.ident().namespace()
    }

    pub fn is_namespace(&self) -> bool {
        self.as_namespace().is_some()
    }

    pub fn as_namespace(&self) -> Option<&IrNamespace<'a>> {
        match self {
            IrNode::Namespace(ns) => Some(ns),
            _ => None,
        }
    }

    pub fn as_namespace_mut(&mut self) -> Option<&mut IrNamespace<'a>> {
        match self {
            IrNode::Namespace(ns) => Some(ns),
            _ => None,
        }
    }

    pub fn into_namespace(self) -> Option<IrNamespace<'a>> {
        match self {
            IrNode::Namespace(ns) => Some(ns),
            _ => None,
        }
    }
}

/// The root of the intermediate representation
///
/// *Not* to be confused with a flatbuffers `root_type`
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct IrRoot<'a> {
    /// Top-level IR nodes
    pub nodes: Vec<IrNode<'a>>,
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct IrNamespace<'a> {
    // The namespace's fully-qualified name
    pub ident: IrDottedIdent<'a>,

    /// IR namespace nodes
    #[builder(default)]
    pub nodes: Vec<IrNode<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrMetadata<'a> {
    _phantom: &'a (),
}
impl<'a> IrMetadata<'a> {
    pub fn is_nested_flatbuffers(&self) -> bool {
        false
    }
}

impl Default for IrMetadata<'_> {
    fn default() -> Self {
        Self { _phantom: &() }
    }
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct IrTable<'a> {
    /// The table's ident
    pub ident: IrDottedIdent<'a>,
    #[builder(default)]
    pub fields: Vec<IrField<'a>>,
    #[builder(default)]
    pub metadata: IrMetadata<'a>,
    #[builder(default)]
    pub doc: Comment<'a>,
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct IrStruct<'a> {
    /// The table's ident
    pub ident: IrDottedIdent<'a>,
    #[builder(default)]
    pub fields: Vec<IrField<'a>>,
    #[builder(default)]
    pub metadata: IrMetadata<'a>,
    #[builder(default)]
    pub doc: Comment<'a>,
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct IrField<'a> {
    /// The fields's ident
    pub ident: IrIdent<'a>,
    pub ty: IrType<'a>,
    #[builder(default)]
    pub default_value: Option<DefaultValue<'a>>,
    #[builder(default)]
    pub metadata: IrMetadata<'a>,
    #[builder(default)]
    pub doc: Comment<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrType<'a> {
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
    Array(Box<IrType<'a>>),
    Custom(IrCustomTypeRef<'a>),
}

impl<'a> IrType<'a> {
    pub fn is_scalar(&self) -> bool {
        match self {
            // Apparently there are fixed sized arrays
            // which are considered scalar if their contents
            // is scalar

            // However they're not currently supported by butte's parser
            IrType::String | IrType::Array(..) => false,
            IrType::Custom(IrCustomTypeRef { ty, .. }) => match ty {
                // Apparently only C++ impl supports union in structs
                // TODO check if unions containing only structs are considered scalar?
                IrCustomType::Table | IrCustomType::Union { .. } => false,
                IrCustomType::Struct { .. } | IrCustomType::Enum { .. } => true,
            },
            _ => true,
        }
    }

    pub fn is_union(&self) -> bool {
        match self {
            IrType::Custom(IrCustomTypeRef { ty, .. }) => match ty {
                IrCustomType::Union { .. } => true,
                _ => false,
            },
            _ => false,
        }
    }

    // For unions, we generate an enum type "companion" that's just a simple
    // ubyte based enum with all the union variants, + None (default 0)
    //
    // This method lets us generate the companion type name from the union.
    pub fn make_union_enum_type_companion(&self) -> Option<&IrDottedIdent<'a>> {
        match self {
            IrType::Custom(IrCustomTypeRef { ty, .. }) => match ty {
                IrCustomType::Union { ref enum_ident, .. } => Some(enum_ident),
                _ => None,
            },
            _ => None,
        }
    }
}

impl Display for IrType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrType::Bool => write!(f, "bool"),
            IrType::Byte => write!(f, "i8"),
            IrType::UByte => write!(f, "u8"),
            IrType::Short => write!(f, "i16"),
            IrType::UShort => write!(f, "u16"),
            IrType::Int => write!(f, "i32"),
            IrType::UInt => write!(f, "u32"),
            IrType::Float => write!(f, "f32"),
            IrType::Long => write!(f, "i64"),
            IrType::ULong => write!(f, "u64"),
            IrType::Double => write!(f, "f64"),
            IrType::Int8 => write!(f, "i8"),
            IrType::UInt8 => write!(f, "u8"),
            IrType::Int16 => write!(f, "i16"),
            IrType::UInt16 => write!(f, "u16"),
            IrType::Int32 => write!(f, "i32"),
            IrType::UInt32 => write!(f, "u32"),
            IrType::Int64 => write!(f, "i64"),
            IrType::UInt64 => write!(f, "u64"),
            IrType::Float32 => write!(f, "f32"),
            IrType::Float64 => write!(f, "f64"),
            IrType::String => write!(f, "&str"),
            IrType::Array(component) => write!(f, "[{}]", component),
            IrType::Custom(IrCustomTypeRef { ident, .. }) => write!(f, "{}", ident),
        }
    }
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct IrCustomTypeRef<'a> {
    pub ident: IrDottedIdent<'a>,
    pub ty: IrCustomType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrCustomType<'a> {
    Table,
    Struct {
        fields: Vec<IrField<'a>>,
    },
    Enum {
        values: Vec<IrEnumVal<'a>>,
        base_type: IrEnumBaseType,
    },
    Union {
        enum_ident: IrDottedIdent<'a>,
        variants: Vec<IrUnionVariant<'a>>,
    },
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct IrEnum<'a> {
    pub ident: IrDottedIdent<'a>,
    pub values: Vec<IrEnumVal<'a>>,
    pub base_type: IrEnumBaseType,

    #[builder(default)]
    pub metadata: IrMetadata<'a>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IrEnumBaseType {
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

impl<'a> std::convert::TryFrom<&Type<'a>> for IrEnumBaseType {
    type Error = ();
    fn try_from(ty: &Type<'a>) -> Result<Self, Self::Error> {
        Ok(match ty {
            Type::Byte => IrEnumBaseType::Byte,
            Type::UByte => IrEnumBaseType::UByte,
            Type::Short => IrEnumBaseType::Short,
            Type::UShort => IrEnumBaseType::UShort,
            Type::Int => IrEnumBaseType::Int,
            Type::UInt => IrEnumBaseType::UInt,
            Type::Long => IrEnumBaseType::Long,
            Type::ULong => IrEnumBaseType::ULong,
            Type::Int8 => IrEnumBaseType::Int8,
            Type::UInt8 => IrEnumBaseType::UInt8,
            Type::Int16 => IrEnumBaseType::Int16,
            Type::UInt16 => IrEnumBaseType::UInt16,
            Type::Int32 => IrEnumBaseType::Int32,
            Type::UInt32 => IrEnumBaseType::UInt32,
            Type::Int64 => IrEnumBaseType::Int64,
            Type::UInt64 => IrEnumBaseType::UInt64,
            _ => return Err(()),
        })
    }
}

#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct IrUnion<'a> {
    pub ident: IrDottedIdent<'a>,

    // Ident of the enum "companion"
    pub enum_ident: IrDottedIdent<'a>,

    pub variants: Vec<IrUnionVariant<'a>>,

    #[builder(default)]
    pub metadata: IrMetadata<'a>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// An RPC service.
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct IrRpc<'a> {
    pub ident: IrDottedIdent<'a>,
    pub methods: Vec<IrRpcMethod<'a>>,

    #[builder(default)]
    pub doc: Comment<'a>,
}

/// A method in an RPC service.
#[derive(Debug, Clone, PartialEq, TypedBuilder)]
pub struct IrRpcMethod<'a> {
    /// The name of the method.
    pub ident: IrIdent<'a>,

    /// The request type of the method.
    pub request_type: IrDottedIdent<'a>,

    /// The response type of the method.
    pub response_type: IrDottedIdent<'a>,

    /// Method metadata.
    #[builder(default)]
    pub metadata: IrRpcMethodMetadata,

    #[builder(default)]
    pub doc: Comment<'a>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, TypedBuilder)]
pub struct IrRpcMethodMetadata {
    pub streaming: IrRpcStreaming,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum IrRpcStreaming {
    None,
    Client,
    Server,
    Bidi,
}

impl Default for IrRpcStreaming {
    fn default() -> Self {
        IrRpcStreaming::None
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrRootType<'a> {
    _phantom: &'a (),
}

/// Type for `Enum` values.
#[derive(Debug, Clone, PartialEq, Hash, Eq, From, TypedBuilder)]
pub struct IrEnumVal<'a> {
    /// The name of the enum value.
    pub ident: IrIdent<'a>,

    /// An optional enum value.
    #[builder(default)]
    pub value: Option<IntegerConstant>,
}

/// Type for `Union` variants.
#[derive(Debug, Clone, PartialEq, From, TypedBuilder)]
pub struct IrUnionVariant<'a> {
    /// The type of the variant
    pub ty: IrType<'a>,
    /// The ident of the variant, matching the companion enum's
    pub ident: IrIdent<'a>,
}

/// An identifier
///
/// Can be either taken from a schema, or generated synthetically,
/// in which case an owned version of the ident is used.
#[derive(Debug, Clone, PartialEq, Hash, Eq, AsRef, From)]
pub struct IrIdent<'a> {
    pub raw: Cow<'a, str>,
}

impl<'a> From<Ident<'a>> for IrIdent<'a> {
    fn from(ident: Ident<'a>) -> Self {
        Self {
            raw: Cow::Borrowed(ident.raw),
        }
    }
}

impl<'a> From<&Ident<'a>> for IrIdent<'a> {
    fn from(ident: &Ident<'a>) -> Self {
        Self {
            raw: Cow::Borrowed(ident.raw),
        }
    }
}
impl<'a> From<&'a str> for IrIdent<'a> {
    fn from(s: &'a str) -> Self {
        Self {
            raw: Cow::Borrowed(s),
        }
    }
}

impl<'a> From<String> for IrIdent<'a> {
    fn from(s: String) -> Self {
        Self { raw: Cow::Owned(s) }
    }
}

impl<'a> Display for IrIdent<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.raw.as_ref())
    }
}

/// An identifier composed of `IrIdent`s separated by dots.
#[derive(Default, Debug, Clone, PartialEq, Hash, Eq, From)]
pub struct IrDottedIdent<'a> {
    pub parts: Vec<IrIdent<'a>>,
}

impl<'a> IrDottedIdent<'a> {
    pub fn namespace(&self) -> Option<IrDottedIdent<'a>> {
        if self.parts.len() > 1 {
            Some(Self {
                parts: self.parts[..self.parts.len() - 1].to_vec(),
            })
        } else {
            None
        }
    }

    pub fn simple(&self) -> &IrIdent<'a> {
        &self.parts[self.parts.len() - 1]
    }

    // Assumes correctly formed ident
    pub fn parse_str(s: &'a str) -> Self {
        IrDottedIdent {
            parts: s.split('.').map(IrIdent::from).collect(),
        }
    }
}

impl<'a> From<&'a str> for IrDottedIdent<'a> {
    fn from(s: &'a str) -> Self {
        Self {
            parts: vec![s.into()],
        }
    }
}

impl<'a> From<DottedIdent<'a>> for IrDottedIdent<'a> {
    fn from(ident: DottedIdent<'a>) -> Self {
        Self {
            parts: ident.parts.into_iter().map(Into::into).collect(),
        }
    }
}

impl<'a> From<&DottedIdent<'a>> for IrDottedIdent<'a> {
    fn from(ident: &DottedIdent<'a>) -> Self {
        Self {
            parts: ident.parts.iter().map(Into::into).collect(),
        }
    }
}

impl<'a> Display for IrDottedIdent<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use itertools::Itertools;
        write!(f, "{}", self.parts.iter().join("."))
    }
}
