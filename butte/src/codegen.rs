use crate::types::*;

#[cfg(test)]
use crate::{field, table};

use flatbuffers;
use heck::SnakeCase;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use std::convert::TryInto;
use syn::spanned::Spanned;

pub trait AsArgType: ToTokens {
    fn as_arg_type(&self, lifetime: Option<impl ToTokens>) -> TokenStream;
}

impl AsArgType for Type<'_> {
    fn as_arg_type(&self, lifetime: Option<impl ToTokens>) -> TokenStream {
        match self {
            Type::String => quote!(flatbuffers::WIPOffset<&#lifetime str>),
            _ => quote!(#self),
        }
    }
}

#[cfg(test)]
fn to_code(value: impl ToTokens) -> String {
    format!("{}", value.to_token_stream())
}

#[cfg(test)]
mod constant_tests {
    use super::*;

    #[test]
    fn test_visit_floating_constant() {
        let result = to_code(1.0);
        let expected = "1f64";
        assert_eq!(result, expected);
    }

    #[test]
    fn test_visit_integer_constant() {
        let result = to_code(1);
        let expected = "1i32";
        assert_eq!(result, expected);
    }

    #[test]
    fn test_visit_string_constant() {
        let result = to_code("abc");
        let expected = "\"abc\"";
        assert_eq!(result, expected);
    }

    #[test]
    fn test_visit_bool_constant() {
        let result = to_code(true);
        let expected = "true";
        assert_eq!(result, expected);

        let result = to_code(false);
        let expected = "false";
        assert_eq!(result, expected);
    }
}

impl ToTokens for Ident<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        format_ident!("{}", self.raw).to_tokens(tokens)
    }
}

#[cfg(test)]
mod ident_tests {
    use super::*;

    #[test]
    fn test_visit_ident() {
        let result = to_code(Ident::from("foo"));
        let expected = "foo";
        assert_eq!(result, expected);
    }
}

impl ToTokens for Scalar {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Scalar::Integer(i) => i.to_tokens(tokens),
            Scalar::Float(f) => f.to_tokens(tokens),
            Scalar::Boolean(b) => b.to_tokens(tokens),
        }
    }
}

fn raw_offset_name(name: impl AsRef<str>) -> String {
    format!("VT_{}", name.as_ref().to_snake_case().to_uppercase())
}

fn offset_name(name: impl AsRef<str>) -> impl ToTokens {
    format_ident!("{}", raw_offset_name(name))
}

fn to_arg_type(ty: &Type, lifetime: impl ToTokens) -> impl ToTokens {
    match ty {
        Type::String => quote!(flatbuffers::WIPOffset<&#lifetime str>),
        _ => quote!(#ty),
    }
}

const FIXED_FIELDS: flatbuffers::VOffsetT = 2; // Vtable size and Object Size.
const SIZE_OF_OFFSET_TYPE: flatbuffers::VOffsetT =
    std::mem::size_of::<flatbuffers::VOffsetT>() as flatbuffers::VOffsetT;

/// Convert a Field ID to a virtual table offset. Ported from the flatc C++ code.
fn field_index_to_offset(field_index: usize) -> flatbuffers::VOffsetT {
    let field_index_value: flatbuffers::VOffsetT = field_index.try_into().unwrap();
    field_index_value + FIXED_FIELDS * SIZE_OF_OFFSET_TYPE
}

impl ToTokens for Table<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            id: struct_id,
            fields,
            doc,
            ..
        } = self;

        let raw_struct_name = struct_id.raw;

        let builder_add_calls = fields.iter().map(|Field { id: field_id, .. }| {
            let raw_field_name = field_id.raw;
            let add_field_method = format_ident!("add_{}", raw_field_name);
            quote!(builder.#add_field_method(args.#field_id);)
        });

        let args = format_ident!("{}Args", raw_struct_name);
        let args_fields = fields.iter().map(
            |Field {
                 id: field_id,
                 ty,
                 scalar,
                 ..
             }| {
                let arg_ty = ty.as_arg_type(Some(quote!('a)));
                // Scalar fields can have a default value
                let default = if let Some(default_value) = scalar {
                    quote!(#[default = #default_value])
                } else {
                    quote!()
                };
                quote! {
                    #default
                    pub #field_id: #arg_ty
                }
            },
        );

        let builder_type = format_ident!("{}Builder", struct_id.raw);

        // TODO: field accessors
        // TODO: check the impl of offset generation
        // TODO: unions
        // TODO: testing this is going to be fun
        let field_nested_flatbuffers = fields.iter().filter_map(|Field { id: field_id, metadata, .. }| {
            let method_name = format_ident!("{}_nested_flatbuffer", field_id.raw);
            if let Some(metadata) = metadata {
                if metadata.values.contains_key(&Ident::from("nested_flatbuffer")) {
                    Some(quote! {
                        pub fn #method_name(&self) -> Option<Self<'a>> {
                            self.#field_id.map(|data| <flatbuffers::ForwardsUOffset<Self<'a>>>::follow(data, 0))
                        }
                    })
                } else {
                    None
                }
            } else {
                None
            }
        });

        let builder_field_methods = fields.iter().map(|field| {
            let Field {
                id: field_id,
                ty,
                scalar,
                ..
            } = field;
            let add_method_name = format_ident!("add_{}", field_id.raw);
            let offset = offset_name(field_id);
            let field_offset = quote!(#struct_id::#offset);
            let arg_ty = to_arg_type(ty, quote!('_));
            let body = if ty.is_scalar() {
                if let Some(default_value) = scalar {
                    quote!(self.fbb.push_slot<#arg_ty>(#field_offset, #field_id, #default_value))
                } else {
                    quote!(self.fbb.push_slot_always::<#arg_ty>(#field_offset, #field_id))
                }
            } else {
                quote!(self.fbb.push_slot_always::<#arg_ty>(#field_offset, #field_id))
            };
            let arg_ty = to_arg_type(ty, quote!('b));
            quote! {
                #[inline]
                fn #add_method_name(&mut self, #field_id: #arg_ty) {
                    #body;
                }
            }
        });

        let field_offset_constants =
            fields
                .iter()
                .enumerate()
                .map(|(index, Field { id: field_id, .. })| {
                    let name = offset_name(field_id);
                    let offset_value = field_index_to_offset(index);
                    quote! {
                        pub const #name: flatbuffers::VOffsetT = #offset_value;
                    }
                });
        let struct_offset_enum_name = format_ident!("{}Offset", struct_id.raw);

        let required_fields = fields.iter().map(|Field { id: field_id, .. }| {
            let snake_name = field_id.raw.to_snake_case();
            let offset_name = offset_name(field_id);
            quote! {
                self.fbb.required(o, <#struct_id>::#offset_name, #snake_name);
            }
        });
        (quote! {
            pub enum #struct_offset_enum_name {}

            #[derive(Copy, Clone, Debug, PartialEq, derive_more::From)]
            #doc
            pub struct #struct_id<'a> {
                table: flatbuffers::Table<'a>,
            }

            impl<'a> #struct_id<'a> {
                pub fn create<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
                    fbb: &'mut_bldr mut flatbuffers::FlatBufferBuilder<'bldr>,
                    args: &'args #args<'args>
                ) -> flatbuffers::WIPOffset<#struct_id<'bldr>> {
                    let mut builder = <#builder_type>::new(fbb);
                    #(#builder_add_calls)*
                    builder.finish()
                }

                // field offset constants
                #(#field_offset_constants)*

                // nested flatbuffers if applicable
                #(#field_nested_flatbuffers)*
            }

            impl<'a> flatbuffers::Follow<'a> for #struct_id<'a> {
                type Inner = Self;

                #[inline]
                fn follow(buf: &'a [u8], loc: usize) -> Self::Inner {
                    let table = flatbuffers::Table { buf, loc };
                    Self { table }
                }
            }

            // Builder Args
            // TODO: Can't use this because we can mix fields that are
            // default-able with those that are not
            pub struct #args<'a> {
                #(#args_fields),*
            }

            //// builder
            pub struct #builder_type<'a, 'b> {
                fbb: &'b mut flatbuffers::FlatBufferBuilder<'a>,
                start: flatbuffers::WIPOffset<flatbuffers::TableUnfinishedWIPOffset>,
            }

            impl<'a: 'b, 'b> #builder_type<'a, 'b> {
                #(#builder_field_methods)*

                #[inline]
                pub fn new(fbb: &'b mut flatbuffers::FlatBufferBuilder<'a>) -> Self {
                    let start = fbb.start_table();
                    #builder_type {
                        fbb, start
                    }
                }

                #[inline]
                pub fn finish(self) -> flatbuffers::WIPOffset<#struct_id<'a>> {
                    let o = self.fbb.end_table(self.start);
                    #(#required_fields)*
                    flatbuffers::WIPOffset::new(o.value())
                }
            }
        })
        .to_tokens(tokens)
    }
}

#[cfg(test)]
mod product_type_tests {
    use super::*;

    #[test]
    fn test_visit_product_type_table() {
        let table = table!(
            MyMessage,
            [field!(message, String), field!(foo, Float64 = 2.0)]
        );
        let result = to_code(table);
        assert!(!result.is_empty());
    }
}

impl ToTokens for Type<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Type::Bool => quote!(bool),
            Type::Byte => quote!(i8),
            Type::UByte => quote!(u8),
            Type::Short => quote!(i16),
            Type::UShort => quote!(u16),
            Type::Int => quote!(i32),
            Type::UInt => quote!(u32),
            Type::Float => quote!(f32),
            Type::Long => quote!(i64),
            Type::ULong => quote!(u64),
            Type::Double => quote!(f64),
            Type::Int8 => quote!(i8),
            Type::UInt8 => quote!(u8),
            Type::Int16 => quote!(i16),
            Type::UInt16 => quote!(u16),
            Type::Int32 => quote!(i32),
            Type::UInt32 => quote!(u32),
            Type::Int64 => quote!(i64),
            Type::UInt64 => quote!(u64),
            Type::Float32 => quote!(f32),
            Type::Float64 => quote!(f64),
            Type::String => quote!(String),
            Type::Array(ty) => quote!(Vec<#ty>),
            Type::Ident(id) => quote!(#id),
        }
        .to_tokens(tokens)
    }
}

#[cfg(test)]
mod type_tests {
    use super::*;

    #[test]
    fn test_visit_type() {
        let result = to_code(Type::Bool);
        let expected = "bool";
        assert_eq!(result, expected);

        let result = to_code(Type::Array(Box::new(Type::String)));
        let expected = "Vec < String >";
        assert_eq!(result, expected);

        let result = to_code(Type::Ident(Ident::from("MyType")));
        let expected = "MyType";
        assert_eq!(result, expected);
    }
}

// TODO: Properly implement this.
// We only generate a trait method right now.
// TODO: Figure out how this will integrate into tonic.
impl ToTokens for RpcMethod<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            id,
            request_type,
            response_type,
            doc,
            ..
        } = self;
        let snake_name = format_ident!("{}", id.raw.to_snake_case());
        (quote! {
            #doc
            fn #snake_name(request: #request_type) -> #response_type;
        })
        .to_tokens(tokens)
    }
}

impl ToTokens for Comment {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let doc = if let Self {
            text: Some(comment),
        } = self
        {
            // I deliberatly chose not to fold over a Vec<Comment> to avoid
            // code complexity. The tradeoff is that the generated code has
            // newlines embedded in docstrings :(
            quote! {
                #[doc = #comment]
            }
        } else {
            quote!()
        };
        doc.to_tokens(tokens)
    }
}

// TODO: This is woefully incomplete
impl ToTokens for Rpc<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            id: Ident { raw },
            methods,
            doc,
        } = self;
        let service_name = format_ident!("{}Service", raw);
        (quote! {
            #doc
            pub trait #service_name {
                #(#methods)*
            }
        })
        .to_tokens(tokens)
    }
}

fn lit_int(value: i64, base_type: impl Spanned + std::fmt::Display) -> impl ToTokens {
    let stringified_int = format!("{}_{}", value, base_type);
    syn::LitInt::new(&stringified_int, base_type.span())
}

// TODO: Unions. Though see structs, because those overlap with unions in a way
// that isn't clear to me ATM. It seems that unions can be enum-like or
// struct-like depending on the element types.
impl ToTokens for Enum<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            id: enum_id,
            values,
            base_type,
            doc,
            ..
        } = self;

        // generate enum variant name => string name of the variant for use in
        // a match statement
        let names_to_strings = values.iter().map(|EnumVal { id: key, .. }| {
            let raw_key = key.raw;
            quote! {
                #enum_id::#key => #raw_key
            }
        });

        // assign a value to the key if one was given, otherwise give it the
        // enumerated index's value
        let fields = values
            .iter()
            .enumerate()
            .map(|(i, EnumVal { id: key, value })| {
                // format the value with the correct type, i.e., base_type
                let scalar_value = lit_int(
                    if let Some(constant) = *value {
                        constant
                    } else {
                        i.try_into().expect("invalid conversion to enum base type")
                    },
                    base_type.to_token_stream(),
                );
                quote! {
                    #key = #scalar_value
                }
            });

        let raw_snake_enum_name = enum_id.raw.to_snake_case();
        let enum_id_fn_name = format_ident!("enum_name_{}", raw_snake_enum_name);

        // TODO: Maybe separate these pieces to avoid variables that used far
        // away from their definition.
        (quote! {
            // force a C-style enum
            #[repr(#base_type)]
            #[allow(non_camel_case_types)]
            #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
            #doc
            pub enum #enum_id {
                #(#fields),*
            }

            impl<'a> flatbuffers::Follow<'a> for #enum_id {
                type Inner = Self;

                fn follow(buf: &'a [u8], loc: usize) -> Self::Inner {
                    flatbuffers::read_scalar_at::<Self>(buf, loc)
                }
            }

            impl flatbuffers::EndianScalar for #enum_id {
                #[inline]
                fn to_little_endian(self) -> Self {
                    let n = #base_type::to_le(self as #base_type);
                    let p = &n as *const #base_type as *const Self;
                    unsafe { *p }
                }

                #[inline]
                fn from_little_endian(self) -> Self {
                    let n = #base_type::from_le(self as #base_type);
                    let p = &n as *const #base_type as *const Self;
                    unsafe { *p }
                }
            }

            impl flatbuffers::Push for #enum_id {
                type Output = Self;

                #[inline]
                fn push(&self, dst: &mut [u8], _rest: &[u8]) {
                    flatbuffers::emplace_scalar::<Self>(dst, *self);
                }
            }

            pub fn #enum_id_fn_name(e: #enum_id) -> &'static str {
                match e {
                    #(#names_to_strings),*
                }
            }
        })
        .to_tokens(tokens)
    }
}

// TODO: better error messages for things that aren't implemented
impl ToTokens for Element<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        // the following constructs are (or should be) handled at the file
        // level:
        // * Namespaces
        // * Root types
        // * File extensions
        // * File identifiers
        //
        // Additionally, attributes do not have corresponding concrete code
        // generated, they are used to *affect* codegen of other items.
        match self {
            Element::Table(t) => t.to_tokens(tokens),
            Element::Struct(_) => unimplemented!(),
            Element::Enum(e) => e.to_tokens(tokens),

            Element::Root(_) => unimplemented!(),
            Element::FileExtension(_) => unimplemented!(),
            Element::FileIdentifier(_) => unimplemented!(),
            Element::Attribute(_) => unimplemented!(),
            Element::Rpc(rpc) => rpc.to_tokens(tokens),
            Element::Object(_) => unimplemented!(),
            element => panic!("{:?}", element),
        }
    }
}

// TODO: actually open up a file
// this will need some notion of a default include path to search for fbs
// files.
impl ToTokens for Include<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { stem, doc, .. } = self;
        let id = format_ident!("{}", stem);
        (quote! {
            #doc
            use #id;
        })
        .to_tokens(tokens)
    }
}

// TODO:
// * Root types
// * File identifiers
// * File extensions
// * Parsing included files
impl ToTokens for Schema<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { includes, elements } = self;

        // split the code into 0 or 1 namespace declarations and a body
        let (namespaces, body): (Vec<_>, Vec<_>) = elements.iter().partition(|e| match e {
            Element::Namespace(_) => true,
            _ => false,
        });

        let (namespace_parts, doc): (Vec<_>, _) = match namespaces.len() {
            0 => (vec![], Comment { text: None }),
            1 => match &namespaces[0] {
                Element::Namespace(Namespace { parts, doc }) => (parts.to_vec(), doc.clone()),
                _ => unreachable!(),
            },
            _ => panic!("more than one namespace declaration found"),
        };

        // reverse fold over the namespace pieces to generate nested modules:
        // if namespace parts is [a, b, c]
        // and body is struct Foo { ... }
        // then
        // reverse:
        //   [c, b, a]
        // fold:
        //    init: struct Foo { ... }
        //    next: pub mod c { struct Foo { ... } }
        //    next: pub mod b { pub mod c { struct Foo { ... } } }
        //   final: pub mod a { pub mod b { pub mod c { struct Foo { ... } } } }
        let schema_body =
            namespace_parts
                .iter()
                .rev()
                .fold(quote! { #(#body)* }, |module_body, module_name| {
                    let name = format_ident!("{}", module_name.raw.to_snake_case());
                    quote! {
                        pub mod #name {
                            #module_body
                        }
                    }
                });

        // finally, glom everything together
        (quote! {
            #(#includes)*
            #doc // this is only filled if the namespace has a doc comment
            #schema_body
        })
        .to_tokens(tokens)
    }
}

impl ToTokens for File<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { schema, .. } = self;
        schema.to_tokens(tokens)
    }
}
