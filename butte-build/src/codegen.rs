use crate::{ir::types::*, parse::types::*};

use butte::VOffsetT;
use heck::{ShoutySnakeCase, SnakeCase};
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use std::{convert::TryInto, fmt::Display};
use syn::spanned::Spanned;

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

impl ToTokens for IrIdent<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        format_ident!("{}", self.raw.as_ref()).to_tokens(tokens)
    }
}

#[cfg(test)]
mod ir_ident_tests {
    use super::*;

    #[test]
    fn test_visit_ident() {
        let result = to_code(IrIdent::from("foo"));
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

/// Convert a `types::Type` to a type with the supplied wrapper for reference types
fn to_type_token(
    ty: &IrType,
    lifetime: &impl ToTokens,
    wrap_refs_types: &impl ToTokens,
    wrap_outer: bool,
) -> impl ToTokens {
    match ty {
        IrType::Bool => quote!(bool),
        IrType::Byte => quote!(i8),
        IrType::UByte => quote!(u8),
        IrType::Short => quote!(i16),
        IrType::UShort => quote!(u16),
        IrType::Int => quote!(i32),
        IrType::UInt => quote!(u32),
        IrType::Float => quote!(f32),
        IrType::Long => quote!(i64),
        IrType::ULong => quote!(u64),
        IrType::Double => quote!(f64),
        IrType::Int8 => quote!(i8),
        IrType::UInt8 => quote!(u8),
        IrType::Int16 => quote!(i16),
        IrType::UInt16 => quote!(u16),
        IrType::Int32 => quote!(i32),
        IrType::UInt32 => quote!(u32),
        IrType::Int64 => quote!(i64),
        IrType::UInt64 => quote!(u64),
        IrType::Float32 => quote!(f32),
        IrType::Float64 => quote!(f64),
        IrType::String => {
            let wrap_tokens = wrap_refs_types.into_token_stream();
            if wrap_tokens.is_empty() || !wrap_outer {
                quote!(&#lifetime str)
            } else {
                quote!(#wrap_tokens::<&#lifetime str>)
            }
        }
        IrType::Array(ty) => {
            // Arrays wrap the wrapping tokens with Vector
            let component_token = to_type_token(ty, lifetime, wrap_refs_types, true);
            let ty = quote!(butte::Vector<#lifetime, #component_token>);

            let wrap_tokens = wrap_refs_types.into_token_stream();
            if wrap_tokens.is_empty() || !wrap_outer {
                ty
            } else {
                quote!(#wrap_tokens::<#ty>)
            }
        }
        IrType::Custom(IrCustomTypeRef { ident, .. }) => {
            // Scalar types are never wrapped and have no lifetimes
            if ty.is_scalar() {
                quote!(#ident)
            } else {
                let ty = quote!(#ident<#lifetime>);
                let wrap_tokens = wrap_refs_types.into_token_stream();
                if wrap_tokens.is_empty() || !wrap_outer {
                    ty
                } else {
                    quote!(#wrap_tokens::<#ty>)
                }
            }
        }
    }
}

/// Convert a `types::DefaultValue` to a default field value
fn to_default_value(arg_ty: &impl ToTokens, default_value: &DefaultValue<'_>) -> impl ToTokens {
    match default_value {
        // Scalar field
        DefaultValue::Scalar(s) => s.to_token_stream(),
        // Enum field default variant
        DefaultValue::Ident(i) => {
            let variant = format_ident!("{}", i.raw);
            quote!(<#arg_ty>::#variant).to_token_stream()
        }
    }
}

/// Convert a `types::DefaultValue` to a doc comment describing the value
fn to_default_value_doc(ty: &IrType, default_value: &Option<DefaultValue<'_>>) -> impl ToTokens {
    if let Some(default_value) = default_value {
        let doc_value = match default_value {
            // Scalar field
            DefaultValue::Scalar(s) => s.to_token_stream().to_string(),
            // Enum field default variant
            DefaultValue::Ident(i) => format!("{}::{}", ty, i.raw),
        };
        let doc_string = format!(" The default value for this field is __{}__", doc_value);
        quote!(#[doc = #doc_string])
    } else {
        quote!()
    }
}

fn offset_id(field: &IrField<'_>) -> impl ToTokens {
    format_ident!("VT_{}", field.ident.as_ref().to_shouty_snake_case())
}

impl ToTokens for IrTable<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            ident: struct_id,
            fields,
            doc,
            ..
        } = self;

        let struct_id = struct_id.simple(); // discard namespace
        let raw_struct_name = struct_id.raw.as_ref();

        let builder_add_calls = fields.iter().map(
            |IrField {
                 ident: field_id, ..
             }| {
                let raw_field_name = field_id.raw.as_ref();
                let add_field_method = format_ident!("add_{}", raw_field_name);
                quote!(builder.#add_field_method(args.#field_id);)
            },
        );

        let args = format_ident!("{}Args", raw_struct_name);
        let args_fields = fields.iter().map(
            |IrField {
                 ident: field_id,
                 ty,
                 default_value,
                 ..
             }| {
                let arg_ty = if ty.is_union() {
                    quote!(butte::WIPOffset<butte::UnionWIPOffset>)
                } else {
                    let arg_ty = to_type_token(ty, &quote!('a), &quote!(butte::WIPOffset), true);
                    quote!(#arg_ty)
                };
                // Scalar or enum fields can have a default value
                let default_doc = to_default_value_doc(&ty, default_value);
                quote! {
                    #default_doc
                    pub #field_id: #arg_ty
                }
            },
        );
        let args_lifetime = |lifetime_name| {
            if fields
                .iter()
                .any(|f| !(f.ty.is_scalar() || f.ty.is_union()))
            {
                quote!(<#lifetime_name>)
            } else {
                quote!()
            }
        };
        let args_lifetime_a = args_lifetime(quote!('a));
        let args_lifetime_args = args_lifetime(quote!('args));

        let builder_type = format_ident!("{}Builder", raw_struct_name);

        // TODO: field accessors
        // TODO: check the impl of offset generation
        // TODO: testing this is going to be fun
        let field_nested_flatbuffers = fields.iter().filter_map(|IrField { ident: field_id, metadata, .. }| {
            let method_name = format_ident!("{}_nested_flatbuffer", field_id.raw.as_ref());
            if metadata.is_nested_flatbuffers() {
                Some(quote! {
                    pub fn #method_name(&self) -> Option<Self> {
                        self.#field_id.map(|data| <butte::ForwardsUOffset<Self>>::follow(data, 0))
                    }
                })
            } else {
                None
            }
        });

        let builder_field_methods = fields.iter().map(|field| {
            let IrField {
                ident: field_id,
                ty,
                default_value,
                ..
            } = field;
            let add_method_name = format_ident!("add_{}", field_id.raw.as_ref());
            let offset = offset_id(field);
            let field_offset = quote!(#struct_id::#offset);

            let arg_ty = if ty.is_union() {
                quote!(butte::WIPOffset<butte::UnionWIPOffset>)
            } else {
                let arg_ty = to_type_token(ty, &quote!('_), &quote!(butte::WIPOffset), true);
                quote!(#arg_ty)
            };

            let body = if ty.is_scalar() {
                if let Some(default_value) = default_value {
                    let default_value = to_default_value(&arg_ty, &default_value);
                    quote!(self.fbb.push_slot::<#arg_ty>(#field_offset, #field_id, #default_value))
                } else {
                    quote!(self.fbb.push_slot_always::<#arg_ty>(#field_offset, #field_id))
                }
            } else {
                quote!(self.fbb.push_slot_always::<#arg_ty>(#field_offset, #field_id))
            };
            //let arg_ty = to_type_token(ty, &quote!('b), &quote!(butte::WIPOffset));
            quote! {
                #[inline]
                fn #add_method_name(&mut self, #field_id: #arg_ty) {
                    #body;
                }
            }
        });

        let field_offset_constants = fields.iter().enumerate().map(|(index, field)| {
            let offset_name = offset_id(field);
            let offset_value = butte::field_index_to_field_offset(index as VOffsetT);
            quote! {
                pub const #offset_name: butte::VOffsetT = #offset_value;
            }
        });

        let field_accessors = fields.iter().map(|field| {
            let snake_name = format_ident!("{}", field.ident.as_ref().to_snake_case());
            let offset_name = offset_id(field);
            let ty = &field.ty;
            let ty_simple_lifetime =
                to_type_token(ty, &quote!('a), &quote!(butte::ForwardsUOffset), false);
            let ty_wrapped = to_type_token(ty, &quote!('a), &quote!(butte::ForwardsUOffset), true);

            if ty.is_union() {
                let (union_ident, enum_ident, variants) = match ty {
                    IrType::Custom(IrCustomTypeRef {
                        ty,
                        ident: ref union_ident,
                    }) => match ty {
                        IrCustomType::Union {
                            ref variants,
                            ref enum_ident,
                        } => (union_ident, enum_ident, variants),
                        _ => panic!("type is union"),
                    },
                    _ => panic!("type is union"),
                };

                let enum_ident = enum_ident.simple();

                let type_snake_name =
                    format_ident!("{}_type", field.ident.as_ref().to_snake_case());

                let names_to_enum_variant = variants.iter().map(
                    |IrUnionVariant {
                         ident: variant_ident,
                         ty: variant_ty,
                     }| {
                        let variant_ty_wrapped = to_type_token(
                            variant_ty,
                            &quote!('a),
                            &quote!(butte::ForwardsUOffset),
                            true,
                        );
                        quote! {
                            Some(#enum_ident::#variant_ident) => self.table
                                .get::<#variant_ty_wrapped>(#struct_id::#offset_name)?
                                .map(#union_ident::#variant_ident)
                        }
                    },
                );
                quote! {
                    #[inline]
                    pub fn #snake_name(&self) -> Result<Option<#ty_simple_lifetime>, butte::Error> {
                        Ok(match self.#type_snake_name()? {
                          #(#names_to_enum_variant),*,
                          None | Some(#enum_ident::None) => None
                        })
                    }
                }
            } else {
                quote! {
                    #[inline]
                    pub fn #snake_name(&self) -> Result<Option<#ty_simple_lifetime>, butte::Error> {
                        self.table
                            .get::<#ty_wrapped>(#struct_id::#offset_name)
                    }
                }
            }
        });

        let struct_offset_enum_name = format_ident!("{}Offset", raw_struct_name);

        let required_fields = fields.iter().map(|field| {
            let snake_name = field.ident.as_ref().to_snake_case();
            let offset_name = offset_id(field);
            quote! {
                self.fbb.required(o, #struct_id::#offset_name, #snake_name);
            }
        });

        (quote! {
            pub enum #struct_offset_enum_name {}

            #[derive(Copy, Clone, Debug, PartialEq)]
            #doc
            pub struct #struct_id<'a> {
                table: butte::Table<'a>,
            }

            impl<'a> From<butte::Table<'a>> for #struct_id<'a> {
                fn from(table: butte::Table<'a>) -> Self {
                    Self { table }
                }
            }

            impl<'a> #struct_id<'a> {
                pub fn create<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
                    fbb: &'mut_bldr mut butte::FlatBufferBuilder<'bldr>,
                    args: &'args #args#args_lifetime_args
                ) -> butte::WIPOffset<#struct_id<'bldr>> {
                    let mut builder = #builder_type::new(fbb);
                    #(#builder_add_calls)*
                    builder.finish()
                }

                // field offset constants
                #(#field_offset_constants)*

                // fields access
                #(#field_accessors)*

                // nested flatbuffers if applicable
                #(#field_nested_flatbuffers)*
            }

            impl<'a> butte::Follow<'a> for #struct_id<'a> {
                type Inner = Self;

                #[inline]
                fn follow(buf: &'a [u8], loc: usize) -> Result<Self::Inner, butte::Error> {
                    let table = butte::Table { buf, loc };
                    Ok(Self { table })
                }
            }

            // Builder Args
            // TODO: Can't use this because we can mix fields that are
            // default-able with those that are not
            pub struct #args#args_lifetime_a {
                #(#args_fields),*
            }

            //// builder
            pub struct #builder_type<'a, 'b> {
                fbb: &'b mut butte::FlatBufferBuilder<'a>,
                start: butte::WIPOffset<butte::TableUnfinishedWIPOffset>,
            }

            impl<'a: 'b, 'b> #builder_type<'a, 'b> {
                #(#builder_field_methods)*

                #[inline]
                pub fn new(fbb: &'b mut butte::FlatBufferBuilder<'a>) -> Self {
                    let start = fbb.start_table();
                    #builder_type {
                        fbb, start
                    }
                }

                #[inline]
                pub fn finish(self) -> butte::WIPOffset<#struct_id<'a>> {
                    let o = self.fbb.end_table(self.start);
                    #(#required_fields)*
                    butte::WIPOffset::new(o.value())
                }
            }
        })
        .to_tokens(tokens)
    }
}

// Do not implement
// Left in the code to prevent a rogue impl
// impl ToTokens for IrType<'_> {
//     fn to_tokens(&self, _: &mut TokenStream) {
//         panic!("This is unimplemented on purpose -- as types need context to be generated")
//     }
// }

impl ToTokens for IrEnumBaseType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            IrEnumBaseType::Byte => quote!(i8),
            IrEnumBaseType::UByte => quote!(u8),
            IrEnumBaseType::Short => quote!(i16),
            IrEnumBaseType::UShort => quote!(u16),
            IrEnumBaseType::Int => quote!(i32),
            IrEnumBaseType::UInt => quote!(u32),
            IrEnumBaseType::Long => quote!(i64),
            IrEnumBaseType::ULong => quote!(u64),
            IrEnumBaseType::Int8 => quote!(i8),
            IrEnumBaseType::UInt8 => quote!(u8),
            IrEnumBaseType::Int16 => quote!(i16),
            IrEnumBaseType::UInt16 => quote!(u16),
            IrEnumBaseType::Int32 => quote!(i32),
            IrEnumBaseType::UInt32 => quote!(u32),
            IrEnumBaseType::Int64 => quote!(i64),
            IrEnumBaseType::UInt64 => quote!(u64),
        }
        .to_tokens(tokens)
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

impl ToTokens for Comment<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let doc = self.lines.iter().rev().fold(quote!(), |docs, line| {
            quote! {
                #[doc = #line]
                #docs
            }
        });
        doc.to_tokens(tokens)
    }
}

impl ToTokens for IrNamespace<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.ident.simple();
        let nodes = &self.nodes;
        (quote! {
            pub mod #ident {
                #(#nodes)*
            }
        })
        .to_tokens(tokens)
    }
}

impl ToTokens for DottedIdent<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let parts = &self.parts;
        debug_assert!(!self.parts.is_empty());
        let code = parts.iter().map(|e| e.raw).join("::");
        let num_parts = parts.len();
        let path_string = if num_parts > 1 {
            format!(
                "{}::{}",
                std::iter::repeat("super").take(num_parts - 1).join("::"),
                code
            )
        } else {
            code
        };
        syn::parse_str::<syn::Path>(&path_string)
            .expect("Cannot parse path")
            .to_tokens(tokens)
    }
}

impl ToTokens for IrDottedIdent<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let parts = &self.parts;
        debug_assert!(!self.parts.is_empty());
        let code = parts.iter().map(|e| e.raw.as_ref()).join("::");
        let num_parts = parts.len();
        let path_string = if num_parts > 1 {
            format!(
                "{}::{}",
                std::iter::repeat("super").take(num_parts - 1).join("::"),
                code
            )
        } else {
            code
        };
        syn::parse_str::<syn::Path>(&path_string)
            .expect("Cannot parse path")
            .to_tokens(tokens)
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

fn lit_int(value: impl Display, base_type: impl Spanned + Display) -> impl ToTokens {
    let stringified_int = format!("{}_{}", value, base_type);
    syn::LitInt::new(&stringified_int, base_type.span())
}

impl ToTokens for IrEnum<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            ident: enum_id,
            values,
            base_type,
            doc,
            ..
        } = self;

        let enum_id = enum_id.simple();
        // generate enum variant name => string name of the variant for use in
        // a match statement
        let names_to_strings = values.iter().map(|IrEnumVal { ident: key, .. }| {
            let raw_key = key.raw.as_ref();
            quote! {
                #enum_id::#key => #raw_key
            }
        });

        // assign a value to the key if one was given, otherwise give it the
        // enumerated index's value
        let variants_and_scalars =
            values
                .iter()
                .enumerate()
                .map(|(i, IrEnumVal { ident: key, value })| {
                    // format the value with the correct type, i.e., base_type
                    let scalar_value = lit_int(
                        if let Some(constant) = *value {
                            constant
                        } else {
                            i.try_into().expect("invalid conversion to enum base type")
                        },
                        base_type.to_token_stream(),
                    );
                    (quote!(#key), quote!(#scalar_value))
                });

        let raw_snake_enum_name = enum_id.raw.as_ref().to_snake_case();
        let enum_id_fn_name = format_ident!("enum_name_{}", raw_snake_enum_name);

        let from_base_to_enum_variants =
            variants_and_scalars
                .clone()
                .map(|(variant_name, scalar_value)| {
                    quote! {
                        #scalar_value => Ok(<#enum_id>::#variant_name)
                    }
                });

        let from_enum_variant_to_base =
            variants_and_scalars
                .clone()
                .map(|(variant_name, scalar_value)| {
                    quote! {
                        <#enum_id>::#variant_name => #scalar_value
                    }
                });

        let fields = variants_and_scalars.map(|(variant_name, scalar_value)| {
            quote! {
                #variant_name = #scalar_value
            }
        });

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

            impl<'a> butte::Follow<'a> for #enum_id {
                type Inner = Self;

                fn follow(buf: &'a [u8], loc: usize) -> Result<Self::Inner, butte::Error> {
                    let scalar = butte::read_scalar_at::<#base_type>(buf, loc)?;
                    <Self as std::convert::TryFrom<#base_type>>::try_from(scalar)
                }
            }

            impl std::convert::TryFrom<#base_type> for #enum_id {
                type Error = butte::Error;
                fn try_from(value: #base_type) -> Result<Self, Self::Error> {
                    match value {
                        #(#from_base_to_enum_variants),*,
                        _ => Err(butte::Error::UnknownEnumVariant)
                    }
                }
            }

            impl From<#enum_id> for #base_type {
                fn from(value: #enum_id) -> #base_type {
                    match value {
                        #(#from_enum_variant_to_base),*
                    }
                }
            }

            impl butte::Push for #enum_id {
                type Output = Self;

                #[inline]
                fn push(&self, dst: &mut [u8], _rest: &[u8]) {
                    let scalar = <#base_type>::from(*self);
                    butte::emplace_scalar::<#base_type>(dst, scalar);
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

impl ToTokens for IrUnion<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            ident: union_id,
            enum_ident,
            variants,
            doc,
            ..
        } = self;

        let union_id = union_id.simple();
        let enum_id = enum_ident.simple();

        // the union's body definition
        let names_to_union_variant = variants.iter().map(
            |IrUnionVariant {
                 ident: variant_ident,
                 ty: variant_ty,
             }| {
                let variant_ty_token = to_type_token(variant_ty, &quote!('a), &quote!(), false);
                quote! {
                    #variant_ident(#variant_ty_token)
                }
            },
        );

        // generate union variant name => enum type name of the variant for use in
        // a match statement
        let names_to_enum_variant = variants.iter().map(
            |IrUnionVariant {
                 ident: variant_ident,
                 ..
             }| {
                quote! {
                    #union_id::#variant_ident(..) => #enum_id::#variant_ident
                }
            },
        );

        (quote! {
            #[derive(Copy, Clone, Debug, PartialEq)]
            #doc
            pub enum #union_id<'a> {
                #(#names_to_union_variant),*
            }


            impl #union_id<'_> {
                pub fn get_type(&self) -> #enum_id {
                    match self {
                        #(#names_to_enum_variant),*,
                    }
                }
            }

        })
        .to_tokens(tokens)
    }
}

// TODO: better error messages for things that aren't implemented
impl ToTokens for IrNode<'_> {
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
            IrNode::Table(t) => t.to_tokens(tokens),
            // Element::Struct(_) => unimplemented!(),
            IrNode::Enum(e) => e.to_tokens(tokens),
            IrNode::Union(u) => u.to_tokens(tokens),
            IrNode::Namespace(n) => n.to_tokens(tokens),
            // Element::Root(_) => unimplemented!(),
            // Element::FileExtension(_) => unimplemented!(),
            // Element::FileIdentifier(_) => unimplemented!(),
            // Element::Attribute(_) => unimplemented!(),
            // Element::Rpc(rpc) => rpc.to_tokens(tokens),
            // Element::Object(_) => unimplemented!(),
            element => panic!("{:?}", element),
        }
    }
}

impl ToTokens for IrRoot<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let nodes = &self.nodes;
        (quote! {
            #(#nodes)*
        })
        .to_tokens(tokens)
    }
}
