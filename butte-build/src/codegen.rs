use crate::{ast::types as ast, ir::types as ir};

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

impl ToTokens for ast::Ident<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        format_ident!("{}", self.raw).to_tokens(tokens)
    }
}

#[cfg(test)]
mod ident_tests {
    use super::*;

    #[test]
    fn test_visit_ident() {
        let result = to_code(ast::Ident::from("foo"));
        let expected = "foo";
        assert_eq!(result, expected);
    }
}

impl ToTokens for ir::Ident<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        format_ident!("{}", self.raw.as_ref()).to_tokens(tokens)
    }
}

#[cfg(test)]
mod ir_ident_tests {
    use super::*;

    #[test]
    fn test_visit_ident() {
        let result = to_code(ir::Ident::from("foo"));
        let expected = "foo";
        assert_eq!(result, expected);
    }
}

impl ToTokens for ast::Scalar {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            ast::Scalar::Integer(i) => i.to_tokens(tokens),
            ast::Scalar::Float(f) => f.to_tokens(tokens),
            ast::Scalar::Boolean(b) => b.to_tokens(tokens),
        }
    }
}

/// Convert a `types::Type` to a type with the supplied wrapper for reference types
fn to_type_token(
    context_namespace: Option<&ir::QualifiedIdent<'_>>,
    ty: &ir::Type<'_>,
    lifetime: &TokenStream,
    wrap_refs_types: &TokenStream,
    wrap_outer: bool,
) -> TokenStream {
    let empty_lifetime = quote!();

    let lifetime_is_named = {
        if lifetime.is_empty() {
            false
        } else {
            let parsed_lifetime =
                syn::parse2::<syn::Lifetime>(lifetime.clone()).expect("lifetime must be valid");

            let anonymous_lifetime = syn::Ident::new("_", proc_macro2::Span::call_site());
            parsed_lifetime.ident != anonymous_lifetime
        }
    };
    // Lifetime for references (&str, &[u8], ...)
    let ref_lifetime = if lifetime_is_named {
        lifetime
    } else {
        &empty_lifetime
    };

    match ty {
        ir::Type::Bool => quote!(bool),
        ir::Type::Byte => quote!(i8),
        ir::Type::UByte => quote!(u8),
        ir::Type::Short => quote!(i16),
        ir::Type::UShort => quote!(u16),
        ir::Type::Int => quote!(i32),
        ir::Type::UInt => quote!(u32),
        ir::Type::Float => quote!(f32),
        ir::Type::Long => quote!(i64),
        ir::Type::ULong => quote!(u64),
        ir::Type::Double => quote!(f64),
        ir::Type::Int8 => quote!(i8),
        ir::Type::UInt8 => quote!(u8),
        ir::Type::Int16 => quote!(i16),
        ir::Type::UInt16 => quote!(u16),
        ir::Type::Int32 => quote!(i32),
        ir::Type::UInt32 => quote!(u32),
        ir::Type::Int64 => quote!(i64),
        ir::Type::UInt64 => quote!(u64),
        ir::Type::Float32 => quote!(f32),
        ir::Type::Float64 => quote!(f64),
        ir::Type::String => {
            let wrap_tokens = wrap_refs_types.into_token_stream();

            if wrap_tokens.is_empty() || !wrap_outer {
                quote!(&#ref_lifetime str)
            } else {
                quote!(#wrap_tokens::<&#ref_lifetime str>)
            }
        }
        ir::Type::Array(ty) => {
            // Arrays wrap the wrapping tokens with Vector
            let component_token =
                to_type_token(context_namespace, ty, lifetime, wrap_refs_types, true);
            let ty = quote!(butte::Vector<#lifetime, #component_token>);

            let wrap_tokens = wrap_refs_types.into_token_stream();
            if wrap_tokens.is_empty() || !wrap_outer {
                ty
            } else {
                quote!(#wrap_tokens::<#ty>)
            }
        }
        ir::Type::Custom(ir::CustomTypeRef { ident, ty }) => {
            // Scalar types are never wrapped and have no lifetimes
            let ident = &ident.relative(context_namespace);
            if ty.is_scalar() {
                quote!(#ident)
            } else {
                let ty = if ty == &ir::CustomType::Table {
                    quote!(#ident<&#ref_lifetime [u8]>) // handle structs
                } else {
                    quote!(#ident<#lifetime>)
                };
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

/// Convert a `types::ast::DefaultValue` to a default field value
fn to_default_value(
    arg_ty: &impl ToTokens,
    default_value: &ast::DefaultValue<'_>,
) -> impl ToTokens {
    match default_value {
        // Scalar field
        ast::DefaultValue::Scalar(s) => s.to_token_stream(),
        // Enum field default variant
        ast::DefaultValue::Ident(i) => {
            let variant = format_ident!("{}", i.raw);
            quote!(<#arg_ty>::#variant).to_token_stream()
        }
    }
}

/// Convert a `types::ast::DefaultValue` to a doc comment describing the value
fn to_default_value_doc(
    ty: &ir::Type<'_>,
    default_value: &Option<ast::DefaultValue<'_>>,
) -> impl ToTokens {
    if let Some(default_value) = default_value {
        let doc_value = match default_value {
            // Scalar field
            ast::DefaultValue::Scalar(s) => s.to_token_stream().to_string(),
            // Enum field default variant
            ast::DefaultValue::Ident(i) => format!("{}::{}", ty, i.raw),
        };
        let doc_string = format!(" The default value for this field is __{}__", doc_value);
        quote!(#[doc = #doc_string])
    } else {
        quote!()
    }
}

fn offset_id(field: &ir::Field<'_>) -> impl ToTokens {
    format_ident!("VT_{}", field.ident.as_ref().to_shouty_snake_case())
}

impl ToTokens for ir::Table<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            ident: struct_qualified_id,
            fields,
            doc,
            ..
        } = self;

        let table_ns = struct_qualified_id.namespace();
        let table_ns_ref = table_ns.as_ref();
        let struct_id = struct_qualified_id.simple(); // discard namespace
        let raw_struct_name = struct_id.raw.as_ref();

        let builder_add_calls = fields.iter().map(
            |ir::Field {
                 ident: field_id,
                 ty,
                 metadata,
                 ..
             }| {
                let raw_field_name = field_id.raw.as_ref();
                let add_field_method = format_ident!("add_{}", raw_field_name);

                if metadata.required || ty.is_scalar() {
                    quote!(builder.#add_field_method(args.#field_id);)
                } else {
                    quote! {
                        if let Some(x) = args.#field_id { builder.#add_field_method(x); }
                    }
                }
            },
        );

        let args = format_ident!("{}Args", raw_struct_name);
        let args_fields = fields.iter().map(
            |ir::Field {
                 ident: field_id,
                 ty,
                 default_value,
                 metadata,
                 ..
             }| {
                let arg_ty = if ty.is_union() {
                    quote!(butte::WIPOffset<butte::UnionWIPOffset>)
                } else {
                    let arg_ty = to_type_token(
                        table_ns_ref,
                        ty,
                        &quote!('a),
                        &quote!(butte::WIPOffset),
                        true,
                    );
                    quote!(#arg_ty)
                };

                let arg_ty = if metadata.required || ty.is_scalar() {
                    arg_ty
                } else {
                    quote!(Option<#arg_ty>)
                };

                let allow_type_complexity = if ty.is_complex() {
                    quote!(#[allow(clippy::type_complexity)])
                } else {
                    quote!()
                };

                // Scalar or enum fields can have a default value
                let default_doc = to_default_value_doc(&ty, default_value);
                quote! {
                    #default_doc
                    #allow_type_complexity
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

        // Can we implement `Default` on this table?
        // True if all the fields are either scalar or optional
        // Scalar fields must always implement `Default`
        let args_can_derive_default = fields
            .iter()
            .all(|field| !field.metadata.required || field.ty.is_scalar());

        let args_default_impl = if args_can_derive_default {
            let args_fields_defaults = fields.iter().map(
                |ir::Field {
                     ident: field_id,
                     ty,
                     default_value,
                     ..
                 }| {
                    let arg_ty = to_type_token(table_ns_ref, ty, &quote!(), &quote!(), false);
                    if !ty.is_scalar() {
                        // optional non-scalar types default to None
                        quote!(#field_id: None)
                    } else if let Some(default_value) = default_value {
                        // Handle customized default values
                        if ty.is_enum() {
                            let default_name = if let ast::DefaultValue::Ident(i) = default_value {
                                format_ident!("{}", i.raw)
                            } else {
                                panic!("expecting default ident for enum")
                            };
                            quote!(#field_id: <#arg_ty>::#default_name)
                        }
                        // TODO: handle structs
                        else {
                            // numeric types
                            let default_val = match default_value {
                                ast::DefaultValue::Scalar(s) => quote!(#s),
                                _ => panic!("expecting numeric default"),
                            };
                            quote!(#field_id: #default_val)
                        }
                    } else {
                        // no custom default value, default to the scalar type's default
                        quote!(#field_id: <#arg_ty>::default())
                    }
                },
            );
            quote! {
                impl#args_lifetime_a Default for #args#args_lifetime_a {
                    fn default() -> Self {
                        Self {
                            #(#args_fields_defaults),*
                        }
                    }
                }
            }
        } else {
            quote!()
        };

        let builder_type = format_ident!("{}Builder", raw_struct_name);

        let builder_field_methods = fields.iter().map(|field| {
            let ir::Field {
                ident: field_id,
                ty,
                default_value,
                ..
            } = field;
            let add_method_name = format_ident!("add_{}", field_id.raw.as_ref());
            let offset = offset_id(field);
            let field_offset = quote!(#struct_id::#offset);

            let allow_type_complexity = if ty.is_complex() {
                quote!(#[allow(clippy::type_complexity)])
            } else {
                quote!()
            };

            let arg_ty = if ty.is_union() {
                quote!(butte::WIPOffset<butte::UnionWIPOffset>)
            } else {
                let arg_ty = to_type_token(
                    table_ns_ref,
                    ty,
                    &quote!('b),
                    &quote!(butte::WIPOffset),
                    true,
                );
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

            quote! {
                #[inline]
                #allow_type_complexity
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
            let ir::Field {
                ident,
                ty,
                metadata,
                doc,
                ..
            } = field;
            let offset_name = offset_id(field);
            let snake_name = format_ident!("{}", ident.as_ref().to_snake_case());
            let snake_name_str = snake_name.to_string();
            let ty_ret = to_type_token(
                table_ns_ref,
                ty,
                &quote!('_),
                &quote!(butte::ForwardsUOffset),
                false,
            );
            let ty_wrapped = to_type_token(
                table_ns_ref,
                ty,
                &quote!('_),
                &quote!(butte::ForwardsUOffset),
                true,
            );

            let allow_type_complexity = if ty.is_complex() {
                quote!(#[allow(clippy::type_complexity)])
            } else {
                quote!()
            };

            if ty.is_union() {
                let (union_ident, enum_ident, variants) = match ty {
                    ir::Type::Custom(ir::CustomTypeRef {
                        ty,
                        ident: ref union_ident,
                    }) => match ty {
                        ir::CustomType::Union {
                            ref variants,
                            ref enum_ident,
                        } => (union_ident, enum_ident, variants),
                        _ => panic!("type is union"),
                    },
                    _ => panic!("type is union"),
                };

                let union_ident = union_ident.relative(table_ns_ref);
                let enum_ident = enum_ident.relative(table_ns_ref);

                let type_snake_name =
                    format_ident!("{}_type", field.ident.as_ref().to_snake_case());

                if field.metadata.required {
                    let names_to_enum_variant = variants.iter().map(
                        |ir::UnionVariant {
                             ident: variant_ident,
                             ty: variant_ty,
                             ..
                         }| {
                            let variant_ty_wrapped = to_type_token(table_ns_ref,
                                variant_ty,
                                &quote!('_),
                                &quote!(butte::ForwardsUOffset),
                                true,
                            );
                            quote! {
                                #enum_ident::#variant_ident => #union_ident::#variant_ident(self.table
                                    .get::<#variant_ty_wrapped>(#struct_id::#offset_name)?
                                    .ok_or_else(|| butte::Error::RequiredFieldMissing(#snake_name_str))?)
                            }
                        },
                    );
                    quote! {
                        #[inline]
                        pub fn #snake_name(&self) -> Result<#ty_ret, butte::Error> {
                            Ok(match self.#type_snake_name()? {
                              #(#names_to_enum_variant,)*
                              #enum_ident::None => return Err(butte::Error::RequiredFieldMissing(#snake_name_str))
                            })
                        }
                    }
                } else {
                    let names_to_enum_variant = variants.iter().map(
                        |ir::UnionVariant {
                             ident: variant_ident,
                             ty: variant_ty,
                             ..
                         }| {
                            let variant_ty_wrapped = to_type_token(table_ns_ref,
                                variant_ty,
                                &quote!('_),
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
                        pub fn #snake_name(&self) -> Result<Option<#ty_ret>, butte::Error> {
                            Ok(match self.#type_snake_name()? {
                              #(#names_to_enum_variant,)*
                              None | Some(#enum_ident::None) => None
                            })
                        }
                    }
                }
            } else if metadata.required {
                quote! {
                    #doc
                    #[inline]
                    #allow_type_complexity
                    pub fn #snake_name(&self) -> Result<#ty_ret, butte::Error> {
                        Ok(self.table
                            .get::<#ty_wrapped>(#struct_id::#offset_name)?
                            .ok_or_else(|| butte::Error::RequiredFieldMissing(#snake_name_str))?)
                    }
                }
            } else {
                quote! {
                    #doc
                    #[inline]
                    #allow_type_complexity
                    pub fn #snake_name(&self) -> Result<Option<#ty_ret>, butte::Error> {
                        self.table
                            .get::<#ty_wrapped>(#struct_id::#offset_name)
                    }
                }
            }
        });

        let struct_offset_enum_name = format_ident!("{}Offset", raw_struct_name);

        let required_fields = fields
            .iter()
            .filter(|field| field.metadata.required)
            .map(|field| {
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
            pub struct #struct_id<B> {
                table: butte::Table<B>,
            }

            impl<B> From<butte::Table<B>> for #struct_id<B> {
                fn from(table: butte::Table<B>) -> Self {
                    Self { table }
                }
            }

            impl<B> From<#struct_id<B>> for butte::Table<B> {
                fn from(s: #struct_id<B>) -> Self {
                    s.table
                }
            }

            impl<'a> #struct_id<&'a [u8]> {
                // field offset constants
                #(#field_offset_constants)*

                pub fn create<'bldr: 'args, 'args: 'mut_bldr, 'mut_bldr>(
                    fbb: &'mut_bldr mut butte::FlatBufferBuilder<'bldr>,
                    args: &'args #args#args_lifetime_args
                ) -> butte::WIPOffset<#struct_id<&'bldr [u8]>> {
                    let mut builder = #builder_type::new(fbb);
                    #(#builder_add_calls)*
                    builder.finish()
                }
            }


            impl<B> #struct_id<B>
            where
                B: std::convert::AsRef<[u8]>
            {
                // fields access
                #(#field_accessors)*

                pub fn get_root(buf: B) -> Result<Self, butte::Error> {
                    let table = butte::Table::get_root(buf)?;
                    Ok(Self { table })
                }
            }

            impl<'a> butte::Follow<'a> for #struct_id<&'a [u8]> {
                type Inner = Self;

                #[inline]
                fn follow(buf: &'a [u8], loc: usize) -> Result<Self::Inner, butte::Error> {
                    let table = butte::Table::new(buf, loc);
                    Ok(Self { table })
                }
            }

            impl<B> butte::FollowBuf for #struct_id<B>
            where
                B: std::convert::AsRef<[u8]>
            {
                type Buf = B;
                type Inner = Self;

                #[inline]
                fn follow_buf(buf: Self::Buf, loc: usize) -> Result<Self::Inner, butte::Error> {
                    let table = butte::Table::new(buf, loc);
                    Ok(Self { table })
                }
            }

            // Builder Args
            pub struct #args#args_lifetime_a {
                #(#args_fields),*
            }

            #args_default_impl

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
                pub fn finish(self) -> butte::WIPOffset<#struct_id<&'a [u8]>> {
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
// impl ToTokens for ir::Type<'_> {
//     fn to_tokens(&self, _: &mut TokenStream) {
//         panic!("This is unimplemented on purpose -- as types need context to be generated")
//     }
// }

impl ToTokens for ir::EnumBaseType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            ir::EnumBaseType::Byte => quote!(i8),
            ir::EnumBaseType::UByte => quote!(u8),
            ir::EnumBaseType::Short => quote!(i16),
            ir::EnumBaseType::UShort => quote!(u16),
            ir::EnumBaseType::Int => quote!(i32),
            ir::EnumBaseType::UInt => quote!(u32),
            ir::EnumBaseType::Long => quote!(i64),
            ir::EnumBaseType::ULong => quote!(u64),
            ir::EnumBaseType::Int8 => quote!(i8),
            ir::EnumBaseType::UInt8 => quote!(u8),
            ir::EnumBaseType::Int16 => quote!(i16),
            ir::EnumBaseType::UInt16 => quote!(u16),
            ir::EnumBaseType::Int32 => quote!(i32),
            ir::EnumBaseType::UInt32 => quote!(u32),
            ir::EnumBaseType::Int64 => quote!(i64),
            ir::EnumBaseType::UInt64 => quote!(u64),
        }
        .to_tokens(tokens)
    }
}

impl ToTokens for ast::Comment<'_> {
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

impl ToTokens for ir::QualifiedIdent<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let parts = &self.parts;
        debug_assert!(!self.parts.is_empty());
        let code = parts.iter().map(|e| e.raw.as_ref()).join("::");
        syn::parse_str::<syn::Path>(&code)
            .expect("Cannot parse path")
            .to_tokens(tokens)
    }
}

fn lit_int(value: impl Display, base_type: impl Spanned + Display) -> impl ToTokens {
    let stringified_int = format!("{}_{}", value, base_type);
    syn::LitInt::new(&stringified_int, base_type.span())
}

impl ToTokens for ir::Enum<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            ident: enum_id,
            variants,
            base_type,
            doc,
            ..
        } = self;

        let enum_id = enum_id.simple();
        // generate enum variant name => string name of the variant for use in
        // a match statement
        let names_to_strings = variants.iter().map(|ir::EnumVariant { ident: key, .. }| {
            let raw_key = key.raw.as_ref();
            quote! {
                #enum_id::#key => #raw_key
            }
        });

        let default_value = variants
            .iter()
            .map(|ir::EnumVariant { ident: key, .. }| {
                quote! {
                    #enum_id::#key
                }
            })
            .next();

        // assign a value to the key if one was given, otherwise give it the
        // enumerated index's value
        let variants_and_scalars = variants.iter().enumerate().map(
            |(
                i,
                ir::EnumVariant {
                    ident: key,
                    value,
                    doc,
                },
            )| {
                // format the value with the correct type, i.e., base_type
                let scalar_value = lit_int(
                    if let Some(constant) = *value {
                        constant
                    } else {
                        i.try_into().expect("invalid conversion to enum base type")
                    },
                    base_type.to_token_stream(),
                );
                (quote!(#key), quote!(#scalar_value), doc)
            },
        );

        let raw_snake_enum_name = enum_id.raw.as_ref().to_snake_case();
        let enum_id_fn_name = format_ident!("enum_name_{}", raw_snake_enum_name);

        let from_base_to_enum_variants =
            variants_and_scalars
                .clone()
                .map(|(variant_name, scalar_value, _)| {
                    quote! {
                        #scalar_value => Ok(<#enum_id>::#variant_name)
                    }
                });

        let from_enum_variant_to_base =
            variants_and_scalars
                .clone()
                .map(|(variant_name, scalar_value, _)| {
                    quote! {
                        <#enum_id>::#variant_name => #scalar_value
                    }
                });

        let fields = variants_and_scalars.map(|(variant_name, scalar_value, doc)| {
            quote! {
                #doc
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

            impl Default for #enum_id {
                fn default() -> Self {
                    #default_value
                }
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
                        #(#from_base_to_enum_variants,)*
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

impl ToTokens for ir::Union<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            ident: union_qualified_id,
            enum_ident,
            variants,
            doc,
            ..
        } = self;

        let union_ns = union_qualified_id.namespace();
        let union_ns_ref = union_ns.as_ref();
        let union_id = union_qualified_id.simple();
        let enum_id = enum_ident.relative(union_ns_ref);

        // the union's body definition
        let names_to_union_variant = variants.iter().map(
            |ir::UnionVariant {
                 ident: variant_ident,
                 ty: variant_ty,
                 doc,
             }| {
                let variant_ty_token =
                    to_type_token(union_ns_ref, variant_ty, &quote!('a), &quote!(), false);
                quote! {
                    #doc
                    #variant_ident(#variant_ty_token)
                }
            },
        );

        // generate union variant name => enum type name of the variant for use in
        // a match statement
        let names_to_enum_variant = variants.iter().map(
            |ir::UnionVariant {
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
                        #(#names_to_enum_variant),*
                    }
                }
            }

        })
        .to_tokens(tokens)
    }
}

pub struct CodeGenerator<'a> {
    pub(crate) root: ir::Root<'a>,
    pub(crate) rpc_gen: Option<Box<dyn RpcGenerator>>,
}

impl<'a> CodeGenerator<'a> {
    pub fn build_token_stream(&mut self) -> TokenStream {
        let mut token_stream = TokenStream::default();
        self.build_tokens(&mut token_stream);
        token_stream
    }

    pub fn build_tokens(&mut self, tokens: &mut TokenStream) {
        let mut rpc_gen = self.rpc_gen.take();

        for node in &self.root.nodes {
            self.node_to_tokens(node, &mut rpc_gen, tokens);
        }
    }

    fn node_to_tokens(
        &self,
        node: &ir::Node<'a>,
        rpc_gen: &mut Option<Box<dyn RpcGenerator>>,
        tokens: &mut TokenStream,
    ) {
        // The following constructs are (or should be) handled at the file level:
        // * Namespaces
        // * Root types
        // * File extensions
        // * File identifiers
        //
        // Additionally, attributes do not have corresponding concrete code generated, they are
        // used to *affect* codegen of other items.
        match node {
            ir::Node::Table(t) => t.to_tokens(tokens),
            // ir::Node::Struct(_) => unimplemented!(),
            ir::Node::Enum(e) => e.to_tokens(tokens),
            ir::Node::Union(u) => u.to_tokens(tokens),
            ir::Node::Namespace(n) => {
                let ident = format_ident!("{}", n.ident.simple().as_ref().to_snake_case());
                let mut nodes_ts = TokenStream::default();
                for node in &n.nodes {
                    self.node_to_tokens(node, rpc_gen, &mut nodes_ts);
                }
                (quote! {
                    pub mod #ident {
                        #nodes_ts
                    }
                })
                .to_tokens(tokens)
            }
            ir::Node::Rpc(rpc) => {
                if let Some(gen) = rpc_gen {
                    gen.generate(rpc, tokens)
                }
            }
            element => panic!("{:?}", element),
        }
    }
}

pub trait RpcGenerator {
    /// Generates a Rust interface or implementation for a service, writing the
    /// result to the provided `token_stream`.
    fn generate<'a>(&mut self, rpc: &ir::Rpc<'a>, token_stream: &mut TokenStream);
}

#[cfg(test)]
mod table_tests {
    use super::*;
    use crate::{
        ir::{types::Node, Builder},
        parser::schema_decl,
    };

    #[test]
    fn test_required_fields() {
        let input = "\
table Hello {
  world: string (required);
  earth: int = 616 (required);
  universe: string;
}";
        let (_, schema) = schema_decl(input).unwrap();
        let actual = Builder::build(schema).unwrap();
        match &actual.nodes[0] {
            Node::Table(table) => {
                let result = to_code(table);
                assert_eq!(2, result.matches("required").count());
            }
            node => panic!("{:?}", node),
        }
    }
}
