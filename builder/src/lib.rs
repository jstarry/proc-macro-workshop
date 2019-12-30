extern crate proc_macro;
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::{quote, ToTokens};
use std::error::Error;
use syn::parse_macro_input;
use syn::spanned::Spanned;
use syn::{
    punctuated, AngleBracketedGenericArguments, DeriveInput, Field, GenericArgument, Lit, Meta,
    NestedMeta, Path, PathArguments, PathSegment, Type, TypePath,
};

struct VecInfo<'a> {
    ty: &'a Type,
    ident: Option<Ident>,
}

struct WrappedField<'a> {
    field: &'a Field,
    option_type: Option<&'a Type>,
    vec_info: Result<VecInfo<'a>, Box<dyn Error>>,
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let named_fields = match input.data {
        syn::Data::Struct(data) => match data.fields {
            syn::Fields::Named(fields) => fields,
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };

    let wrapped_fields: Vec<WrappedField> = wrapped_fields(&named_fields).collect();
    for wf in wrapped_fields.iter() {
        match &wf.vec_info {
            Err(err) => match err.downcast_ref::<syn::Error>() {
                Some(err) => return TokenStream::from(err.to_compile_error()),
                None => {}
            },
            _ => {}
        }
    }
    let builder_name = Ident::new(&format!("{}Builder", name), Span::call_site());
    let builder_optional_fields = field_defs(wrapped_fields.iter());
    let builder_fields_init = field_inits(wrapped_fields.iter());
    let builder_field_setters = field_setters(wrapped_fields.iter());
    let builder_field_builders = field_builders(wrapped_fields.iter());
    let expanded = quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_fields_init)*
                }
            }
        }
        impl #builder_name {
            #(#builder_field_setters)*
            pub fn build(&mut self) -> ::std::result::Result<#name, ::std::boxed::Box<dyn ::std::error::Error>> {
                Ok(#name {
                    #(#builder_field_builders)*
                })
            }
        }
        pub struct #builder_name {
            #(#builder_optional_fields)*
        }
    };

    TokenStream::from(expanded)
}

fn field_defs<'a>(
    wrapped_fields: impl Iterator<Item = &'a WrappedField<'a>>,
) -> impl Iterator<Item = impl ToTokens + 'a> {
    wrapped_fields.map(|wf| {
        let syn::Field {
            ident: name, ty, ..
        } = &wf.field;
        let field_type = match wf.option_type {
            Some(ty) => ty,
            None => ty,
        };
        match wf.vec_info {
            Ok(_) => quote! {
                #name: #field_type,
            },
            Err(_) => quote! {
                #name: ::std::option::Option<#field_type>,
            },
        }
    })
}

fn field_setters<'a>(
    wrapped_fields: impl Iterator<Item = &'a WrappedField<'a>>,
) -> impl Iterator<Item = impl ToTokens + 'a> {
    wrapped_fields.map(|wf| {
        let syn::Field {
            ident: name, ty, ..
        } = &wf.field;
        let arg_type = match wf.option_type {
            Some(ty) => ty,
            None => match &wf.vec_info {
                Ok(vi) => vi.ty,
                Err(_) => ty,
            },
        };
        match &wf.vec_info {
            Ok(vi) => {
                let setter_name = &vi.ident;
                quote! {
                    fn #setter_name(&mut self, #setter_name: #arg_type) -> &mut Self {
                        self.#name.push(#setter_name);
                        self
                    }
                }
            }
            Err(_) => quote! {
                fn #name(&mut self, #name: #arg_type) -> &mut Self {
                    self.#name = ::std::option::Option::Some(#name);
                    self
                }
            },
        }
    })
}

fn field_builders<'a>(
    wrapped_fields: impl Iterator<Item = &'a WrappedField<'a>>,
) -> impl Iterator<Item = impl ToTokens + 'a> {
    wrapped_fields.map(|wf| {
        let syn::Field { ident: name, .. } = &wf.field;
        match wf.option_type {
            Some(_) => quote! {
                #name: self.#name.take(),
            },
            None => match wf.vec_info {
                Ok(_) => quote! {
                    #name: self.#name.drain(..).collect(),
                },
                Err(_) => quote! {
                    #name: self.#name.take().ok_or("unset")?,
                },
            },
        }
    })
}

fn field_inits<'a>(
    wrapped_fields: impl Iterator<Item = &'a WrappedField<'a>>,
) -> impl Iterator<Item = impl ToTokens + 'a> {
    wrapped_fields.map(|wf| {
        let syn::Field { ident: name, .. } = &wf.field;
        match wf.vec_info {
            Ok(_) => quote! { #name: vec![], },
            Err(_) => quote! { #name: ::std::option::Option::None, },
        }
    })
}

fn wrapped_fields<'a>(fields: &'a syn::FieldsNamed) -> impl Iterator<Item = WrappedField> {
    fields.named.iter().map(|f| WrappedField {
        field: &f,
        option_type: get_optional_type(f),
        vec_info: get_vec_info(f),
    })
}

fn get_vec_info(field: &syn::Field) -> Result<VecInfo, Box<dyn Error>> {
    let (_, wrapped_ty) = unwrap_type(&field.ty).ok_or("failed to unwrap field type")?;
    let attr = field.attrs.first().ok_or("no attrs found")?;
    let meta = attr.parse_meta().map_err(|_| "failed to parse meta")?;
    let meta_list = match meta {
        Meta::List(meta_list) => Some(meta_list),
        _ => None,
    }
    .ok_or("didn't find builder attr")?;
    let nested_meta = match meta_list.ident.to_string().as_str() {
        "builder" => Some(meta_list.nested.clone()),
        _ => None,
    }
    .ok_or("didn't find builder attr")?;

    let err = Box::new(syn::Error::new_spanned(
        meta_list,
        "expected `builder(each = \"...\")`",
    ));

    let first_nested = match nested_meta.first() {
        Some(f) => f,
        None => return Err(err),
    };
    let name_value = match first_nested {
        punctuated::Pair::End(NestedMeta::Meta(Meta::NameValue(name_value))) => name_value,
        _ => return Err(err),
    };

    if name_value.ident.to_string() != String::from("each") {
        return Err(err);
    }

    let arg_name = match &name_value.lit {
        Lit::Str(lit_str) => lit_str.value(),
        _ => return Err(err),
    };

    Ok(VecInfo {
        ident: Some(Ident::new(&arg_name, Span::call_site())),
        ty: wrapped_ty,
    })
}

fn unwrap_type(ty: &syn::Type) -> Option<(&Ident, &syn::Type)> {
    let segments = match ty {
        Type::Path(TypePath {
            qself: None,
            path:
                Path {
                    leading_colon: None,
                    segments,
                },
        }) => Some(segments),
        _ => None,
    }?;

    let first_segment = segments.first()?;
    let (wrapping_ident, type_args) = match first_segment {
        punctuated::Pair::End(PathSegment {
            ident,
            arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }),
        }) => Some((ident, args)),
        _ => None,
    }?;

    let wrapped_ty = match type_args.first()? {
        punctuated::Pair::End(GenericArgument::Type(ref ty)) => Some(ty),
        _ => None,
    }?;

    Some((wrapping_ident, wrapped_ty))
}

fn get_optional_type(field: &syn::Field) -> Option<&syn::Type> {
    let (wrapping_ident, wrapped_ty) = unwrap_type(&field.ty)?;
    if wrapping_ident.to_string() != String::from("Option") {
        return None;
    }
    Some(wrapped_ty)
}
