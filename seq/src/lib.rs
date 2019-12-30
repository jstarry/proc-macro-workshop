extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Group, Ident, Literal, TokenTree};
use quote::quote;
use std::iter::FromIterator;
use syn::parse::{Parse, ParseStream, Result};
use syn::{braced, parse_macro_input, ExprBlock, LitInt, Token};

struct Sequence {
    ident: Ident,
    begin: LitInt,
    end: LitInt,
    body: proc_macro2::TokenStream,
}

impl Parse for Sequence {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: Ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let begin: LitInt = input.parse()?;
        input.parse::<Token![..]>()?;
        let end: LitInt = input.parse()?;

        let content;
        braced!(content in input);
        let mut tree: Vec<TokenTree> = vec![];
        loop {
            match content.parse() {
                Ok(next) => tree.push(next),
                _ => break,
            }
        }
        let body = tree.into_iter().collect();
        Ok(Sequence {
            ident,
            begin,
            end,
            body,
        })
    }
}

// should expect the input to contain a syn::Ident, Token![in], syn::LitInt,
// Token![..], syn::LitInt.

fn map_replace(tt: TokenTree, replace_ident: &str, lit: Literal) -> TokenTree {
    match tt {
        TokenTree::Group(group) => TokenTree::Group(Group::new(
            group.delimiter(),
            group
                .stream()
                .into_iter()
                .map(|tt| map_replace(tt, replace_ident, lit.clone()))
                .collect(),
        )),
        TokenTree::Ident(ident) => match ident.to_string().as_str() {
            i if i == replace_ident => TokenTree::Literal(lit),
            _ => TokenTree::Ident(ident),
        },
        o @ _ => o,
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq = parse_macro_input!(input as Sequence);
    let name = seq.ident;
    let body = seq.body;

    let bodies = (seq.begin.value()..seq.end.value()).map(|iter| {
        let replaced = body
            .clone()
            .into_iter()
            .map(|tt| map_replace(tt, name.to_string().as_str(), Literal::u64_unsuffixed(iter)));

        quote! {
            #(#replaced)*
        }
    });

    TokenStream::from(quote! { #(#bodies)* })
    // TokenStream::from(quote! { let x = 3; })
}
