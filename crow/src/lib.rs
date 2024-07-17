extern crate proc_macro;
use std::fmt::Write;

use crate::parse::paren_to_ast;
use anyhow::{anyhow, Context, Result};
use itertools::Itertools;
use proc_macro2::{
    token_stream::{IntoIter, TokenStream},
    Delimiter, Group, Literal, TokenTree,
};
use quote::{format_ident, quote, ToTokens};
use syn::{parse::Parse, parse2, token::Type};

mod codegen;
mod parse;

use codegen::ssa_block;

#[proc_macro]
pub fn crow(ts: proc_macro::TokenStream) -> proc_macro::TokenStream {
    proc_macro::TokenStream::from(crow2(TokenStream::from(ts)))
}

fn crow2(ts: TokenStream) -> TokenStream {
    if let Some(TokenTree::Group(root)) = ts.into_iter().next() {
        let ast = paren_to_ast(root);
        ssa_block(ast.unwrap())
    } else {
        panic!()
    }
}
