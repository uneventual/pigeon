extern crate proc_macro;

use crate::parse::SIRParse;
use proc_macro2::{token_stream::TokenStream, TokenTree};

mod codegen;
mod explicit_types;
mod parse;

use codegen::ssa_block;

#[proc_macro]
pub fn crow(ts: proc_macro::TokenStream) -> proc_macro::TokenStream {
    proc_macro::TokenStream::from(crow2(TokenStream::from(ts)))
}

fn crow2(ts: TokenStream) -> TokenStream {
    if let Some(TokenTree::Group(root)) = ts.into_iter().next() {
        let ast = root.to_sir();
        ssa_block(ast.unwrap())
    } else {
        panic!()
    }
}
