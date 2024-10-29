extern crate proc_macro;

use parse::SIRNode;
use proc_macro2::{token_stream::TokenStream, TokenTree};
use quote::quote;
use syn::parse2;

mod codegen;
mod explicit_types;
mod parse;

#[proc_macro]
pub fn crow(ts: proc_macro::TokenStream) -> proc_macro::TokenStream {
    proc_macro::TokenStream::from(crow2(TokenStream::from(ts)))
}

fn crow2(ts: TokenStream) -> TokenStream {
    if let Some(TokenTree::Group(root)) = ts.into_iter().next() {
        let ast_result = parse2::<SIRNode>(quote!(#root));
        match ast_result {
            Ok(ast) => {
                quote!({ #ast })
            }
            Err(ce) => syn::Error::new(ce.span(), ce).to_compile_error(),
        }
    } else {
        panic!()
    }
}
