use std::fmt::Write;

use crate::parse::{IfBlock, SIRNode};
use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{parse::Parse, Token};

impl std::fmt::Debug for TypesList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('[')?;
        for ty in &self.0 {
            f.write_fmt(format_args!("{:?}", ty))?;
            f.write_str(", ")?;
        }
        f.write_char(']')?;
        Ok(())
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ft = self.0.to_token_stream().to_string();
        let st = match self.0 {
            syn::Type::Array(_) => "Array",
            syn::Type::BareFn(_) => "BareFn",
            syn::Type::Group(_) => "Group",
            syn::Type::ImplTrait(_) => "ImplTrait",
            syn::Type::Infer(_) => "Infer",
            syn::Type::Macro(_) => "Macro",
            syn::Type::Never(_) => "Never",
            syn::Type::Paren(_) => "Paren",
            syn::Type::Path(_) => "Path",
            syn::Type::Ptr(_) => "Ptr",
            syn::Type::Reference(_) => "Reference",
            syn::Type::Slice(_) => "Slice",
            syn::Type::TraitObject(_) => "TraitObject",
            syn::Type::Tuple(_) => "Tuple",
            syn::Type::Verbatim(_) => "Verbatim",
            _ => "Unknown",
        };

        f.write_fmt(format_args!("{}({})", st, &ft))?;
        Ok(())
    }
}

#[derive(Clone)]
pub struct TypesList(pub Vec<Type>);

impl Parse for TypesList {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut types: Vec<Type> = vec![];
        while let Ok(p) = input.parse() {
            types.push(p);
        }
        Ok(TypesList(types))
    }
}

pub struct FuncStart {
    pub start: TokenStream,
    pub rest: TokenStream,
}

impl Parse for IfBlock {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let predicate = Box::new(input.parse::<SIRNode>()?);
        let true_branch = Box::new(input.parse::<SIRNode>()?);
        let false_branch = Box::new(input.parse::<SIRNode>()?);
        Ok(IfBlock {
            predicate,
            true_branch,
            false_branch,
        })
    }
}

impl Parse for FuncStart {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut toks = TokenStream::new();
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![fn]) {
            let tok = input.parse::<Token![fn]>()?;
            tok.to_tokens(&mut toks);
        } else if lookahead.peek(Token![let]) {
            let tok = input.parse::<Token![let]>()?;
            tok.to_tokens(&mut toks);
        } else if lookahead.peek(Token![if]) {
            let tok = input.parse::<Token![if]>()?;
            tok.to_tokens(&mut toks);
        } else if let Ok(ty) = input.parse::<Type>() {
            ty.to_tokens(&mut toks);
        };

        let ts = input.parse::<TokenStream>()?;

        Ok(FuncStart {
            start: toks,
            rest: ts,
        })
    }
}

#[derive(Clone)]
pub struct Type(pub syn::Type);

impl Parse for Type {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Type(input.parse()?))
    }
}

#[cfg(test)]
mod tests {
    use quote::quote;

    use super::*;

    #[test]
    fn parse_typeslist_test() {
        let list = quote!(String &str Vec<u8>);

        let types = syn::parse2::<TypesList>(list).unwrap();
        assert!(matches!(types.0[0].0, syn::Type::Path(_)));
    }
}
