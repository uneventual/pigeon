use crate::codegen::{Func, FuncDef, FuncSig, LetAssignments, LetBlock};
use crate::explicit_types::TypesList;
use itertools::Itertools;
use proc_macro2::{Group, Ident, TokenStream, TokenTree};
use quote::ToTokens;
use syn::parse::discouraged::Speculative;
use syn::parse::{self, Parse};
use syn::{parse2, Token, Type};

use crate::codegen::SyntaxErrorable;
use proc_macro2::Literal;

#[derive(Clone, Debug)]
pub enum SIRNode {
    Ident(TokenStream),
    Literal(Literal),
    Ref(Box<SIRNode>),
    FuncLike(FuncLike),
}

#[derive(Clone, Debug)]
pub enum FuncLike {
    FuncDef(FuncDef),
    LetBlock(LetBlock),
    Func(Func),
    IfBlock(IfBlock),
    LoopBlock(),
}

#[derive(Clone, Debug)]
pub struct IfBlock {
    pub predicate: Box<SIRNode>,
    pub true_branch: Box<SIRNode>,
    pub false_branch: Box<SIRNode>,
}

impl Parse for LetBlock {
    fn parse(input: parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Token![let]) {
            input.parse::<Token![let]>()?;
        }

        let letblock = input.parse::<Group>()?;
        let letblock_parsed = parse2::<LetAssignments>(letblock.stream())?;

        let mut body = vec![];
        while let Ok(sn) = input.parse::<SIRNode>() {
            body.push(sn);
        }

        Ok(LetBlock {
            assignments: letblock_parsed,
            body,
        })
    }
}

impl Parse for SIRNode {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        while input.peek(Token![&]) {
            input.parse::<Token![&]>()?;
            return Ok(SIRNode::Ref(Box::new(input.parse::<SIRNode>()?)));
        }

        let fork = input.fork();
        let tt_parsed = input.parse::<TokenTree>()?;

        Ok(match tt_parsed {
            TokenTree::Group(g) => Ok(SIRNode::FuncLike(parse2::<FuncLike>(g.stream())?)),
            TokenTree::Ident(_) => {
                let ty = fork.parse::<Type>()?;
                input.advance_to(&fork);
                let typestring = ty.to_token_stream();
                Ok(SIRNode::Ident(typestring))
            }
            TokenTree::Punct(_) => Err(tt_parsed.error("punctuation not allowed here")),
            TokenTree::Literal(l) => Ok(SIRNode::Literal(l.clone())),
        }?)
    }
}

struct IdentList(Vec<String>);

impl Parse for IdentList {
    fn parse(input: parse::ParseStream) -> syn::Result<Self> {
        let mut idents = Vec::new();
        while !input.is_empty() {
            let id = input.parse::<Ident>()?;
            idents.push(id.to_string())
        }
        Ok(IdentList(idents))
    }
}

impl Parse for FuncDef {
    fn parse(input: parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Token![fn]) {
            let _ = input.parse::<Token![fn]>()?;
        }
        let argslist = parse2::<IdentList>(input.parse::<Group>()?.stream())?;
        let typeslist = parse2::<TypesList>(input.parse::<Group>()?.stream())?;
        let mut body = vec![];

        while let Ok(n) = input.parse::<SIRNode>() {
            body.push(n);
        }

        let (last, rest) = typeslist.0.split_last().unwrap();

        Ok(FuncDef {
            body,
            signature: FuncSig {
                return_type: last.clone(),
                args: argslist
                    .0
                    .into_iter()
                    .zip(rest.iter().cloned())
                    .collect_vec(),
            },
        })
    }
}

impl Parse for Func {
    fn parse(input: parse::ParseStream) -> syn::Result<Self> {
        let fun = input.parse::<SIRNode>()?;

        let mut args = vec![];
        while let Ok(arg) = input.parse::<SIRNode>() {
            args.push(arg);
        }

        Ok(Func {
            name: fun.to_token_stream(),
            args,
        })
    }
}

impl Parse for FuncLike {
    fn parse(input: parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Token![fn]) {
            let fl = input.parse::<FuncDef>();
            let fl = FuncLike::FuncDef(fl?);
            return Ok(fl);
        }
        if input.peek(Token![let]) {
            let fl = input.parse::<LetBlock>();

            let fl = FuncLike::LetBlock(fl?);
            return Ok(fl);
        }
        if input.peek(Token![if]) {
            let fl = input.parse::<IfBlock>();
            let fl = FuncLike::IfBlock(fl?);
            return Ok(fl);
        }

        Ok(FuncLike::Func(input.parse::<Func>()?))
    }
}

#[cfg(test)]
mod tests {
    use quote::quote;

    use super::*;

    #[test]
    fn parse_typeslist_test() {
        let quoth = quote!( (fn [a b] [i32 i32 i32] (mul a (add b 1))) );
        let group = parse2::<Group>(quoth).unwrap();
        let parsed = parse2::<FuncDef>(group.stream());

        assert!(parsed.is_ok());
    }
}
