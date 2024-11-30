use crate::codegen::{Func, FuncDef, FuncSig, LetBlock};
use crate::explicit_types::TypesList;
use itertools::Itertools;
use proc_macro2::{Group, Ident, TokenStream, TokenTree};
use quote::ToTokens;
use syn::parse::discouraged::Speculative;
use syn::parse::{self, Parse};
use syn::{parse2, LitFloat, LitInt, Token, Type};

#[derive(Clone)]
pub enum SIRNode {
    Ident(TokenStream),
    Literal(syn::Lit),
    Ref(Box<SIRNode>),
    FuncLike(FuncLike),
}

#[derive(Clone)]
pub enum FuncLike {
    FuncDef(FuncDef),
    LetBlock(LetBlock),
    Func(Func),
    IfBlock(IfBlock),
    LoopBlock(LoopBlock),
    RecurBlock(RecurBlock),
    MethodBlock(MethodBlock),
    AwaitBlock(Box<SIRNode>),
    AsyncBlock(Box<SIRNode>),
}

#[derive(Clone)]
pub struct MethodBlock {
    pub method: Type,
    pub method_self: Box<SIRNode>,
    pub args: Vec<SIRNode>,
}

#[derive(Clone)]
pub struct LoopBlock(pub LetBlock);

#[derive(Clone)]
pub struct RecurBlock(pub LetAssignments);

impl Parse for RecurBlock {
    fn parse(input: parse::ParseStream) -> syn::Result<Self> {
        if input.peek(syn::Ident) {
            input.parse::<Ident>()?;
        };

        let letblock = input.parse::<Group>()?;
        let letblock_parsed = parse2::<LetAssignments>(letblock.stream())?;

        Ok(RecurBlock(letblock_parsed))
    }
}

impl LoopBlock {
    fn new(mut input: LetBlock) -> Self {
        for a in input.assignments.0.iter_mut() {
            a.mutable = true;
        }
        LoopBlock(input)
    }
}

impl From<LoopBlock> for LetBlock {
    fn from(value: LoopBlock) -> Self {
        value.0
    }
}

#[derive(Clone)]
pub struct IfBlock {
    pub predicate: Box<SIRNode>,
    pub true_branch: Box<SIRNode>,
    pub false_branch: Box<SIRNode>,
}

impl Parse for LoopBlock {
    fn parse(input: parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Token![loop]) {
            input.parse::<Token![loop]>()?;
        }
        let letblock = input.parse::<LetBlock>();

        Ok(LoopBlock::new(letblock?))
    }
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

// TODO: Support negative numbers
impl Parse for SIRNode {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Token![&]) {
            input.parse::<Token![&]>()?;
            return Ok(SIRNode::Ref(Box::new(input.parse::<SIRNode>()?)));
        }
        if input.peek(Token![-]) && input.peek2(LitInt) {
            let int = input.parse::<LitInt>()?;
            return Ok(SIRNode::Literal(syn::Lit::Int(int)));
        }
        if input.peek(Token![-]) && input.peek2(LitFloat) {
            let fl = input.parse::<LitFloat>()?;
            return Ok(SIRNode::Literal(syn::Lit::Float(fl)));
        }

        let fork = input.fork();
        let tt_parsed = input.parse::<TokenTree>()?;

        match tt_parsed {
            TokenTree::Group(g) => Ok(SIRNode::FuncLike(parse2::<FuncLike>(g.stream())?)),
            TokenTree::Ident(_) => {
                let ty = fork.parse::<Type>()?;
                input.advance_to(&fork);
                let typestring = ty.to_token_stream();
                Ok(SIRNode::Ident(typestring))
            }
            TokenTree::Punct(_) => Err(input.error("punctuation not allowed here")),
            TokenTree::Literal(l) => Ok(SIRNode::Literal(syn::Lit::Verbatim(l.clone()))),
        }
    }
}

#[derive(Clone)]
pub struct LetAssignment {
    pub mutable: bool,
    pub name: String,
    pub val: SIRNode,
}

#[derive(Clone)]
pub struct LetAssignments(pub Vec<LetAssignment>);

impl Parse for LetAssignments {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut assignments = vec![];
        while let Ok(la) = input.parse::<LetAssignment>() {
            assignments.push(la);
        }
        Ok(LetAssignments(assignments))
    }
}

impl Parse for LetAssignment {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name = input.parse::<Ident>()?.to_string();
        let val = input.parse::<SIRNode>()?;
        Ok(LetAssignment {
            name,
            val,
            mutable: false,
        })
    }
}

// needs async/await
impl Parse for FuncDef {
    fn parse(input: parse::ParseStream) -> syn::Result<Self> {
        let mut is_async = false;
        let fork = input.fork();
        if let Ok(id) = fork.parse::<Ident>() {
            println!("{}", id);
            if id == "async_fn" {
                input.advance_to(&fork);
                is_async = true;
            }
        }

        if input.peek(Token![fn]) {
            input.parse::<Token![fn]>()?;
        }

        // we want users to be able to omit these and end up with nones
        // actually just consume them
        let argsfork = input.fork();
        let mut args: Vec<(String, crate::explicit_types::Type)> = vec![];
        loop {
            let id = argsfork.parse::<Ident>();
            let typename = argsfork.parse::<crate::explicit_types::Type>();

            if id.is_err() || typename.is_err() {
                break;
            }
            input.advance_to(&argsfork);
            args.push((id?.to_string(), typename?));
        }
        let return_type = if input.peek(Token![-]) && input.peek2(Token![>]) {
            input.parse::<Token![-]>()?;
            input.parse::<Token![>]>()?;
            let ty = input.parse::<crate::explicit_types::Type>()?;
            Some(ty)
        } else {
            None
        };

        let mut body = vec![];

        while let Ok(n) = input.parse::<SIRNode>() {
            body.push(n);
        }
        Ok(FuncDef {
            is_async,
            body,
            signature: FuncSig { return_type, args },
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

impl Parse for crate::explicit_types::Type {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(crate::explicit_types::Type(input.parse()?))
    }
}

impl Parse for TypesList {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut types: Vec<crate::explicit_types::Type> = vec![];
        while let Ok(p) = input.parse() {
            types.push(p);
        }
        Ok(TypesList(types))
    }
}

impl Parse for IfBlock {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Token![if]) {
            let _ = input.parse::<Token![if]>();
        }
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

impl Parse for MethodBlock {
    fn parse(input: parse::ParseStream) -> syn::Result<Self> {
        input.parse::<Token![.]>()?;

        let method_self = Box::new(input.parse::<SIRNode>()?);
        let method = input.parse::<Type>()?;

        let mut args = vec![];

        while let Ok(arg) = input.parse::<SIRNode>() {
            args.push(arg);
        }

        Ok(MethodBlock {
            method,
            method_self,
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
        if input.peek(Token![loop]) {
            let fl = input.parse::<LoopBlock>();
            let fl = FuncLike::LoopBlock(fl?);
            return Ok(fl);
        }
        if input.peek(Token![.]) {
            let fl = input.parse::<MethodBlock>();
            let fl = FuncLike::MethodBlock(fl?);
            return Ok(fl);
        }
        if input.peek(Token![await]) {
            input.parse::<Token![await]>()?;
            let fl = Box::new(input.parse::<SIRNode>()?);
            let fl = FuncLike::AwaitBlock(fl);
            return Ok(fl);
        }
        if input.peek(Token![async]) {
            input.parse::<Token![async]>()?;
            let fl = Box::new(input.parse::<SIRNode>()?);
            let fl = FuncLike::AsyncBlock(fl);
            return Ok(fl);
        }
        let fork = input.fork();
        if let Ok(id) = fork.parse::<Ident>() {
            if id == "async_fn" {
                let fl = input.parse::<FuncDef>();
                let fl = FuncLike::FuncDef(fl?);
                return Ok(fl);
            }
            if id == "recur" {
                input.advance_to(&fork);
                return Ok(FuncLike::RecurBlock(input.parse::<RecurBlock>()?));
            }
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
