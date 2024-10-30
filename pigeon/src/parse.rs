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

        let (last, rest) = typeslist
            .0
            .split_last()
            .ok_or_else(|| input.error("needs at least a return type"))?;

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
        let fork = input.fork();
        if let Ok(id) = fork.parse::<Ident>() {
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
