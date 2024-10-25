use crate::codegen::{Func, FuncDef, FuncSig, LetAssignments, LetBlock, ValId};
use crate::explicit_types::{Type, TypesList};
use anyhow::Result;
use proc_macro2::{token_stream::IntoIter, Delimiter, Group, TokenTree};

use crate::codegen::{CodeError, SyntaxErrorable};
use proc_macro2::Literal;

#[derive(Clone, Debug)]
pub enum SIRNode {
    Func(Func),
    Ident(String),
    Literal(Literal),
    Stat(ValId),
    FuncDef(FuncDef),
    LetBlock(LetBlock),
    Ref(Box<SIRNode>),
}

fn fn_ast(group: &Group) -> Result<FuncDef, CodeError> {
    let mut st = group.stream().into_iter();

    // Skip the "fn" token
    st.next();

    // Parse the argument names
    let args: Vec<String> = match st.next() {
        Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Bracket => g
            .stream()
            .into_iter()
            .filter_map(|tt| {
                if let TokenTree::Ident(id) = tt {
                    Some(id.to_string())
                } else {
                    None
                }
            })
            .collect(),
        _ => return Err(group.error("Expected argument list in brackets after function name")),
    };

    // Parse the types
    let types = match st.next() {
        Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Bracket => {
            match syn::parse2::<TypesList>(g.stream()) {
                Ok(types_list) => types_list,
                Err(_) => return Err(group.error("Failed to parse types list")),
            }
        }
        _ => return Err(group.error("Expected types list in brackets after argument list")),
    };

    // Parse the function body
    let body = match st.next() {
        Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Parenthesis => g.to_sir()?,
        _ => return Err(group.error("Expected function body in parentheses")),
    };

    // Construct the FuncSig
    let return_type = types
        .0
        .last()
        .cloned()
        .unwrap_or_else(|| Type(syn::parse_str("()").unwrap()));
    let arg_types = types
        .0
        .iter()
        .take(types.0.len() - 1)
        .cloned()
        .collect::<Vec<_>>();
    let signature = FuncSig {
        return_type,
        args: args.into_iter().zip(arg_types).collect(),
    };

    // Construct the FuncDef
    Ok(FuncDef {
        body: vec![body],
        signature,
    })
}

fn let_ast(group: &Group) -> Result<LetBlock, CodeError> {
    let mut st = group.stream().into_iter();

    st.next();
    let letblock = st
        .next()
        .ok_or(group.error("let blocks require a valid [name assignment] block"))?;
    let letblock_parsed: Result<LetAssignments, CodeError> = match letblock {
        TokenTree::Group(g) => (&g).try_into(),
        _ => Err(letblock.error("let blocks require a valid [name assignment] block")),
    };

    let evaluables: Result<Vec<SIRNode>, CodeError> = st.map(|f| f.to_sir()).collect();

    Ok(LetBlock {
        assignments: letblock_parsed?,
        body: evaluables?,
    })
}

impl SIRParse for TokenTree {
    fn to_sir(&self) -> Result<SIRNode, CodeError> {
        let tt = self;
        match tt {
            TokenTree::Group(g) => Ok(g.to_sir()?),
            TokenTree::Ident(id) => Ok(SIRNode::Ident(id.to_string())),
            TokenTree::Punct(_) => Err(self.error("punctuation not allowed here")),
            TokenTree::Literal(l) => Ok(SIRNode::Literal(l.clone())),
        }
    }
}

fn eat_puncts(it: IntoIter) -> String {
    it.filter(|i| matches!(i, TokenTree::Punct(_)))
        .map(|i| i.to_string())
        .collect::<String>()
}

fn eat_start(group: &Group) -> Result<String, CodeError> {
    let mut st = group.stream().into_iter();

    if let Some(first) = st.next() {
        match first {
            TokenTree::Ident(id) => Ok(id.to_string()),
            TokenTree::Punct(p) => Ok(p.to_string() + &eat_puncts(st)),
            _ => Err(group.error("invalid first argument")),
        }
    } else {
        Err(group.error("empty group"))
    }
}

impl SIRParse for Group {
    fn to_sir(&self) -> Result<SIRNode, CodeError> {
        let group = self;
        assert!(group.delimiter() == Delimiter::Parenthesis);
        let st = group.stream().into_iter();

        let name_st = eat_start(group)?;

        match name_st.as_str() {
            "fn" => return Ok(fn_ast(group)?.into()),
            "let" => return Ok(let_ast(group)?.into()),
            // "->" => return Ok(functype_ast(group).context("-> arm")?),
            _ => (),
        }

        // so we want to take this and make it a for loop instead that joins together
        // parts that are separated by punctuation and tries to parse them as types
        let mut evvec = vec![];
        let mut refdepth = 0;
        for root in st.skip(1) {
            if let TokenTree::Punct(p) = &root {
                if p.as_char() == '&' {
                    refdepth += 1;
                } else {
                    return Err(root.error("this punctuation not allowed here"));
                }
            } else {
                let mut sir = root.to_sir()?;
                for _ in 0..refdepth {
                    sir = SIRNode::Ref(Box::new(sir))
                }
                refdepth = 0;
                evvec.push(sir);
            }
        }
        // let evvec: Result<Vec<SIRNode>, CodeError> = st.skip(1).map(|m| m.to_sir()).collect();
        Ok(SIRNode::Func(Func {
            name: name_st,
            args: evvec,
        }))
    }
}

pub trait SIRParse {
    fn to_sir(&self) -> Result<SIRNode, CodeError>;
}
