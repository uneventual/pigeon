use crate::codegen::{Func, FuncDef, LetAssignments, LetBlock, ValId};
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
}

fn defn_ast(_group: &Group) -> Result<FuncDef, CodeError> {
    todo!()
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
            "defn" => return Ok(defn_ast(group)?.into()),
            "let" => return Ok(let_ast(group)?.into()),
            // "->" => return Ok(functype_ast(group).context("-> arm")?),
            _ => (),
        }

        let evvec: Result<Vec<SIRNode>, CodeError> = st.skip(1).map(|m| m.to_sir()).collect();
        Ok(SIRNode::Func(Func {
            name: name_st,
            args: evvec?,
        }))
    }
}

pub trait SIRParse {
    fn to_sir(&self) -> Result<SIRNode, CodeError>;
}
