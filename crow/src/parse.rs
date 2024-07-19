use crate::codegen::{Func, FuncDef, LetBlock, ValId};
use anyhow::{anyhow, Context, Result};
use proc_macro2::{token_stream::IntoIter, Delimiter, Group, TokenTree};

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

fn defn_ast(group: &Group) -> Result<SIRNode> {
    todo!()
}

fn let_ast(group: &Group) -> Result<SIRNode> {
    todo!()
}

impl SIRParse for TokenTree {
    fn to_sir(&self) -> Result<SIRNode> {
        let tt = self;
        match tt {
            TokenTree::Group(g) => Ok(g.to_sir()?),
            TokenTree::Ident(id) => Ok(SIRNode::Ident(id.to_string())),
            TokenTree::Punct(_) => Err(anyhow!("invalid syntax")),
            TokenTree::Literal(l) => Ok(SIRNode::Literal(l.clone())),
        }
    }
}

fn eat_puncts(it: IntoIter) -> String {
    it.filter(|i| matches!(i, TokenTree::Punct(_)))
        .map(|i| i.to_string())
        .collect::<String>()
}

fn eat_start(group: &Group) -> Result<String> {
    let mut st = group.stream().into_iter();

    if let Some(first) = st.next() {
        match first {
            TokenTree::Ident(id) => Ok(id.to_string()),
            TokenTree::Punct(p) => Ok(p.to_string() + &eat_puncts(st)),
            _ => Err(anyhow!("invalid first argument")),
        }
    } else {
        Err(anyhow!("empty group"))
    }
}

impl SIRParse for Group {
    fn to_sir(&self) -> Result<SIRNode> {
        let group = self;
        assert!(group.delimiter() == Delimiter::Parenthesis);
        let st = group.stream().into_iter();

        let name_st = eat_start(&group)?;

        match name_st.as_str() {
            "defn" => return defn_ast(group).context("defn arm"),
            "let" => return let_ast(group).context("let arm"),
            // "->" => return Ok(functype_ast(group).context("-> arm")?),
            _ => (),
        }

        let evvec: Vec<SIRNode> = st.skip(1).map(|m| m.to_sir().unwrap()).collect();
        Ok(SIRNode::Func(Func {
            name: name_st,
            args: evvec,
        }))
    }
}

pub trait SIRParse {
    fn to_sir(&self) -> Result<SIRNode>;
}
