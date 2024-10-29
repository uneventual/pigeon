use crate::codegen::{Func, FuncDef, FuncSig, LetAssignments, LetBlock};
use crate::explicit_types::{FuncStart, Type, TypesList};
use anyhow::Result;
use proc_macro2::{Delimiter, Group, TokenTree};
use syn::parse::Parse;
use syn::parse2;

use crate::codegen::{CodeError, SyntaxErrorable};
use proc_macro2::Literal;

#[derive(Clone, Debug)]
pub enum SIRNode {
    Func(Func),
    Ident(String),
    Literal(Literal),
    FuncDef(FuncDef),
    LetBlock(LetBlock),
    Ref(Box<SIRNode>),
    IfBlock(IfBlock),
    LoopBlock(),
}

#[derive(Clone, Debug)]
pub struct IfBlock {
    pub predicate: Box<SIRNode>,
    pub true_branch: Box<SIRNode>,
    pub false_branch: Box<SIRNode>,
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

impl Parse for SIRNode {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let its = input.parse::<TokenTree>()?;
        let sir = its.to_sir();
        match sir {
            Ok(s) => return Ok(s),
            Err(e) => return Err(e.into()),
        }
    }
}

impl SIRParse for Group {
    fn to_sir(&self) -> Result<SIRNode, CodeError> {
        let group = self;
        if group.delimiter() != Delimiter::Parenthesis {
            return Err(
                group.error("We can only parse parenthesis groups. This is a compiler bug!")
            )?;
        }

        let funcstart = parse2::<FuncStart>(self.stream());

        if let Err(e) = funcstart {
            return Err(group.error(&e.to_string()));
        }

        let funcstart = funcstart.unwrap();

        let name_st = funcstart.start;

        match name_st.to_string().as_str() {
            "fn" => return Ok(fn_ast(group)?.into()),
            "let" => return Ok(let_ast(group)?.into()),
            "if" => {
                let ifparse = parse2(funcstart.rest.clone());
                let if_unerror = match ifparse {
                    Ok(v) => Ok(v),
                    Err(_) => Err(group.error("failed to parse if statement")),
                }?;
                return Ok(SIRNode::IfBlock(if_unerror));
            }
            // "->" => return Ok(functype_ast(group).context("-> arm")?),
            _ => (),
        }

        // so we want to take this and make it a for loop instead that joins together
        // parts that are separated by punctuation and tries to parse them as types
        let mut evvec = vec![];
        let mut refdepth = 0;
        for root in funcstart.rest {
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
        Ok(SIRNode::Func(Func {
            name: name_st,
            args: evvec,
        }))
    }
}

pub trait SIRParse {
    fn to_sir(&self) -> Result<SIRNode, CodeError>;
}
