use std::fmt::Write;

use crate::codegen::{Func, FuncDef, LetAssignment, LetBlock, ValId};
use anyhow::{anyhow, Context, Result};
use itertools::Itertools;
use proc_macro2::{token_stream::IntoIter, Delimiter, Group, TokenTree};
use syn::{parse::Parse, parse2, token::Type};

use proc_macro2::{token_stream::TokenStream, Literal};

#[derive(Clone, Debug)]
pub enum SIRNode {
    Func(Func),
    Ident(String),
    Literal(Literal),
    Stat(ValId),
    FuncDef(FuncDef),
    LetBlock(LetBlock),
    TokenBlock(TokenStream),
    TypesList(TypesList),
}

fn defn_ast(group: Group) -> Result<SIRNode> {
    let ivs = Err(anyhow::anyhow!(
        "Invalid syntax: let block requires [variable definition]"
    ));
    let mut st = group.stream().into_iter().skip(1);

    let name = match st.next().context("function needs a name")? {
        TokenTree::Ident(n) => Some(n),
        _ => None,
    }
    .context("function needs a name")?
    .to_string();

    // let resulttype = if let Some(TokenTree::Group(func_type)) = st.next() {
    //     if func_type.delimiter() == Delimiter::Bracket {
    //         functype_ast(func_type)?
    //     } else {
    //         return ivs;
    //     }
    // } else {
    //     return ivs;
    // };

    // if let Some(TokenTree::Group(arguments)) = st.next() {
    //     if arguments.delimiter() == Delimiter::Bracket {
    //         let assignments_stream = arguments.stream().into_iter();

    //         let mut assignments_vector: Vec<String> = vec![];

    //         for mut name in assignments_stream.into_iter() {
    //             match name {
    //                 TokenTree::Ident(s) => assignments_vector.push(s.to_string()),
    //                 _ => return ivs,
    //             };
    //         }

    //         let evaluables: Vec<Evaluable> = st.map(|m| any_evaluable(m).unwrap()).collect();

    //         let block = LetBlock {
    //             assignments: assignments_vector,
    //             body: evaluables,
    //         };

    //         // return Ok(Evaluable::LetBlock(block));

    //         let defn = FuncDef {
    //             name,
    //             args: assignments_vector,
    //             body: evaluables,
    //             signature: resulttype,
    //         };

    //         return Ok(Evaluable::FuncDef(defn));
    //     }
    // }
    ivs
}

fn let_ast(group: Group) -> Result<SIRNode> {
    let syn_error = Err(anyhow::anyhow!(
        "Invalid syntax: let block requires [variable definition]"
    ));
    let mut after_let = group.stream().into_iter().skip(1);
    if let Some(TokenTree::Group(assignments)) = after_let.next() {
        if assignments.delimiter() == Delimiter::Bracket {
            let assignments_stream = assignments.stream().into_iter().chunks(2);

            let mut assignments_vector: Vec<LetAssignment> = vec![];

            for mut c in assignments_stream.into_iter() {
                let uneven = "uneven number of args to let block";
                let name = c.next().context(uneven)?;
                let value = c.next().context(uneven)?;
                match name {
                    TokenTree::Ident(s) => assignments_vector.push(LetAssignment {
                        name: s.to_string(),
                        val: any_evaluable(value).unwrap(),
                    }),
                    _ => return syn_error,
                };
            }

            let evaluables: Vec<SIRNode> = after_let.map(|m| any_evaluable(m).unwrap()).collect();

            let block = LetBlock {
                assignments: assignments_vector,
                body: evaluables,
            };

            return Ok(SIRNode::LetBlock(block));
        }
    }
    syn_error
}

// okay i think that syn is just fully useless here

// fn functype_ast(group: Group) -> Result<SIRNode> {
//     let funcerr = anyhow!("couldn't parse functype definition");

//     let st = group.stream();

//     Ok(parse2(st)?)
// }

impl std::fmt::Debug for TypesList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('{');
        for ty in &self.0 {
            let name = format!(
                "{}",
                &ty.span
                    .source_text()
                    .unwrap_or("[missing type]".to_string())
            );
            f.write_str(&name)?;
            f.write_char(',')?;
        }
        f.write_char('}')?;
        Ok(())
    }
}

impl std::fmt::Debug for SingleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}",
            &self
                .0
                .span
                .source_text()
                .unwrap_or("[missing type]".to_string())
        ));
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

#[derive(Clone)]
pub struct SingleType(pub Type);

fn any_evaluable(tt: TokenTree) -> Result<SIRNode> {
    match tt {
        TokenTree::Group(g) => Ok(paren_to_ast(g)?),
        TokenTree::Ident(id) => Ok(SIRNode::Ident(id.to_string())),
        TokenTree::Punct(_) => Err(anyhow!("invalid syntax")),
        TokenTree::Literal(l) => Ok(SIRNode::Literal(l)),
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
            TokenTree::Ident(id) => return Ok(id.to_string()),
            TokenTree::Punct(p) => return Ok(p.to_string() + &eat_puncts(st)),
            _ => return Err(anyhow!("invalid first argument")),
        }
    } else {
        Err(anyhow!("empty group"))
    }
}

pub fn paren_to_ast(group: Group) -> Result<SIRNode> {
    assert!(group.delimiter() == Delimiter::Parenthesis);
    let mut st = group.stream().into_iter();

    let name_st = eat_start(&group)?;

    match name_st.as_str() {
        "defn" => return Ok(defn_ast(group).context("defn arm")?),
        "let" => return Ok(let_ast(group).context("let arm")?),
        // "->" => return Ok(functype_ast(group).context("-> arm")?),
        _ => (),
    }

    let evvec: Vec<SIRNode> = st.skip(1).map(|m| any_evaluable(m).unwrap()).collect();
    Ok(SIRNode::Func(Func {
        name: name_st,
        args: evvec,
    }))
}
