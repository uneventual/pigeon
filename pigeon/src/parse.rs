use crate::codegen::{Func, FuncDef, FuncSig, LetAssignments, LetBlock};
use crate::explicit_types::TypesList;
use itertools::Itertools;
use proc_macro2::{Group, Ident, TokenTree};
use quote::ToTokens;
use syn::parse::{self, Parse};
use syn::{parse2, Token};

use crate::codegen::SyntaxErrorable;
use proc_macro2::Literal;

#[derive(Clone, Debug)]
pub enum SIRNode {
    Ident(String),
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
        let its = input.parse::<TokenTree>()?;
        Ok(match its {
            TokenTree::Group(g) => Ok(SIRNode::FuncLike(parse2::<FuncLike>(g.stream())?)),
            TokenTree::Ident(id) => Ok(SIRNode::Ident(id.to_string())),
            TokenTree::Punct(_) => Err(its.error("punctuation not allowed here")),
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

// this needs to accept generic sirnodes as the first argument
impl Parse for Func {
    fn parse(input: parse::ParseStream) -> syn::Result<Self> {
        let fun = input.parse::<Ident>()?;

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

// impl SIRParse for Group {
//     fn to_sir(&self) -> Result<SIRNode, CodeError> {
//         let group = self;
//         if group.delimiter() != Delimiter::Parenthesis {
//             return Err(
//                 group.error("We can only parse parenthesis groups. This is a compiler bug!")
//             )?;
//         }

//         let funcstart = parse2::<FuncStart>(self.stream());

//         if let Err(e) = funcstart {
//             return Err(group.error(&e.to_string()));
//         }

//         let funcstart = funcstart.unwrap();

//         let name_st = funcstart.start;

//         let stst = name_st.to_string();

//         if stst == "fn" {
//             return Ok(SIRNode::FuncLike(FuncLike::FuncDef(
//                 parse2::<FuncDef>(self.stream()).unwrap(),
//             )));
//         }
//         if stst == "let" {
//             let letblock = parse2::<LetBlock>(self.stream()).unwrap();
//             return Ok(SIRNode::FuncLike(FuncLike::LetBlock(letblock)));
//         }

//         if stst == "if" {
//             let ifblock = parse2::<IfBlock>(funcstart.rest.clone())
//                 .map_err(|_| group.error("if statement parse failed"))?;

//             return Ok(SIRNode::FuncLike(FuncLike::IfBlock(ifblock)));
//         }

//         // so we want to take this and make it a for loop instead that joins together
//         // parts that are separated by punctuation and tries to parse them as types
//         let mut evvec = vec![];
//         let mut refdepth = 0;
//         for root in funcstart.rest {
//             if let TokenTree::Punct(p) = &root {
//                 if p.as_char() == '&' {
//                     refdepth += 1;
//                 } else {
//                     return Err(root.error("this punctuation not allowed here"));
//                 }
//             } else {
//                 let mut sir = root.to_sir()?;
//                 for _ in 0..refdepth {
//                     sir = SIRNode::Ref(Box::new(sir))
//                 }
//                 refdepth = 0;
//                 evvec.push(sir);
//             }
//         }
//         Ok(SIRNode::FuncLike(FuncLike::Func(Func {
//             name: name_st,
//             args: evvec,
//         })))
//     }
// }

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
