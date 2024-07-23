use itertools::Itertools;
use proc_macro2::token_stream::TokenStream;
use proc_macro2::{Delimiter, Group, Span, TokenTree};
use syn::spanned::Spanned;

use crate::explicit_types::{Type};

use crate::parse::SIRNode;
use crate::parse::SIRParse;
use quote::{format_ident, quote, ToTokens};
use std::fmt::Debug;


pub fn ssa_block(ast: SIRNode) -> TokenStream {
    // eprintln!("{:?}", ast);
    let mut bfs = vec![ast];

    let mut i: usize = 0;
    while i < bfs.len() {
        let mut bv = {
            let mut addr = bfs.len();
            let b = &mut bfs[i];
            let mut bv: Vec<SIRNode> = vec![];
            if let SIRNode::Func(func) = b {
                for arg in func.args.iter_mut() {
                    bv.push(arg.clone());
                    *arg = SIRNode::Stat(ValId::Reference(addr));
                    addr += 1;
                }
            };
            // probably we want to put this in the letblock function actually

            bv
        };

        bfs.append(&mut bv);
        i += 1;
    }

    let mut stream = TokenStream::new();

    // eprintln!("{:?}", bfs);
    for (i, ev) in bfs.into_iter().enumerate().rev() {
        stream.extend(letline(ev, i));
    }

    stream.extend(usize_name(0));

    let ret = quote!({ #stream });

    ret
}

fn usize_name(id: usize) -> TokenStream {
    let idf = format_ident!("__{}", id);
    quote!(#idf)
}

fn letline(ev: SIRNode, num: usize) -> TokenStream {
    let noname = usize_name(num);
    quote!(let #noname = #ev ;)
}

// what do we need here to make it actually work?
// uh parse imports, types, lifetimes
// be able to declare functions
#[derive(Clone, Debug)]
pub enum ValId {
    Reference(usize),
    Ident(proc_macro2::Ident),
}

fn to_valids(ev: &SIRNode) -> Option<ValId> {
    // eprintln!("{:?}", ev);
    
    // eprintln!("{:?}", x);
    match ev {
        SIRNode::Stat(valid) => Some(valid.clone()),
        SIRNode::Ident(st) => Some(ValId::Ident(format_ident!("{}", st))),
        _ => None,
    }
}

fn funcfrom(func: &Func) -> TokenStream {
    let args = func.args.iter().filter_map(to_valids);
    let name = format_ident!("{}", func.name);
    let quo = quote!(#name(#(#args),*));
    quo
}

impl ToTokens for ValId {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let tok = match self {
            Self::Ident(id) => quote!(#id),
            Self::Reference(r) => usize_name(*r),
        };
        tokens.extend(tok);
    }
}

// function signature is defined like (-> arg1 arg2 arg3 return)

// okay so how do we do this
// we only care a

#[derive(Clone, Debug)]
pub struct FuncDef {
    pub name: String,
    pub body: Vec<SIRNode>,
    pub signature: FuncSig,
}

#[derive(Clone, Debug)]
pub struct FuncSig {
    return_type: Type,
    args: Vec<(String, Type)>,
}

trait BSDebug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
}

#[derive(Clone, Debug)]
pub struct LetBlock {
    pub assignments: LetAssignments,
    pub body: Vec<SIRNode>,
}

#[derive(Clone, Debug)]
pub struct LetAssignment {
    pub name: String,
    pub val: SIRNode,
}

#[derive(Clone, Debug)]
pub struct LetAssignments(Vec<LetAssignment>);

use std::error::Error;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct CodeError {
    errors: Vec<CodeErrorInstance>,
}

impl CodeError {
    pub fn span(&self) -> Span {
        let last_error = self.errors.last().unwrap();
        last_error.error_span.unwrap_or(last_error.node.span())
    }
}

#[derive(Debug, Clone)]
pub struct CodeErrorInstance {
    message: String,
    node: TokenTree,
    error_span: Option<Span>,
}

impl From<CodeErrorInstance> for CodeError {
    fn from(value: CodeErrorInstance) -> Self {
        CodeError {
            errors: vec![value],
        }
    }
}

impl Display for CodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Error: ")?;
        for e in &self.errors {
            f.write_fmt(format_args!("{} \n", e.message))?;

            // if let Some(source_text) = e.node.span().source_text() {
            //     f.write_fmt(format_args!("{}\n", source_text))?;
            // }
        }
        Ok(())
    }
}

impl Error for CodeError {}

impl TryFrom<&Group> for LetAssignments {
    type Error = CodeError;
    fn try_from(value: &Group) -> std::result::Result<Self, Self::Error> {
        if !matches!(value.delimiter(), Delimiter::Bracket) {
            return Err(value.error("let block must be delimited by [square brackets]"));
        }

        let x: Result<Vec<_>, Self::Error> = value
            .stream()
            .into_iter()
            .chunks(2)
            .into_iter()
            .map(|mut vv| {
                let err = value.error("unmatched pairs in let block");

                let name = vv
                    .next()
                    .ok_or(err.clone())?
                    .ident_string()
                    .context(value.error("let blocks require [ident sexpr] pairs"))?;
                let value = vv.next().ok_or(err.clone())?.to_sir()?;

                Ok(LetAssignment { name, val: value })
            })
            .collect();

        Ok(LetAssignments(x?))
    }
}

pub trait IdentString {
    fn ident_string(&self) -> Result<String, CodeError>;
}

impl IdentString for TokenTree {
    fn ident_string(&self) -> Result<String, CodeError> {
        if let TokenTree::Ident(i) = self {
            Ok(i.to_string())
        } else {
            Err(self.error(&format!("\"{}\" is not an ident", self)))
        }
    }
}

trait Contextable<T> {
    fn context(self, context: CodeError) -> Result<T, CodeError>;
}

impl<T> Contextable<T> for Result<T, CodeError> {
    fn context(self, context: CodeError) -> Result<T, CodeError> {
        match self {
            Ok(s) => Ok(s),
            Err(mut s) => {
                s.errors.extend(context.errors);
                Err(s)
            }
        }
    }
}

pub trait SyntaxErrorable {
    fn error(&self, message: &str) -> CodeError;
}

impl<T: Into<TokenTree> + Clone> SyntaxErrorable for T {
    fn error(&self, message: &str) -> CodeError {
        let t: TokenTree = self.clone().into();
        CodeErrorInstance {
            message: message.to_string(),
            node: t,
            error_span: None,
        }
        .into()
    }
}

impl From<LetBlock> for SIRNode {
    fn from(value: LetBlock) -> Self {
        SIRNode::LetBlock(value)
    }
}

impl From<FuncDef> for SIRNode {
    fn from(value: FuncDef) -> Self {
        SIRNode::FuncDef(value)
    }
}

impl ToTokens for LetAssignment {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = format_ident!("{}", self.name);
        let val = &self.val;
        let quote = quote!(let #name = #val;);
        tokens.extend(quote);
    }
}

// impl ToTokens for FuncArg {
//     fn to_tokens(&self, tokens: &mut TokenStream) {
//         let name = &self.name;
//         let typename = &self.typename;
//         let quote = quote!(#name: #typename);

//         tokens.extend(quote);
//     }
// }

// fn funcdef(fd: &FuncDef) -> TokenStream {
//     let body_first = fd.body[..fd.body.len() - 1].iter();
//     let body_last = &fd.body[fd.body.len() - 1];
//     let args = fd.args.iter();
//     let name = &fd.name;
//     let result = &fd.result_type;

//     quote!(fn #name(#(#args),*) -> #result {
//         #(#body_first);*
//         #body_last
//     })
// }

fn letblock(lb: &LetBlock) -> TokenStream {
    let body = lb.body.clone().into_iter().map(ssa_block);
    let blocks = lb.assignments.0.iter();

    let _last_b: Option<SIRNode> = None;

    quote!({ #(#blocks)*  { #(#body)* } };)
}

impl ToTokens for Type {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens)
    }
}

impl ToTokens for SIRNode {
    fn to_tokens(&self, stream: &mut proc_macro2::TokenStream) {
        let toks = match self {
            SIRNode::Func(f) => funcfrom(f),
            SIRNode::Ident(s) => {
                let id = format_ident!("{}", s);
                quote!(#id)
            }
            SIRNode::Literal(l) => quote!(#l),
            SIRNode::Stat(s) => quote!(#s),
            SIRNode::FuncDef(_f) => todo!(),
            SIRNode::LetBlock(l) => letblock(l),
        };
        stream.extend(toks);
    }
}

#[derive(Clone, Debug)]
pub struct Func {
    pub name: String,
    pub args: Vec<SIRNode>,
}
