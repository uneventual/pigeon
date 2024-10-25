use itertools::Itertools;
use proc_macro2::token_stream::TokenStream;
use proc_macro2::{Delimiter, Group, Span, TokenTree};

use crate::explicit_types::Type;

use crate::parse::SIRNode;
use crate::parse::SIRParse;
use quote::{format_ident, quote, ToTokens};
use std::fmt::Debug;

pub fn ssa_block(ast: SIRNode) -> TokenStream {
    let mut bfs = vec![ast];

    let mut i: usize = 0;
    while i < bfs.len() {
        let mut block_vector = {
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
            bv
        };

        bfs.append(&mut block_vector);
        i += 1;
    }

    let mut stream = TokenStream::new();

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

#[derive(Clone, Debug)]
pub enum ValId {
    Reference(usize),
}

impl ToTokens for Func {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let func = self;
        let args = func.args.iter();
        let name = format_ident!("{}", func.name);
        let quo = quote!(#name(#(#args),*));
        tokens.extend(quo);
    }
}

impl ToTokens for ValId {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let tok = match self {
            Self::Reference(r) => usize_name(*r),
        };
        tokens.extend(tok);
    }
}

#[derive(Clone, Debug)]
pub struct FuncDef {
    pub body: Vec<SIRNode>,
    pub signature: FuncSig,
}

#[derive(Clone, Debug)]
pub struct FuncSig {
    pub return_type: Type,
    pub args: Vec<(String, Type)>,
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

impl ToTokens for FuncDef {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let args = self.signature.args.iter().map(|(arg_name, arg_type)| {
            let arg_ident = format_ident!("{}", arg_name);
            quote!(#arg_ident: #arg_type)
        });
        let return_type = &self.signature.return_type;
        let body = &self.body;

        let func_def = quote! {
            |#(#args),*| -> #return_type {
                #(#body)*
            }
        };

        tokens.extend(func_def);
    }
}

impl ToTokens for SIRNode {
    fn to_tokens(&self, stream: &mut proc_macro2::TokenStream) {
        let toks = match self {
            SIRNode::Func(f) => quote!(#f),
            SIRNode::Ident(s) => {
                let id = format_ident!("{}", s);
                quote!(#id)
            }
            SIRNode::Literal(l) => quote!(#l),
            SIRNode::Stat(s) => quote!(#s),
            SIRNode::FuncDef(f) => quote!(#f),
            SIRNode::LetBlock(l) => letblock(l),
            SIRNode::Ref(r) => quote!(&#r),
        };
        stream.extend(toks);
    }
}

#[derive(Clone, Debug)]
pub struct Func {
    pub name: String,
    pub args: Vec<SIRNode>,
}
