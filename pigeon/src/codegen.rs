use proc_macro2::token_stream::TokenStream;
use proc_macro2::{Ident, Span, TokenTree};
use syn::parse::Parse;

use crate::explicit_types::Type;

use crate::parse::{FuncLike, IfBlock, SIRNode};
use quote::{format_ident, quote, ToTokens};
use std::fmt::Debug;

impl ToTokens for Func {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let func = self;
        let args = func.args.iter();
        let name = &func.name;
        let quo = quote!(#name(#(#args),*));
        tokens.extend(quo);
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
        Ok(LetAssignment { name, val })
    }
}

use std::error::Error;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct CodeError {
    // ought to be guaranteed to be nonempty; make a constructor
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

impl From<CodeError> for syn::Error {
    fn from(value: CodeError) -> Self {
        let first = value.errors.last().unwrap();
        let node = first.node.clone();
        let message = first.message.clone();
        syn::Error::new_spanned(node, message)
    }
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

impl ToTokens for LetAssignment {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = format_ident!("{}", self.name);
        let val = &self.val;
        let quote = quote!(let #name = #val;);
        tokens.extend(quote);
    }
}

impl ToTokens for LetBlock {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let body = self.body.clone().into_iter().map(|f| quote!({ #f }));
        let blocks = self.assignments.0.iter();

        let _last_b: Option<SIRNode> = None;

        let quo = quote!({ #(#blocks)*  { #(#body)* } });
        tokens.extend(quo)
    }
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

impl ToTokens for IfBlock {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let predicate = self.predicate.clone();
        let true_arm = self.true_branch.clone();
        let false_arm = self.false_branch.clone();
        let ifblock = quote!(if #predicate {#true_arm} else {#false_arm});
        tokens.extend(ifblock)
    }
}

impl ToTokens for FuncLike {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let toks = match self {
            FuncLike::FuncDef(func_def) => quote!(#func_def),
            FuncLike::LetBlock(let_block) => quote!(#let_block),
            FuncLike::Func(func) => quote!(#func),
            FuncLike::IfBlock(if_block) => quote!(#if_block),
            FuncLike::LoopBlock() => todo!(),
        };
        tokens.extend(toks);
    }
}

impl ToTokens for SIRNode {
    fn to_tokens(&self, stream: &mut proc_macro2::TokenStream) {
        let toks = match self {
            SIRNode::Ident(s) => {
                let id = format_ident!("{}", s);
                quote!(#id)
            }
            SIRNode::Literal(l) => quote!(#l),
            SIRNode::Ref(r) => quote!(&#r),
            SIRNode::FuncLike(f) => quote!(#f),
        };
        stream.extend(toks);
    }
}

#[derive(Clone, Debug)]
pub struct Func {
    pub name: TokenStream,
    pub args: Vec<SIRNode>,
}
