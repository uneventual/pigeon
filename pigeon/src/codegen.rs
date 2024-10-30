use proc_macro2::token_stream::TokenStream;
use proc_macro2::{Ident, Span, TokenTree};
use syn::parse::Parse;
use syn::token::Loop;

use crate::explicit_types::Type;

use crate::parse::{FuncLike, IfBlock, LoopBlock, RecurBlock, SIRNode};
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

#[derive(Clone)]
pub struct FuncDef {
    pub body: Vec<SIRNode>,
    pub signature: FuncSig,
}

#[derive(Clone, Debug)]
pub struct FuncSig {
    pub return_type: Type,
    pub args: Vec<(String, Type)>,
}

#[derive(Clone)]
pub struct LetBlock {
    pub assignments: LetAssignments,
    pub body: Vec<SIRNode>,
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

impl ToTokens for LetAssignment {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = format_ident!("{}", self.name);
        let val = &self.val;
        let quote = if self.mutable {
            quote!(let mut #name = #val;)
        } else {
            quote!(let #name = #val;)
        };

        tokens.extend(quote);
    }
}

const PIGEON_LOOP_RETURN: &str = "___pigeon_loop_return";

impl ToTokens for LoopBlock {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let body = self.0.body.clone().into_iter().map(|f| quote!({ #f }));
        let blocks = self.0.assignments.0.iter();

        let _last_b: Option<SIRNode> = None;

        let cont = format_ident!("{}", PIGEON_RECUR_CONTINUE);
        let return_placeholder = format_ident!("{}", PIGEON_LOOP_RETURN);

        let quo = quote!({ #(#blocks)* let mut #cont = true; let mut #return_placeholder: Option<_> = None; while #cont { #cont = false; #return_placeholder = Some(#(#body)*);  } #return_placeholder.unwrap()});
        tokens.extend(quo)
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

const PIGEON_RECUR_CONTINUE: &str = "___pigeon_loop_continue";

struct RecurAssignment<'a>(&'a LetAssignment);

impl<'a> From<&'a LetAssignment> for RecurAssignment<'a> {
    fn from(value: &'a LetAssignment) -> Self {
        RecurAssignment(value)
    }
}

impl<'a> ToTokens for RecurAssignment<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = format_ident!("{}", self.0.name);
        let val = &self.0.val;
        let quo = quote!(#name = #val;);

        tokens.extend(quo);
    }
}

impl ToTokens for RecurBlock {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let cont = format_ident!("{}", PIGEON_RECUR_CONTINUE);
        tokens.extend(quote!(#cont = true;));

        let blocks = self.0 .0.iter().map(|v| {
            let x: RecurAssignment = v.into();
            x
        });

        let quo = quote!( #(#blocks)*; continue;  );
        tokens.extend(quo)
    }
}

impl ToTokens for FuncLike {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let toks = match self {
            FuncLike::FuncDef(func_def) => quote!(#func_def),
            FuncLike::LetBlock(let_block) => quote!(#let_block),
            FuncLike::Func(func) => quote!(#func),
            FuncLike::IfBlock(if_block) => quote!(#if_block),
            FuncLike::LoopBlock(l) => {
                quote!(#l)
            }
            FuncLike::RecurBlock(r) => quote!(#r),
        };
        tokens.extend(toks);
    }
}

impl ToTokens for SIRNode {
    fn to_tokens(&self, stream: &mut proc_macro2::TokenStream) {
        let toks = match self {
            SIRNode::Ident(s) => s.clone(),
            SIRNode::Literal(l) => quote!(#l),
            SIRNode::Ref(r) => quote!(&#r),
            SIRNode::FuncLike(f) => quote!(#f),
        };
        stream.extend(toks);
    }
}

// just do loop/recur like clojure
// trace down from loop
// unconditionally return
// top level and bottom level function

fn trampoline() {
    // we call it
    // then inside we know what we return because it's inside the recur
    //
    let mut x = 0;
    let mut y = 0;
    let mut z = 0;
    let mut cont = true;
    let maybe = true;
    while !cont {
        cont = false;
        let x = if maybe {
            "HMM"
        } else {
            cont = true;
            continue;
        };
    }
}

#[derive(Clone)]
pub struct Func {
    pub name: TokenStream,
    pub args: Vec<SIRNode>,
}
