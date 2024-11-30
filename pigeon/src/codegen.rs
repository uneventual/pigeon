use proc_macro2::token_stream::TokenStream;

use crate::explicit_types::Type;

use crate::parse::{
    FuncLike, IfBlock, LetAssignment, LetAssignments, LoopBlock, MethodBlock, RecurBlock, SIRNode,
};
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
    pub is_async: bool,
    pub body: Vec<SIRNode>,
    pub signature: FuncSig,
}

#[derive(Clone, Debug)]
pub struct FuncSig {
    pub return_type: Option<Type>,
    pub args: Vec<(String, Type)>,
}

#[derive(Clone)]
pub struct LetBlock {
    pub assignments: LetAssignments,
    pub body: Vec<SIRNode>,
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

        let args = quote! {
        |#(#args),*|};

        let rettype = if let Some(rt) = return_type {
            quote!(-> #rt)
        } else {
            quote!()
        };

        let block = quote! {
             {
                #(#body)*
            }
        };

        let optionally_async = if self.is_async {
            quote!(async)
        } else {
            quote!()
        };

        let closure = quote! {#optionally_async #args  #rettype #block};

        tokens.extend(closure);
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

impl ToTokens for MethodBlock {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ms = self.method_self.clone();
        let method = self.method.clone();
        let args = self.args.clone();
        tokens.extend(quote!(#ms.#method(#(#args),*)));
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
            FuncLike::MethodBlock(mb) => quote!(#mb),
            FuncLike::AwaitBlock(ab) => quote!({#ab}.await),
            FuncLike::AsyncBlock(sirnode) => quote!(async {#sirnode}),
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

#[derive(Clone)]
pub struct Func {
    pub name: TokenStream,
    pub args: Vec<SIRNode>,
}
