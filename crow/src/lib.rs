extern crate proc_macro;
use std::fmt::Write;

use anyhow::{anyhow, Context, Result};
use itertools::Itertools;
use proc_macro2::{
    token_stream::{IntoIter, TokenStream},
    Delimiter, Group, Literal, TokenTree,
};
use quote::{format_ident, quote, ToTokens};
use syn::{parse::Parse, parse2, token::Type};
mod codegen;

use codegen::ssa_block;

#[proc_macro]
pub fn crow(ts: proc_macro::TokenStream) -> proc_macro::TokenStream {
    proc_macro::TokenStream::from(crow2(TokenStream::from(ts)))
}

fn crow2(ts: TokenStream) -> TokenStream {
    if let Some(TokenTree::Group(root)) = ts.into_iter().next() {
        let ast = paren_to_ast(root);
        ssa_block(ast.unwrap())
    } else {
        panic!()
    }
}

fn to_valids(ev: &Evaluable) -> Option<ValId> {
    eprintln!("{:?}", ev);
    let x = match ev {
        Evaluable::Stat(valid) => Some(valid.clone()),
        Evaluable::Ident(st) => Some(ValId::Ident(format_ident!("{}", st))),
        _ => None,
    };
    eprintln!("{:?}", x);
    x
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

// todo:
// - qualified names
// - +/-*
// - references
// -- just carry through as part of ident ig
// - defn
// - quote/reflection/etc
// - go down list of clojure features
// - use blocks

// todo:
// - qualified names
// - +/-*
// - references
// -- just carry through as part of ident ig
// - defn
// - quote/reflection/etc
// - go down list of clojure features
// - use blocks

// 1. defn
// 2. let blocks

// function signature is defined like (-> arg1 arg2 arg3 return)

fn defn_ast(group: Group) -> Result<Evaluable> {
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

    let resulttype = if let Some(TokenTree::Group(func_type)) = st.next() {
        if func_type.delimiter() == Delimiter::Bracket {
            functype_ast(func_type)?
        } else {
            return ivs;
        }
    } else {
        return ivs;
    };

    if let Some(TokenTree::Group(arguments)) = st.next() {
        if arguments.delimiter() == Delimiter::Bracket {
            let assignments_stream = arguments.stream().into_iter();

            let mut assignments_vector: Vec<String> = vec![];

            for mut name in assignments_stream.into_iter() {
                match name {
                    TokenTree::Ident(s) => assignments_vector.push(s.to_string()),
                    _ => return ivs,
                };
            }

            let evaluables: Vec<Evaluable> = st.map(|m| any_evaluable(m).unwrap()).collect();

            let block = LetBlock {
                assignments: assignments_vector,
                body: evaluables,
            };

            // return Ok(Evaluable::LetBlock(block));

            let defn = FuncDef {
                name,
                args: assignments_vector,
                body: evaluables,
                signature: resulttype,
            };

            return Ok(Evaluable::FuncDef(defn));
        }
    }
    ivs
}

fn let_ast(group: Group) -> Result<Evaluable> {
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

            let evaluables: Vec<Evaluable> = after_let.map(|m| any_evaluable(m).unwrap()).collect();

            let block = LetBlock {
                assignments: assignments_vector,
                body: evaluables,
            };

            return Ok(Evaluable::LetBlock(block));
        }
    }
    syn_error
}

// okay i think that syn is just fully useless here

fn functype_ast(group: Group) -> Result<TypesList> {
    let funcerr = anyhow!("couldn't parse functype definition");

    let st = group.stream();

    Ok(parse2(st)?)
}

impl std::fmt::Debug for TypesList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('{');
        for ty in self.0 {
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

#[derive(Clone)]
struct TypesList(pub Vec<Type>);

impl Parse for TypesList {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut types: Vec<Type> = vec![];
        while let Ok(p) = input.parse() {
            types.push(p);
        }
        Ok(TypesList(types))
    }
}

fn any_evaluable(tt: TokenTree) -> Result<Evaluable> {
    match tt {
        TokenTree::Group(g) => Ok(paren_to_ast(g)?),
        TokenTree::Ident(id) => Ok(Evaluable::Ident(id.to_string())),
        TokenTree::Punct(_) => Err(anyhow!("invalid syntax")),
        TokenTree::Literal(l) => Ok(Evaluable::Literal(l)),
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

fn paren_to_ast(group: Group) -> Result<Evaluable> {
    assert!(group.delimiter() == Delimiter::Parenthesis);
    let mut st = group.stream().into_iter();

    let name_st = eat_start(&group)?;

    match name_st.as_str() {
        "defn" => return Ok(defn_ast(group).context("defn arm")?),
        "let" => return Ok(let_ast(group).context("let arm")?),
        "->" => return Ok(functype_ast(group).context("-> arm")?),
        _ => (),
    }

    let evvec: Vec<Evaluable> = st.skip(1).map(|m| any_evaluable(m).unwrap()).collect();
    Ok(Evaluable::Func(Func {
        name: name_st,
        args: evvec,
    }))
}

// what do we need here to make it actually work?
// uh parse imports, types, lifetimes
// be able to declare functions
#[derive(Clone, Debug)]
enum ValId {
    Reference(usize),
    Ident(proc_macro2::Ident),
}

#[derive(Clone, Debug)]
struct FuncDef {
    name: String,
    args: Vec<String>,
    body: Vec<Evaluable>,
    signature: TypesList,
}

#[derive(Clone, Debug)]
struct FuncArg {
    typename: String,
    name: String,
}

#[derive(Clone, Debug)]
struct LetBlock {
    assignments: Vec<LetAssignment>,
    body: Vec<Evaluable>,
}

#[derive(Clone, Debug)]
struct LetAssignment {
    name: String,
    val: Evaluable,
}

impl ToTokens for LetAssignment {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = format_ident!("{}", self.name);
        let val = &self.val;
        let quote = quote!(let #name = #val;);
        tokens.extend(quote);
    }
}

#[derive(Clone, Debug)]
enum Evaluable {
    Func(Func),
    Ident(String),
    Literal(Literal),
    Stat(ValId),
    FuncDef(FuncDef),
    LetBlock(LetBlock),
    TokenBlock(TokenStream),
    TypesList(TypesList),
}

impl ToTokens for FuncArg {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.name;
        let typename = &self.typename;
        let quote = quote!(#name: #typename);

        tokens.extend(quote);
    }
}

fn funcdef(fd: &FuncDef) -> TokenStream {
    let body_first = fd.body[..fd.body.len() - 1].iter();
    let body_last = &fd.body[fd.body.len() - 1];
    let args = fd.args.iter();
    let name = &fd.name;
    let result = &fd.resulttype;

    quote!(fn #name(#(#args),*) -> #result {
        #(#body_first);*
        #body_last
    })
}

fn letblock(lb: &LetBlock) -> TokenStream {
    let body = lb.body.clone().into_iter().map(ssa_block);
    let mut blocks = lb.assignments.iter();

    let mut last_b: Option<Evaluable> = None;

    quote!({ #(#blocks)*  { #(#body)* } };)
}

impl ToTokens for Evaluable {
    fn to_tokens(&self, stream: &mut proc_macro2::TokenStream) {
        let toks = match self {
            Evaluable::Func(f) => funcfrom(f),
            Evaluable::Ident(s) => {
                let id = format_ident!("{}", s);
                quote!(#id)
            }
            Evaluable::Literal(l) => quote!(#l),
            Evaluable::Stat(s) => quote!(#s),
            Evaluable::FuncDef(f) => funcdef(f),
            Evaluable::LetBlock(l) => letblock(l),
            Evaluable::TokenBlock(t) => t.clone(),
            Evaluable::TypesList(_) => todo!(),
        };
        stream.extend(toks);
    }
}

#[derive(Clone, Debug)]
struct Func {
    pub name: String,
    pub args: Vec<Evaluable>,
}
