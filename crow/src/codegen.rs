use proc_macro2::token_stream::TokenStream;

use crate::explicit_types::{SingleType, TypesList};
use crate::parse::SIRNode;
use quote::{format_ident, quote, ToTokens};

pub fn ssa_block(ast: SIRNode) -> TokenStream {
    eprintln!("{:?}", ast);
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

    eprintln!("{:?}", bfs);
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
    eprintln!("{:?}", ev);
    let x = match ev {
        SIRNode::Stat(valid) => Some(valid.clone()),
        SIRNode::Ident(st) => Some(ValId::Ident(format_ident!("{}", st))),
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

// function signature is defined like (-> arg1 arg2 arg3 return)

// okay so how do we do this
// we only care a

#[derive(Clone, Debug)]
pub struct FuncDef {
    pub name: String,
    pub args: Vec<String>,
    pub body: Vec<SIRNode>,
    pub signature: TypesList,
    pub result_type: SingleType,
}

#[derive(Clone, Debug)]
pub struct FuncArg {
    typename: String,
    name: String,
}

#[derive(Clone, Debug)]
pub struct LetBlock {
    pub assignments: Vec<LetAssignment>,
    pub body: Vec<SIRNode>,
}

#[derive(Clone, Debug)]
pub struct LetAssignment {
    pub name: String,
    pub val: SIRNode,
}

impl ToTokens for LetAssignment {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = format_ident!("{}", self.name);
        let val = &self.val;
        let quote = quote!(let #name = #val;);
        tokens.extend(quote);
    }
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
    let result = &fd.result_type;

    quote!(fn #name(#(#args),*) -> #result {
        #(#body_first);*
        #body_last
    })
}

fn letblock(lb: &LetBlock) -> TokenStream {
    let body = lb.body.clone().into_iter().map(ssa_block);
    let blocks = lb.assignments.iter();

    let _last_b: Option<SIRNode> = None;

    quote!({ #(#blocks)*  { #(#body)* } };)
}

impl ToTokens for SingleType {
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
            SIRNode::FuncDef(f) => funcdef(f),
            SIRNode::LetBlock(l) => letblock(l),
            SIRNode::TokenBlock(t) => t.clone(),
            SIRNode::TypesList(_) => todo!(),
        };
        stream.extend(toks);
    }
}

#[derive(Clone, Debug)]
pub struct Func {
    pub name: String,
    pub args: Vec<SIRNode>,
}
