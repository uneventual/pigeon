#![feature(iter_intersperse)]
extern crate proc_macro;
use proc_macro2::{token_stream::TokenStream, Delimiter, Group, Literal, TokenTree};
use quote::{format_ident, quote, ToTokens};

#[proc_macro]
pub fn crow(ts: proc_macro::TokenStream) -> proc_macro::TokenStream {
    proc_macro::TokenStream::from(crow2(TokenStream::from(ts)))
}

fn crow2(ts: TokenStream) -> TokenStream {
    if let Some(TokenTree::Group(root)) = ts.into_iter().next() {
        let ast = to_ast(root);

        let mut bfs = vec![ast];

        let mut i: usize = 0;
        while i < bfs.len() {
            let mut bv = {
                let mut addr = bfs.len();
                let b = &mut bfs[i];
                let mut bv: Vec<Evaluable> = vec![];
                if let Evaluable::Func(func) = b {
                    for arg in func.args.iter_mut() {
                        bv.push(arg.clone());
                        *arg = Evaluable::Stat(addr);
                        addr += 1;
                    }
                };
                bv
            };

            bfs.append(&mut bv);
            i += 1;
        }

        let mut stream = TokenStream::new();

        for (i, ev) in bfs.into_iter().enumerate().rev() {
            stream.extend(letline(ev, i));
        }

        stream.extend(usize_name(0));

        let ret = quote!({ #stream });

        ret
    } else {
        panic!()
    }
}

fn letline(ev: Evaluable, num: usize) -> TokenStream {
    let noname = usize_name(num);
    quote!(let #noname = #ev ;)
}

fn to_valids(ev: &Evaluable) -> Option<ValId> {
    match ev {
        Evaluable::Stat(valid) => Some(*valid),
        _ => None,
    }
}

fn funcfrom(func: &Func) -> TokenStream {
    let args = func.args.iter().filter_map(to_valids).map(usize_name);
    let name = format_ident!("{}", func.name);
    let quo = quote!(#name(#(#args),*));
    quo
}

fn usize_name(id: usize) -> TokenStream {
    let idf = format_ident!("__{}", id);
    quote!(#idf)
}

fn to_ast(group: Group) -> Evaluable {
    assert!(group.delimiter() == Delimiter::Parenthesis);
    let mut st = group.stream().into_iter();
    if let Some(TokenTree::Ident(name)) = st.next() {
        let name_st = name.to_string();
        let evvec: Vec<Evaluable> = st
            .map(|m| match m {
                TokenTree::Group(g) => to_ast(g),
                TokenTree::Ident(id) => Evaluable::Ident(id.to_string()),
                TokenTree::Punct(_) => panic!(),
                TokenTree::Literal(l) => Evaluable::Literal(l),
            })
            .collect();
        Evaluable::Func(Func {
            name: name_st,
            args: evvec,
        })
    } else {
        panic!()
    }
}

type ValId = usize;

#[derive(Clone, Debug)]
enum Evaluable {
    Func(Func),
    Ident(String),
    Literal(Literal),
    Stat(ValId),
}

impl ToTokens for Evaluable {
    fn to_tokens(&self, stream: &mut proc_macro2::TokenStream) {
        let toks = match self {
            Evaluable::Func(f) => funcfrom(f),
            Evaluable::Ident(s) => quote!(#s),
            Evaluable::Literal(l) => quote!(#l),
            Evaluable::Stat(s) => usize_name(*s),
        };
        stream.extend(toks);
    }
}

#[derive(Clone, Debug)]
struct Func {
    pub name: String,
    pub args: Vec<Evaluable>,
}
