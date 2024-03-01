#![feature(iter_intersperse)]
extern crate proc_macro;
use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Span, TokenStream, TokenTree};

#[proc_macro]
pub fn crow(ts: TokenStream) -> TokenStream {
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

        stream.extend(vec![TokenTree::Ident(Ident::new(
            &usize_name(0),
            Span::call_site(),
        ))]);
        let mut return_stream = TokenStream::new();

        return_stream.extend(vec![TokenTree::Group(Group::new(Delimiter::Brace, stream))]);
        return_stream
    } else {
        panic!()
    }
}

fn usize_name(v: usize) -> String {
    format!("__{}", v)
}

fn letline(ev: Evaluable, num: usize) -> Vec<TokenTree> {
    let prelude = vec![
        TokenTree::Ident(proc_macro::Ident::new("let", proc_macro::Span::call_site())),
        TokenTree::Ident(proc_macro::Ident::new(
            &usize_name(num),
            proc_macro::Span::call_site(),
        )),
        TokenTree::Punct(proc_macro::Punct::new('=', proc_macro::Spacing::Alone)),
    ];

    let val = match ev {
        Evaluable::Func(f) => funcfrom(f),
        Evaluable::Ident(s) => vec![TokenTree::Ident(Ident::new(&s, Span::call_site()))],
        Evaluable::Literal(l) => vec![TokenTree::Literal(l)],
        Evaluable::Stat(s) => vec![TokenTree::Ident(Ident::new(
            &usize_name(s),
            Span::call_site(),
        ))],
    };

    let postlude = vec![TokenTree::Punct(proc_macro::Punct::new(
        ';',
        proc_macro::Spacing::Alone,
    ))];

    vec![prelude, val, postlude].into_iter().flatten().collect()
}

fn to_valids(args: Vec<Evaluable>) -> Vec<ValId> {
    args.into_iter()
        .filter_map(|ev| match ev {
            Evaluable::Stat(valid) => Some(valid),
            _ => None,
        })
        .collect()
}

fn funcfrom(func: Func) -> Vec<TokenTree> {
    let args = to_valids(func.args);
    vec![
        TokenTree::Ident(proc_macro::Ident::new(
            &func.name,
            proc_macro::Span::call_site(),
        )),
        TokenTree::Group(Group::new(Delimiter::Parenthesis, commasep(args))),
    ]
}

fn commasep(vals: Vec<usize>) -> TokenStream {
    let comma = TokenTree::Punct(Punct::new(',', proc_macro::Spacing::Alone));
    vals.into_iter()
        .map(|s| TokenTree::Ident(Ident::new(&usize_name(s), proc_macro::Span::call_site())))
        .intersperse(comma)
        .collect()
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

#[derive(Clone, Debug)]
struct Func {
    pub name: String,
    pub args: Vec<Evaluable>,
}
