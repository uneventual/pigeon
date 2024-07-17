use crate::{Evaluable, ValId};
use proc_macro2::token_stream::TokenStream;

use quote::{format_ident, quote};

pub fn ssa_block(ast: Evaluable) -> TokenStream {
    eprintln!("{:?}", ast);
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
                    *arg = Evaluable::Stat(ValId::Reference(addr));
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

fn letline(ev: Evaluable, num: usize) -> TokenStream {
    let noname = usize_name(num);
    quote!(let #noname = #ev ;)
}
