use anyhow::Result;
use proc_macro2::Group;
use std::fmt::Debug;
use std::fmt::Write;
use syn::{parse::Parse, parse2};

impl std::fmt::Debug for TypesList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('{')?;
        for ty in &self.0 {
            let name =
                ty.0.span
                    .source_text()
                    .unwrap_or("[missing type]".to_string())
                    .to_string();
            f.write_str(&name)?;
            f.write_char(',')?;
        }
        f.write_char('}')?;
        Ok(())
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}",
            &self
                .0
                .span
                .source_text()
                .unwrap_or("[missing type]".to_string())
        ))?;
        Ok(())
    }
}

#[derive(Clone)]
pub struct TypesList(pub Vec<Type>);

// impl Parse for TypesList {
//     fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
//         let mut types: Vec<Type> = vec![];
//         while let Ok(p) = input.parse() {
//             types.push(p);
//         }
//         Ok(TypesList(types))
//     }
// }

#[derive(Clone)]
pub struct Type(pub syn::token::Type);

// pub fn parse_typeslist(group: Group) -> Result<TypesList> {
//     let st = group.stream();
//     let parsed = parse2::<TypesList>(st)?;

//     Ok(parsed)
// }
