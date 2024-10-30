use std::fmt::Write;

use quote::ToTokens;

impl std::fmt::Debug for TypesList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('[')?;
        for ty in &self.0 {
            f.write_fmt(format_args!("{:?}", ty))?;
            f.write_str(", ")?;
        }
        f.write_char(']')?;
        Ok(())
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ft = self.0.to_token_stream().to_string();
        let st = match self.0 {
            syn::Type::Array(_) => "Array",
            syn::Type::BareFn(_) => "BareFn",
            syn::Type::Group(_) => "Group",
            syn::Type::ImplTrait(_) => "ImplTrait",
            syn::Type::Infer(_) => "Infer",
            syn::Type::Macro(_) => "Macro",
            syn::Type::Never(_) => "Never",
            syn::Type::Paren(_) => "Paren",
            syn::Type::Path(_) => "Path",
            syn::Type::Ptr(_) => "Ptr",
            syn::Type::Reference(_) => "Reference",
            syn::Type::Slice(_) => "Slice",
            syn::Type::TraitObject(_) => "TraitObject",
            syn::Type::Tuple(_) => "Tuple",
            syn::Type::Verbatim(_) => "Verbatim",
            _ => "Unknown",
        };

        f.write_fmt(format_args!("{}({})", st, &ft))?;
        Ok(())
    }
}

#[derive(Clone)]
pub struct TypesList(pub Vec<Type>);

#[derive(Clone)]
pub struct Type(pub syn::Type);

#[cfg(test)]
mod tests {
    use quote::quote;

    use super::*;

    #[test]
    fn parse_typeslist_test() {
        let list = quote!(String &str Vec<u8>);

        let types = syn::parse2::<TypesList>(list).unwrap();
        assert!(matches!(types.0[0].0, syn::Type::Path(_)));
    }
}
