extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::parse_macro_input;

struct Combinations {
    name: syn::Ident,
    s: syn::LitStr,
}

impl Parse for Combinations {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        input.parse::<syn::Token![,]>()?;
        let s = input.parse()?;
        Ok(Combinations { name, s })
    }
}

#[proc_macro]
pub fn minimal(input: TokenStream) -> TokenStream {
    let Combinations { name, s } = parse_macro_input!(input as Combinations);
    let expand = quote! {
        fn #name() {
            println!("{}", #s);
        }
    };

    expand.into()
}
