use proc_macro::{self, TokenStream};
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Data};

#[proc_macro_derive(FlatDebug)]
pub fn flatdebug_derive(input: TokenStream) -> TokenStream {
	let ast: DeriveInput = parse_macro_input!(input);
	if let Data::Enum(enum_data) = ast.data {
		let ident = ast.ident;

		let variants: Vec<proc_macro2::TokenStream> = enum_data.variants.iter().map(|v|{
				let variant = &v.ident;
				quote!{ Self::#variant(data) => data.fmt(f), }
		}).collect();

		let output = quote! {
			impl std::fmt::Debug for #ident<'_> {
				fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
					match self {
						#(#variants)*
					}
				}
			}
		};
		
		return output.into();
	}
	TokenStream::new()
}