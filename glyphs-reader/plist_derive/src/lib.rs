extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::{quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{parse_macro_input, Data, DeriveInput, Fields};

mod attrs;

#[proc_macro_derive(FromPlist, attributes(fromplist))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let deser = match add_deser(&input) {
        Ok(thing) => thing,
        Err(e) => return e.into_compile_error().into(),
    };

    let name = input.ident;

    let expanded = quote! {
        impl crate::from_plist::FromPlist for #name {
            fn from_plist(plist: crate::plist::Plist) -> Self {
                let mut map = plist.into_btreemap();
                #name {
                    #deser
                }
            }
        }
    };
    proc_macro::TokenStream::from(expanded)
}

#[proc_macro_derive(ToPlist, attributes(fromplist))]
pub fn derive_to(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match derive_to_impl(&input) {
        Ok(thing) => thing,
        Err(e) => e.into_compile_error().into(),
    }
}

fn derive_to_impl(input: &DeriveInput) -> syn::Result<proc_macro::TokenStream> {
    let name = &input.ident;

    let ser_rest = add_ser_rest(&input.data)?;
    let ser = add_ser(&input.data);

    let expanded = quote! {
        impl crate::to_plist::ToPlist for #name {
            fn to_plist(self) -> crate::plist::Plist {
                #ser_rest
                #ser
                map.into()
            }
        }
    };
    Ok(proc_macro::TokenStream::from(expanded))
}

fn add_deser(input: &DeriveInput) -> syn::Result<TokenStream> {
    let Data::Struct(data) = &input.data else {
        return Err(syn::Error::new(
            input.ident.span(),
            "FromPlist only supports structs",
        ));
    };

    let Fields::Named(fields) = &data.fields else {
        return Err(syn::Error::new(
            input.ident.span(),
            "FromPlist only supports named fields",
        ));
    };

    let fields = fields.named.iter().map(|f| {
        let attrs = attrs::FieldAttrs::from_attrs(&f.attrs)?;
        let name = &f.ident;
        if attrs.rest.is_none() {
            let name_str = name.as_ref().unwrap().to_string();
            let snake_name = snake_to_camel_case(&name_str);
            if attrs.default.is_none() {
                Ok(quote_spanned! {f.span() =>
                    #name: crate::from_plist::FromPlistOpt::from_plist(
                        map.remove(#snake_name)
                    ),
                })
            } else {
                Ok(quote_spanned! {f.span() =>
                    #name: map.remove(#snake_name).map(crate::from_plist::FromPlist::from_plist).unwrap_or_default(),
                })
            }
        } else {
            Ok(quote_spanned! {f.span() =>
                #name: map,
            })
        }
        }).collect::<Result<Vec<_>, syn::Error>>()?;

    Ok(quote! {
        #( #fields )*
    })
}

fn add_ser(data: &Data) -> TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let recurse = fields.named.iter().filter_map(|f| {
                    let attrs = attrs::FieldAttrs::from_attrs(&f.attrs)
                        .expect("already checked in add_der");
                    if attrs.rest.is_none() {
                        let name = &f.ident;
                        let name_str = name.as_ref().unwrap().to_string();
                        let snake_name = snake_to_camel_case(&name_str);
                        Some(quote_spanned! {f.span() =>
                            if let Some(plist) = crate::to_plist::ToPlistOpt::to_plist(self.#name) {
                                map.insert(#snake_name.to_string(), plist);
                            }
                        })
                    } else {
                        None
                    }
                });
                quote! {
                    #( #recurse )*
                }
            }
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

fn add_ser_rest(data: &Data) -> syn::Result<TokenStream> {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                for f in fields.named.iter() {
                    let attrs = attrs::FieldAttrs::from_attrs(&f.attrs)?;
                    if attrs.rest.is_some() {
                        let name = &f.ident;
                        return Ok(quote_spanned! { f.span() =>
                            let mut map = self.#name;
                        });
                    }
                }
                Ok(quote! { let mut map = BTreeMap::new(); })
            }
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

fn snake_to_camel_case(id: &str) -> String {
    let mut result = String::new();
    let mut hump = false;
    for c in id.chars() {
        if c == '_' {
            hump = true;
        } else {
            if hump {
                result.push(c.to_ascii_uppercase());
            } else {
                result.push(c);
            }
            hump = false;
        }
    }
    result
}

/*
fn to_snake_case(id: &str) -> String {
    let mut result = String::new();
    for c in id.chars() {
        if c.is_ascii_uppercase() {
            result.push('_');
            result.push(c.to_ascii_lowercase());
        } else {
            result.push(c);
        }
    }
    result
}
*/
