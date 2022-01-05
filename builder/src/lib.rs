use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Ident, TokenTree};
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

fn is_type(ty: &syn::Type, type_str: &str) -> bool {
    if let syn::Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.first() {
            return format!("{}", segment.ident) == type_str;
        }
    }
    false
}

fn unwrap_type(ty: &syn::Type, unwrap_ty: &str) -> syn::Type {
    if let syn::Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.first() {
            assert!(format!("{}", segment.ident) == unwrap_ty);
            if let syn::PathArguments::AngleBracketed(angle_args) = &segment.arguments {
                if let syn::GenericArgument::Type(inner_ty) = angle_args.args.first().unwrap() {
                    return inner_ty.clone();
                }
            }
        }
    }
    unreachable!()
}

fn get_each_metadata(field: &syn::Field) -> Option<(Ident, syn::Type)> {
    if field.attrs.len() == 0 {
        return None;
    } else if field.attrs.len() > 1 {
        panic!("invalid number of attrs")
    }
    if let Some(attr) = field.attrs.first() {
        if format!("{}", attr.path.segments.first().unwrap().ident) == "builder" {
            let tokens: Vec<_> = attr.clone().tokens.into_iter().collect();
            if tokens.len() != 1 {
                unreachable!()
            }
            if let Some(TokenTree::Group(group)) = tokens.first().clone() {
                assert!(group.delimiter() == Delimiter::Parenthesis);
                let tokens = group.stream().into_iter().collect::<Vec<_>>();
                assert!(tokens.len() == 3);
                if let proc_macro2::TokenTree::Ident(ident) = &tokens[0] {
                    if format!("{}", &ident) != "each" {
                        panic!("{} must equal 'each'", &ident);
                    }
                    if !is_type(&field.ty, "Vec") {
                        panic!("expected Vec type for 'each'");
                    }
                } else {
                    panic!("expected ident")
                }
                if let proc_macro2::TokenTree::Punct(punct) = &tokens[1] {
                    if punct.as_char() != '=' {
                        panic!("{} must equal '='", &punct);
                    }
                } else {
                    panic!("expected '='")
                }
                if let proc_macro2::TokenTree::Literal(lit) = &tokens[2] {
                    let lit_string = lit.to_string();
                    assert!(
                        &lit_string[0..1] == "\"" && &lit_string[lit_string.len() - 1..] == "\"",
                        "literal must be a string: {:#?}",
                        lit_string,
                    );
                    let each_fn_ident =
                        syn::Ident::new(&lit_string[1..&lit_string.len() - 1], lit.span());
                    let each_fn_input_type = unwrap_type(&field.ty, "Vec");
                    return Some((each_fn_ident, each_fn_input_type));
                } else {
                    panic!(
                        "expected string literal of function name, received {}",
                        &tokens[2]
                    );
                };
            }
            unreachable!()
        }
        return None;
    }
    unreachable!()
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let name = &ast.ident;
    let builder_name = format!("{}Builder", name);
    let builder_ident = syn::Ident::new(&builder_name, name.span());
    let fields = if let syn::Data::Struct(data) = ast.data {
        data.fields
    } else {
        unimplemented!()
    };
    let builder_fields = fields.iter().map(|field| {
        let field_name = &field.ident;
        let field_type = &field.ty;
        if is_type(field_type, "Option") {
            quote! {
                #field_name: #field_type,
            }
        } else {
            quote! {
                #field_name: std::option::Option<#field_type>,
            }
        }
    });

    let builder_field_defaults = fields.iter().map(|field| {
        let field_name = field.ident.clone().unwrap();
        if let Some(_) = get_each_metadata(field) {
            quote! {
                #field_name: Some(Vec::new()),
            }
        } else {
            quote! {
                #field_name: None,
            }
        }
    });

    let builder_methods = fields.iter().map(|field| {
        let field_name = &field.ident.clone().unwrap();
        let field_type = &field.ty;
        if is_type(field_type, "Option") {
            let inner_type = unwrap_type(field_type, "Option");
            quote! {
                pub fn #field_name(&mut self, #field_name: #inner_type) -> &mut #builder_ident {
                    self.#field_name = Some(#field_name);
                    self
                }
            }
        } else {
            let foo = Some(vec![1, 2, 3]);
            foo.unwrap_or(Vec::new()).push(3);

            let each_fn = if let Some((each_ident, each_ty)) = get_each_metadata(field) {
                let each_fn_dec = quote! {
                    pub fn #each_ident(&mut self, #each_ident: #each_ty) -> &mut #builder_ident {
                        let mut v = self.#field_name.get_or_insert(Vec::new());
                        v.push(#each_ident);
                        self
                    }
                };
                if field_name.to_string() == each_ident.to_string() {
                    return each_fn_dec;
                }
                each_fn_dec
            } else {
                quote! {}
            };
            quote! {
                pub fn #field_name(&mut self, #field_name: #field_type) -> &mut #builder_ident {
                    self.#field_name = Some(#field_name);
                    self
                }

                #each_fn
            }
        }
    });

    let builder_build_fields = fields.iter().map(|field| {
        let field_name = field.ident.clone().unwrap();
        let error_msg = format!("{} not set!", field_name);
        if let Some(_) = get_each_metadata(field) {
            quote! {
                #field_name: self.#field_name.clone().unwrap(),
            }
        } else {
            if is_type(&field.ty, "Option") {
                quote! {
                    #field_name: self.#field_name.clone(),
                }
            } else {
                quote! {
                    #field_name: self.#field_name.clone().ok_or(#error_msg)?,
                }
            }
        }
    });

    quote! {
        pub struct #builder_ident {
            #(#builder_fields)*
        }
        impl #name {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#builder_field_defaults)*
                }
            }
        }
        impl #builder_ident {
            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok (
                    #name {
                        #(#builder_build_fields)*
                    }
                )
            }

            #(#builder_methods)*
        }
    }
    .into()
}
