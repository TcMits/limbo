use quote::quote;
use std::collections::HashMap;

use proc_macro::TokenStream;
use syn::{parse_macro_input, spanned::Spanned, Arm, ExprMatch, Lit, Pat};

pub fn match_ignore_ascci_case(input: TokenStream) -> TokenStream {
    let match_block = parse_macro_input!(input as ExprMatch);
    if match_block.arms.is_empty() {
        return syn::Error::new(
            match_block.span(),
            "expected at least one arm with literal string/byte/bytes/char",
        )
        .to_compile_error()
        .into();
    }
    let mut arms: Vec<(Vec<u8>, Arm)> = Vec::with_capacity(match_block.arms.len());
    let mut fallback_arm: Option<Arm> = None;
    for arm in &match_block.arms {
        match &arm.pat {
            Pat::Lit(lit) => match &lit.lit {
                Lit::ByteStr(bs) => {
                    arms.push((bs.value().to_ascii_uppercase(), arm.clone()));
                }
                _ => {
                    return syn::Error::new(
                        arm.pat.span().span(),
                        "expected literal string/byte/bytes/char",
                    )
                    .to_compile_error()
                    .into();
                }
            },
            Pat::Wild(_) => {
                fallback_arm = Some(arm.clone());
            }
            Pat::Or(or) => {
                for case in &or.cases {
                    match case {
                        Pat::Lit(lit) => match &lit.lit {
                            Lit::ByteStr(bs) => {
                                arms.push((bs.value().to_ascii_uppercase(), arm.clone()));
                            }
                            _ => {
                                return syn::Error::new(
                                    arm.pat.span().span(),
                                    "expected literal string/byte/bytes/char",
                                )
                                .to_compile_error()
                                .into();
                            }
                        },
                        _ => {
                            return syn::Error::new(
                                arm.pat.span().span(),
                                "expected literal string/byte/bytes/char",
                            )
                            .to_compile_error()
                            .into();
                        }
                    }
                }
            }
            _b => {
                return syn::Error::new(
                    arm.pat.span().span(),
                    "expected literal string/byte/bytes/char",
                )
                .to_compile_error()
                .into();
            }
        }
    }

    struct PathEntry {
        id: usize,
        result: Option<Arm>,
        sub_entries: HashMap<u8, Box<PathEntry>>,
    }

    let mut paths = Box::new(PathEntry {
        id: 0,
        result: None,
        sub_entries: HashMap::new(),
    });
    let mut counter = 1;

    for (keyword_b, arm) in arms.drain(..) {
        let mut current = &mut paths;

        for b in keyword_b {
            match current.sub_entries.get(&b) {
                Some(_) => {
                    current = current.sub_entries.get_mut(&b).unwrap();
                }
                None => {
                    let new_entry = Box::new(PathEntry {
                        id: counter,
                        result: None,
                        sub_entries: HashMap::new(),
                    });
                    counter += 1;
                    current.sub_entries.insert(b, new_entry);
                    current = current.sub_entries.get_mut(&b).unwrap();
                }
            }
        }

        assert!(current.result.is_none());
        current.result = Some(arm);
    }

    fn write_entry(
        idx: usize,
        var_name: proc_macro2::TokenStream,
        entry: &PathEntry,
        result_arms: &mut Vec<proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        let id = entry.id;
        let eof_handle = if let Some(ref result) = entry.result {
            let guard = if let Some(ref b) = result.guard {
                let expr = &b.1;
                quote! { if #expr }
            } else {
                quote! {}
            };
            let body = &result.body;
            result_arms.push(quote! { (true, #id) => { #body } });
            quote! { None #guard => { break 'match_loop true; } }
        } else {
            quote! {}
        };

        let mut arms = Vec::with_capacity(entry.sub_entries.len());
        for (&b, sub_entry) in &entry.sub_entries {
            let id = sub_entry.id;
            if b.is_ascii_alphabetic() {
                let b_lower = b.to_ascii_lowercase();
                arms.push(quote! { Some(#b) | Some(#b_lower) => { state = #id; } });
            } else {
                arms.push(quote! { Some(#b) => { state = #id; } });
            }
        }

        let mut sub_entries = Vec::with_capacity(entry.sub_entries.len());
        for sub_entry in entry.sub_entries.values() {
            sub_entries.push(write_entry(
                idx + 1,
                var_name.clone(),
                sub_entry,
                result_arms,
            ));
        }

        quote! {
            #id => match #var_name.get(#idx) {
                #eof_handle
                #(#arms)*
                _ => { break 'match_loop false; }
            }
            #(#sub_entries)*
        }
    }

    let fallback_handle = if let Some(ref result) = fallback_arm {
        let body = &result.body;
        quote! { _ => { #body } }
    } else {
        quote! {}
    };

    let expr = match_block.expr;
    let mut result_arms = Vec::with_capacity(counter);
    let match_arms = write_entry(0, quote! { #expr }, &paths, &mut result_arms);
    TokenStream::from(quote! {
        {
            let mut state = 0usize;

            #[cfg(debug_assertions)]
            let mut loop_counter = 0usize;

            let has_result = 'match_loop: loop {
                #[cfg(debug_assertions)]
                {
                    debug_assert!(loop_counter < #counter);
                    loop_counter += 1;
                }

                match state {
                    #match_arms
                    _ => unreachable!(),
                }
            };

            match (has_result, state) {
                #(#result_arms)*
                #fallback_handle
            }
        }
    })
}
