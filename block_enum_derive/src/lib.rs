use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{Data, DeriveInput, parse_macro_input, Attribute};


fn variant_has_lifetime(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|attr| {
        attr.path().is_ident("has_lifetime")
    })
}

#[proc_macro_derive(BlockEnum, attributes(has_lifetime))]
pub fn derive_block_enum(input: TokenStream) -> TokenStream {
    // Extract enum name and variants
    let ast = parse_macro_input!(input as DeriveInput);
    let kind_enum_name = ast.ident;
    let vis = &ast.vis;

    let variants = if let Data::Enum(data) = ast.data {
        data.variants
            .into_iter()
            .map(|v| (v.ident, variant_has_lifetime(&v.attrs)))
            .collect::<Vec<_>>()
    } else {
        return syn::Error::new_spanned(kind_enum_name, "BlockEnum can only be applied to Enum")
            .to_compile_error()
            .into();
    };

    let enum_variants = variants.iter().map(|(ident, has_lt)| {
        if *has_lt {

            quote! { #ident ( Commented<'a, #ident<'a>> ), }
        } else {
            quote! { #ident ( Commented<'a, #ident> ), }
        }
    }).collect::<Vec<_>>();

    // generate the enum definition
    let block_enum_name = format_ident!("Block");

    // generate each variant's parsing function
    let try_parse_branches = variants.iter().map(|(ident, has_lt)| {
        if *has_lt {
            quote! {
                #kind_enum_name::#ident => {
                    <#ident<'a> as BlockParser>::try_parse_block(
                        &input,
                        raw_header,
                        context
                    ).map(|(rest, v)| (rest, #block_enum_name::#ident(v)))
                }
            }
        } else {
            quote! {
                #kind_enum_name::#ident => {
                    <#ident as BlockParser>::try_parse_block(
                        &input,
                        raw_header,
                        context
                    ).map(|(rest, v)| (rest, #block_enum_name::#ident(v)))
                }
            }
        }
    });

    let comment_branches = variants.iter().map(|(v, _)| {
        quote! {
            #block_enum_name::#v(c) => &c.comments,
        }
    });

    let comment_mut_branches = variants.iter().map(|(v, _)| {
        quote! {
            #block_enum_name::#v(c) => &mut c.comments,
        }
    });
    
    let block_kind_branches = variants.iter().map(|(v, _)| {
        let block_kind = format_ident!("{}", v);
        quote! {
            #block_enum_name::#block_kind(_) => #kind_enum_name::#block_kind,
        }
    });



    // generate the enum definition
    let enum_def = quote! {
        #[derive(Debug)]
        #vis enum #block_enum_name<'a> {
            #(
                #enum_variants
            )*
        }

        impl<'a> #block_enum_name<'a> {
            pub fn try_parse<'b>(input: &'a str, raw_header: RawHeader<'a>, context: &'b mut ParseContext) -> IResult<&'a str, Self, RError> {
                {
                    match raw_header.block_kind{
                    #(
                        #try_parse_branches
                    )*,
                    _ => Err(nom::Err::Error(Error::UnknownKeyword {
                        keyword: raw_header.block_kind.as_ref().to_string(),
                        scope: ErrorScope::Document
                    }.into()))
                }
                }
            }
            pub fn comments(&self) -> &Comments<'a> {
                match self {
                    #(
                        #comment_branches
                    )*
                }
            }

            pub fn comments_mut(&mut self) -> &mut Comments<'a> {
                match self {
                    #(
                        #comment_mut_branches
                    )*
                }
            }

            pub fn kind(&self) -> BlockKind {
                match self {
                    #(
                        #block_kind_branches
                    )*
                }
            }
        }
    };

    // generate the trait `TypedBlock` for each struct
    let typed_block_impls = variants.iter().map(|(v, lt)| {
        let block_kind = format_ident!("{}", v);
        if *lt {
            quote! {
            impl<'a> TypedBlock for #block_kind<'a> {
                fn block_kind() -> BlockKind {
                    BlockKind::#block_kind
                }
            }
            }
        } else {
            quote! {
            impl TypedBlock for #block_kind {
                fn block_kind() -> BlockKind {
                    BlockKind::#block_kind
                }
            }
            }
        }

    });

    let final_output = quote! {

        #(
            #typed_block_impls
        )*

        #enum_def
    };

    final_output.into()
}
