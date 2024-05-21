use syn::{
    punctuated::Punctuated, spanned::Spanned, Attribute, Expr, ExprLit, Lit, Meta, MetaNameValue,
    Token,
};

static IGNORE: &str = "ignore";
static KEY: &str = "key";
static ALT_NAME: &str = "alt_name";
static DOC: &str = "doc";

#[derive(Clone, Default, Debug)]
pub(crate) struct FieldAttrs {
    pub(crate) ignore: bool,
    pub(crate) plist_field_name: Option<String>,
    pub(crate) plist_addtl_names: Vec<String>,
}

impl FieldAttrs {
    pub(crate) fn from_attrs(attrs: &[Attribute]) -> syn::Result<Self> {
        let mut this = FieldAttrs::default();
        for attr in attrs {
            if attr.path().is_ident(DOC) {
                continue;
            }
            // https://docs.rs/syn/latest/syn/struct.Attribute.html#alternatives
            let args = attr.parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)?;
            for meta in args {
                match &meta {
                    Meta::Path(path) if path.is_ident(IGNORE) => this.ignore = true,
                    Meta::NameValue(MetaNameValue {
                        path,
                        eq_token: _,
                        value,
                    }) if path.is_ident(KEY) => {
                        let Expr::Lit(ExprLit { attrs: _, lit }) = value else {
                            panic!("Expected key = \"value\"");
                        };
                        let Lit::Str(lit) = lit else {
                            panic!("Expected key = \"value\"");
                        };
                        this.plist_field_name = Some(lit.value());
                    }
                    Meta::NameValue(MetaNameValue {
                        path,
                        eq_token: _,
                        value,
                    }) if path.is_ident(ALT_NAME) => {
                        let Expr::Lit(ExprLit { attrs: _, lit }) = value else {
                            panic!("Expected key = \"value\"");
                        };
                        let Lit::Str(lit) = lit else {
                            panic!("Expected key = \"value\"");
                        };
                        this.plist_addtl_names.push(lit.value());
                    }
                    _ => return Err(syn::Error::new(meta.span(), "unsupported attribute")),
                }
            }
        }
        Ok(this)
    }
}
