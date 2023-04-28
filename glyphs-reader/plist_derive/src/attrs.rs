use syn::{spanned::Spanned, Attribute};

/// Attributes on fields
#[derive(Clone, Default)]
pub(crate) struct FieldAttrs {
    pub(crate) rest: Option<syn::Path>,
    pub(crate) default: Option<syn::Path>,
}

static REST: &str = "rest";
static DEFAULT: &str = "default";

impl FieldAttrs {
    pub(crate) fn from_attrs(attrs: &[Attribute]) -> syn::Result<Self> {
        let mut this = FieldAttrs::default();
        for attr in attrs {
            let args: syn::Path = attr.parse_args()?;
            if args.is_ident(REST) {
                this.rest = Some(attr.path.clone());
            } else if args.is_ident(DEFAULT) {
                this.default = Some(attr.path.clone());
            } else {
                return Err(syn::Error::new(args.span(), "unsupported attribute"));
            }
        }
        Ok(this)
    }
}
