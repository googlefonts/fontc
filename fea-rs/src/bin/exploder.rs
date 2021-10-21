use fonttools::font::Font;

fn main() {
    let args = flags::ValidateNames::from_env().unwrap();

    let font = Font::load(&args.path).expect("failed to load font");
    fea_rs::debug::explode_font(&font, args.verbose);
}

mod flags {
    use std::path::PathBuf;
    xflags::xflags! {

        cmd validate-names
            required path: PathBuf
            {
                optional -v, --verbose
            }
    }
}
