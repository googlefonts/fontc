pub fn glyph_file(glyph_name: &str, suffix: &str) -> String {
    // TODO handle names that are invalid for the filesystem
    // Ref https://github.com/unified-font-object/ufo-spec/issues/164
    glyph_name.to_owned() + suffix
}
