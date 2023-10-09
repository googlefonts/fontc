pub fn glyph_file(glyph_name: &str, suffix: &str) -> String {
    // TODO handle names that are invalid for the filesystem
    // Ref https://github.com/unified-font-object/ufo-spec/issues/164
    let mut filename = Vec::new();
    for ch in glyph_name.chars() {
        filename.push(ch);
        if ch == '_' || ch.is_uppercase() {
            filename.push('_');
        }
    }
    filename.extend(suffix.chars());
    filename.into_iter().collect()
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::glyph_file;

    /// <https://github.com/googlefonts/fontc/issues/41>
    fn assert_unique_for_caseinsensitive_fs(names: &[&str]) {
        let filenames: HashSet<_> = names
            .iter()
            .map(|n| glyph_file(n, ""))
            .map(|n| n.to_lowercase())
            .collect();
        assert_eq!(
            names.len(),
            filenames.len(),
            "{names:?} became {filenames:?}"
        );
    }

    #[test]
    fn lower_and_upper_a() {
        assert_unique_for_caseinsensitive_fs(&["a", "A"]);
    }

    #[test]
    fn adding_underscore_avoids_collisions() {
        // if we don't add _ to _ the resulting names are identical
        assert_unique_for_caseinsensitive_fs(&["Aa", "a_a"]);
    }
}
