//! Corner components support (not yet implemented)

#[cfg(test)]
mod tests {
    use crate::font::{Font, Layer, Shape};
    use rstest::rstest;
    use std::path::{Path, PathBuf};

    fn testdata_dir() -> PathBuf {
        let mut dir = Path::new("../resources/testdata");
        if !dir.is_dir() {
            dir = Path::new("./resources/testdata");
        }
        dir.to_path_buf()
    }

    fn glyphs3_dir() -> PathBuf {
        testdata_dir().join("glyphs3")
    }

    /// Compare two layers' paths for equality
    fn compare_paths(test_layer: &Layer, expectation_layer: &Layer, glyph_name: &str) {
        // Extract only Path shapes, ignoring Components
        let test_paths: Vec<_> = test_layer
            .shapes
            .iter()
            .filter_map(Shape::as_path)
            .collect();

        let expectation_paths: Vec<_> = expectation_layer
            .shapes
            .iter()
            .filter_map(Shape::as_path)
            .collect();

        assert_eq!(
            test_paths.len(),
            expectation_paths.len(),
            "Number of paths differs for glyph '{glyph_name}': expected {}, got {}",
            expectation_paths.len(),
            test_paths.len()
        );

        for (i, (test_path, expectation_path)) in
            test_paths.iter().zip(expectation_paths.iter()).enumerate()
        {
            assert_eq!(
                test_path.to_points(),
                expectation_path.to_points(),
                "Path {i} differs for glyph '{glyph_name}'",
            );
        }
    }

    fn test_corner_component_glyph(glyph_name: &str) {
        let font_path = glyphs3_dir().join("CornerComponents.glyphs");
        let font = Font::load(&font_path).expect("Failed to load CornerComponents.glyphs");

        let test_glyph = font
            .glyphs
            .get(glyph_name)
            .unwrap_or_else(|| panic!("Test glyph '{}' not found", glyph_name));

        let expectation_glyph_name = format!("{}.expectation", glyph_name);
        let expectation_glyph = font
            .glyphs
            .get(expectation_glyph_name.as_str())
            .unwrap_or_else(|| panic!("Expectation glyph '{}' not found", expectation_glyph_name));

        // Get the first master's layer (assuming single master for test)
        assert!(
            !test_glyph.layers.is_empty(),
            "Test glyph '{}' has no layers",
            glyph_name
        );
        assert!(
            !expectation_glyph.layers.is_empty(),
            "Expectation glyph '{}' has no layers",
            expectation_glyph_name
        );

        let test_layer = test_glyph.layers[0].clone();
        let expectation_layer = &expectation_glyph.layers[0];

        // Compare the results
        compare_paths(&test_layer, expectation_layer, glyph_name);
    }

    #[rstest]
    #[case::aa_simple_angleinstroke("aa_simple_angleinstroke")]
    #[case::ab_simple_angled("ab_simple_angled")]
    #[case::ac_scale("ac_scale")]
    #[case::ad_curved_instroke("ad_curved_instroke")]
    #[case::ae_curved_corner_firstseg("ae_curved_corner_firstseg")]
    #[case::af_curved_corner_firstseg_slanted("af_curved_corner_firstseg_slanted")]
    #[case::ag_curved_corner_bothsegs("ag_curved_corner_bothsegs")]
    #[case::ag_curved_corner_bothsegs_rotated("ag_curved_corner_bothsegs_rotated")]
    #[case::ah_origin("ah_origin")]
    #[case::ai_curved_outstroke("ai_curved_outstroke")]
    #[case::aj_right_alignment("aj_right_alignment")]
    #[case::ak_right_slanted("ak_right_slanted")]
    #[case::al_unaligned("al_unaligned")]
    #[case::am_middle("am_middle")]
    #[case::an_flippy("an_flippy")]
    #[case::ao_firstnode("ao_firstnode")]
    #[case::ap_twoofthem("ap_twoofthem")]
    #[case::aq_rightleg("aq_rightleg")]
    #[case::ar_leftleg("ar_leftleg")]
    #[case::as_closedpaths("as_closedpaths")]
    #[case::at_unaligned_lastseg("at_unaligned_lastseg")]
    #[case::au_left_anchoronpath("au_left_anchoronpath")]
    #[case::av_left_anchoroffpath("av_left_anchoroffpath")]
    #[case::aw_direction("aw_direction")]
    #[case::ax_curved_instroke2("ax_curved_instroke2")]
    // ported from glyphsLib: https://github.com/googlefonts/glyphsLib/blob/f90e4060ba/tests/corner_components_test.py#L14
    fn test_corner_components(#[case] glyph_name: &str) {
        // Skip glyphs with left_anchor as noted in the Python test
        if glyph_name.contains("left_anchor") {
            // In rstest we can't easily skip tests, so we just return early
            eprintln!(
                "Skipping '{}': left anchors not quite working yet",
                glyph_name
            );
            return;
        }

        test_corner_component_glyph(glyph_name);
    }
}
