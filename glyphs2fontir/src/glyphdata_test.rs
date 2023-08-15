//! Handwritten tests for generated glyphdata.rs

#[cfg(test)]
mod tests {
    use crate::glyphdata::{is_nonspacing_mark_name, might_be_a_nonspacing_mark_name};

    #[test]
    fn potential_mark_a() {
        assert!(!might_be_a_nonspacing_mark_name("a"));
    }

    #[test]
    fn potential_mark_vs1() {
        assert!(might_be_a_nonspacing_mark_name("VS1"));
    }

    #[test]
    fn potential_mark_accutcomb() {
        assert!(might_be_a_nonspacing_mark_name("acutecomb"));
        assert!(is_nonspacing_mark_name("acutecomb"));
    }

    #[test]
    fn potential_mark_accut_whatever() {
        assert!(might_be_a_nonspacing_mark_name("acutWHATEVER"));
        assert!(!is_nonspacing_mark_name("acutWHATEVER"));
    }
}
