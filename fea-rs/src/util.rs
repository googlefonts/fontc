//! helpers and utilties (mostly for testing/debugging?)

pub mod debug;
pub(crate) mod highlighting;
pub mod paths;
#[cfg(any(test, feature = "diff"))]
pub mod pretty_diff;
#[cfg(any(test, feature = "test"))]
pub mod ttx;

pub use highlighting::style_for_kind;
#[cfg(any(test, feature = "diff"))]
pub use pretty_diff::write_line_diff;

#[doc(hidden)]
pub static SPACES: &str = "                                                                                                                                                                                    ";

// just used for sanity checking
pub(crate) fn is_sorted<T: PartialOrd>(slice: &[T]) -> bool {
    if slice.len() > 2 {
        let mut prev = &slice[0];
        for item in &slice[1..] {
            if prev > item {
                return false;
            }
            prev = item;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_sorted_() {
        assert!(is_sorted::<u32>(&[]));
        assert!(is_sorted(&[1]));
        assert!(is_sorted(&[1, 1, 1]));
        assert!(is_sorted(&[1, 2, 11, 11, 12]));
        assert!(!is_sorted(&[3, 2, 11, 11, 12]));
        assert!(!is_sorted(&[2, 11, 11, 12, 11]));
    }
}
