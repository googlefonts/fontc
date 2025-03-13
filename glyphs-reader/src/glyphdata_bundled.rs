//! Accessors for bundled glyphsLib data

use std::{cmp::Ordering, marker::PhantomData, str::from_utf8_unchecked};

use crate::glyphdata::{Category, QueryResult, Subcategory};

type U24 = usize;

fn read_u24le(bytes: &[u8]) -> usize {
    ((bytes[2] as usize) << 16) | ((bytes[1] as usize) << 8) | (bytes[0] as usize)
}

pub(crate) trait BundledEntry {
    fn element_size() -> usize;
    fn from_slice(raw: &[u8]) -> Self;
}

impl BundledEntry for U24 {
    fn element_size() -> usize {
        3
    }

    fn from_slice(raw: &[u8]) -> Self {
        read_u24le(raw)
    }
}

impl BundledEntry for (U24, U24) {
    fn element_size() -> usize {
        6
    }

    fn from_slice(raw: &[u8]) -> Self {
        (read_u24le(&raw[0..3]), read_u24le(&raw[3..6]))
    }
}

struct ArrayOf<T: BundledEntry>(&'static [u8], PhantomData<T>);

impl<T> ArrayOf<T>
where
    T: BundledEntry,
{
    const fn new(data: &'static [u8]) -> Self {
        Self(data, PhantomData)
    }

    fn len(&self) -> usize {
        self.0.len() / T::element_size()
    }

    fn get(&self, i: usize) -> Option<T> {
        let start = T::element_size() * i;
        let end = start + T::element_size();
        if end > self.0.len() {
            return None;
        }
        Some(T::from_slice(&self.0[start..end]))
    }
}

const NAME_OFFSETS: ArrayOf<U24> = ArrayOf::new(include_bytes!("../resources/name_offsets.dat"));
const NAMES: &[u8] = include_bytes!("../resources/names.dat");

const CODEPOINT_TO_INFO_IDX: ArrayOf<(U24, U24)> =
    ArrayOf::new(include_bytes!("../resources/codepoints_to_idx.dat"));

const CODEPOINTS: ArrayOf<U24> = ArrayOf::new(include_bytes!("../resources/codepoints.dat"));
const CATEGORIES: ArrayOf<Category> = ArrayOf::new(include_bytes!("../resources/categories.dat"));
const SUBCATEGORIES: ArrayOf<Option<Subcategory>> =
    ArrayOf::new(include_bytes!("../resources/subcategories.dat"));

fn name_offset(idx: usize) -> usize {
    // The last offset extends to EOF
    if idx == NAME_OFFSETS.len() {
        return NAMES.len();
    }
    NAME_OFFSETS.get(idx).unwrap_or_else(|| {
        panic!(
            "Asked for name {idx} but we only have {} names",
            NAMES.len()
        )
    })
}

fn name(idx: usize) -> &'static str {
    let start = name_offset(idx);
    let end = name_offset(idx + 1);
    // SAFETY: we only write ascii names in glyphs-reader/data/update.py
    unsafe { from_utf8_unchecked(&NAMES[start..end]) }
}

fn bsearch<T: Ord>(len: usize, needle: T, get: impl Fn(usize) -> (T, usize)) -> Option<usize> {
    let mut upper = len as i32;
    let mut lower = 0;
    while lower <= upper {
        let mid = ((lower + upper) / 2) as usize;
        let (c, i) = get(mid);
        match c.cmp(&needle) {
            Ordering::Equal => return Some(i),
            Ordering::Less => lower = mid as i32 + 1,
            Ordering::Greater => upper = mid as i32 - 1,
        }
    }
    None
}

pub(crate) fn find_pos_by_name(needle: &str) -> Option<usize> {
    bsearch(NAME_OFFSETS.len(), needle, |i| (name(i), i))
}

pub(crate) fn find_pos_by_codepoint(needle: u32) -> Option<usize> {
    bsearch(CODEPOINT_TO_INFO_IDX.len(), needle as usize, |i| {
        CODEPOINT_TO_INFO_IDX.get(i).unwrap()
    })
}

pub(crate) fn get(i: usize) -> Option<QueryResult> {
    if i >= NAMES.len() {
        return None;
    }
    let category = CATEGORIES
        .get(i)
        .unwrap_or_else(|| panic!("We have names[{i}] but not categories[{i}] ?!"));
    let subcategory = SUBCATEGORIES
        .get(i)
        .unwrap_or_else(|| panic!("We have names[{i}] but not subcategory[{i}] ?!"));
    let codepoint = CODEPOINTS
        .get(i)
        .unwrap_or_else(|| panic!("We have names[{i}] but not codepoints[{i}] ?!"));
    let codepoint = if codepoint > 0 {
        Some(codepoint as u32)
    } else {
        None
    };

    Some(QueryResult {
        category,
        subcategory,
        codepoint,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn how_many_names() {
        assert!(
            NAME_OFFSETS.len() > 50000,
            "Oddly few names: {}",
            NAME_OFFSETS.len()
        );
    }

    #[test]
    fn bundled_lengths() {
        let num_names = NAME_OFFSETS.len();
        assert_eq!(
            (num_names, num_names),
            (CATEGORIES.len(), SUBCATEGORIES.len())
        )
    }

    #[test]
    fn wry_cat_by_codepoint() {
        assert_eq!(
            (
                "catFaceWithWrySmile",
                QueryResult {
                    category: Category::Symbol,
                    subcategory: Some(Subcategory::Emoji),
                    codepoint: Some(0x1F63C),
                }
            ),
            find_pos_by_codepoint(0x1F63C)
                .map(|i| (name(i), get(i).unwrap()))
                .unwrap()
        );
    }

    #[test]
    fn find_pos_by_name_empty() {
        assert_eq!(None, find_pos_by_name(" "));
    }

    #[test]
    fn find_pos_by_name_first() {
        assert_eq!(Some(0), find_pos_by_name(name(0)));
    }

    #[test]
    fn find_pos_by_name_last() {
        let last = NAME_OFFSETS.len() - 1;
        assert_eq!(Some(last), find_pos_by_name(name(last)));
    }
}
