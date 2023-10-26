use std::ops::Range;

use crate::token_tree::Token;

//NOTE: in order to save allocation for each item in the range, we adopt
//the pattern of having the caller pass in a callback that is called with
//each member in the range. The caller is then responsible for doing things like
//ensuring that the item is in the glyph map.

/// iter glyph ids in a cid range.
///
/// Returns an error if the range is not well-formed. If it is well-formed,
/// the `callback` is called with each cid in the range.
pub(crate) fn cid(start: &Token, end: &Token, mut callback: impl FnMut(u16)) -> Result<(), String> {
    let start_cid = start.text.parse::<u16>().unwrap();
    let end_cid = end.text.parse::<u16>().unwrap();
    if start_cid >= end_cid {
        return Err("Range end must be greater than start".into());
    }

    for i in start_cid..=end_cid {
        callback(i);
    }
    Ok(())
}

/// iter glyph ids in a named range.
///
/// Returns an error if the range is not well-formed. If it is well-formed,
/// the `callback` is called with each name in the range.
pub(crate) fn named(start: &Token, end: &Token, callback: impl FnMut(&str)) -> Result<(), String> {
    if start.text.len() != end.text.len() {
        return Err("glyph range components must have equal length".into());
    }
    let diff_range = get_diff_range(&start.text, &end.text);

    if diff_range.len() == 1 {
        let one_byte = start.text.as_bytes()[diff_range.start];
        let two_byte = end.text.as_bytes()[diff_range.start];
        if one_byte >= two_byte {
            return Err("glyph range end must be greater than start".into());
        }
        if one_byte.is_ascii_alphabetic() && two_byte.is_ascii_alphabetic()
        // range must be between two lowercase or two uppercase ascii letters
        && ((one_byte > b'Z') == (two_byte > b'Z'))
        {
            alpha_range(&start.text, &end.text, diff_range, callback);
            return Ok(());
        }
    }
    let one = &start.text[diff_range.clone()];
    let two = &end.text[diff_range.clone()];
    match (one.parse::<u16>(), two.parse::<u16>()) {
    (Ok(one), Ok(two)) if one < two => num_range(&start.text, one..two, diff_range, callback),
        _ => return Err("range glyphs must differ by a single letter a-Z or A-Z, or by a run of up to three decimal digits".into()),
    };
    Ok(())
}

fn alpha_range(start: &str, end: &str, sub_range: Range<usize>, mut out: impl FnMut(&str)) {
    let mut template = start.to_string();
    let start_char = start.as_bytes()[sub_range.start] as char;
    let end_char = end.as_bytes()[sub_range.start] as char;
    for chr in start_char..=end_char {
        debug_assert_eq!(chr.len_utf8(), 1);
        // safety: validate glyph name is all ascii, so we only ever overwrite
        // a single byte with another single byte
        unsafe {
            chr.encode_utf8(&mut template.as_bytes_mut()[sub_range.start..sub_range.end]);
        }
        out(&template);
    }
}

fn num_range(
    start: &str,
    sub_range: Range<u16>,
    text_range: Range<usize>,
    mut out: impl FnMut(&str),
) {
    let mut temp = String::new();
    let mut template = start.to_string();

    use std::fmt::Write;
    let width = text_range.len();
    for val in sub_range {
        temp.clear();
        write!(&mut temp, "{:0width$}", val, width = width).unwrap();
        template.replace_range(text_range.clone(), &temp);
        out(&template);
    }
}

fn get_diff_range(one: &str, two: &str) -> Range<usize> {
    assert_eq!(one.len(), two.len());
    let front = one
        .bytes()
        .zip(two.bytes())
        .take_while(|(a, b)| a == b)
        .count();
    let back = one
        .bytes()
        .rev()
        .zip(two.bytes().rev())
        .take_while(|(a, b)| a == b)
        .count();
    let back = one.len() - back;
    if back < front {
        0..0
    } else {
        // expand number range to all adjacent digits
        let mut front = front;
        while front > 0 && one.as_bytes()[front - 1].is_ascii_digit() {
            front -= 1;
        }
        let mut back = back;
        while back < one.len()
            && one
                .as_bytes()
                .get(back)
                .map(u8::is_ascii_digit)
                .unwrap_or(false)
        {
            back += 1;
        }

        front..back
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        common::GlyphIdent,
        token_tree::TreeBuilder,
        typed::{self, AstNode},
        GlyphMap, Kind, Node,
    };

    use super::*;

    /// A helper for testing, that just returns the names/cids that should be part
    /// of a given range. (This does not test if they're in the font.)
    fn glyph_range(node: &Node) -> Result<Vec<GlyphIdent>, String> {
        let range = typed::GlyphRange::cast(&node.clone().into()).unwrap();
        let start = range.start();
        let end = range.end();
        let mut result = Vec::new();

        match (start.kind, end.kind) {
            (Kind::Cid, Kind::Cid) => cid(start, end, |cid| result.push(GlyphIdent::Cid(cid)))?,
            (Kind::GlyphName, Kind::GlyphName) => named(start, end, |string| {
                result.push(GlyphIdent::Name(string.into()))
            })?,
            (_, _) => return Err("Invalid glyph range".to_string()),
        }

        Ok(result)
    }

    #[test]
    fn diff_range_smoke_test() {
        let one = "hi.a";
        let two = "hi.z";
        assert_eq!(&one[get_diff_range(one, two)], "a");

        let one = "hi";
        let two = "hi";
        assert_eq!(&one[get_diff_range(one, two)], "");

        let one = "A.hi";
        let two = "C.hi";
        assert_eq!(&one[get_diff_range(one, two)], "A");

        let one = "f_x_i";
        let two = "f_g_i";
        assert_eq!(&one[get_diff_range(one, two)], "x");

        let one = "a.01";
        let two = "a.42";
        assert_eq!(&one[get_diff_range(one, two)], "01");

        let one = "a.123a";
        let two = "a.153a";
        assert_eq!(&one[get_diff_range(one, two)], "123");
    }

    fn make_range_node(k1: Kind, t1: &str, k2: Kind, t2: &str) -> Node {
        let mut builder = TreeBuilder::default();
        builder.start_node(Kind::GlyphRange);
        builder.token(k1, t1);
        builder.token(Kind::Hyphen, "-");
        builder.token(k2, t2);
        builder.finish_node(false, None);
        builder.finish()
    }

    #[test]
    fn cid_range() {
        let range = make_range_node(Kind::Cid, "4", Kind::Cid, "12");
        let idents = glyph_range(&range).unwrap();
        let map: GlyphMap = idents.into_iter().collect();
        for val in 4u16..=12 {
            assert!(map.contains(&val));
        }
    }

    #[test]
    fn cid_range_bad() {
        let range = make_range_node(Kind::Cid, "12", Kind::Cid, "1");
        let idents = glyph_range(&range);
        assert!(idents.is_err());
    }

    #[test]
    fn mixed_range() {
        let range = make_range_node(Kind::Cid, "12", Kind::GlyphName, "hi");
        let idents = glyph_range(&range);
        assert!(idents.is_err());
    }

    #[test]
    fn this_is_really_bad() {
        let range = make_range_node(Kind::Number, "12", Kind::GlyphName, "hi");
        let idents = glyph_range(&range);
        assert!(idents.is_err());
    }

    #[test]
    fn named_range_() {
        let range = make_range_node(Kind::GlyphName, "A.hi", Kind::GlyphName, "E.hi");
        let idents = glyph_range(&range).unwrap();
        let map: GlyphMap = idents.into_iter().collect();
        assert_eq!(map.len(), 5, "{:?}", map);
        for val in ["A.hi", "B.hi", "C.hi", "D.hi", "E.hi"] {
            assert!(map.contains(val));
        }
    }

    #[test]
    fn named_range_bad() {
        let range = make_range_node(Kind::GlyphName, "A.hi", Kind::GlyphName, "Ez.hi");
        let idents = glyph_range(&range);
        assert!(idents.is_err());

        let range = make_range_node(Kind::GlyphName, "A.hi", Kind::GlyphName, "B");
        let idents = glyph_range(&range);
        assert!(idents.is_err());

        let range = make_range_node(Kind::GlyphName, "A1.hi", Kind::GlyphName, "B1.hi");
        let idents = glyph_range(&range);
        assert!(idents.is_err());

        let range = make_range_node(Kind::GlyphName, "Z.hi", Kind::GlyphName, "A.hi");
        let idents = glyph_range(&range);
        assert!(idents.is_err());

        let range = make_range_node(Kind::GlyphName, "a", Kind::GlyphName, "A");
        let idents = glyph_range(&range);
        assert!(idents.is_err());

        let range = make_range_node(Kind::GlyphName, "Z", Kind::GlyphName, "z");
        let idents = glyph_range(&range);
        assert!(idents.is_err());

        let range = make_range_node(Kind::GlyphName, "a", Kind::GlyphName, "z");
        let idents = glyph_range(&range);
        assert!(idents.is_ok());
    }
}
