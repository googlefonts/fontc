//! facilities for editing the AST.
//!
//! This is currently unused.

#![allow(dead_code)]
use std::ops::Range;

use super::{cursor::Cursor, Node, TreeBuilder};

/// if 'skip_parent' is true, then the children of inserted nodes are added
/// but the inserted node itself is skipped.
pub(crate) fn apply_edits(
    base: &Node,
    mut edits: Vec<(Range<usize>, Node)>,
    skip_parent: bool,
) -> Node {
    edits.sort_unstable_by_key(|(range, _)| range.start);
    edits.reverse();
    let mut builder = TreeBuilder::default();
    let mut cursor = base.cursor();
    apply_edits_recurse(&mut cursor, &mut builder, &mut edits, skip_parent);
    builder.finish()
}

fn apply_edits_recurse(
    cursor: &mut Cursor,
    builder: &mut TreeBuilder,
    edits: &mut Vec<(Range<usize>, Node)>,
    skip_parent: bool,
) {
    builder.start_node(cursor.parent_kind());
    while let Some(current) = cursor.current() {
        let next_edit_range = match edits.last() {
            None => {
                builder.push_raw(current.clone());
                cursor.step_over();
                continue;
            }
            Some((range, _)) => range.clone(),
        };

        // now either:
        // - the edit *is* this item, in which case we replace it
        // - the edit is *inside* this item, in which case we recurse,
        // - the edit does not touch this item in which case we push this item
        //   and step over.
        let cur_range = cursor.pos()..cursor.pos() + current.text_len();
        match op_for_node(cur_range, next_edit_range) {
            EditOp::Copy => {
                builder.push_raw(current.clone());
                cursor.step_over();
                //continue;
            }
            EditOp::Replace => {
                let to_add = edits.pop().unwrap().1;
                if skip_parent {
                    for child in to_add.children.iter() {
                        builder.push_raw(child.to_owned());
                    }
                } else {
                    builder.push_raw(to_add.into());
                }
                cursor.step_over();
            }
            EditOp::Recurse => {
                cursor.descend_current();
                apply_edits_recurse(cursor, builder, edits, skip_parent);
                cursor.ascend();
                // invariant: we have copied or edited all
                // the items in this subtree.
                cursor.step_over();
            }
        }
    }
    builder.finish_node(false, None);
}

fn op_for_node(node_range: Range<usize>, edit_range: Range<usize>) -> EditOp {
    assert!(edit_range.start >= node_range.start);
    if node_range == edit_range {
        EditOp::Replace
    } else if edit_range.start > node_range.start && edit_range.end < node_range.end {
        EditOp::Recurse
    } else {
        assert!(
            edit_range.end <= node_range.start || edit_range.start >= node_range.end,
            "{edit_range:?} {node_range:?}",
        );
        EditOp::Copy
    }
}

#[derive(Debug, Clone, Copy)]
enum EditOp {
    Replace,
    Recurse,
    Copy,
}

#[cfg(test)]
mod tests {
    use crate::{
        parse::{FileId, Parser},
        token_tree::AstSink,
        TokenSet,
    };

    use super::*;

    #[test]
    fn rewrite() {
        let fea = "\
languagesystem DFLT dftl;
feature liga {
    substitute f i by f_i;
    substitute f l by f_l;
} liga;
";
        let expected = "\
languagesystem hihi ohno;
feature liga {
    substitute f i by f_i;
    sub gg by w_p;
} liga;
";

        let mut sink = AstSink::new(fea, FileId::CURRENT_FILE, None);
        let mut parser = Parser::new(fea, &mut sink);
        crate::parse::grammar::root(&mut parser);
        let (root, _errs, _) = sink.finish();

        let replace_lang = {
            let fea = "languagesystem hihi ohno;";
            crate::parse::parse_node(fea, crate::parse::grammar::language_system)
        };
        let replace_sub = {
            let fea = "sub gg by w_p;";
            crate::parse::parse_node(fea, |p| {
                crate::parse::grammar::gsub_rule(p, TokenSet::FEATURE_STATEMENT)
            })
        };

        let edits = vec![(0..25, replace_lang), (72..94, replace_sub)];
        let edited = apply_edits(&root, edits, false);
        let result = edited.iter_tokens().map(|t| t.as_str()).collect::<String>();
        crate::assert_eq_str!(expected, result);
    }
}
