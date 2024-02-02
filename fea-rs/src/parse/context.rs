//! parsing and resolving includes

use std::{
    collections::{HashMap, HashSet},
    ffi::OsString,
    ops::Range,
    sync::Arc,
};

use super::source::{Source, SourceLoadError, SourceLoader, SourceResolver};
use super::{FileId, ParseTree, Parser, SourceList, SourceMap};
use crate::{
    token_tree::{
        typed::{self, AstNode as _},
        AstSink,
    },
    Diagnostic, DiagnosticSet, GlyphMap, Node,
};

const MAX_INCLUDE_DEPTH: usize = 50;

/// Oversees parsing, following, resolving and validating input statements.
///
/// Includes are annoying. Existing tools tend to handle them as they're
/// encountered, pushing another parser onto the stack and building the tree
/// in-place. This doesn't work for us, because we want to be able to preserve
/// the original source locations for tokens so that we can provide good error
/// messages.
///
/// We handle this in a reasonably straight-forward way: instead of parsing
/// includes immediately, we return a list of the includes found in each
/// source file. We use these to build a graph of include statements, and then
/// we also add these files to a queue, skipping files that have been parsed
/// already. Importantly, we don't worry about recursion or include depth
/// at parse time; we just parse every file we find, and if there's a cycle
/// we avoid it by keeping track of what we've already parsed.
///
/// Once parsing is finished, we use our `IncludeGraph` to validate that there
/// are no cycles, and that the depth limit is not exceeded.
///
/// After parsing, you use [`generate_parse_tree`] to validate and assemble
/// the parsed sources into a single parse tree. This is also where validation
/// occurs; if there are any errors in include statements, those statements
/// are ignored when the tree is built, and an error is recorded: however we will
/// always attempt to construct *some* tree.
///
/// In the future, it should be possible to augment this type to allow for use by
/// a language server or similar, where an edit could be applied to a particular
/// source, and then only that source would need to be recompiled.
///
/// But we don't take advantage of that, yet, so this is currently more like
/// an intermediate type for generating a `ParseTree`.
///
/// [`generate_parse_tree`]: ParseContext::generate_parse_tree
#[derive(Debug)]
pub(crate) struct ParseContext {
    root_id: FileId,
    sources: Arc<SourceList>,
    parsed_files: HashMap<FileId, (Node, Vec<Diagnostic>)>,
    graph: IncludeGraph,
}

/// A simple graph of files and their includes.
///
/// We maintain this in order to validate that the input does not contain
/// any cyclical include statements, and does not exceed the maximum include
/// depth of 50.
#[derive(Clone, Debug, Default)]
struct IncludeGraph {
    // source file -> (destination file, span-in-source-for-error)
    nodes: HashMap<FileId, Vec<(FileId, Range<usize>)>>,
}

/// An include statement in a source file.
pub struct IncludeStatement(pub(crate) typed::Include);

struct IncludeError {
    file: FileId,
    /// the index of the problem statement, in the list of that file's includes
    statement_idx: usize,
    range: Range<usize>,
    kind: IncludeErrorKind,
}

enum IncludeErrorKind {
    Cycle,
    ToDeep,
}

impl IncludeStatement {
    /// The path part of the statement.
    ///
    /// For the statement `include(file.fea)`, this is `file.fea`.
    fn path(&self) -> &str {
        &self.0.path().text
    }

    /// The range of the entire include statement.
    fn stmt_range(&self) -> Range<usize> {
        self.0.range()
    }

    /// The range of just the path text.
    fn path_range(&self) -> Range<usize> {
        self.0.path().range()
    }
}

impl ParseContext {
    /// Attempt to parse the feature file at `path` and any includes.
    ///
    /// This will only error in unrecoverable cases, such as if `path` cannot
    /// be read.
    ///
    /// After parsing, you can call [`generate_parse_tree`] in order to generate
    /// a unified parse tree suitable for compilation.
    ///
    /// [`generate_parse_tree`]: ParseContext::generate_parse_tree
    pub(crate) fn parse(
        path: OsString,
        glyph_map: Option<&GlyphMap>,
        resolver: Box<dyn SourceResolver>,
    ) -> Result<Self, SourceLoadError> {
        let mut sources = SourceLoader::new(resolver);
        let root_id = sources.source_for_path(&path, None)?;
        let mut queue = vec![root_id];
        let mut parsed_files = HashMap::new();
        let mut includes = IncludeGraph::default();

        while let Some(id) = queue.pop() {
            // skip things we've already parsed.
            if parsed_files.contains_key(&id) {
                continue;
            }
            let source = sources.get(&id).unwrap();
            let (node, mut errors, include_stmts) = parse_src(source, glyph_map);
            errors.iter_mut().for_each(|e| e.message.file = id);

            parsed_files.insert(source.id(), (node, errors));
            if include_stmts.is_empty() {
                continue;
            }

            // we need to drop `source` so we can mutate source_map below
            let source_id = source.id();

            for include in &include_stmts {
                match sources.source_for_path(&include.path(), Some(source_id)) {
                    Ok(included_id) => {
                        includes.add_edge(id, (included_id, include.stmt_range()));
                        queue.push(included_id);
                    }
                    Err(e) => {
                        let range = include.path_range();
                        parsed_files.get_mut(&id).unwrap().1.push(Diagnostic::error(
                            id,
                            range,
                            e.to_string(),
                        ));
                    }
                }
            }
        }

        Ok(ParseContext {
            root_id,
            sources: sources.into_inner(),
            parsed_files,
            graph: includes,
        })
    }

    pub(crate) fn root_id(&self) -> FileId {
        self.root_id
    }

    /// Construct a `ParseTree`, and return any diagnostics.
    ///
    /// This method also performs validation of include statements.
    pub(crate) fn generate_parse_tree(self) -> (ParseTree, DiagnosticSet) {
        let mut all_errors = self
            .parsed_files
            .iter()
            .flat_map(|(_, (_, errs))| errs.iter())
            .cloned()
            .collect::<Vec<_>>();
        let include_errors = self.graph.validate(self.root_id());
        // record any errors:
        for IncludeError {
            file, range, kind, ..
        } in &include_errors
        {
            // find statement
            let message = match kind {
                IncludeErrorKind::Cycle => "cyclical include statement",
                IncludeErrorKind::ToDeep => "exceded maximum include depth",
            };
            all_errors.push(Diagnostic::error(*file, range.clone(), message));
        }

        let mut map = SourceMap::default();
        let mut root = self.generate_recurse(self.root_id(), &include_errors, &mut map, 0);
        let needs_update_positions = self.parsed_files.len() > 1;
        // we need to do this before updating positions, since it mutates and
        // requires that there exist only one reference (via Arc) to the node
        drop(self.parsed_files);
        if needs_update_positions {
            root.update_positions_from_root();
        }

        let diagnostics = DiagnosticSet {
            messages: all_errors,
            sources: self.sources.clone(),
            max_to_print: usize::MAX,
        };

        (
            ParseTree {
                root,
                map: Arc::new(map),
                sources: self.sources,
            },
            diagnostics,
        )
    }

    /// recursively construct the output tree.
    fn generate_recurse(
        &self,
        id: FileId,
        skip: &[IncludeError],
        source_map: &mut SourceMap,
        offset: usize,
    ) -> Node {
        let this_node = self.parsed_files[&id].0.clone();
        let self_len = this_node.text_len();
        let mut self_pos = 0;
        let mut global_pos = offset;
        let this_node = match self.graph.includes_for_file(id) {
            Some(includes) => {
                let mut edits = Vec::with_capacity(includes.len());

                for (i, (child_id, stmt)) in includes.iter().enumerate() {
                    if skip
                        .iter()
                        .any(|err| err.file == id && err.statement_idx == i)
                    {
                        continue;
                    }
                    // add everything up to this attach to the sourcemap
                    let pre_len = stmt.start - self_pos;
                    let pre_range = global_pos..global_pos + pre_len;
                    source_map.add_entry(pre_range, (id, self_pos));
                    self_pos = stmt.end;
                    global_pos += pre_len;
                    let child_node = self.generate_recurse(*child_id, skip, source_map, global_pos);
                    global_pos += child_node.text_len();
                    edits.push((stmt.clone(), child_node));
                }
                this_node.edit(edits, true)
            }
            None => this_node,
        };
        // now add any remaining contents to source_map
        let remain_len = self_len - self_pos;
        let remaining_range = global_pos..global_pos + remain_len;
        source_map.add_entry(remaining_range, (id, self_pos));
        this_node
    }
}

impl IncludeGraph {
    fn add_edge(&mut self, from: FileId, to: (FileId, Range<usize>)) {
        self.nodes.entry(from).or_default().push(to);
    }

    fn includes_for_file(&self, file: FileId) -> Option<&[(FileId, Range<usize>)]> {
        self.nodes.get(&file).map(|f| f.as_slice())
    }

    /// Validate the graph of include statements, returning any problems.
    ///
    /// If the result is non-empty, each returned error should be converted to
    /// d to diagnostics by the caller, and those statements should
    /// not be resolved when building the final tree.
    fn validate(&self, root: FileId) -> Vec<IncludeError> {
        let edges = match self.nodes.get(&root) {
            None => return Vec::new(),
            Some(edges) => edges,
        };

        let mut stack = vec![(root, edges, 0_usize)];
        let mut seen = HashSet::new();
        let mut bad_edges = Vec::new();

        while let Some((node, edges, cur_edge)) = stack.pop() {
            if let Some((child, stmt)) = edges.get(cur_edge) {
                // push parent, advancing idx
                stack.push((node, edges, cur_edge + 1));
                if stack.len() >= MAX_INCLUDE_DEPTH - 1 {
                    bad_edges.push(IncludeError {
                        file: node,
                        statement_idx: cur_edge,
                        range: stmt.clone(),
                        kind: IncludeErrorKind::ToDeep,
                    });
                    continue;
                }

                // only recurse if we haven't seen this node yet
                if seen.insert(*child) {
                    if let Some(child_edges) = self.nodes.get(child) {
                        stack.push((*child, child_edges, 0));
                    }
                } else if stack.iter().any(|(ancestor, _, _)| ancestor == child) {
                    // we have a cycle
                    bad_edges.push(IncludeError {
                        file: node,
                        statement_idx: cur_edge,
                        range: stmt.clone(),
                        kind: IncludeErrorKind::Cycle,
                    });
                }
            }
        }
        bad_edges
    }
}

/// Parse a single source file.
pub(crate) fn parse_src(
    src: &Source,
    glyph_map: Option<&GlyphMap>,
) -> (Node, Vec<Diagnostic>, Vec<IncludeStatement>) {
    let mut sink = AstSink::new(src.text(), src.id(), glyph_map);
    {
        let mut parser = Parser::new(src.text(), &mut sink);
        super::grammar::root(&mut parser);
    }
    sink.finish()
}

#[cfg(test)]
mod tests {

    use std::ffi::OsStr;

    use super::*;
    use crate::{
        token_tree::{typed, TreeBuilder},
        Kind,
    };

    fn make_ids<const N: usize>() -> [FileId; N] {
        let mut result = [FileId::CURRENT_FILE; N];
        result.iter_mut().for_each(|id| *id = FileId::next());
        result
    }

    /// Ensure we error if there are cyclical includes
    #[test]
    fn cycle_detection() {
        let [a, b, c, d] = make_ids();
        let statement = {
            let mut builder = TreeBuilder::default();
            builder.start_node(Kind::IncludeNode);
            builder.token(Kind::IncludeKw, "include");
            builder.token(Kind::LParen, "(");
            builder.token(Kind::Path, "file.fea");
            builder.token(Kind::LParen, ")");
            builder.token(Kind::Semi, ";");
            builder.finish_node(false, None);
            builder.finish()
        };
        let statement = typed::Include::cast(&statement.into()).unwrap();
        let mut graph = IncludeGraph::default();
        graph.add_edge(a, (b, statement.range()));
        graph.add_edge(b, (c, statement.range()));
        graph.add_edge(c, (d, statement.range()));
        graph.add_edge(d, (b, statement.range()));

        let result = graph.validate(a);
        assert_eq!(result[0].file, d);
        assert_eq!(result[0].range, 0..18);
    }

    #[test]
    fn skip_cycle_in_build() {
        let parse = ParseContext::parse(
            "a".into(),
            None,
            Box::new(|path: &OsStr| match path.to_str().unwrap() {
                "a" => Ok("include(bb);".into()),
                "bb" => Ok("include(a);".into()),
                _ => Err(SourceLoadError::new(
                    path.to_owned(),
                    std::io::Error::new(std::io::ErrorKind::NotFound, "oh no"),
                )),
            }),
        )
        .unwrap();
        let (resolved, errs) = parse.generate_parse_tree();
        assert_eq!(errs.len(), 1);
        assert_eq!(resolved.root.text_len(), "include(bb);".len());
    }

    #[test]
    fn assembly_basic() {
        let file_a = "\
        include(b);\n\
        # hmm\n\
        include(c);";
        let file_b = "languagesystem dflt DFLT;\n";
        let file_c = "feature kern {\n pos a b 20;\n } kern;";

        let b_len = file_b.len();
        let c_len = file_c.len();

        let parse = ParseContext::parse(
            "file_a".into(),
            None,
            Box::new(|path: &OsStr| match path.to_str().unwrap() {
                "file_a" => Ok(file_a.into()),
                "b" => Ok(file_b.into()),
                "c" => Ok(file_c.into()),
                _ => Err(SourceLoadError::new(
                    path.into(),
                    std::io::Error::new(std::io::ErrorKind::NotFound, "oh no"),
                )),
            }),
        )
        .unwrap();

        let a_id = parse.sources.id_for_path("file_a").unwrap();
        let b_id = parse.sources.id_for_path("b").unwrap();
        let c_id = parse.sources.id_for_path("c").unwrap();

        let (resolved, errs) = parse.generate_parse_tree();
        assert!(errs.is_empty(), "{errs:?}");
        let top_level_nodes = resolved
            .root
            .iter_children()
            .filter_map(|n| n.as_node())
            .collect::<Vec<_>>();
        let inter_node_len = "\n# hmm\n".len();
        assert_eq!(top_level_nodes.len(), 2);
        assert_eq!(top_level_nodes[0].kind(), Kind::LanguageSystemNode);
        assert_eq!(top_level_nodes[0].range(), 0..b_len - 1); // ignore newline
        let node_2_start = b_len + inter_node_len;
        assert_eq!(
            top_level_nodes[1].range(),
            node_2_start..node_2_start + c_len,
        );
        assert_eq!(top_level_nodes[1].kind(), Kind::FeatureNode);

        //resolved.root.debug_print_structure(true);
        assert_eq!(resolved.map.resolve_range(10..15), (b_id, 10..15));
        assert_eq!(resolved.map.resolve_range(29..33), (a_id, 14..18));
        assert_eq!(resolved.map.resolve_range(49..52), (c_id, 16..19));
    }
}
