use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::path::{Path, PathBuf};

use super::source::Source;
use super::{FileId, Parser, SourceList, SourceMap};
use crate::{
    token_tree::{
        typed::{self, AstNode as _},
        AstSink,
    },
    util, Diagnostic, GlyphMap, Node,
};

const MAX_INCLUDE_DEPTH: usize = 50;

/// A simple graph of files and their includes.
///
/// We maintain this in order to validate that the input does not contain
/// any cyclical include statements, and does not exceed the maximum include
/// depth of 50.
#[derive(Clone, Debug, Default)]
struct IncludeGraph {
    // (source file, (destination file, span-in-source-for-error))
    nodes: HashMap<FileId, Vec<(FileId, Range<usize>)>>,
}

/// The result of parsing a FEA file + its included files.
///
/// The idea here is that you can edit a single file and we can rebuild the tree
/// without needing to parse everything else again.
///
/// But we don't take advantage of that, yet, so this is currently more like
/// an intermediate type generates a `ParseTree`.
pub struct ParseContext {
    sources: SourceList,
    parsed_files: HashMap<FileId, (Node, Vec<Diagnostic>)>,
    graph: IncludeGraph,
}

/// A fully parsed feature file, with attached imports and a sourcemap.
pub struct ParseTree {
    root: Node,
    sources: SourceList,
    map: SourceMap,
}

/// An unrecoverable error that occurs during parsing.
///
/// This is in contrast with things like syntax errors, which we collect
/// and report when we're finished parsing.
#[derive(Debug)]
pub enum HardError {
    /// A file could not be found or could not be read.
    IoError {
        path: PathBuf,
        cause: std::io::Error,
    },
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
    pub fn path(&self) -> &str {
        &self.0.path().text
    }

    /// The range of the entire include statement.
    pub fn stmt_range(&self) -> Range<usize> {
        self.0.range()
    }

    /// The range of just the path text.
    //FIXME: is this accurate? has it seen a cursor yet?
    pub fn path_range(&self) -> Range<usize> {
        self.0.path().range()
    }
}

impl ParseContext {
    pub fn root_id(&self) -> FileId {
        self.sources.root_id()
    }

    pub fn get_raw(&self, file: FileId) -> Option<&(Node, Vec<Diagnostic>)> {
        self.parsed_files.get(&file)
    }

    /// Attempt to parse the feature file at `path` and any includes.
    ///
    /// This will only error in unrecoverable cases, such as if `path` cannot
    /// be read.
    ///
    /// After parsing, you can call [`generate_parse_tree`] in order to generate
    /// a unified parse tree suitable for compilation.
    pub fn generate(
        path: PathBuf,
        glyph_map: Option<&GlyphMap>,
        project_root: Option<PathBuf>,
    ) -> Result<Self, HardError> {
        let source = Source::from_path(path).map_err(|e| HardError::IoError {
            path: e.path,
            cause: e.cause,
        })?;
        let mut sources = SourceList::new(source, project_root);
        let mut queue = vec![sources.root_id()];
        let mut parsed_files = HashMap::new();
        let mut includes = IncludeGraph::default();

        while let Some(id) = queue.pop() {
            // skip things we've already parsed.
            if parsed_files.contains_key(&id) {
                continue;
            }
            let source = sources.get(&id).unwrap();
            let (node, mut errors, include_stmts) = parse_src(&source, glyph_map);
            errors.iter_mut().for_each(|e| e.message.file = id);

            parsed_files.insert(source.id(), (node, errors));
            if include_stmts.is_empty() {
                continue;
            }

            // we need to drop `source` so we can mutate source_map below
            let source_path = source.path().map(PathBuf::from);

            for include in &include_stmts {
                let path = Path::new(include.path());
                let path = util::paths::resolve_path(
                    path,
                    sources.project_root(),
                    source_path.as_ref().and_then(|p| p.parent()),
                );

                match sources.source_for_path(path) {
                    Ok(included_id) => {
                        includes.add_edge(id, (included_id, include.stmt_range()));
                        queue.push(included_id);
                    }
                    Err(e) => {
                        let range = include.path_range();
                        parsed_files.get_mut(&id).unwrap().1.push(Diagnostic::error(
                            id,
                            range,
                            format!("Unable to resolve import: '{}'", e.cause),
                        ));
                    }
                }
            }
        }

        Ok(ParseContext {
            sources,
            parsed_files,
            graph: includes,
        })
    }

    pub fn has_errors(&self) -> bool {
        self.parsed_files
            .values()
            .flat_map(|(_, errs)| errs.iter())
            .any(|e| e.is_error())
    }

    /// If there are no errors, returns a generated `ParseTree`.
    pub fn generate_parse_tree(&self) -> (ParseTree, Vec<Diagnostic>) {
        let mut all_errors = self
            .parsed_files
            .iter()
            .flat_map(|(_, (_, errs))| errs.iter())
            .cloned()
            .collect::<Vec<_>>();
        let include_errors = self.graph.validate(self.sources.root_id());
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
        let root = self.assemble_recurse(self.sources.root_id(), &include_errors, &mut map, 0);
        (
            ParseTree {
                root,
                map,
                sources: self.sources.clone(),
            },
            all_errors,
        )
    }

    fn assemble_recurse(
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
                    let child_node = self.assemble_recurse(*child_id, skip, source_map, global_pos);
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

//TODO: move to another file if this gets at all big
impl ParseTree {
    pub fn root(&self) -> &Node {
        &self.root
    }

    pub fn typed_root(&self) -> typed::Root {
        typed::Root::try_from_node(&self.root).expect("parse tree has invalid root node type")
    }

    pub fn source_map(&self) -> &SourceMap {
        &self.map
    }

    pub fn get_source(&self, id: FileId) -> Option<&Source> {
        self.sources.get(&id)
    }

    pub fn format_diagnostic(&self, err: &Diagnostic) -> String {
        let mut s = String::new();
        let source = self.get_source(err.message.file).unwrap();
        crate::util::highlighting::write_diagnostic(&mut s, err, source, None);
        s
    }
}

/// Parse a single source file.
pub fn parse_src(
    src: &Source,
    glyph_map: Option<&GlyphMap>,
) -> (Node, Vec<Diagnostic>, Vec<IncludeStatement>) {
    let mut sink = AstSink::new(src.contents(), src.id(), glyph_map);
    let mut parser = Parser::new(src.contents(), &mut sink);
    super::grammar::root(&mut parser);
    sink.finish()
}

#[cfg(test)]
mod tests {
    use std::iter::FromIterator;

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
        let file_a = Source::from_str("include(bb);");
        let file_b = Source::from_str("include(a)");
        let (a, b) = (file_a.id(), file_b.id());
        let a_len = file_a.contents().len();

        let (node_a, er, includes_a) = parse_src(&file_a, None);
        assert!(er.is_empty());
        let (node_b, er, includes_b) = parse_src(&file_b, None);
        assert!(!er.is_empty());
        let mut graph = IncludeGraph::default();
        graph.add_edge(a, (b, includes_a[0].path_range()));
        graph.add_edge(b, (a, includes_b[0].path_range()));

        let mut sources = SourceList::new(file_a, None);
        sources.add_source(file_b);

        let parse = ParseContext {
            sources,
            parsed_files: HashMap::from_iter([(a, (node_a, vec![])), (b, (node_b, vec![]))]),
            graph,
        };

        let (resolved, errs) = parse.generate_parse_tree();
        assert_eq!(errs.len(), 1);
        assert_eq!(resolved.root.text_len(), a_len);
    }

    #[test]
    fn assembly_basic() {
        let file_a = Source::from_str(
            "\
        include(b);\n\
        # hmm\n\
        include(c);",
        );
        let file_b = Source::from_str("languagesystem dflt DFLT;\n");
        let file_c = Source::from_str("feature kern {\n pos a b 20;\n } kern;");

        let (a, b, c) = (file_a.id(), file_b.id(), file_c.id());
        let b_len = file_b.contents().len();
        let c_len = file_c.contents().len();

        let (node_a, er, includes) = parse_src(&file_a, None);
        assert!(er.is_empty());
        let (node_b, er, _) = parse_src(&file_b, None);
        assert!(er.is_empty());
        let (node_c, er, _) = parse_src(&file_c, None);
        assert!(er.is_empty());
        let mut graph = IncludeGraph::default();
        for include in includes {
            match include.path() {
                "b" => graph.add_edge(a, (b, include.stmt_range())),
                "c" => graph.add_edge(a, (c, include.stmt_range())),
                other => panic!("unexpectd include '{}'", other),
            }
        }

        let mut sources = SourceList::new(file_a, None);
        sources.add_source(file_b);
        sources.add_source(file_c);

        let parse = ParseContext {
            sources,
            parsed_files: HashMap::from_iter([
                (a, (node_a, vec![])),
                (b, (node_b, vec![])),
                (c, (node_c, vec![])),
            ]),
            graph,
        };

        let (resolved, errs) = parse.generate_parse_tree();
        assert!(errs.is_empty());
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

        resolved.root.debug_print_structure(true);
        assert_eq!(resolved.map.resolve_range(10..15), (b, 10..15));
        assert_eq!(resolved.map.resolve_range(29..33), (a, 14..18));
        assert_eq!(resolved.map.resolve_range(49..52), (c, 16..19));
    }
}
