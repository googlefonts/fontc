//! source files

use std::{
    collections::{HashMap, HashSet},
    num::NonZeroU32,
    ops::Range,
    path::{Path, PathBuf},
};

use crate::{
    typed::{self, AstNode as _},
    util, AstSink, Diagnostic, GlyphMap, Node, Parser,
};

const MAX_INCLUDE_DEPTH: usize = 50;

/// Uniquely identifies a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct FileId(NonZeroU32);

impl FileId {
    pub fn next() -> FileId {
        use std::sync::atomic;
        static COUNTER: atomic::AtomicU32 = atomic::AtomicU32::new(1);
        FileId(NonZeroU32::new(COUNTER.fetch_add(1, atomic::Ordering::Relaxed)).unwrap())
    }
}

/// A single source file, corresponding to a file on disk.
///
/// We keep hold of all sources used in a given compilation so that we can
/// do error reporting.
#[derive(Clone, Debug)]
pub struct Source {
    id: FileId,
    /// The non-canonical path to this source, suitable for printing.
    path: PathBuf,
    contents: String,
    /// The index of each newline character, for efficiently fetching lines
    /// (for error reporting, e.g.)
    line_offsets: Vec<usize>,
}

/// A list of sources in a project.
#[derive(Clone, Debug)]
pub struct SourceList {
    project_root: PathBuf,
    root_id: FileId,
    ids: HashMap<util::paths::CanonicalPath, FileId>,
    sources: HashMap<FileId, Source>,
}

/// A map from positions in a resolved token tree (which may contain the
/// contents of multiple files) to locations in specific files.
#[derive(Clone, Debug, Default)]
pub struct SourceMap {
    /// sorted vec of (offset_in_combined_tree, (file_id, offest_in_source_file));
    offsets: Vec<(Range<usize>, (FileId, usize))>,
}

/// A simple graph of files and their includes.
///
/// We maintain this in order to validate that the input does not contain
/// any cyclical include statements, and does not exceed the maximum include
/// depth of 50.
#[derive(Clone, Debug, Default)]
struct IncludeGraph {
    nodes: HashMap<FileId, Vec<(FileId, typed::Include)>>,
}

// internal; we convert this to a diagnostic immediately.
#[cfg_attr(test, derive(Debug))]
enum IncludeError {
    Cycle(FileId),
    ToDeep(FileId),
}

/// An unrecoverable error that occurs during parsing.
///
/// This is in contrast with things like syntax errors, which we collect
/// and report when we're finished parsing.
pub enum HardError {
    /// A file could not be found or could not be read.
    IoError {
        path: PathBuf,
        cause: std::io::Error,
    },
}

/// The result of parsing a FEA file + its included files.
///
/// The idea here is that you can edit a single file and we can rebuild the tree
/// without needing to parse everything else again.
pub struct Parse {
    sources: SourceList,
    parsed_files: HashMap<FileId, Node>,
    errors: Vec<Diagnostic>,
    graph: IncludeGraph,
}

/// A fully parsed feature file, with attached imports and a sourcemap.
pub struct FeaTree {
    root: Node,
    map: SourceMap,
}

impl Source {
    pub fn new(path: impl Into<PathBuf>) -> Result<Self, HardError> {
        let path = path.into();
        let contents = std::fs::read_to_string(&path).map_err(|cause| HardError::IoError {
            path: path.clone(),
            cause,
        })?;
        let line_offsets = line_offsets(&contents);
        Ok(Source {
            path,
            id: FileId::next(),
            contents,
            line_offsets,
        })
    }

    #[cfg(test)]
    fn new_for_test(id: FileId, path: impl Into<PathBuf>, contents: impl Into<String>) -> Self {
        let contents = contents.into();
        Source {
            id,
            path: path.into(),
            line_offsets: line_offsets(&contents),
            contents,
        }
    }
}

fn line_offsets(text: &str) -> Vec<usize> {
    // we could use memchar for this; benefits would require benchmarking
    text.bytes()
        .enumerate()
        .filter_map(|(i, b)| if b == b'\n' { Some(i) } else { None })
        .collect()
}

impl SourceMap {
    fn add_entry(&mut self, src: Range<usize>, dest: (FileId, usize)) {
        if !src.is_empty() {
            self.offsets.push((src, dest));
        }
    }

    /// panics if `global_range` crosses a file barrier?
    fn resolve_range(&self, global_range: Range<usize>) -> (FileId, Range<usize>) {
        // it is hard to imagine more than a couple hundred include statements,
        // and even that would be extremely rare, so I don't think it's really
        // worth doing a binary search here?
        let (chunk, (file, local_offset)) = self
            .offsets
            .iter()
            .find(|item| item.0.contains(&global_range.start))
            .unwrap();
        let chunk_offset = global_range.start - chunk.start;
        let range_start = *local_offset + chunk_offset;
        let len = global_range.end - global_range.start;
        (*file, range_start..range_start + len)
    }
}

impl IncludeGraph {
    fn add_edge(&mut self, from: FileId, to: (FileId, typed::Include)) {
        self.nodes.entry(from).or_default().push(to);
    }

    fn includes_for_file(&self, file: FileId) -> Option<&[(FileId, typed::Include)]> {
        self.nodes.get(&file).map(|f| f.as_slice())
    }

    fn validate(&self, root: FileId) -> Result<(), IncludeError> {
        // rules:
        // - no cycles
        // - max depth of 50

        // ... how do we do DFS again

        // (parent, node)
        let edges = match self.nodes.get(&root) {
            None => return Ok(()),
            Some(edges) => edges,
        };

        let mut stack = vec![(root, edges, 0_usize)];
        let mut seen = HashSet::new();

        while let Some((node, edges, cur_edge)) = stack.pop() {
            if let Some((child, _)) = edges.get(cur_edge) {
                // push parent, advancing idx
                stack.push((node, edges, cur_edge + 1));
                if stack.len() >= MAX_INCLUDE_DEPTH - 1 {
                    return Err(IncludeError::ToDeep(*child));
                }

                // only recurse if we haven't seen this node yet
                if seen.insert(*child) {
                    if let Some(child_edges) = self.nodes.get(child) {
                        stack.push((*child, child_edges, 0));
                    }
                } else if stack.iter().any(|(ancestor, _, _)| ancestor == child) {
                    // we have a cycle

                    return Err(IncludeError::Cycle(*child));
                }
            }
        }

        Ok(())
    }
}

impl SourceList {
    fn new(project_root: Option<PathBuf>, root_fea: PathBuf) -> Result<Self, HardError> {
        assert!(root_fea.exists());
        assert!(root_fea.is_file());
        let project_root = project_root.unwrap_or_else(|| root_fea.parent().unwrap().to_owned());
        let canonical =
            util::paths::CanonicalPath::new(&root_fea).map_err(|cause| HardError::IoError {
                path: root_fea.clone(),
                cause,
            })?;
        let source = Source::new(root_fea)?;
        let root_id = source.id;

        let mut myself = SourceList {
            project_root,
            root_id,
            ids: Default::default(),
            sources: Default::default(),
        };

        myself.ids.insert(canonical, source.id);
        myself.sources.insert(source.id, source);
        Ok(myself)
    }

    #[cfg(test)]
    fn new_for_debug(root_id: FileId) -> Self {
        SourceList {
            root_id,
            project_root: Default::default(),
            ids: Default::default(),
            sources: Default::default(),
        }
    }

    fn root_id(&self) -> FileId {
        self.root_id
    }

    fn get(&self, id: &FileId) -> Option<&Source> {
        self.sources.get(id)
    }

    /// Attempt to load the source at `path`.
    ///
    /// If an error occurs whe loading the file, returns an Error.
    ///
    /// `path` should have already been normalized with `util::paths::resolve_path`.
    fn source_for_path(&mut self, path: PathBuf) -> Result<FileId, HardError> {
        let canonical =
            util::paths::CanonicalPath::new(&path).map_err(|cause| HardError::IoError {
                cause,
                path: path.clone(),
            })?;
        if let Some(src) = self.ids.get(&canonical) {
            return Ok(*src);
        }

        let source = Source::new(path)?;
        let id = source.id;
        self.ids.insert(canonical, id);
        self.sources.insert(id, source);
        Ok(id)
    }
}

impl Parse {
    fn assemble(&self) -> FeaTree {
        let mut map = SourceMap::default();
        let root = self.assemble_recurse(self.sources.root_id, &mut map, 0);
        FeaTree { root, map }
    }

    fn assemble_recurse(&self, id: FileId, source_map: &mut SourceMap, offset: usize) -> Node {
        let this_node = self.parsed_files[&id].clone();
        let self_len = this_node.text_len();
        let mut self_pos = 0;
        let mut global_pos = offset;
        let this_node = match self.graph.includes_for_file(id) {
            Some(includes) => {
                let mut edits = Vec::with_capacity(includes.len());

                for (child_id, stmt) in includes {
                    let stmt_range = stmt.range();
                    // add everything up to this attach to the sourcemap
                    let pre_len = stmt_range.start - self_pos;
                    let pre_range = global_pos..global_pos + pre_len;
                    source_map.add_entry(pre_range, (id, self_pos));
                    self_pos = stmt_range.end;
                    global_pos += pre_len;
                    let child_node = self.assemble_recurse(*child_id, source_map, global_pos);
                    global_pos += child_node.text_len();
                    edits.push((stmt_range, child_node));
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

/// the main entrypoint for parsing
pub fn parse_root(
    path: impl Into<PathBuf>,
    glyph_map: Option<&GlyphMap>,
    project_root: Option<PathBuf>,
) -> Result<(), HardError> {
    let path = path.into();

    let mut source_map = SourceList::new(project_root, path)?;
    let mut queue = vec![source_map.root_id()];
    let mut results = HashMap::<FileId, Node>::new();
    let mut includes = IncludeGraph::default();
    let mut all_errs = Vec::new();

    while let Some(id) = queue.pop() {
        // skip things we've already parsed.
        if results.contains_key(&id) {
            continue;
        }
        let source = source_map.get(&id).unwrap();
        let (node, mut errors, include_stmts) = parse_source(&source.contents, glyph_map);

        // we need to drop `source` so we can mutate source_map to add new includes
        let source_path = if !include_stmts.is_empty() {
            source.path.clone()
        } else {
            PathBuf::new()
        };

        all_errs.append(&mut errors);
        results.insert(source.id, node);
        for include in &include_stmts {
            let path_token = include.path();
            let path = Path::new(path_token.text.as_str());
            let path = util::paths::resolve_path(
                path,
                &source_map.project_root,
                source_path
                    .parent()
                    .expect("always a file; must have parent"),
            );

            match source_map.source_for_path(path) {
                Ok(included_id) => {
                    includes.add_edge(id, (included_id, include.clone()));
                    queue.push(included_id);
                }
                Err(HardError::IoError { cause, .. }) => {
                    let range = path_token.range();
                    all_errs.push(Diagnostic::error(
                        range,
                        format!("Unable to resolve import: '{}'", cause),
                    ));
                }
            }
        }
    }
    Ok(())
}

fn parse_source(
    src: &str,
    glyph_map: Option<&GlyphMap>,
) -> (Node, Vec<Diagnostic>, Vec<typed::Include>) {
    let mut sink = AstSink::new(src, glyph_map);
    let mut parser = Parser::new(src, &mut sink);
    crate::parse::grammar::root(&mut parser);
    sink.finish2()
}

#[cfg(test)]
mod tests {
    use std::iter::FromIterator;

    use crate::{token_tree::TreeBuilder, Kind};

    use super::*;

    fn make_ids<const N: usize>() -> [FileId; N] {
        let one = FileId::next();
        let mut result = [one; N];
        for i in 1..N {
            result[i] = FileId::next();
        }
        result
    }

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
        graph.add_edge(a, (b, statement.clone()));
        graph.add_edge(b, (c, statement.clone()));
        graph.add_edge(c, (d, statement.clone()));
        graph.add_edge(d, (b, statement.clone()));

        let r = graph.validate(a);
        assert!(
            matches!(r, Err(IncludeError::Cycle(id)) if id == b ),
            "{:?}",
            r
        );
    }

    #[test]
    fn assembly_basic() {
        let [a, b, c] = make_ids();
        let sources = SourceList::new_for_debug(a);
        let file_a = "\
        include(b);\n\
        # hmm\n\
        include(c);";
        let file_b = "languagesystem dflt DFLT;\n";
        let file_c = "feature kern {\n pos a b 20;\n } kern;";

        let (node_a, er, includes) = parse_source(file_a, None);
        assert!(er.is_empty());
        let (node_b, er, _) = parse_source(file_b, None);
        assert!(er.is_empty());
        let (node_c, er, _) = parse_source(file_c, None);
        assert!(er.is_empty());
        let mut graph = IncludeGraph::default();
        for include in includes {
            match include.path().text.as_str() {
                "b" => graph.add_edge(a, (b, include)),
                "c" => graph.add_edge(a, (c, include)),
                other => panic!("unexpectd include '{}'", other),
            }
        }
        let parse = Parse {
            sources,
            parsed_files: HashMap::from_iter([(a, node_a), (b, node_b), (c, node_c)]),
            errors: Default::default(),
            graph,
        };

        let resolved = parse.assemble();
        let top_level_nodes = resolved
            .root
            .iter_children()
            .filter_map(|n| n.as_node())
            .collect::<Vec<_>>();
        let inter_node_len = "\n# hmm\n".len();
        assert_eq!(top_level_nodes.len(), 2);
        assert_eq!(top_level_nodes[0].kind(), Kind::LanguageSystemNode);
        assert_eq!(top_level_nodes[0].range(), 0..file_b.len() - 1); // ignore newline
        let node_2_start = file_b.len() + inter_node_len;
        assert_eq!(
            top_level_nodes[1].range(),
            node_2_start..node_2_start + file_c.len()
        );
        assert_eq!(top_level_nodes[1].kind(), Kind::FeatureNode);

        resolved.root.debug_print_structure(true);
        assert_eq!(resolved.map.resolve_range(10..15), (b, 10..15));
        assert_eq!(resolved.map.resolve_range(29..33), (a, 14..18));
        assert_eq!(resolved.map.resolve_range(49..52), (c, 16..19));
    }
}
