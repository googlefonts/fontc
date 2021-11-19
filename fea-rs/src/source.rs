//! source files

use std::{
    collections::{HashMap, HashSet},
    num::NonZeroU32,
    path::{Path, PathBuf},
};

use crate::token_tree::{typed, Token};
use crate::util;
use crate::{AstSink, Diagnostic, GlyphMap, Node, Parser};

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

impl Source {
    pub fn new(path: impl Into<PathBuf>) -> Result<Self, HardError> {
        let path = path.into();
        let contents = std::fs::read_to_string(&path).map_err(|cause| HardError::IoError {
            path: path.clone(),
            cause,
        })?;
        // we could use memchar for this; benefits would require benchmarking
        let line_offsets = contents
            .bytes()
            .enumerate()
            .filter_map(|(i, b)| if b == b'\n' { Some(i) } else { None })
            .collect();
        Ok(Source {
            path,
            id: FileId::next(),
            contents,
            line_offsets,
        })
    }
}

#[derive(Clone, Debug)]
pub struct SourceMap {
    project_root: PathBuf,
    root_id: FileId,
    ids: HashMap<util::paths::CanonicalPath, FileId>,
    sources: HashMap<FileId, Source>,
}

#[derive(Clone, Debug, Default)]
struct IncludeGraph {
    nodes: HashMap<FileId, Vec<(FileId, Token)>>,
}

impl IncludeGraph {
    fn add_edge(&mut self, from: FileId, to: (FileId, Token)) {
        self.nodes.entry(from).or_default().push(to);
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
                    if let Some(child_edges) = self.nodes.get(&child) {
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

// internal; we convert this to a diagnostic immediately.
#[cfg_attr(test, derive(Debug))]
enum IncludeError {
    Cycle(FileId),
    ToDeep(FileId),
}

//struct ParseCtx1<'a> {
//glyph_map: Option<&'a GlyphMap>,
//sources: SourceMap,
//errors: Vec<Diagnostic>,
//results: HashMap<FileId, Node>,
//}

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

impl SourceMap {
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

        let mut myself = SourceMap {
            project_root,
            root_id,
            ids: Default::default(),
            sources: Default::default(),
        };

        myself.ids.insert(canonical, source.id);
        myself.sources.insert(source.id, source);
        Ok(myself)
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

impl ParseCtx1<'_> {
    //fn project_root(&self) -> &Path {
    //self.explicit_root.as_ref().unwrap_or(&self.top_level_path)
    //}

    //fn resolve_path(&self, include_path: &Path, parent_path: &Path) -> PathBuf {
    //if include_path.is_absolute() {
    //return include_path.to_owned();
    //}

    //let relative_to_project_root = |rel_path: PathBuf| {
    //let root = self.explicit_root.as_ref().unwrap_or(&self.top_level_path);
    //match rel_path
    //.canonicalize()
    //.expect("already valididated")
    //.strip_prefix(root)
    //{
    //Ok(path) => path.to_owned(),
    //// this would happen if we ran into some symlinks or something?
    //// these code paths are going to be badly exercised, idk
    //Err(_) => rel_path,
    //}
    //};

    //if let Some(root) = self.explicit_root.as_ref() {
    //let maybe = root.join(include_path);
    //if maybe.exists() {
    //return relative_to_project_root(maybe).canonicalize().unwrap();
    //}
    //}

    //let maybe = self.top_level_path.join(include_path);
    //if maybe.exists() {
    //return relative_to_project_root(maybe);
    //}
    //if let Some(parent) = parent_path.parent() {
    //let maybe = parent.join(include_path);
    //if maybe.exists() {
    //return relative_to_project_root(maybe);
    //}
    //}

    //// fallback, just return input, we will error later
    //include_path.to_owned()
    //}
}

/// the main entrypoint for parsing
pub fn parse_root(
    path: impl Into<PathBuf>,
    glyph_map: Option<&GlyphMap>,
    project_root: Option<PathBuf>,
) -> Result<(), HardError> {
    let path = path.into();

    let mut source_map = SourceMap::new(project_root, path.into())?;
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
        let (node, mut errors, include_stmts) = parse_source(&source);

        // we need to drop `source` so we can mutate source_map to add new includes
        let source_path = if !include_stmts.is_empty() {
            source.path.clone()
        } else {
            PathBuf::new()
        };

        all_errs.extend(errors.drain(..));
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
                    includes.add_edge(id, (included_id, path_token.to_owned()));
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

fn parse_source(src: &Source) -> (Node, Vec<Diagnostic>, Vec<typed::Include>) {
    let mut sink = AstSink::new(&src.contents, None);
    let mut parser = Parser::new(&src.contents, &mut sink);
    crate::parse::grammar::root(&mut parser);
    sink.finish2()
}

#[cfg(test)]
mod tests {
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
        let [a, b, c, d, e] = make_ids();
        let token = Token::new_for_test(crate::Kind::Path, "/");
        let mut graph = IncludeGraph::default();
        graph.add_edge(a, (b, token.clone()));
        graph.add_edge(b, (c, token.clone()));
        graph.add_edge(c, (d, token.clone()));
        graph.add_edge(d, (b, token.clone()));

        let r = graph.validate(a);
        assert!(
            matches!(r, Err(IncludeError::Cycle(id)) if id == b ),
            "{:?}",
            r
        );
    }
}
