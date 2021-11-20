use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::path::{Path, PathBuf};

use super::{AstSink, FileId, Parser, SourceList, SourceMap};
use crate::token_tree::typed::{self, AstNode as _};
use crate::{util, Diagnostic, GlyphMap, Node};

const MAX_INCLUDE_DEPTH: usize = 50;

/// A simple graph of files and their includes.
///
/// We maintain this in order to validate that the input does not contain
/// any cyclical include statements, and does not exceed the maximum include
/// depth of 50.
#[derive(Clone, Debug, Default)]
struct IncludeGraph {
    nodes: HashMap<FileId, Vec<(FileId, typed::Include)>>,
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
pub enum HardError {
    /// A file could not be found or could not be read.
    IoError {
        path: PathBuf,
        cause: std::io::Error,
    },
}

/// internal; we convert this to a diagnostic immediately.
#[cfg_attr(test, derive(Debug))]
enum IncludeError {
    Cycle { file: FileId, span: Range<usize> },
    ToDeep { file: FileId, span: Range<usize> },
}

impl ParseContext {
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
        let path = path.into();

        let mut sources = SourceList::new(project_root, path).map_err(|e| HardError::IoError {
            path: e.path,
            cause: e.cause,
        })?;
        let mut queue = vec![sources.root_id()];
        let mut parsed_files = HashMap::new();
        let mut includes = IncludeGraph::default();

        while let Some(id) = queue.pop() {
            // skip things we've already parsed.
            if parsed_files.contains_key(&id) {
                continue;
            }
            let source = sources.get(&id).unwrap();
            let (node, errors, include_stmts) = parse_source(source.contents(), glyph_map);

            // we need to drop `source` so we can mutate source_map to add new includes
            let source_path = if !include_stmts.is_empty() {
                source.path().to_owned()
            } else {
                PathBuf::new()
            };

            parsed_files.insert(source.id(), (node, errors));
            for include in &include_stmts {
                let path_token = include.path();
                let path = Path::new(path_token.text.as_str());
                let path = util::paths::resolve_path(
                    path,
                    sources.project_root(),
                    source_path
                        .parent()
                        .expect("always a file; must have parent"),
                );

                match sources.source_for_path(path) {
                    Ok(included_id) => {
                        includes.add_edge(id, (included_id, include.clone()));
                        queue.push(included_id);
                    }
                    Err(e) => {
                        let range = path_token.range();
                        parsed_files.get_mut(&id).unwrap().1.push(Diagnostic::error(
                            range,
                            format!("Unable to resolve import: '{}'", e.cause),
                        ));
                    }
                }
            }
        }

        if let Err(e) = includes.validate(sources.root_id()) {
            match e {
                IncludeError::Cycle { file, span } => {
                    //TODO: we could have a fancy error here showing the cycle
                    let err = Diagnostic::error(span.clone(), "cyclical include statement");
                    parsed_files.get_mut(&file).unwrap().1.push(err);
                }
                IncludeError::ToDeep { file, span } => {
                    let err = Diagnostic::error(span.clone(), "maximum include depth exceded");
                    parsed_files.get_mut(&file).unwrap().1.push(err);
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
    pub fn generate_parse_tree(&self) -> Option<ParseTree> {
        if self.has_errors() {
            return None;
        }

        let mut map = SourceMap::default();
        let root = self.assemble_recurse(self.sources.root_id(), &mut map, 0);
        Some(ParseTree {
            root,
            map,
            sources: self.sources.clone(),
        })
    }

    fn assemble_recurse(&self, id: FileId, source_map: &mut SourceMap, offset: usize) -> Node {
        let this_node = self.parsed_files[&id].0.clone();
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

impl IncludeGraph {
    fn add_edge(&mut self, from: FileId, to: (FileId, typed::Include)) {
        self.nodes.entry(from).or_default().push(to);
    }

    fn includes_for_file(&self, file: FileId) -> Option<&[(FileId, typed::Include)]> {
        self.nodes.get(&file).map(|f| f.as_slice())
    }

    fn validate(&self, root: FileId) -> Result<(), IncludeError> {
        let edges = match self.nodes.get(&root) {
            None => return Ok(()),
            Some(edges) => edges,
        };

        let mut stack = vec![(root, edges, 0_usize)];
        let mut seen = HashSet::new();

        while let Some((node, edges, cur_edge)) = stack.pop() {
            if let Some((child, stmt)) = edges.get(cur_edge) {
                // push parent, advancing idx
                stack.push((node, edges, cur_edge + 1));
                if stack.len() >= MAX_INCLUDE_DEPTH - 1 {
                    return Err(IncludeError::ToDeep {
                        file: node,
                        span: stmt.range(),
                    });
                }

                // only recurse if we haven't seen this node yet
                if seen.insert(*child) {
                    if let Some(child_edges) = self.nodes.get(child) {
                        stack.push((*child, child_edges, 0));
                    }
                } else if stack.iter().any(|(ancestor, _, _)| ancestor == child) {
                    // we have a cycle
                    return Err(IncludeError::Cycle {
                        file: node,
                        span: stmt.range(),
                    });
                }
            }
        }

        Ok(())
    }
}

fn parse_source(
    src: &str,
    glyph_map: Option<&GlyphMap>,
) -> (
    Node,
    Vec<Diagnostic>,
    Vec<crate::token_tree::typed::Include>,
) {
    let mut sink = AstSink::new(src, glyph_map);
    let mut parser = Parser::new(src, &mut sink);
    crate::parse::grammar::root(&mut parser);
    sink.finish2()
}

#[cfg(test)]
mod tests {
    use crate::{
        token_tree::{typed, TreeBuilder},
        Kind,
    };

    use std::iter::FromIterator;

    use super::super::make_ids;
    use super::*;

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
        graph.add_edge(a, (b, statement.clone()));
        graph.add_edge(b, (c, statement.clone()));
        graph.add_edge(c, (d, statement.clone()));
        graph.add_edge(d, (b, statement.clone()));

        let r = graph.validate(a);
        eprintln!("{:?}", d);
        assert!(
            matches!(r, Err(IncludeError::Cycle { file: id, span: Range { start: 0, end: 18 } }) if id == d ),
            "{:?}",
            r
        );
    }

    #[test]
    fn assembly_basic() {
        let [a, b, c] = make_ids();
        let sources = SourceList::new_for_test(a);
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
        let parse = ParseContext {
            sources,
            parsed_files: HashMap::from_iter([
                (a, (node_a, vec![])),
                (b, (node_b, vec![])),
                (c, (node_c, vec![])),
            ]),
            graph,
        };

        let resolved = parse.generate_parse_tree().unwrap();
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
