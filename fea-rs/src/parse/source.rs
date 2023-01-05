//! source files

use std::{
    collections::HashMap,
    num::NonZeroU32,
    ops::Range,
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::util;

/// Uniquely identifies a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct FileId(NonZeroU32);

/// A single source file, corresponding to a file on disk.
///
/// We keep hold of all sources used in a given compilation so that we can
/// do error reporting.
#[derive(Clone, Debug)]
pub struct Source {
    id: FileId,
    /// The non-canonical path to this source, suitable for printing.
    path: Option<PathBuf>,
    contents: Arc<str>,
    /// The index of each newline character, for efficiently fetching lines
    /// (for error reporting, e.g.)
    line_offsets: Arc<[usize]>,
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

/// An error that occurs when trying to read a file from disk.
pub struct SourceLoadError {
    pub(crate) cause: std::io::Error,
    pub(crate) path: PathBuf,
}

impl FileId {
    /// A reserved FileId used during parsing.
    pub(crate) const CURRENT_FILE: FileId = FileId(unsafe { NonZeroU32::new_unchecked(1) });

    pub(crate) fn next() -> FileId {
        use std::sync::atomic;
        static COUNTER: atomic::AtomicU32 = atomic::AtomicU32::new(2);
        FileId(NonZeroU32::new(COUNTER.fetch_add(1, atomic::Ordering::Relaxed)).unwrap())
    }
}

impl Source {
    /// Attempts to generate a `Source` from the contents of the provided path.
    pub fn from_path(path: impl Into<PathBuf>) -> Result<Self, SourceLoadError> {
        let path = path.into();
        let contents = std::fs::read_to_string(&path).map_err(|cause| SourceLoadError {
            path: path.clone(),
            cause,
        })?;
        let line_offsets = line_offsets(&contents);
        Ok(Source {
            path: Some(path),
            id: FileId::next(),
            contents: contents.into(),
            line_offsets,
        })
    }

    /// Create a source from a string.
    ///
    /// This is useful for things like testing.
    pub fn from_text(contents: impl Into<Arc<str>>) -> Source {
        let contents = contents.into();
        let line_offsets = line_offsets(&contents);
        Source {
            id: FileId::next(),
            contents,
            path: None,
            line_offsets,
        }
    }

    /// The raw text for this source
    pub fn text(&self) -> &str {
        &self.contents
    }

    /// The path of the underlying file, if one exists
    pub fn path(&self) -> Option<&Path> {
        self.path.as_deref()
    }

    /// The [`FileId`] for this source.
    pub fn id(&self) -> FileId {
        self.id
    }

    /// Compute the line and column for a given utf-8 offset.
    pub fn line_col_for_offset(&self, offset: usize) -> (usize, usize) {
        let offset_idx = match self.line_offsets.binary_search(&offset) {
            Ok(x) => x,
            Err(x) => x - 1, // cannot underflow as 0 is always in list
        };
        let offset_of_line = self.line_offsets[offset_idx];
        let offset_in_line = offset - offset_of_line;
        (offset_idx + 1, offset_in_line)
    }

    /// returns the (1-indexed) number and text.
    pub fn line_containing_offset(&self, offset: usize) -> (usize, &str) {
        let offset_idx = match self.line_offsets.binary_search(&offset) {
            Ok(x) => x,
            Err(x) => x - 1, // cannot underflow as 0 is always in list
        };
        let start_offset = self.line_offsets[offset_idx];
        let end_offset = self
            .line_offsets
            .get(offset_idx + 1)
            .copied()
            .unwrap_or(self.contents.len());

        (
            offset_idx + 1,
            self.contents[start_offset..end_offset].trim_end_matches('\n'),
        )
    }

    /// Return the offset of the start of the (1-indexed) line.
    ///
    /// Panics if the line number exceeds the total number of lines in the file.
    pub fn offset_for_line_number(&self, line_number: usize) -> usize {
        self.line_offsets[line_number - 1]
    }
}

fn line_offsets(text: &str) -> Arc<[usize]> {
    // we could use memchar for this; benefits would require benchmarking
    let mut result = vec![0];
    result.extend(
        text.bytes()
            .enumerate()
            .filter_map(|(i, b)| if b == b'\n' { Some(i + 1) } else { None }),
    );
    result.into()
}

impl SourceMap {
    pub(crate) fn add_entry(&mut self, src: Range<usize>, dest: (FileId, usize)) {
        if !src.is_empty() {
            self.offsets.push((src, dest));
        }
    }

    /// panics if `global_range` crosses a file barrier?
    pub(crate) fn resolve_range(&self, global_range: Range<usize>) -> (FileId, Range<usize>) {
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

impl SourceList {
    pub(crate) fn new(root: Source, project_root: Option<PathBuf>) -> Self {
        // if root has no source it means we're testing, and project_root can
        // be empty? (future me: if this caused a bug, I'm sorry.
        // it looked harmless.)
        let project_root = project_root.unwrap_or_else(|| match root.path() {
            // if path is real, it should exist and have a parent
            Some(path) => path.parent().unwrap().to_owned(),
            None => PathBuf::new(),
        });
        let root_id = root.id;
        let canonical = match root.path() {
            Some(path) => util::paths::CanonicalPath::from_path(path).unwrap(),
            None => util::paths::CanonicalPath::fake(format!("path/to/{}.fea", root_id.0)),
        };

        let mut myself = SourceList {
            project_root,
            root_id,
            ids: Default::default(),
            sources: Default::default(),
        };

        myself.ids.insert(canonical, root.id);
        myself.sources.insert(root.id, root);
        myself
    }

    pub(crate) fn project_root(&self) -> &Path {
        &self.project_root
    }

    pub(crate) fn root_id(&self) -> FileId {
        self.root_id
    }

    #[cfg(test)]
    pub(crate) fn add_source(&mut self, source: Source) {
        self.sources.insert(source.id, source);
    }

    pub(crate) fn get(&self, id: &FileId) -> Option<&Source> {
        self.sources.get(id)
    }

    /// Attempt to load the source at `path`.
    ///
    /// If an error occurs whe loading the file, returns an Error.
    ///
    /// `path` should have already been normalized with `util::paths::resolve_path`.
    pub(crate) fn source_for_path(&mut self, path: PathBuf) -> Result<FileId, SourceLoadError> {
        let canonical =
            util::paths::CanonicalPath::from_path(&path).map_err(|cause| SourceLoadError {
                cause,
                path: path.clone(),
            })?;
        if let Some(src) = self.ids.get(&canonical) {
            return Ok(*src);
        }

        let source = Source::from_path(path)?;
        let id = source.id;
        self.ids.insert(canonical, id);
        self.sources.insert(id, source);
        Ok(id)
    }
}
