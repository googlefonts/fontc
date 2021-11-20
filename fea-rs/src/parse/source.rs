//! source files

use std::{
    collections::HashMap,
    num::NonZeroU32,
    ops::Range,
    path::{Path, PathBuf},
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

pub(crate) struct SourceLoadError {
    pub(crate) cause: std::io::Error,
    pub(crate) path: PathBuf,
}

impl FileId {
    pub fn next() -> FileId {
        use std::sync::atomic;
        static COUNTER: atomic::AtomicU32 = atomic::AtomicU32::new(1);
        FileId(NonZeroU32::new(COUNTER.fetch_add(1, atomic::Ordering::Relaxed)).unwrap())
    }
}

impl Source {
    pub(crate) fn new(path: impl Into<PathBuf>) -> Result<Self, SourceLoadError> {
        let path = path.into();
        let contents = std::fs::read_to_string(&path).map_err(|cause| SourceLoadError {
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

    pub fn contents(&self) -> &str {
        &self.contents
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn id(&self) -> FileId {
        self.id
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

    //pub(crate) fn reverse_resolve(&self, file_id)
}

impl SourceList {
    pub(crate) fn new(
        project_root: Option<PathBuf>,
        root_fea: PathBuf,
    ) -> Result<Self, SourceLoadError> {
        assert!(root_fea.exists());
        assert!(root_fea.is_file());
        let project_root = project_root.unwrap_or_else(|| root_fea.parent().unwrap().to_owned());
        let canonical =
            util::paths::CanonicalPath::new(&root_fea).map_err(|cause| SourceLoadError {
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

    pub(crate) fn project_root(&self) -> &Path {
        &self.project_root
    }

    #[cfg(test)]
    pub(crate) fn new_for_test(root_id: FileId) -> Self {
        SourceList {
            root_id,
            project_root: Default::default(),
            ids: Default::default(),
            sources: Default::default(),
        }
    }

    pub(crate) fn root_id(&self) -> FileId {
        self.root_id
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
            util::paths::CanonicalPath::new(&path).map_err(|cause| SourceLoadError {
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
