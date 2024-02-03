//! Loads fontra source and converts it to fontc IR

mod fontra;
pub mod source;
mod toir;

#[cfg(test)]
mod test {
    use std::path::{Path, PathBuf};

    pub(crate) fn testdata_dir() -> PathBuf {
        let dir = Path::new("../resources/testdata/fontra");
        assert!(dir.is_dir());
        dir.to_path_buf()
    }
}
