//! working with paths :(

use std::path::{Path, PathBuf};

/// Attempt to normalize a path relative to a base path.
///
/// This cannot always work perfectly. The main goal is just to avoid the situation
/// where you have duplicate paths of the form 'font/includes/features.fea' and
/// 'font/includes/../../font/includes/features.fea'.
pub fn rebase_path(path: &Path, base: &Path) -> PathBuf {
    use std::path::Component;
    #[cfg(not(test))]
    {
        assert!(base.is_dir());
        assert!(path.is_relative());
    }
    let mut components: Vec<_> = base.components().collect();
    for component in path.components() {
        match component {
            Component::CurDir => (),
            Component::ParentDir => {
                match components.pop() {
                    Some(Component::ParentDir) => {
                        // put back the one we popped and add another
                        components.push(Component::ParentDir);
                        components.push(Component::ParentDir);
                    }
                    // just pop
                    Some(Component::Normal(_)) => (),
                    // push a pardir
                    Some(Component::CurDir) | None => components.push(Component::ParentDir),
                    // no absolute paths make it this far
                    _ => unreachable!(),
                }
            }
            Component::Normal(_) => components.push(component),
            Component::RootDir | Component::Prefix(_) => unreachable!(),
        }
    }

    components.into_iter().fold(PathBuf::new(), |mut acc, el| {
        acc.push(el);
        acc
    })
}

#[cfg(test)]
mod testz {
    use super::*;
    #[test]
    fn test_rebase_path() {
        let base = Path::new("../../");
        let path = Path::new("../../hi.fea");
        assert_eq!(rebase_path(path, base), Path::new("../../../../hi.fea"));

        // should resolve to the same location
        let path2 = Path::new("./../../hi.fea");
        assert_eq!(rebase_path(path2, base), Path::new("../../../../hi.fea"));

        // folder/
        //   includes/
        //   font/
        //     other.fea

        let base = Path::new("../includes/");
        let path = Path::new("../font/other.fea");
        assert_eq!(rebase_path(path, base), Path::new("../font/other.fea"));

        let base = Path::new("font/includes");
        let path = Path::new("../../font/includes/features.fea");
        assert_eq!(
            rebase_path(path, base),
            Path::new("font/includes/features.fea")
        );
    }
}
