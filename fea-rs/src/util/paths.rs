//! working with paths :(

use std::path::{Path, PathBuf};

/// Attempt to normalize a path relative to a base path.
///
/// This cannot always work perfectly. The main goal is just to avoid the situation
/// where you have duplicate paths of the form 'font/includes/features.fea' and
/// 'font/includes/../../font/includes/features.fea'.
fn rebase_path(path: &Path, base: &Path) -> PathBuf {
    use std::path::Component;

    let mut components: Vec<_> = base.components().collect();

    // This is disabled for tests so that we can test it using non-existent
    // paths, which trigger the is_dir assert.
    #[cfg(not(test))]
    {
        assert!(components.is_empty() || base.is_dir());
        assert!(path.is_relative());
    }

    if components.is_empty() {
        return path.to_path_buf();
    }

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

/// Given a relative path, resolve it to a specific path per [the spec][].
///
/// The second argument is the root of the project, and the third argument is the
/// path to the *including* file, if one exists.
///
/// [the spec]: http://adobe-type-tools.github.io/afdko/OpenTypeFeatureFileSpecification.html#3-including-files
pub(crate) fn resolve_path(path: &Path, root: &Path, parent: Option<&Path>) -> PathBuf {
    if path.is_absolute() {
        log::info!("path {} is absolute", path.display());
        return path.to_path_buf();
    }

    if root.join(path).exists() {
        return rebase_path(path, root);
    }
    if let Some(parent) = parent {
        if parent.join(path).exists() {
            return rebase_path(path, parent);
        }
    }
    path.to_owned()
}

#[cfg(test)]
mod tests {
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

        let empty_base = Path::new("");
        assert!(!empty_base.is_dir());
        let path = Path::new("font/includes/features.fea");
        assert_eq!(
            rebase_path(path, empty_base),
            Path::new("font/includes/features.fea")
        );
    }
}
