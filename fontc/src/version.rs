//! The canonical fontc version string, shared by the `name` table (see
//! `name.rs`) and `fontc --version`/`--vv`, which must agree.
//! See <https://github.com/googlefonts/fontc/issues/2048>.
//!
//! It's `git describe` rendered as SemVer: a build past a release tag is a dev
//! pre-release of the *next* patch, e.g. `0.6.1-dev.394+gd62ba016.dirty` (394
//! commits past `fontc-v0.6.0`, dirty tree); on the tag itself, just `0.6.0`.
//!
//! Two details are deliberate, both for ordering (see the sort test below):
//! bumping to `0.6.1` rather than `0.6.0-dev.N`, since a pre-release sorts
//! *below* its own version, so the bump keeps a dev build after the release it
//! follows; and the `.` in `dev.394`, which makes the commit count a numeric
//! identifier so it orders numerically (`dev.9 < dev.394`) instead of as text.

/// Map a `git describe --tags --dirty --match fontc-v*` string
/// (`fontc-v<tag>[-<distance>-g<sha>][-dirty]`) to the SemVer version.
///
/// `crate_version` (`CARGO_PKG_VERSION`) is the fallback when git is unavailable
/// (e.g. a published crate, source tarball, or shallow clone) where `describe`
/// is empty or vergen's `VERGEN_IDEMPOTENT_OUTPUT` sentinel. Both are passed in
/// rather than read here so the tests can drive this with literal inputs; the
/// `version()` wrapper supplies the real env values.
pub(crate) fn version_string(describe: &str, crate_version: &str) -> String {
    let Some(rest) = describe.strip_prefix("fontc-v") else {
        return crate_version.to_string();
    };
    let (rest, dirty) = match rest.strip_suffix("-dirty") {
        Some(clean) => (clean, true),
        None => (rest, false),
    };
    // Past a tag, describe is "<tag>-<distance>-g<sha>".
    if let Some((tag_and_distance, sha)) = rest.rsplit_once("-g")
        && let Some((tag, distance)) = tag_and_distance.rsplit_once('-')
        && !distance.is_empty()
        && distance.bytes().all(|b| b.is_ascii_digit())
    {
        let mut version = format!("{}-dev.{distance}+g{sha}", bump_patch(tag));
        if dirty {
            version.push_str(".dirty");
        }
        return version;
    }
    // Exactly on a tag: git omits the distance and sha, leaving just "<tag>".
    if dirty {
        format!("{rest}+dirty")
    } else {
        rest.to_string()
    }
}

/// The base of the candidate next release after `tag` (the `X.Y.Z` in `X.Y.Z-dev.N`):
/// bumps the patch (`0.6.0` -> `0.6.1`), or returns `tag` unchanged if it isn't
/// `MAJOR.MINOR.PATCH`.
///
/// Assumes plain `MAJOR.MINOR.PATCH` release tags, the only kind fontc cuts
/// today. A pre-release tag like `0.6.0-rc.1` would wrongly become `0.6.1`,
/// sorting *above* the eventual `0.6.0` -- that path is currently unreachable,
/// and should be handled if/when fontc starts tagging pre-releases.
fn bump_patch(tag: &str) -> String {
    let release = tag.split('-').next().unwrap_or(tag);
    let parts: Vec<&str> = release.split('.').collect();
    if let Some(patch) = parts.last().and_then(|p| p.parse::<u64>().ok()) {
        let prefix = &parts[..parts.len() - 1];
        if prefix.is_empty() {
            return (patch + 1).to_string();
        }
        return format!("{}.{}", prefix.join("."), patch + 1);
    }
    tag.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn past_a_tag() {
        assert_eq!(
            version_string("fontc-v0.6.0-394-gd62ba016", "0.6.0"),
            "0.6.1-dev.394+gd62ba016"
        );
        assert_eq!(
            version_string("fontc-v0.6.0-394-gd62ba016-dirty", "0.6.0"),
            "0.6.1-dev.394+gd62ba016.dirty"
        );
    }

    #[test]
    fn on_a_release_tag() {
        assert_eq!(version_string("fontc-v0.6.0", "0.6.0"), "0.6.0");
        assert_eq!(version_string("fontc-v0.6.0-dirty", "0.6.0"), "0.6.0+dirty");
    }

    #[test]
    fn without_git() {
        // `cargo install`, source tarball, shallow clone with no reachable tag.
        // The describe is empty or a vergen sentinel; fall back to crate version.
        for describe in ["", "VERGEN_IDEMPOTENT_OUTPUT"] {
            let v = version_string(describe, "0.6.0");
            assert_eq!(v, "0.6.0");
            assert!(!v.contains("VERGEN"), "{v:?}");
        }
    }

    // The exact strings we emit, across the lifecycle. The tests above pin that
    // `version_string` actually produces these; the tests below validate the
    // strings *themselves* against the SemVer crate -- an independent oracle --
    // so we're checking our own format, not git's.
    const EMITTED: &[&str] = &[
        "0.6.1-dev.394+gd62ba016",       // past a release tag
        "0.6.1-dev.394+gd62ba016.dirty", // ...from a dirty tree
        "0.6.0",                         // exactly on a release tag
        "0.6.0+dirty",                   // ...from a dirty tree
    ];

    /// Each string we emit parses under the SemVer 2.0.0 grammar and round-trips.
    #[test]
    fn emitted_versions_are_valid_semver() {
        for s in EMITTED {
            let v = semver::Version::parse(s)
                .unwrap_or_else(|e| panic!("{s:?} is not valid semver: {e}"));
            assert_eq!(&v.to_string(), s, "did not round-trip");
        }

        // The dev form decomposes into the SemVer fields we documented:
        // 0.6.1 release + "dev.<distance>" pre-release + "g<sha>.dirty" build.
        let v = semver::Version::parse("0.6.1-dev.394+gd62ba016.dirty").unwrap();
        assert_eq!((v.major, v.minor, v.patch), (0, 6, 1));
        assert_eq!(v.pre.as_str(), "dev.394");
        assert_eq!(v.build.as_str(), "gd62ba016.dirty");
    }

    /// Our strings sort the way the format intends, per SemVer precedence
    /// (`cmp_precedence` is the spec compare, which ignores build metadata).
    #[test]
    fn emitted_versions_sort_per_semver() {
        use std::cmp::Ordering;
        let v = |s: &str| semver::Version::parse(s).unwrap();
        let snapshot = v("0.6.1-dev.394+gd62ba016");

        // A dev build sits above its base release and below any future release
        // (SemVer §11: a pre-release has lower precedence than the normal version).
        assert_eq!(v("0.6.0").cmp_precedence(&snapshot), Ordering::Less);
        assert_eq!(snapshot.cmp_precedence(&v("0.6.1")), Ordering::Less);
        assert_eq!(snapshot.cmp_precedence(&v("0.7.0")), Ordering::Less);

        // The dot makes the commit distance a *numeric* identifier, so it orders
        // numerically rather than as text: dev.9 < dev.394 < dev.1000.
        assert_eq!(
            v("0.6.1-dev.9+gd62ba016").cmp_precedence(&snapshot),
            Ordering::Less
        );
        assert_eq!(
            snapshot.cmp_precedence(&v("0.6.1-dev.1000+gd62ba016")),
            Ordering::Less
        );

        // Build metadata (sha, dirty) does not affect precedence (SemVer §10).
        assert_eq!(
            snapshot.cmp_precedence(&v("0.6.1-dev.394+gd62ba016.dirty")),
            Ordering::Equal
        );
    }

    #[test]
    fn patch_bump() {
        assert_eq!(bump_patch("0.6.0"), "0.6.1");
        assert_eq!(bump_patch("0.6.9"), "0.6.10");
        assert_eq!(bump_patch("1.0.0"), "1.0.1");
        assert_eq!(bump_patch("0.6.0-rc.1"), "0.6.1");
    }
}
