//! The canonical fontc version string, shared by the `name` table (see
//! `name.rs`) and `fontc --version`/`--vv`, which must agree.
//! See <https://github.com/googlefonts/fontc/issues/2048>.
//!
//! It's `git describe` rendered as SemVer: a build past a release tag is a dev
//! pre-release of the *next* patch, e.g. `0.6.1-dev.394+gd62ba016.dirty` (394
//! commits past `fontc-v0.6.0`, dirty tree); on the tag itself, just `0.6.0`.
//! Past a pre-release tag like `fontc-v1.0.0-rc.1` the pre-release identifiers
//! extend instead (`1.0.0-rc.1.dev.394+g...`), sorting after the tag and
//! before the final it precedes.
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
        // Past a plain release tag, a dev build is a pre-release of the *next*
        // patch (`0.6.0` -> `0.6.1-dev.N`): a `-dev` suffix on the tag's own
        // version would sort below the release it follows. Past a pre-release
        // tag, the identifiers extend instead (`1.0.0-rc.1` -> `1.0.0-rc.1.dev.N`),
        // which SemVer §11 orders after the tag and before the final; a patch
        // bump there would wrongly sort above the unreleased final.
        let base = if tag.contains('-') {
            format!("{tag}.dev")
        } else {
            format!("{}-dev", bump_patch(tag))
        };
        let mut version = format!("{base}.{distance}+g{sha}");
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
/// Only plain `MAJOR.MINOR.PATCH` tags reach this: `version_string` routes
/// pre-release tags to the identifier-extension path instead. Should one slip
/// through anyway, its pre-release part is dropped before bumping (defensive;
/// pinned by the `patch_bump` test).
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
    fn past_a_prerelease_tag() {
        // Past a pre-release tag the identifiers extend (`rc.1` -> `rc.1.dev.5`),
        // sorting after the tag and before the final. A patch bump here would
        // produce `1.0.1-dev.5`, wrongly sorting above the unreleased `1.0.0`.
        assert_eq!(
            version_string("fontc-v1.0.0-rc.1-5-gabc1234", "1.0.0-rc.1"),
            "1.0.0-rc.1.dev.5+gabc1234"
        );
        assert_eq!(
            version_string("fontc-v1.0.0-rc.1-5-gabc1234-dirty", "1.0.0-rc.1"),
            "1.0.0-rc.1.dev.5+gabc1234.dirty"
        );
    }

    #[test]
    fn on_a_prerelease_tag() {
        assert_eq!(
            version_string("fontc-v1.0.0-rc.1", "1.0.0-rc.1"),
            "1.0.0-rc.1"
        );
        assert_eq!(
            version_string("fontc-v1.0.0-rc.1-dirty", "1.0.0-rc.1"),
            "1.0.0-rc.1+dirty"
        );
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
        "0.6.1-dev.394+gd62ba016",         // past a release tag
        "0.6.1-dev.394+gd62ba016.dirty",   // ...from a dirty tree
        "0.6.0",                           // exactly on a release tag
        "0.6.0+dirty",                     // ...from a dirty tree
        "1.0.0-rc.1.dev.5+gabc1234",       // past a pre-release tag
        "1.0.0-rc.1.dev.5+gabc1234.dirty", // ...from a dirty tree
        "1.0.0-rc.1",                      // exactly on a pre-release tag
        "1.0.0-rc.1+dirty",                // ...from a dirty tree
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

        // A dev build past a pre-release tag extends its identifiers, so it
        // sits above that pre-release and below both the next pre-release and
        // the final (SemVer §11: with an equal prefix, the larger set of
        // pre-release fields has higher precedence).
        let rc_dev = v("1.0.0-rc.1.dev.5+gabc1234");
        assert_eq!(v("1.0.0-rc.1").cmp_precedence(&rc_dev), Ordering::Less);
        assert_eq!(rc_dev.cmp_precedence(&v("1.0.0-rc.2")), Ordering::Less);
        assert_eq!(rc_dev.cmp_precedence(&v("1.0.0")), Ordering::Less);
    }

    #[test]
    fn patch_bump() {
        assert_eq!(bump_patch("0.6.0"), "0.6.1");
        assert_eq!(bump_patch("0.6.9"), "0.6.10");
        assert_eq!(bump_patch("1.0.0"), "1.0.1");
        assert_eq!(bump_patch("0.6.0-rc.1"), "0.6.1");
    }
}
