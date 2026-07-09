//! Configuration for the `com.github.googlei18n.ufo2ft.featureWriters` lib key.
//!
//! ufo2ft lets sources override which automatic feature writers run (kern, mark,
//! curs, GDEF) and in what mode. fontc plays the combined fontmake+ufo2ft role, so
//! it reads the same key and mirrors ufo2ft's semantics. The frontends translate
//! their own plist types into the plist-agnostic [`FeatureWriterOptionValue`] and
//! feed each entry to [`validate_feature_writer`], which holds the shared
//! string-matching and error policy.

use log::warn;
use serde::{Deserialize, Serialize};
use write_fonts::types::Tag;

use crate::error::Error;

/// The lib/userData key carrying the feature-writer configuration.
pub const FEATURE_WRITERS_LIB_KEY: &str = "com.github.googlei18n.ufo2ft.featureWriters";

const DEFAULT_MODULE: &str = "ufo2ft.featureWriters";
/// The legacy kern writer; ufo2ft honours it but fontc can't match its
/// direction-based lookup grouping, so it's a hard error (see [`validate_feature_writer`]).
const KERN2_MODULE: &str = "ufo2ft.featureWriters.kernFeatureWriter2";
/// glyphsLib's mark writer module. Its `ContextualMarkFeatureWriter` has been an
/// empty compatibility subclass of ufo2ft's `MarkFeatureWriter` since glyphsLib 6.10
/// (contextual-anchor handling moved into ufo2ft's regular writer), so entries naming
/// it map to [`KnownFeatureWriter::Mark`]. Sources converted by older glyphsLib still
/// carry this spelling (e.g. KumbhSans).
const GLYPHSLIB_MARK_MODULE: &str = "glyphsLib.featureWriters.markFeatureWriter";

const CURS: Tag = Tag::new(b"curs");
const KERN: Tag = Tag::new(b"kern");
const DIST: Tag = Tag::new(b"dist");
const MARK: Tag = Tag::new(b"mark");
const MKMK: Tag = Tag::new(b"mkmk");
const ABVM: Tag = Tag::new(b"abvm");
const BLWM: Tag = Tag::new(b"blwm");

/// Whether a writer respects existing manual feature blocks or is appended after them.
#[derive(Serialize, Deserialize, Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum FeatureWriterMode {
    /// Skip generating a tag when the source has a manual block for it without an
    /// insertion marker; this is fontc's historical behavior.
    #[default]
    Skip,
    /// Ignore insertion markers; generated lookups land after all user lookups.
    Append,
}

/// The feature writers fontc knows how to emulate.
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
pub enum KnownFeatureWriter {
    Curs,
    Kern,
    Mark,
    Gdef,
}

impl KnownFeatureWriter {
    /// The feature tags a writer generates. Empty for `Gdef`, which writes a table.
    fn tags(self) -> &'static [Tag] {
        match self {
            KnownFeatureWriter::Curs => &[CURS],
            KnownFeatureWriter::Kern => &[KERN, DIST],
            KnownFeatureWriter::Mark => &[MARK, MKMK, ABVM, BLWM],
            KnownFeatureWriter::Gdef => &[],
        }
    }
}

/// A single validated entry from the `featureWriters` list.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FeatureWriterSpec {
    pub writer: KnownFeatureWriter,
    pub mode: FeatureWriterMode,
    /// The `features` option: restrict generation to this subset of the writer's
    /// tags. `None` means all of the writer's tags.
    pub features: Option<Vec<Tag>>,
}

/// A plist-agnostic option value.
///
/// Each frontend maps its own plist scalar types onto this so that
/// [`validate_feature_writer`] can be shared. Anything we don't recognise (a
/// nested dict, a non-string array element, ...) becomes [`Other`], which the
/// validator treats as a malformed option and drops.
///
/// [`Other`]: FeatureWriterOptionValue::Other
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FeatureWriterOptionValue {
    Boolean(bool),
    Integer(i64),
    String(String),
    /// An array of strings (used by the `features` option).
    Array(Vec<String>),
    /// A value whose type we don't handle.
    Other,
}

impl FeatureWriterOptionValue {
    fn as_str(&self) -> Option<&str> {
        match self {
            FeatureWriterOptionValue::String(s) => Some(s),
            _ => None,
        }
    }
}

/// Validate one `featureWriters` list entry against the shared error policy.
///
/// A built-in writer is recognised under every spelling ufo2ft can import: the
/// bare class name (resolved in the default `ufo2ft.featureWriters` package),
/// the class qualified by its defining submodule, and glyphsLib's legacy
/// `ContextualMarkFeatureWriter` alias (see [`GLYPHSLIB_MARK_MODULE`]).
///
/// Mirrors ufo2ft's `loadFeatureWriters(..., ignoreErrors=True)`:
/// - malformed or unrecognised entries (unknown module/class combination, unknown
///   option key, wrong option type, invalid mode, invalid `features` subset) log a
///   warning and are dropped (`Ok(None)`), matching fontmake's silent feature loss;
/// - a recognised writer carrying a recognised option that ufo2ft would honour
///   but fontc can't match (`ignoreMarks=false`, `quantization != 1`,
///   `groupMarkClasses=true`, a valid Gdef `features` subset), and the legacy
///   `kernFeatureWriter2`, are hard errors -- refusing beats silent divergence.
///
/// Drop wins over the hard error: ufo2ft binds all constructor kwargs at once, so
/// a malformation (e.g. an unknown option) drops the whole entry before any
/// honoured option takes effect. We therefore scan the entire entry and only
/// raise a deferred hard error if nothing else dropped it.
pub fn validate_feature_writer(
    class: &str,
    module: Option<&str>,
    options: &[(String, FeatureWriterOptionValue)],
) -> Result<Option<FeatureWriterSpec>, Error> {
    let module = module.unwrap_or(DEFAULT_MODULE);
    // Recognised-but-unimplemented; deferred so a malformed option still drops the
    // entry the way fontmake would (rather than us failing a build it completes).
    let mut hard_error: Option<String> = None;
    let writer = if module == KERN2_MODULE && class == "KernFeatureWriter" {
        hard_error = Some("legacy kernFeatureWriter2 is not supported".into());
        // Recognise the kern option names while scanning.
        KnownFeatureWriter::Kern
    } else {
        // Accept the (module, class) pairs ufo2ft can import: each built-in class
        // is re-exported from the ufo2ft.featureWriters package and defined in its
        // own submodule. Anything else fails ufo2ft's import and is dropped.
        match (module, class) {
            (DEFAULT_MODULE | "ufo2ft.featureWriters.cursFeatureWriter", "CursFeatureWriter") => {
                KnownFeatureWriter::Curs
            }
            (DEFAULT_MODULE | "ufo2ft.featureWriters.kernFeatureWriter", "KernFeatureWriter") => {
                KnownFeatureWriter::Kern
            }
            (DEFAULT_MODULE | "ufo2ft.featureWriters.markFeatureWriter", "MarkFeatureWriter")
            | (GLYPHSLIB_MARK_MODULE, "MarkFeatureWriter" | "ContextualMarkFeatureWriter") => {
                KnownFeatureWriter::Mark
            }
            (DEFAULT_MODULE | "ufo2ft.featureWriters.gdefFeatureWriter", "GdefFeatureWriter") => {
                KnownFeatureWriter::Gdef
            }
            _ => {
                warn!("featureWriters: unknown writer '{module}.{class}'; dropping entry");
                return Ok(None);
            }
        }
    };

    let mut mode = FeatureWriterMode::Skip;
    let mut features = None;
    for (key, value) in options {
        match key.as_str() {
            "mode" => {
                let Some(parsed) = value.as_str().and_then(parse_mode) else {
                    warn!("featureWriters: invalid mode {value:?}; dropping entry");
                    return Ok(None);
                };
                mode = parsed;
            }
            "features" => {
                if writer == KnownFeatureWriter::Gdef {
                    // ufo2ft's GdefFeatureWriter takes a GlyphClassDefs/LigatureCarets
                    // subset, which fontc doesn't implement: honouring the entry
                    // without the subset would silently emit a different GDEF, and
                    // dropping it would silently emit none. A valid subset is a hard
                    // error; anything else fails ufo2ft's own validation and drops.
                    if is_valid_gdef_features(value) {
                        hard_error = Some(format!("GdefFeatureWriter features={value:?}"));
                        continue;
                    }
                    warn!("featureWriters: invalid features {value:?}; dropping entry");
                    return Ok(None);
                }
                let Some(parsed) = parse_features(writer, value) else {
                    warn!("featureWriters: invalid features {value:?}; dropping entry");
                    return Ok(None);
                };
                features = Some(parsed);
            }
            "ignoreMarks" if writer == KnownFeatureWriter::Kern => match value {
                FeatureWriterOptionValue::Boolean(true) => (),
                FeatureWriterOptionValue::Boolean(false) => {
                    hard_error = Some("KernFeatureWriter ignoreMarks=false".into());
                }
                _ => {
                    warn!("featureWriters: invalid ignoreMarks {value:?}; dropping entry");
                    return Ok(None);
                }
            },
            "quantization"
                if writer == KnownFeatureWriter::Kern || writer == KnownFeatureWriter::Mark =>
            {
                match value {
                    FeatureWriterOptionValue::Integer(1) => (),
                    FeatureWriterOptionValue::Integer(other) => {
                        hard_error = Some(format!("{class} quantization={other}"));
                    }
                    _ => {
                        warn!("featureWriters: invalid quantization {value:?}; dropping entry");
                        return Ok(None);
                    }
                }
            }
            "groupMarkClasses" if writer == KnownFeatureWriter::Mark => match value {
                FeatureWriterOptionValue::Boolean(false) => (),
                FeatureWriterOptionValue::Boolean(true) => {
                    hard_error = Some("MarkFeatureWriter groupMarkClasses=true".into());
                }
                _ => {
                    warn!("featureWriters: invalid groupMarkClasses {value:?}; dropping entry");
                    return Ok(None);
                }
            },
            other => {
                warn!("featureWriters: unknown option '{other}' on {class}; dropping entry");
                return Ok(None);
            }
        }
    }

    if let Some(reason) = hard_error {
        return Err(Error::UnsupportedFeatureWriters(reason));
    }
    Ok(Some(FeatureWriterSpec {
        writer,
        mode,
        features,
    }))
}

fn parse_mode(raw: &str) -> Option<FeatureWriterMode> {
    match raw {
        "skip" => Some(FeatureWriterMode::Skip),
        "append" => Some(FeatureWriterMode::Append),
        _ => None,
    }
}

/// The values ufo2ft's `GdefFeatureWriter` accepts for its `features` option.
///
/// Unlike the other writers these are not feature tags: they select which parts
/// of the GDEF table to generate.
const GDEF_WRITER_FEATURES: [&str; 2] = ["GlyphClassDefs", "LigatureCarets"];

/// Whether a Gdef `features` value would pass ufo2ft's own validation
/// (a non-empty subset of [`GDEF_WRITER_FEATURES`]).
fn is_valid_gdef_features(value: &FeatureWriterOptionValue) -> bool {
    let FeatureWriterOptionValue::Array(raw) = value else {
        return false;
    };
    !raw.is_empty()
        && raw
            .iter()
            .all(|v| GDEF_WRITER_FEATURES.contains(&v.as_str()))
}

/// Parse the `features` option: a non-empty subset of the writer's tags.
fn parse_features(
    writer: KnownFeatureWriter,
    value: &FeatureWriterOptionValue,
) -> Option<Vec<Tag>> {
    let FeatureWriterOptionValue::Array(raw) = value else {
        return None;
    };
    if raw.is_empty() {
        return None;
    }
    let allowed = writer.tags();
    let mut tags = Vec::with_capacity(raw.len());
    for tag in raw {
        let tag = Tag::new_checked(tag.as_bytes()).ok()?;
        if !allowed.contains(&tag) {
            return None;
        }
        tags.push(tag);
    }
    Some(tags)
}

/// Per-writer settings in the resolved [`FeatureWriterPlan`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FeatureWriterSettings {
    pub mode: FeatureWriterMode,
    /// Restrict generation to this subset of the writer's tags; `None` = all.
    pub features: Option<Vec<Tag>>,
}

/// The `featureWriters` config resolved into a per-writer plan.
///
/// `None` for a writer means it is disabled and its features must not
/// be generated. Note ufo2ft's Mark writer covers mark/mkmk/abvm/blwm while Curs
/// is a separate writer, so `curs` is gated independently.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FeatureWriterPlan {
    pub curs: Option<FeatureWriterSettings>,
    pub kern: Option<FeatureWriterSettings>,
    pub mark: Option<FeatureWriterSettings>,
    pub gdef: bool,
}

impl FeatureWriterSpec {
    fn settings(&self) -> FeatureWriterSettings {
        FeatureWriterSettings {
            mode: self.mode,
            features: self.features.clone(),
        }
    }
}

/// Resolve the `feature_writers` config into a per-writer plan.
///
/// `None` (key absent) enables everything in [`Skip`] mode -- the compiler's
/// historical behavior. `Some(list)` fully replaces the defaults: only listed
/// writers are enabled; an empty list disables all automatic features.
///
/// [`Skip`]: FeatureWriterMode::Skip
pub fn resolve_feature_writers(specs: &Option<Vec<FeatureWriterSpec>>) -> FeatureWriterPlan {
    let Some(specs) = specs else {
        let enabled = FeatureWriterSettings {
            mode: FeatureWriterMode::Skip,
            features: None,
        };
        return FeatureWriterPlan {
            curs: Some(enabled.clone()),
            kern: Some(enabled.clone()),
            mark: Some(enabled),
            gdef: true,
        };
    };
    let mut plan = FeatureWriterPlan {
        curs: None,
        kern: None,
        mark: None,
        gdef: false,
    };
    // Duplicate writers are rejected at parse time ([`reject_duplicate_writers`]),
    // so each slot is assigned at most once.
    for spec in specs {
        match spec.writer {
            KnownFeatureWriter::Curs => plan.curs = Some(spec.settings()),
            KnownFeatureWriter::Kern => plan.kern = Some(spec.settings()),
            KnownFeatureWriter::Mark => plan.mark = Some(spec.settings()),
            KnownFeatureWriter::Gdef => plan.gdef = true,
        }
    }
    plan
}

/// Reject a config that lists the same writer more than once.
///
/// ufo2ft instantiates and runs every entry in order, so repeated writers can be
/// meaningful there (e.g. two Mark writers with disjoint `features` subsets);
/// fontc resolves one settings slot per writer and would silently keep only the
/// last. Refusing beats silently changing what the config means.
pub fn reject_duplicate_writers(specs: &[FeatureWriterSpec]) -> Result<(), Error> {
    let mut seen = Vec::with_capacity(specs.len());
    for spec in specs {
        if seen.contains(&spec.writer) {
            return Err(Error::UnsupportedFeatureWriters(format!(
                "featureWriters lists the {:?} writer more than once",
                spec.writer
            )));
        }
        seen.push(spec.writer);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn opt(key: &str, value: FeatureWriterOptionValue) -> (String, FeatureWriterOptionValue) {
        (key.to_string(), value)
    }

    #[test]
    fn recognised_writer_default_options() {
        let spec = validate_feature_writer("KernFeatureWriter", None, &[])
            .unwrap()
            .unwrap();
        assert_eq!(spec.writer, KnownFeatureWriter::Kern);
        assert_eq!(spec.mode, FeatureWriterMode::Skip);
        assert_eq!(spec.features, None);
    }

    #[test]
    fn append_mode() {
        let spec = validate_feature_writer(
            "CursFeatureWriter",
            Some(DEFAULT_MODULE),
            &[opt(
                "mode",
                FeatureWriterOptionValue::String("append".into()),
            )],
        )
        .unwrap()
        .unwrap();
        assert_eq!(spec.mode, FeatureWriterMode::Append);
    }

    #[test]
    fn unknown_class_is_dropped() {
        assert_eq!(
            validate_feature_writer("NopeWriter", None, &[]).unwrap(),
            None
        );
    }

    #[test]
    fn unknown_module_is_dropped() {
        assert_eq!(
            validate_feature_writer("KernFeatureWriter", Some("some.other.module"), &[]).unwrap(),
            None
        );
    }

    #[test]
    fn module_qualified_builtin_is_accepted() {
        // ufo2ft imports the class from its defining submodule just as happily as
        // from the package root, so both spellings must resolve.
        for (class, module, expected) in [
            (
                "CursFeatureWriter",
                "ufo2ft.featureWriters.cursFeatureWriter",
                KnownFeatureWriter::Curs,
            ),
            (
                "KernFeatureWriter",
                "ufo2ft.featureWriters.kernFeatureWriter",
                KnownFeatureWriter::Kern,
            ),
            (
                "MarkFeatureWriter",
                "ufo2ft.featureWriters.markFeatureWriter",
                KnownFeatureWriter::Mark,
            ),
            (
                "GdefFeatureWriter",
                "ufo2ft.featureWriters.gdefFeatureWriter",
                KnownFeatureWriter::Gdef,
            ),
        ] {
            let spec = validate_feature_writer(class, Some(module), &[])
                .unwrap()
                .unwrap();
            assert_eq!(spec.writer, expected, "{module}.{class}");
        }
    }

    #[test]
    fn mismatched_module_and_class_is_dropped() {
        // ufo2ft's getattr on the submodule would fail, dropping the entry.
        assert_eq!(
            validate_feature_writer(
                "MarkFeatureWriter",
                Some("ufo2ft.featureWriters.kernFeatureWriter"),
                &[]
            )
            .unwrap(),
            None
        );
    }

    #[test]
    fn glyphslib_contextual_mark_alias_maps_to_mark() {
        let spec = validate_feature_writer(
            "ContextualMarkFeatureWriter",
            Some(GLYPHSLIB_MARK_MODULE),
            &[],
        )
        .unwrap()
        .unwrap();
        assert_eq!(spec.writer, KnownFeatureWriter::Mark);
        // The re-exported regular writer resolves from the same module too.
        let spec = validate_feature_writer("MarkFeatureWriter", Some(GLYPHSLIB_MARK_MODULE), &[])
            .unwrap()
            .unwrap();
        assert_eq!(spec.writer, KnownFeatureWriter::Mark);
    }

    #[test]
    fn contextual_mark_alias_requires_its_module() {
        // The alias is not importable from the default ufo2ft package, nor from the
        // glyphsLib.featureWriters package root (whose __init__ exports nothing).
        assert_eq!(
            validate_feature_writer("ContextualMarkFeatureWriter", None, &[]).unwrap(),
            None
        );
        assert_eq!(
            validate_feature_writer(
                "ContextualMarkFeatureWriter",
                Some("glyphsLib.featureWriters"),
                &[]
            )
            .unwrap(),
            None
        );
    }

    #[test]
    fn invalid_mode_is_dropped() {
        assert_eq!(
            validate_feature_writer(
                "KernFeatureWriter",
                None,
                &[opt("mode", FeatureWriterOptionValue::String("nope".into()))]
            )
            .unwrap(),
            None
        );
    }

    #[test]
    fn unknown_option_is_dropped() {
        assert_eq!(
            validate_feature_writer(
                "KernFeatureWriter",
                None,
                &[opt("bogus", FeatureWriterOptionValue::Boolean(true))]
            )
            .unwrap(),
            None
        );
    }

    #[test]
    fn default_option_values_are_noops() {
        // ignoreMarks=true, quantization=1 are the defaults; accepted.
        let spec = validate_feature_writer(
            "KernFeatureWriter",
            None,
            &[
                opt("ignoreMarks", FeatureWriterOptionValue::Boolean(true)),
                opt("quantization", FeatureWriterOptionValue::Integer(1)),
            ],
        )
        .unwrap()
        .unwrap();
        assert_eq!(spec.writer, KnownFeatureWriter::Kern);
    }

    #[test]
    fn non_default_option_is_hard_error() {
        assert!(
            validate_feature_writer(
                "KernFeatureWriter",
                None,
                &[opt("quantization", FeatureWriterOptionValue::Integer(2))]
            )
            .is_err()
        );
    }

    #[test]
    fn drop_wins_over_hard_error() {
        // quantization=2 would be a hard error, but the unknown option drops the
        // whole entry first -- just as ufo2ft binds all kwargs at once.
        assert_eq!(
            validate_feature_writer(
                "KernFeatureWriter",
                None,
                &[
                    opt("quantization", FeatureWriterOptionValue::Integer(2)),
                    opt("bogus", FeatureWriterOptionValue::Boolean(true)),
                ]
            )
            .unwrap(),
            None
        );
    }

    #[test]
    fn kern2_is_hard_error() {
        assert!(validate_feature_writer("KernFeatureWriter", Some(KERN2_MODULE), &[]).is_err());
    }

    #[test]
    fn kern2_with_wrong_class_is_dropped() {
        // ufo2ft's import of a class the kern2 module doesn't define fails, so the
        // entry drops before the writer could be honoured; no hard error.
        assert_eq!(
            validate_feature_writer("MarkFeatureWriter", Some(KERN2_MODULE), &[]).unwrap(),
            None
        );
    }

    #[test]
    fn kern2_dropped_by_malformed_option() {
        // Even the kern2 hard error yields to a malformed option.
        assert_eq!(
            validate_feature_writer(
                "KernFeatureWriter",
                Some(KERN2_MODULE),
                &[opt("bogus", FeatureWriterOptionValue::Boolean(true))]
            )
            .unwrap(),
            None
        );
    }

    #[test]
    fn features_subset_is_honoured() {
        let spec = validate_feature_writer(
            "KernFeatureWriter",
            None,
            &[opt(
                "features",
                FeatureWriterOptionValue::Array(vec!["kern".into()]),
            )],
        )
        .unwrap()
        .unwrap();
        assert_eq!(spec.features, Some(vec![KERN]));
    }

    #[test]
    fn features_not_a_subset_is_dropped() {
        // curs is not one of the kern writer's tags.
        assert_eq!(
            validate_feature_writer(
                "KernFeatureWriter",
                None,
                &[opt(
                    "features",
                    FeatureWriterOptionValue::Array(vec!["curs".into()])
                )]
            )
            .unwrap(),
            None
        );
    }

    #[test]
    fn gdef_features_valid_subset_is_error() {
        // ufo2ft would honour a GlyphClassDefs/LigatureCarets subset and emit a
        // partial GDEF; fontc doesn't implement that, so it must refuse rather
        // than silently emit a different (or no) GDEF.
        for subset in [
            vec!["GlyphClassDefs".to_string()],
            vec!["LigatureCarets".to_string()],
            vec!["GlyphClassDefs".to_string(), "LigatureCarets".to_string()],
        ] {
            let result = validate_feature_writer(
                "GdefFeatureWriter",
                None,
                &[opt("features", FeatureWriterOptionValue::Array(subset))],
            );
            assert!(matches!(result, Err(Error::UnsupportedFeatureWriters(_))));
        }
    }

    #[test]
    fn gdef_features_invalid_subset_is_dropped() {
        // A value ufo2ft's own validation would reject (ValueError -> dropped
        // under ignoreErrors=True).
        assert_eq!(
            validate_feature_writer(
                "GdefFeatureWriter",
                None,
                &[opt(
                    "features",
                    FeatureWriterOptionValue::Array(vec!["kern".into()])
                )]
            )
            .unwrap(),
            None
        );
    }

    #[test]
    fn gdef_features_drop_wins_over_error() {
        // An unknown option drops the whole entry in ufo2ft before the features
        // kwarg could take effect, so the deferred hard error must not fire.
        assert_eq!(
            validate_feature_writer(
                "GdefFeatureWriter",
                None,
                &[
                    opt(
                        "features",
                        FeatureWriterOptionValue::Array(vec!["GlyphClassDefs".into()])
                    ),
                    opt("nonsense", FeatureWriterOptionValue::Boolean(true)),
                ]
            )
            .unwrap(),
            None
        );
    }

    #[test]
    fn duplicate_writers_are_rejected() {
        let kern = FeatureWriterSpec {
            writer: KnownFeatureWriter::Kern,
            mode: FeatureWriterMode::Skip,
            features: None,
        };
        let mark = FeatureWriterSpec {
            writer: KnownFeatureWriter::Mark,
            ..kern.clone()
        };
        assert!(reject_duplicate_writers(&[kern.clone(), mark.clone()]).is_ok());
        // Same writer twice is an error even when the settings differ: ufo2ft
        // would run both entries, fontc would silently keep only the last.
        let kern_append = FeatureWriterSpec {
            mode: FeatureWriterMode::Append,
            ..kern.clone()
        };
        assert!(matches!(
            reject_duplicate_writers(&[kern, kern_append]),
            Err(Error::UnsupportedFeatureWriters(_))
        ));
    }

    #[test]
    fn resolve_absent_enables_everything_in_skip() {
        let plan = resolve_feature_writers(&None);
        assert_eq!(plan.curs.as_ref().unwrap().mode, FeatureWriterMode::Skip);
        assert_eq!(plan.kern.as_ref().unwrap().mode, FeatureWriterMode::Skip);
        assert_eq!(plan.mark.as_ref().unwrap().mode, FeatureWriterMode::Skip);
        assert!(plan.gdef);
    }

    #[test]
    fn resolve_selective() {
        let specs = Some(vec![FeatureWriterSpec {
            writer: KnownFeatureWriter::Kern,
            mode: FeatureWriterMode::Append,
            features: None,
        }]);
        let plan = resolve_feature_writers(&specs);
        assert_eq!(plan.kern.as_ref().unwrap().mode, FeatureWriterMode::Append);
        assert!(plan.curs.is_none());
        assert!(plan.mark.is_none());
        assert!(!plan.gdef);
    }

    #[test]
    fn resolve_empty_disables_everything() {
        let plan = resolve_feature_writers(&Some(vec![]));
        assert!(plan.curs.is_none());
        assert!(plan.kern.is_none());
        assert!(plan.mark.is_none());
        assert!(!plan.gdef);
    }
}
