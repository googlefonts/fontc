//! A font compiler with aspirations of being fast and safe.

mod args;
mod change_detector;
mod config;
mod error;
mod timing;
pub mod work;
mod workload;

pub use args::Args;
pub use change_detector::ChangeDetector;
pub use config::Config;
pub use error::Error;

pub use timing::{create_timer, JobTimer};
use workload::Workload;

use std::{fs, io, path::Path};

use fontbe::{
    avar::create_avar_work,
    cmap::create_cmap_work,
    features::{
        create_gather_ir_kerning_work, create_kerns_work, create_mark_work, FeatureCompilationWork,
        FeatureParsingWork,
    },
    font::create_font_work,
    fvar::create_fvar_work,
    glyphs::{create_glyf_loca_work, create_glyf_work},
    gvar::create_gvar_work,
    head::create_head_work,
    hvar::create_hvar_work,
    metrics_and_limits::create_metric_and_limit_work,
    mvar::create_mvar_work,
    name::create_name_work,
    orchestration::AnyWorkId,
    os2::create_os2_work,
    post::create_post_work,
    stat::create_stat_work,
};

use fontdrasil::{coords::NormalizedLocation, types::GlyphName};
use fontir::{glyph::create_glyph_order_work, source::DeleteWork};

use fontbe::orchestration::Context as BeContext;
use fontbe::paths::Paths as BePaths;
use fontir::paths::Paths as IrPaths;

use log::{debug, warn};

pub fn require_dir(dir: &Path) -> Result<(), io::Error> {
    // skip empty paths
    if dir == Path::new("") {
        return Ok(());
    }
    if dir.exists() && !dir.is_dir() {
        panic!("{dir:#?} is taken by something that isn't a directory");
    }
    if !dir.exists() {
        fs::create_dir(dir)?
    }
    debug!("require_dir {:?}", dir);
    Ok(())
}

pub fn init_paths(args: &Args) -> Result<(IrPaths, BePaths), Error> {
    let ir_paths = IrPaths::new(&args.build_dir);
    let be_paths = if let Some(output_file) = &args.output_file {
        BePaths::with_output_file(&args.build_dir, output_file)
    } else {
        BePaths::new(&args.build_dir)
    };
    // create the output file's parent directory if it doesn't exist
    if let Some(output_file) = &args.output_file {
        if let Some(parent) = output_file.parent() {
            require_dir(parent)?;
        }
    }

    // the build dir stores the IR (for incremental builds) and the default output
    // file ('font.ttf') so we don't need to create one unless we're writing to it
    if args.output_file.is_none() || args.incremental {
        require_dir(&args.build_dir)?;
    }
    if args.incremental {
        require_dir(ir_paths.anchor_ir_dir())?;
        require_dir(ir_paths.glyph_ir_dir())?;
        require_dir(be_paths.glyph_dir())?;
    }
    // It's confusing to have leftover debug files
    if be_paths.debug_dir().is_dir() {
        fs::remove_dir_all(be_paths.debug_dir()).map_err(Error::IoError)?;
    }
    if args.emit_debug {
        require_dir(be_paths.debug_dir())?;
    }
    Ok((ir_paths, be_paths))
}

pub fn write_font_file(args: &Args, be_context: &BeContext) -> Result<(), Error> {
    // if IR is off the font didn't get written yet (nothing did), otherwise it's done already
    let font_file = be_context.font_file();
    if !args.incremental {
        fs::write(font_file, be_context.font.get().get()).map_err(Error::IoError)?;
    } else if !font_file.exists() {
        return Err(Error::FileExpected(font_file));
    }
    Ok(())
}

fn add_glyph_order_ir_job(workload: &mut Workload) -> Result<(), Error> {
    let work = create_glyph_order_work().into();
    workload.add(work, workload.change_detector.glyph_order_ir_change());

    Ok(())
}

fn add_feature_ir_job(workload: &mut Workload) -> Result<(), Error> {
    let work = workload
        .change_detector
        .ir_source()
        .create_feature_ir_work(workload.change_detector.current_inputs())?
        .into();
    workload.add(
        work,
        workload.change_detector.feature_ir_change()
            && !workload.change_detector.should_skip_features(),
    );
    Ok(())
}

fn add_feature_parse_be_job(workload: &mut Workload) -> Result<(), Error> {
    let work = FeatureParsingWork::create();

    // copied from below
    if workload.change_detector.feature_be_change() {
        if workload.change_detector.glyph_name_filter().is_some() {
            warn!("Not processing BE Features because a glyph name filter is active");
        }
        if workload.change_detector.should_skip_features() {
            debug!("Not processing BE Features because FEA compilation is disabled");
        }
    }

    workload.add(
        work.into(),
        workload.change_detector.feature_be_change()
            && workload.change_detector.glyph_name_filter().is_none()
            && !workload.change_detector.should_skip_features(),
    );
    Ok(())
}

fn add_feature_comp_be_job(workload: &mut Workload) -> Result<(), Error> {
    let work = FeatureCompilationWork::create();
    // Features are extremely prone to not making sense when glyphs are filtered
    if workload.change_detector.feature_be_change() {
        if workload.change_detector.glyph_name_filter().is_some() {
            warn!("Not processing BE Features because a glyph name filter is active");
        }
        if workload.change_detector.should_skip_features() {
            debug!("Not processing BE Features because FEA compilation is disabled");
        }
    }
    workload.add(
        work.into(),
        workload.change_detector.feature_be_change()
            && workload.change_detector.glyph_name_filter().is_none()
            && !workload.change_detector.should_skip_features(),
    );
    Ok(())
}

fn add_marks_be_job(workload: &mut Workload) -> Result<(), Error> {
    let work = create_mark_work();
    // Features are extremely prone to not making sense when glyphs are filtered
    if workload.change_detector.mark_be_change() {
        if workload.change_detector.glyph_name_filter().is_some() {
            warn!("Not processing BE marks because a glyph name filter is active");
        }
        if workload.change_detector.should_skip_features() {
            debug!("Not processing BE marks because FEA compilation is disabled");
        }
    }
    workload.add(
        work.into(),
        workload.change_detector.mark_be_change()
            && workload.change_detector.glyph_name_filter().is_none()
            && !workload.change_detector.should_skip_features(),
    );
    Ok(())
}

fn add_gather_ir_kerning_be_job(workload: &mut Workload) -> Result<(), Error> {
    let work = create_gather_ir_kerning_work();
    // Features are extremely prone to not making sense when glyphs are filtered
    if workload.change_detector.kerning_be_change() {
        if workload.change_detector.glyph_name_filter().is_some() {
            warn!("Not processing BE kerning because a glyph name filter is active");
        }
        if workload.change_detector.should_skip_features() {
            debug!("Not processing BE kerning because FEA compilation is disabled");
        }
    }
    workload.add(
        work.into(),
        workload.change_detector.kerning_be_change()
            && workload.change_detector.glyph_name_filter().is_none()
            && !workload.change_detector.should_skip_features(),
    );
    Ok(())
}

fn add_kerns_be_job(workload: &mut Workload) -> Result<(), Error> {
    let work = create_kerns_work();
    // Features are extremely prone to not making sense when glyphs are filtered
    if workload.change_detector.kerning_be_change() {
        if workload.change_detector.glyph_name_filter().is_some() {
            warn!("Not processing BE kerning because a glyph name filter is active");
        }
        if workload.change_detector.should_skip_features() {
            debug!("Not processing BE kerning because FEA compilation is disabled");
        }
    }
    workload.add(
        work.into(),
        workload.change_detector.kerning_be_change()
            && workload.change_detector.glyph_name_filter().is_none()
            && !workload.change_detector.should_skip_features(),
    );
    Ok(())
}

fn add_kerning_group_ir_job(workload: &mut Workload) -> Result<(), Error> {
    let work = workload
        .change_detector
        .ir_source()
        .create_kerning_group_ir_work(workload.change_detector.current_inputs())?;
    workload.add(
        work.into(),
        workload.change_detector.kerning_groups_ir_change()
            && !workload.change_detector.should_skip_features(),
    );
    Ok(())
}

fn add_kern_instance_ir_job(workload: &mut Workload, at: NormalizedLocation) -> Result<(), Error> {
    let work = workload
        .change_detector
        .ir_source()
        .create_kerning_instance_ir_work(workload.current_inputs(), at.clone())?
        .into();
    workload.add(
        work,
        workload.change_detector.kerning_at_ir_change(at)
            && !workload.change_detector.should_skip_features(),
    );
    Ok(())
}

fn add_glyph_ir_jobs(workload: &mut Workload) -> Result<(), Error> {
    // Destroy IR for deleted glyphs. No dependencies.
    for glyph_name in workload.change_detector.glyphs_deleted().iter() {
        let work = DeleteWork::create(glyph_name.clone());
        workload.add(work.into(), true);
    }

    // Generate IR for changed glyphs
    let glyphs_changed = workload.change_detector.glyphs_changed();
    let glyph_work = workload
        .change_detector
        .ir_source()
        .create_glyph_ir_work(glyphs_changed, workload.change_detector.current_inputs())?;
    for work in glyph_work {
        workload.add(work.into(), true);
    }

    Ok(())
}

fn add_glyph_be_jobs(workload: &mut Workload) -> Result<(), Error> {
    let glyphs_changed = workload.change_detector.glyphs_changed();
    for glyph_name in glyphs_changed {
        add_glyph_be_job(workload, glyph_name.clone());
    }
    Ok(())
}

fn add_glyf_loca_be_job(workload: &mut Workload) -> Result<(), Error> {
    let glyphs_changed = workload.change_detector.glyphs_changed();

    // If no glyph has changed there isn't a lot of merging to do
    let work = create_glyf_loca_work().into();
    workload.add(work, !glyphs_changed.is_empty());

    Ok(())
}

fn add_avar_be_job(workload: &mut Workload) -> Result<(), Error> {
    let work = create_avar_work().into();
    workload.add(work, workload.change_detector.avar_be_change());
    Ok(())
}

fn add_stat_be_job(workload: &mut Workload) -> Result<(), Error> {
    let work = create_stat_work().into();
    workload.add(work, workload.change_detector.stat_be_change());
    Ok(())
}

fn add_fvar_be_job(workload: &mut Workload) -> Result<(), Error> {
    let work = create_fvar_work().into();
    workload.add(work, workload.change_detector.fvar_be_change());
    Ok(())
}

fn add_gvar_be_job(workload: &mut Workload) -> Result<(), Error> {
    let glyphs_changed = workload.change_detector.glyphs_changed();

    // If no glyph has changed there isn't a lot of merging to do
    let work = create_gvar_work().into();
    workload.add(work, !glyphs_changed.is_empty());

    Ok(())
}

fn add_cmap_be_job(workload: &mut Workload) -> Result<(), Error> {
    let glyphs_changed = workload.change_detector.glyphs_changed();

    // If no glyph has changed there isn't a lot of merging to do
    let work = create_cmap_work().into();
    workload.add(work, !glyphs_changed.is_empty());

    Ok(())
}

fn add_post_be_job(workload: &mut Workload) -> Result<(), Error> {
    let work = create_post_work().into();
    workload.add(work, workload.change_detector.post_be_change());
    Ok(())
}

fn add_head_be_job(workload: &mut Workload) -> Result<(), Error> {
    let work = create_head_work().into();
    workload.add(work, workload.change_detector.glyph_order_ir_change());
    Ok(())
}

fn add_name_be_job(workload: &mut Workload) -> Result<(), Error> {
    let work = create_name_work().into();
    workload.add(work, workload.change_detector.static_metadata_ir_change());
    Ok(())
}

fn add_os2_be_job(workload: &mut Workload) -> Result<(), Error> {
    let work = create_os2_work().into();
    workload.add(work, workload.change_detector.static_metadata_ir_change());
    Ok(())
}

fn add_metric_and_limits_job(workload: &mut Workload) -> Result<(), Error> {
    let glyphs_changed = workload.change_detector.glyphs_changed();

    // If no glyph has changed there isn't a lot to do
    let work = create_metric_and_limit_work().into();
    workload.add(work, !glyphs_changed.is_empty());
    Ok(())
}

fn add_hvar_be_job(workload: &mut Workload) -> Result<(), Error> {
    let glyphs_changed = workload.change_detector.glyphs_changed();

    let work = create_hvar_work().into();
    workload.add(
        work,
        // Static metadata contains VariationModel, if axes coordinates change
        // we need to recompute variable tables such as HVAR.
        // Glyph order matters because HVAR may choose to store variation items
        // directly mapping to glyph indices. And glyph IR contains advance width
        // among other things.
        // TODO: ideally be more granular here e.g. by storing axes and advance widths
        // in a separate IR file.
        // https://github.com/googlefonts/fontc/issues/526
        workload.change_detector.static_metadata_ir_change()
            || workload.change_detector.glyph_order_ir_change()
            || !glyphs_changed.is_empty(),
    );
    Ok(())
}

fn add_mvar_be_job(workload: &mut Workload) -> Result<(), Error> {
    let work = create_mvar_work().into();
    workload.add(
        work,
        workload.change_detector.static_metadata_ir_change()
            || workload.change_detector.global_metrics_ir_change(),
    );
    Ok(())
}

fn add_font_be_job(workload: &mut Workload) -> Result<(), Error> {
    let glyphs_changed = workload.change_detector.glyphs_changed();

    // If glyphs or features changed we better do the thing
    let work = create_font_work();
    workload.add(
        work.into(),
        !glyphs_changed.is_empty() || workload.change_detector.feature_be_change(),
    );
    Ok(())
}

fn add_glyph_be_job(workload: &mut Workload, glyph_name: GlyphName) {
    let work = create_glyf_work(glyph_name).into();
    let should_run = workload.change_detector.simple_should_run(&work);
    workload.add(work, should_run);
}

//FIXME: I should be a method on ChangeDetector
pub fn create_workload(
    change_detector: &mut ChangeDetector,
    timer: JobTimer,
) -> Result<Workload, Error> {
    let time = create_timer(AnyWorkId::InternalTiming("Create workload"), 0)
        .queued()
        .run();
    let mut workload = change_detector.create_workload(timer)?;

    // FE: f(source) => IR
    add_feature_ir_job(&mut workload)?;
    add_kerning_group_ir_job(&mut workload)?;
    add_glyph_ir_jobs(&mut workload)?;
    add_glyph_order_ir_job(&mut workload)?;

    // BE: f(IR, maybe other BE work) => binary

    add_feature_parse_be_job(&mut workload)?;
    add_feature_comp_be_job(&mut workload)?;
    add_glyph_be_jobs(&mut workload)?;
    add_glyf_loca_be_job(&mut workload)?;
    add_avar_be_job(&mut workload)?;
    add_stat_be_job(&mut workload)?;
    add_cmap_be_job(&mut workload)?;
    add_fvar_be_job(&mut workload)?;
    add_gvar_be_job(&mut workload)?;
    add_head_be_job(&mut workload)?;
    add_gather_ir_kerning_be_job(&mut workload)?;
    add_kerns_be_job(&mut workload)?;
    add_marks_be_job(&mut workload)?;
    add_metric_and_limits_job(&mut workload)?;
    add_hvar_be_job(&mut workload)?;
    add_mvar_be_job(&mut workload)?;
    add_name_be_job(&mut workload)?;
    add_os2_be_job(&mut workload)?;
    add_post_be_job(&mut workload)?;

    // Make a damn font
    add_font_be_job(&mut workload)?;

    workload.timer.add(time.complete());

    Ok(workload)
}

#[cfg(test)]
pub fn testdata_dir() -> std::path::PathBuf {
    // cargo test seems to run in the project directory
    // VSCode test seems to run in the workspace directory
    // probe for the file we want in hopes of finding it regardless

    ["./resources/testdata", "../resources/testdata"]
        .iter()
        .map(std::path::PathBuf::from)
        .find(|pb| pb.exists())
        .unwrap()
}

#[cfg(test)]
mod tests {

    use std::{
        collections::{HashMap, HashSet, VecDeque},
        fs::{self, File},
        io::Read,
        path::{Path, PathBuf},
        str::FromStr,
        sync::Arc,
        time::Instant,
    };

    use chrono::{Duration, TimeZone, Utc};
    use fontbe::orchestration::{
        AnyWorkId, Context as BeContext, Glyph, LocaFormatWrapper, WorkId as BeWorkIdentifier,
    };
    use fontdrasil::{coords::NormalizedCoord, paths::string_to_filename, types::GlyphName};
    use fontir::{
        ir::{self, GlyphOrder, KernGroup, KernPair, KernSide},
        orchestration::{Context as FeContext, Persistable, WorkId as FeWorkIdentifier},
    };
    use indexmap::IndexSet;
    use kurbo::{Point, Rect};
    use log::info;
    use pretty_assertions::assert_eq;

    use skrifa::{
        charmap::Charmap,
        instance::Size,
        outline::DrawSettings,
        raw::{
            tables::{
                cmap::{Cmap, CmapSubtable},
                glyf::{self, CompositeGlyph, CurvePoint, Glyf},
                hmtx::Hmtx,
                loca::Loca,
            },
            types::F2Dot14,
            FontData, FontRead, FontReadWithArgs, FontRef, TableProvider,
        },
        GlyphId, MetadataProvider, Tag,
    };
    use tempfile::{tempdir, TempDir};
    use write_fonts::{
        dump_table,
        tables::{
            gdef::GlyphClassDef,
            glyf::{Bbox, Glyph as RawGlyph},
            loca::LocaFormat,
        },
    };
    use write_fonts::{
        read::{
            tables::{
                gpos::{AnchorTable, Gpos, MarkBasePosFormat1Marker, PositionLookup},
                name::Name,
                os2::SelectionFlags,
                variations::{DeltaSetIndexMap, ItemVariationData},
            },
            TableRef,
        },
        types::NameId,
    };

    use super::*;

    struct TestCompile {
        /// we need to hold onto this because when it goes out of scope,
        /// the directory is deleted.
        _temp_dir: TempDir,
        build_dir: PathBuf,
        args: Args,
        work_executed: HashSet<AnyWorkId>,
        glyphs_changed: IndexSet<GlyphName>,
        glyphs_deleted: IndexSet<GlyphName>,
        fe_context: FeContext,
        be_context: BeContext,
        raw_font: Vec<u8>,
    }

    impl TestCompile {
        fn compile_source(source: &str) -> TestCompile {
            TestCompile::compile(source, |args| args)
        }

        fn compile_again(prior_compile: &TestCompile) -> TestCompile {
            TestCompile::compile(prior_compile.args.source().to_str().unwrap(), |_| {
                prior_compile.args.clone()
            })
        }

        fn compile(source: &str, adjust_args: impl Fn(Args) -> Args) -> TestCompile {
            let mut timer = JobTimer::new(Instant::now());
            let _ = env_logger::builder().is_test(true).try_init();

            let temp_dir = tempdir().unwrap();
            let build_dir = temp_dir.path();
            let args = adjust_args(Args::for_test(build_dir, source));

            info!("Compile {args:?}");

            let (ir_paths, be_paths) = init_paths(&args).unwrap();
            let config = Config::new(args).unwrap();

            let prev_inputs = config.init().unwrap();

            let mut change_detector = ChangeDetector::new(
                config.clone(),
                ir_paths.clone(),
                be_paths.clone(),
                prev_inputs,
                &mut timer,
            )
            .unwrap();
            let build_dir = change_detector.be_paths().build_dir().to_path_buf();

            let fe_context = FeContext::new_root(
                config.args.flags(),
                ir_paths,
                change_detector.current_inputs().clone(),
            );
            let be_context =
                BeContext::new_root(config.args.flags(), be_paths, &fe_context.read_only());
            let mut result = TestCompile {
                _temp_dir: temp_dir,
                build_dir,
                args: config.args.clone(),
                work_executed: HashSet::new(),
                glyphs_changed: change_detector.glyphs_changed().clone(),
                glyphs_deleted: change_detector.glyphs_deleted().clone(),
                fe_context,
                be_context,
                raw_font: Vec::new(),
            };

            let mut workload = create_workload(&mut change_detector, timer).unwrap();
            let completed = workload.run_for_test(&result.fe_context, &result.be_context);

            change_detector.finish_successfully().unwrap();
            result.work_executed = completed;

            write_font_file(&config.args, &result.be_context).unwrap();

            result.raw_font = fs::read(result.build_dir.join("font.ttf")).unwrap();

            result
        }

        fn get_glyph_index(&self, name: &str) -> Option<u32> {
            self.fe_context
                .glyph_order
                .get()
                .glyph_id(&name.into())
                .map(|v| v.to_u16() as u32)
        }

        /// Returns the GlyphId (or NOTDEF, if not present)
        fn get_gid(&self, name: &str) -> GlyphId {
            let raw = self.get_glyph_index(name).unwrap_or_default();
            GlyphId::new(raw as u16)
        }

        fn glyphs(&self) -> Glyphs {
            Glyphs::new(&self.build_dir)
        }

        fn font(&self) -> FontRef {
            FontRef::new(&self.raw_font).unwrap()
        }
    }

    struct Glyphs {
        loca_format: LocaFormat,
        raw_glyf: Vec<u8>,
        raw_loca: Vec<u8>,
    }

    impl Glyphs {
        fn new(build_dir: &Path) -> Self {
            Glyphs {
                loca_format: LocaFormatWrapper::read(
                    &mut File::open(build_dir.join("loca.format")).unwrap(),
                )
                .into(),
                raw_glyf: read_file(&build_dir.join("glyf.table")),
                raw_loca: read_file(&build_dir.join("loca.table")),
            }
        }

        fn read(&self) -> Vec<Option<glyf::Glyph>> {
            let glyf = Glyf::read(FontData::new(&self.raw_glyf)).unwrap();
            let loca = Loca::read(
                FontData::new(&self.raw_loca),
                self.loca_format == LocaFormat::Long,
            )
            .unwrap();
            (0..loca.len())
                .map(|gid| loca.get_glyf(GlyphId::new(gid as u16), &glyf))
                .map(|r| r.unwrap())
                .collect()
        }
    }

    /// Copy testdata => tempdir so the test can modify it
    fn copy_testdata(from: impl IntoIterator<Item = impl AsRef<Path>>, to_dir: &Path) {
        let from_dir = testdata_dir();

        let mut from: VecDeque<PathBuf> = from.into_iter().map(|p| from_dir.join(p)).collect();
        while let Some(source) = from.pop_back() {
            let rel_source = source.strip_prefix(&from_dir).unwrap();
            let dest = to_dir.join(rel_source);
            assert!(
                source.exists(),
                "cannot copy '{source:?}'; it doesn't exist"
            );

            let dest_dir = if source.is_dir() {
                dest.as_path()
            } else {
                dest.parent().unwrap()
            };
            if !dest_dir.exists() {
                fs::create_dir_all(dest_dir).unwrap();
            }

            if source.is_file() {
                fs::copy(&source, &dest).unwrap();
            }
            if source.is_dir() {
                for entry in fs::read_dir(source).unwrap() {
                    from.push_back(entry.unwrap().path());
                }
            }
        }
    }

    fn assert_compile_work(source: &str, glyphs: Vec<&str>) {
        let result = TestCompile::compile_source(source);
        let mut completed = result.work_executed.iter().cloned().collect::<Vec<_>>();

        let mut expected = vec![
            AnyWorkId::Fe(FeWorkIdentifier::StaticMetadata),
            FeWorkIdentifier::GlobalMetrics.into(),
            FeWorkIdentifier::PreliminaryGlyphOrder.into(),
            FeWorkIdentifier::GlyphOrder.into(),
            FeWorkIdentifier::Features.into(),
            FeWorkIdentifier::KerningGroups.into(),
            FeWorkIdentifier::KernInstance(NormalizedLocation::for_pos(&[("wght", 0.0)])).into(),
            FeWorkIdentifier::KernInstance(NormalizedLocation::for_pos(&[("wght", 1.0)])).into(),
            BeWorkIdentifier::Features.into(),
            BeWorkIdentifier::FeaturesAst.into(),
            BeWorkIdentifier::Avar.into(),
            BeWorkIdentifier::Cmap.into(),
            BeWorkIdentifier::Font.into(),
            BeWorkIdentifier::Fvar.into(),
            BeWorkIdentifier::Glyf.into(),
            BeWorkIdentifier::Gpos.into(),
            BeWorkIdentifier::Gsub.into(),
            BeWorkIdentifier::Gdef.into(),
            BeWorkIdentifier::Gvar.into(),
            BeWorkIdentifier::Head.into(),
            BeWorkIdentifier::Hhea.into(),
            BeWorkIdentifier::Hmtx.into(),
            BeWorkIdentifier::Hvar.into(),
            BeWorkIdentifier::GatherIrKerning.into(),
            BeWorkIdentifier::KernFragment(0).into(),
            BeWorkIdentifier::GatherBeKerning.into(),
            BeWorkIdentifier::Loca.into(),
            BeWorkIdentifier::LocaFormat.into(),
            BeWorkIdentifier::Marks.into(),
            BeWorkIdentifier::Maxp.into(),
            BeWorkIdentifier::Mvar.into(),
            BeWorkIdentifier::Name.into(),
            BeWorkIdentifier::Os2.into(),
            BeWorkIdentifier::Post.into(),
            BeWorkIdentifier::Stat.into(),
        ];

        expected.extend(
            glyphs
                .iter()
                .map(|n| FeWorkIdentifier::Glyph((*n).into()).into()),
        );
        expected.extend(
            glyphs
                .iter()
                .map(|n| FeWorkIdentifier::Anchor((*n).into()).into()),
        );
        expected.extend(
            glyphs
                .iter()
                .map(|n| BeWorkIdentifier::GlyfFragment((*n).into()).into()),
        );
        expected.extend(
            glyphs
                .iter()
                .map(|n| BeWorkIdentifier::GvarFragment((*n).into()).into()),
        );

        expected.extend(vec![
            BeWorkIdentifier::GlyfFragment((".notdef").into()).into(),
            BeWorkIdentifier::GvarFragment((".notdef").into()).into(),
        ]);

        completed.sort();
        expected.sort();
        assert_eq!(expected, completed);
    }

    #[test]
    fn compile_work_for_designspace() {
        assert_compile_work("wght_var.designspace", vec!["bar", "plus"])
    }

    #[test]
    fn compile_work_for_glyphs() {
        assert_compile_work(
            "glyphs3/WghtVar.glyphs",
            vec![
                "bracketleft",
                "bracketright",
                "exclam",
                "hyphen",
                "manual-component",
                "space",
            ],
        )
    }

    #[test]
    fn second_compile_is_nop() {
        let result = TestCompile::compile_source("wght_var.designspace");
        assert_eq!(
            IndexSet::from(["bar".into(), "plus".into()]),
            result.glyphs_changed
        );
        assert!(result.glyphs_deleted.is_empty());

        let result = TestCompile::compile_again(&result);
        assert!(result.work_executed.is_empty());
        assert!(result.glyphs_changed.is_empty());
        assert!(result.glyphs_deleted.is_empty());
    }

    #[test]
    fn second_compile_only_glyph_ir() {
        // glyph depends on static metadata, which isn't going to run
        let result = TestCompile::compile_source("wght_var.designspace");
        assert!(result.work_executed.len() > 1);

        let glyph_ir_file = result.build_dir.join("glyph_ir/bar.yml");
        fs::remove_file(&glyph_ir_file).unwrap();

        let result = TestCompile::compile_again(&result);
        assert!(glyph_ir_file.exists(), "{glyph_ir_file:?}");
        let mut completed = result.work_executed.iter().cloned().collect::<Vec<_>>();
        completed.sort();
        assert_eq!(
            vec![
                AnyWorkId::Fe(FeWorkIdentifier::Glyph("bar".into())),
                FeWorkIdentifier::Anchor("bar".into()).into(),
                BeWorkIdentifier::Features.into(),
                BeWorkIdentifier::FeaturesAst.into(),
                BeWorkIdentifier::Cmap.into(),
                BeWorkIdentifier::Font.into(),
                BeWorkIdentifier::Glyf.into(),
                BeWorkIdentifier::Gpos.into(),
                BeWorkIdentifier::Gsub.into(),
                BeWorkIdentifier::Gdef.into(),
                BeWorkIdentifier::Gvar.into(),
                BeWorkIdentifier::Hhea.into(),
                BeWorkIdentifier::Hmtx.into(),
                BeWorkIdentifier::Hvar.into(),
                BeWorkIdentifier::Loca.into(),
                BeWorkIdentifier::LocaFormat.into(),
                BeWorkIdentifier::Marks.into(),
                BeWorkIdentifier::Maxp.into(),
            ],
            completed,
            "{completed:#?}"
        );
    }

    #[test]
    fn second_compile_only_kerning() {
        let result = TestCompile::compile_source("glyphs3/WghtVar.glyphs");
        assert!(result.work_executed.len() > 1);

        fs::remove_file(result.build_dir.join("kern_groups.yml")).unwrap();
        fs::remove_file(result.build_dir.join("kern_wght_0.00.yml")).unwrap();
        fs::remove_file(result.build_dir.join("kern_wght_1.00.yml")).unwrap();

        let result = TestCompile::compile_again(&result);
        let mut completed = result.work_executed.iter().cloned().collect::<Vec<_>>();
        completed.sort();
        assert_eq!(
            vec![
                AnyWorkId::Fe(FeWorkIdentifier::Features),
                FeWorkIdentifier::KerningGroups.into(),
                FeWorkIdentifier::KernInstance(NormalizedLocation::for_pos(&[("wght", 0.0)]))
                    .into(),
                FeWorkIdentifier::KernInstance(NormalizedLocation::for_pos(&[("wght", 1.0)]))
                    .into(),
                BeWorkIdentifier::Features.into(),
                BeWorkIdentifier::FeaturesAst.into(),
                BeWorkIdentifier::Font.into(),
                BeWorkIdentifier::Gpos.into(),
                BeWorkIdentifier::Gsub.into(),
                BeWorkIdentifier::Gdef.into(),
                BeWorkIdentifier::GatherIrKerning.into(),
                BeWorkIdentifier::KernFragment(0).into(),
                BeWorkIdentifier::GatherBeKerning.into(),
            ],
            completed
        );
    }

    #[test]
    fn deleted_ir_recreated() {
        let result = TestCompile::compile_source("wght_var.designspace");
        assert_eq!(
            IndexSet::from(["bar".into(), "plus".into()]),
            result.glyphs_changed
        );
        assert!(result.glyphs_deleted.is_empty());

        let bar_ir = result.build_dir.join("glyph_ir/bar.yml");
        assert!(bar_ir.is_file(), "no file {bar_ir:#?}");
        fs::remove_file(bar_ir).unwrap();

        let result = TestCompile::compile_again(&result);
        assert_eq!(IndexSet::from(["bar".into()]), result.glyphs_changed);
        assert!(result.glyphs_deleted.is_empty());
    }

    fn assert_compiles_with_gpos_and_gsub(
        source: &str,
        adjust_args: impl Fn(Args) -> Args,
    ) -> TestCompile {
        let result = TestCompile::compile(source, adjust_args);

        let font = result.font();

        let gpos = font.gpos();
        let gsub = font.gsub();
        assert!(
            gpos.is_ok() && gsub.is_ok(),
            "\ngpos: {gpos:?}\ngsub: {gsub:?}"
        );

        result
    }
    #[test]
    fn compile_fea() {
        assert_compiles_with_gpos_and_gsub("static.designspace", |a| a);
    }

    #[test]
    fn compile_fea_with_includes() {
        assert_compiles_with_gpos_and_gsub("fea_include.designspace", |a| a);
    }

    #[test]
    fn compile_fea_with_includes_no_ir() {
        assert_compiles_with_gpos_and_gsub("fea_include.designspace", |mut args| {
            args.emit_debug = false;
            args.incremental = false;
            args
        });
    }

    #[test]
    fn compile_fea_again_with_modified_include() {
        let temp_dir = tempdir().unwrap();
        let source_dir = temp_dir.path().join("sources");
        let build_dir = temp_dir.path().join("build");
        fs::create_dir(&source_dir).unwrap();
        fs::create_dir(build_dir).unwrap();

        copy_testdata(
            [
                "fea_include.designspace",
                "fea_include_ufo/common.fea",
                "fea_include_ufo/FeaInc-Regular.ufo",
                "fea_include_ufo/FeaInc-Bold.ufo",
            ],
            &source_dir,
        );

        let source = source_dir
            .join("fea_include.designspace")
            .canonicalize()
            .unwrap();
        let result = TestCompile::compile_source(source.to_str().unwrap());

        let shared_fea = source_dir.join("fea_include_ufo/common.fea");
        fs::write(
            &shared_fea,
            fs::read_to_string(&shared_fea).unwrap() + "\n\nfeature k2 { pos bar bar -100; } k2;",
        )
        .unwrap();

        let result = TestCompile::compile_again(&result);

        // Features and things downstream of features should rebuild
        let mut completed = result.work_executed.iter().cloned().collect::<Vec<_>>();
        completed.sort();
        assert_eq!(
            vec![
                AnyWorkId::Fe(FeWorkIdentifier::Features),
                AnyWorkId::Fe(FeWorkIdentifier::KerningGroups),
                AnyWorkId::Fe(FeWorkIdentifier::KernInstance(NormalizedLocation::for_pos(
                    &[("wght", 0.0)]
                ))),
                AnyWorkId::Fe(FeWorkIdentifier::KernInstance(NormalizedLocation::for_pos(
                    &[("wght", 1.0)]
                ))),
                BeWorkIdentifier::Features.into(),
                BeWorkIdentifier::FeaturesAst.into(),
                BeWorkIdentifier::Font.into(),
                BeWorkIdentifier::Gpos.into(),
                BeWorkIdentifier::Gsub.into(),
                BeWorkIdentifier::Gdef.into(),
                BeWorkIdentifier::GatherIrKerning.into(),
                BeWorkIdentifier::KernFragment(0).into(),
                BeWorkIdentifier::GatherBeKerning.into(),
            ],
            completed
        );
    }

    fn build_contour_and_composite_glyph(prefer_simple_glyphs: bool) -> (TestCompile, ir::Glyph) {
        let result = TestCompile::compile("glyphs2/MixedContourComponent.glyphs", |mut args| {
            args.prefer_simple_glyphs = prefer_simple_glyphs; // <-- important :)
            args
        });

        let glyph = result
            .fe_context
            .glyphs
            .get(&FeWorkIdentifier::Glyph("contour_and_component".into()));

        assert_eq!(1, glyph.sources().len());
        (result, (*glyph).clone())
    }

    fn read_file(path: &Path) -> Vec<u8> {
        assert!(path.exists(), "{path:?} not found");
        let mut buf = Vec::new();
        File::open(path).unwrap().read_to_end(&mut buf).unwrap();
        buf
    }

    fn read_ir_glyph(build_dir: &Path, name: &str) -> ir::Glyph {
        let raw_glyph = read_file(
            &build_dir
                .join("glyph_ir")
                .join(string_to_filename(name, ".yml")),
        );
        ir::Glyph::read(&mut raw_glyph.as_slice())
    }

    fn read_be_glyph(build_dir: &Path, name: &str) -> RawGlyph {
        let raw_glyph = read_file(
            &build_dir
                .join("glyphs")
                .join(string_to_filename(name, ".glyf")),
        );
        let read: &mut dyn Read = &mut raw_glyph.as_slice();
        Glyph::read(read).data
    }

    #[test]
    fn resolve_contour_and_composite_glyph_in_non_legacy_mode() {
        let (result, glyph) = build_contour_and_composite_glyph(false);
        assert!(glyph.default_instance().contours.is_empty(), "{glyph:?}");
        assert_eq!(2, glyph.default_instance().components.len(), "{glyph:?}");

        let RawGlyph::Composite(glyph) = read_be_glyph(&result.build_dir, glyph.name.as_str())
        else {
            panic!("Expected a simple glyph");
        };
        let raw_glyph = dump_table(&glyph).unwrap();
        let glyph = CompositeGlyph::read(FontData::new(&raw_glyph)).unwrap();
        // -1: composite, per https://learn.microsoft.com/en-us/typography/opentype/spec/glyf
        assert_eq!(-1, glyph.number_of_contours());
    }

    #[test]
    fn resolve_contour_and_composite_glyph_in_legacy_mode() {
        let (result, glyph) = build_contour_and_composite_glyph(true);
        assert!(glyph.default_instance().components.is_empty(), "{glyph:?}");
        assert_eq!(2, glyph.default_instance().contours.len(), "{glyph:?}");

        let RawGlyph::Simple(glyph) = read_be_glyph(&result.build_dir, glyph.name.as_str()) else {
            panic!("Expected a simple glyph");
        };
        assert_eq!(2, glyph.contours().len());
    }

    #[test]
    fn compile_simple_binary_glyph() {
        let result = TestCompile::compile_source("static.designspace");

        let RawGlyph::Simple(glyph) = read_be_glyph(&result.build_dir, "bar") else {
            panic!("Expected a simple glyph");
        };

        assert_eq!(1, glyph.contours().len());
        assert_eq!(
            4_usize,
            glyph
                .contours()
                .iter()
                .map(|c| c.iter().count())
                .sum::<usize>()
        );
        assert_eq!(
            Bbox {
                x_min: 222,
                y_min: -241,
                x_max: 295,
                y_max: 760,
            },
            glyph.bbox,
        );
    }

    #[test]
    fn compile_simple_glyphs_to_glyf_loca() {
        let result = TestCompile::compile_source("static.designspace");

        // See resources/testdata/Static-Regular.ufo/glyphs
        // generated .notdef, 8 points, 2 contours
        // space, 0 points, 0 contour
        // bar, 4 points, 1 contour
        // plus, 12 points, 1 contour
        // element of, 0 points, 0 contours
        assert_eq!(
            vec![(8, 2), (0, 0), (4, 1), (12, 1), (0, 0)],
            result
                .glyphs()
                .read()
                .iter()
                .map(|g| match g {
                    None => (0, 0),
                    Some(glyf::Glyph::Simple(glyph)) =>
                        (glyph.num_points(), glyph.number_of_contours()),
                    Some(glyf::Glyph::Composite(glyph)) => (0, glyph.number_of_contours()),
                })
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn compile_variable_simple_glyph_with_implied_oncurves() {
        let result = TestCompile::compile_source("glyphs3/Oswald-O.glyphs");

        let glyph_data = result.glyphs();
        let glyphs = glyph_data.read();
        // the glyph 'O' contains several quad splines
        let uppercase_o = &glyphs[result.get_glyph_index("O").unwrap() as usize];
        let Some(glyf::Glyph::Simple(glyph)) = uppercase_o else {
            panic!("Expected 'O' to be a simple glyph, got {:?}", uppercase_o);
        };
        assert_eq!(2, glyph.number_of_contours());
        assert_eq!(35, glyph.num_points());
        assert_eq!(
            [48, -9, 491, 817],
            [glyph.x_min(), glyph.y_min(), glyph.x_max(), glyph.y_max()]
        );
        // for brevity, we only check the first 5 points to confirm that we now get
        // multiple off-curve points in a row with impliable on-curve points omitted
        assert_eq!(
            vec![
                CurvePoint::on_curve(270, -9),
                CurvePoint::off_curve(188, -9),
                CurvePoint::off_curve(90, 55),
                CurvePoint::off_curve(48, 174),
                CurvePoint::on_curve(48, 254),
            ],
            glyph.points().take(5).collect::<Vec<_>>()
        );
    }

    #[test]
    fn compile_composite_glyphs_has_expected_glyph_types() {
        let result = TestCompile::compile_source("glyphs2/Component.glyphs");
        // Per source, glyphs should be period, comma, non_uniform_scale
        // Period is simple, the other two use it as a component
        let glyph_data = result.glyphs();
        let all_glyphs = glyph_data.read();

        assert!(matches!(
            (
                all_glyphs[result.get_glyph_index("period").unwrap() as usize]
                    .as_ref()
                    .unwrap(),
                all_glyphs[result.get_glyph_index("comma").unwrap() as usize]
                    .as_ref()
                    .unwrap(),
                all_glyphs[result.get_glyph_index("non_uniform_scale").unwrap() as usize]
                    .as_ref()
                    .unwrap(),
            ),
            (
                glyf::Glyph::Simple(..),
                glyf::Glyph::Composite(..),
                glyf::Glyph::Composite(..),
            ),
        ));
    }

    #[test]
    fn compile_composite_glyphs_to_glyf_loca() {
        let result = TestCompile::compile_source("glyphs2/Component.glyphs");
        // non-uniform scaling of period
        let period_idx = result.get_glyph_index("period").unwrap();
        let non_uniform_scale_idx = result.get_glyph_index("non_uniform_scale").unwrap();
        let glyph_data = result.glyphs();
        let glyphs = glyph_data.read();
        let Some(glyf::Glyph::Composite(glyph)) = &glyphs[non_uniform_scale_idx as usize] else {
            panic!("Expected a composite\n{glyphs:#?}");
        };
        let component = glyph.components().next().unwrap();
        assert_eq!(period_idx, component.glyph.to_u16() as u32);

        // If we all work together we're a real transform!
        assert_component_transform(
            &component,
            [0.84519, 0.58921, -1.16109, 1.66553, -233.0, -129.0],
        );
    }

    fn assert_component_transform(component: &glyf::Component, expected_transform: [f32; 6]) {
        let [xx, yx, xy, yy, dx, dy] = expected_transform;
        assert_eq!(
            glyf::Anchor::Offset {
                x: dx as i16,
                y: dy as i16
            },
            component.anchor
        );
        assert_eq!(
            glyf::Transform {
                xx: F2Dot14::from_f32(xx),
                yx: F2Dot14::from_f32(yx),
                xy: F2Dot14::from_f32(xy),
                yy: F2Dot14::from_f32(yy),
            },
            component.transform
        );
    }

    #[test]
    fn compile_composite_glyphs_to_glyf_loca_applies_transforms() {
        let result = TestCompile::compile_source("glyphs2/Component.glyphs");

        let gid = result.get_glyph_index("simple_transform_again").unwrap();
        let glyph_data = result.glyphs();
        let glyphs = glyph_data.read();
        let Some(glyf::Glyph::Composite(glyph)) = &glyphs[gid as usize] else {
            panic!("Expected a composite\n{glyphs:#?}");
        };

        let components: Vec<_> = glyph.components().collect();
        assert_eq!(2, components.len(), "{components:#?}");
        assert_component_transform(&components[0], [2.0, 0.0, 0.0, 1.5, 50.0, 50.0]);
        assert_component_transform(&components[1], [1.5, 0.0, 0.0, 2.0, 50.0, 50.0]);

        // Have ye bbox?
        assert_eq!(
            [425, 125, 800, 250],
            [glyph.x_min(), glyph.y_min(), glyph.x_max(), glyph.y_max()]
        );
    }

    #[test]
    fn eliminate_2x2_transforms() {
        let result = TestCompile::compile("glyphs2/Component.glyphs", |mut args| {
            args.decompose_transformed_components = true;
            args
        });

        let glyph_data = result.glyphs();
        let glyphs = glyph_data.read();

        // Not an identity 2x2, should be simplified
        let Some(glyf::Glyph::Simple(..)) =
            &glyphs[result.get_glyph_index("simple_transform_again").unwrap() as usize]
        else {
            panic!("Expected a simple glyph\n{glyphs:#?}");
        };

        // Identity 2x2, should be left as a component
        let Some(glyf::Glyph::Composite(..)) =
            &glyphs[result.get_glyph_index("translate_only").unwrap() as usize]
        else {
            panic!("Expected a composite glyph\n{glyphs:#?}");
        };
    }

    #[test]
    fn writes_cmap() {
        let result = TestCompile::compile_source("glyphs2/Component.glyphs");

        let raw_cmap = dump_table(&result.be_context.cmap.get().0).unwrap();
        let font_data = FontData::new(&raw_cmap);
        let cmap = Cmap::read(font_data).unwrap();

        assert!(!cmap.encoding_records().is_empty());
        for encoding_record in cmap.encoding_records() {
            let CmapSubtable::Format4(cmap4) = encoding_record.subtable(font_data).unwrap() else {
                panic!("Wrong subtable {encoding_record:#?}");
            };

            // Hopefully we find the codepoints in the .glyphs file, with glyph ids based on order in that file
            let cp_and_gid: Vec<(u16, u16)> = cmap4
                .start_code()
                .iter()
                .zip(cmap4.end_code())
                .zip(cmap4.id_delta())
                .filter_map(|((start, end), id_delta)| {
                    let start = start.get();
                    let end = end.get();
                    if (start, end) == (0xFFFF, 0xFFFF) {
                        return None;
                    }
                    Some((start, end, id_delta.get()))
                })
                .flat_map(|(start, end, id_delta)| {
                    (start..=end)
                        .map(move |cp| (cp, (cp as i32 + id_delta as i32).try_into().unwrap()))
                })
                .collect();
            assert_eq!(
                vec![
                    (0x002C, 2),
                    (0x002E, 1),
                    (0x0030, 3),
                    (0x031, 4),
                    (0x032, 5),
                    (0x033, 6)
                ],
                cp_and_gid,
                "start {:?}\nend {:?}id_delta {:?}",
                cmap4.start_code(),
                cmap4.end_code(),
                cmap4.id_delta()
            );
        }
    }

    #[test]
    fn hmtx_of_one() {
        let result = TestCompile::compile_source("glyphs2/NotDef.glyphs");

        let raw_hmtx = result.be_context.hmtx.get();
        let hmtx = Hmtx::read_with_args(FontData::new(raw_hmtx.get()), &(1, 1)).unwrap();
        assert_eq!(
            vec![(600, 250)],
            hmtx.h_metrics()
                .iter()
                .map(|m| (m.advance.get(), m.side_bearing.get()))
                .collect::<Vec<_>>()
        );
        assert!(hmtx.left_side_bearings().is_empty());
    }

    #[test]
    fn metrics_and_limits_of_mono() {
        let result = TestCompile::compile_source("glyphs2/Mono.glyphs");

        let arc_hhea = result.be_context.hhea.get();
        let Some(hhea) = &arc_hhea.0 else {
            panic!("Missing hhea");
        };
        assert_eq!(1, hhea.number_of_long_metrics);
        assert_eq!(175, hhea.min_left_side_bearing.to_i16());
        assert_eq!(50, hhea.min_right_side_bearing.to_i16());
        assert_eq!(375, hhea.x_max_extent.to_i16());
        assert_eq!(425, hhea.advance_width_max.to_u16());

        let arc_maxp = result.be_context.maxp.get();
        let Some(maxp) = &arc_maxp.0 else {
            panic!("Missing maxp");
        };
        assert_eq!(3, maxp.num_glyphs);
        assert_eq!(Some(4), maxp.max_points);
        assert_eq!(Some(1), maxp.max_contours);

        let raw_hmtx = result.be_context.hmtx.get();
        let hmtx = Hmtx::read_with_args(
            FontData::new(raw_hmtx.get()),
            &(hhea.number_of_long_metrics, maxp.num_glyphs),
        )
        .unwrap();
        assert_eq!(
            vec![(425, 175)],
            hmtx.h_metrics()
                .iter()
                .map(|m| (m.advance.get(), m.side_bearing.get()))
                .collect::<Vec<_>>()
        );
        assert_eq!(
            vec![200, 225],
            hmtx.left_side_bearings()
                .iter()
                .map(|m| m.get())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn wght_var_has_expected_tables() {
        let result = TestCompile::compile_source("wght_var.designspace");
        let font = result.font();

        assert_eq!(
            vec![
                Tag::new(b"GDEF"),
                Tag::new(b"GPOS"),
                Tag::new(b"HVAR"),
                Tag::new(b"MVAR"),
                Tag::new(b"OS/2"),
                Tag::new(b"STAT"),
                Tag::new(b"cmap"),
                Tag::new(b"fvar"),
                Tag::new(b"glyf"),
                Tag::new(b"gvar"),
                Tag::new(b"head"),
                Tag::new(b"hhea"),
                Tag::new(b"hmtx"),
                Tag::new(b"loca"),
                Tag::new(b"maxp"),
                Tag::new(b"name"),
                Tag::new(b"post"),
            ],
            font.table_directory
                .table_records()
                .iter()
                .map(|tr| tr.tag())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn compile_mov_xy_and_move_around() {
        let result = TestCompile::compile_source("mov_xy.designspace");
        let font = result.font();
        // Confirm movx then movy
        assert_eq!(
            vec!["movx", "movy"],
            font.fvar()
                .unwrap()
                .axes()
                .unwrap()
                .iter()
                .map(|a| a.axis_tag.get().to_string())
                .collect::<Vec<_>>()
        );

        // Confirm movement on axes has the intended effect
        assert_eq!(
            vec![
                Rect::new(50.0, 50.0, 150.0, 150.0),   // default
                Rect::new(850.0, 50.0, 950.0, 150.0),  // movx max
                Rect::new(50.0, 850.0, 150.0, 950.0),  // movy max
                Rect::new(850.0, 850.0, 950.0, 950.0), // movx and movy max
            ],
            vec![
                cbox_of_char(0x2e, &font, vec![0.0, 0.0]),
                cbox_of_char(0x2e, &font, vec![1.0, 0.0]),
                cbox_of_char(0x2e, &font, vec![0.0, 1.0]),
                cbox_of_char(0x2e, &font, vec![1.0, 1.0]),
            ]
        )
    }

    fn cbox_of_char(ch: u32, font: &FontRef, coords: Vec<f32>) -> Rect {
        let gid = Charmap::new(font).map(ch).unwrap();

        let mut bp = CboxPen::new();
        let coords = coords
            .into_iter()
            .map(F2Dot14::from_f32)
            .collect::<Vec<_>>();
        font.outline_glyphs()
            .get(gid)
            .unwrap()
            .draw(
                DrawSettings::unhinted(Size::new(1000.0), coords.as_slice()),
                &mut bp,
            )
            .unwrap();
        bp.cbox().unwrap()
    }

    struct CboxPen {
        last_move: Point,
        bounds: Option<Rect>,
    }

    impl CboxPen {
        fn new() -> Self {
            Self {
                last_move: Default::default(),
                bounds: None,
            }
        }

        fn cbox(self) -> Option<Rect> {
            self.bounds
        }

        fn update_bounds(&mut self, x: f32, y: f32) {
            let pt = Point::new(x as f64, y as f64);
            self.bounds = Some(match self.bounds {
                None => Rect::from_points(pt, pt),
                Some(bounds) => bounds.union_pt(pt),
            });
        }
    }

    impl skrifa::outline::OutlinePen for CboxPen {
        fn move_to(&mut self, x: f32, y: f32) {
            self.last_move = Point::new(x as f64, y as f64);
            self.update_bounds(x, y);
        }

        fn line_to(&mut self, x: f32, y: f32) {
            self.update_bounds(x, y);
        }

        fn quad_to(&mut self, cx0: f32, cy0: f32, x: f32, y: f32) {
            self.update_bounds(cx0, cy0);
            self.update_bounds(x, y);
        }

        fn curve_to(&mut self, cx0: f32, cy0: f32, cx1: f32, cy1: f32, x: f32, y: f32) {
            self.update_bounds(cx0, cy0);
            self.update_bounds(cx1, cy1);
            self.update_bounds(x, y);
        }

        fn close(&mut self) {
            // nop
        }
    }

    fn axes(font: &FontRef) -> Vec<(Tag, f32, f32, f32)> {
        font.fvar()
            .unwrap()
            .axes()
            .unwrap()
            .iter()
            .map(|a| {
                (
                    a.axis_tag(),
                    a.min_value.get().to_f64() as f32,
                    a.default_value.get().to_f64() as f32,
                    a.max_value.get().to_f64() as f32,
                )
            })
            .collect::<Vec<_>>()
    }

    #[test]
    fn compile_generates_notdef() {
        let result = TestCompile::compile_source("glyphs2/WghtVar.glyphs");

        assert!(!result
            .fe_context
            .preliminary_glyph_order
            .get()
            .contains(&GlyphName::NOTDEF));
        assert_eq!(
            Some(GlyphId::NOTDEF),
            result
                .fe_context
                .glyph_order
                .get()
                .glyph_id(&GlyphName::NOTDEF)
        );

        let font = result.font();

        // Character 0x0000 (NULL) != '.notdef' glyph, and neither are any other
        // characters actually, because '.notdef' (glyph index 0) means the absence
        // of a character-to-glyph mapping:
        // https://github.com/googlefonts/fontc/pull/423/files#r1309257127
        // https://learn.microsoft.com/en-us/typography/opentype/spec/cmap#overview
        assert_eq!(None, font.cmap().unwrap().map_codepoint(0u32));
    }

    #[test]
    fn compile_glyphs_font_with_weight_axis() {
        let result = TestCompile::compile_source("glyphs2/WghtVar.glyphs");
        let font = result.font();

        assert_eq!(
            vec![(Tag::from_str("wght").unwrap(), 400.0, 400.0, 700.0)],
            axes(&font),
        );

        assert_eq!(
            vec![
                GlyphId::new(1),
                GlyphId::new(2),
                GlyphId::new(3),
                GlyphId::new(6),
            ],
            [0x20, 0x21, 0x2d, 0x3d]
                .iter()
                .map(|cp| font.cmap().unwrap().map_codepoint(*cp as u32).unwrap())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn compile_glyphs_font_with_avar() {
        let result = TestCompile::compile_source("glyphs2/WghtVar_Avar.glyphs");
        let font = result.font();
        // Default 400 is important, it means we found the index of Regular
        assert_eq!(
            vec![(Tag::from_str("wght").unwrap(), 300.0, 400.0, 700.0)],
            axes(&font),
        );

        let axis_maps: Vec<_> = font
            .avar()
            .unwrap()
            .axis_segment_maps()
            .iter()
            .map(|m| m.unwrap())
            .map(|m| {
                m.axis_value_maps()
                    .iter()
                    .map(|e| (e.from_coordinate().to_f32(), e.to_coordinate().to_f32()))
                    .collect::<Vec<_>>()
            })
            .collect();

        // 0 is 400, 1 is 700, .33 => .66 is mapping 500 => 600 as per instance definition
        assert_eq!(
            vec![vec![
                (-1.0, -1.0),
                (0.0, 0.0),
                (0.333313, 0.666687),
                (1.0, 1.0)
            ]],
            axis_maps
        );
    }

    #[test]
    fn compile_without_ir() {
        let result = TestCompile::compile("glyphs2/WghtVar.glyphs", |mut args| {
            args.incremental = false;
            args
        });

        let outputs = fs::read_dir(&result.build_dir)
            .unwrap()
            .map(|e| {
                e.unwrap()
                    .path()
                    .strip_prefix(&result.build_dir)
                    .unwrap()
                    .to_owned()
            })
            .collect::<Vec<_>>();
        assert_eq!(vec![Path::new("font.ttf")], outputs);
    }

    fn resolve_name(name: &Name, id: NameId) -> Option<String> {
        name.name_record().iter().find_map(|nr| {
            (nr.name_id() == id).then(|| {
                nr.string(name.string_data())
                    .unwrap()
                    .chars()
                    .collect::<String>()
            })
        })
    }

    fn assert_named_instances(source: &str, expected: Vec<(String, Vec<(&str, f32)>)>) {
        let result = TestCompile::compile_source(source);
        let font = result.font();

        let name = font.name().unwrap();
        let fvar = font.fvar().unwrap();
        let axis_names: Vec<_> = fvar
            .axes()
            .unwrap()
            .iter()
            .map(|axis| resolve_name(&name, axis.axis_name_id()).unwrap())
            .collect();
        let instances = fvar.instances().unwrap();

        assert_eq!(
            expected,
            instances
                .iter()
                .map(|i| i.unwrap())
                .map(|inst| (
                    resolve_name(&name, inst.subfamily_name_id)
                        .unwrap_or_else(|| format!("{:?}", inst.subfamily_name_id)),
                    inst.coordinates
                        .iter()
                        .enumerate()
                        .map(|(i, coord)| { (axis_names[i].as_str(), coord.get().to_f64() as f32) })
                        .collect(),
                ))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn compile_named_instances_from_glyphs() {
        assert_named_instances(
            "glyphs3/WghtVar_Instances.glyphs",
            vec![
                ("Regular".to_string(), vec![("Weight", 400.0)]),
                ("Bold".to_string(), vec![("Weight", 700.0)]),
            ],
        );
    }

    #[test]
    fn compile_named_instances_from_designspace() {
        assert_named_instances(
            "wght_var.designspace",
            vec![
                ("Regular".to_string(), vec![("Weight", 400.0)]),
                ("Bold".to_string(), vec![("Weight", 700.0)]),
            ],
        );
    }

    #[test]
    fn compile_named_instances_from_designspace_with_stylename_attrs() {
        assert_named_instances(
            "wght_var_instance_stylename.designspace",
            vec![
                ("Foo".to_string(), vec![("Weight", 400.0)]),
                ("Bar".to_string(), vec![("Weight", 700.0)]),
            ],
        );
    }

    fn assert_fs_selection(source: &str, expected: SelectionFlags) {
        let result = TestCompile::compile_source(source);
        let font = result.font();
        let os2 = font.os2().unwrap();
        assert_eq!(expected, os2.fs_selection());
    }

    #[test]
    fn wght_var_fs_selection() {
        assert_fs_selection(
            "glyphs3/WghtVar.glyphs",
            SelectionFlags::REGULAR | SelectionFlags::USE_TYPO_METRICS | SelectionFlags::WWS,
        );
    }

    #[test]
    fn static_bold_italic_fs_selection() {
        assert_fs_selection(
            "glyphs3/StaticBoldItalic.glyphs",
            SelectionFlags::BOLD | SelectionFlags::ITALIC,
        );
    }

    #[test]
    fn populates_unicode_range() {
        let result = TestCompile::compile_source("static.designspace");
        let font = result.font();
        let os2 = font.os2().unwrap();

        assert_eq!(
            (1, 1 << 6, 0, 0),
            (
                os2.ul_unicode_range_1(),
                os2.ul_unicode_range_2(),
                os2.ul_unicode_range_3(),
                os2.ul_unicode_range_4()
            ),
            "Should set bit 1, Latin-1 Supplement and bit 38, Math Operators",
        );
    }

    #[test]
    fn glyphs_app_ir_categories() {
        let result = TestCompile::compile_source("glyphs3/Oswald-glyph-categories.glyphs");
        let staticmeta = result.fe_context.static_metadata.get();
        let categories = &staticmeta.gdef_categories.categories;
        assert_eq!(categories.get("a"), Some(&GlyphClassDef::Base));
        assert_eq!(categories.get("acutecomb"), Some(&GlyphClassDef::Mark));
        assert_eq!(categories.get("brevecomb"), Some(&GlyphClassDef::Mark));
        assert_eq!(
            categories.get("brevecomb_acutecomb"),
            Some(&GlyphClassDef::Mark)
        );
        assert_eq!(categories.get("f_a"), Some(&GlyphClassDef::Ligature));
        assert_eq!(categories.get(".notdef"), None);
    }

    #[test]
    fn ufo_app_ir_categories_matches_glyphs() {
        let glyphs = TestCompile::compile_source("glyphs3/Oswald-glyph-categories.glyphs");
        let ufo = TestCompile::compile_source("Oswald-glyph-categories/Oswald-Regular.designspace");

        let gmeta = glyphs.fe_context.static_metadata.get();
        let ufometa = ufo.fe_context.static_metadata.get();

        assert_eq!(
            gmeta.gdef_categories.categories,
            ufometa.gdef_categories.categories
        )
    }

    #[test]
    fn ufo_and_glyphs_set_prefer_fea_flag() {
        let glyphs = TestCompile::compile_source("glyphs3/Oswald-glyph-categories.glyphs");
        let ufo = TestCompile::compile_source("Oswald-glyph-categories/Oswald-Regular.designspace");

        let gmeta = glyphs.fe_context.static_metadata.get();
        let ufometa = ufo.fe_context.static_metadata.get();
        assert!(!gmeta.gdef_categories.prefer_gdef_categories_in_fea);
        assert!(ufometa.gdef_categories.prefer_gdef_categories_in_fea);
    }

    #[test]
    fn captures_vendor_id() {
        let result = TestCompile::compile_source("glyphs3/TheBestNames.glyphs");
        let font = result.font();

        let os2 = font.os2().unwrap();
        assert_eq!(Tag::new(b"RODS"), os2.ach_vend_id());
    }

    fn assert_maxp_composite(
        src: &str,
        max_component_depth: u16,
        max_composite_points: u16,
        max_composite_contours: u16,
    ) {
        let result = TestCompile::compile_source(src);
        let font = result.font();
        let maxp = font.maxp().unwrap();

        assert_eq!(
            (
                max_component_depth,
                max_composite_points,
                max_composite_contours
            ),
            (
                maxp.max_component_depth().unwrap(),
                maxp.max_composite_points().unwrap(),
                maxp.max_composite_contours().unwrap()
            )
        );
    }

    #[test]
    fn maxp_with_no_components() {
        assert_maxp_composite("glyphs3/StaticBoldItalic.glyphs", 0, 0, 0);
    }

    #[test]
    fn maxp_with_shallow_components() {
        assert_maxp_composite("glyphs2/Component.glyphs", 1, 4, 1);
    }

    #[test]
    fn maxp_with_deep_components() {
        assert_maxp_composite("glyphs3/NestedComponent.glyphs", 3, 12, 3);
    }

    fn assert_created_set(source: &str) {
        let result = TestCompile::compile_source(source);
        let font = result.font();
        let head = font.head().unwrap();

        // TIL Mac has an epoch of it's own
        let mac_epoch = Utc.with_ymd_and_hms(1904, 1, 1, 0, 0, 0).unwrap();
        let created = mac_epoch
            .checked_add_signed(Duration::try_seconds(head.created().as_secs()).unwrap())
            .unwrap();

        // We should match 2023-05-05 15:11:55 +0000
        assert_eq!(
            Utc.with_ymd_and_hms(2023, 5, 5, 15, 11, 55).unwrap(),
            created
        );
    }

    #[test]
    fn captures_created_from_glyphs() {
        assert_created_set("glyphs3/Dated.glyphs");
    }

    #[test]
    fn captures_created_from_designspace() {
        assert_created_set("wght_var.designspace");
    }

    #[test]
    fn generates_stat() {
        let result = TestCompile::compile_source("wght_var.designspace");
        let font = result.font();

        let name = font.name().unwrap();
        let stat = font.stat().unwrap();
        assert_eq!(
            ("Regular", vec![Tag::from_str("wght").unwrap(),]),
            (
                resolve_name(&name, stat.elided_fallback_name_id().unwrap())
                    .unwrap()
                    .as_str(),
                stat.design_axes()
                    .unwrap()
                    .iter()
                    .map(|ar| ar.axis_tag())
                    .collect::<Vec<_>>(),
            )
        );
    }

    fn assert_simple_kerning(source: &str) {
        let result = TestCompile::compile_source(source);

        let kerning_groups = result.fe_context.kerning_groups.get();

        let mut groups: Vec<_> = kerning_groups
            .groups
            .iter()
            .map(|(name, entries)| {
                let mut entries: Vec<_> = entries.iter().map(|e| e.as_str()).collect();
                entries.sort();
                (name.to_owned(), entries)
            })
            .collect();
        groups.sort();

        let wght = Tag::new(b"wght");
        let mut kerns: HashMap<KernPair, Vec<(String, f32)>> = HashMap::new();
        for kern_loc in kerning_groups.locations.iter() {
            assert_eq!(
                vec![wght],
                kern_loc.axis_tags().cloned().collect::<Vec<_>>()
            );

            let kerns_at = result
                .fe_context
                .kerning_at
                .get(&FeWorkIdentifier::KernInstance(kern_loc.clone()));
            for (pair, adjustment) in kerns_at.kerns.iter() {
                kerns.entry(pair.clone()).or_default().push((
                    format!(
                        "wght {}",
                        kern_loc.iter().map(|(_, v)| *v).next().unwrap().to_f32()
                    ),
                    adjustment.0,
                ));
            }
        }
        let mut kerns: Vec<_> = kerns
            .into_iter()
            .map(|((left, right), mut values)| {
                values.sort_by_key(|(s, _)| s.clone());
                (left, right, values)
            })
            .collect();
        kerns.sort_by_key(|(left, right, _)| (left.clone(), right.clone()));

        assert_eq!(
            (groups, kerns),
            (
                vec![
                    (
                        KernGroup::Side1("bracketleft_R".into()),
                        vec!["bracketleft"],
                    ),
                    (
                        KernGroup::Side1("bracketright_R".into()),
                        vec!["bracketright"],
                    ),
                    (
                        KernGroup::Side2("bracketleft_L".into()),
                        vec!["bracketleft"],
                    ),
                    (
                        KernGroup::Side2("bracketright_L".into()),
                        vec!["bracketright"],
                    ),
                ],
                vec![
                    (
                        KernSide::Glyph("bracketleft".into()),
                        KernSide::Glyph("bracketright".into()),
                        vec![
                            ("wght 0".to_string(), -300.0),
                            ("wght 1".to_string(), -150.0)
                        ],
                    ),
                    (
                        KernSide::Glyph("exclam".into()),
                        KernSide::Glyph("exclam".into()),
                        vec![
                            ("wght 0".to_string(), -360.0),
                            ("wght 1".to_string(), -100.0)
                        ],
                    ),
                    (
                        KernSide::Glyph("exclam".into()),
                        KernSide::Glyph("hyphen".into()),
                        vec![("wght 0".to_string(), 20.0),],
                    ),
                    (
                        KernSide::Glyph("exclam".into()),
                        KernSide::Group(KernGroup::Side2("bracketright_L".into())),
                        vec![("wght 0".to_string(), -160.0),],
                    ),
                    (
                        KernSide::Glyph("hyphen".into()),
                        KernSide::Glyph("hyphen".into()),
                        vec![
                            ("wght 0".to_string(), -150.0),
                            ("wght 1".to_string(), -50.0)
                        ],
                    ),
                    (
                        KernSide::Group(KernGroup::Side1("bracketleft_R".into())),
                        KernSide::Glyph("exclam".into()),
                        vec![("wght 0".to_string(), -165.0),],
                    ),
                ],
            )
        );
    }

    #[test]
    fn kerning_from_glyphs() {
        assert_simple_kerning("glyphs3/WghtVar.glyphs");
    }

    #[test]
    fn kerning_from_ufo() {
        assert_simple_kerning("designspace_from_glyphs/WghtVar.designspace");
    }

    fn assert_intermediate_layer(src: &str) {
        let result = TestCompile::compile_source(src);
        let font = result.font();

        assert_eq!(
            vec!["CPHT", "wght"],
            font.fvar()
                .unwrap()
                .axes()
                .unwrap()
                .iter()
                .map(|a| a.axis_tag().to_string())
                .collect::<Vec<_>>()
        );

        // Confirm movement on the wght axis is as expected:
        // glyph "i" (char 0x69) defines three masters at wght=400, 700 and 900
        assert_eq!(
            vec![
                Rect::new(239.0, 0.0, 364.0, 705.0),
                Rect::new(213.0, 0.0, 388.0, 737.0),
                Rect::new(191.0, 0.0, 410.0, 761.0),
            ],
            vec![
                cbox_of_char(0x69, &font, vec![0.0, 0.0]), // CPHT=700, wght=400
                cbox_of_char(0x69, &font, vec![0.0, 0.6]), // CPHT=700, wght=700
                cbox_of_char(0x69, &font, vec![0.0, 1.0]), // CPHT=700, wght=900
            ]
        );

        // glyph "I" (char 0x49) only defines two masters along wght at 400 and 900
        // (and default CPHT=700) so wght=700 should be interpolated between them.
        assert_eq!(
            vec![
                Rect::new(231.0, 0.0, 364.0, 700.0),
                Rect::new(195.0, 0.0, 400.0, 700.0),
                Rect::new(171.0, 0.0, 424.0, 700.0),
            ],
            vec![
                cbox_of_char(0x49, &font, vec![0.0, 0.0]), // CPHT=700, wght=400
                cbox_of_char(0x49, &font, vec![0.0, 0.6]), // CPHT=700, wght=700
                cbox_of_char(0x49, &font, vec![0.0, 1.0]), // CPHT=700, wght=900
            ]
        );

        // glyph "I" also defines two additional ('virtual') masters along CPHT at 600 and 800
        // (and default wght=400), check that the cap-height (bbox.yMax) matches that throughout
        // the wght axis.
        assert_eq!(
            vec![
                Rect::new(231.0, 0.0, 364.0, 600.0),
                Rect::new(171.0, 0.0, 424.0, 600.0),
                Rect::new(231.0, 0.0, 364.0, 800.0),
                Rect::new(171.0, 0.0, 424.0, 800.0),
            ],
            vec![
                cbox_of_char(0x49, &font, vec![-1.0, 0.0]), // CPHT=600, wght=400
                cbox_of_char(0x49, &font, vec![-1.0, 1.0]), // CPHT=600, wght=900
                cbox_of_char(0x49, &font, vec![1.0, 0.0]),  // CPHT=800, wght=400
                cbox_of_char(0x49, &font, vec![1.0, 1.0]),  // CPHT=800, wght=900
            ]
        )
    }

    #[test]
    fn intermediate_layer_in_glyphs2() {
        // https://github.com/googlefonts/fontc/issues/62
        assert_intermediate_layer("glyphs2/IntermediateLayer.glyphs");
    }

    #[test]
    fn intermediate_layer_in_glyphs3() {
        // https://github.com/googlefonts/fontc/issues/62
        assert_intermediate_layer("glyphs3/IntermediateLayer.glyphs");
    }

    #[test]
    fn intermediate_layer_in_designspace() {
        // https://github.com/googlefonts/fontc/issues/400
        assert_intermediate_layer("designspace_from_glyphs/IntermediateLayer.designspace");
    }

    #[test]
    fn empty_glyph_is_zero_length() {
        let result = TestCompile::compile_source("glyphs2/WghtVar.glyphs");
        let space_idx = result.get_glyph_index("space").unwrap() as usize;
        let font = result.font();

        let is_long = match font.head().unwrap().index_to_loc_format() {
            0 => false,
            1 => true,
            _ => panic!("Unrecognized loca format"),
        };
        let loca = font.loca(is_long).unwrap();
        assert_eq!(
            loca.get_raw(space_idx).unwrap(),
            loca.get_raw(space_idx + 1).unwrap(),
            "space has no outline and should take 0 bytes of glyf"
        );
    }

    fn assert_no_compile_features(source: &str, table_tags: &[&[u8; 4]]) {
        // default is to skip_features=false
        let default_result = TestCompile::compile_source(source);

        let nofea_result = TestCompile::compile(source, |mut args| {
            args.skip_features = true;
            args
        });

        let font_with_fea = default_result.font();
        let font_without_fea = nofea_result.font();

        for table_tag in table_tags {
            let tag = Tag::new(table_tag);
            assert!(
                font_with_fea.data_for_tag(tag).is_some(),
                "Expected font built from {source} with fea to have {tag}"
            );
            assert!(
                font_without_fea.data_for_tag(tag).is_none(),
                "Expected font build from {source} without fea to not have {tag}"
            );
        }
    }

    #[test]
    fn no_compile_features_from_glyphs_2() {
        assert_no_compile_features("glyphs2/WghtVar.glyphs", &[b"GDEF", b"GPOS"]);
    }

    #[test]
    fn no_compile_features_from_glyphs_3() {
        assert_no_compile_features("glyphs3/WghtVar.glyphs", &[b"GDEF", b"GPOS"]);
    }

    #[test]
    fn no_compile_features_from_ufo() {
        assert_no_compile_features(
            "designspace_from_glyphs/WghtVar.designspace",
            &[b"GDEF", b"GPOS"],
        );
        assert_no_compile_features("static.designspace", &[b"GPOS", b"GSUB"]);
    }

    #[test]
    fn compile_simple_glyph_keep_direction() {
        let source = "glyphs3/WghtVar.glyphs";

        // first compile with default args (keep_direction=false)
        let default_result = TestCompile::compile_source(source);

        let ir_glyph = read_ir_glyph(&default_result.build_dir, "hyphen");
        let ir_default_instance = ir_glyph.default_instance();
        assert_eq!(
            &ir_default_instance.contours[0].to_svg(),
            "M131,330 L131,250 L470,250 L470,330 L131,330 Z"
        );

        let RawGlyph::Simple(glyph) = read_be_glyph(&default_result.build_dir, "hyphen") else {
            panic!("Expected a simple glyph");
        };

        // point order in the compiled glyph is reversed compared to the IR glyph
        // while the starting point is the same
        assert_eq!(
            vec![
                CurvePoint::on_curve(131, 330),
                CurvePoint::on_curve(470, 330),
                CurvePoint::on_curve(470, 250),
                CurvePoint::on_curve(131, 250),
            ],
            glyph
                .contours()
                .iter()
                .flat_map(|c| c.iter())
                .copied()
                .collect::<Vec<_>>()
        );
        drop(default_result); // let's not use that by mistake

        // recompile with --keep-direction
        let keep_direction_result = TestCompile::compile(source, |mut args| {
            args.keep_direction = true;
            args
        });

        let RawGlyph::Simple(glyph) = read_be_glyph(&keep_direction_result.build_dir, "hyphen")
        else {
            panic!("Expected a simple glyph");
        };

        // order/starting point in the compiled glyph are the same as in the IR glyph
        assert_eq!(
            vec![
                CurvePoint::on_curve(131, 330),
                CurvePoint::on_curve(131, 250),
                CurvePoint::on_curve(470, 250),
                CurvePoint::on_curve(470, 330),
            ],
            glyph
                .contours()
                .iter()
                .flat_map(|c| c.iter())
                .copied()
                .collect::<Vec<_>>()
        );
    }

    fn delta_sets(var_data: &ItemVariationData) -> Vec<Vec<i32>> {
        (0..var_data.item_count())
            .map(|i| var_data.delta_set(i).collect::<Vec<_>>())
            .collect()
    }

    #[test]
    fn compile_hvar_single_model_direct_varstore() {
        // All glyphs define the same locations (single model is ok) so a direct
        // VarStore (without a mapping) can be built. In this particular case, this
        // turns out to be more compact than the equivalent indirect VarStore
        // so we check it gets preferred over the latter.
        let result = TestCompile::compile_source(
            "HVAR/SingleModel_Direct/HVARSingleModelDirect.designspace",
        );
        let font = result.font();
        let num_glyphs = font.maxp().unwrap().num_glyphs();
        assert_eq!(num_glyphs, 14);
        let hvar = font.hvar().unwrap();
        assert!(hvar.advance_width_mapping().is_none());
        let varstore = hvar.item_variation_store().unwrap();
        let regions = varstore
            .variation_region_list()
            .unwrap()
            .variation_regions();
        // this simple test font only has two masters (Regular [default] and Bold)
        // so we expect a single region with a single wght axis
        assert_eq!(regions.len(), 1);
        let region_coords = regions
            .get(0)
            .unwrap()
            .region_axes()
            .iter()
            .map(|coords| {
                [
                    coords.start_coord().to_f32(),
                    coords.peak_coord().to_f32(),
                    coords.end_coord().to_f32(),
                ]
            })
            .collect::<Vec<_>>();
        assert_eq!(region_coords, vec![[0.0, 1.0, 1.0]]);
        // we expect one ItemVariationData and one delta set per glyph
        assert_eq!(varstore.item_variation_data_count(), 1, "{varstore:#?}");
        let vardata = varstore.item_variation_data().get(0).unwrap().unwrap();
        assert_eq!(vardata.region_indexes(), &[0]);
        assert_eq!(
            vec![
                vec![0],
                vec![100],
                vec![120],
                vec![70],
                vec![100],
                vec![120],
                vec![120],
                vec![70],
                vec![70],
                vec![120],
                vec![70],
                vec![0],
                vec![0],
                vec![0],
            ],
            delta_sets(&vardata)
        );
    }

    #[test]
    fn compile_hvar_single_model_indirect_varstore() {
        // This is the same as the previous font but with additional 10 glyphs that
        // share the same deltas (the number was found empirically), enough to tip
        // the balance and make an indirect store more compact despite the overhead
        // of the additional DeltaSetIndexMap.
        let result = TestCompile::compile_source(
            "HVAR/SingleModel_Indirect/HVARSingleModelIndirect.designspace",
        );
        let font = result.font();
        let num_glyphs = font.maxp().unwrap().num_glyphs();
        assert_eq!(num_glyphs, 24);
        let hvar = font.hvar().unwrap();
        let varstore = hvar.item_variation_store().unwrap();
        assert_eq!(
            varstore
                .variation_region_list()
                .unwrap()
                .variation_regions()
                .len(),
            1
        );
        assert_eq!(varstore.item_variation_data_count(), 1);
        let vardata = varstore.item_variation_data().get(0).unwrap().unwrap();
        assert_eq!(vardata.region_indexes(), &[0]);
        assert_eq!(
            vec![vec![0], vec![70], vec![100], vec![120],],
            delta_sets(&vardata)
        );
        let Some(Ok(DeltaSetIndexMap::Format0(varidx_map))) = hvar.advance_width_mapping() else {
            panic!("Expected advance width mapping with DeltaSetIndexMap::Format0");
        };
        // since the last 10 glyphs have the same advance width deltas and share the same varidx,
        // the mapping omits the trailing duplicate entries
        assert_eq!(varidx_map.map_count(), num_glyphs - 10 + 1);
    }

    struct HvarReader<'a> {
        hvar: write_fonts::read::tables::hvar::Hvar<'a>,
        glyph_order: Arc<GlyphOrder>,
    }

    impl<'a> HvarReader<'a> {
        fn new(
            hvar: write_fonts::read::tables::hvar::Hvar<'a>,
            glyph_order: Arc<GlyphOrder>,
        ) -> Self {
            Self { hvar, glyph_order }
        }

        fn width_delta(&self, name: &str, coords: &[NormalizedCoord]) -> f64 {
            let name = GlyphName::from(name);
            let gid = self.glyph_order.glyph_id(&name).unwrap();
            let coords: Vec<F2Dot14> = coords
                .iter()
                .map(|coord| F2Dot14::from_f32(coord.into_inner().into()))
                .collect();
            self.hvar
                .advance_width_delta(gid, &coords)
                .unwrap()
                .to_f64()
        }
    }

    #[test]
    fn compile_hvar_multi_model_indirect_varstore() {
        // Some glyphs are 'sparse' and define different sets of locations, so multiple
        // sub-models are required to compute the advance width deltas.
        // FontTools always builds an indirect VarStore in this case and we do the same.
        let result = TestCompile::compile_source(
            "HVAR/MultiModel_Indirect/HVARMultiModelIndirect.designspace",
        );
        let font = result.font();
        let num_glyphs = font.maxp().unwrap().num_glyphs();
        assert_eq!(num_glyphs, 5);
        let hvar = font.hvar().unwrap();
        let varstore = hvar.item_variation_store().unwrap();
        let region_coords = varstore
            .variation_region_list()
            .unwrap()
            .variation_regions()
            .iter()
            .map(|region| {
                region
                    .unwrap()
                    .region_axes()
                    .iter()
                    .map(|coords| {
                        [
                            coords.start_coord().to_f32(),
                            coords.peak_coord().to_f32(),
                            coords.end_coord().to_f32(),
                        ]
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        // Glyph "A" defines three masters (Regular, Medium and Bold) so uses two
        // regions (the first two in the list); the rest of the glyphs only have
        // two masters so use only one region (the last one).
        assert_eq!(
            region_coords,
            vec![
                vec![[0.0, 0.333313, 1.0]],
                vec![[0.333313, 1.0, 1.0]],
                vec![[0.0, 1.0, 1.0]],
            ]
        );
        // in this particular test font, despite being multi-model, we still end up with
        // a single VarData subtable, filled with zeros for regions that don't apply
        assert_eq!(varstore.item_variation_data_count(), 1);
        let vardata = varstore.item_variation_data().get(0).unwrap().unwrap();
        assert_eq!(vardata.region_indexes(), &[0, 1, 2]);
        // 4 rows insted of 5 because gid1 and gid4 happen to share the same varidx
        assert_eq!(vardata.item_count(), 4);
        let Some(Ok(varidx_map)) = hvar.advance_width_mapping() else {
            panic!("Expected advance width mapping");
        };
        assert_eq!(
            (0..num_glyphs)
                .map(|gid| {
                    let varidx = varidx_map.get(gid as u32).unwrap();
                    assert_eq!(varidx.outer, 0);
                    varidx.inner
                })
                .collect::<Vec<_>>(),
            vec![0, 2, 3, 1, 2],
        );
        let hvar = HvarReader::new(hvar, result.fe_context.glyph_order.get());
        // .notdef is not variable
        assert_eq!(
            hvar.width_delta(".notdef", &[NormalizedCoord::new(0.0)]),
            0.0
        );
        // 'space' has two masters, with Bold 100 units wider than Regular
        assert_eq!(
            hvar.width_delta("space", &[NormalizedCoord::new(1.0)]),
            100.0
        );
        // ... so it will be exactly 50 units wider half-way in between
        assert_eq!(
            hvar.width_delta("space", &[NormalizedCoord::new(0.5)]),
            50.0
        );
        // 'A' is 120 units wider than Regular in the Bold master
        assert_eq!(hvar.width_delta("A", &[NormalizedCoord::new(1.0)]), 120.0);
        // ... but also has an additional Medium (wght=500, normalized 0.3333) master with
        // a delta of 70; so at normalized 0.5 (wght=550), its delta will *not* be 60
        assert_eq!(hvar.width_delta("A", &[NormalizedCoord::new(0.5)]), 83.0);
    }

    fn anchor_coords(at: AnchorTable) -> (i32, i32) {
        match at {
            AnchorTable::Format1(at) => (at.x_coordinate() as i32, at.y_coordinate() as i32),
            AnchorTable::Format2(at) => (at.x_coordinate() as i32, at.y_coordinate() as i32),
            AnchorTable::Format3(at) => (at.x_coordinate() as i32, at.y_coordinate() as i32),
        }
    }

    fn mark_base_lookups<'a>(gpos: &'a Gpos) -> Vec<TableRef<'a, MarkBasePosFormat1Marker>> {
        // If only we had more indirections
        gpos.lookup_list()
            .iter()
            .flat_map(|l| l.lookups().iter().map(|l| l.unwrap()))
            .filter_map(|l| match l {
                PositionLookup::MarkToBase(mark_base) => Some(mark_base),
                _ => None,
            })
            .flat_map(|mb| mb.subtables().iter().map(|s| s.unwrap()))
            .collect()
    }

    #[test]
    fn compile_basic_gpos_mark_base() {
        let result = TestCompile::compile_source("glyphs3/WghtVar_Anchors.glyphs");
        let font = result.font();
        let gpos = font.gpos().unwrap();

        let base_gid = result.get_gid("A");
        let macroncomb_gid = result.get_gid("macroncomb");
        let brevecomb_gid = result.get_gid("brevecomb");

        // If only we had more indirections
        let mark_base_lookups = mark_base_lookups(&gpos);

        let bases = mark_base_lookups
            .iter()
            .flat_map(|mb| mb.base_coverage().unwrap().iter().collect::<Vec<_>>())
            .zip(
                mark_base_lookups
                    .iter()
                    .flat_map(|mb| {
                        let base_array = mb.base_array().unwrap();
                        let data = base_array.offset_data();
                        base_array
                            .base_records()
                            .iter()
                            .map(move |r| (data, r.unwrap()))
                    })
                    .flat_map(|(data, b)| b.base_anchors(data).iter().map(|b| b.unwrap().unwrap()))
                    .map(anchor_coords),
            )
            .collect::<Vec<_>>();

        let marks = mark_base_lookups
            .iter()
            .flat_map(|mb| mb.mark_coverage().unwrap().iter().collect::<Vec<_>>())
            .zip(
                mark_base_lookups
                    .iter()
                    .flat_map(|mb| {
                        let mark_array = mb.mark_array().unwrap();
                        let data = mark_array.offset_data();
                        mb.mark_array()
                            .unwrap()
                            .mark_records()
                            .iter()
                            .map(move |mr| (data, mr))
                    })
                    .map(|(data, mr)| {
                        (
                            mr.mark_class(),
                            anchor_coords(mr.mark_anchor(data).unwrap()),
                        )
                    }),
            )
            .collect::<Vec<_>>();

        assert_eq!(
            (
                vec![(base_gid, (300, 700))],
                vec![
                    // (glyph id, (mark class, (anchor x, anchor y)))
                    (macroncomb_gid, (0, (300, 600))),
                    (brevecomb_gid, (0, (200, 500)))
                ]
            ),
            (bases, marks)
        );
    }

    #[test]
    fn compile_mark_mark() {
        let result = TestCompile::compile_source("glyphs3/Oswald-AE-comb.glyphs");
        let font = result.font();
        let gpos = font.gpos().unwrap();
        let acutecomb = result.get_gid("acutecomb");
        let brevecomb = result.get_gid("brevecomb");
        let tildecomb = result.get_gid("tildecomb");
        let macroncomb = result.get_gid("macroncomb");

        let mark_mark_lookups = gpos
            .lookup_list()
            .unwrap()
            .lookups()
            .iter()
            .filter_map(|lookup| match lookup {
                Ok(PositionLookup::MarkToMark(lookup)) => Some(lookup),
                _ => None,
            })
            .flat_map(|lookup| lookup.subtables().iter())
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(mark_mark_lookups.len(), 1);

        let mut bases = Vec::new();
        let mut marks = Vec::new();
        for sub in &mark_mark_lookups {
            let mark_cov = sub.mark1_coverage().unwrap();
            let base_cov = sub.mark2_coverage().unwrap();
            let mark1array = sub.mark1_array().unwrap();
            let mark2array = sub.mark2_array().unwrap();

            assert_eq!(mark_cov.iter().count(), mark1array.mark_count() as usize);
            assert_eq!(base_cov.iter().count(), mark2array.mark2_count() as usize);

            for (gid, rec) in mark_cov.iter().zip(mark1array.mark_records().iter()) {
                let mark_class = rec.mark_class();
                let anchor = rec.mark_anchor(mark1array.offset_data()).unwrap();
                let x = anchor.x_coordinate();
                let y = anchor.y_coordinate();
                marks.push((gid, mark_class, (x, y)));
            }

            for (i, gid) in base_cov.iter().enumerate() {
                let rec = mark2array.mark2_records().get(i).unwrap();
                let anchors = rec
                    .mark2_anchors(mark2array.offset_data())
                    .iter()
                    .map(|anchor| {
                        anchor
                            .transpose()
                            .unwrap()
                            .map(|anchor| (anchor.x_coordinate(), anchor.y_coordinate()))
                            .unwrap_or_default()
                    })
                    .collect::<Vec<_>>();
                bases.push((gid, anchors));
            }
        }

        assert_eq!(
            marks,
            vec![
                (acutecomb, 0, (0, 578)),
                (brevecomb, 0, (-1, 578)),
                (tildecomb, 0, (0, 578)),
                (macroncomb, 0, (0, 578)),
            ]
        );

        assert_eq!(
            bases,
            [
                (acutecomb, vec![(0, 810)]),
                (brevecomb, vec![(-1, 776)]),
                (tildecomb, vec![(0, 776)]),
                (macroncomb, vec![(0, 810)])
            ]
        );
    }

    fn assert_noexport(source: &str) {
        let result = TestCompile::compile_source(source);

        assert!(result.get_glyph_index("hyphen").is_none());
        let fe_hyphen_consumer = result
            .fe_context
            .glyphs
            .get(&FeWorkIdentifier::Glyph("manual-component".into()));
        assert!(
            !fe_hyphen_consumer
                .default_instance()
                .components
                .iter()
                .any(|c| c.base == "hyphen".into()),
            "IR glyph should not reference hyphen {fe_hyphen_consumer:?}"
        );
        let be_hyphen_consumer = result
            .be_context
            .glyphs
            .get(&BeWorkIdentifier::GlyfFragment("manual-component".into()).into());
        assert!(
            be_hyphen_consumer.is_simple(),
            "{be_hyphen_consumer:?} should be a simple glyph"
        );
    }

    #[test]
    fn compile_obeys_no_export_glyphs() {
        assert_noexport("glyphs3/WghtVar_NoExport.glyphs");
    }

    #[test]
    fn compile_obeys_no_export_designspace() {
        assert_noexport("designspace_from_glyphs/WghtVar_NoExport.designspace");
    }

    fn assert_fs_type(source: &str, expected_fs_type: u16) {
        let compile = TestCompile::compile_source(source);
        let os2 = compile.font().os2().unwrap();

        assert_eq!(expected_fs_type, os2.fs_type());
    }

    #[test]
    fn default_fs_type_glyphs() {
        assert_fs_type("glyphs3/WghtVar.glyphs", 1 << 3);
    }

    #[test]
    fn default_fs_type_designspace() {
        assert_fs_type("designspace_from_glyphs/WghtVar.designspace", 1 << 2);
    }

    #[test]
    fn one_lookup_per_group() {
        let compile = TestCompile::compile_source("glyphs3/Oswald-AE-comb.glyphs");
        let gpos = compile.font().gpos().unwrap();

        // We had a bug where it was 2
        assert_eq!(1, mark_base_lookups(&gpos).len());
    }

    fn assert_post_italic_angle(source: &str, expected_angle: f32) {
        let compile = TestCompile::compile_source(source);
        let post = compile.font().post().unwrap();

        assert_eq!(expected_angle, post.italic_angle().to_f32());
    }

    #[test]
    fn italic_angle_in_glyphs2() {
        assert_post_italic_angle("glyphs2/SlantedFont.glyphs", -12.0);
    }

    #[test]
    fn italic_angle_in_glyphs3() {
        assert_post_italic_angle("glyphs3/SlantedFont.glyphs", -12.0);
    }

    #[test]
    fn italic_angle_in_designspace() {
        assert_post_italic_angle("designspace_from_glyphs/SlantedFont.designspace", -12.0);
    }

    fn assert_hhea_caret_slope(source: &str, expected_slope_rise: i16, expected_slope_run: i16) {
        let compile = TestCompile::compile_source(source);
        let hhea = compile.font().hhea().unwrap();

        assert_eq!(expected_slope_rise, hhea.caret_slope_rise());
        assert_eq!(expected_slope_run, hhea.caret_slope_run());
        assert_eq!(0, hhea.caret_offset());
    }

    #[test]
    fn default_caret_slope_in_glyphs2() {
        assert_hhea_caret_slope("glyphs2/SlantedFont.glyphs", 1000, 213);
    }

    #[test]
    fn default_caret_slope_in_glyphs3() {
        assert_hhea_caret_slope("glyphs3/SlantedFont.glyphs", 1000, 213);
    }

    #[test]
    fn default_caret_slope_in_designspace() {
        assert_hhea_caret_slope("designspace_from_glyphs/SlantedFont.designspace", 1000, 213);
    }

    fn assert_mvar(
        mvar: write_fonts::read::tables::mvar::Mvar,
        expected_value_records: Vec<(Tag, u16)>,
        expected_delta_sets: Vec<Vec<i32>>,
    ) {
        assert_eq!(mvar.value_records().len(), expected_value_records.len());
        let actual_records = mvar
            .value_records()
            .iter()
            .map(|rec| {
                assert_eq!(rec.delta_set_outer_index(), 0);
                (rec.value_tag(), rec.delta_set_inner_index())
            })
            .collect::<Vec<_>>();
        assert_eq!(actual_records, expected_value_records);

        let Some(Ok(varstore)) = mvar.item_variation_store() else {
            panic!("MVAR has no ItemVariationStore?!");
        };

        assert!(!expected_delta_sets.is_empty());
        let expected_num_regions = expected_delta_sets[0].len() as u16;
        assert!(expected_num_regions > 0);
        assert!(expected_delta_sets
            .iter()
            .all(|row| row.len() as u16 == expected_num_regions));

        assert_eq!(
            varstore.variation_region_list().unwrap().region_count(),
            expected_num_regions
        );

        assert_eq!(varstore.item_variation_data_count(), 1);
        let vardata = varstore.item_variation_data().get(0).unwrap().unwrap();
        assert_eq!(
            vardata.region_indexes(),
            (0..expected_num_regions).collect::<Vec<_>>()
        );
        assert_eq!(delta_sets(&vardata), expected_delta_sets);
    }

    #[test]
    fn mvar_from_designspace() {
        // This test font contains 3 masters defined over the 'slnt' axis, the middle
        // one is upright, the other two are slanted backwards and forwards +/-5 degrees.
        // For the sake of simplicity, all variable global metrics in the back-slanted
        // master where offset by -5 units, and in the forward-slanted master by +5 units,
        // so their MVAR records will all point to the same delta set.
        let compile = TestCompile::compile_source("MVAR.designspace");

        assert_mvar(
            compile.font().mvar().unwrap(),
            vec![
                (Tag::new(b"cpht"), 0),
                (Tag::new(b"hasc"), 0),
                (Tag::new(b"hcla"), 0),
                (Tag::new(b"hcld"), 0),
                (Tag::new(b"hcof"), 0),
                (Tag::new(b"hcrn"), 0),
                (Tag::new(b"hcrs"), 0),
                (Tag::new(b"hdsc"), 0),
                (Tag::new(b"hlgp"), 0),
                (Tag::new(b"sbxo"), 0),
                (Tag::new(b"sbxs"), 0),
                (Tag::new(b"sbyo"), 0),
                (Tag::new(b"sbys"), 0),
                (Tag::new(b"spxo"), 0),
                (Tag::new(b"spxs"), 0),
                (Tag::new(b"spyo"), 0),
                (Tag::new(b"spys"), 0),
                (Tag::new(b"stro"), 0),
                (Tag::new(b"strs"), 0),
                (Tag::new(b"undo"), 0),
                (Tag::new(b"unds"), 0),
                (Tag::new(b"xhgt"), 0),
            ],
            vec![vec![5, -5]],
        );
    }

    fn assert_mvar_from_glyphs(mvar: write_fonts::read::tables::mvar::Mvar) {
        // This is almost the same font as MVAR.designspace, with the only difference
        // that the hhea caret slope rise and offset cannot be tested because Glyphs.app
        // has no custom parameters for them; only the hhea caret slope run 'hcrn' can
        // be indirectly tweaked via the italic angle.
        assert_mvar(
            mvar,
            vec![
                (Tag::new(b"cpht"), 0),
                (Tag::new(b"hasc"), 0),
                (Tag::new(b"hcla"), 0),
                (Tag::new(b"hcld"), 0),
                // (Tag::new(b"hcof"), ?),
                // (Tag::new(b"hcrs"), ?),
                (Tag::new(b"hcrn"), 1),
                (Tag::new(b"hdsc"), 0),
                (Tag::new(b"hlgp"), 0),
                (Tag::new(b"sbxo"), 0),
                (Tag::new(b"sbxs"), 0),
                (Tag::new(b"sbyo"), 0),
                (Tag::new(b"sbys"), 0),
                (Tag::new(b"spxo"), 0),
                (Tag::new(b"spxs"), 0),
                (Tag::new(b"spyo"), 0),
                (Tag::new(b"spys"), 0),
                (Tag::new(b"stro"), 0),
                (Tag::new(b"strs"), 0),
                (Tag::new(b"undo"), 0),
                (Tag::new(b"unds"), 0),
                (Tag::new(b"xhgt"), 0),
            ],
            vec![
                vec![5, -5],   // deltas for everything except 'hcrn'
                vec![87, -87], // 'hcrn' deltas, derived from the italic angle
            ],
        );
    }

    #[test]
    fn mvar_from_glyphs2() {
        let compile = TestCompile::compile_source("glyphs2/MVAR.glyphs");
        assert_mvar_from_glyphs(compile.font().mvar().unwrap());
    }

    #[test]
    fn mvar_from_glyphs3() {
        let compile = TestCompile::compile_source("glyphs3/MVAR.glyphs");
        assert_mvar_from_glyphs(compile.font().mvar().unwrap());
    }

    #[test]
    fn compile_ufo() {
        let result = TestCompile::compile_source("WghtVar-Bold.ufo");
        let font = result.font();
        let name = font.name().unwrap();

        // Expected values based on
        //  fontmake resources/testdata/WghtVar-Bold.ufo
        //  ttx -o - -t name master_ttf/WghtVar-Bold.ttf
        assert_eq!(
            vec![
                (NameId::FAMILY_NAME, "Wght Var".to_string()),
                (NameId::SUBFAMILY_NAME, "Regular".to_string()),
                (NameId::UNIQUE_ID, "0.000;NONE;WghtVar-Regular".to_string()),
                (NameId::FULL_NAME, "Wght Var Regular".to_string()),
                (NameId::VERSION_STRING, "Version 0.000".to_string()),
                (NameId::POSTSCRIPT_NAME, "WghtVar-Regular".to_string()),
            ],
            name.name_record()
                .iter()
                .map(|nr| (nr.name_id.get(), resolve_name(&name, nr.name_id()).unwrap()))
                .collect::<Vec<_>>()
        );
    }
}
