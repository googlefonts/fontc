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

use workload::Workload;

use std::{
    fs, io,
    path::{Path, PathBuf},
    time::Instant,
};

use fontbe::{
    avar::create_avar_work,
    cmap::create_cmap_work,
    features::FeatureWork,
    font::create_font_work,
    fvar::create_fvar_work,
    glyphs::{create_glyf_loca_work, create_glyf_work},
    gvar::create_gvar_work,
    head::create_head_work,
    hvar::create_hvar_work,
    metrics_and_limits::create_metric_and_limit_work,
    name::create_name_work,
    os2::create_os2_work,
    post::create_post_work,
    stat::create_stat_work,
};

use fontdrasil::types::GlyphName;
use fontir::{glyph::create_glyph_order_work, source::DeleteWork};

use fontbe::orchestration::Context as BeContext;
use fontbe::paths::Paths as BePaths;
use fontir::paths::Paths as IrPaths;

use log::{debug, warn};

pub fn require_dir(dir: &Path) -> Result<PathBuf, io::Error> {
    if dir.exists() && !dir.is_dir() {
        panic!("{dir:#?} is taken by something that isn't a directory");
    }
    if !dir.exists() {
        fs::create_dir(dir)?
    }
    debug!("require_dir {:?}", dir);
    Ok(dir.to_path_buf())
}

pub fn init_paths(args: &Args) -> Result<(IrPaths, BePaths), Error> {
    let ir_paths = IrPaths::new(&args.build_dir);
    let be_paths = BePaths::new(&args.build_dir);

    require_dir(ir_paths.build_dir())?;
    if args.emit_ir {
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
    if !args.emit_ir {
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

fn add_feature_be_job(workload: &mut Workload) -> Result<(), Error> {
    let work = FeatureWork::create();
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

fn add_kerning_ir_job(workload: &mut Workload) -> Result<(), Error> {
    let work = workload
        .change_detector
        .ir_source()
        .create_kerning_ir_work(workload.change_detector.current_inputs())?;
    workload.add(work.into(), workload.change_detector.kerning_ir_change());
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
        .create_glyph_ir_work(&glyphs_changed, workload.change_detector.current_inputs())?;
    for work in glyph_work {
        workload.add(work.into(), true);
    }

    Ok(())
}

fn add_glyph_be_jobs(workload: &mut Workload) -> Result<(), Error> {
    let glyphs_changed = workload.change_detector.glyphs_changed();
    for glyph_name in glyphs_changed {
        add_glyph_be_job(workload, glyph_name);
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
        workload.change_detector.static_metadata_ir_change()
            || workload.change_detector.glyph_order_ir_change()
            || !glyphs_changed.is_empty(),
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
    t0: Instant,
) -> Result<Workload, Error> {
    let mut workload = change_detector.create_workload(t0)?;

    // FE: f(source) => IR
    add_feature_ir_job(&mut workload)?;
    add_kerning_ir_job(&mut workload)?;
    add_glyph_ir_jobs(&mut workload)?;
    add_glyph_order_ir_job(&mut workload)?;

    // BE: f(IR, maybe other BE work) => binary
    add_feature_be_job(&mut workload)?;
    add_glyph_be_jobs(&mut workload)?;
    add_glyf_loca_be_job(&mut workload)?;
    add_avar_be_job(&mut workload)?;
    add_stat_be_job(&mut workload)?;
    add_cmap_be_job(&mut workload)?;
    add_fvar_be_job(&mut workload)?;
    add_gvar_be_job(&mut workload)?;
    add_head_be_job(&mut workload)?;
    add_metric_and_limits_job(&mut workload)?;
    add_hvar_be_job(&mut workload)?;
    add_name_be_job(&mut workload)?;
    add_os2_be_job(&mut workload)?;
    add_post_be_job(&mut workload)?;

    // Make a damn font
    add_font_be_job(&mut workload)?;

    Ok(workload)
}

#[cfg(test)]
pub fn testdata_dir() -> PathBuf {
    // cargo test seems to run in the project directory
    // VSCode test seems to run in the workspace directory
    // probe for the file we want in hopes of finding it regardless
    ["./resources/testdata", "../resources/testdata"]
        .iter()
        .map(PathBuf::from)
        .find(|pb| pb.exists())
        .unwrap()
}

#[cfg(test)]
mod tests {

    use std::{
        collections::{HashSet, VecDeque},
        fs::{self, File},
        io::Read,
        path::{Path, PathBuf},
        str::FromStr,
    };

    use chrono::{Duration, TimeZone, Utc};
    use fontbe::orchestration::{
        AnyWorkId, Context as BeContext, Glyph, LocaFormatWrapper, WorkId as BeWorkIdentifier,
    };
    use fontdrasil::types::GlyphName;
    use fontir::{
        ir::{self, KernParticipant},
        orchestration::{Context as FeContext, Persistable, WorkId as FeWorkIdentifier},
    };
    use indexmap::IndexSet;
    use kurbo::{Point, Rect};
    use log::info;
    use pretty_assertions::assert_eq;

    use read_fonts::{
        tables::{name::Name, os2::SelectionFlags},
        types::NameId,
    };
    use skrifa::{
        charmap::Charmap,
        instance::Size,
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
        GlyphId, Tag,
    };
    use tempfile::{tempdir, TempDir};
    use write_fonts::{
        dump_table,
        tables::{
            glyf::{Bbox, Glyph as RawGlyph},
            loca::LocaFormat,
        },
    };

    use super::*;

    struct TestCompile {
        build_dir: PathBuf,
        work_executed: HashSet<AnyWorkId>,
        glyphs_changed: IndexSet<GlyphName>,
        glyphs_deleted: IndexSet<GlyphName>,
        fe_context: FeContext,
        be_context: BeContext,
    }

    impl TestCompile {
        fn new(
            change_detector: &ChangeDetector,
            fe_context: FeContext,
            be_context: BeContext,
        ) -> TestCompile {
            let build_dir = change_detector.be_paths().build_dir().to_path_buf();
            TestCompile {
                build_dir,
                work_executed: HashSet::new(),
                glyphs_changed: change_detector.glyphs_changed(),
                glyphs_deleted: change_detector.glyphs_deleted(),
                fe_context,
                be_context,
            }
        }

        fn get_glyph_index(&self, name: &str) -> u32 {
            self.fe_context
                .glyph_order
                .get()
                .glyph_id(&name.into())
                .unwrap()
        }

        fn glyphs(&self) -> Glyphs {
            Glyphs::new(&self.build_dir)
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

    fn compile(args: Args) -> TestCompile {
        let t0 = Instant::now();
        let _ = env_logger::builder().is_test(true).try_init();

        info!("Compile {args:?}");

        let (ir_paths, be_paths) = init_paths(&args).unwrap();
        let config = Config::new(args).unwrap();

        let prev_inputs = config.init().unwrap();

        let mut change_detector =
            ChangeDetector::new(config.clone(), ir_paths.clone(), prev_inputs).unwrap();

        let mut workload = create_workload(&mut change_detector, t0).unwrap();

        // Try to do the work
        // As we currently don't stress dependencies just run one by one
        // This will likely need to change when we start doing things like glyphs with components

        let fe_root = FeContext::new_root(
            config.args.flags(),
            ir_paths,
            workload.change_detector.current_inputs().clone(),
        );
        let be_root = BeContext::new_root(config.args.flags(), be_paths, &fe_root.read_only());
        let mut result = TestCompile::new(
            workload.change_detector,
            fe_root.read_only(),
            be_root.copy_read_only(),
        );
        let completed = workload.run_for_test(&fe_root, &be_root);

        change_detector.finish_successfully().unwrap();
        result.work_executed = completed;

        write_font_file(&config.args, &be_root).unwrap();

        result
    }

    fn assert_compile_work(source: &str, glyphs: Vec<&str>) {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(Args::for_test(build_dir, source));
        let mut completed = result.work_executed.iter().cloned().collect::<Vec<_>>();

        let mut expected = vec![
            AnyWorkId::Fe(FeWorkIdentifier::StaticMetadata),
            FeWorkIdentifier::GlobalMetrics.into(),
            FeWorkIdentifier::PreliminaryGlyphOrder.into(),
            FeWorkIdentifier::GlyphOrder.into(),
            FeWorkIdentifier::Features.into(),
            FeWorkIdentifier::Kerning.into(),
            BeWorkIdentifier::Features.into(),
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
            BeWorkIdentifier::Loca.into(),
            BeWorkIdentifier::LocaFormat.into(),
            BeWorkIdentifier::Maxp.into(),
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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(Args::for_test(build_dir, "wght_var.designspace"));
        assert_eq!(
            IndexSet::from(["bar".into(), "plus".into()]),
            result.glyphs_changed
        );
        assert_eq!(IndexSet::new(), result.glyphs_deleted);

        let result = compile(Args::for_test(build_dir, "wght_var.designspace"));
        assert_eq!(HashSet::new(), result.work_executed);
        assert_eq!(IndexSet::new(), result.glyphs_changed);
        assert_eq!(IndexSet::new(), result.glyphs_deleted);
    }

    #[test]
    fn second_compile_only_glyph() {
        // glyph depends on static metadata, which isn't going to run
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(Args::for_test(build_dir, "wght_var.designspace"));
        assert!(result.work_executed.len() > 1);

        let glyph_ir_file = build_dir.join("glyph_ir/bar.yml");
        fs::remove_file(&glyph_ir_file).unwrap();

        let result = compile(Args::for_test(build_dir, "wght_var.designspace"));
        assert!(glyph_ir_file.exists(), "{glyph_ir_file:?}");
        let mut completed = result.work_executed.iter().cloned().collect::<Vec<_>>();
        completed.sort();
        assert_eq!(
            vec![
                AnyWorkId::Fe(FeWorkIdentifier::Glyph("bar".into())),
                FeWorkIdentifier::Anchor("bar".into()).into(),
                BeWorkIdentifier::Cmap.into(),
                BeWorkIdentifier::Font.into(),
                BeWorkIdentifier::Glyf.into(),
                BeWorkIdentifier::Gvar.into(),
                BeWorkIdentifier::Hhea.into(),
                BeWorkIdentifier::Hmtx.into(),
                BeWorkIdentifier::Hvar.into(),
                BeWorkIdentifier::Loca.into(),
                BeWorkIdentifier::LocaFormat.into(),
                BeWorkIdentifier::Maxp.into(),
            ],
            completed,
            "{completed:#?}"
        );
    }

    #[test]
    fn second_compile_only_kerning() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(Args::for_test(build_dir, "glyphs3/WghtVar.glyphs"));
        assert!(result.work_executed.len() > 1);

        fs::remove_file(build_dir.join("kerning.yml")).unwrap();

        let result = compile(Args::for_test(build_dir, "glyphs3/WghtVar.glyphs"));
        let mut completed = result.work_executed.iter().cloned().collect::<Vec<_>>();
        completed.sort();
        assert_eq!(
            vec![
                AnyWorkId::Fe(FeWorkIdentifier::Features),
                FeWorkIdentifier::Kerning.into(),
                BeWorkIdentifier::Features.into(),
                BeWorkIdentifier::Font.into(),
                BeWorkIdentifier::Gpos.into(),
                BeWorkIdentifier::Gsub.into(),
                BeWorkIdentifier::Gdef.into(),
            ],
            completed
        );
    }

    #[test]
    fn deleted_ir_recreated() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(Args::for_test(build_dir, "wght_var.designspace"));
        assert_eq!(
            IndexSet::from(["bar".into(), "plus".into()]),
            result.glyphs_changed
        );
        assert_eq!(IndexSet::new(), result.glyphs_deleted);

        let bar_ir = build_dir.join("glyph_ir/bar.yml");
        assert!(bar_ir.is_file(), "no file {bar_ir:#?}");
        fs::remove_file(bar_ir).unwrap();

        let result = compile(Args::for_test(build_dir, "wght_var.designspace"));
        assert_eq!(IndexSet::from(["bar".into()]), result.glyphs_changed);
        assert_eq!(IndexSet::new(), result.glyphs_deleted);
    }

    fn assert_compiles_with_gpos_and_gsub(
        source: &str,
        adjust_args: impl Fn(&mut Args),
    ) -> TestCompile {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let mut args = Args::for_test(build_dir, source);
        adjust_args(&mut args);
        let result = compile(args);

        let font_file = build_dir.join("font.ttf");
        assert!(font_file.exists());
        let buf = fs::read(font_file).unwrap();
        let font = FontRef::new(&buf).unwrap();

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
        assert_compiles_with_gpos_and_gsub("static.designspace", |_| ());
    }

    #[test]
    fn compile_fea_with_includes() {
        assert_compiles_with_gpos_and_gsub("fea_include.designspace", |_| ());
    }

    #[test]
    fn compile_fea_with_includes_no_ir() {
        assert_compiles_with_gpos_and_gsub("fea_include.designspace", |args| {
            args.emit_debug = false;
            args.emit_ir = false;
        });
    }

    #[test]
    fn compile_fea_again_with_modified_include() {
        let temp_dir = tempdir().unwrap();
        let source_dir = temp_dir.path().join("sources");
        let build_dir = temp_dir.path().join("build");
        fs::create_dir(&source_dir).unwrap();
        fs::create_dir(&build_dir).unwrap();

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
        compile(Args::for_test(&build_dir, source.to_str().unwrap()));

        let shared_fea = source_dir.join("fea_include_ufo/common.fea");
        fs::write(
            &shared_fea,
            fs::read_to_string(&shared_fea).unwrap() + "\n\nfeature k2 { pos bar bar -100; } k2;",
        )
        .unwrap();

        let result = compile(Args::for_test(&build_dir, source.to_str().unwrap()));

        // Features and things downstream of features should rebuild
        let mut completed = result.work_executed.iter().cloned().collect::<Vec<_>>();
        completed.sort();
        assert_eq!(
            vec![
                AnyWorkId::Fe(FeWorkIdentifier::Features),
                AnyWorkId::Fe(FeWorkIdentifier::Kerning),
                BeWorkIdentifier::Features.into(),
                BeWorkIdentifier::Font.into(),
                BeWorkIdentifier::Gpos.into(),
                BeWorkIdentifier::Gsub.into(),
                BeWorkIdentifier::Gdef.into(),
            ],
            completed
        );
    }

    fn build_contour_and_composite_glyph(
        temp_dir: &TempDir,
        prefer_simple_glyphs: bool,
    ) -> ir::Glyph {
        let build_dir = temp_dir.path();

        let mut args = Args::for_test(build_dir, "glyphs2/MixedContourComponent.glyphs");
        args.prefer_simple_glyphs = prefer_simple_glyphs; // <-- important :)
        let result = compile(args);

        let glyph = result
            .fe_context
            .glyphs
            .get(&FeWorkIdentifier::Glyph("contour_and_component".into()));

        assert_eq!(1, glyph.sources().len());
        (*glyph).clone()
    }

    fn read_file(path: &Path) -> Vec<u8> {
        assert!(path.exists(), "{path:?} not found");
        let mut buf = Vec::new();
        File::open(path).unwrap().read_to_end(&mut buf).unwrap();
        buf
    }

    fn read_ir_glyph(build_dir: &Path, name: &str) -> ir::Glyph {
        let raw_glyph = read_file(&build_dir.join(format!("glyph_ir/{name}.yml")));
        ir::Glyph::read(&mut raw_glyph.as_slice())
    }

    fn read_be_glyph(build_dir: &Path, name: &str) -> RawGlyph {
        let raw_glyph = read_file(&build_dir.join(format!("glyphs/{name}.glyf")));
        let read: &mut dyn Read = &mut raw_glyph.as_slice();
        Glyph::read(read).data
    }

    #[test]
    fn resolve_contour_and_composite_glyph_in_non_legacy_mode() {
        let temp_dir = tempdir().unwrap();
        let glyph = build_contour_and_composite_glyph(&temp_dir, false);
        assert!(glyph.default_instance().contours.is_empty(), "{glyph:?}");
        assert_eq!(2, glyph.default_instance().components.len(), "{glyph:?}");

        let RawGlyph::Composite(glyph) = read_be_glyph(temp_dir.path(), glyph.name.as_str()) else {
            panic!("Expected a simple glyph");
        };
        let raw_glyph = dump_table(&glyph).unwrap();
        let glyph = CompositeGlyph::read(FontData::new(&raw_glyph)).unwrap();
        // -1: composite, per https://learn.microsoft.com/en-us/typography/opentype/spec/glyf
        assert_eq!(-1, glyph.number_of_contours());
    }

    #[test]
    fn resolve_contour_and_composite_glyph_in_legacy_mode() {
        let temp_dir = tempdir().unwrap();
        let glyph = build_contour_and_composite_glyph(&temp_dir, true);
        assert!(glyph.default_instance().components.is_empty(), "{glyph:?}");
        assert_eq!(2, glyph.default_instance().contours.len(), "{glyph:?}");

        let RawGlyph::Simple(glyph) = read_be_glyph(temp_dir.path(), glyph.name.as_str()) else {
            panic!("Expected a simple glyph");
        };
        assert_eq!(2, glyph.contours().len());
    }

    #[test]
    fn compile_simple_binary_glyph() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        compile(Args::for_test(build_dir, "static.designspace"));

        let RawGlyph::Simple(glyph) = read_be_glyph(build_dir, "bar") else {
            panic!("Expected a simple glyph");
        };

        assert_eq!(1, glyph.contours().len());
        assert_eq!(
            4_usize,
            glyph.contours().iter().map(|c| c.iter().count()).sum()
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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, "static.designspace"));

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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, "glyphs3/Oswald-O.glyphs"));

        let glyph_data = result.glyphs();
        let glyphs = glyph_data.read();
        // the glyph 'O' contains several quad splines
        let uppercase_o = &glyphs[result.get_glyph_index("O") as usize];
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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, "glyphs2/Component.glyphs"));

        // Per source, glyphs should be period, comma, non_uniform_scale
        // Period is simple, the other two use it as a component
        let glyph_data = result.glyphs();
        let all_glyphs = glyph_data.read();

        assert!(matches!(
            (
                all_glyphs[result.get_glyph_index("period") as usize]
                    .as_ref()
                    .unwrap(),
                all_glyphs[result.get_glyph_index("comma") as usize]
                    .as_ref()
                    .unwrap(),
                all_glyphs[result.get_glyph_index("non_uniform_scale") as usize]
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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, "glyphs2/Component.glyphs"));
        // non-uniform scaling of period
        let period_idx = result.get_glyph_index("period");
        let non_uniform_scale_idx = result.get_glyph_index("non_uniform_scale");
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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, "glyphs2/Component.glyphs"));

        let gid = result.get_glyph_index("simple_transform_again");
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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let mut args = Args::for_test(build_dir, "glyphs2/Component.glyphs");
        args.decompose_transformed_components = true;
        let result = compile(args);

        let glyph_data = result.glyphs();
        let glyphs = glyph_data.read();

        // Not an identity 2x2, should be simplified
        let Some(glyf::Glyph::Simple(..)) =
            &glyphs[result.get_glyph_index("simple_transform_again") as usize]
        else {
            panic!("Expected a simple glyph\n{glyphs:#?}");
        };

        // Identity 2x2, should be left as a component
        let Some(glyf::Glyph::Composite(..)) =
            &glyphs[result.get_glyph_index("translate_only") as usize]
        else {
            panic!("Expected a composite glyph\n{glyphs:#?}");
        };
    }

    #[test]
    fn writes_cmap() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, "glyphs2/Component.glyphs"));

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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, "glyphs2/NotDef.glyphs"));

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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, "glyphs2/Mono.glyphs"));

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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        compile(Args::for_test(build_dir, "wght_var.designspace"));

        let font_file = build_dir.join("font.ttf");
        assert!(font_file.exists());

        let buf = fs::read(font_file).unwrap();
        let font = FontRef::new(&buf).unwrap();

        assert_eq!(
            vec![
                Tag::new(b"HVAR"),
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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        compile(Args::for_test(build_dir, "mov_xy.designspace"));

        let font_file = build_dir.join("font.ttf");
        assert!(font_file.exists());
        let buf = fs::read(font_file).unwrap();
        let font = FontRef::new(&buf).unwrap();

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
        let mut cx = skrifa::scale::Context::new();
        cx.new_scaler()
            .size(Size::new(1000.0))
            .normalized_coords(
                coords
                    .into_iter()
                    .map(F2Dot14::from_f32)
                    .collect::<Vec<_>>(),
            )
            .build(font)
            .outline(gid, &mut bp)
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

    impl skrifa::scale::Pen for CboxPen {
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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, "glyphs2/WghtVar.glyphs"));

        assert!(!result
            .fe_context
            .preliminary_glyph_order
            .get()
            .contains(&GlyphName::NOTDEF));
        assert_eq!(
            Some(0),
            result
                .fe_context
                .glyph_order
                .get()
                .glyph_id(&GlyphName::NOTDEF)
        );

        let font_file = build_dir.join("font.ttf");
        assert!(font_file.exists());
        let buf = fs::read(font_file).unwrap();
        let font = FontRef::new(&buf).unwrap();

        // Character 0x0000 (NULL) != '.notdef' glyph, and neither are any other
        // characters actually, because '.notdef' (glyph index 0) means the absence
        // of a character-to-glyph mapping:
        // https://github.com/googlefonts/fontc/pull/423/files#r1309257127
        // https://learn.microsoft.com/en-us/typography/opentype/spec/cmap#overview
        assert_eq!(None, font.cmap().unwrap().map_codepoint(0u32));
    }

    #[test]
    fn compile_glyphs_font_with_weight_axis() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        compile(Args::for_test(build_dir, "glyphs2/WghtVar.glyphs"));

        let font_file = build_dir.join("font.ttf");
        assert!(font_file.exists());
        let buf = fs::read(font_file).unwrap();
        let font = FontRef::new(&buf).unwrap();

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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        compile(Args::for_test(build_dir, "glyphs2/WghtVar_Avar.glyphs"));

        let font_file = build_dir.join("font.ttf");
        assert!(font_file.exists());
        let buf = fs::read(font_file).unwrap();
        let font = FontRef::new(&buf).unwrap();

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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let mut args = Args::for_test(build_dir, "glyphs2/WghtVar.glyphs");
        args.emit_ir = false;
        compile(args);

        let outputs = fs::read_dir(build_dir)
            .unwrap()
            .map(|e| {
                e.unwrap()
                    .path()
                    .strip_prefix(build_dir)
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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        compile(Args::for_test(build_dir, source));

        let font_file = build_dir.join("font.ttf");
        assert!(font_file.exists());
        let buf = fs::read(font_file).unwrap();
        let font = FontRef::new(&buf).unwrap();

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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        compile(Args::for_test(build_dir, source));

        let font_file = build_dir.join("font.ttf");
        assert!(font_file.exists());
        let buf = fs::read(font_file).unwrap();
        let font = FontRef::new(&buf).unwrap();
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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        compile(Args::for_test(build_dir, "static.designspace"));

        let font_file = build_dir.join("font.ttf");
        assert!(font_file.exists());
        let buf = fs::read(font_file).unwrap();
        let font = FontRef::new(&buf).unwrap();
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
    fn captures_vendor_id() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        compile(Args::for_test(build_dir, "glyphs3/TheBestNames.glyphs"));

        let font_file = build_dir.join("font.ttf");
        assert!(font_file.exists());
        let buf = fs::read(font_file).unwrap();
        let font = FontRef::new(&buf).unwrap();
        let os2 = font.os2().unwrap();
        assert_eq!(Tag::new(b"RODS"), os2.ach_vend_id());
    }

    fn assert_maxp_composite(
        src: &str,
        max_component_depth: u16,
        max_composite_points: u16,
        max_composite_contours: u16,
    ) {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        compile(Args::for_test(build_dir, src));

        let font_file = build_dir.join("font.ttf");
        assert!(font_file.exists());
        let buf = fs::read(font_file).unwrap();
        let font = FontRef::new(&buf).unwrap();
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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        compile(Args::for_test(build_dir, source));

        let font_file = build_dir.join("font.ttf");
        assert!(font_file.exists());
        let buf = fs::read(font_file).unwrap();
        let font = FontRef::new(&buf).unwrap();
        let head = font.head().unwrap();

        // TIL Mac has an epoch of it's own
        let mac_epoch = Utc.with_ymd_and_hms(1904, 1, 1, 0, 0, 0).unwrap();
        let created = mac_epoch
            .checked_add_signed(Duration::seconds(head.created().as_secs()))
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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        compile(Args::for_test(build_dir, "wght_var.designspace"));

        let font_file = build_dir.join("font.ttf");
        assert!(font_file.exists());
        let buf = fs::read(font_file).unwrap();
        let font = FontRef::new(&buf).unwrap();

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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, source));

        let kerning = result.fe_context.kerning.get();

        let mut groups: Vec<_> = kerning
            .groups
            .iter()
            .map(|(name, entries)| {
                let mut entries: Vec<_> = entries.iter().map(|e| e.as_str()).collect();
                entries.sort();
                (name.as_str(), entries)
            })
            .collect();
        groups.sort();

        let mut kerns: Vec<_> = kerning
            .kerns
            .iter()
            .map(|((side1, side2), values)| {
                (
                    side1,
                    side2,
                    values
                        .iter()
                        .map(|(loc, val)| {
                            assert_eq!(loc.axis_tags().count(), 1, "Should be only wght");
                            let (axis, pos) = loc.iter().next().unwrap();
                            (format!("{axis} {}", pos.to_f32()), val.0)
                        })
                        .collect::<Vec<_>>(),
                )
            })
            .collect();
        kerns.sort_by_key(|(side1, side2, _)| (*side1, *side2));

        assert_eq!(
            (groups, kerns),
            (
                vec![
                    ("public.kern1.bracketleft_R", vec!["bracketleft"],),
                    ("public.kern1.bracketright_R", vec!["bracketright"],),
                    ("public.kern2.bracketleft_L", vec!["bracketleft"],),
                    ("public.kern2.bracketright_L", vec!["bracketright"],),
                ],
                vec![
                    (
                        &KernParticipant::Glyph("bracketleft".into()),
                        &KernParticipant::Glyph("bracketright".into()),
                        vec![
                            ("wght 0".to_string(), -300.0),
                            ("wght 1".to_string(), -150.0)
                        ],
                    ),
                    (
                        &KernParticipant::Glyph("exclam".into()),
                        &KernParticipant::Glyph("exclam".into()),
                        vec![
                            ("wght 0".to_string(), -360.0),
                            ("wght 1".to_string(), -100.0)
                        ],
                    ),
                    (
                        &KernParticipant::Glyph("exclam".into()),
                        &KernParticipant::Glyph("hyphen".into()),
                        vec![("wght 0".to_string(), 20.0),],
                    ),
                    (
                        &KernParticipant::Glyph("exclam".into()),
                        &KernParticipant::Group("public.kern2.bracketright_L".into()),
                        vec![("wght 0".to_string(), -160.0),],
                    ),
                    (
                        &KernParticipant::Glyph("hyphen".into()),
                        &KernParticipant::Glyph("hyphen".into()),
                        vec![
                            ("wght 0".to_string(), -150.0),
                            ("wght 1".to_string(), -50.0)
                        ],
                    ),
                    (
                        &KernParticipant::Group("public.kern1.bracketleft_R".into()),
                        &KernParticipant::Glyph("exclam".into()),
                        vec![("wght 0".to_string(), -165.0),],
                    ),
                ],
            ),
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

    #[test]
    fn intermediate_layer_in_designspace() {
        // https://github.com/googlefonts/fontc/issues/400
        let tmp_dir = tempdir().unwrap();
        let build_dir = tmp_dir.path();
        compile(Args::for_test(
            build_dir,
            "designspace_from_glyphs/IntermediateLayer/IntermediateLayer.designspace",
        ));

        let font_file = build_dir.join("font.ttf");
        assert!(font_file.exists());
        let buf = fs::read(font_file).unwrap();
        let font = FontRef::new(&buf).unwrap();

        assert_eq!(
            vec!["wght"],
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
                cbox_of_char(0x69, &font, vec![0.0]), // wght=400
                cbox_of_char(0x69, &font, vec![0.6]), // wght=700
                cbox_of_char(0x69, &font, vec![1.0]), // wght=900
            ]
        );

        // glyph "I" (char 0x49) only defines two masters at wght=400 and wght=900,
        // so wght=700 should be interpolated between them.
        assert_eq!(
            vec![
                Rect::new(231.0, 0.0, 364.0, 700.0),
                Rect::new(195.0, 0.0, 400.0, 700.0),
                Rect::new(171.0, 0.0, 424.0, 700.0),
            ],
            vec![
                cbox_of_char(0x49, &font, vec![0.0]), // wght=400
                cbox_of_char(0x49, &font, vec![0.6]), // wght=700
                cbox_of_char(0x49, &font, vec![1.0]), // wght=900
            ]
        );
    }

    #[test]
    fn empty_glyph_is_zero_length() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, "glyphs2/WghtVar.glyphs"));

        let space_idx = result.get_glyph_index("space") as usize;

        let font_file = build_dir.join("font.ttf");
        assert!(font_file.exists());
        let buf = fs::read(font_file).unwrap();
        let font = FontRef::new(&buf).unwrap();
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
        let temp_dir = tempdir().unwrap();

        let default_build_dir = temp_dir.path().join("default");
        // default is to skip_features=false
        compile(Args::for_test(&default_build_dir, source));

        let nofea_build_dir = temp_dir.path().join("nofea");
        let mut args = Args::for_test(&nofea_build_dir, source);
        args.skip_features = true;
        compile(args);

        let buf1 = fs::read(default_build_dir.join("font.ttf")).unwrap();
        let font_with_fea = FontRef::new(&buf1).unwrap();

        let buf2 = fs::read(nofea_build_dir.join("font.ttf")).unwrap();
        let font_without_fea = FontRef::new(&buf2).unwrap();

        for table_tag in table_tags {
            let tag = Tag::new(table_tag);
            assert!(font_with_fea.data_for_tag(tag).is_some());
            assert!(font_without_fea.data_for_tag(tag).is_none());
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
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let mut args = Args::for_test(build_dir, "glyphs3/WghtVar.glyphs");
        // first compile with default args (keep_direction=false)
        compile(args.clone());

        let ir_glyph = read_ir_glyph(build_dir, "hyphen");
        let ir_default_instance = ir_glyph.default_instance();
        assert_eq!(
            &ir_default_instance.contours[0].to_svg(),
            "M131,330 L131,250 L470,250 L470,330 L131,330 Z"
        );

        let RawGlyph::Simple(glyph) = read_be_glyph(build_dir, "hyphen") else {
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

        // recompile with --keep-direction
        args.keep_direction = true;
        compile(args);

        let RawGlyph::Simple(glyph) = read_be_glyph(build_dir, "hyphen") else {
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
}
