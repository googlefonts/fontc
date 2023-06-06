//! A font compiler with aspirations of being fast and safe.

mod args;
mod change_detector;
mod config;
mod error;
pub mod work;
mod workload;

pub use args::Args;
pub use change_detector::ChangeDetector;
pub use config::Config;
pub use error::Error;

use work::ReadAccess;
use workload::{Job, Workload};

use std::{
    collections::HashSet,
    fs, io,
    path::{Path, PathBuf},
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
    metrics_and_limits::create_metric_and_limit_work,
    name::create_name_work,
    orchestration::{AnyWorkId, WorkId as BeWorkIdentifier},
    os2::create_os2_work,
    post::create_post_work,
    stat::create_stat_work,
};

use fontdrasil::{orchestration::Access, types::GlyphName};
use fontir::{
    glyph::create_finalize_static_metadata_work,
    orchestration::{Context as FeContext, WorkId as FeWorkIdentifier},
    source::DeleteWork,
};

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
    // if IR is off the font didn't get written yet, otherwise it's done already
    let font_file = be_context.paths.target_file(&BeWorkIdentifier::Font);
    if !args.emit_ir {
        fs::write(font_file, be_context.get_font().get()).map_err(Error::IoError)?;
    } else if !font_file.exists() {
        return Err(Error::FileExpected(font_file));
    }
    Ok(())
}

fn add_init_static_metadata_ir_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.init_static_metadata_ir_change() {
        let id: AnyWorkId = FeWorkIdentifier::InitStaticMetadata.into();
        let write_access = Access::one(id.clone());
        workload.insert(
            id,
            Job {
                //FIXME: it's weird that we call a method on this type, passing
                //in an argument we also get from this type? In a world where there
                //is a general trait for 'create work' on Source we can have
                //a method on change_detector that calls this method with the current inputs?
                work: change_detector
                    .ir_source()
                    .create_static_metadata_work(change_detector.current_inputs())?
                    .into(),
                dependencies: HashSet::new(),
                read_access: ReadAccess::Dependencies,
                write_access,
            },
        );
    } else {
        workload.mark_success(FeWorkIdentifier::InitStaticMetadata);
    }
    Ok(())
}

fn add_finalize_static_metadata_ir_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.final_static_metadata_ir_change() {
        let mut dependencies: HashSet<_> = change_detector
            .glyphs_changed()
            .iter()
            .map(|gn| FeWorkIdentifier::Glyph(gn.clone()).into())
            .collect();
        dependencies.insert(FeWorkIdentifier::InitStaticMetadata.into());

        // Finalize may create new glyphs so allow read/write to *all* glyphs
        let read_access = Access::custom(|an_id: &AnyWorkId| {
            matches!(
                an_id,
                AnyWorkId::Fe(FeWorkIdentifier::Glyph(..))
                    | AnyWorkId::Fe(FeWorkIdentifier::InitStaticMetadata)
            )
        });
        let write_access = Access::custom(|an_id: &AnyWorkId| {
            matches!(
                an_id,
                AnyWorkId::Fe(FeWorkIdentifier::Glyph(..))
                    | AnyWorkId::Fe(FeWorkIdentifier::FinalizeStaticMetadata)
            )
        });
        workload.insert(
            FeWorkIdentifier::FinalizeStaticMetadata.into(),
            Job {
                work: create_finalize_static_metadata_work().into(),
                dependencies,
                read_access: ReadAccess::Custom(read_access),
                write_access,
            },
        );
    } else {
        workload.mark_success(FeWorkIdentifier::FinalizeStaticMetadata);
    }
    Ok(())
}

fn add_global_metric_ir_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.global_metrics_ir_change() {
        let id: AnyWorkId = FeWorkIdentifier::GlobalMetrics.into();
        let write_access = Access::one(id.clone());
        let mut dependencies = HashSet::new();
        dependencies.insert(FeWorkIdentifier::InitStaticMetadata.into());
        workload.insert(
            id,
            Job {
                //FIXME: it's weird that we call a method on this type, passing
                //in an argument we also get from this type? In a world where there
                //is a general trait for 'create work' on Source we can have
                //a method on change_detector that calls this method with the current inputs?
                work: change_detector
                    .ir_source()
                    .create_global_metric_work(change_detector.current_inputs())?
                    .into(),
                dependencies,
                read_access: ReadAccess::Dependencies,
                write_access,
            },
        );
    } else {
        workload.mark_success(FeWorkIdentifier::GlobalMetrics);
    }
    Ok(())
}

fn add_feature_ir_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.feature_ir_change() {
        let id: AnyWorkId = FeWorkIdentifier::Features.into();
        let write_access = Access::one(id.clone());
        workload.insert(
            id,
            Job {
                work: change_detector
                    .ir_source()
                    .create_feature_ir_work(change_detector.current_inputs())?
                    .into(),
                dependencies: HashSet::new(),
                read_access: ReadAccess::Dependencies,
                write_access,
            },
        );
    } else {
        workload.mark_success(FeWorkIdentifier::Features);
    }
    Ok(())
}

fn add_feature_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.feature_be_change() && change_detector.glyph_name_filter().is_none() {
        let id: AnyWorkId = BeWorkIdentifier::Features.into();

        // fea-rs compiles, or will compile, several tables
        // we want to capture them individually
        let write_access = Access::Set(HashSet::from([
            BeWorkIdentifier::Gsub.into(),
            BeWorkIdentifier::Gpos.into(),
        ]));
        workload.insert(
            id,
            Job {
                work: FeatureWork::create().into(),
                dependencies: HashSet::from([
                    FeWorkIdentifier::FinalizeStaticMetadata.into(),
                    FeWorkIdentifier::Features.into(),
                ]),
                read_access: ReadAccess::Dependencies,
                write_access,
            },
        );
    } else {
        // Features are extremely prone to not making sense when glyphs are filtered
        if change_detector.glyph_name_filter().is_some() {
            warn!("Not processing BE Features because a glyph name filter is active");
        }
        workload.mark_success(BeWorkIdentifier::Features);
        workload.mark_success(BeWorkIdentifier::Gpos);
        workload.mark_success(BeWorkIdentifier::Gsub);
    }
    Ok(())
}

fn add_kerning_ir_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.kerning_ir_change() {
        let mut dependencies = HashSet::new();
        dependencies.insert(FeWorkIdentifier::FinalizeStaticMetadata.into());

        let id: AnyWorkId = FeWorkIdentifier::Kerning.into();
        let write_access = Access::one(id.clone());
        workload.insert(
            id,
            Job {
                work: change_detector
                    .ir_source()
                    .create_kerning_ir_work(change_detector.current_inputs())?
                    .into(),
                dependencies,
                read_access: ReadAccess::Dependencies,
                write_access,
            },
        );
    } else {
        workload.mark_success(FeWorkIdentifier::Kerning);
    }
    Ok(())
}

fn add_glyph_ir_jobs(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    // Destroy IR for deleted glyphs. No dependencies.
    for glyph_name in change_detector.glyphs_deleted().iter() {
        let id = FeWorkIdentifier::Glyph(glyph_name.clone());
        let path = change_detector.ir_paths().target_file(&id);
        workload.insert(
            id.into(),
            Job {
                work: DeleteWork::create(path).into(),
                dependencies: HashSet::new(),
                read_access: ReadAccess::Dependencies,
                write_access: Access::none(),
            },
        );
    }

    // Generate IR for changed glyphs
    let glyphs_changed = change_detector.glyphs_changed();
    let glyph_work = change_detector
        .ir_source()
        .create_glyph_ir_work(&glyphs_changed, change_detector.current_inputs())?;
    for (glyph_name, work) in glyphs_changed.iter().zip(glyph_work) {
        let id = FeWorkIdentifier::Glyph(glyph_name.clone());
        let work = work.into();
        let dependencies = HashSet::from([FeWorkIdentifier::InitStaticMetadata.into()]);

        let id: AnyWorkId = id.into();
        let write_access = Access::one(id.clone());
        workload.insert(
            id,
            Job {
                work,
                dependencies,
                read_access: ReadAccess::Dependencies,
                write_access,
            },
        );
    }

    Ok(())
}

fn add_glyf_loca_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    let glyphs_changed = change_detector.glyphs_changed();

    // If no glyph has changed there isn't a lot of merging to do
    if !glyphs_changed.is_empty() {
        let mut dependencies: HashSet<_> = glyphs_changed
            .iter()
            .map(|gn| BeWorkIdentifier::GlyfFragment(gn.clone()).into())
            .collect();
        dependencies.insert(FeWorkIdentifier::FinalizeStaticMetadata.into());

        // Write the merged glyphs and write individual glyphs that are updated, such as composites with bboxes
        let write_access = Access::custom(|id| {
            matches!(
                id,
                AnyWorkId::Be(BeWorkIdentifier::Glyf)
                    | AnyWorkId::Be(BeWorkIdentifier::Loca)
                    | AnyWorkId::Be(BeWorkIdentifier::LocaFormat)
                    | AnyWorkId::Be(BeWorkIdentifier::GlyfFragment(..))
            )
        });
        let id: AnyWorkId = BeWorkIdentifier::Glyf.into();
        workload.insert(
            id,
            Job {
                work: create_glyf_loca_work().into(),
                dependencies,
                // We need to read all glyphs, even unchanged ones, plus static metadata
                read_access: ReadAccess::custom(|id| {
                    matches!(
                        id,
                        AnyWorkId::Be(BeWorkIdentifier::Glyf)
                            | AnyWorkId::Be(BeWorkIdentifier::Loca)
                            | AnyWorkId::Be(BeWorkIdentifier::GlyfFragment(..))
                    )
                }),
                write_access,
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Glyf);
        workload.mark_success(BeWorkIdentifier::Loca);
        workload.mark_success(BeWorkIdentifier::LocaFormat);
    }
    Ok(())
}

fn add_avar_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.avar_be_change() {
        let mut dependencies = HashSet::new();
        dependencies.insert(FeWorkIdentifier::InitStaticMetadata.into());

        let id: AnyWorkId = BeWorkIdentifier::Avar.into();
        workload.insert(
            id.clone(),
            Job {
                work: create_avar_work().into(),
                dependencies,
                read_access: ReadAccess::Dependencies,
                write_access: Access::one(id),
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Avar);
    }
    Ok(())
}

fn add_stat_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.stat_be_change() {
        let mut dependencies = HashSet::new();
        dependencies.insert(FeWorkIdentifier::InitStaticMetadata.into());

        let id: AnyWorkId = BeWorkIdentifier::Stat.into();
        workload.insert(
            id.clone(),
            Job {
                work: create_stat_work().into(),
                dependencies,
                read_access: ReadAccess::Dependencies,
                write_access: Access::one(id),
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Stat);
    }
    Ok(())
}

fn add_fvar_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.fvar_be_change() {
        let mut dependencies = HashSet::new();
        dependencies.insert(FeWorkIdentifier::InitStaticMetadata.into());

        let id: AnyWorkId = BeWorkIdentifier::Fvar.into();
        workload.insert(
            id.clone(),
            Job {
                work: create_fvar_work().into(),
                dependencies,
                read_access: ReadAccess::Dependencies,
                write_access: Access::one(id),
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Fvar);
    }
    Ok(())
}

fn add_gvar_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    let glyphs_changed = change_detector.glyphs_changed();

    // If no glyph has changed there isn't a lot of merging to do
    if !glyphs_changed.is_empty() {
        let mut dependencies: HashSet<_> = glyphs_changed
            .iter()
            .map(|gn| BeWorkIdentifier::GvarFragment(gn.clone()).into())
            .collect();
        dependencies.insert(FeWorkIdentifier::FinalizeStaticMetadata.into());

        let id: AnyWorkId = BeWorkIdentifier::Gvar.into();
        workload.insert(
            id.clone(),
            Job {
                work: create_gvar_work().into(),
                dependencies,
                // We need to read all gvar fragments, even unchanged ones, plus static metadata
                read_access: ReadAccess::custom(|id| {
                    matches!(id, AnyWorkId::Be(BeWorkIdentifier::GvarFragment(..)))
                }),
                write_access: Access::one(id),
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Gvar);
    }
    Ok(())
}

fn add_cmap_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    let glyphs_changed = change_detector.glyphs_changed();

    // If no glyph has changed there isn't a lot of merging to do
    if !glyphs_changed.is_empty() {
        let mut dependencies: HashSet<_> = glyphs_changed
            .iter()
            .map(|gn| FeWorkIdentifier::Glyph(gn.clone()).into())
            .collect();
        dependencies.insert(FeWorkIdentifier::FinalizeStaticMetadata.into());

        let id: AnyWorkId = BeWorkIdentifier::Cmap.into();
        workload.insert(
            id.clone(),
            Job {
                work: create_cmap_work().into(),
                dependencies,
                // We need to read all glyph IR, even unchanged ones, plus static metadata
                read_access: ReadAccess::custom(|id| {
                    matches!(id, AnyWorkId::Fe(FeWorkIdentifier::Glyph(..)))
                }),
                write_access: Access::one(id),
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Cmap);
    }
    Ok(())
}

fn add_post_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.post_be_change() {
        let mut dependencies = HashSet::new();
        dependencies.insert(FeWorkIdentifier::FinalizeStaticMetadata.into());

        let id: AnyWorkId = BeWorkIdentifier::Post.into();
        workload.insert(
            id.clone(),
            Job {
                work: create_post_work().into(),
                dependencies,
                read_access: ReadAccess::Dependencies,
                write_access: Access::one(id),
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Post);
    }
    Ok(())
}

fn add_head_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.final_static_metadata_ir_change() {
        let mut dependencies = HashSet::new();
        dependencies.insert(FeWorkIdentifier::FinalizeStaticMetadata.into());
        dependencies.insert(BeWorkIdentifier::Glyf.into());
        dependencies.insert(BeWorkIdentifier::LocaFormat.into());

        let id: AnyWorkId = BeWorkIdentifier::Head.into();
        workload.insert(
            id.clone(),
            Job {
                work: create_head_work().into(),
                dependencies,
                read_access: ReadAccess::Dependencies,
                write_access: Access::one(id),
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Head);
    }
    Ok(())
}

fn add_name_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.init_static_metadata_ir_change() {
        let mut dependencies = HashSet::new();
        dependencies.insert(FeWorkIdentifier::InitStaticMetadata.into());

        let id: AnyWorkId = BeWorkIdentifier::Name.into();
        workload.insert(
            id.clone(),
            Job {
                work: create_name_work().into(),
                dependencies,
                read_access: ReadAccess::Dependencies,
                write_access: Access::one(id),
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Name);
    }
    Ok(())
}

fn add_os2_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    if change_detector.init_static_metadata_ir_change() {
        let mut dependencies = HashSet::new();
        dependencies.insert(BeWorkIdentifier::Features.into());
        dependencies.insert(FeWorkIdentifier::FinalizeStaticMetadata.into());
        dependencies.insert(FeWorkIdentifier::GlobalMetrics.into());
        dependencies.insert(BeWorkIdentifier::Hhea.into());
        dependencies.insert(BeWorkIdentifier::Hmtx.into());
        dependencies.insert(BeWorkIdentifier::Gpos.into());
        dependencies.insert(BeWorkIdentifier::Gsub.into());

        let id: AnyWorkId = BeWorkIdentifier::Os2.into();
        workload.insert(
            id.clone(),
            Job {
                work: create_os2_work().into(),
                dependencies,
                // We want to read all the glyphs, which must be done if hmtx is done, for codepoints
                read_access: ReadAccess::custom(|id| {
                    matches!(
                        id,
                        AnyWorkId::Fe(FeWorkIdentifier::Glyph(..))
                            | AnyWorkId::Be(BeWorkIdentifier::Hhea)
                            | AnyWorkId::Be(BeWorkIdentifier::Hmtx)
                            | AnyWorkId::Be(BeWorkIdentifier::Gpos)
                            | AnyWorkId::Be(BeWorkIdentifier::Gsub)
                    )
                }),
                write_access: Access::one(id),
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Os2);
    }
    Ok(())
}

fn add_metric_and_limits_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    let glyphs_changed = change_detector.glyphs_changed();

    // If no glyph has changed there isn't a lot to do
    if !glyphs_changed.is_empty() {
        let mut dependencies = HashSet::new();
        dependencies.insert(FeWorkIdentifier::GlobalMetrics.into());
        dependencies.insert(FeWorkIdentifier::FinalizeStaticMetadata.into());
        // https://github.com/googlefonts/fontmake-rs/issues/285: we need bboxes and those are done for glyf
        // since we depend on glyf we needn't block on any individual glyphs
        dependencies.insert(BeWorkIdentifier::Glyf.into());
        // We want to update head with glyph extents so await the first pass at head
        dependencies.insert(BeWorkIdentifier::Head.into());

        let id: AnyWorkId = BeWorkIdentifier::Hmtx.into();
        workload.insert(
            id,
            Job {
                work: create_metric_and_limit_work().into(),
                dependencies,
                // We need to read all FE and BE glyphs, even unchanged ones, plus static metadata
                read_access: ReadAccess::custom(|id| {
                    matches!(
                        id,
                        AnyWorkId::Fe(FeWorkIdentifier::Glyph(..))
                            | AnyWorkId::Be(BeWorkIdentifier::GlyfFragment(..))
                            | AnyWorkId::Be(BeWorkIdentifier::Maxp)
                            | AnyWorkId::Be(BeWorkIdentifier::Head)
                    )
                }),
                write_access: Access::custom(|id| {
                    matches!(
                        id,
                        AnyWorkId::Be(BeWorkIdentifier::Hmtx)
                            | AnyWorkId::Be(BeWorkIdentifier::Hhea)
                            | AnyWorkId::Be(BeWorkIdentifier::Maxp)
                            | AnyWorkId::Be(BeWorkIdentifier::Head)
                    )
                }),
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Hhea);
        workload.mark_success(BeWorkIdentifier::Hmtx);
        workload.mark_success(BeWorkIdentifier::Maxp);
    }
    Ok(())
}

fn add_font_be_job(
    change_detector: &mut ChangeDetector,
    workload: &mut Workload,
) -> Result<(), Error> {
    let glyphs_changed = change_detector.glyphs_changed();

    // If glyphs or features changed we better do the thing
    if !glyphs_changed.is_empty() || change_detector.feature_be_change() {
        let mut dependencies = HashSet::new();
        dependencies.insert(FeWorkIdentifier::FinalizeStaticMetadata.into());
        dependencies.insert(BeWorkIdentifier::Features.into());
        dependencies.insert(BeWorkIdentifier::Avar.into());
        dependencies.insert(BeWorkIdentifier::Cmap.into());
        dependencies.insert(BeWorkIdentifier::Fvar.into());
        dependencies.insert(BeWorkIdentifier::Glyf.into());
        dependencies.insert(BeWorkIdentifier::Gpos.into());
        dependencies.insert(BeWorkIdentifier::Gsub.into());
        dependencies.insert(BeWorkIdentifier::Gvar.into());
        dependencies.insert(BeWorkIdentifier::Head.into());
        dependencies.insert(BeWorkIdentifier::Hhea.into());
        dependencies.insert(BeWorkIdentifier::Hmtx.into());
        dependencies.insert(BeWorkIdentifier::Loca.into());
        dependencies.insert(BeWorkIdentifier::LocaFormat.into());
        dependencies.insert(BeWorkIdentifier::Maxp.into());
        dependencies.insert(BeWorkIdentifier::Name.into());
        dependencies.insert(BeWorkIdentifier::Os2.into());
        dependencies.insert(BeWorkIdentifier::Post.into());
        dependencies.insert(BeWorkIdentifier::Stat.into());

        let id: AnyWorkId = BeWorkIdentifier::Font.into();
        workload.insert(
            id.clone(),
            Job {
                work: create_font_work().into(),
                dependencies,
                read_access: ReadAccess::Dependencies,
                write_access: Access::one(id),
            },
        );
    } else {
        workload.mark_success(BeWorkIdentifier::Font);
    }
    Ok(())
}

fn add_glyph_be_job(workload: &mut Workload, fe_root: &FeContext, glyph_name: GlyphName) {
    let glyph_ir = fe_root.get_glyph_ir(&glyph_name);

    // To build a glyph we need it's components, plus static metadata
    let mut dependencies: HashSet<_> = glyph_ir
        .sources()
        .values()
        .flat_map(|s| &s.components)
        .map(|c| AnyWorkId::Fe(FeWorkIdentifier::Glyph(c.base.clone())))
        .collect();
    dependencies.insert(FeWorkIdentifier::FinalizeStaticMetadata.into());

    let id = AnyWorkId::Be(BeWorkIdentifier::GlyfFragment(glyph_name.clone()));
    let gvar_id = AnyWorkId::Be(BeWorkIdentifier::GvarFragment(glyph_name.clone()));

    // this job should already be a dependency of glyf, gvar, and hmtx; if not terrible things will happen
    if !workload.is_dependency(&BeWorkIdentifier::Glyf.into(), &id) {
        panic!("BE glyph '{glyph_name}' is being built but not participating in glyf",);
    }
    if !workload.is_dependency(&BeWorkIdentifier::Gvar.into(), &gvar_id) {
        panic!("BE glyph '{glyph_name}' is being built but not participating in gvar",);
    }

    let write_access = Access::Set(HashSet::from([id.clone(), gvar_id]));
    workload.insert(
        id,
        Job {
            work: create_glyf_work(glyph_name).into(),
            dependencies,
            read_access: ReadAccess::Dependencies,
            write_access,
        },
    );
}

//FIXME: I should be a method on ChangeDetector
pub fn create_workload(change_detector: &mut ChangeDetector) -> Result<Workload, Error> {
    let mut workload = Workload::new();

    // FE: f(source) => IR
    add_init_static_metadata_ir_job(change_detector, &mut workload)?;
    add_global_metric_ir_job(change_detector, &mut workload)?;
    add_feature_ir_job(change_detector, &mut workload)?;
    add_kerning_ir_job(change_detector, &mut workload)?;
    add_glyph_ir_jobs(change_detector, &mut workload)?;
    add_finalize_static_metadata_ir_job(change_detector, &mut workload)?;

    // BE: f(IR, maybe other BE work) => binary
    add_feature_be_job(change_detector, &mut workload)?;
    add_glyf_loca_be_job(change_detector, &mut workload)?;
    add_avar_be_job(change_detector, &mut workload)?;
    add_stat_be_job(change_detector, &mut workload)?;
    add_cmap_be_job(change_detector, &mut workload)?;
    add_fvar_be_job(change_detector, &mut workload)?;
    add_gvar_be_job(change_detector, &mut workload)?;
    add_head_be_job(change_detector, &mut workload)?;
    add_metric_and_limits_job(change_detector, &mut workload)?;
    add_name_be_job(change_detector, &mut workload)?;
    add_os2_be_job(change_detector, &mut workload)?;
    add_post_be_job(change_detector, &mut workload)?;

    // Make a damn font
    add_font_be_job(change_detector, &mut workload)?;

    Ok(workload)
}

#[cfg(test)]
mod tests {

    use std::{
        collections::HashSet,
        fs::{self, File},
        io::Read,
        path::{Path, PathBuf},
        str::FromStr,
    };

    use chrono::{Duration, TimeZone, Utc};
    use fontbe::orchestration::{
        loca_format_from_file, AnyWorkId, Context as BeContext, LocaFormat,
        WorkId as BeWorkIdentifier,
    };
    use fontdrasil::types::GlyphName;
    use fontir::{
        ir::{self, KernParticipant},
        orchestration::{Context as FeContext, WorkId as FeWorkIdentifier},
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
                glyf::{self, CompositeGlyph, CurvePoint, Glyf, SimpleGlyph},
                hmtx::Hmtx,
                loca::Loca,
            },
            types::F2Dot14,
            FontData, FontRead, FontReadWithArgs, FontRef, TableProvider,
        },
        GlyphId, Tag,
    };
    use tempfile::{tempdir, TempDir};
    use write_fonts::dump_table;

    use super::*;

    struct TestCompile {
        build_dir: PathBuf,
        work_completed: HashSet<AnyWorkId>,
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
                work_completed: HashSet::new(),
                glyphs_changed: change_detector.glyphs_changed(),
                glyphs_deleted: change_detector.glyphs_deleted(),
                fe_context,
                be_context,
            }
        }

        fn get_glyph_index(&self, name: &str) -> u32 {
            self.fe_context
                .get_final_static_metadata()
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
                loca_format: loca_format_from_file(&build_dir.join("loca.format")),
                raw_glyf: read_file(&build_dir.join("glyf.table")),
                raw_loca: read_file(&build_dir.join("loca.table")),
            }
        }

        fn read(&self) -> Vec<glyf::Glyph> {
            let glyf = Glyf::read(FontData::new(&self.raw_glyf)).unwrap();
            let loca = Loca::read_with_args(
                FontData::new(&self.raw_loca),
                &(self.loca_format == LocaFormat::Long),
            )
            .unwrap();
            (0..loca.len())
                .map(|gid| loca.get_glyf(GlyphId::new(gid as u16), &glyf))
                .map(|r| r.unwrap().unwrap())
                .collect()
        }
    }

    fn compile(args: Args) -> TestCompile {
        let _ = env_logger::builder().is_test(true).try_init();

        info!("Compile {args:?}");

        let (ir_paths, be_paths) = init_paths(&args).unwrap();
        let config = Config::new(args).unwrap();

        let prev_inputs = config.init().unwrap();

        let mut change_detector =
            ChangeDetector::new(config.clone(), ir_paths.clone(), prev_inputs).unwrap();

        let mut workload = Workload::new();

        add_init_static_metadata_ir_job(&mut change_detector, &mut workload).unwrap();
        add_global_metric_ir_job(&mut change_detector, &mut workload).unwrap();
        add_finalize_static_metadata_ir_job(&mut change_detector, &mut workload).unwrap();
        add_glyph_ir_jobs(&mut change_detector, &mut workload).unwrap();
        add_feature_ir_job(&mut change_detector, &mut workload).unwrap();
        add_kerning_ir_job(&mut change_detector, &mut workload).unwrap();
        add_feature_be_job(&mut change_detector, &mut workload).unwrap();

        add_glyf_loca_be_job(&mut change_detector, &mut workload).unwrap();
        add_avar_be_job(&mut change_detector, &mut workload).unwrap();
        add_stat_be_job(&mut change_detector, &mut workload).unwrap();
        add_cmap_be_job(&mut change_detector, &mut workload).unwrap();
        add_fvar_be_job(&mut change_detector, &mut workload).unwrap();
        add_gvar_be_job(&mut change_detector, &mut workload).unwrap();
        add_head_be_job(&mut change_detector, &mut workload).unwrap();
        add_metric_and_limits_job(&mut change_detector, &mut workload).unwrap();
        add_name_be_job(&mut change_detector, &mut workload).unwrap();
        add_os2_be_job(&mut change_detector, &mut workload).unwrap();
        add_post_be_job(&mut change_detector, &mut workload).unwrap();

        add_font_be_job(&mut change_detector, &mut workload).unwrap();

        // Try to do the work
        // As we currently don't stress dependencies just run one by one
        // This will likely need to change when we start doing things like glyphs with components

        let fe_root = FeContext::new_root(
            config.args.flags(),
            ir_paths,
            change_detector.current_inputs().clone(),
        );
        let be_root = BeContext::new_root(config.args.flags(), be_paths, &fe_root.read_only());
        let mut result = TestCompile::new(
            &change_detector,
            fe_root.read_only(),
            be_root.copy_read_only(),
        );
        let completed = workload.run_for_test(&fe_root, &be_root);

        change_detector.finish_successfully().unwrap();
        result.work_completed = completed;

        write_font_file(&config.args, &be_root).unwrap();

        result
    }

    #[test]
    fn compile_work() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(Args::for_test(build_dir, "wght_var.designspace"));
        let mut completed = result.work_completed.iter().cloned().collect::<Vec<_>>();
        completed.sort();
        assert_eq!(
            vec![
                AnyWorkId::Fe(FeWorkIdentifier::InitStaticMetadata),
                FeWorkIdentifier::GlobalMetrics.into(),
                FeWorkIdentifier::Glyph("bar".into()).into(),
                FeWorkIdentifier::Glyph("plus".into()).into(),
                FeWorkIdentifier::FinalizeStaticMetadata.into(),
                FeWorkIdentifier::Features.into(),
                FeWorkIdentifier::Kerning.into(),
                BeWorkIdentifier::Features.into(),
                BeWorkIdentifier::Avar.into(),
                BeWorkIdentifier::Cmap.into(),
                BeWorkIdentifier::Font.into(),
                BeWorkIdentifier::Fvar.into(),
                BeWorkIdentifier::Glyf.into(),
                BeWorkIdentifier::GlyfFragment("bar".into()).into(),
                BeWorkIdentifier::GlyfFragment("plus".into()).into(),
                BeWorkIdentifier::Gpos.into(),
                BeWorkIdentifier::Gsub.into(),
                BeWorkIdentifier::Gvar.into(),
                BeWorkIdentifier::GvarFragment("bar".into()).into(),
                BeWorkIdentifier::GvarFragment("plus".into()).into(),
                BeWorkIdentifier::Head.into(),
                BeWorkIdentifier::Hhea.into(),
                BeWorkIdentifier::Hmtx.into(),
                BeWorkIdentifier::Loca.into(),
                BeWorkIdentifier::LocaFormat.into(),
                BeWorkIdentifier::Maxp.into(),
                BeWorkIdentifier::Name.into(),
                BeWorkIdentifier::Os2.into(),
                BeWorkIdentifier::Post.into(),
                BeWorkIdentifier::Stat.into(),
            ],
            completed
        );
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
        assert_eq!(HashSet::new(), result.work_completed);
        assert_eq!(IndexSet::new(), result.glyphs_changed);
        assert_eq!(IndexSet::new(), result.glyphs_deleted);
    }

    #[test]
    fn second_compile_only_glyph() {
        // glyph depends on static metadata, which isn't going to run
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();

        let result = compile(Args::for_test(build_dir, "wght_var.designspace"));
        assert!(result.work_completed.len() > 1);

        fs::remove_file(build_dir.join("glyph_ir/bar.yml")).unwrap();

        let result = compile(Args::for_test(build_dir, "wght_var.designspace"));
        let mut completed = result.work_completed.iter().cloned().collect::<Vec<_>>();
        completed.sort();
        assert_eq!(
            vec![
                AnyWorkId::Fe(FeWorkIdentifier::Glyph("bar".into())),
                BeWorkIdentifier::Cmap.into(),
                BeWorkIdentifier::Font.into(),
                BeWorkIdentifier::Glyf.into(),
                BeWorkIdentifier::GlyfFragment("bar".into()).into(),
                BeWorkIdentifier::Gvar.into(),
                BeWorkIdentifier::GvarFragment("bar".into()).into(),
                BeWorkIdentifier::Hhea.into(),
                BeWorkIdentifier::Hmtx.into(),
                BeWorkIdentifier::Loca.into(),
                BeWorkIdentifier::LocaFormat.into(),
                BeWorkIdentifier::Maxp.into(),
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

    #[test]
    fn compile_fea() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        compile(Args::for_test(build_dir, "static.designspace"));

        let font_file = build_dir.join("font.ttf");
        assert!(font_file.exists());
        let buf = fs::read(font_file).unwrap();
        let font = FontRef::new(&buf).unwrap();

        assert!(font.gpos().is_ok());
        assert!(font.gsub().is_ok());
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
            .get_glyph_ir(&"contour_and_component".into());

        assert_eq!(1, glyph.sources().len());
        (*glyph).clone()
    }

    fn read_file(path: &Path) -> Vec<u8> {
        assert!(path.exists(), "{path:?} not found");
        let mut buf = Vec::new();
        File::open(path).unwrap().read_to_end(&mut buf).unwrap();
        buf
    }

    fn glyph_glyf_bytes(build_dir: &Path, name: &str) -> Vec<u8> {
        read_file(&build_dir.join(format!("glyphs/{name}.glyf")))
    }

    #[test]
    fn resolve_contour_and_composite_glyph_in_non_legacy_mode() {
        let temp_dir = tempdir().unwrap();
        let glyph = build_contour_and_composite_glyph(&temp_dir, false);
        assert!(glyph.default_instance().contours.is_empty(), "{glyph:?}");
        assert_eq!(2, glyph.default_instance().components.len(), "{glyph:?}");

        let raw_glyph = glyph_glyf_bytes(temp_dir.path(), glyph.name.as_str());
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

        let raw_glyph = glyph_glyf_bytes(temp_dir.path(), glyph.name.as_str());
        let glyph = SimpleGlyph::read(FontData::new(&raw_glyph)).unwrap();
        assert_eq!(2, glyph.number_of_contours());
    }

    #[test]
    fn compile_simple_binary_glyph() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        compile(Args::for_test(build_dir, "static.designspace"));

        let raw_glyph = glyph_glyf_bytes(build_dir, "bar");
        let glyph = SimpleGlyph::read(FontData::new(&raw_glyph)).unwrap();
        assert_eq!(1, glyph.number_of_contours());
        assert_eq!(4, glyph.num_points());
        assert_eq!(
            [222, -241, 295, 760],
            [glyph.x_min(), glyph.y_min(), glyph.x_max(), glyph.y_max()]
        );
    }

    #[test]
    fn compile_simple_glyphs_to_glyf_loca() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, "static.designspace"));

        // See resources/testdata/Static-Regular.ufo/glyphs
        // space, 0 points, 0 contour
        // bar, 4 points, 1 contour
        // plus, 12 points, 1 contour
        // element of, 0 points, 0 contours
        assert_eq!(
            vec![(0, 0), (4, 1), (12, 1), (0, 0)],
            result
                .glyphs()
                .read()
                .iter()
                .map(|g| match g {
                    glyf::Glyph::Simple(glyph) => (glyph.num_points(), glyph.number_of_contours()),
                    glyf::Glyph::Composite(glyph) => (0, glyph.number_of_contours()),
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
        let glyf::Glyph::Simple(glyph) = uppercase_o else {
            panic!("Expected 'O' to be a simple glyph, got {:?}", uppercase_o);
        };
        assert_eq!(2, glyph.number_of_contours());
        assert_eq!(36, glyph.num_points());
        assert_eq!(
            [48, -9, 491, 817],
            [glyph.x_min(), glyph.y_min(), glyph.x_max(), glyph.y_max()]
        );
        // for brevity, we only check the first 5 points to confirm that we now get
        // multiple off-curve points in a row with impliable on-curve points omitted
        assert_eq!(
            vec![
                CurvePoint::on_curve(270, -9),
                CurvePoint::off_curve(352, -9),
                CurvePoint::off_curve(448, 56),
                CurvePoint::off_curve(491, 174),
                CurvePoint::on_curve(491, 253),
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
        let glyphs = glyph_data.read();
        assert!(glyphs.len() > 1, "{glyphs:#?}");
        let period_idx = result.get_glyph_index("period");
        assert!(matches!(glyphs[0], glyf::Glyph::Simple(..)), "{glyphs:#?}");
        for (idx, glyph) in glyphs.iter().enumerate() {
            if idx == period_idx.try_into().unwrap() {
                assert!(
                    matches!(glyphs[idx], glyf::Glyph::Simple(..)),
                    "glyphs[{idx}] should be simple\n{glyph:#?}\nAll:\n{glyphs:#?}"
                );
            } else {
                assert!(
                    matches!(glyphs[idx], glyf::Glyph::Composite(..)),
                    "glyphs[{idx}] should be composite\n{glyph:#?}\nAll:\n{glyphs:#?}"
                );
            }
        }
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
        let glyf::Glyph::Composite(glyph) = &glyphs[non_uniform_scale_idx as usize] else {
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
        let glyf::Glyph::Composite(glyph) = &glyphs[gid as usize] else {
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
    fn writes_cmap() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let result = compile(Args::for_test(build_dir, "glyphs2/Component.glyphs"));

        let raw_cmap = dump_table(&*result.be_context.get_cmap()).unwrap();
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
                    (0x002C, 1),
                    (0x002E, 0),
                    (0x0030, 2),
                    (0x031, 3),
                    (0x032, 4)
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

        let raw_hmtx = result.be_context.get_hmtx();
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

        let hhea = result.be_context.get_hhea();
        assert_eq!(1, hhea.number_of_long_metrics);
        assert_eq!(175, hhea.min_left_side_bearing.to_i16());
        assert_eq!(50, hhea.min_right_side_bearing.to_i16());
        assert_eq!(375, hhea.x_max_extent.to_i16());
        assert_eq!(425, hhea.advance_width_max.to_u16());

        let maxp = result.be_context.get_maxp();
        assert_eq!(3, maxp.num_glyphs);
        assert_eq!(Some(4), maxp.max_points);
        assert_eq!(Some(1), maxp.max_contours);

        let raw_hmtx = result.be_context.get_hmtx();
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
                Tag::new(b"OS/2"),
                Tag::new(b"STAT"),
                Tag::new(b"avar"),
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
                GlyphId::new(0),
                GlyphId::new(1),
                GlyphId::new(2),
                GlyphId::new(5)
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

        let kerning = result.fe_context.get_kerning();

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
                            assert_eq!(loc.axis_names().count(), 1, "Should be only weight");
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
                    ("public.kern1.brackets", vec!["bracketleft", "bracketright"],),
                    ("public.kern2.brackets", vec!["bracketleft", "bracketright"],)
                ],
                vec![
                    (
                        &KernParticipant::Glyph("bracketleft".into()),
                        &KernParticipant::Glyph("bracketright".into()),
                        vec![
                            ("Weight 0".to_string(), -300.0),
                            ("Weight 1".to_string(), -150.0)
                        ],
                    ),
                    (
                        &KernParticipant::Glyph("exclam".into()),
                        &KernParticipant::Glyph("exclam".into()),
                        vec![
                            ("Weight 0".to_string(), -360.0),
                            ("Weight 1".to_string(), -100.0)
                        ],
                    ),
                    (
                        &KernParticipant::Glyph("exclam".into()),
                        &KernParticipant::Glyph("hyphen".into()),
                        vec![("Weight 0".to_string(), 20.0),],
                    ),
                    (
                        &KernParticipant::Glyph("exclam".into()),
                        &KernParticipant::Group("public.kern2.brackets".into()),
                        vec![("Weight 0".to_string(), -160.0),],
                    ),
                    (
                        &KernParticipant::Glyph("hyphen".into()),
                        &KernParticipant::Glyph("hyphen".into()),
                        vec![
                            ("Weight 0".to_string(), -150.0),
                            ("Weight 1".to_string(), -50.0)
                        ],
                    ),
                    (
                        &KernParticipant::Group("public.kern1.brackets".into()),
                        &KernParticipant::Glyph("exclam".into()),
                        vec![("Weight 0".to_string(), -165.0),],
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
}
