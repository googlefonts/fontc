use std::{fs, io::Write};

use clap::Parser;

use fontbe::{orchestration::Context as BeContext, paths::Paths as BePaths};
use fontc::{Args, ChangeDetector, Config, Error};
use fontir::{orchestration::Context as FeContext, paths::Paths as IrPaths};

fn main() -> Result<(), Error> {
    env_logger::builder()
        .format(|buf, record| {
            let ts = buf.timestamp_micros();
            writeln!(
                buf,
                "{}: {:?}: {}: {}",
                ts,
                std::thread::current().id(),
                buf.default_level_style(record.level())
                    .value(record.level()),
                record.args()
            )
        })
        .init();

    let args = Args::parse();
    let ir_paths = IrPaths::new(&args.build_dir);
    let be_paths = BePaths::new(&args.build_dir);
    fontc::require_dir(ir_paths.build_dir())?;
    fontc::require_dir(ir_paths.glyph_ir_dir())?;
    // It's confusing to have leftover debug files
    if be_paths.debug_dir().is_dir() {
        fs::remove_dir_all(be_paths.debug_dir()).map_err(Error::IoError)?;
    }
    fontc::require_dir(be_paths.debug_dir())?;
    fontc::require_dir(be_paths.glyph_dir())?;
    let config = Config::new(args)?;
    let prev_inputs = config.init()?;

    let mut change_detector = ChangeDetector::new(config.clone(), ir_paths.clone(), prev_inputs)?;
    let workload = fontc::create_workload(&mut change_detector)?;

    let fe_root = FeContext::new_root(
        config.args.flags(),
        ir_paths,
        change_detector.current_inputs().clone(),
    );
    let be_root = BeContext::new_root(config.args.flags(), be_paths, &fe_root);
    workload.exec(fe_root, be_root)?;

    change_detector.finish_successfully()
}
