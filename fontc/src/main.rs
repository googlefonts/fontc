use std::io::Write;

use clap::Parser;

use fontbe::orchestration::Context as BeContext;
use fontc::{init_paths, write_font_file, Args, ChangeDetector, Config, Error};
use fontir::orchestration::Context as FeContext;

fn main() -> Result<(), Error> {
    env_logger::builder()
        .format(|buf, record| {
            let ts = buf.timestamp_micros();
            writeln!(
                buf,
                "[{ts} {:?} {} {}] {}",
                std::thread::current().id(),
                record.target(),
                buf.default_level_style(record.level())
                    .value(record.level()),
                record.args()
            )
        })
        .init();

    let args = Args::parse();
    let (ir_paths, be_paths) = init_paths(&args)?;
    let config = Config::new(args)?;
    let prev_inputs = config.init()?;

    let mut change_detector = ChangeDetector::new(config.clone(), ir_paths.clone(), prev_inputs)?;
    let workload = fontc::create_workload(&mut change_detector)?;

    let fe_root = FeContext::new_root(
        config.args.flags(),
        ir_paths,
        workload.current_inputs().clone(),
    );
    let be_root = BeContext::new_root(config.args.flags(), be_paths, &fe_root);
    workload.exec(&fe_root, &be_root)?;

    change_detector.finish_successfully()?;

    write_font_file(&config.args, &be_root)
}
