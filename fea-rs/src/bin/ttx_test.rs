//! Run the compiler against a bunch of inputs, comparing them with
//! the results of fonttools.

use fea_rs::util::ttx;

static TEST_DATA: &str = "./fea-rs/test-data/fonttools-tests";

fn main() {
    let args = flags::Args::from_env().unwrap();

    if let Err(err) = ttx::run_all_tests(TEST_DATA, args.test.as_ref()) {
        eprintln!("{:?}", err.printer(args.verbose));
        std::process::exit(1);
    }
}

mod flags {
    xflags::xflags! {

        /// Compile a fea file into a source font
        cmd args {
            optional -v, --verbose
            /// Optional comma separated list of words matching tests to run.
            ///
            /// e.g.: -t "spec5,GPOS" matches spec5h1.fea, spec5fi2.fea, GPOS_2.fea, etc
            optional -t, --test test_filter: String
        }
    }
}
