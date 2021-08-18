use std::{env, ffi::OsStr, fs, path::PathBuf};

mod lexer;

fn main() {
    let args = Args::get_from_env_or_exit();
    let contents = fs::read_to_string(&args.path).expect("file read failed");
    let tokens = lexer::tokenize(&contents);
    for repr in lexer::debug_tokens(&tokens) {
        println!("{}", repr);
    }
    //eprintln!("{}", lexer::debug_tokens(&tokens));
}

macro_rules! exit_err {
    ($($arg:tt)*) => ({
        eprintln!($($arg)*);
        //eprintln!("{}", HELP);
        std::process::exit(1);
    })
}

struct Args {
    path: PathBuf,
}

impl Args {
    fn get_from_env_or_exit() -> Self {
        let mut args = env::args().skip(1);
        let path = match args.next().map(PathBuf::from) {
            Some(ref p) if p.exists() && p.extension() == Some(OsStr::new("fea")) => p.to_owned(),
            Some(ref p) => exit_err!("path {:?} is not an existing .fea file, exiting", p),
            None => exit_err!("Please supply a path to a .fea file"),
        };

        Args { path }
    }
}
