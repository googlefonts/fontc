use std::{env, ffi::OsStr, fs, path::PathBuf};

mod lexer;

fn main() {
    let args = Args::get_from_env_or_exit();
    let contents = fs::read_to_string(&args.path)
        .expect("file read failed");
    let (tokens, _) = lexer::tokenize(&contents);
    eprintln!("{}", lexer::debug_tokens(&tokens));
}

// microbenchmarks to do, one day

//fn is_special_match(byte: u8) -> bool {
//match byte {
//b';' | b',' | b'@' | b'\\' | b'-' | b'=' | b'{' | b'}' | b'[' | b']' | b'(' | b')'
//| b'<' | b'>' | b'\'' => true,
//_ => false,
//}
//}

//fn is_special_ranges(byte: u8) -> bool {
//(39..=45).contains(&byte)
//|| (59..=64).contains(&byte)
//|| (91..=93).contains(&byte)
//|| byte == 123
//|| byte == 125
//}

//fn is_special_bsearch(byte: u8) -> bool {
//[39, 40, 41, 44, 45, 59, 60, 61, 62, 64, 91, 92, 93, 123, 125]
//.binary_search(&byte)
//.is_ok()
//}

//// could improve this by sorting by frequency
//fn is_special_linear_scan(byte: u8) -> bool {
//[39, 40, 41, 44, 45, 59, 60, 61, 62, 64, 91, 92, 93, 123, 125].contains(&byte)
//}

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
