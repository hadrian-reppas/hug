mod ast;
mod error;
mod io;
mod lex;
mod modules;
mod parse;
mod span;

use std::env;
use std::path::PathBuf;
use std::process::ExitCode;

use error::{Error, Note};

fn main() -> ExitCode {
    let mut map = io::FileMap::new();
    if let Err(err) = compile(&mut map) {
        err.println(&map);
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn compile(map: &mut io::FileMap) -> Result<(), error::Error> {
    let mut args: Vec<_> = env::args().collect();
    let main_path: PathBuf = if args.len() == 2 {
        args.pop().unwrap().into()
    } else {
        return Err(Error::Io(
            "invalid arguments".to_string(),
            None,
            vec![Note::new("usage is `hug <filepath>`".to_string(), None)],
        ));
    };

    let mut prefix = main_path.clone();
    prefix.pop();
    let main_items = map.parse(main_path)?;

    let tree = modules::collect(main_items, prefix, map)?;

    println!("{tree:#?}");

    todo!()
}
