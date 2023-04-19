mod ast;
mod ast_lowering;
mod error;
mod hir;
mod io;
mod lex;
mod parse;
mod span;

use std::collections::HashMap;
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

    let tree = map.parse_all(main_path)?;
    println!("self: {tree:#?}");

    let std = map.parse_std()?;
    println!("std: {std:#?}");

    let other_trees = HashMap::from([("std".to_string(), std)]);
    let lowered = ast_lowering::lower(tree, other_trees)?;
    println!("lowered: {lowered:#?}");

    todo!()
}
