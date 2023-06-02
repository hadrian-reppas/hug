mod ast;
mod error;
mod hir;
mod io;
mod lex;
mod parse;
mod resolve;
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
    let mut args = env::args();
    let main_path: PathBuf = if args.len() == 2 {
        args.next_back().unwrap().into()
    } else {
        return Err(Error::new("invalid arguments", None).note("usage is `hug <filepath>`", None));
    };

    let tree = map.parse_all(main_path)?;
    println!("crate: {tree:#?}");

    let std = map.parse_std()?;
    // println!("std: {std:#?}");

    let other_trees = HashMap::from([("std".to_string(), std)]);
    let lowered = resolve::resolve(tree, other_trees)?;
    println!("lowered: {lowered:#?}");

    todo!()
}
