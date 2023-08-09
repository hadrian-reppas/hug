#![allow(dead_code)]

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
    let path: PathBuf = if args.len() == 2 {
        args.next_back().unwrap().into()
    } else {
        return Err(
            error::Error::new("invalid arguments", None).note("usage is `hug <filepath>`", None)
        );
    };

    let tree = map.parse_all(path, "crate".to_string())?;
    let std = map.parse_std()?;

    let crates = HashMap::from([("crate".to_string(), tree), ("std".to_string(), std)]);
    let _resolved = resolve::resolve(&crates, map)?;

    todo!()
}
