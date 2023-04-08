mod ast;
mod error;
mod io;
mod lex;
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

    let main_unit = get_unit(main_path, map)?;

    for item in main_unit {
        println!("{:#?}", item);
        item.span().println(map);
    }

    todo!()
}

fn get_unit(path: PathBuf, map: &mut io::FileMap) -> Result<Vec<ast::Item>, error::Error> {
    let id = map.load(path)?;
    let code = map.get_code(id);
    let unit = parse::parse(code, id)?;
    Ok(unit)
}
