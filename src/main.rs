mod ast;
mod error;
mod lex;
mod parse;
mod span;

const CODE: &str = "
use std::collections::HashMap;
use std::fs::{
    io,
    vec::Vec as Vector,
    File,
};

pub struct Vec<T> {

}

// pub use ds::Vector;

";

const FILE_NAME: &str = "main.hug";

fn main() {
    let mut tokens = lex::Tokens::new(CODE, FILE_NAME);
    match parse::unit(&mut tokens) {
        Ok(unit) => {
            for item in unit {
                println!("{item:#?}");
                item.span().println();
            }
        }
        Err(error) => error.println(),
    }
}
