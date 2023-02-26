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
    ptr: *Vec<T>,
    tup: (String, path::to::Type, !, ()),
}

// pub use ds::Vector;

";

const FILE_NAME: &str = "main.hug";

fn main() {
    match parse::parse(CODE, FILE_NAME) {
        Ok(unit) => {
            for item in unit {
                println!("{item:#?}");
                item.span().println();
            }
        }
        Err(error) => error.println(),
    }
}
