mod ast;
mod error;
mod lex;
mod parse;
mod span;

const CODE: &str = r#"
use std::collections::HashMap;
use std::fs::{
    io,
    vec::Vec as Vector,
    File,
};

pub struct Vec<T> {
    ptr: *Vec<T>,
    tup: (String, path::to::Type, !, ()),
    pub count: u8<Self, fn(!,) -> (!,),>,
}

pub enum Option<T> {
    Some(T,),
    None,
}

type MyResult<T> = Result<T, LongErrorType>;

mod math;
pub mod helpers;
pub mod math @ "misc/math.hug";

extern {
    type Module;
    pub type Function;

    global COUNTER: u8;
    pub global FLAG: bool;

    fn take<T>(*self, x: u8, y: Vec<T>,) -> T
    where
        Self: Fn(u8) -> () + From<T>,
        T: path::to::Trait<> + Clone;
}

// pub use ds::Vector;

"#;

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
