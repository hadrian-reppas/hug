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

extern {
    type Module;
    pub type Function;

    static COUNTER: u8;
    pub static FLAG: bool;

    fn take<T>(*self, x: u8, y: Vec<T>,) -> T
    where
        T: Fn(u8) -> () + From<T>,
        U: path::to::Trait<> + Clone,
        V = Vec<[usize; 3]>;
}

trait CloneIterator<T>: Clone + Debug
where
    T: Clone + Debug,
{
    fn next(*self) -> Option<T>; 
    fn count(self) -> usize {}
}

fn g<T>(*self) where T: Clone {}
pub fn f(x: u32, _: u64) {
    let Struct { x, y: Some(x, _), .. };
    let path::to::Enum::Variant(S { .. }, _,);
}

impl Vec<T> as Fn(usize) -> T where T: Clone {
    fn call(*self, index: usize) -> T {}
}

impl Vec<T> as Hash where T: Hash {
    fn hash(*self) -> u64 {}
}

fn compose<F, G, A, B, C>(f: F, g: G, a: A) -> C
where
    F: Fn(A) -> B,
    G: Fn(B) -> C,
{}

pub static COUNTER: usize;
static COUNTER: usize;
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
        Err(error) => dbg!(error).println(),
    }
}
