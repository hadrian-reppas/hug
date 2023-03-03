mod ast;
mod error;
mod lex;
mod parse;
mod span;

const FILE_PATH: &str = "test/main.hug";

fn main() {
    let code = std::fs::read_to_string(FILE_PATH).unwrap();
    match parse::parse(&code, FILE_PATH) {
        Ok(unit) => {
            for item in unit {
                println!("{:#?}", item);
                item.span().println();
            }
        }
        Err(error) => {
            println!("{:#?}", error);
            error.println();
        }
    }
}
