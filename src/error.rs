use std::io::{self, Write};

use crate::span::Span;
use crate::{color, reset};

#[derive(Debug)]
pub enum Error<'a, 'b> {
    Lex(String, Span<'a, 'b>, Vec<Note<'a, 'b>>),
    Parse(String, Span<'a, 'b>, Vec<Note<'a, 'b>>),
}

impl Error<'_, '_> {
    pub fn print(&self) {
        self.write(&mut io::stdout(), true).unwrap();
    }

    pub fn println(&self) {
        self.print();
        println!();
    }

    pub fn write<W: Write>(&self, out: &mut W, color: bool) -> io::Result<()> {
        match self {
            Error::Lex(msg, span, notes) => {
                writeln!(
                    out,
                    "{}lex error:{} {}",
                    color!(Red, color),
                    reset!(color),
                    msg
                )?;

                span.write(out, color)?;

                for note in notes {
                    writeln!(out)?;
                    note.write(out, color)?;
                }
                Ok(())
            }
            Error::Parse(msg, span, notes) => {
                writeln!(
                    out,
                    "{}parse error:{} {}",
                    color!(Red, color),
                    reset!(color),
                    msg
                )?;

                span.write(out, color)?;

                for note in notes {
                    writeln!(out)?;
                    note.write(out, color)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
pub struct Note<'a, 'b> {
    msg: String,
    span: Option<Span<'a, 'b>>,
}

impl<'a, 'b> Note<'a, 'b> {
    pub fn new(msg: String, span: Option<Span<'a, 'b>>) -> Self {
        Note { msg, span }
    }

    fn write<W: Write>(&self, out: &mut W, color: bool) -> io::Result<()> {
        write!(
            out,
            "{}note:{} {}",
            color!(Blue, color),
            reset!(color),
            self.msg
        )?;

        if let Some(span) = self.span {
            writeln!(out)?;
            span.write(out, color)?;
        }
        Ok(())
    }
}
