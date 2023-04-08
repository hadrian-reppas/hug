use std::io::{self, Write};

use crate::io::FileMap;
use crate::span::Span;
use crate::{color, reset};

#[derive(Debug)]
pub enum Error {
    Lex(String, Span, Vec<Note>),
    Parse(String, Span, Vec<Note>),
    Io(String, Option<Span>, Vec<Note>),
}

impl Error {
    pub fn print(&self, map: &FileMap) {
        self.write(&mut io::stdout(), true, map).unwrap();
    }

    pub fn println(&self, map: &FileMap) {
        self.print(map);
        println!();
    }

    pub fn write<W: Write>(&self, out: &mut W, color: bool, map: &FileMap) -> io::Result<()> {
        match self {
            Error::Lex(msg, span, notes) => {
                writeln!(
                    out,
                    "{}lex error:{} {}",
                    color!(Red, color),
                    reset!(color),
                    msg
                )?;

                span.write(out, color, map)?;

                for note in notes {
                    writeln!(out)?;
                    note.write(out, color, map)?;
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

                span.write(out, color, map)?;

                for note in notes {
                    writeln!(out)?;
                    note.write(out, color, map)?;
                }
                Ok(())
            }
            Error::Io(msg, span, notes) => {
                write!(
                    out,
                    "{}io error:{} {}",
                    color!(Red, color),
                    reset!(color),
                    msg
                )?;

                if let Some(span) = span {
                    writeln!(out)?;
                    span.write(out, color, map)?;
                }

                for note in notes {
                    writeln!(out)?;
                    note.write(out, color, map)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
pub struct Note {
    msg: String,
    span: Option<Span>,
}

impl Note {
    pub fn new(msg: String, span: Option<Span>) -> Self {
        Note { msg, span }
    }

    fn write<W: Write>(&self, out: &mut W, color: bool, map: &FileMap) -> io::Result<()> {
        write!(
            out,
            "{}note:{} {}",
            color!(Blue, color),
            reset!(color),
            self.msg
        )?;

        if let Some(span) = self.span {
            writeln!(out)?;
            span.write(out, color, map)?;
        }
        Ok(())
    }
}
