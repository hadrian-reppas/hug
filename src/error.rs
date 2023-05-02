use std::io::{self, Write};

use crate::io::FileMap;
use crate::span::Span;
use crate::{color, reset};

#[derive(Debug)]
pub struct Error {
    msg: String,
    span: Option<Span>,
    notes: Vec<Note>,
}

impl Error {
    pub fn new(msg: impl Into<String>, span: Option<Span>) -> Self {
        Error {
            msg: msg.into(),
            span: span,
            notes: Vec::new(),
        }
    }

    pub fn note(mut self, msg: impl Into<String>, span: Option<Span>) -> Self {
        self.notes.push(Note {
            msg: msg.into(),
            span,
        });
        self
    }

    pub fn print(&self, map: &FileMap) {
        self.write(&mut io::stdout(), true, map).unwrap();
    }

    pub fn println(&self, map: &FileMap) {
        self.print(map);
        println!();
    }

    pub fn write<W: Write>(&self, out: &mut W, color: bool, map: &FileMap) -> io::Result<()> {
        write!(
            out,
            "{}error:{} {}",
            color!(Red, color),
            reset!(color),
            self.msg
        )?;

        if let Some(span) = self.span {
            writeln!(out)?;
            span.write(out, color, map)?;
        }

        for note in &self.notes {
            writeln!(out)?;
            note.write(out, color, map)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct Note {
    msg: String,
    span: Option<Span>,
}

impl Note {
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
