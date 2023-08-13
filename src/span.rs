use std::cmp;
use std::fmt::{self, Debug};
use std::io::{self, Write};

use crate::error::Error;
use crate::io::{CrateId, FileId, FileMap};

const MAX_LINES: usize = 4;

#[macro_export]
macro_rules! color {
    ($color:ident, $apply:expr) => {
        if $apply {
            format!(
                "{}{}",
                termion::color::Fg(termion::color::$color),
                termion::style::Bold
            )
        } else {
            String::new()
        }
    };
}

#[macro_export]
macro_rules! reset {
    ($apply:expr) => {
        if $apply {
            format!(
                "{}{}",
                termion::color::Fg(termion::color::Reset),
                termion::style::Reset
            )
        } else {
            String::new()
        }
    };
}

#[derive(Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
    pub file_id: FileId,
    pub crate_id: CrateId,
}

impl Span {
    pub fn to(mut self, other: Self) -> Self {
        self.end = other.end;
        self
    }

    pub fn by(self, other: Self, code: &str) -> Result<Self, Error> {
        let both = self.to(other);
        if both.len() == self.len() + other.len() {
            Ok(both)
        } else {
            Err(Error::new(
                format!(
                    "there should be no space between `{}` and `{}`",
                    &code[self.start..self.end],
                    &code[other.start..other.end]
                ),
                Some(both),
            ))
        }
    }

    pub fn len(self) -> usize {
        self.end - self.start
    }

    pub fn after(mut self) -> Self {
        self.start = self.end;
        self
    }

    pub const fn empty() -> Self {
        Span {
            start: 0,
            end: 0,
            line: 0,
            column: 0,
            file_id: FileId::first(),
            crate_id: CrateId::std_id(),
        }
    }

    pub fn print(self, map: &FileMap) {
        self.write(&mut io::stdout(), true, map).unwrap();
    }

    pub fn println(self, map: &FileMap) {
        self.print(map);
        println!();
    }

    pub fn write<W: Write>(self, out: &mut W, color: bool, map: &FileMap) -> io::Result<()> {
        let code = map.get_code(self.file_id);
        if code[self.start..self.end].contains('\n') {
            self.write_multiline(out, color, map)
        } else {
            self.write_simple(out, color, map)
        }
    }

    fn write_multiline<W: Write>(self, out: &mut W, color: bool, map: &FileMap) -> io::Result<()> {
        let code = map.get_code(self.file_id);

        let mut lines: Vec<_> = map
            .text_at(self)
            .lines()
            .enumerate()
            .map(|(i, line)| (self.line + i + 1, line))
            .collect();
        let mut prefix = get_prefix(code, lines[0].1);
        let suffix = get_suffix(code, lines.last().unwrap().1);

        let min_spaces = cmp::min(
            count_leading_spaces(prefix),
            lines
                .iter()
                .skip(1)
                .map(|(_, line)| count_leading_spaces(line))
                .min()
                .unwrap(),
        );

        let last_line_num = format!("{}", self.line + lines.len());
        let num_width = last_line_num.len();
        let space = " ".repeat(num_width);

        let mut ellipsis = false;
        if lines.len() > 2 * MAX_LINES + 1 {
            lines.drain(MAX_LINES..(lines.len() - MAX_LINES));
            ellipsis = true;
        }

        if min_spaces > 4 {
            let offset = min_spaces - 4;
            prefix = &prefix[offset..];
            lines
                .iter_mut()
                .skip(1)
                .for_each(|(_, line)| *line = &line[offset..]);
        }

        let last_len = lines.last().unwrap().1.len();

        write!(out, "{}{}-->{} ", space, color!(Blue, color), reset!(color))?;
        self.write_location(out, map)?;
        writeln!(out, "\n{} {}|{}", space, color!(Blue, color), reset!(color))?;
        if prefix.trim().is_empty() {
            writeln!(
                out,
                "{}{:>num_width$} |{} {}/{} {}{}",
                color!(Blue, color),
                self.line + 1,
                reset!(color),
                color!(Red, color),
                reset!(color),
                prefix,
                lines[0].1
            )?;
        } else {
            writeln!(
                out,
                "{}{:>num_width$} |{}   {}{}",
                color!(Blue, color),
                self.line + 1,
                reset!(color),
                prefix,
                lines[0].1
            )?;
            writeln!(
                out,
                "{} {}|{}  {}{}^{}",
                space,
                color!(Blue, color),
                reset!(color),
                color!(Red, color),
                "_".repeat(prefix.chars().count() + 1),
                reset!(color)
            )?;
        }

        for (j, (i, line)) in lines.iter().skip(1).enumerate() {
            if ellipsis && j == MAX_LINES - 1 {
                writeln!(
                    out,
                    "{}{}...{} {}|{}",
                    " ".repeat(num_width - 1),
                    color!(Blue, color),
                    reset!(color),
                    color!(Red, color),
                    reset!(color)
                )?;
            }
            if j == lines.len() - 2 {
                writeln!(
                    out,
                    "{}{:>num_width$} |{} {}|{} {}{}",
                    color!(Blue, color),
                    i,
                    reset!(color),
                    color!(Red, color),
                    reset!(color),
                    line,
                    suffix
                )?;
            } else {
                writeln!(
                    out,
                    "{}{:>num_width$} |{} {}|{} {}",
                    color!(Blue, color),
                    i,
                    reset!(color),
                    color!(Red, color),
                    reset!(color),
                    line
                )?;
            }
        }

        write!(
            out,
            "{} {}|{} {}|{}^{}",
            space,
            color!(Blue, color),
            reset!(color),
            color!(Red, color),
            "_".repeat(last_len),
            reset!(color)
        )
    }

    fn write_simple<W: Write>(self, out: &mut W, color: bool, map: &FileMap) -> io::Result<()> {
        let text = map.text_at(self);
        let code = map.get_code(self.file_id);

        let mut prefix = get_prefix(code, text);
        let suffix = get_suffix(code, text);

        let leading_spaces = count_leading_spaces(prefix);
        if leading_spaces > 4 {
            let offset = leading_spaces - 4;
            prefix = &prefix[offset..];
        }

        let line_num = format!("{}", self.line + 1);
        let space = " ".repeat(line_num.len());

        write!(out, "{}{}-->{} ", space, color!(Blue, color), reset!(color))?;
        self.write_location(out, map)?;
        writeln!(out, "\n{} {}|{}", space, color!(Blue, color), reset!(color))?;
        writeln!(
            out,
            "{}{} |{} {}{}{}",
            color!(Blue, color),
            line_num,
            reset!(color),
            prefix,
            text,
            suffix
        )?;
        write!(
            out,
            "{} {}|{} {}{}{}{}",
            space,
            color!(Blue, color),
            reset!(color),
            " ".repeat(prefix.chars().count()),
            color!(Red, color),
            "^".repeat(cmp::max(text.chars().count(), 1)),
            reset!(color)
        )
    }

    fn write_location<W: Write>(self, out: &mut W, map: &FileMap) -> io::Result<()> {
        let path = map.get_path(self.file_id);
        write!(
            out,
            "{}:{}:{}",
            path.as_os_str().to_str().unwrap(),
            self.line + 1,
            self.column + 1
        )
    }
}

fn get_prefix<'a>(big: &'a str, small: &'a str) -> &'a str {
    let len = small.as_ptr() as usize - big.as_ptr() as usize;
    let left = &big[..len];

    if left.is_empty() || left.ends_with('\n') {
        ""
    } else {
        left.lines().next_back().unwrap()
    }
}

fn get_suffix<'a>(big: &'a str, small: &'a str) -> &'a str {
    let start = small.as_ptr() as usize - big.as_ptr() as usize + small.len();
    let right = &big[start..];

    if right.is_empty() || right.starts_with('\n') || right.starts_with("\r\n") {
        ""
    } else {
        right.lines().next().unwrap()
    }
}

fn count_leading_spaces(s: &str) -> usize {
    s.chars().take_while(|&c| c == ' ').count()
}

impl Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Span")
    }
}
