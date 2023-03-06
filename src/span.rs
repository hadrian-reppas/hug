use std::cmp;
use std::fmt::{self, Debug};
use std::io::{self, Write};

use crate::error::Error;

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
pub struct Span<'a, 'b> {
    pub text: &'a str,
    pub code: &'a str,
    pub location: Location<'b>,
}

impl<'a, 'b> Span<'a, 'b> {
    pub fn to(self, other: Self) -> Self {
        let combined = combine(self.text, other.text, self.code);
        Span {
            text: combined,
            code: self.code,
            location: self.location,
        }
    }

    pub fn by(self, other: Self) -> Result<Self, Error<'a, 'b>> {
        let both = self.to(other);
        if both.text.len() == self.text.len() + other.text.len() {
            Ok(both)
        } else {
            Err(Error::Parse(
                format!(
                    "there should be no space between `{}` and `{}`",
                    self.text, other.text
                ),
                both,
                vec![],
            ))
        }
    }

    pub fn after(mut self) -> Self {
        self.text = &self.text[self.text.len()..];
        self
    }

    pub fn print(self) {
        self.write(&mut io::stdout(), true).unwrap();
    }

    pub fn println(self) {
        self.print();
        println!();
    }

    pub fn write<W: Write>(self, out: &mut W, color: bool) -> io::Result<()> {
        if self.text.contains('\n') {
            self.write_multiline(out, color)
        } else {
            self.write_simple(out, color)
        }
    }

    fn write_multiline<W: Write>(self, out: &mut W, color: bool) -> io::Result<()> {
        let mut lines: Vec<_> = self
            .text
            .lines()
            .enumerate()
            .map(|(i, line)| (self.location.line + i + 1, line))
            .collect();
        let mut prefix = get_prefix(self.code, lines[0].1);
        let suffix = get_suffix(self.code, lines.last().unwrap().1);

        let min_spaces = cmp::min(
            count_leading_spaces(prefix),
            lines
                .iter()
                .skip(1)
                .map(|(_, line)| count_leading_spaces(line))
                .min()
                .unwrap(),
        );

        let last_line_num = format!("{}", self.location.line + lines.len());
        let num_width = last_line_num.len();
        let space = " ".repeat(num_width);
        let last_len = lines.last().unwrap().1.len();

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

        writeln!(
            out,
            "{}{}-->{} {}",
            space,
            color!(Blue, color),
            reset!(color),
            self.location
        )?;
        writeln!(out, "{} {}|{}", space, color!(Blue, color), reset!(color))?;
        if prefix.trim().is_empty() {
            writeln!(
                out,
                "{}{:>num_width$} |{} {}/{} {}{}",
                color!(Blue, color),
                self.location.line + 1,
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
                self.location.line + 1,
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
            writeln!(
                out,
                "{}{:>num_width$} |{} {}|{} {}{}",
                color!(Blue, color),
                i,
                reset!(color),
                color!(Red, color),
                reset!(color),
                line,
                suffix,
            )?;
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

    fn write_simple<W: Write>(self, out: &mut W, color: bool) -> io::Result<()> {
        let mut prefix = get_prefix(self.code, self.text);
        let suffix = get_suffix(self.code, self.text);

        let leading_spaces = count_leading_spaces(prefix);
        if leading_spaces > 4 {
            let offset = leading_spaces - 4;
            prefix = &prefix[offset..];
        }

        let line_num = format!("{}", self.location.line + 1);
        let space = " ".repeat(line_num.len());

        writeln!(
            out,
            "{}{}-->{} {}",
            space,
            color!(Blue, color),
            reset!(color),
            self.location
        )?;
        writeln!(out, "{} {}|{}", space, color!(Blue, color), reset!(color))?;
        writeln!(
            out,
            "{}{} |{} {}{}{}",
            color!(Blue, color),
            line_num,
            reset!(color),
            prefix,
            self.text,
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
            "^".repeat(cmp::max(self.text.chars().count(), 1)),
            reset!(color)
        )
    }
}

fn combine<'a>(left: &'a str, right: &'a str, big: &'a str) -> &'a str {
    let start = left.as_ptr() as usize - big.as_ptr() as usize;
    let end = right.as_ptr() as usize - big.as_ptr() as usize + right.len();
    &big[start..end]
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
    let mut n = 0;
    while s[n..].starts_with(' ') {
        n += 1;
    }
    n
}

impl Debug for Span<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Span({:?})", self.text)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Location<'a> {
    pub line: usize,
    pub column: usize,
    pub file: &'a str,
}

impl fmt::Display for Location<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line + 1, self.column + 1)
    }
}
