use lazy_static::lazy_static;
use regex::Regex;

use crate::error::{Error};
use crate::io::FileId;
use crate::span::{Location, Span};

#[derive(Clone, Copy, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub struct Tokens<'a> {
    suffix: &'a str,
    code: &'a str,
    start: usize,
    line: usize,
    column: usize,
    file_id: FileId,
}

impl<'a> Tokens<'a> {
    pub fn new(code: &'a str, file_id: FileId) -> Self {
        Tokens {
            suffix: code,
            code,
            start: 0,
            line: 0,
            column: 0,
            file_id,
        }
    }

    pub fn code(&self) -> &str {
        self.code
    }

    pub fn next(&mut self) -> Result<Token, Error> {
        self.skip_whitespace();
        if self.suffix.is_empty() {
            Ok(Token {
                kind: TokenKind::Eof,
                span: self.make_span(0),
            })
        } else {
            self.next_token()
        }
    }

    fn make_span(&mut self, len: usize) -> Span {
        let location = Location {
            line: self.line,
            column: self.column,
            file_id: self.file_id,
        };
        let span = Span {
            start: self.start,
            end: self.start + len,
            location,
        };

        for c in self.suffix[..len].chars() {
            if c == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }
        }

        self.suffix = &self.suffix[len..];
        self.start += len;

        span
    }

    fn skip_whitespace(&mut self) {
        let mut len = 0;
        while self.suffix[len..].starts_with(char::is_whitespace) {
            len += 1;
        }
        self.make_span(len);
    }

    fn next_token(&mut self) -> Result<Token, Error> {
        macro_rules! token {
            ($kind:ident) => {
                Ok(Token {
                    kind: TokenKind::$kind,
                    span: self.make_span(1),
                })
            };
        }

        if self.suffix.starts_with("b'") {
            self.lex_byte()
        } else if self.suffix.starts_with("b\"") {
            self.lex_byte_string()
        } else if self.suffix.starts_with(is_name_start) {
            let mut len = 0;
            while self.suffix[len..].starts_with(is_name_continue) {
                len += self.suffix[len..].chars().next().unwrap().len_utf8();
            }

            let kind = match &self.suffix[..len] {
                "and" => TokenKind::And,
                "as" => TokenKind::As,
                "break" => TokenKind::Break,
                "const" => TokenKind::Const,
                "continue" => TokenKind::Continue,
                "crate" => TokenKind::Crate,
                "else" => TokenKind::Else,
                "enum" => TokenKind::Enum,
                "extern" => TokenKind::Extern,
                "false" => TokenKind::False,
                "fn" => TokenKind::Fn,
                "for" => TokenKind::For,
                "goto" => TokenKind::Goto,
                "if" => TokenKind::If,
                "impl" => TokenKind::Impl,
                "in" => TokenKind::In,
                "let" => TokenKind::Let,
                "loop" => TokenKind::Loop,
                "match" => TokenKind::Match,
                "mod" => TokenKind::Mod,
                "mut" => TokenKind::Mut,
                "not" => TokenKind::Not,
                "or" => TokenKind::Or,
                "pub" => TokenKind::Pub,
                "return" => TokenKind::Return,
                "Self" => TokenKind::SelfType,
                "self" => TokenKind::SelfValue,
                "static" => TokenKind::Static,
                "struct" => TokenKind::Struct,
                "trait" => TokenKind::Trait,
                "try" => TokenKind::Try,
                "true" => TokenKind::True,
                "type" => TokenKind::Type,
                "unique" => TokenKind::Unique,
                "use" => TokenKind::Use,
                "where" => TokenKind::Where,
                "while" => TokenKind::While,
                "_" => TokenKind::Under,
                name if name.chars().all(|c| c == '_') => {
                    return Err(Error::new(
                        format!("illegal identifier `{name}`"),
                        Some(self.make_span(len)),
                    ))
                }
                _ => TokenKind::Ident,
            };
            let span = self.make_span(len);

            Ok(Token { kind, span })
        } else if self.suffix.starts_with(';') {
            token!(Semi)
        } else if self.suffix.starts_with(',') {
            token!(Comma)
        } else if self.suffix.starts_with('.') {
            token!(Dot)
        } else if self.suffix.starts_with('(') {
            token!(LParen)
        } else if self.suffix.starts_with(')') {
            token!(RParen)
        } else if self.suffix.starts_with('{') {
            token!(LBrace)
        } else if self.suffix.starts_with('}') {
            token!(RBrace)
        } else if self.suffix.starts_with('[') {
            token!(LBrack)
        } else if self.suffix.starts_with(']') {
            token!(RBrack)
        } else if self.suffix.starts_with('@') {
            token!(At)
        } else if self.suffix.starts_with('~') {
            token!(Tilde)
        } else if self.suffix.starts_with('?') {
            token!(QMark)
        } else if self.suffix.starts_with(':') {
            token!(Colon)
        } else if self.suffix.starts_with('=') {
            token!(Eq)
        } else if self.suffix.starts_with('!') {
            token!(Bang)
        } else if self.suffix.starts_with('<') {
            token!(Lt)
        } else if self.suffix.starts_with('>') {
            token!(Gt)
        } else if self.suffix.starts_with('-') {
            if self.suffix[1..].starts_with(|c: char| c.is_ascii_digit()) {
                self.lex_number()
            } else {
                token!(Dash)
            }
        } else if self.suffix.starts_with('+') {
            token!(Plus)
        } else if self.suffix.starts_with('*') {
            token!(Star)
        } else if self.suffix.starts_with('^') {
            token!(Caret)
        } else if self.suffix.starts_with('%') {
            token!(Percent)
        } else if self.suffix.starts_with('&') {
            token!(Amp)
        } else if self.suffix.starts_with('|') {
            token!(Bar)
        } else if self.suffix.starts_with("//") {
            let mut len = 0;
            for c in self.suffix.chars() {
                if c == '\n' {
                    len += 1;
                    break;
                }
                len += c.len_utf8();
            }
            self.make_span(len);
            self.next()
        } else if self.suffix.starts_with('/') {
            token!(Slash)
        } else if self.suffix.starts_with('\'') {
            self.lex_char()
        } else if self.suffix.starts_with('"') {
            self.lex_string()
        } else if self.suffix.starts_with(|c: char| c.is_ascii_digit()) {
            self.lex_number()
        } else {
            let c = self.suffix.chars().next().unwrap();
            Err(Error::new(
                format!("unexpected character {c:?}"),
                Some(self.make_span(1)),
            ))
        }
    }

    fn lex_number(&mut self) -> Result<Token, Error> {
        lazy_static! {
            static ref NUMBER: Regex = Regex::new(
                r"\A-?\d+(\.\d+)?([Ee][+-]?\d+)?(_?\p{Alphabetic}[\p{Alphabetic}0-9]*)?"
            )
            .unwrap();
            static ref SUFFIX: Regex = Regex::new(r"\p{Alphabetic}[\p{Alphabetic}0-9]*\z").unwrap();
        }
        const SUFFIXES: [&str; 30] = [
            "u8", "u16", "u32", "u64", "usize", "uz", "z", "i8", "i16", "i32", "i64", "isize",
            "iz", "f32", "f64", "_u8", "_u16", "_u32", "_u64", "_usize", "_uz", "_z", "_i8",
            "_i16", "_i32", "_i64", "_isize", "_iz", "_f32", "_f64",
        ];

        let len = NUMBER.find(self.suffix).unwrap().end();
        let text = &self.suffix[..len];
        let is_float = text.chars().any(|c| ".eE".contains(c));
        let span = self.make_span(len);

        if let Some(mat) = SUFFIX.find(text) {
            if let Some(suffix) = SUFFIXES.into_iter().find(|s| s == &mat.as_str()) {
                if suffix.ends_with("f32") || suffix.ends_with("f64") {
                    Ok(Token {
                        kind: TokenKind::Float,
                        span,
                    })
                } else if is_float {
                    Err(Error::new(
                        format!("invalid suffix `{}` for float literal", mat.as_str()),
                        Some(span),
                    )
                    .note("valid suffixes are `f32` and `f64`", None))
                } else {
                    Ok(Token {
                        kind: TokenKind::Int,
                        span,
                    })
                }
            } else if is_float {
                Err(Error::new(
                    format!("invalid suffix `{}` for float literal", mat.as_str()),
                    Some(span),
                )
                .note("valid suffixes are `f32` and `f64`", None))
            } else {
                Err(Error::new(
                    format!("invalid suffix `{}` for number literal", mat.as_str()),
                    Some(span),
                )
                .note(
                    "the suffix must be one of the numeric types (`i32`, `usize`, `f32`, etc.)",
                    None,
                ))
            }
        } else if is_float {
            Ok(Token {
                kind: TokenKind::Float,
                span,
            })
        } else {
            Ok(Token {
                kind: TokenKind::Int,
                span,
            })
        }
    }

    fn lex_char(&mut self) -> Result<Token, Error> {
        if let Some(len) = self.lex_char_or_escape(&self.suffix[1..], 1, '\'', false)? {
            if self.suffix[1 + len..].starts_with('\'') {
                Ok(Token {
                    kind: TokenKind::Char,
                    span: self.make_span(len + 2),
                })
            } else {
                Err(Error::new(
                    "unterminated char literal",
                    Some(self.make_span(1)),
                ))
            }
        } else {
            Err(Error::new("empty char literal", Some(self.make_span(2))))
        }
    }

    fn lex_string(&mut self) -> Result<Token, Error> {
        let mut len = 1;
        while let Some(l) = self.lex_char_or_escape(&self.suffix[len..], len, '"', false)? {
            len += l;
        }

        Ok(Token {
            kind: TokenKind::String,
            span: self.make_span(len + 1),
        })
    }

    fn lex_byte(&mut self) -> Result<Token, Error> {
        if let Some(len) = self.lex_char_or_escape(&self.suffix[2..], 2, '\'', true)? {
            if self.suffix[2 + len..].starts_with('\'') {
                Ok(Token {
                    kind: TokenKind::Byte,
                    span: self.make_span(len + 3),
                })
            } else {
                Err(Error::new(
                    "unterminated byte literal",
                    Some(self.make_span(2)),
                ))
            }
        } else {
            Err(Error::new("empty byte literal", Some(self.make_span(3))))
        }
    }

    fn lex_byte_string(&mut self) -> Result<Token, Error> {
        let mut len = 2;
        while let Some(l) = self.lex_char_or_escape(&self.suffix[len..], len, '"', true)? {
            len += l;
        }

        Ok(Token {
            kind: TokenKind::ByteString,
            span: self.make_span(len + 1),
        })
    }

    fn lex_char_or_escape(
        &mut self,
        suffix: &str,
        len: usize,
        end: char,
        byte: bool,
    ) -> Result<Option<usize>, Error> {
        let unterminated_msg = if end == '"' {
            "unterminated string literal"
        } else {
            "unterminated char literal"
        };

        let mut chars = suffix.chars();
        if let Some(c) = chars.next() {
            if c == end {
                Ok(None)
            } else if c == '\\' {
                match chars.next() {
                    Some('\\') => Ok(Some(2)),
                    Some('n') => Ok(Some(2)),
                    Some('r') => Ok(Some(2)),
                    Some('t') => Ok(Some(2)),
                    Some('u') => self
                        .lex_hex_digits(4, suffix, len, 'u', byte)
                        .map(|_| Some(6)),
                    Some('U') => self
                        .lex_hex_digits(8, suffix, len, 'U', byte)
                        .map(|_| Some(10)),
                    Some('x') => self
                        .lex_hex_digits(2, suffix, len, 'x', byte)
                        .map(|_| Some(4)),
                    Some('0') => Ok(Some(2)),
                    Some(c) if c == end => Ok(Some(2)),
                    Some(unknown) => {
                        self.make_span(len);
                        Err(Error::new(
                            format!("unknown escape character {unknown:?}"),
                            Some(self.make_span(1 + unknown.len_utf8())),
                        ))
                    }
                    None => Err(Error::new(unterminated_msg, Some(self.make_span(1)))),
                }
            } else {
                Ok(Some(c.len_utf8()))
            }
        } else {
            Err(Error::new(unterminated_msg, Some(self.make_span(1))))
        }
    }

    fn lex_hex_digits(
        &mut self,
        n: usize,
        suffix: &str,
        len: usize,
        escape: char,
        byte: bool,
    ) -> Result<(), Error> {
        for i in 0..n {
            if !suffix[(2 + i)..].starts_with(|c: char| c.is_ascii_hexdigit()) {
                let offset = if suffix.len() > 2 + i { 3 } else { 2 };

                self.make_span(len);
                return Err(Error::new(
                    format!("'\\{escape}' must be followed by {n} hex digits"),
                    Some(self.make_span(offset + i)),
                ));
            }
        }

        let i = u32::from_str_radix(&suffix[2..(2 + n)], 16).unwrap();
        if escape == 'x' && !byte && i > 0x7f {
            self.make_span(len);
            Err(
                Error::new("invalid hex escape", Some(self.make_span(2 + n)))
                    .note("hex escapes must be in the range [\\x00-\\x7f]", None),
            )
        } else if char::from_u32(i).is_some() {
            Ok(())
        } else {
            self.make_span(len);
            Err(Error::new(
                "invalid unicode code point",
                Some(self.make_span(2 + n)),
            ))
        }
    }
}

fn is_name_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_name_continue(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Ident,
    Under,

    // literals
    Int,
    Float,
    Char,
    String,
    Byte,
    ByteString,

    // symbols
    Semi,
    Comma,
    Dot,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBrack,
    RBrack,
    At,
    Tilde,
    QMark,
    Colon,
    Eq,
    Bang,
    Lt,
    Gt,
    Dash,
    Plus,
    Star,
    Slash,
    Caret,
    Percent,
    Amp,
    Bar,

    // keywords
    And,
    As,
    Break,
    Const,
    Continue,
    Crate,
    Else,
    Enum,
    Extern,
    False,
    Fn,
    For,
    Goto,
    If,
    Impl,
    In,
    Let,
    Loop,
    Match,
    Mod,
    Mut,
    Not,
    Or,
    Pub,
    Return,
    SelfType,
    SelfValue,
    Static,
    Struct,
    Trait,
    True,
    Try,
    Type,
    Unique,
    Use,
    Where,
    While,

    Eof,
}

impl TokenKind {
    pub fn desc(self) -> &'static str {
        match self {
            TokenKind::Ident => "identifier",
            TokenKind::Under => "`_`",
            TokenKind::Int => "integer",
            TokenKind::Float => "float",
            TokenKind::Char => "character",
            TokenKind::String => "string",
            TokenKind::ByteString => "byte string",
            TokenKind::Byte => "byte",
            TokenKind::Semi => "`;`",
            TokenKind::Comma => "`,`",
            TokenKind::Dot => "`.`",
            TokenKind::LParen => "`(`",
            TokenKind::RParen => "`)`",
            TokenKind::LBrace => "`{`",
            TokenKind::RBrace => "`}`",
            TokenKind::LBrack => "`[`",
            TokenKind::RBrack => "`]`",
            TokenKind::At => "`@`",
            TokenKind::Tilde => "`~`",
            TokenKind::QMark => "`?`",
            TokenKind::Colon => "`:`",
            TokenKind::Eq => "`=`",
            TokenKind::Bang => "`!`",
            TokenKind::Lt => "`<`",
            TokenKind::Gt => "`>`",
            TokenKind::Dash => "`-`",
            TokenKind::Plus => "`+`",
            TokenKind::Star => "`*`",
            TokenKind::Slash => "`/`",
            TokenKind::Caret => "`^`",
            TokenKind::Percent => "`%`",
            TokenKind::Amp => "`&`",
            TokenKind::Bar => "`|`",
            TokenKind::And => "`and`",
            TokenKind::As => "`as`",
            TokenKind::Break => "`break`",
            TokenKind::Const => "`const`",
            TokenKind::Crate => "`crate`",
            TokenKind::Continue => "`continue`",
            TokenKind::Else => "`else`",
            TokenKind::Enum => "`enum`",
            TokenKind::Extern => "`extern`",
            TokenKind::False => "`false`",
            TokenKind::Fn => "`fn`",
            TokenKind::For => "`for`",
            TokenKind::Goto => "`goto`",
            TokenKind::If => "`if`",
            TokenKind::Impl => "`impl`",
            TokenKind::In => "`in`",
            TokenKind::Let => "`let`",
            TokenKind::Loop => "`loop`",
            TokenKind::Match => "`match`",
            TokenKind::Mod => "`mod`",
            TokenKind::Mut => "`mut`",
            TokenKind::Not => "`not`",
            TokenKind::Or => "`or`",
            TokenKind::Pub => "`pub`",
            TokenKind::Return => "`return`",
            TokenKind::SelfType => "`Self`",
            TokenKind::SelfValue => "`self`",
            TokenKind::Static => "`static`",
            TokenKind::Struct => "`struct`",
            TokenKind::Trait => "`trait`",
            TokenKind::Try => "`try`",
            TokenKind::True => "`true`",
            TokenKind::Type => "`type`",
            TokenKind::Unique => "`unique`",
            TokenKind::Use => "`use`",
            TokenKind::Where => "`where`",
            TokenKind::While => "`while`",
            TokenKind::Eof => "end of file",
        }
    }

    pub fn is_expr_start(self) -> bool {
        matches!(
            self,
            TokenKind::Int
                | TokenKind::Float
                | TokenKind::Char
                | TokenKind::String
                | TokenKind::Byte
                | TokenKind::ByteString
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Dash
                | TokenKind::Bang
                | TokenKind::Not
                | TokenKind::Star
                | TokenKind::Amp
                | TokenKind::Dot
                | TokenKind::Ident
                | TokenKind::LParen
                | TokenKind::LBrack
                | TokenKind::LBrace
                | TokenKind::Crate
                | TokenKind::If
                | TokenKind::While
                | TokenKind::Match
                | TokenKind::For
                | TokenKind::Loop
                | TokenKind::Try
                | TokenKind::Goto
                | TokenKind::Break
                | TokenKind::Continue
                | TokenKind::Return
                | TokenKind::SelfValue
                | TokenKind::At
        )
    }
}
