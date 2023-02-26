use std::collections::VecDeque;

use crate::ast::*;
use crate::error::Error;
use crate::lex::{TokenKind, TokenKind::*, Tokens, Token};
use crate::span::Span;

pub fn parse<'a, 'b>(code: &'a str, file: &'b str) -> Result<Vec<Item<'a, 'b>>, Error<'a, 'b>> {
    let mut parser = Parser::new(code, file);
    parser.unit()
}

struct Parser<'a, 'b> {
    tokens: Tokens<'a, 'b>,
    peek: VecDeque<Token<'a, 'b>>,
}

impl<'a, 'b> Parser<'a, 'b> {
    fn new(code: &'a str, file: &'b str) -> Self {
        Parser {
            tokens: Tokens::new(code, file),
            peek: VecDeque::new(),
        }
    }

    fn next(&mut self) -> Result<Token<'a, 'b>, Error<'a, 'b>> {
        if let Some(token) = self.peek.pop_front() {
            Ok(token)
        } else {
            self.tokens.next()
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Token<'a, 'b>, Error<'a, 'b>> {
        let token = self.next()?;
        if token.kind == expected {
            Ok(token)
        } else {
            Err(Error::Parse(
                format!("expected {}", expected.description()),
                token.span,
                vec![],
            ))
        }
    }

    fn peek<S: Sequence>(&mut self, sequence: S) -> Result<bool, Error<'a, 'b>> {
        for (i, &kind) in sequence.as_slice().iter().enumerate() {
            if i == self.peek.len() {
                self.peek.push_back(self.tokens.next()?);
            }
            if self.peek[i].kind != kind {
                return Ok(false);
            }
        }
        Ok(true)
    }

    fn peek_kind(&mut self) -> Result<TokenKind, Error<'a, 'b>> {
        if self.peek.is_empty() {
            self.peek.push_back(self.tokens.next()?);
        }
        Ok(self.peek[0].kind)
    }

    fn peek_span(&mut self) -> Result<Span<'a, 'b>, Error<'a, 'b>> {
        if self.peek.is_empty() {
            self.peek.push_back(self.tokens.next()?);
        }
        Ok(self.peek[0].span)
    }

    fn consume<S: Sequence>(&mut self, sequence: S) -> Result<bool, Error<'a, 'b>> {
        let len = sequence.as_slice().len();
        if self.peek(sequence)? {
            for _ in 0..len {
                self.next()?;
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn unit(&mut self) -> Result<Vec<Item<'a, 'b>>, Error<'a, 'b>> {
        let mut items = Vec::new();
        while !self.peek(Eof)? {
            items.push(self.item()?);
        }
        Ok(items)
    }

    fn item(&mut self) -> Result<Item<'a, 'b>, Error<'a, 'b>> {
        let pub_span = if self.peek(Pub)? {
            Some(self.next()?.span)
        } else {
            None
        };

        match self.peek_kind()? {
            Fn => todo!(),
            Struct => self.struct_decl(pub_span),
            Enum => todo!(),
            Trait => todo!(),
            Type => todo!(),
            Const => todo!(),
            Global => todo!(),
            Use => self.use_decl(pub_span),
            Mod => todo!(),
            Extern => todo!(),
            Pub => todo!(),
            kind => Err(Error::Parse(
                format!("expected item, found {}", kind.description()),
                self.peek_span()?,
                vec![],
            )),
        }
    }

    fn use_decl(&mut self, pub_span: Option<Span<'a, 'b>>) -> Result<Item<'a, 'b>, Error<'a, 'b>> {
        if let Some(span) = pub_span {
            return Err(Error::Parse(
                format!(
                    "{} not permitted before {}",
                    Pub.description(),
                    Use.description()
                ),
                span,
                vec![],
            ));
        }

        let first = self.expect(Use)?;
        let tree = self.use_tree()?;
        let last = self.expect(Semicolon)?;
        Ok(Item::Use {
            tree,
            span: first.span.to(last.span),
        })
    }

    fn use_tree(&mut self) -> Result<UseTree<'a, 'b>, Error<'a, 'b>> {
        let prefix = self.path()?;
        if self.consume([Colon, Colon, LBrace])? {
            if self.peek(RBrace)? {
                let last = self.expect(RBrace)?;
                let span = prefix.span.to(last.span);
                return Ok(UseTree {
                    prefix,
                    kind: UseTreeKind::Nested(Vec::new()),
                    span,
                });
            }

            let mut nested = vec![self.use_tree()?];
            while !self.peek(RBrace)? && !self.peek([Comma, RBrace])? {
                self.expect(Comma)?;
                nested.push(self.use_tree()?);
            }
            self.consume(Comma)?;

            let last = self.expect(RBrace)?;
            let span = prefix.span.to(last.span);
            Ok(UseTree {
                prefix,
                kind: UseTreeKind::Nested(nested),
                span,
            })
        } else if self.consume(As)? {
            let rename = self.name()?;
            let span = prefix.span.to(rename.span);
            Ok(UseTree {
                prefix,
                kind: UseTreeKind::Rename(rename),
                span,
            })
        } else {
            Ok(UseTree {
                span: prefix.span,
                prefix,
                kind: UseTreeKind::Simple,
            })
        }
    }

    fn struct_decl(
        &mut self,
        pub_span: Option<Span<'a, 'b>>,
    ) -> Result<Item<'a, 'b>, Error<'a, 'b>> {
        let (first_span, is_pub) = if let Some(span) = pub_span {
            self.expect(Struct)?;
            (span, true)
        } else {
            (self.expect(Struct)?.span, false)
        };

        let name = self.name()?;
        let generic_params = if self.peek(Lt)? {
            Some(self.generic_params()?)
        } else {
            None
        };

        self.expect(LBrace)?;

        if self.peek(RBrace)? {
            let last = self.expect(RBrace)?;
            let span = first_span.to(last.span);
            return Ok(Item::Struct {
                is_pub,
                name,
                generic_params,
                fields: Vec::new(),
                span,
            });
        }

        let mut fields = vec![self.struct_field()?];
        while !self.peek(RBrace)? && !self.peek([Comma, RBrace])? {
            self.expect(Comma)?;
            fields.push(self.struct_field()?);
        }
        self.consume(Comma)?;
        let last = self.expect(RBrace)?;

        let span = first_span.to(last.span);
        Ok(Item::Struct {
            is_pub,
            name,
            generic_params,
            fields,
            span,
        })
    }

    fn generic_params(&mut self) -> Result<GenericParams<'a, 'b>, Error<'a, 'b>> {
        let first = self.expect(Lt)?;

        if self.peek(Gt)? {
            let last = self.expect(Gt)?;
            let span = first.span.to(last.span);
            return Ok(GenericParams {
                params: Vec::new(),
                span,
            });
        }

        let mut params = vec![self.name()?];
        while !self.peek(Gt)? && !self.peek([Comma, Gt])? {
            self.expect(Comma)?;
            params.push(self.name()?);
        }
        self.consume(Comma)?;
        let last = self.expect(Gt)?;

        let span = first.span.to(last.span);
        Ok(GenericParams { params, span })
    }

    fn struct_field(&mut self) -> Result<StructField<'a, 'b>, Error<'a, 'b>> {
        todo!()
    }

    fn name(&mut self) -> Result<Name<'a, 'b>, Error<'a, 'b>> {
        let token = self.expect(Ident)?;
        Ok(Name {
            name: token.span.text,
            span: token.span,
        })
    }

    fn path(&mut self) -> Result<Path<'a, 'b>, Error<'a, 'b>> {
        let mut path = vec![self.name()?];
        while self.peek([Colon, Colon, Ident])? {
            self.expect(Colon)?;
            self.expect(Colon)?;
            path.push(self.name()?);
        }
        let span = path[0].span.to(path.last().unwrap().span);
        Ok(Path { path, span })
    }
}

trait Sequence {
    fn as_slice(&self) -> &[TokenKind];
}

impl Sequence for TokenKind {
    fn as_slice(&self) -> &[TokenKind] {
        macro_rules! block {
            ($($names:ident),* $(,)?) => {
                match self {
                    $(TokenKind::$names => {
                        const TOKEN: &[TokenKind] = &[TokenKind::$names];
                        TOKEN
                    })*
                }
            };
        }

        block! {
            Ident, Int, Float, Char, String, ByteString, Byte, Semicolon, Comma, Dot, LParen,
            RParen, LBrace, RBrace, LBrack, RBrack, At, Tilde, QMark, Colon, Eq, Bang, Lt, Gt,
            Minus, Plus, Star, Slash, Caret, Percent, And, As, Break, Const, Continue, Else, Enum,
            Extern, Flase, Fn, For, Global, Goto, If, Impl, In, Let, Loop, Match, Mod, Mut, Not,
            Or, Pub, Return, SelfType, SelfValue, Struct, Trait, Try, True, Type, Use, Where,
            While, Eof,
        }
    }
}

impl<const N: usize> Sequence for [TokenKind; N] {
    fn as_slice(&self) -> &[TokenKind] {
        self
    }
}
