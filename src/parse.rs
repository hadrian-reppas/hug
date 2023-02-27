use std::collections::VecDeque;

use crate::ast::*;
use crate::error::Error;
use crate::lex::{Token, TokenKind, TokenKind::*, Tokens};
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
                format!(
                    "expected {}, found {}",
                    expected.description(),
                    token.kind.description()
                ),
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
            Struct => self.struct_def(pub_span),
            Enum => self.enum_def(pub_span),
            Impl => todo!(),
            Trait => todo!(),
            Type => self.type_alias(pub_span),
            Const => todo!(),
            Global => todo!(),
            Use => self.use_decl(pub_span),
            Mod => self.module(pub_span),
            Extern => self.extern_block(pub_span),
            kind => Err(Error::Parse(
                format!("expected item, found {}", kind.description()),
                self.peek_span()?,
                vec![],
            )),
        }
    }

    fn ty(&mut self) -> Result<Ty<'a, 'b>, Error<'a, 'b>> {
        match self.peek_kind()? {
            Ident => {
                let path = self.path()?;
                let (generic_args, span) = if self.peek(Lt)? {
                    let generic_args = self.generic_args()?;
                    let span = path.span.to(generic_args.span);
                    (Some(generic_args), span)
                } else {
                    (None, path.span)
                };

                Ok(Ty::Path {
                    path,
                    generic_args,
                    span,
                })
            }
            Star => {
                let star_span = self.expect(Star)?.span;
                let ty = self.ty()?;

                let span = star_span.to(ty.span());
                Ok(Ty::Ptr {
                    ty: Box::new(ty),
                    span,
                })
            }
            LParen => {
                let first = self.expect(LParen)?;

                if self.peek(RParen)? {
                    let last = self.expect(RParen)?;
                    let span = first.span.to(last.span);
                    return Ok(Ty::Tuple {
                        tys: Vec::new(),
                        span,
                    });
                }

                let ty = self.ty()?;
                if self.peek(RParen)? {
                    self.expect(RParen)?;
                    return Ok(ty);
                }

                let mut tys = vec![ty];
                while !self.peek(RParen)? && !self.peek([Comma, RParen])? {
                    self.expect(Comma)?;
                    tys.push(self.ty()?);
                }
                self.consume(Comma)?;
                let last = self.expect(RParen)?;

                let span = first.span.to(last.span);
                Ok(Ty::Tuple { tys, span })
            }
            LBrack => {
                let first = self.expect(LBrack)?;
                let ty = self.ty()?;
                if self.peek(RBrack)? {
                    let last = self.expect(RBrack)?;
                    let span = first.span.to(last.span);
                    Ok(Ty::Slice {
                        ty: Box::new(ty),
                        span,
                    })
                } else if self.peek(Semi)? {
                    self.expect(Semi)?;
                    let count = self.expect(Int)?.span;
                    let last = self.expect(RBrack)?;

                    let span = first.span.to(last.span);
                    Ok(Ty::Array {
                        ty: Box::new(ty),
                        count,
                        span,
                    })
                } else {
                    Err(Error::Parse(
                        format!(
                            "expected {} or {}, found {}",
                            RBrack.description(),
                            Semi.description(),
                            self.peek_kind()?.description()
                        ),
                        self.peek_span()?,
                        vec![],
                    ))
                }
            }
            Fn => {
                let first = self.expect(Fn)?;
                self.expect(LParen)?;
                let (params, last) = if self.peek(RParen)? {
                    (Vec::new(), self.expect(RParen)?)
                } else {
                    let mut params = vec![self.ty()?];
                    while !self.peek(RParen)? && !self.peek([Comma, RParen])? {
                        self.expect(Comma)?;
                        params.push(self.ty()?);
                    }
                    self.consume(Comma)?;
                    let last = self.expect(RParen)?;
                    (params, last)
                };

                if self.peek(Dash)? {
                    self.expect(Dash)?;
                    self.expect(Gt)?;
                    let ret = self.ty()?;

                    let span = first.span.to(ret.span());
                    Ok(Ty::Fn {
                        params,
                        ret: Some(Box::new(ret)),
                        span,
                    })
                } else {
                    let span = first.span.to(last.span);
                    Ok(Ty::Fn {
                        params,
                        ret: None,
                        span,
                    })
                }
            }
            SelfType => Ok(Ty::SelfType {
                span: self.expect(SelfType)?.span,
            }),
            Bang => Ok(Ty::Never {
                span: self.expect(Bang)?.span,
            }),
            kind => Err(Error::Parse(
                format!("expected type, found {}", kind.description()),
                self.peek_span()?,
                vec![],
            )),
        }
    }

    fn generic_args(&mut self) -> Result<GenericArgs<'a, 'b>, Error<'a, 'b>> {
        let first = self.expect(Lt)?;

        if self.peek(Gt)? {
            let last = self.expect(Gt)?;
            let span = first.span.to(last.span);
            return Ok(GenericArgs {
                args: Vec::new(),
                span,
            });
        }

        let mut args = vec![self.ty()?];
        while !self.peek(Gt)? && !self.peek([Comma, Gt])? {
            self.expect(Comma)?;
            args.push(self.ty()?);
        }
        self.consume(Comma)?;
        let last = self.expect(Gt)?;

        let span = first.span.to(last.span);
        Ok(GenericArgs { args, span })
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
        let last = self.expect(Semi)?;
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

    fn struct_def(
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
        if self.peek(Pub)? {
            let pub_span = self.expect(Pub)?.span;
            let name = self.name()?;
            self.expect(Colon)?;
            let ty = self.ty()?;

            let span = pub_span.to(ty.span());
            Ok(StructField {
                is_pub: true,
                name,
                ty,
                span,
            })
        } else {
            let name = self.name()?;
            self.expect(Colon)?;
            let ty = self.ty()?;

            let span = name.span.to(ty.span());
            Ok(StructField {
                is_pub: false,
                name,
                ty,
                span,
            })
        }
    }

    fn enum_def(&mut self, pub_span: Option<Span<'a, 'b>>) -> Result<Item<'a, 'b>, Error<'a, 'b>> {
        let (first_span, is_pub) = if let Some(span) = pub_span {
            self.expect(Enum)?;
            (span, true)
        } else {
            (self.expect(Enum)?.span, false)
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
            return Ok(Item::Enum {
                is_pub,
                name,
                generic_params,
                items: Vec::new(),
                span,
            });
        }

        let mut items = vec![self.enum_item()?];
        while !self.peek(RBrace)? && !self.peek([Comma, RBrace])? {
            self.expect(Comma)?;
            items.push(self.enum_item()?);
        }
        self.consume(Comma)?;
        let last = self.expect(RBrace)?;

        let span = first_span.to(last.span);
        Ok(Item::Enum {
            is_pub,
            name,
            generic_params,
            items,
            span,
        })
    }

    fn enum_item(&mut self) -> Result<EnumItem<'a, 'b>, Error<'a, 'b>> {
        let name = self.name()?;

        if !self.consume(LParen)? {
            return Ok(EnumItem {
                name,
                tuple: None,
                span: name.span,
            });
        }

        if self.peek(RParen)? {
            let last = self.expect(RParen)?;
            let span = name.span.to(last.span);
            return Ok(EnumItem {
                name,
                tuple: Some(Vec::new()),
                span,
            });
        }

        let mut tuple = vec![self.ty()?];
        while !self.peek(RParen)? && !self.peek([Comma, RParen])? {
            self.expect(Comma)?;
            tuple.push(self.ty()?);
        }
        self.consume(Comma)?;
        let last = self.expect(RParen)?;

        let span = name.span.to(last.span);
        Ok(EnumItem {
            name,
            tuple: Some(tuple),
            span,
        })
    }

    fn type_alias(
        &mut self,
        pub_span: Option<Span<'a, 'b>>,
    ) -> Result<Item<'a, 'b>, Error<'a, 'b>> {
        let (first_span, is_pub) = if let Some(span) = pub_span {
            self.expect(Type)?;
            (span, true)
        } else {
            (self.expect(Type)?.span, false)
        };

        let name = self.name()?;
        let generic_params = if self.peek(Lt)? {
            Some(self.generic_params()?)
        } else {
            None
        };

        self.expect(Eq)?;
        let ty = self.ty()?;
        let last = self.expect(Semi)?;

        let span = first_span.to(last.span);
        Ok(Item::Type {
            is_pub,
            name,
            generic_params,
            ty,
            span,
        })
    }

    fn module(&mut self, pub_span: Option<Span<'a, 'b>>) -> Result<Item<'a, 'b>, Error<'a, 'b>> {
        let (first_span, is_pub) = if let Some(span) = pub_span {
            self.expect(Mod)?;
            (span, true)
        } else {
            (self.expect(Mod)?.span, false)
        };

        let name = self.name()?;
        if self.peek(Semi)? {
            let last = self.expect(Semi)?;
            let span = first_span.to(last.span);
            Ok(Item::Mod {
                is_pub,
                name,
                at: None,
                span,
            })
        } else if self.peek(At)? {
            self.expect(At)?;
            let path = self.expect(String)?;
            let last = self.expect(Semi)?;

            let span = first_span.to(last.span);
            Ok(Item::Mod {
                is_pub,
                name,
                at: Some(path.span),
                span,
            })
        } else {
            Err(Error::Parse(
                format!(
                    "expected {} or {}, found {}",
                    At.description(),
                    Semi.description(),
                    self.peek_kind()?.description()
                ),
                self.peek_span()?,
                vec![],
            ))
        }
    }

    fn extern_block(
        &mut self,
        pub_span: Option<Span<'a, 'b>>,
    ) -> Result<Item<'a, 'b>, Error<'a, 'b>> {
        if let Some(span) = pub_span {
            return Err(Error::Parse(
                format!(
                    "{} not permitted before {}",
                    Pub.description(),
                    Extern.description()
                ),
                span,
                vec![],
            ));
        }

        let first = self.expect(Extern)?;
        self.expect(LBrace)?;
        let mut items = Vec::new();
        while !self.peek(RBrace)? {
            items.push(self.extern_item()?);
        }
        let last = self.expect(RBrace)?;

        let span = first.span.to(last.span);
        Ok(Item::Extern { items, span })
    }

    fn extern_item(&mut self) -> Result<ExternItem<'a, 'b>, Error<'a, 'b>> {
        let pub_span = if self.peek(Pub)? {
            Some(self.next()?.span)
        } else {
            None
        };

        match self.peek_kind()? {
            Fn => {
                let signature = self.signature(pub_span)?;
                let last = self.expect(Semi)?;
                let span = signature.span.to(last.span);
                Ok(ExternItem::Fn { signature, span })
            }
            Type => {
                let (first_span, is_pub) = if let Some(span) = pub_span {
                    self.expect(Type)?;
                    (span, true)
                } else {
                    (self.expect(Type)?.span, false)
                };

                let name = self.name()?;
                let last = self.expect(Semi)?;

                let span = first_span.to(last.span);
                Ok(ExternItem::Type { is_pub, name, span })
            }
            Global => {
                let (first_span, is_pub) = if let Some(span) = pub_span {
                    self.expect(Global)?;
                    (span, true)
                } else {
                    (self.expect(Global)?.span, false)
                };

                let name = self.name()?;
                self.expect(Colon)?;
                let ty = self.ty()?;
                let last = self.expect(Semi)?;

                let span = first_span.to(last.span);
                Ok(ExternItem::Global {
                    is_pub,
                    name,
                    ty,
                    span,
                })
            }
            kind => Err(Error::Parse(
                format!(
                    "expected {}, {} or {}, found {}",
                    Fn.description(),
                    Type.description(),
                    Global.description(),
                    kind.description()
                ),
                self.peek_span()?,
                vec![],
            )),
        }
    }

    fn signature(
        &mut self,
        pub_span: Option<Span<'a, 'b>>,
    ) -> Result<Signature<'a, 'b>, Error<'a, 'b>> {
        let (first_span, is_pub) = if let Some(span) = pub_span {
            self.expect(Fn)?;
            (span, true)
        } else {
            (self.expect(Fn)?.span, false)
        };

        let name = self.name()?;
        let generic_params = if self.peek(Lt)? {
            Some(self.generic_params()?)
        } else {
            None
        };

        self.expect(LParen)?;
        let self_kind = if self.peek(Star)? {
            let first = self.expect(Star)?;
            let last = self.expect(SelfValue)?;
            SelfKind::Ptr(first.span.to(last.span))
        } else if self.peek(SelfValue)? {
            SelfKind::Value(self.expect(SelfValue)?.span)
        } else {
            SelfKind::None
        };

        let params = self.params(!self_kind.is_none())?;
        let last_span = self.expect(RParen)?.span;

        let (ret, last_span) = if self.peek(Dash)? {
            self.expect(Dash)?;
            self.expect(Gt)?;
            let ty = self.ty()?;
            let span = ty.span();
            (Some(ty), span)
        } else {
            (None, last_span)
        };

        let (where_clause, last_span) = if self.peek(Where)? {
            let where_clause = self.where_clause()?;
            let span = where_clause.span;
            (Some(where_clause), span)
        } else {
            (None, last_span)
        };

        let span = first_span.to(last_span);
        Ok(Signature {
            is_pub,
            name,
            generic_params,
            self_kind,
            params,
            ret,
            where_clause,
            span,
        })
    }

    fn params(&mut self, has_self_param: bool) -> Result<Vec<Param<'a, 'b>>, Error<'a, 'b>> {
        if has_self_param && !self.peek(RParen)? {
            self.expect(Comma)?;
        }
        if self.peek(Comma)? {
            return Err(Error::Parse(
                format!("expected parameter, found {}", Comma.description()),
                self.peek_span()?,
                vec![],
            ));
        }

        if self.peek(RParen)? {
            return Ok(Vec::new());
        }

        let name = self.name()?;
        self.expect(Colon)?;
        let ty = self.ty()?;
        let span = name.span.to(ty.span());

        let mut params = vec![Param { name, ty, span }];
        while !self.peek(RParen)? && !self.peek([Comma, RParen])? {
            self.expect(Comma)?;
            let name = self.name()?;
            self.expect(Colon)?;
            let ty = self.ty()?;
            let span = name.span.to(ty.span());
            params.push(Param { name, ty, span });
        }
        self.consume(Comma)?;

        Ok(params)
    }

    fn where_clause(&mut self) -> Result<WhereClause<'a, 'b>, Error<'a, 'b>> {
        let first = self.expect(Where)?;

        let mut items = vec![self.where_item()?];
        while self.peek([Comma, Ident])? || self.peek([Comma, SelfType])? {
            self.expect(Comma)?;
            items.push(self.where_item()?);
        }

        let last_span = if self.peek(Comma)? {
            self.expect(Comma)?.span
        } else {
            items.last().unwrap().span()
        };

        let span = first.span.to(last_span);
        Ok(WhereClause { items, span })
    }

    fn where_item(&mut self) -> Result<WhereItem<'a, 'b>, Error<'a, 'b>> {
        if self.peek(Ident)? {
            let param = self.name()?;
            self.expect(Colon)?;

            let mut bounds = vec![self.trait_bound()?];
            while self.peek(Plus)? {
                self.expect(Plus)?;
                bounds.push(self.trait_bound()?);
            }

            let span = param.span.to(bounds.last().unwrap().span());
            Ok(WhereItem::ParamBound {
                param,
                bounds,
                span,
            })
        } else if self.peek(SelfType)? {
            let self_type = self.expect(SelfType)?;
            self.expect(Colon)?;

            let mut bounds = vec![self.trait_bound()?];
            while self.peek(Plus)? {
                self.expect(Plus)?;
                bounds.push(self.trait_bound()?);
            }

            let span = self_type.span.to(bounds.last().unwrap().span());
            Ok(WhereItem::SelfBound { bounds, span })
        } else {
            Err(Error::Parse(
                format!(
                    "expected generic parameter or {}, found {}",
                    SelfType.description(),
                    self.peek_kind()?.description()
                ),
                self.peek_span()?,
                vec![],
            ))
        }
    }

    fn trait_bound(&mut self) -> Result<TraitBound<'a, 'b>, Error<'a, 'b>> {
        let path = self.path()?;
        if self.peek(Lt)? {
            let generic_args = self.generic_args()?;
            let span = path.span.to(generic_args.span);
            Ok(TraitBound::Trait {
                path,
                generic_args: Some(generic_args),
                span,
            })
        } else if self.peek(LParen)? {
            self.expect(LParen)?;

            let params = if self.peek(RParen)? {
                Vec::new()
            } else {
                let mut params = vec![self.ty()?];
                while !self.peek(RParen)? && !self.peek([Comma, RParen])? {
                    self.expect(Comma)?;
                    params.push(self.ty()?);
                }
                self.consume(Comma)?;
                params
            };
            let last = self.expect(RParen)?;

            let (ret, last_span) = if self.peek(Dash)? {
                self.expect(Dash)?;
                self.expect(Gt)?;
                let ret = self.ty()?;

                let span = ret.span();
                (Some(ret), span)
            } else {
                (None, last.span)
            };

            let span = path.span.to(last_span);
            Ok(TraitBound::Fn {
                path,
                params,
                ret,
                span,
            })
        } else {
            let span = path.span;
            Ok(TraitBound::Trait {
                path,
                generic_args: None,
                span,
            })
        }
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
            Ident, Int, Float, Char, String, ByteString, Byte, Semi, Comma, Dot, LParen,
            RParen, LBrace, RBrace, LBrack, RBrack, At, Tilde, QMark, Colon, Eq, Bang, Lt, Gt,
            Dash, Plus, Star, Slash, Caret, Percent, And, As, Break, Const, Continue, Else, Enum,
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
