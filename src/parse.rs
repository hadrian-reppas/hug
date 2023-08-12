use std::collections::VecDeque;

use crate::ast::*;
use crate::error::Error;
use crate::hir::{HirId, IdCell};
use crate::io::FileId;
use crate::lex::{Token, TokenKind, TokenKind::*, Tokens};
use crate::span::Span;

pub fn parse(code: &str, file_id: FileId) -> Result<Vec<UnloadedItem>, Error> {
    let mut parser = Parser::new(code, file_id);
    parser.items()
}

struct Parser<'a> {
    tokens: Tokens<'a>,
    peek: VecDeque<Token>,
}

impl<'a> Parser<'a> {
    fn new(code: &'a str, file_id: FileId) -> Self {
        Parser {
            tokens: Tokens::new(code, file_id),
            peek: VecDeque::new(),
        }
    }

    fn next(&mut self) -> Result<Token, Error> {
        if let Some(token) = self.peek.pop_front() {
            Ok(token)
        } else {
            self.tokens.next()
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Token, Error> {
        let token = self.next()?;
        if token.kind == expected {
            Ok(token)
        } else {
            Err(Error::new(
                format!("expected {}, found {}", expected.desc(), token.kind.desc()),
                Some(token.span),
            ))
        }
    }

    fn peek<S: Sequence>(&mut self, sequence: S) -> Result<bool, Error> {
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

    fn peek_kind(&mut self) -> Result<TokenKind, Error> {
        if self.peek.is_empty() {
            self.peek.push_back(self.tokens.next()?);
        }
        Ok(self.peek[0].kind)
    }

    fn peek_span(&mut self) -> Result<Span, Error> {
        if self.peek.is_empty() {
            self.peek.push_back(self.tokens.next()?);
        }
        Ok(self.peek[0].span)
    }

    fn consume<S: Sequence>(&mut self, sequence: S) -> Result<bool, Error> {
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

    fn items(&mut self) -> Result<Vec<UnloadedItem>, Error> {
        let mut items = Vec::new();
        while !self.peek(Eof)? {
            let annotations = self.annotations()?;
            items.push(self.item(annotations)?);
        }
        Ok(items)
    }

    fn item(&mut self, annotations: Vec<Annotation>) -> Result<UnloadedItem, Error> {
        let pub_span = if self.peek(Pub)? {
            Some(self.next()?.span)
        } else {
            None
        };

        match self.peek_kind()? {
            Fn => self.fn_def(annotations, pub_span),
            Struct => self.struct_def(annotations, pub_span),
            Enum => self.enum_def(annotations, pub_span),
            Impl => self.impl_block(annotations, pub_span),
            Type => self.type_alias(annotations, pub_span),
            Trait => self.trait_def(annotations, pub_span),
            Const => self.const_def(annotations, pub_span),
            Static => self.static_decl(annotations, pub_span),
            Use => self.use_decl(annotations, pub_span),
            Mod => self.module(annotations, pub_span),
            Extern => self.extern_item(annotations, pub_span),
            kind => Err(Error::new(
                format!("expected item, found {}", kind.desc()),
                Some(self.peek_span()?),
            )),
        }
    }

    fn annotations(&mut self) -> Result<Vec<Annotation>, Error> {
        let mut annotations = Vec::new();
        while self.peek(Hash)? {
            annotations.push(self.annotation()?);
        }
        Ok(annotations)
    }

    fn annotation(&mut self) -> Result<Annotation, Error> {
        let first = self.expect(Hash)?;
        self.expect(LBrack)?;
        let item = self.annotation_item()?;
        let last = self.expect(RBrack)?;

        let span = first.span.to(last.span);
        Ok(Annotation { item, span })
    }

    fn annotation_item(&mut self) -> Result<AnnotationItem, Error> {
        if self.peek(String)? {
            let span = self.expect(String)?.span;
            Ok(AnnotationItem::String { span })
        } else if self.peek(Ident)? {
            let name = self.name()?;

            let (args, span) = if self.consume(LParen)? {
                if self.peek(RParen)? {
                    let last = self.expect(RParen)?;
                    (Some(Vec::new()), name.span.to(last.span))
                } else {
                    let mut args = vec![self.annotation_item()?];
                    while !self.peek(RParen)? && !self.peek([Comma, RParen])? {
                        self.expect(Comma)?;
                        args.push(self.annotation_item()?);
                    }
                    self.consume(Comma)?;
                    let last = self.expect(RParen)?;
                    (Some(args), name.span.to(last.span))
                }
            } else {
                (None, name.span)
            };

            Ok(AnnotationItem::Name { name, args, span })
        } else {
            Err(Error::new(
                format!(
                    "expected annotation item, found {}",
                    self.peek_kind()?.desc()
                ),
                Some(self.peek_span()?),
            ))
        }
    }

    fn ty(&mut self) -> Result<Ty, Error> {
        match self.peek_kind()? {
            Ident | Crate => {
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
                let is_mut = self.consume(Mut)?;
                let ty = self.ty()?;

                let span = star_span.to(ty.span());
                Ok(Ty::Ptr {
                    is_mut,
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
                } else if self.consume(Semi)? {
                    let count = self.expect(Int)?.span;
                    let last = self.expect(RBrack)?;

                    let span = first.span.to(last.span);
                    Ok(Ty::Array {
                        ty: Box::new(ty),
                        count,
                        span,
                    })
                } else {
                    Err(Error::new(
                        format!(
                            "expected {} or {}, found {}",
                            RBrack.desc(),
                            Semi.desc(),
                            self.peek_kind()?.desc()
                        ),
                        Some(self.peek_span()?),
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
            kind => Err(Error::new(
                format!("expected type, found {}", kind.desc()),
                Some(self.peek_span()?),
            )),
        }
    }

    fn generic_args(&mut self) -> Result<GenericArgs, Error> {
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

    fn use_decl(
        &mut self,
        annotations: Vec<Annotation>,
        pub_span: Option<Span>,
    ) -> Result<UnloadedItem, Error> {
        let (first_span, is_pub) = self.handle_pub(pub_span, Use)?;

        let has_crate_prefix = self.consume(Crate)?;
        if has_crate_prefix {
            self.expect(Colon)?;
            self.expect(Colon)?;
        }

        let tree = self.use_tree()?;
        let last = self.expect(Semi)?;
        Ok(UnloadedItem::Use {
            annotations,
            is_pub,
            has_crate_prefix,
            tree,
            span: first_span.to(last.span),
        })
    }

    fn use_tree(&mut self) -> Result<UseTree, Error> {
        let prefix = self.pure_path()?;
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
        annotations: Vec<Annotation>,
        pub_span: Option<Span>,
    ) -> Result<UnloadedItem, Error> {
        let (first_span, is_pub) = self.handle_pub(pub_span, Struct)?;

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
            return Ok(UnloadedItem::Struct {
                annotations,
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
        Ok(UnloadedItem::Struct {
            annotations,
            is_pub,
            name,
            generic_params,
            fields,
            span,
        })
    }

    fn generic_params(&mut self) -> Result<GenericParams, Error> {
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

    fn struct_field(&mut self) -> Result<StructField, Error> {
        let annotations = self.annotations()?;
        if self.peek(Pub)? {
            let first = self.expect(Pub)?;
            let name = self.name()?;
            self.expect(Colon)?;
            let ty = self.ty()?;

            let span = first.span.to(ty.span());
            Ok(StructField {
                annotations,
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
                annotations,
                is_pub: false,
                name,
                ty,
                span,
            })
        }
    }

    fn enum_def(
        &mut self,
        annotations: Vec<Annotation>,
        pub_span: Option<Span>,
    ) -> Result<UnloadedItem, Error> {
        let (first_span, is_pub) = self.handle_pub(pub_span, Enum)?;

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
            return Ok(UnloadedItem::Enum {
                annotations,
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
        Ok(UnloadedItem::Enum {
            annotations,
            is_pub,
            name,
            generic_params,
            items,
            span,
        })
    }

    fn enum_item(&mut self) -> Result<EnumItem, Error> {
        let annotations = self.annotations()?;
        let name = self.name()?;

        if !self.consume(LParen)? {
            let span = name.span;
            return Ok(EnumItem {
                annotations,
                name,
                tuple: None,
                span,
            });
        }

        if self.peek(RParen)? {
            let last = self.expect(RParen)?;
            let span = name.span.to(last.span);
            return Ok(EnumItem {
                annotations,
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
            annotations,
            name,
            tuple: Some(tuple),
            span,
        })
    }

    fn type_alias(
        &mut self,
        annotations: Vec<Annotation>,
        pub_span: Option<Span>,
    ) -> Result<UnloadedItem, Error> {
        let (first_span, is_pub) = self.handle_pub(pub_span, Type)?;

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
        Ok(UnloadedItem::Type {
            annotations,
            is_pub,
            name,
            generic_params,
            ty,
            span,
        })
    }

    fn module(
        &mut self,
        annotations: Vec<Annotation>,
        pub_span: Option<Span>,
    ) -> Result<UnloadedItem, Error> {
        let (first_span, is_pub) = self.handle_pub(pub_span, Mod)?;
        let name = self.name()?;
        let last = self.expect(Semi)?;

        let span = first_span.to(last.span);
        Ok(UnloadedItem::Mod {
            annotations,
            is_pub,
            name,
            span,
        })
    }

    fn trait_def(
        &mut self,
        annotations: Vec<Annotation>,
        pub_span: Option<Span>,
    ) -> Result<UnloadedItem, Error> {
        let (first_span, is_pub) = self.handle_pub(pub_span, Trait)?;

        let name = self.name()?;
        let generic_params = if self.peek(Lt)? {
            Some(self.generic_params()?)
        } else {
            None
        };

        let self_bounds = if self.consume(Colon)? {
            let mut bounds = vec![self.trait_bound()?];
            while self.peek(Plus)? {
                self.consume(Plus)?;
                bounds.push(self.trait_bound()?);
            }
            bounds
        } else {
            Vec::new()
        };

        let where_clause = if self.peek(Where)? {
            Some(self.trait_where_clause()?)
        } else {
            None
        };

        self.expect(LBrace)?;
        let mut items = Vec::new();
        while !self.peek(RBrace)? {
            items.push(self.trait_item()?);
        }
        let last = self.expect(RBrace)?;

        let span = first_span.to(last.span);
        Ok(UnloadedItem::Trait {
            annotations,
            is_pub,
            name,
            generic_params,
            self_bounds,
            where_clause,
            items,
            span,
        })
    }

    fn trait_item(&mut self) -> Result<TraitItem, Error> {
        if self.peek(Pub)? {
            return Err(Error::new(
                format!("{} not permitted here", Pub.desc()),
                Some(self.peek_span()?),
            )
            .note(
                format!("{} is implied for trait functions", Pub.desc()),
                None,
            ));
        }

        let signature = self.signature()?;
        if self.peek(Semi)? {
            let last = self.expect(Semi)?;
            let span = signature.span.to(last.span);
            Ok(TraitItem::Required { signature, span })
        } else if self.peek(LBrace)? {
            let block = self.block()?;
            let span = signature.span.to(block.span);
            Ok(TraitItem::Provided {
                signature,
                block,
                span,
            })
        } else {
            Err(Error::new(
                format!(
                    "expected block or {} after signature, found {}",
                    Semi.desc(),
                    self.peek_kind()?.desc()
                ),
                Some(self.peek_span()?),
            ))
        }
    }

    fn trait_where_clause(&mut self) -> Result<TraitWhereClause, Error> {
        let first = self.expect(Where)?;

        let mut items = vec![self.trait_where_item()?];
        while self.peek([Comma, Ident])? || self.peek([Comma, LParen])? {
            self.expect(Comma)?;
            items.push(self.trait_where_item()?);
        }

        let last_span = if self.peek(Comma)? {
            self.expect(Comma)?.span
        } else {
            items.last().unwrap().span()
        };

        let span = first.span.to(last_span);
        Ok(TraitWhereClause { items, span })
    }

    fn trait_where_item(&mut self) -> Result<TraitWhereItem, Error> {
        if self.peek(LParen)? {
            let first = self.expect(LParen)?;
            let left = if self.consume(RParen)? {
                Vec::new()
            } else {
                let mut left = vec![self.name()?];
                while !self.peek(RParen)? && !self.peek([Comma, RParen])? {
                    self.expect(Comma)?;
                    left.push(self.name()?);
                }
                self.consume(Comma)?;
                self.consume(RParen)?;
                left
            };

            let dash = self.expect(Dash)?;
            let gt = self.expect(Gt)?;
            dash.span.by(gt.span, self.tokens.code())?;
            let right = self.name()?;

            let span = first.span.to(right.span);
            Ok(TraitWhereItem::Dependency { left, right, span })
        } else if self.peek([Ident, Colon])? {
            let param = self.name()?;
            self.expect(Colon)?;

            let mut bounds = vec![self.trait_bound()?];
            while self.peek(Plus)? {
                self.expect(Plus)?;
                bounds.push(self.trait_bound()?);
            }

            let span = param.span.to(bounds.last().unwrap().span());
            Ok(TraitWhereItem::Bound {
                param,
                bounds,
                span,
            })
        } else {
            let left = self.name()?;
            let dash = self.expect(Dash)?;
            let gt = self.expect(Gt)?;
            dash.span.by(gt.span, self.tokens.code())?;
            let right = self.name()?;

            let span = left.span.to(right.span);
            Ok(TraitWhereItem::Dependency {
                left: vec![left],
                right,
                span,
            })
        }
    }

    fn block(&mut self) -> Result<Block, Error> {
        let first = self.expect(LBrace)?;
        let mut stmts = Vec::new();
        while !self.peek(RBrace)? {
            let annotations = self.annotations()?;
            match self.peek_kind()? {
                Let => stmts.push(self.local(annotations)?),
                Use | Struct | Enum | Extern | Type | Fn | Const | Static => {
                    stmts.push(self.item(annotations)?.try_into().unwrap())
                }
                Semi => {
                    self.expect(Semi)?;
                }
                _ => {
                    let expr = self.expr(BindingPower::Start, true)?;
                    if self.peek(Semi)? {
                        let last = self.expect(Semi)?;
                        let span = expr.span().to(last.span);
                        stmts.push(Stmt::Expr {
                            annotations,
                            expr,
                            span,
                        });
                    } else if self.peek(RBrace)? {
                        if let (Some(first), Some(last)) = (annotations.get(0), annotations.last())
                        {
                            return Err(Error::new(
                                "annotations are not allowed before the final expression",
                                Some(first.span.to(last.span)),
                            ));
                        }

                        let last = self.expect(RBrace)?;
                        let span = first.span.to(last.span);
                        return Ok(Block {
                            stmts,
                            expr: Some(Box::new(expr)),
                            span,
                        });
                    } else if expr.has_block() {
                        let span = expr.span();
                        stmts.push(Stmt::Expr {
                            annotations,
                            expr,
                            span,
                        });
                    } else {
                        return Err(Error::new(
                            format!(
                                "expected {} or {} after expression, found {}",
                                Semi.desc(),
                                RBrace.desc(),
                                self.peek_kind()?.desc()
                            ),
                            Some(self.peek_span()?),
                        ));
                    }
                }
            }
        }
        let last = self.expect(RBrace)?;

        let span = first.span.to(last.span);
        Ok(Block {
            stmts,
            expr: None,
            span,
        })
    }

    fn local(&mut self, annotations: Vec<Annotation>) -> Result<Stmt, Error> {
        let first = self.expect(Let)?;
        let pattern = self.pattern()?;

        let ty = if self.consume(Colon)? {
            Some(self.ty()?)
        } else {
            None
        };

        let expr = if self.consume(Eq)? {
            Some(self.expr(BindingPower::Start, true)?)
        } else {
            None
        };

        let last = self.expect(Semi)?;
        let span = first.span.to(last.span);
        Ok(Stmt::Local {
            annotations,
            pattern,
            ty,
            expr,
            span,
        })
    }

    fn expr(&mut self, bp: BindingPower, allow_struct: bool) -> Result<Expr, Error> {
        macro_rules! literal {
            ($token:ident, $kind:ident) => {{
                let span = self.expect($token)?.span;
                Expr::Literal {
                    kind: LiteralKind::$kind,
                    span,
                }
            }};
        }

        macro_rules! uop {
            ($token:ident, $kind:ident, $bp:ident) => {{
                let op_span = self.expect($token)?.span;
                let expr = self.expr(BindingPower::$bp, allow_struct)?;
                let span = op_span.to(expr.span());
                Expr::Unary {
                    op: UnOp::$kind,
                    expr: Box::new(expr),
                    op_span,
                    span,
                }
            }};
        }

        let mut lhs = match self.peek_kind()? {
            Int => literal!(Int, Int),
            Float => literal!(Float, Float),
            Char => literal!(Char, Char),
            String => literal!(String, String),
            Byte => literal!(Byte, Byte),
            ByteString => literal!(ByteString, ByteString),
            True => literal!(True, Bool),
            False => literal!(False, Bool),

            Dash => uop!(Dash, Neg, Prefix),
            Bang => uop!(Bang, Not, Prefix),
            Not => uop!(Not, LogicalNot, Prefix),
            Star => uop!(Star, Deref, Prefix),
            Amp => uop!(Amp, AddrOf, Prefix),

            Ident | Crate => self.ident_expr(allow_struct)?,
            SelfValue => Expr::SelfValue {
                span: self.expect(SelfValue)?.span,
            },

            Lt => self.qualified_path()?,

            LParen => self.paren_expr()?,
            LBrack => self.brack_expr()?,
            LBrace => {
                let block = self.block()?;
                let span = block.span;
                Expr::Block { block, span }
            }

            If => self.if_expr()?,
            While => self.while_expr()?,
            Match => self.match_expr()?,
            For => {
                let first = self.expect(For)?;
                let pattern = self.pattern()?;
                self.expect(In)?;
                let iter = self.expr(BindingPower::Start, false)?;
                let block = self.block()?;
                let span = first.span.to(block.span);
                Expr::For {
                    pattern,
                    iter: Box::new(iter),
                    block,
                    span,
                }
            }
            Loop => {
                let first = self.expect(Loop)?;
                let block = self.block()?;
                let span = first.span.to(block.span);
                Expr::Loop { block, span }
            }
            Try => {
                let first = self.expect(Try)?;
                let block = self.block()?;
                let span = first.span.to(block.span);
                Expr::TryBlock { block, span }
            }
            Goto => {
                let first = self.expect(Goto)?;
                let label = self.label()?;
                if self.peek_kind()?.is_expr_start() {
                    let expr = self.expr(BindingPower::Start, allow_struct)?;
                    let span = first.span.to(expr.span());
                    Expr::Goto {
                        label,
                        expr: Some(Box::new(expr)),
                        span,
                    }
                } else {
                    Expr::Goto {
                        label,
                        expr: None,
                        span: first.span,
                    }
                }
            }
            Break => {
                let first = self.expect(Break)?;
                if self.peek_kind()?.is_expr_start() {
                    let expr = self.expr(BindingPower::Start, allow_struct)?;
                    let span = first.span.to(expr.span());
                    Expr::Break {
                        expr: Some(Box::new(expr)),
                        span,
                    }
                } else {
                    Expr::Break {
                        expr: None,
                        span: first.span,
                    }
                }
            }
            Continue => {
                let span = self.expect(Continue)?.span;
                Expr::Continue { span }
            }
            Return => {
                let first = self.expect(Return)?;
                if self.peek_kind()?.is_expr_start() {
                    let expr = self.expr(BindingPower::Start, allow_struct)?;
                    let span = first.span.to(expr.span());
                    Expr::Return {
                        expr: Some(Box::new(expr)),
                        span,
                    }
                } else {
                    Expr::Return {
                        expr: None,
                        span: first.span,
                    }
                }
            }
            At => {
                let label = self.label()?;
                let span = label.span;
                Expr::Label { label, span }
            }

            Dot => {
                let first_span = self.expect(Dot)?.span;
                let second_span = self.expect(Dot)?.span;

                let range_span = if self.peek(Eq)? {
                    let last_span = self.expect(Eq)?.span;
                    first_span
                        .by(second_span, self.tokens.code())?
                        .by(last_span, self.tokens.code())?
                } else {
                    first_span.by(second_span, self.tokens.code())?
                };

                if self.peek_kind()?.is_expr_start() {
                    let high = self.expr(BindingPower::Range, allow_struct)?;
                    if high.is_range() {
                        return Err(
                            Error::new("range bounds cannot be ranges", Some(high.span()))
                                .note("consider adding parentheses", None),
                        );
                    }
                    let span = range_span.to(high.span());
                    Expr::Range {
                        low: None,
                        high: Some(Box::new(high)),
                        range_span,
                        span,
                    }
                } else {
                    Expr::Range {
                        low: None,
                        high: None,
                        range_span,
                        span: range_span,
                    }
                }
            }

            kind => {
                return Err(Error::new(
                    format!("expected expression, found {}", kind.desc()),
                    Some(self.peek_span()?),
                ))
            }
        };

        while let Some(op_info) = self.next_op(bp)? {
            lhs = match op_info {
                OpInfo::BinOp {
                    op,
                    op_span,
                    bp,
                    is_cmp,
                } => {
                    let rhs = self.expr(bp, allow_struct)?;
                    let span = lhs.span().to(rhs.span());

                    if is_cmp && (lhs.is_cmp() || rhs.is_cmp()) {
                        return Err(Error::new(
                            "comparison operators cannot be chained",
                            Some(span),
                        )
                        .note("consider adding parentheses", None));
                    }

                    Expr::Binary {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        op_span,
                        span,
                    }
                }
                OpInfo::AssignOp { op, op_span } => {
                    let rhs = self.expr(BindingPower::Assign, allow_struct)?;
                    let span = lhs.span().to(rhs.span());
                    Expr::AssignOp {
                        op,
                        target: Box::new(lhs),
                        rhs: Box::new(rhs),
                        op_span,
                        span,
                    }
                }
                OpInfo::Assign => {
                    let rhs = self.expr(BindingPower::Assign, allow_struct)?;
                    let span = lhs.span().to(rhs.span());
                    Expr::Assign {
                        target: Box::new(lhs),
                        rhs: Box::new(rhs),
                        span,
                    }
                }
                OpInfo::Call => {
                    if lhs.is_cast() {
                        return Err(Error::new(
                            "cast cannot be followed by a call",
                            Some(lhs.span()),
                        )
                        .note("consider adding parentheses", None));
                    }

                    if self.peek(RParen)? {
                        let last = self.expect(RParen)?;
                        let span = lhs.span().to(last.span);
                        Expr::Call {
                            func: Box::new(lhs),
                            args: Vec::new(),
                            span,
                        }
                    } else {
                        let mut args = vec![self.expr(BindingPower::Start, true)?];
                        while !self.peek(RParen)? && !self.peek([Comma, RParen])? {
                            self.expect(Comma)?;
                            args.push(self.expr(BindingPower::Start, true)?);
                        }
                        self.consume(Comma)?;
                        let last = self.expect(RParen)?;
                        let span = lhs.span().to(last.span);
                        Expr::Call {
                            func: Box::new(lhs),
                            args,
                            span,
                        }
                    }
                }
                OpInfo::MethodCall => {
                    if lhs.is_cast() {
                        return Err(Error::new(
                            "cast cannot be followed by a method call",
                            Some(lhs.span()),
                        )
                        .note("consider adding parentheses", None));
                    }

                    let name = self.generic_segment()?;
                    self.expect(LParen)?;
                    if self.peek(RParen)? {
                        let last = self.expect(RParen)?;
                        let span = lhs.span().to(last.span);
                        Expr::MethodCall {
                            receiver: Box::new(lhs),
                            name,
                            args: Vec::new(),
                            span,
                        }
                    } else {
                        let mut args = vec![self.expr(BindingPower::Start, true)?];
                        while !self.peek(RParen)? && !self.peek([Comma, RParen])? {
                            self.expect(Comma)?;
                            args.push(self.expr(BindingPower::Start, true)?);
                        }
                        self.consume(Comma)?;
                        let last = self.expect(RParen)?;
                        let span = lhs.span().to(last.span);
                        Expr::MethodCall {
                            receiver: Box::new(lhs),
                            name,
                            args,
                            span,
                        }
                    }
                }
                OpInfo::Index => {
                    if lhs.is_cast() {
                        return Err(Error::new(
                            "cast cannot be followed by indexing",
                            Some(lhs.span()),
                        )
                        .note("consider adding parentheses", None));
                    }

                    let index = self.expr(BindingPower::Start, true)?;
                    let last = self.expect(RBrack)?;
                    let span = lhs.span().to(last.span);
                    Expr::Index {
                        expr: Box::new(lhs),
                        index: Box::new(index),
                        span,
                    }
                }
                OpInfo::Field => {
                    if lhs.is_cast() {
                        return Err(Error::new(
                            "cast cannot be followed by a field access",
                            Some(lhs.span()),
                        )
                        .note("consider adding parentheses", None));
                    }

                    if self.peek(Ident)? {
                        let name = self.name()?;
                        let span = lhs.span().to(name.span);
                        Expr::Field {
                            expr: Box::new(lhs),
                            name,
                            span,
                        }
                    } else if self.peek(Int)? {
                        let index = self.expect(Int)?.span;
                        let span = lhs.span().to(index);
                        Expr::TupleField {
                            expr: Box::new(lhs),
                            index,
                            span,
                        }
                    } else {
                        return Err(Error::new(
                            format!(
                                "expected {} or {}, found {}",
                                Ident.desc(),
                                Int.desc(),
                                self.peek_kind()?.desc()
                            ),
                            Some(self.peek_span()?),
                        ));
                    }
                }
                OpInfo::Cast => {
                    let ty = self.ty()?;
                    let span = lhs.span().to(ty.span());
                    Expr::Cast {
                        expr: Box::new(lhs),
                        ty,
                        span,
                    }
                }
                OpInfo::Range { range_span } => {
                    if lhs.is_range() {
                        return Err(
                            Error::new("range bounds cannot be ranges", Some(lhs.span()))
                                .note("consider adding parentheses", None),
                        );
                    }

                    let (high, span) = if self.peek_kind()?.is_expr_start() {
                        let high = self.expr(BindingPower::Range, allow_struct)?;
                        if high.is_range() {
                            return Err(Error::new(
                                "range bounds cannot be ranges",
                                Some(high.span()),
                            )
                            .note("consider adding parentheses", None));
                        }
                        let span = lhs.span().to(high.span());
                        (Some(Box::new(high)), span)
                    } else {
                        (None, lhs.span().to(range_span))
                    };

                    Expr::Range {
                        low: Some(Box::new(lhs)),
                        high,
                        range_span,
                        span,
                    }
                }
                OpInfo::Try { qmark_span } => {
                    let span = lhs.span().to(qmark_span);
                    Expr::Try {
                        expr: Box::new(lhs),
                        qmark_span,
                        span,
                    }
                }
            };
        }

        Ok(lhs)
    }

    fn next_op(&mut self, bp: BindingPower) -> Result<Option<OpInfo>, Error> {
        macro_rules! assign_op {
            ($tok1:ident, $tok2:ident => $op:ident) => {{
                if BindingPower::Assign < bp {
                    return Ok(None);
                }

                let span1 = self.expect($tok1)?.span;
                let span2 = self.expect($tok2)?.span;
                let op_span = span1.by(span2, self.tokens.code())?;
                Ok(Some(OpInfo::AssignOp {
                    op: AssignOp::$op,
                    op_span,
                }))
            }};

            ($tok1:ident, $tok2:ident, $tok3:ident => $op:ident) => {{
                if BindingPower::Assign < bp {
                    return Ok(None);
                }

                let span1 = self.expect($tok1)?.span;
                let span2 = self.expect($tok2)?.span;
                let span3 = self.expect($tok3)?.span;
                let op_span = span1
                    .by(span2, self.tokens.code())?
                    .by(span3, self.tokens.code())?;
                Ok(Some(OpInfo::AssignOp {
                    op: AssignOp::$op,
                    op_span,
                }))
            }};
        }

        macro_rules! bop {
            ($tok:ident => $op:ident, $bp:ident) => {{
                if BindingPower::$bp <= bp {
                    return Ok(None);
                }

                let op_span = self.expect($tok)?.span;
                Ok(Some(OpInfo::BinOp {
                    op: BinOp::$op,
                    op_span,
                    bp: BindingPower::$bp,
                    is_cmp: BindingPower::$bp == BindingPower::Cmp,
                }))
            }};

            ($tok1:ident, $tok2:ident => $op:ident, $bp:ident) => {{
                if BindingPower::$bp <= bp {
                    return Ok(None);
                }

                let first_span = self.expect($tok1)?.span;
                let last_span = self.expect($tok2)?.span;
                let op_span = first_span.by(last_span, self.tokens.code())?;

                Ok(Some(OpInfo::BinOp {
                    op: BinOp::$op,
                    op_span,
                    bp: BindingPower::$bp,
                    is_cmp: BindingPower::$bp == BindingPower::Cmp,
                }))
            }};
        }

        if self.peek([Plus, Eq])? {
            assign_op!(Plus, Eq => Add)
        } else if self.peek([Dash, Eq])? {
            assign_op!(Dash, Eq => Sub)
        } else if self.peek([Star, Eq])? {
            assign_op!(Star, Eq => Mul)
        } else if self.peek([Slash, Eq])? {
            assign_op!(Slash, Eq => Div)
        } else if self.peek([Percent, Eq])? {
            assign_op!(Percent, Eq => Rem)
        } else if self.peek([Amp, Eq])? {
            assign_op!(Amp, Eq => And)
        } else if self.peek([Bar, Eq])? {
            assign_op!(Bar, Eq => Or)
        } else if self.peek([Caret, Eq])? {
            assign_op!(Caret, Eq => Xor)
        } else if self.peek([Lt, Lt, Eq])? {
            assign_op!(Lt, Lt, Eq => Shl)
        } else if self.peek([Gt, Gt, Eq])? {
            assign_op!(Gt, Gt, Eq => Shr)
        } else if self.peek(Plus)? {
            bop!(Plus => Add, Sum)
        } else if self.peek(Dash)? {
            bop!(Dash => Sub, Sum)
        } else if self.peek(Star)? {
            bop!(Star => Mul, Prod)
        } else if self.peek(Slash)? {
            bop!(Slash => Div, Prod)
        } else if self.peek(Percent)? {
            bop!(Percent => Rem, Prod)
        } else if self.peek(Amp)? {
            bop!(Amp => And, And)
        } else if self.peek(Bar)? {
            bop!(Bar => Or, Or)
        } else if self.peek(Caret)? {
            bop!(Caret => Xor, Xor)
        } else if self.peek(And)? {
            bop!(And => LogicalAnd, LogicalAnd)
        } else if self.peek(Or)? {
            bop!(Or => LogicalOr, LogicalOr)
        } else if self.peek([Lt, Lt])? {
            bop!(Lt, Lt => Shl, Shift)
        } else if self.peek([Gt, Gt])? {
            bop!(Gt, Gt => Shr, Shift)
        } else if self.peek([Eq, Eq])? {
            bop!(Eq, Eq => Eq, Cmp)
        } else if self.peek([Bang, Eq])? {
            bop!(Bang, Eq => Ne, Cmp)
        } else if self.peek([Lt, Eq])? {
            bop!(Lt, Eq => Le, Cmp)
        } else if self.peek([Gt, Eq])? {
            bop!(Gt, Eq => Ge, Cmp)
        } else if self.peek(Lt)? {
            bop!(Lt => Lt, Cmp)
        } else if self.peek(Gt)? {
            bop!(Gt => Gt, Cmp)
        } else if self.peek(Eq)? {
            self.expect(Eq)?;
            Ok(Some(OpInfo::Assign))
        } else if self.peek(QMark)? {
            let qmark_span = self.expect(QMark)?.span;
            Ok(Some(OpInfo::Try { qmark_span }))
        } else if self.peek(As)? {
            self.expect(As)?;
            Ok(Some(OpInfo::Cast))
        } else if self.peek(LParen)? {
            self.expect(LParen)?;
            Ok(Some(OpInfo::Call))
        } else if self.peek(LBrack)? {
            self.expect(LBrack)?;
            Ok(Some(OpInfo::Index))
        } else if self.peek([Dot, Dot, Eq])? {
            let first_span = self.expect(Dot)?.span;
            let second_span = self.expect(Dot)?.span;
            let last_span = self.expect(Eq)?.span;
            let range_span = first_span
                .by(second_span, self.tokens.code())?
                .by(last_span, self.tokens.code())?;
            Ok(Some(OpInfo::Range { range_span }))
        } else if self.peek([Dot, Dot])? {
            let first_span = self.expect(Dot)?.span;
            let last_span = self.expect(Dot)?.span;
            let range_span = first_span.by(last_span, self.tokens.code())?;
            Ok(Some(OpInfo::Range { range_span }))
        } else if self.peek(Dot)? {
            self.expect(Dot)?;
            if self.peek([Ident, LParen])? || self.peek([Ident, Colon])? {
                Ok(Some(OpInfo::MethodCall))
            } else {
                Ok(Some(OpInfo::Field))
            }
        } else {
            Ok(None)
        }
    }

    fn paren_expr(&mut self) -> Result<Expr, Error> {
        let first = self.expect(LParen)?;
        if self.peek(RParen)? {
            let last = self.expect(RParen)?;
            let span = first.span.to(last.span);
            return Ok(Expr::Tuple {
                exprs: Vec::new(),
                span,
            });
        }

        let expr = self.expr(BindingPower::Start, true)?;
        if self.peek(RParen)? {
            let last = self.expect(RParen)?;
            let span = first.span.to(last.span);
            return Ok(Expr::Paren {
                expr: Box::new(expr),
                span,
            });
        }

        let mut exprs = vec![expr];
        while !self.peek(RParen)? && !self.peek([Comma, RParen])? {
            self.expect(Comma)?;
            exprs.push(self.expr(BindingPower::Start, true)?);
        }
        self.consume(Comma)?;
        let last = self.expect(RParen)?;

        let span = first.span.to(last.span);
        Ok(Expr::Tuple { exprs, span })
    }

    fn brack_expr(&mut self) -> Result<Expr, Error> {
        let first = self.expect(LBrack)?;
        if self.peek(RBrack)? {
            let last = self.expect(RBrack)?;
            let span = first.span.to(last.span);
            return Ok(Expr::Array {
                exprs: Vec::new(),
                span,
            });
        }

        let expr = self.expr(BindingPower::Start, true)?;
        if self.consume(Semi)? {
            let count = self.expect(Int)?.span;
            let last = self.expect(RBrack)?;
            let span = first.span.to(last.span);
            return Ok(Expr::Repeat {
                expr: Box::new(expr),
                count,
                span,
            });
        }

        let mut exprs = vec![expr];
        while !self.peek(RBrack)? && !self.peek([Comma, RBrack])? {
            self.expect(Comma)?;
            exprs.push(self.expr(BindingPower::Start, true)?);
        }
        self.consume(Comma)?;
        let last = self.expect(RBrack)?;

        let span = first.span.to(last.span);
        Ok(Expr::Array { exprs, span })
    }

    fn ident_expr(&mut self, allow_struct: bool) -> Result<Expr, Error> {
        if self.peek([Ident, Bang])? {
            return self.macro_expr();
        }

        let path = self.generic_path()?;
        if allow_struct && self.consume(LBrace)? {
            if self.peek(RBrace)? {
                let last = self.expect(RBrace)?;
                let span = path.span.to(last.span);
                return Ok(Expr::Struct {
                    path,
                    fields: Vec::new(),
                    span,
                });
            }

            let mut fields = vec![self.expr_field()?];
            while !self.peek(RBrace)? && !self.peek([Comma, RBrace])? {
                self.expect(Comma)?;
                fields.push(self.expr_field()?);
            }
            self.consume(Comma)?;
            let last = self.expect(RBrace)?;

            let span = path.span.to(last.span);
            Ok(Expr::Struct { path, fields, span })
        } else {
            let span = path.span;
            Ok(Expr::Path { path, span })
        }
    }

    fn qualified_path(&mut self) -> Result<Expr, Error> {
        let first = self.expect(Lt)?;
        let ty = self.ty()?;
        let as_trait = if self.consume(As)? {
            Some(self.trait_bound()?)
        } else {
            None
        };
        self.expect(Gt)?;
        self.expect(Colon)?;
        self.expect(Colon)?;
        let segment = self.generic_segment()?;

        let span = first.span.to(segment.span);
        Ok(Expr::QualifiedPath {
            ty,
            as_trait,
            segment,
            span,
        })
    }

    fn macro_expr(&mut self) -> Result<Expr, Error> {
        let name = self.name()?;
        let kind = match name.name.as_str() {
            "assert" => MacroKind::Assert,
            "assert_eq" => MacroKind::AssertEq,
            "assert_ne" => MacroKind::AssertNe,
            "column" => MacroKind::Column,
            "dbg" => MacroKind::Dbg,
            "eprint" => MacroKind::Eprint,
            "eprintln" => MacroKind::Eprintln,
            "file" => MacroKind::File,
            "format" => MacroKind::Format,
            "line" => MacroKind::Line,
            "panic" => MacroKind::Panic,
            "print" => MacroKind::Print,
            "println" => MacroKind::Println,
            "todo" => MacroKind::Todo,
            "unreachable" => MacroKind::Unreachable,
            "vec" => MacroKind::Vec,
            "write" => MacroKind::Write,
            "writeln" => MacroKind::Writeln,
            _ => {
                return Err(Error::new(
                    format!("the macro `{}` does not exist", name.name),
                    Some(name.span),
                ))
            }
        };
        self.expect(Bang)?;
        let close = if self.peek(LParen)? {
            self.expect(LParen)?;
            RParen
        } else if self.peek(LBrack)? {
            self.expect(LBrack)?;
            RBrack
        } else {
            return Err(Error::new(
                format!(
                    "expected {} or {} after macro name",
                    LParen.desc(),
                    LBrack.desc()
                ),
                Some(self.peek_span()?),
            ));
        };

        if self.peek(close)? {
            let last = self.expect(close)?.span;
            let span = name.span.to(last);
            return Ok(Expr::Macro {
                name,
                kind,
                args: Vec::new(),
                span,
            });
        }

        let mut args = vec![self.expr(BindingPower::Start, true)?];
        while !self.peek(close)? && !self.peek([Comma, close])? {
            self.expect(Comma)?;
            args.push(self.expr(BindingPower::Start, true)?);
        }
        self.consume(Comma)?;
        let last = self.expect(close)?.span;

        let span = name.span.to(last);
        Ok(Expr::Macro {
            name,
            kind,
            args,
            span,
        })
    }

    fn generic_path(&mut self) -> Result<GenericPath, Error> {
        let has_crate_prefix = self.consume(Crate)?;
        if has_crate_prefix {
            self.expect(Colon)?;
            self.expect(Colon)?;
        }

        let mut segments = vec![self.generic_segment()?];
        while self.peek([Colon, Colon, Ident])? {
            let first_colon = self.expect(Colon)?;
            let last_colon = self.expect(Colon)?;
            first_colon.span.by(last_colon.span, self.tokens.code())?;
            segments.push(self.generic_segment()?);
        }

        let span = segments[0].span.to(segments.last().unwrap().span);
        Ok(GenericPath {
            has_crate_prefix,
            segments,
            span,
        })
    }

    fn generic_segment(&mut self) -> Result<GenericSegment, Error> {
        let name = self.name()?;
        if self.peek([Colon, Colon, Lt])? {
            let first_colon = self.expect(Colon)?;
            let last_colon = self.expect(Colon)?;
            first_colon.span.by(last_colon.span, self.tokens.code())?;
            let generic_args = self.generic_args()?;

            let span = name.span.to(generic_args.span);
            Ok(GenericSegment {
                name,
                generic_args: Some(generic_args),
                span,
            })
        } else {
            let span = name.span;
            Ok(GenericSegment {
                name,
                generic_args: None,
                span,
            })
        }
    }

    fn expr_field(&mut self) -> Result<ExprField, Error> {
        let name = self.name()?;
        if self.consume(Colon)? {
            let expr = self.expr(BindingPower::Start, true)?;
            let span = name.span.to(expr.span());
            Ok(ExprField::Expr { name, expr, span })
        } else {
            let span = name.span;
            Ok(ExprField::Name { name, span })
        }
    }

    fn if_expr(&mut self) -> Result<Expr, Error> {
        let first = self.expect(If)?;
        if self.consume(Let)? {
            let pattern = self.pattern()?;
            self.expect(Eq)?;
            let expr = self.expr(BindingPower::Start, false)?;
            let block = self.block()?;
            let else_kind = self.else_kind(block.span)?;

            let span = first.span.to(else_kind.span());
            Ok(Expr::IfLet {
                pattern,
                expr: Box::new(expr),
                block,
                else_kind,
                span,
            })
        } else {
            let test = self.expr(BindingPower::Start, false)?;
            let block = self.block()?;
            let else_kind = self.else_kind(block.span)?;

            let span = first.span.to(else_kind.span());
            Ok(Expr::If {
                test: Box::new(test),
                block,
                else_kind,
                span,
            })
        }
    }

    fn else_kind(&mut self, span: Span) -> Result<ElseKind, Error> {
        if !self.peek(Else)? {
            return Ok(ElseKind::Nothing { span: span.after() });
        }

        let first = self.expect(Else)?;
        if self.consume(If)? {
            if self.consume(Let)? {
                let pattern = self.pattern()?;
                self.expect(Eq)?;
                let expr = self.expr(BindingPower::Start, false)?;
                let block = self.block()?;
                let else_kind = self.else_kind(block.span)?;

                let span = first.span.to(else_kind.span());
                Ok(ElseKind::ElseIfLet {
                    pattern,
                    expr: Box::new(expr),
                    block,
                    else_kind: Box::new(else_kind),
                    span,
                })
            } else {
                let test = self.expr(BindingPower::Start, false)?;
                let block = self.block()?;
                let else_kind = self.else_kind(block.span)?;

                let span = first.span.to(else_kind.span());
                Ok(ElseKind::ElseIf {
                    test: Box::new(test),
                    block,
                    else_kind: Box::new(else_kind),
                    span,
                })
            }
        } else {
            let block = self.block()?;
            let span = first.span.to(block.span);
            Ok(ElseKind::Else { block, span })
        }
    }

    fn while_expr(&mut self) -> Result<Expr, Error> {
        let first = self.expect(While)?;
        if self.consume(Let)? {
            let pattern = self.pattern()?;
            self.expect(Eq)?;
            let expr = self.expr(BindingPower::Start, false)?;
            let block = self.block()?;
            let span = first.span.to(block.span);
            Ok(Expr::WhileLet {
                pattern,
                expr: Box::new(expr),
                block,
                span,
            })
        } else {
            let test = self.expr(BindingPower::Start, false)?;
            let block = self.block()?;
            let span = first.span.to(block.span);
            Ok(Expr::While {
                test: Box::new(test),
                block,
                span,
            })
        }
    }

    fn match_expr(&mut self) -> Result<Expr, Error> {
        let first = self.expect(Match)?;
        let expr = self.expr(BindingPower::Start, false)?;
        self.expect(LBrace)?;

        let mut arms = Vec::new();
        while !self.peek(RBrace)? {
            arms.push(self.match_arm()?);
        }
        let last = self.expect(RBrace)?;

        let span = first.span.to(last.span);
        Ok(Expr::Match {
            expr: Box::new(expr),
            arms,
            span,
        })
    }

    fn match_arm(&mut self) -> Result<MatchArm, Error> {
        let pattern = self.pattern()?;
        let eq = self.expect(Eq)?;
        let gt = self.expect(Gt)?;
        eq.span.by(gt.span, self.tokens.code())?;
        let body = self.expr(BindingPower::Start, true)?;
        if self.peek(RBrace)? {
            self.consume(Comma)?;
        } else {
            self.expect(Comma)?;
        }

        let span = pattern.span().to(body.span());
        Ok(MatchArm {
            pattern,
            body,
            span,
        })
    }

    fn label(&mut self) -> Result<Label, Error> {
        let first = self.expect(At)?;
        let name = self.name()?;
        let span = first.span.to(name.span);
        Ok(Label { name, span })
    }

    fn extern_item(
        &mut self,
        annotations: Vec<Annotation>,
        pub_span: Option<Span>,
    ) -> Result<UnloadedItem, Error> {
        let (first_span, is_pub) = self.handle_pub(pub_span, Extern)?;

        match self.peek_kind()? {
            Fn => {
                let signature = self.signature()?;
                let last = self.expect(Semi)?.span;
                let span = first_span.to(last);
                Ok(UnloadedItem::ExternFn {
                    annotations,
                    is_pub,
                    signature,
                    span,
                })
            }
            Type => {
                self.expect(Type)?;
                let name = self.name()?;
                let info = if self.consume(Eq)? {
                    let name = self.name()?;
                    self.expect(LParen)?;
                    let size = self.expect(Int)?.span;
                    self.expect(Comma)?;
                    let align = self.expect(Int)?.span;
                    let last = self.expect(RParen)?;
                    let span = name.span.to(last.span);
                    Some(ExternTypeInfo {
                        name,
                        size,
                        align,
                        span,
                    })
                } else {
                    None
                };
                let last = self.expect(Semi)?;

                let span = first_span.to(last.span);
                Ok(UnloadedItem::ExternType {
                    annotations,
                    is_pub,
                    name,
                    info,
                    span,
                })
            }
            Static => {
                self.expect(Static)?;
                let name = self.name()?;
                self.expect(Colon)?;
                let ty = self.ty()?;
                let last = self.expect(Semi)?;

                let span = first_span.to(last.span);
                Ok(UnloadedItem::ExternStatic {
                    annotations,
                    is_pub,
                    name,
                    ty,
                    span,
                })
            }
            kind => Err(Error::new(
                format!(
                    "expected {}, {} or {}, found {}",
                    Fn.desc(),
                    Type.desc(),
                    Static.desc(),
                    kind.desc()
                ),
                Some(self.peek_span()?),
            )),
        }
    }

    fn signature(&mut self) -> Result<Signature, Error> {
        let first = self.expect(Fn)?;
        let name = self.name()?;
        let generic_params = if self.peek(Lt)? {
            Some(self.generic_params()?)
        } else {
            None
        };

        self.expect(LParen)?;
        let self_kind = if self.peek(Star)? {
            let first = self.expect(Star)?;
            let is_mut = self.consume(Mut)?;
            let last = self.expect(SelfValue)?;
            let span = first.span.to(last.span);
            SelfKind::Ptr { is_mut, span }
        } else if self.peek(SelfValue)? {
            let span = self.expect(SelfValue)?.span;
            SelfKind::Value {
                is_mut: false,
                span,
            }
        } else if self.peek([Mut, SelfValue])? {
            let first = self.expect(Mut)?;
            let last = self.expect(SelfValue)?;
            let span = first.span.to(last.span);
            SelfKind::Value { is_mut: true, span }
        } else {
            SelfKind::None
        };

        let (params, variadic) = self.params(!self_kind.is_none())?;
        let last_span = self.expect(RParen)?.span;

        let (ret, last_span) = if self.peek(Dash)? {
            let dash = self.expect(Dash)?;
            let gt = self.expect(Gt)?;
            dash.span.by(gt.span, self.tokens.code())?;

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

        let span = first.span.to(last_span);
        Ok(Signature {
            name,
            generic_params,
            self_kind,
            params,
            variadic,
            ret,
            where_clause,
            span,
        })
    }

    fn params(&mut self, has_self_param: bool) -> Result<(Vec<Param>, Option<Variadic>), Error> {
        if has_self_param && !self.peek(RParen)? {
            self.expect(Comma)?;
        }
        if self.peek(Comma)? {
            return Err(Error::new(
                format!("expected parameter, found {}", Comma.desc()),
                Some(self.peek_span()?),
            ));
        }

        if self.peek(RParen)? {
            return Ok((Vec::new(), None));
        }

        let pattern = self.pattern()?;
        self.expect(Colon)?;

        if self.peek(Dot)? {
            let first = self.expect(Dot)?;
            let second = self.expect(Dot)?;
            let last = self.expect(Dot)?;
            let dots = first
                .span
                .by(second.span, self.tokens.code())?
                .by(last.span, self.tokens.code())?;

            let span = pattern.span();
            let Pattern::Name { name, .. } = pattern else {
                return Err(Error::new(
                    "variadic arguments cannot be deconstructed",
                    Some(span),
                ));
            };

            let span = name.span.to(dots);
            return Ok((Vec::new(), Some(Variadic { name, span })));
        }

        let ty = self.ty()?;
        let span = pattern.span().to(ty.span());

        let mut params = vec![Param { pattern, ty, span }];
        while !self.peek(RParen)? && !self.peek([Comma, RParen])? {
            self.expect(Comma)?;

            let pattern = self.pattern()?;
            self.expect(Colon)?;

            if self.peek(Dot)? {
                let first = self.expect(Dot)?;
                let second = self.expect(Dot)?;
                let last = self.expect(Dot)?;
                let dots = first
                    .span
                    .by(second.span, self.tokens.code())?
                    .by(last.span, self.tokens.code())?;

                let span = pattern.span();
                let Pattern::Name { name, .. } = pattern else {
                    return Err(Error::new(
                        "variadic arguments cannot be deconstructed",
                        Some(span),
                    ));
                };

                let span = name.span.to(dots);
                return Ok((params, Some(Variadic { name, span })));
            }

            let ty = self.ty()?;
            let span = pattern.span().to(ty.span());
            params.push(Param { pattern, ty, span });
        }
        self.consume(Comma)?;

        Ok((params, None))
    }

    fn pattern(&mut self) -> Result<Pattern, Error> {
        let mut patterns = vec![self.single_pattern()?];
        while self.consume(Bar)? {
            patterns.push(self.single_pattern()?);
        }
        if patterns.len() == 1 {
            Ok(patterns.pop().unwrap())
        } else {
            let first_span = patterns[0].span();
            let last_span = patterns.last().unwrap().span();
            let span = first_span.to(last_span);
            Ok(Pattern::Or { patterns, span })
        }
    }

    fn single_pattern(&mut self) -> Result<Pattern, Error> {
        macro_rules! literal {
            ($name:ident) => {
                Ok(Pattern::Literal {
                    kind: PatternLiteralKind::$name,
                    span: self.expect($name)?.span,
                })
            };
        }
        match self.peek_kind()? {
            Under => {
                let span = self.expect(Under)?.span;
                Ok(Pattern::Wild { span })
            }
            Ident | Crate => {
                let path = self.path()?;
                if self.peek(LBrace)? {
                    self.struct_pattern(path)
                } else if self.peek(LParen)? {
                    let (tuple, last_span) = self.tuple_pattern(true)?;
                    let span = path.span.to(last_span);
                    Ok(Pattern::Enum { path, tuple, span })
                } else {
                    let span = path.span;
                    match path.into_name() {
                        Ok(name) => Ok(Pattern::Name {
                            is_mut: false,
                            name,
                            span,
                        }),
                        Err(path) => Ok(Pattern::Path { path, span }),
                    }
                }
            }
            Mut => {
                let first = self.expect(Mut)?;
                let name = self.name()?;
                let span = first.span.to(name.span);
                Ok(Pattern::Name {
                    is_mut: true,
                    name,
                    span,
                })
            }
            LParen => {
                let (tuple, span) = self.tuple_pattern(false)?;
                Ok(Pattern::Tuple { tuple, span })
            }
            LBrack => {
                let first = self.expect(LBrack)?;
                if self.peek(RBrack)? {
                    let last = self.expect(RBrack)?;
                    let span = first.span.to(last.span);
                    return Ok(Pattern::Array {
                        array: Vec::new(),
                        span,
                    });
                }

                let mut array = vec![self.pattern()?];
                while !self.peek(RBrack)? && !self.peek([Comma, RBrack])? {
                    self.expect(Comma)?;
                    array.push(self.pattern()?);
                }
                self.consume(Comma)?;
                let last = self.expect(RBrack)?;

                let span = first.span.to(last.span);
                Ok(Pattern::Array { array, span })
            }
            Int => literal!(Int),
            Char => literal!(Char),
            String => literal!(String),
            ByteString => literal!(ByteString),
            Byte => literal!(Byte),
            True => Ok(Pattern::Literal {
                kind: PatternLiteralKind::Bool,
                span: self.expect(True)?.span,
            }),
            False => Ok(Pattern::Literal {
                kind: PatternLiteralKind::Bool,
                span: self.expect(False)?.span,
            }),
            Float => Err(Error::new(
                "floating-point types cannot be used in patterns",
                Some(self.peek_span()?),
            )),
            kind => Err(Error::new(
                format!("expected pattern, found {}", kind.desc()),
                Some(self.peek_span()?),
            )),
        }
    }

    fn struct_pattern(&mut self, path: Path) -> Result<Pattern, Error> {
        self.expect(LBrace)?;
        if self.peek(RBrace)? {
            let last = self.expect(RBrace)?;
            let span = path.span.to(last.span);
            return Ok(Pattern::Struct {
                path,
                fields: Vec::new(),
                dots: None,
                span,
            });
        } else if self.peek(Dot)? {
            let first_dot = self.expect(Dot)?;
            let last_dot = self.expect(Dot)?;
            let dots = first_dot.span.by(last_dot.span, self.tokens.code())?;
            let last = self.expect(RBrace)?;
            let span = path.span.to(last.span);
            return Ok(Pattern::Struct {
                path,
                fields: Vec::new(),
                dots: Some(dots),
                span,
            });
        }

        let mut fields = vec![self.field_pattern()?];
        while !self.peek(RBrace)? && !self.peek([Comma, RBrace])? && !self.peek([Comma, Dot])? {
            self.expect(Comma)?;
            fields.push(self.field_pattern()?);
        }
        self.consume(Comma)?;
        let dots = if self.peek(Dot)? {
            let first = self.expect(Dot)?;
            let last = self.expect(Dot)?;
            Some(first.span.to(last.span))
        } else {
            None
        };
        let last = self.expect(RBrace)?;

        let span = path.span.to(last.span);
        Ok(Pattern::Struct {
            path,
            fields,
            dots,
            span,
        })
    }

    fn field_pattern(&mut self) -> Result<FieldPattern, Error> {
        if self.peek(Mut)? {
            let first = self.expect(Mut)?;
            let name = self.name()?;
            let span = first.span.to(name.span);
            Ok(FieldPattern::Name {
                is_mut: true,
                name,
                span,
            })
        } else {
            let name = self.name()?;
            if self.consume(Colon)? {
                let pattern = self.pattern()?;
                let span = name.span.to(pattern.span());
                Ok(FieldPattern::Pattern {
                    name,
                    pattern,
                    span,
                })
            } else {
                let span = name.span;
                Ok(FieldPattern::Name {
                    is_mut: false,
                    name,
                    span,
                })
            }
        }
    }

    fn tuple_pattern(&mut self, allow_singleton: bool) -> Result<(Vec<Pattern>, Span), Error> {
        let first = self.expect(LParen)?;
        if self.peek(RParen)? {
            let last = self.expect(RParen)?;
            let span = first.span.to(last.span);
            return Ok((Vec::new(), span));
        }

        let mut tuple = vec![self.pattern()?];
        while !self.peek(RParen)? && !self.peek([Comma, RParen])? {
            self.expect(Comma)?;
            tuple.push(self.pattern()?);
        }
        if !self.consume(Comma)? && !allow_singleton && tuple.len() == 1 {
            let last_span = self.peek_span()?;
            let span = first.span.to(last_span);
            return Err(Error::new(
                "parenthesized patterns are not allowed",
                Some(span),
            ));
        }
        let last = self.expect(RParen)?;

        let span = first.span.to(last.span);
        Ok((tuple, span))
    }

    fn where_clause(&mut self) -> Result<WhereClause, Error> {
        let first = self.expect(Where)?;

        let mut items = vec![self.where_item()?];
        while self.peek([Comma, Ident])? {
            self.expect(Comma)?;
            items.push(self.where_item()?);
        }

        let last_span = if self.peek(Comma)? {
            self.expect(Comma)?.span
        } else {
            items.last().unwrap().span
        };

        let span = first.span.to(last_span);
        Ok(WhereClause { items, span })
    }

    fn where_item(&mut self) -> Result<WhereItem, Error> {
        let param = self.name()?;
        self.expect(Colon)?;

        let mut bounds = vec![self.trait_bound()?];
        while self.peek(Plus)? {
            self.expect(Plus)?;
            bounds.push(self.trait_bound()?);
        }

        let span = param.span.to(bounds.last().unwrap().span());
        Ok(WhereItem {
            param,
            bounds,
            span,
        })
    }

    fn trait_bound(&mut self) -> Result<TraitBound, Error> {
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
                let dash = self.expect(Dash)?;
                let gt = self.expect(Gt)?;
                dash.span.by(gt.span, self.tokens.code())?;

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

    fn fn_def(
        &mut self,
        annotations: Vec<Annotation>,
        pub_span: Option<Span>,
    ) -> Result<UnloadedItem, Error> {
        let signature = self.signature()?;
        let block = self.block()?;

        let (span, is_pub) = if let Some(span) = pub_span {
            (span.to(block.span), true)
        } else {
            (signature.span.to(block.span), false)
        };
        Ok(UnloadedItem::Fn {
            annotations,
            is_pub,
            signature,
            block,
            span,
        })
    }

    fn impl_block(
        &mut self,
        annotations: Vec<Annotation>,
        pub_span: Option<Span>,
    ) -> Result<UnloadedItem, Error> {
        self.disallow_pub(pub_span, Impl)?;

        let first = self.expect(Impl)?;
        let generic_params = if self.peek(Lt)? {
            Some(self.generic_params()?)
        } else {
            None
        };

        let ty = self.ty()?;

        let as_trait = if self.consume(As)? {
            Some(self.trait_bound()?)
        } else {
            None
        };

        let where_clause = if self.peek(Where)? {
            Some(self.where_clause()?)
        } else {
            None
        };

        self.expect(LBrace)?;
        let mut fns = Vec::new();
        while !self.peek(RBrace)? {
            fns.push(self.impl_fn()?);
        }
        let last = self.expect(RBrace)?;

        let span = first.span.to(last.span);
        Ok(UnloadedItem::Impl {
            annotations,
            generic_params,
            ty,
            as_trait,
            where_clause,
            fns,
            span,
        })
    }

    fn impl_fn(&mut self) -> Result<ImplFn, Error> {
        let annotations = self.annotations()?;
        let pub_span = if self.peek(Pub)? {
            Some(self.next()?.span)
        } else {
            None
        };

        let signature = self.signature()?;
        let block = self.block()?;

        let (span, is_pub) = if let Some(span) = pub_span {
            (span.to(block.span), true)
        } else {
            (signature.span.to(block.span), false)
        };

        Ok(ImplFn {
            annotations,
            is_pub,
            signature,
            block,
            span,
        })
    }

    fn const_def(
        &mut self,
        annotations: Vec<Annotation>,
        pub_span: Option<Span>,
    ) -> Result<UnloadedItem, Error> {
        let (first_span, is_pub) = self.handle_pub(pub_span, Const)?;
        let name = self.name()?;
        self.expect(Colon)?;
        let ty = self.ty()?;
        self.expect(Eq)?;
        let expr = self.expr(BindingPower::Start, true)?;
        let last = self.expect(Semi)?;

        let span = first_span.to(last.span);
        Ok(UnloadedItem::Const {
            annotations,
            is_pub,
            name,
            ty,
            expr,
            span,
        })
    }

    fn static_decl(
        &mut self,
        annotations: Vec<Annotation>,
        pub_span: Option<Span>,
    ) -> Result<UnloadedItem, Error> {
        let (first_span, is_pub) = self.handle_pub(pub_span, Static)?;
        let name = self.name()?;
        self.expect(Colon)?;
        let ty = self.ty()?;
        let expr = if self.consume(Eq)? {
            Some(self.expr(BindingPower::Start, true)?)
        } else {
            None
        };
        let last = self.expect(Semi)?;

        let span = first_span.to(last.span);
        Ok(UnloadedItem::Static {
            annotations,
            is_pub,
            name,
            ty,
            expr,
            span,
        })
    }

    fn handle_pub(
        &mut self,
        pub_span: Option<Span>,
        kind: TokenKind,
    ) -> Result<(Span, bool), Error> {
        if let Some(span) = pub_span {
            self.expect(kind)?;
            Ok((span, true))
        } else {
            Ok((self.expect(kind)?.span, false))
        }
    }

    fn disallow_pub(&mut self, pub_span: Option<Span>, kind: TokenKind) -> Result<(), Error> {
        if let Some(span) = pub_span {
            Err(Error::new(
                format!("{} not permitted before {}", Pub.desc(), kind.desc()),
                Some(span),
            ))
        } else {
            Ok(())
        }
    }

    fn name(&mut self) -> Result<Name, Error> {
        let token = self.expect(Ident)?;
        let span = token.span;
        let name = self.tokens.code()[span.start..span.end].to_string();
        Ok(Name {
            name,
            span,
            id: HirId::new(),
            res: IdCell::uninit(),
        })
    }

    fn path(&mut self) -> Result<Path, Error> {
        let has_crate_prefix = self.consume(Crate)?;
        if has_crate_prefix {
            self.expect(Colon)?;
            self.expect(Colon)?;
        }

        let mut path = vec![self.name()?];
        while self.peek([Colon, Colon, Ident])? {
            self.expect(Colon)?;
            self.expect(Colon)?;
            path.push(self.name()?);
        }
        let span = path[0].span.to(path.last().unwrap().span);
        Ok(Path {
            has_crate_prefix,
            path,
            span,
        })
    }

    fn pure_path(&mut self) -> Result<PurePath, Error> {
        let mut path = vec![self.name()?];
        while self.peek([Colon, Colon, Ident])? {
            self.expect(Colon)?;
            self.expect(Colon)?;
            path.push(self.name()?);
        }
        let span = path[0].span.to(path.last().unwrap().span);
        Ok(PurePath { path, span })
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
            Ident, Under, Int, Float, Char, String, ByteString, Byte, Semi, Comma, Dot, LParen,
            RParen, LBrace, RBrace, LBrack, RBrack, At, Tilde, QMark, Colon, Eq, Bang, Lt, Gt,
            Dash, Plus, Star, Slash, Caret, Percent, Amp, Bar, Hash, And, As, Break, Const, Continue,
            Crate, Else, Enum, Extern, False, Fn, For, Goto, If, Impl, In, Let, Loop, Match, Mod,
            Mut, Not, Or, Pub, Return, SelfType, SelfValue, Static, Struct, Trait, Try, True,
            Type, Use, Where, While, Eof,
        }
    }
}

impl<const N: usize> Sequence for [TokenKind; N] {
    fn as_slice(&self) -> &[TokenKind] {
        self
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum BindingPower {
    Start,
    Assign,
    Range,
    LogicalOr,
    LogicalAnd,
    Cmp,
    Or,
    Xor,
    And,
    Shift,
    Sum,
    Prod,
    Prefix,
}

enum OpInfo {
    BinOp {
        op: BinOp,
        op_span: Span,
        bp: BindingPower,
        is_cmp: bool,
    },
    AssignOp {
        op: AssignOp,
        op_span: Span,
    },
    Assign,
    Call,
    MethodCall,
    Index,
    Field,
    Cast,
    Range {
        range_span: Span,
    },
    Try {
        qmark_span: Span,
    },
}
