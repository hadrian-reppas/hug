use crate::ast::*;
use crate::error::Error;
use crate::lex::{TokenKind::*, Tokens};
use crate::span::Span;

pub fn unit<'a, 'b>(tokens: &mut Tokens<'a, 'b>) -> Result<Vec<Item<'a, 'b>>, Error<'a, 'b>> {
    let mut items = Vec::new();
    while !tokens.peek(Eof)? {
        items.push(item(tokens)?);
    }
    Ok(items)
}

fn item<'a, 'b>(tokens: &mut Tokens<'a, 'b>) -> Result<Item<'a, 'b>, Error<'a, 'b>> {
    let pub_span = if tokens.peek(Pub)? {
        Some(tokens.next()?.span)
    } else {
        None
    };

    match tokens.peek_kind()? {
        Fn => todo!(),
        Struct => struct_decl(tokens, pub_span),
        Enum => todo!(),
        Trait => todo!(),
        Type => todo!(),
        Const => todo!(),
        Global => todo!(),
        Use => use_decl(tokens, pub_span),
        Mod => todo!(),
        Extern => todo!(),
        Pub => todo!(),
        kind => Err(Error::Parse(
            format!("expected item, found {}", kind.description()),
            tokens.peek_span()?,
            vec![],
        )),
    }
}

fn use_decl<'a, 'b>(
    tokens: &mut Tokens<'a, 'b>,
    pub_span: Option<Span<'a, 'b>>,
) -> Result<Item<'a, 'b>, Error<'a, 'b>> {
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

    let first = tokens.expect(Use)?;
    let tree = use_tree(tokens)?;
    let last = tokens.expect(Semicolon)?;
    Ok(Item::Use {
        tree,
        span: first.span.to(last.span),
    })
}

fn use_tree<'a, 'b>(tokens: &mut Tokens<'a, 'b>) -> Result<UseTree<'a, 'b>, Error<'a, 'b>> {
    let prefix = path(tokens)?;
    if tokens.consume([Colon, Colon, LBrace])? {
        if tokens.peek(RBrace)? {
            let last = tokens.expect(RBrace)?;
            let span = prefix.span.to(last.span);
            return Ok(UseTree {
                prefix,
                kind: UseTreeKind::Nested(Vec::new()),
                span,
            });
        }

        let mut nested = vec![use_tree(tokens)?];
        while !tokens.peek(RBrace)? && !tokens.peek([Comma, RBrace])? {
            tokens.expect(Comma)?;
            nested.push(use_tree(tokens)?);
        }
        tokens.consume(Comma)?;

        let last = tokens.expect(RBrace)?;
        let span = prefix.span.to(last.span);
        Ok(UseTree {
            prefix,
            kind: UseTreeKind::Nested(nested),
            span,
        })
    } else if tokens.consume(As)? {
        let rename = name(tokens)?;
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

fn struct_decl<'a, 'b>(
    tokens: &mut Tokens<'a, 'b>,
    pub_span: Option<Span<'a, 'b>>,
) -> Result<Item<'a, 'b>, Error<'a, 'b>> {
    let (first_span, is_pub) = if let Some(span) = pub_span {
        tokens.expect(Struct)?;
        (span, true)
    } else {
        (tokens.expect(Struct)?.span, false)
    };

    let name = name(tokens)?;
    let generic_params = if tokens.peek(Lt)? {
        Some(generic_params(tokens)?)
    } else {
        None
    };

    tokens.expect(LBrace)?;

    if tokens.peek(RBrace)? {
        let last = tokens.expect(RBrace)?;
        let span = first_span.to(last.span);
        return Ok(Item::Struct {
            is_pub,
            name,
            generic_params,
            fields: Vec::new(),
            span,
        });
    }

    let mut fields = vec![struct_field(tokens)?];
    while !tokens.peek(RBrace)? && !tokens.peek([Comma, RBrace])? {
        tokens.expect(Comma)?;
        fields.push(struct_field(tokens)?);
    }
    tokens.consume(Comma)?;
    let last = tokens.expect(RBrace)?;

    let span = first_span.to(last.span);
    Ok(Item::Struct {
        is_pub,
        name,
        generic_params,
        fields,
        span,
    })
}

fn generic_params<'a, 'b>(
    tokens: &mut Tokens<'a, 'b>,
) -> Result<GenericParams<'a, 'b>, Error<'a, 'b>> {
    let first = tokens.expect(Lt)?;

    if tokens.peek(Gt)? {
        let last = tokens.expect(Gt)?;
        let span = first.span.to(last.span);
        return Ok(GenericParams {
            params: Vec::new(),
            span,
        });
    }

    let mut params = vec![name(tokens)?];
    while !tokens.peek(Gt)? && !tokens.peek([Comma, Gt])? {
        tokens.expect(Comma)?;
        params.push(name(tokens)?);
    }
    tokens.consume(Comma)?;
    let last = tokens.expect(Gt)?;

    let span = first.span.to(last.span);
    Ok(GenericParams { params, span })
}

fn struct_field<'a, 'b>(tokens: &mut Tokens<'a, 'b>) -> Result<StructField<'a, 'b>, Error<'a, 'b>> {
    todo!()
}

fn name<'a, 'b>(tokens: &mut Tokens<'a, 'b>) -> Result<Name<'a, 'b>, Error<'a, 'b>> {
    let token = tokens.expect(Ident)?;
    Ok(Name {
        name: token.span.text,
        span: token.span,
    })
}

fn path<'a, 'b>(tokens: &mut Tokens<'a, 'b>) -> Result<Path<'a, 'b>, Error<'a, 'b>> {
    let mut path = vec![name(tokens)?];
    while tokens.peek([Colon, Colon, Ident])? {
        tokens.expect(Colon)?;
        tokens.expect(Colon)?;
        path.push(name(tokens)?);
    }
    let span = path[0].span.to(path.last().unwrap().span);
    Ok(Path { path, span })
}
