use std::fmt::{self, Debug, Formatter};

use crate::span::Span;

pub struct Name<'a, 'b> {
    pub name: &'a str,
    pub span: Span<'a, 'b>,
}

impl Debug for Name<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Name").field(&self.name).finish()
    }
}

pub struct Path<'a, 'b> {
    pub path: Vec<Name<'a, 'b>>,
    pub span: Span<'a, 'b>,
}

impl Debug for Path<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Path").field(&self.path).finish()
    }
}

pub enum Item<'a, 'b> {
    Use {
        tree: UseTree<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Struct {
        is_pub: bool,
        name: Name<'a, 'b>,
        generic_params: Option<GenericParams<'a, 'b>>,
        fields: Vec<StructField<'a, 'b>>,
        span: Span<'a, 'b>,
    },
}

impl<'a, 'b> Item<'a, 'b> {
    pub fn span(&self) -> Span<'a, 'b> {
        match self {
            Item::Use { span, .. } | Item::Struct { span, .. } => *span,
        }
    }
}

impl Debug for Item<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Item::Use { tree, .. } => f.debug_tuple("Use").field(&tree).finish(),
            Item::Struct {
                is_pub,
                name,
                generic_params,
                fields,
                ..
            } => f
                .debug_struct("StructDecl")
                .field("is_pub", is_pub)
                .field("name", name)
                .field("generic_params", generic_params)
                .field("fields", fields)
                .finish(),
        }
    }
}

pub struct UseTree<'a, 'b> {
    pub prefix: Path<'a, 'b>,
    pub kind: UseTreeKind<'a, 'b>,
    pub span: Span<'a, 'b>,
}

impl Debug for UseTree<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("UseTree")
            .field("prefix", &self.prefix)
            .field("kind", &self.kind)
            .finish()
    }
}

#[derive(Debug)]
pub enum UseTreeKind<'a, 'b> {
    Simple,
    Rename(Name<'a, 'b>),
    Nested(Vec<UseTree<'a, 'b>>),
}

pub struct StructField<'a, 'b> {
    pub is_pub: bool,
    pub name: Name<'a, 'b>,
    pub type_: (),
    pub span: Span<'a, 'b>,
}

impl Debug for StructField<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("StructField")
            .field("is_pub", &self.is_pub)
            .field("name", &self.name)
            .field("type", &self.type_)
            .finish()
    }
}

pub struct GenericParams<'a, 'b> {
    pub params: Vec<Name<'a, 'b>>,
    pub span: Span<'a, 'b>,
}

impl Debug for GenericParams<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("GenericParams").field(&self.params).finish()
    }
}
