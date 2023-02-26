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

pub enum Ty<'a, 'b> {
    Path {
        path: Path<'a, 'b>,
        generic_args: Option<GenericArgs<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Ptr {
        ty: Box<Ty<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Tuple {
        tys: Vec<Ty<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Slice {
        ty: Box<Ty<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Array {
        ty: Box<Ty<'a, 'b>>,
        count: Span<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Fn {
        params: Vec<Ty<'a, 'b>>,
        ret: Option<Box<Ty<'a, 'b>>>,
        span: Span<'a, 'b>,
    },
    SelfType {
        span: Span<'a, 'b>,
    },
    Never {
        span: Span<'a, 'b>,
    },
}

impl<'a, 'b> Ty<'a, 'b> {
    pub fn span(&self) -> Span<'a, 'b> {
        match self {
            Ty::Path { span, .. }
            | Ty::Ptr { span, .. }
            | Ty::Tuple { span, .. }
            | Ty::Slice { span, .. }
            | Ty::Array { span, .. }
            | Ty::Fn { span, .. }
            | Ty::SelfType { span }
            | Ty::Never { span } => *span,
        }
    }
}

impl Debug for Ty<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Path {
                path, generic_args, ..
            } => f
                .debug_struct("Path")
                .field("path", path)
                .field("generic_args", generic_args)
                .finish(),
            Ty::Ptr { ty, .. } => f.debug_tuple("Ptr").field(ty).finish(),
            Ty::Tuple { tys, .. } => f.debug_tuple("Tuple").field(tys).finish(),
            Ty::Slice { ty, .. } => f.debug_tuple("Slice").field(ty).finish(),
            Ty::Array { ty, count, .. } => f
                .debug_struct("Array")
                .field("ty", ty)
                .field("count", count)
                .finish(),
            Ty::Fn { params, ret, .. } => f
                .debug_struct("Fn")
                .field("params", params)
                .field("ret", ret)
                .finish(),
            Ty::SelfType { .. } => f.debug_tuple("SelfType").finish(),
            Ty::Never { .. } => f.debug_tuple("Never").finish(),
        }
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
    pub ty: Ty<'a, 'b>,
    pub span: Span<'a, 'b>,
}

impl Debug for StructField<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("StructField")
            .field("is_pub", &self.is_pub)
            .field("name", &self.name)
            .field("type", &self.ty)
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

pub struct GenericArgs<'a, 'b> {
    pub args: Vec<Ty<'a, 'b>>,
    pub span: Span<'a, 'b>,
}

impl Debug for GenericArgs<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("GenericArgs").field(&self.args).finish()
    }
}
