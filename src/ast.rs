use std::fmt::{self, Debug, Formatter};

use crate::span::Span;

#[derive(Clone, Copy)]
pub struct Name<'a, 'b> {
    pub name: &'a str,
    pub span: Span<'a, 'b>,
}

impl Debug for Name<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Name({:?})", self.name)
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

pub struct GenericArgs<'a, 'b> {
    pub args: Vec<Ty<'a, 'b>>,
    pub span: Span<'a, 'b>,
}

impl Debug for GenericArgs<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("GenericArgs").field(&self.args).finish()
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
    Enum {
        is_pub: bool,
        name: Name<'a, 'b>,
        generic_params: Option<GenericParams<'a, 'b>>,
        items: Vec<EnumItem<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Type {
        is_pub: bool,
        name: Name<'a, 'b>,
        generic_params: Option<GenericParams<'a, 'b>>,
        ty: Ty<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Mod {
        is_pub: bool,
        name: Name<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Extern {
        items: Vec<ExternItem<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Trait {
        is_pub: bool,
        name: Name<'a, 'b>,
        generic_params: Option<GenericParams<'a, 'b>>,
        self_bounds: Vec<TraitBound<'a, 'b>>,
        where_clause: Option<WhereClause<'a, 'b>>,
        items: Vec<TraitItem<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Fn {
        is_pub: bool,
        signature: Signature<'a, 'b>,
        block: Block<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Impl {
        name: Name<'a, 'b>,
        generic_params: Option<GenericParams<'a, 'b>>,
        as_trait: Option<TraitBound<'a, 'b>>,
        where_clause: Option<WhereClause<'a, 'b>>,
        fns: Vec<ImplFn<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Const {
        is_pub: bool,
        name: Name<'a, 'b>,
        ty: Ty<'a, 'b>,
        expr: Expr<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Static {
        is_pub: bool,
        name: Name<'a, 'b>,
        ty: Ty<'a, 'b>,
        expr: Option<Expr<'a, 'b>>,
        span: Span<'a, 'b>,
    },
}

impl<'a, 'b> Item<'a, 'b> {
    pub fn span(&self) -> Span<'a, 'b> {
        match self {
            Item::Use { span, .. }
            | Item::Struct { span, .. }
            | Item::Enum { span, .. }
            | Item::Type { span, .. }
            | Item::Mod { span, .. }
            | Item::Extern { span, .. }
            | Item::Trait { span, .. }
            | Item::Fn { span, .. }
            | Item::Impl { span, .. }
            | Item::Const { span, .. }
            | Item::Static { span, .. } => *span,
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
                .debug_struct("Struct")
                .field("is_pub", is_pub)
                .field("name", name)
                .field("generic_params", generic_params)
                .field("fields", fields)
                .finish(),
            Item::Enum {
                is_pub,
                name,
                generic_params,
                items,
                ..
            } => f
                .debug_struct("Enum")
                .field("is_pub", is_pub)
                .field("name", name)
                .field("generic_params", generic_params)
                .field("items", items)
                .finish(),
            Item::Type {
                is_pub,
                name,
                generic_params,
                ty,
                ..
            } => f
                .debug_struct("Type")
                .field("is_pub", is_pub)
                .field("name", name)
                .field("generic_params", generic_params)
                .field("ty", ty)
                .finish(),
            Item::Mod { is_pub, name, .. } => f
                .debug_struct("Mod")
                .field("is_pub", is_pub)
                .field("name", name)
                .finish(),
            Item::Extern { items, .. } => f.debug_tuple("Extern").field(items).finish(),
            Item::Trait {
                name,
                generic_params,
                self_bounds,
                where_clause,
                items,
                ..
            } => f
                .debug_struct("Trait")
                .field("name", name)
                .field("generic_params", generic_params)
                .field("self_bounds", self_bounds)
                .field("where_clause", where_clause)
                .field("items", items)
                .finish(),
            Item::Fn {
                is_pub,
                signature,
                block,
                ..
            } => f
                .debug_struct("Fn")
                .field("is_pub", is_pub)
                .field("signature", signature)
                .field("block", block)
                .finish(),
            Item::Impl {
                name,
                generic_params,
                as_trait,
                where_clause,
                fns,
                ..
            } => f
                .debug_struct("Impl")
                .field("name", name)
                .field("generic_params", generic_params)
                .field("as_trait", as_trait)
                .field("where_clause", where_clause)
                .field("fns", fns)
                .finish(),
            Item::Const {
                is_pub,
                name,
                ty,
                expr,
                ..
            } => f
                .debug_struct("Const")
                .field("is_pub", is_pub)
                .field("name", name)
                .field("ty", ty)
                .field("expr", expr)
                .finish(),
            Item::Static {
                is_pub,
                name,
                ty,
                expr,
                ..
            } => f
                .debug_struct("Static")
                .field("is_pub", is_pub)
                .field("name", name)
                .field("ty", ty)
                .field("expr", expr)
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

pub struct EnumItem<'a, 'b> {
    pub name: Name<'a, 'b>,
    pub tuple: Option<Vec<Ty<'a, 'b>>>,
    pub span: Span<'a, 'b>,
}

impl Debug for EnumItem<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("EnumItem")
            .field("name", &self.name)
            .field("tuple", &self.tuple)
            .finish()
    }
}

pub enum ExternItem<'a, 'b> {
    Fn {
        is_pub: bool,
        signature: Signature<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Type {
        is_pub: bool,
        name: Name<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Static {
        is_pub: bool,
        name: Name<'a, 'b>,
        ty: Ty<'a, 'b>,
        span: Span<'a, 'b>,
    },
}

impl<'a, 'b> ExternItem<'a, 'b> {
    pub fn span(&self) -> Span<'a, 'b> {
        match self {
            ExternItem::Fn { span, .. }
            | ExternItem::Type { span, .. }
            | ExternItem::Static { span, .. } => *span,
        }
    }
}

impl Debug for ExternItem<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ExternItem::Fn {
                is_pub, signature, ..
            } => f
                .debug_struct("Fn")
                .field("is_pub", is_pub)
                .field("signature", signature)
                .finish(),
            ExternItem::Type { is_pub, name, .. } => f
                .debug_struct("Type")
                .field("is_pub", is_pub)
                .field("name", name)
                .finish(),
            ExternItem::Static {
                is_pub, name, ty, ..
            } => f
                .debug_struct("Static")
                .field("is_pub", is_pub)
                .field("name", name)
                .field("ty", ty)
                .finish(),
        }
    }
}

pub struct Signature<'a, 'b> {
    pub name: Name<'a, 'b>,
    pub generic_params: Option<GenericParams<'a, 'b>>,
    pub self_kind: SelfKind<'a, 'b>,
    pub params: Vec<Param<'a, 'b>>,
    pub ret: Option<Ty<'a, 'b>>,
    pub where_clause: Option<WhereClause<'a, 'b>>,
    pub span: Span<'a, 'b>,
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

impl Debug for Signature<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Signature")
            .field("name", &self.name)
            .field("generic_params", &self.generic_params)
            .field("self_kind", &self.self_kind)
            .field("params", &self.params)
            .field("ret", &self.ret)
            .field("where_clause", &self.where_clause)
            .finish()
    }
}

pub enum SelfKind<'a, 'b> {
    Ptr(Span<'a, 'b>),
    Value(Span<'a, 'b>),
    None,
}

impl<'a, 'b> SelfKind<'a, 'b> {
    pub fn span(&self) -> Option<Span<'a, 'b>> {
        match self {
            SelfKind::Ptr(span) | SelfKind::Value(span) => Some(*span),
            SelfKind::None => None,
        }
    }

    pub fn is_none(&self) -> bool {
        matches!(self, SelfKind::None)
    }
}

impl Debug for SelfKind<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SelfKind::Ptr(..) => f.debug_tuple("Ptr").finish(),
            SelfKind::Value(..) => f.debug_tuple("Value").finish(),
            SelfKind::None => f.debug_tuple("None").finish(),
        }
    }
}

pub struct Param<'a, 'b> {
    pub pattern: Pattern<'a, 'b>,
    pub ty: Ty<'a, 'b>,
    pub span: Span<'a, 'b>,
}

impl Debug for Param<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Param")
            .field("pattern", &self.pattern)
            .field("ty", &self.ty)
            .finish()
    }
}

pub struct WhereClause<'a, 'b> {
    pub items: Vec<WhereItem<'a, 'b>>,
    pub span: Span<'a, 'b>,
}

impl Debug for WhereClause<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("WhereClause").field(&self.items).finish()
    }
}

pub enum WhereItem<'a, 'b> {
    Bound {
        param: Name<'a, 'b>,
        bounds: Vec<TraitBound<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Eq {
        param: Name<'a, 'b>,
        ty: Ty<'a, 'b>,
        span: Span<'a, 'b>,
    },
}

impl<'a, 'b> WhereItem<'a, 'b> {
    pub fn span(&self) -> Span<'a, 'b> {
        match self {
            WhereItem::Bound { span, .. } | WhereItem::Eq { span, .. } => *span,
        }
    }
}

impl Debug for WhereItem<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            WhereItem::Bound { param, bounds, .. } => f
                .debug_struct("Bound")
                .field("param", param)
                .field("bounds", bounds)
                .finish(),
            WhereItem::Eq { param, ty, .. } => f
                .debug_struct("Eq")
                .field("param", param)
                .field("ty", ty)
                .finish(),
        }
    }
}

pub enum TraitBound<'a, 'b> {
    Trait {
        path: Path<'a, 'b>,
        generic_args: Option<GenericArgs<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Fn {
        path: Path<'a, 'b>,
        params: Vec<Ty<'a, 'b>>,
        ret: Option<Ty<'a, 'b>>,
        span: Span<'a, 'b>,
    },
}

impl<'a, 'b> TraitBound<'a, 'b> {
    pub fn span(&self) -> Span<'a, 'b> {
        match self {
            TraitBound::Trait { span, .. } | TraitBound::Fn { span, .. } => *span,
        }
    }
}

impl Debug for TraitBound<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TraitBound::Trait {
                path, generic_args, ..
            } => f
                .debug_struct("Trait")
                .field("path", path)
                .field("generic_args", generic_args)
                .finish(),
            TraitBound::Fn {
                path, params, ret, ..
            } => f
                .debug_struct("Fn")
                .field("path", path)
                .field("params", params)
                .field("ret", ret)
                .finish(),
        }
    }
}

pub enum TraitItem<'a, 'b> {
    Required {
        signature: Signature<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Provided {
        signature: Signature<'a, 'b>,
        block: Block<'a, 'b>,
        span: Span<'a, 'b>,
    },
}

impl<'a, 'b> TraitItem<'a, 'b> {
    pub fn span(&self) -> Span<'a, 'b> {
        match self {
            TraitItem::Required { span, .. } | TraitItem::Provided { span, .. } => *span,
        }
    }
}

impl Debug for TraitItem<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TraitItem::Required { signature, .. } => {
                f.debug_tuple("Required").field(signature).finish()
            }
            TraitItem::Provided {
                signature, block, ..
            } => f
                .debug_struct("Provided")
                .field("signature", signature)
                .field("block", block)
                .finish(),
        }
    }
}

pub struct Block<'a, 'b> {
    pub stmts: Vec<Stmt<'a, 'b>>,
    pub expr: Option<Expr<'a, 'b>>,
    pub span: Span<'a, 'b>,
}

impl Debug for Block<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Block")
            .field("stmts", &self.stmts)
            .field("expr", &self.expr)
            .finish()
    }
}

pub enum Stmt<'a, 'b> {
    Local {
        pattern: Pattern<'a, 'b>,
        ty: Option<Ty<'a, 'b>>,
        expr: Option<Expr<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Item {
        item: Item<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Expr {
        expr: Expr<'a, 'b>,
        span: Span<'a, 'b>,
    },
}

impl<'a, 'b> Stmt<'a, 'b> {
    pub fn span(&self) -> Span<'a, 'b> {
        match self {
            Stmt::Local { span, .. } | Stmt::Item { span, .. } | Stmt::Expr { span, .. } => *span,
        }
    }
}

impl Debug for Stmt<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Local {
                pattern, ty, expr, ..
            } => f
                .debug_struct("Local")
                .field("pattern", pattern)
                .field("ty", ty)
                .field("expr", expr)
                .finish(),
            Stmt::Item { item, .. } => f.debug_tuple("Item").field(item).finish(),
            Stmt::Expr { expr, .. } => f.debug_tuple("Expr").field(expr).finish(),
        }
    }
}

#[derive(Debug)]
pub enum Expr<'a, 'b> {
    __marker(std::marker::PhantomData<&'a &'b ()>),
}

impl<'a, 'b> Expr<'a, 'b> {
    pub fn span(&self) -> Span<'a, 'b> {
        todo!()
    }
}

pub enum Pattern<'a, 'b> {
    Wild {
        span: Span<'a, 'b>,
    },
    Path {
        path: Path<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Struct {
        path: Path<'a, 'b>,
        fields: Vec<FieldPattern<'a, 'b>>,
        dots: Option<Span<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Enum {
        path: Path<'a, 'b>,
        tuple: Vec<Pattern<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Tuple {
        tuple: Vec<Pattern<'a, 'b>>,
        span: Span<'a, 'b>,
    },
}

impl<'a, 'b> Pattern<'a, 'b> {
    pub fn span(&self) -> Span<'a, 'b> {
        match self {
            Pattern::Wild { span, .. }
            | Pattern::Path { span, .. }
            | Pattern::Struct { span, .. }
            | Pattern::Enum { span, .. }
            | Pattern::Tuple { span, .. } => *span,
        }
    }
}

impl Debug for Pattern<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Wild { .. } => f.debug_tuple("Wild").finish(),
            Pattern::Path { path, .. } => f.debug_tuple("Path").field(path).finish(),
            Pattern::Struct {
                path, fields, dots, ..
            } => f
                .debug_struct("Struct")
                .field("path", path)
                .field("fields", fields)
                .field("dots", dots)
                .finish(),
            Pattern::Enum { path, tuple, .. } => f
                .debug_struct("Enum")
                .field("path", path)
                .field("tuple", tuple)
                .finish(),
            Pattern::Tuple { tuple, .. } => f.debug_tuple("Tuple").field(tuple).finish(),
        }
    }
}

pub struct FieldPattern<'a, 'b> {
    pub name: Name<'a, 'b>,
    pub pattern: Option<Pattern<'a, 'b>>,
    pub span: Span<'a, 'b>,
}

impl Debug for FieldPattern<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("FieldPattern")
            .field("name", &self.name)
            .field("pattern", &self.pattern)
            .finish()
    }
}

pub struct ImplFn<'a, 'b> {
    pub is_pub: bool,
    pub signature: Signature<'a, 'b>,
    pub block: Block<'a, 'b>,
    pub span: Span<'a, 'b>,
}

impl Debug for ImplFn<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("ImplFn")
            .field("is_pub", &self.is_pub)
            .field("signature", &self.signature)
            .field("block", &self.block)
            .finish()
    }
}
