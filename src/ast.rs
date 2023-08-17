use std::fmt::{self, Debug};

use crate::error::Error;
use crate::hir::{HirId, IdCell};
use crate::parse::CmpOp;
use crate::span::Span;

pub struct Name {
    pub name: String,
    pub span: Span,
    pub id: HirId,
    pub res: IdCell,
}

impl Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Name({:?}, {:?}, {:?})", self.name, self.id, self.res)
    }
}

#[derive(Debug)]
pub struct Path {
    pub has_crate_prefix: bool,
    pub path: Vec<Name>,
    pub span: Span,
}

impl Path {
    pub fn into_name(mut self) -> Result<Name, Self> {
        if !self.has_crate_prefix && self.path.len() == 1 {
            Ok(self.path.pop().unwrap())
        } else {
            Err(self)
        }
    }
}

#[derive(Debug)]
pub struct PurePath {
    pub path: Vec<Name>,
    pub span: Span,
}

#[derive(Debug)]
pub enum Ty {
    Path {
        path: Path,
        generic_args: Option<GenericArgs>,
        span: Span,
    },
    Ptr {
        is_mut: bool,
        ty: Box<Ty>,
        span: Span,
    },
    Tuple {
        tys: Vec<Ty>,
        span: Span,
    },
    Slice {
        ty: Box<Ty>,
        span: Span,
    },
    Array {
        ty: Box<Ty>,
        count: Span,
        span: Span,
    },
    Fn {
        params: Vec<Ty>,
        ret: Option<Box<Ty>>,
        span: Span,
    },
    SelfType {
        span: Span,
    },
    Never {
        span: Span,
    },
}

impl Ty {
    pub fn span(&self) -> Span {
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

#[derive(Debug)]
pub struct GenericArgs {
    pub args: Vec<Ty>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Annotation {
    pub item: AnnotationItem,
    pub span: Span,
}

#[derive(Debug)]
pub enum AnnotationItem {
    String {
        span: Span,
    },
    Name {
        name: Name,
        args: Option<Vec<AnnotationItem>>,
        span: Span,
    },
}

impl AnnotationItem {
    fn span(&self) -> Span {
        match self {
            AnnotationItem::String { span } | AnnotationItem::Name { span, .. } => *span,
        }
    }
}

#[derive(Debug)]
pub enum UnloadedItem {
    Use {
        annotations: Vec<Annotation>,
        is_pub: bool,
        has_crate_prefix: bool,
        tree: UseTree,
        span: Span,
    },
    Struct {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        members: Vec<Member>,
        span: Span,
    },
    Enum {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        items: Vec<EnumItem>,
        span: Span,
    },
    Union {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        members: Vec<Member>,
        span: Span,
    },
    Type {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        ty: Ty,
        span: Span,
    },
    Mod {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        span: Span,
    },
    ExternFn {
        annotations: Vec<Annotation>,
        is_pub: bool,
        signature: Signature,
        span: Span,
    },
    ExternType {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        extern_name: Name,
        size: Span,
        align: Span,
        span: Span,
    },
    ExternStatic {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        ty: Ty,
        span: Span,
    },
    ExternStruct {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        extern_name: Name,
        members: Vec<Member>,
        span: Span,
    },
    ExternUnion {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        extern_name: Name,
        members: Vec<Member>,
        span: Span,
    },
    Trait {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        self_bounds: Vec<TraitBound>,
        where_clause: Option<TraitWhereClause>,
        items: Vec<TraitItem>,
        span: Span,
    },
    Fn {
        annotations: Vec<Annotation>,
        is_pub: bool,
        signature: Signature,
        block: Block,
        span: Span,
    },
    Impl {
        annotations: Vec<Annotation>,
        generic_params: Option<GenericParams>,
        ty: Ty,
        as_trait: Option<TraitBound>,
        where_clause: Option<WhereClause>,
        fns: Vec<ImplFn>,
        span: Span,
    },
    Const {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        ty: Ty,
        expr: Expr,
        span: Span,
    },
    Static {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        ty: Ty,
        expr: Option<Expr>,
        span: Span,
    },
}

impl UnloadedItem {
    pub fn span(&self) -> Span {
        match self {
            UnloadedItem::Use { span, .. }
            | UnloadedItem::Struct { span, .. }
            | UnloadedItem::Enum { span, .. }
            | UnloadedItem::Union { span, .. }
            | UnloadedItem::Type { span, .. }
            | UnloadedItem::Mod { span, .. }
            | UnloadedItem::ExternFn { span, .. }
            | UnloadedItem::ExternType { span, .. }
            | UnloadedItem::ExternStatic { span, .. }
            | UnloadedItem::ExternStruct { span, .. }
            | UnloadedItem::ExternUnion { span, .. }
            | UnloadedItem::Trait { span, .. }
            | UnloadedItem::Fn { span, .. }
            | UnloadedItem::Impl { span, .. }
            | UnloadedItem::Const { span, .. }
            | UnloadedItem::Static { span, .. } => *span,
        }
    }
}

#[derive(Debug)]
pub enum Item {
    Use {
        annotations: Vec<Annotation>,
        is_pub: bool,
        has_crate_prefix: bool,
        tree: UseTree,
        span: Span,
    },
    Struct {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        members: Vec<Member>,
        span: Span,
    },
    Enum {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        items: Vec<EnumItem>,
        span: Span,
    },
    Union {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        members: Vec<Member>,
        span: Span,
    },
    Type {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        ty: Ty,
        span: Span,
    },
    Mod {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        span: Span,
        items: Vec<Item>,
    },
    ExternFn {
        annotations: Vec<Annotation>,
        is_pub: bool,
        signature: Signature,
        span: Span,
    },
    ExternType {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        extern_name: Name,
        size: Span,
        align: Span,
        span: Span,
    },
    ExternStatic {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        ty: Ty,
        span: Span,
    },
    ExternStruct {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        extern_name: Name,
        members: Vec<Member>,
        span: Span,
    },
    ExternUnion {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        extern_name: Name,
        members: Vec<Member>,
        span: Span,
    },
    Trait {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        self_bounds: Vec<TraitBound>,
        where_clause: Option<TraitWhereClause>,
        items: Vec<TraitItem>,
        span: Span,
    },
    Fn {
        annotations: Vec<Annotation>,
        is_pub: bool,
        signature: Signature,
        block: Block,
        span: Span,
    },
    Impl {
        annotations: Vec<Annotation>,
        generic_params: Option<GenericParams>,
        ty: Ty,
        as_trait: Option<TraitBound>,
        where_clause: Option<WhereClause>,
        fns: Vec<ImplFn>,
        span: Span,
    },
    Const {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        ty: Ty,
        expr: Expr,
        span: Span,
    },
    Static {
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        ty: Ty,
        expr: Option<Expr>,
        span: Span,
    },
}

impl Item {
    pub fn span(&self) -> Span {
        match self {
            Item::Use { span, .. }
            | Item::Struct { span, .. }
            | Item::Enum { span, .. }
            | Item::Union { span, .. }
            | Item::Type { span, .. }
            | Item::Mod { span, .. }
            | Item::ExternFn { span, .. }
            | Item::ExternType { span, .. }
            | Item::ExternStatic { span, .. }
            | Item::ExternStruct { span, .. }
            | Item::ExternUnion { span, .. }
            | Item::Trait { span, .. }
            | Item::Fn { span, .. }
            | Item::Impl { span, .. }
            | Item::Const { span, .. }
            | Item::Static { span, .. } => *span,
        }
    }
}

impl TryFrom<UnloadedItem> for Item {
    type Error = (Vec<Annotation>, bool, Name, Span);
    fn try_from(item: UnloadedItem) -> Result<Item, Self::Error> {
        match item {
            UnloadedItem::Use {
                annotations,
                is_pub,
                has_crate_prefix,
                tree,
                span,
            } => Ok(Item::Use {
                annotations,
                is_pub,
                has_crate_prefix,
                tree,
                span,
            }),
            UnloadedItem::Struct {
                annotations,
                is_pub,
                name,
                generic_params,
                members,
                span,
            } => Ok(Item::Struct {
                annotations,
                is_pub,
                name,
                generic_params,
                members,
                span,
            }),
            UnloadedItem::Enum {
                annotations,
                is_pub,
                name,
                generic_params,
                items,
                span,
            } => Ok(Item::Enum {
                annotations,
                is_pub,
                name,
                generic_params,
                items,
                span,
            }),
            UnloadedItem::Union {
                annotations,
                is_pub,
                name,
                generic_params,
                members,
                span,
            } => Ok(Item::Union {
                annotations,
                is_pub,
                name,
                generic_params,
                members,
                span,
            }),
            UnloadedItem::Type {
                annotations,
                is_pub,
                name,
                generic_params,
                ty,
                span,
            } => Ok(Item::Type {
                annotations,
                is_pub,
                name,
                generic_params,
                ty,
                span,
            }),
            UnloadedItem::Mod {
                annotations,
                is_pub,
                name,
                span,
            } => Err((annotations, is_pub, name, span)),
            UnloadedItem::ExternFn {
                annotations,
                is_pub,
                signature,
                span,
            } => Ok(Item::ExternFn {
                annotations,
                is_pub,
                signature,
                span,
            }),
            UnloadedItem::ExternType {
                annotations,
                is_pub,
                name,
                extern_name,
                size,
                align,
                span,
            } => Ok(Item::ExternType {
                annotations,
                is_pub,
                name,
                extern_name,
                size,
                align,
                span,
            }),
            UnloadedItem::ExternStatic {
                annotations,
                is_pub,
                name,
                ty,
                span,
            } => Ok(Item::ExternStatic {
                annotations,
                is_pub,
                name,
                ty,
                span,
            }),
            UnloadedItem::ExternStruct {
                annotations,
                is_pub,
                name,
                extern_name,
                members,
                span,
            } => Ok(Item::ExternStruct {
                annotations,
                is_pub,
                name,
                extern_name,
                members,
                span,
            }),
            UnloadedItem::ExternUnion {
                annotations,
                is_pub,
                name,
                extern_name,
                members,
                span,
            } => Ok(Item::ExternUnion {
                annotations,
                is_pub,
                name,
                extern_name,
                members,
                span,
            }),
            UnloadedItem::Trait {
                annotations,
                is_pub,
                name,
                generic_params,
                self_bounds,
                where_clause,
                items,
                span,
            } => Ok(Item::Trait {
                annotations,
                is_pub,
                name,
                generic_params,
                self_bounds,
                where_clause,
                items,
                span,
            }),
            UnloadedItem::Fn {
                annotations,
                is_pub,
                signature,
                block,
                span,
            } => Ok(Item::Fn {
                annotations,
                is_pub,
                signature,
                block,
                span,
            }),
            UnloadedItem::Impl {
                annotations,
                generic_params,
                ty,
                as_trait,
                where_clause,
                fns,
                span,
            } => Ok(Item::Impl {
                annotations,
                generic_params,
                ty,
                as_trait,
                where_clause,
                fns,
                span,
            }),
            UnloadedItem::Const {
                annotations,
                is_pub,
                name,
                ty,
                expr,
                span,
            } => Ok(Item::Const {
                annotations,
                is_pub,
                name,
                ty,
                expr,
                span,
            }),
            UnloadedItem::Static {
                annotations,
                is_pub,
                name,
                ty,
                expr,
                span,
            } => Ok(Item::Static {
                annotations,
                is_pub,
                name,
                ty,
                expr,
                span,
            }),
        }
    }
}

#[derive(Debug)]
pub struct UseTree {
    pub prefix: PurePath,
    pub kind: UseTreeKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum UseTreeKind {
    Simple,
    Rename(Name),
    Nested(Vec<UseTree>),
}

#[derive(Debug)]
pub struct Member {
    pub annotations: Vec<Annotation>,
    pub is_pub: bool,
    pub name: Name,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug)]
pub struct EnumItem {
    pub annotations: Vec<Annotation>,
    pub name: Name,
    pub tuple: Option<Vec<Ty>>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Signature {
    pub name: Name,
    pub generic_params: Option<GenericParams>,
    pub self_kind: SelfKind,
    pub params: Vec<Param>,
    pub variadic: Option<Variadic>,
    pub ret: Option<Ty>,
    pub where_clause: Option<WhereClause>,
    pub span: Span,
}

#[derive(Debug)]
pub struct GenericParams {
    pub params: Vec<Name>,
    pub span: Span,
}

#[derive(Debug)]
pub enum SelfKind {
    Ptr { is_mut: bool, span: Span },
    Value { is_mut: bool, span: Span },
    None,
}

impl SelfKind {
    pub fn span(&self) -> Option<Span> {
        match self {
            SelfKind::Ptr { span, .. } | SelfKind::Value { span, .. } => Some(*span),
            SelfKind::None => None,
        }
    }

    pub fn is_none(&self) -> bool {
        matches!(self, SelfKind::None)
    }
}

#[derive(Debug)]
pub struct Param {
    pub pattern: Pattern,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug)]
pub struct Variadic {
    pub name: Name,
    pub span: Span,
}

#[derive(Debug)]
pub struct WhereClause {
    pub items: Vec<WhereItem>,
    pub span: Span,
}

#[derive(Debug)]
pub struct WhereItem {
    pub ty: Ty,
    pub bounds: Vec<TraitBound>,
    pub span: Span,
}

#[derive(Debug)]
pub struct TraitWhereClause {
    pub items: Vec<TraitWhereItem>,
    pub span: Span,
}

#[derive(Debug)]
pub enum TraitWhereItem {
    Bound {
        ty: Ty,
        bounds: Vec<TraitBound>,
        span: Span,
    },
    Dependency {
        left: Vec<Name>,
        right: Vec<Name>,
        span: Span,
    },
}

impl TraitWhereItem {
    pub fn span(&self) -> Span {
        match self {
            TraitWhereItem::Bound { span, .. } | TraitWhereItem::Dependency { span, .. } => *span,
        }
    }
}

#[derive(Debug)]
pub enum TraitBound {
    Trait {
        path: Path,
        generic_args: Option<GenericArgs>,
        span: Span,
    },
    Fn {
        path: Path,
        params: Vec<Ty>,
        ret: Option<Ty>,
        span: Span,
    },
}

impl TraitBound {
    pub fn span(&self) -> Span {
        match self {
            TraitBound::Trait { span, .. } | TraitBound::Fn { span, .. } => *span,
        }
    }
}

#[derive(Debug)]
pub enum TraitItem {
    Required {
        signature: Signature,
        span: Span,
    },
    Provided {
        signature: Signature,
        block: Block,
        span: Span,
    },
}

impl TraitItem {
    pub fn span(&self) -> Span {
        match self {
            TraitItem::Required { span, .. } | TraitItem::Provided { span, .. } => *span,
        }
    }
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug)]
pub enum Stmt {
    Local {
        annotations: Vec<Annotation>,
        pattern: Pattern,
        ty: Option<Ty>,
        expr: Option<Expr>,
        span: Span,
    },
    Expr {
        annotations: Vec<Annotation>,
        expr: Expr,
        span: Span,
    },
    Use {
        annotations: Vec<Annotation>,
        has_crate_prefix: bool,
        tree: UseTree,
        span: Span,
    },
    Struct {
        annotations: Vec<Annotation>,
        name: Name,
        generic_params: Option<GenericParams>,
        members: Vec<Member>,
        span: Span,
    },
    Enum {
        annotations: Vec<Annotation>,
        name: Name,
        generic_params: Option<GenericParams>,
        items: Vec<EnumItem>,
        span: Span,
    },
    Union {
        annotations: Vec<Annotation>,
        name: Name,
        generic_params: Option<GenericParams>,
        members: Vec<Member>,
        span: Span,
    },
    Type {
        annotations: Vec<Annotation>,
        name: Name,
        generic_params: Option<GenericParams>,
        ty: Ty,
        span: Span,
    },
    ExternFn {
        annotations: Vec<Annotation>,
        signature: Signature,
        span: Span,
    },
    ExternType {
        annotations: Vec<Annotation>,
        name: Name,
        extern_name: Name,
        size: Span,
        align: Span,
        span: Span,
    },
    ExternStatic {
        annotations: Vec<Annotation>,
        name: Name,
        ty: Ty,
        span: Span,
    },
    ExternStruct {
        annotations: Vec<Annotation>,
        name: Name,
        extern_name: Name,
        members: Vec<Member>,
        span: Span,
    },
    ExternUnion {
        annotations: Vec<Annotation>,
        name: Name,
        extern_name: Name,
        members: Vec<Member>,
        span: Span,
    },
    Fn {
        annotations: Vec<Annotation>,
        signature: Signature,
        block: Block,
        span: Span,
    },
    Const {
        annotations: Vec<Annotation>,
        name: Name,
        ty: Ty,
        expr: Expr,
        span: Span,
    },
    Static {
        annotations: Vec<Annotation>,
        name: Name,
        ty: Ty,
        expr: Option<Expr>,
        span: Span,
    },
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self {
            Stmt::Local { span, .. }
            | Stmt::Expr { span, .. }
            | Stmt::Use { span, .. }
            | Stmt::Struct { span, .. }
            | Stmt::Enum { span, .. }
            | Stmt::Union { span, .. }
            | Stmt::Type { span, .. }
            | Stmt::ExternFn { span, .. }
            | Stmt::ExternType { span, .. }
            | Stmt::ExternStatic { span, .. }
            | Stmt::ExternStruct { span, .. }
            | Stmt::ExternUnion { span, .. }
            | Stmt::Fn { span, .. }
            | Stmt::Const { span, .. }
            | Stmt::Static { span, .. } => *span,
        }
    }
}

impl TryFrom<UnloadedItem> for Stmt {
    type Error = ();
    fn try_from(item: UnloadedItem) -> Result<Stmt, ()> {
        match item {
            UnloadedItem::Use {
                annotations,
                has_crate_prefix,
                tree,
                span,
                ..
            } => Ok(Stmt::Use {
                annotations,
                has_crate_prefix,
                tree,
                span,
            }),
            UnloadedItem::Struct {
                annotations,
                name,
                generic_params,
                members,
                span,
                ..
            } => Ok(Stmt::Struct {
                annotations,
                name,
                generic_params,
                members,
                span,
            }),
            UnloadedItem::Enum {
                annotations,
                name,
                generic_params,
                items,
                span,
                ..
            } => Ok(Stmt::Enum {
                annotations,
                name,
                generic_params,
                items,
                span,
            }),
            UnloadedItem::Union {
                annotations,
                name,
                generic_params,
                members,
                span,
                ..
            } => Ok(Stmt::Union {
                annotations,
                name,
                generic_params,
                members,
                span,
            }),
            UnloadedItem::Type {
                annotations,
                name,
                generic_params,
                ty,
                span,
                ..
            } => Ok(Stmt::Type {
                annotations,
                name,
                generic_params,
                ty,
                span,
            }),
            UnloadedItem::Fn {
                annotations,
                signature,
                block,
                span,
                ..
            } => Ok(Stmt::Fn {
                annotations,
                signature,
                block,
                span,
            }),
            UnloadedItem::Const {
                annotations,
                name,
                ty,
                expr,
                span,
                ..
            } => Ok(Stmt::Const {
                annotations,
                name,
                ty,
                expr,
                span,
            }),
            UnloadedItem::Static {
                annotations,
                name,
                ty,
                expr,
                span,
                ..
            } => Ok(Stmt::Static {
                annotations,
                name,
                ty,
                expr,
                span,
            }),
            UnloadedItem::ExternFn {
                annotations,
                signature,
                span,
                ..
            } => Ok(Stmt::ExternFn {
                annotations,
                signature,
                span,
            }),
            UnloadedItem::ExternType {
                annotations,
                name,
                extern_name,
                size,
                align,
                span,
                ..
            } => Ok(Stmt::ExternType {
                annotations,
                name,
                extern_name,
                size,
                align,
                span,
            }),
            UnloadedItem::ExternStatic {
                annotations,
                name,
                ty,
                span,
                ..
            } => Ok(Stmt::ExternStatic {
                annotations,
                name,
                ty,
                span,
            }),
            UnloadedItem::ExternStruct {
                annotations,
                name,
                extern_name,
                members,
                span,
                ..
            } => Ok(Stmt::ExternStruct {
                annotations,
                name,
                extern_name,
                members,
                span,
            }),
            UnloadedItem::ExternUnion {
                annotations,
                name,
                extern_name,
                members,
                span,
                ..
            } => Ok(Stmt::ExternUnion {
                annotations,
                name,
                extern_name,
                members,
                span,
            }),
            UnloadedItem::Trait { .. } | UnloadedItem::Mod { .. } | UnloadedItem::Impl { .. } => {
                Err(())
            }
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Array {
        exprs: Vec<Expr>,
        span: Span,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },
    MethodCall {
        receiver: Box<Expr>,
        name: GenericSegment,
        args: Vec<Expr>,
        span: Span,
    },
    Macro {
        name: Name,
        kind: MacroKind,
        args: Vec<Expr>,
        span: Span,
    },
    Tuple {
        exprs: Vec<Expr>,
        span: Span,
    },
    Binary {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op_span: Span,
        span: Span,
    },
    Unary {
        op: UnOp,
        expr: Box<Expr>,
        op_span: Span,
        span: Span,
    },
    AddrOf {
        is_mut: bool,
        expr: Box<Expr>,
        span: Span,
    },
    Literal {
        kind: LiteralKind,
        span: Span,
    },
    SelfValue {
        span: Span,
    },
    Cast {
        expr: Box<Expr>,
        ty: Ty,
        span: Span,
    },
    If {
        test: Box<Expr>,
        block: Block,
        else_kind: ElseKind,
        span: Span,
    },
    IfLet {
        pattern: Pattern,
        expr: Box<Expr>,
        block: Block,
        else_kind: ElseKind,
        span: Span,
    },
    While {
        test: Box<Expr>,
        block: Block,
        span: Span,
    },
    WhileLet {
        pattern: Pattern,
        expr: Box<Expr>,
        block: Block,
        span: Span,
    },
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
        span: Span,
    },
    Block {
        block: Block,
        span: Span,
    },
    For {
        pattern: Pattern,
        iter: Box<Expr>,
        block: Block,
        span: Span,
    },
    Loop {
        block: Block,
        span: Span,
    },
    TryBlock {
        block: Block,
        span: Span,
    },
    Label {
        label: Label,
        span: Span,
    },
    Goto {
        label: Label,
        expr: Option<Box<Expr>>,
        span: Span,
    },
    Try {
        expr: Box<Expr>,
        qmark_span: Span,
        span: Span,
    },
    Assign {
        target: Box<Expr>,
        rhs: Box<Expr>,
        span: Span,
    },
    AssignOp {
        op: AssignOp,
        target: Box<Expr>,
        rhs: Box<Expr>,
        op_span: Span,
        span: Span,
    },
    Member {
        expr: Box<Expr>,
        name: Name,
        span: Span,
    },
    TupleMember {
        expr: Box<Expr>,
        index: Span,
        span: Span,
    },
    Index {
        expr: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },
    Range {
        low: Option<Box<Expr>>,
        high: Option<Box<Expr>>,
        range_span: Span,
        span: Span,
    },
    Path {
        path: GenericPath,
        span: Span,
    },
    QualifiedPath {
        ty: Ty,
        as_trait: Option<TraitBound>,
        segment: GenericSegment,
        span: Span,
    },
    Break {
        expr: Option<Box<Expr>>,
        span: Span,
    },
    Continue {
        span: Span,
    },
    Return {
        expr: Option<Box<Expr>>,
        span: Span,
    },
    Struct {
        path: GenericPath,
        members: Vec<ExprMember>,
        span: Span,
    },
    Repeat {
        expr: Box<Expr>,
        count: Span,
        span: Span,
    },
    Compare {
        compare: Compare,
        span: Span,
    },
    Paren {
        expr: Box<Expr>,
        span: Span,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Array { span, .. }
            | Expr::Call { span, .. }
            | Expr::MethodCall { span, .. }
            | Expr::Macro { span, .. }
            | Expr::Tuple { span, .. }
            | Expr::Binary { span, .. }
            | Expr::Unary { span, .. }
            | Expr::AddrOf { span, .. }
            | Expr::Literal { span, .. }
            | Expr::SelfValue { span, .. }
            | Expr::Cast { span, .. }
            | Expr::If { span, .. }
            | Expr::IfLet { span, .. }
            | Expr::While { span, .. }
            | Expr::WhileLet { span, .. }
            | Expr::Match { span, .. }
            | Expr::Block { span, .. }
            | Expr::For { span, .. }
            | Expr::Loop { span, .. }
            | Expr::TryBlock { span, .. }
            | Expr::Label { span, .. }
            | Expr::Goto { span, .. }
            | Expr::Try { span, .. }
            | Expr::Assign { span, .. }
            | Expr::AssignOp { span, .. }
            | Expr::Member { span, .. }
            | Expr::TupleMember { span, .. }
            | Expr::Index { span, .. }
            | Expr::Range { span, .. }
            | Expr::Path { span, .. }
            | Expr::QualifiedPath { span, .. }
            | Expr::Break { span, .. }
            | Expr::Continue { span, .. }
            | Expr::Return { span, .. }
            | Expr::Struct { span, .. }
            | Expr::Repeat { span, .. }
            | Expr::Compare { span, .. }
            | Expr::Paren { span, .. } => *span,
        }
    }

    pub fn has_block(&self) -> bool {
        matches!(
            self,
            Expr::If { .. }
                | Expr::IfLet { .. }
                | Expr::While { .. }
                | Expr::WhileLet { .. }
                | Expr::Match { .. }
                | Expr::Block { .. }
                | Expr::For { .. }
                | Expr::Loop { .. }
                | Expr::TryBlock { .. }
        )
    }

    pub fn is_range(&self) -> bool {
        matches!(self, Expr::Range { .. })
    }

    pub fn is_cast(&self) -> bool {
        matches!(self, Expr::Cast { .. })
    }
}

#[derive(Debug)]
pub enum Compare {
    Eq {
        lhs: Box<Expr>,
        others: Vec<(Span, Expr)>,
        span: Span,
    },
    Ne {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op_span: Span,
        span: Span,
    },
    Increasing {
        lhs: Box<Expr>,
        comparators: Vec<(Comparison, Expr)>,
        span: Span,
    },
    Decreasing {
        lhs: Box<Expr>,
        comparators: Vec<(Comparison, Expr)>,
        span: Span,
    },
}

impl Compare {
    pub fn append(self, rhs: Expr, op: CmpOp, op_span: Span) -> Result<Compare, Error> {
        let span = self.span().to(rhs.span());
        match (self, op) {
            (
                Compare::Eq {
                    lhs, mut others, ..
                },
                CmpOp::Eq,
            ) => {
                others.push((op_span, rhs));
                Ok(Compare::Eq { lhs, others, span })
            }
            (Compare::Eq { lhs, others, .. }, CmpOp::Lt) => {
                let mut comparators: Vec<_> = others
                    .into_iter()
                    .map(|(span, expr)| (Comparison::Equal(span), expr))
                    .collect();
                comparators.push((Comparison::Exclusive(op_span), rhs));
                Ok(Compare::Increasing {
                    lhs,
                    comparators,
                    span,
                })
            }
            (Compare::Eq { lhs, others, .. }, CmpOp::Le) => {
                let mut comparators: Vec<_> = others
                    .into_iter()
                    .map(|(span, expr)| (Comparison::Equal(span), expr))
                    .collect();
                comparators.push((Comparison::Inclusive(op_span), rhs));
                Ok(Compare::Increasing {
                    lhs,
                    comparators,
                    span,
                })
            }
            (Compare::Eq { lhs, others, .. }, CmpOp::Gt) => {
                let mut comparators: Vec<_> = others
                    .into_iter()
                    .map(|(span, expr)| (Comparison::Equal(span), expr))
                    .collect();
                comparators.push((Comparison::Exclusive(op_span), rhs));
                Ok(Compare::Decreasing {
                    lhs,
                    comparators,
                    span,
                })
            }
            (Compare::Eq { lhs, others, .. }, CmpOp::Ge) => {
                let mut comparators: Vec<_> = others
                    .into_iter()
                    .map(|(span, expr)| (Comparison::Equal(span), expr))
                    .collect();
                comparators.push((Comparison::Inclusive(op_span), rhs));
                Ok(Compare::Decreasing {
                    lhs,
                    comparators,
                    span,
                })
            }
            (Compare::Eq { .. }, CmpOp::Ne) => {
                Err(Error::new("illegal comparison chain", Some(span))
                    .note("`!=` is not allowed in an equality chain", Some(op_span)))
            }
            (Compare::Ne { op_span, .. }, _) => {
                Err(Error::new("illegal comparison chain", Some(span))
                    .note("`!=` operators cannot be chained", Some(op_span)))
            }
            (
                Compare::Increasing {
                    lhs,
                    mut comparators,
                    ..
                },
                CmpOp::Eq,
            ) => {
                comparators.push((Comparison::Equal(op_span), rhs));
                Ok(Compare::Increasing {
                    lhs,
                    comparators,
                    span,
                })
            }
            (Compare::Increasing { .. }, CmpOp::Ne) => {
                Err(Error::new("illegal comparison chain", Some(span))
                    .note("`!=` is not allowed in an increasing chain", Some(op_span)))
            }
            (
                Compare::Increasing {
                    lhs,
                    mut comparators,
                    ..
                },
                CmpOp::Lt,
            ) => {
                comparators.push((Comparison::Exclusive(op_span), rhs));
                Ok(Compare::Increasing {
                    lhs,
                    comparators,
                    span,
                })
            }
            (
                Compare::Increasing {
                    lhs,
                    mut comparators,
                    ..
                },
                CmpOp::Le,
            ) => {
                comparators.push((Comparison::Inclusive(op_span), rhs));
                Ok(Compare::Increasing {
                    lhs,
                    comparators,
                    span,
                })
            }
            (Compare::Increasing { .. }, CmpOp::Gt) => {
                Err(Error::new("illegal comparison chain", Some(span))
                    .note("`>` is not allowed in an increasing chain", Some(op_span)))
            }
            (Compare::Increasing { .. }, CmpOp::Ge) => {
                Err(Error::new("illegal comparison chain", Some(span))
                    .note("`>=` is not allowed in an increasing chain", Some(op_span)))
            }
            (
                Compare::Decreasing {
                    lhs,
                    mut comparators,
                    ..
                },
                CmpOp::Eq,
            ) => {
                comparators.push((Comparison::Equal(op_span), rhs));
                Ok(Compare::Decreasing {
                    lhs,
                    comparators,
                    span,
                })
            }
            (Compare::Decreasing { .. }, CmpOp::Ne) => {
                Err(Error::new("illegal comparison chain", Some(span))
                    .note("`!=` is not allowed in a decreasing chain", Some(op_span)))
            }
            (Compare::Decreasing { .. }, CmpOp::Lt) => {
                Err(Error::new("illegal comparison chain", Some(span))
                    .note("`<` is not allowed in a decreasing chain", Some(op_span)))
            }
            (Compare::Decreasing { .. }, CmpOp::Le) => {
                Err(Error::new("illegal comparison chain", Some(span))
                    .note("`<=` is not allowed in a decreasing chain", Some(op_span)))
            }
            (
                Compare::Decreasing {
                    lhs,
                    mut comparators,
                    ..
                },
                CmpOp::Gt,
            ) => {
                comparators.push((Comparison::Exclusive(op_span), rhs));
                Ok(Compare::Decreasing {
                    lhs,
                    comparators,
                    span,
                })
            }
            (
                Compare::Decreasing {
                    lhs,
                    mut comparators,
                    ..
                },
                CmpOp::Ge,
            ) => {
                comparators.push((Comparison::Inclusive(op_span), rhs));
                Ok(Compare::Decreasing {
                    lhs,
                    comparators,
                    span,
                })
            }
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Compare::Eq { span, .. }
            | Compare::Ne { span, .. }
            | Compare::Increasing { span, .. }
            | Compare::Decreasing { span, .. } => *span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Comparison {
    Equal(Span),
    Inclusive(Span),
    Exclusive(Span),
}

impl Comparison {
    pub fn span(self) -> Span {
        match self {
            Comparison::Equal(span) | Comparison::Inclusive(span) | Comparison::Exclusive(span) => {
                span
            }
        }
    }
}

#[derive(Debug)]
pub struct GenericPath {
    pub has_crate_prefix: bool,
    pub segments: Vec<GenericSegment>,
    pub span: Span,
}

#[derive(Debug)]
pub struct GenericSegment {
    pub name: Name,
    pub generic_args: Option<GenericArgs>,
    pub span: Span,
}

#[derive(Debug)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Copy, Debug)]
pub enum MacroKind {
    Assert,
    AssertEq,
    AssertNe,
    Column,
    Dbg,
    Eprint,
    Eprintln,
    File,
    Format,
    Line,
    Panic,
    Print,
    Println,
    Todo,
    Unreachable,
    Vec,
    Write,
    Writeln,
}

#[derive(Debug)]
pub enum LiteralKind {
    Int,
    Float,
    Char,
    String,
    Byte,
    ByteString,
    Bool,
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    In,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug)]
pub enum UnOp {
    Deref,
    Not,
    LogicalNot,
    Neg,
}

#[derive(Debug)]
pub enum AssignOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    Shl,
    Shr,
}

#[derive(Debug)]
pub struct Label {
    pub name: Name,
    pub span: Span,
}

#[derive(Debug)]
pub enum ElseKind {
    Else {
        block: Block,
        span: Span,
    },
    ElseIf {
        test: Box<Expr>,
        block: Block,
        else_kind: Box<ElseKind>,
        span: Span,
    },
    ElseIfLet {
        pattern: Pattern,
        expr: Box<Expr>,
        block: Block,
        else_kind: Box<ElseKind>,
        span: Span,
    },
    Nothing {
        span: Span,
    },
}

impl ElseKind {
    pub fn span(&self) -> Span {
        match self {
            ElseKind::Else { span, .. }
            | ElseKind::ElseIf { span, .. }
            | ElseKind::ElseIfLet { span, .. }
            | ElseKind::Nothing { span, .. } => *span,
        }
    }
}

#[derive(Debug)]
pub enum ExprMember {
    Name { name: Name, span: Span },
    Expr { name: Name, expr: Expr, span: Span },
}

impl ExprMember {
    pub fn span(&self) -> Span {
        match self {
            ExprMember::Name { span, .. } | ExprMember::Expr { span, .. } => *span,
        }
    }
}

#[derive(Debug)]
pub enum Pattern {
    Wild {
        span: Span,
    },
    Name {
        is_mut: bool,
        name: Name,
        span: Span,
    },
    Path {
        path: Path,
        span: Span,
    },
    Struct {
        path: Path,
        members: Vec<MemberPattern>,
        dots: Option<Span>,
        span: Span,
    },
    Enum {
        path: Path,
        tuple: Vec<Pattern>,
        span: Span,
    },
    Tuple {
        tuple: Vec<Pattern>,
        span: Span,
    },
    Array {
        array: Vec<Pattern>,
        span: Span,
    },
    Or {
        patterns: Vec<Pattern>,
        span: Span,
    },
    Literal {
        kind: PatternLiteralKind,
        span: Span,
    },
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Wild { span, .. }
            | Pattern::Name { span, .. }
            | Pattern::Path { span, .. }
            | Pattern::Struct { span, .. }
            | Pattern::Enum { span, .. }
            | Pattern::Tuple { span, .. }
            | Pattern::Array { span, .. }
            | Pattern::Or { span, .. }
            | Pattern::Literal { span, .. } => *span,
        }
    }
}

#[derive(Debug)]
pub enum PatternLiteralKind {
    Int,
    Char,
    String,
    Byte,
    ByteString,
    Bool,
}

#[derive(Debug)]
pub enum MemberPattern {
    Name {
        is_mut: bool,
        name: Name,
        span: Span,
    },
    Pattern {
        name: Name,
        pattern: Pattern,
        span: Span,
    },
}

impl MemberPattern {
    pub fn span(&self) -> Span {
        match self {
            MemberPattern::Name { span, .. } | MemberPattern::Pattern { span, .. } => *span,
        }
    }
}

#[derive(Debug)]
pub struct ImplFn {
    pub annotations: Vec<Annotation>,
    pub is_pub: bool,
    pub signature: Signature,
    pub block: Block,
    pub span: Span,
}
