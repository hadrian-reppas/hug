use std::fmt::{self, Debug};

use crate::span::Span;

pub struct Name {
    pub name: String,
    pub span: Span,
}

impl Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Name({:?})", self.name)
    }
}

#[derive(Debug)]
pub struct Path {
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
pub enum UnloadedItem {
    Use {
        self_span: Option<Span>,
        tree: UseTree,
        span: Span,
    },
    Struct {
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        fields: Vec<StructField>,
        span: Span,
    },
    Enum {
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        items: Vec<EnumItem>,
        span: Span,
    },
    Type {
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        ty: Ty,
        span: Span,
    },
    Mod {
        is_pub: bool,
        name: Name,
        span: Span,
    },
    Extern {
        items: Vec<ExternItem>,
        span: Span,
    },
    Trait {
        is_pub: bool,
        is_unique: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        self_bounds: Vec<TraitBound>,
        where_clause: Option<WhereClause>,
        items: Vec<TraitItem>,
        span: Span,
    },
    Fn {
        is_pub: bool,
        signature: Signature,
        block: Block,
        span: Span,
    },
    Impl {
        name: Name,
        generic_params: Option<GenericParams>,
        as_trait: Option<TraitBound>,
        where_clause: Option<WhereClause>,
        fns: Vec<ImplFn>,
        span: Span,
    },
    Const {
        is_pub: bool,
        name: Name,
        ty: Ty,
        expr: Expr,
        span: Span,
    },
    Static {
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
            | UnloadedItem::Type { span, .. }
            | UnloadedItem::Mod { span, .. }
            | UnloadedItem::Extern { span, .. }
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
        self_span: Option<Span>,
        tree: UseTree,
        span: Span,
    },
    Struct {
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        fields: Vec<StructField>,
        span: Span,
    },
    Enum {
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        items: Vec<EnumItem>,
        span: Span,
    },
    Type {
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        ty: Ty,
        span: Span,
    },
    Mod {
        is_pub: bool,
        name: Name,
        span: Span,
        items: Vec<Item>,
    },
    Extern {
        items: Vec<ExternItem>,
        span: Span,
    },
    Trait {
        is_pub: bool,
        is_unique: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        self_bounds: Vec<TraitBound>,
        where_clause: Option<WhereClause>,
        items: Vec<TraitItem>,
        span: Span,
    },
    Fn {
        is_pub: bool,
        signature: Signature,
        block: Block,
        span: Span,
    },
    Impl {
        name: Name,
        generic_params: Option<GenericParams>,
        as_trait: Option<TraitBound>,
        where_clause: Option<WhereClause>,
        fns: Vec<ImplFn>,
        span: Span,
    },
    Const {
        is_pub: bool,
        name: Name,
        ty: Ty,
        expr: Expr,
        span: Span,
    },
    Static {
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

impl TryFrom<UnloadedItem> for Item {
    type Error = (bool, Name, Span);
    fn try_from(item: UnloadedItem) -> Result<Item, (bool, Name, Span)> {
        match item {
            UnloadedItem::Use {
                self_span,
                tree,
                span,
            } => Ok(Item::Use {
                self_span,
                tree,
                span,
            }),
            UnloadedItem::Struct {
                is_pub,
                name,
                generic_params,
                fields,
                span,
            } => Ok(Item::Struct {
                is_pub,
                name,
                generic_params,
                fields,
                span,
            }),
            UnloadedItem::Enum {
                is_pub,
                name,
                generic_params,
                items,
                span,
            } => Ok(Item::Enum {
                is_pub,
                name,
                generic_params,
                items,
                span,
            }),
            UnloadedItem::Type {
                is_pub,
                name,
                generic_params,
                ty,
                span,
            } => Ok(Item::Type {
                is_pub,
                name,
                generic_params,
                ty,
                span,
            }),
            UnloadedItem::Mod { is_pub, name, span } => Err((is_pub, name, span)),
            UnloadedItem::Extern { items, span } => Ok(Item::Extern { items, span }),
            UnloadedItem::Trait {
                is_pub,
                is_unique,
                name,
                generic_params,
                self_bounds,
                where_clause,
                items,
                span,
            } => Ok(Item::Trait {
                is_pub,
                is_unique,
                name,
                generic_params,
                self_bounds,
                where_clause,
                items,
                span,
            }),
            UnloadedItem::Fn {
                is_pub,
                signature,
                block,
                span,
            } => Ok(Item::Fn {
                is_pub,
                signature,
                block,
                span,
            }),
            UnloadedItem::Impl {
                name,
                generic_params,
                as_trait,
                where_clause,
                fns,
                span,
            } => Ok(Item::Impl {
                name,
                generic_params,
                as_trait,
                where_clause,
                fns,
                span,
            }),
            UnloadedItem::Const {
                is_pub,
                name,
                ty,
                expr,
                span,
            } => Ok(Item::Const {
                is_pub,
                name,
                ty,
                expr,
                span,
            }),
            UnloadedItem::Static {
                is_pub,
                name,
                ty,
                expr,
                span,
            } => Ok(Item::Static {
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
    pub prefix: Path,
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
pub struct StructField {
    pub is_pub: bool,
    pub name: Name,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug)]
pub struct EnumItem {
    pub name: Name,
    pub tuple: Option<Vec<Ty>>,
    pub span: Span,
}

#[derive(Debug)]
pub enum ExternItem {
    Fn {
        is_pub: bool,
        signature: Signature,
        span: Span,
    },
    Type {
        is_pub: bool,
        name: Name,
        span: Span,
    },
    Static {
        is_pub: bool,
        name: Name,
        ty: Ty,
        span: Span,
    },
}

impl ExternItem {
    pub fn span(&self) -> Span {
        match self {
            ExternItem::Fn { span, .. }
            | ExternItem::Type { span, .. }
            | ExternItem::Static { span, .. } => *span,
        }
    }
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
    Ptr(Span),
    Value(Span),
    None,
}

impl SelfKind {
    pub fn span(&self) -> Option<Span> {
        match self {
            SelfKind::Ptr(span) | SelfKind::Value(span) => Some(*span),
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
    pub param: Name,
    pub bounds: Vec<TraitBound>,
    pub span: Span,
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
        pattern: Pattern,
        ty: Option<Ty>,
        expr: Option<Expr>,
        span: Span,
    },
    Expr {
        expr: Expr,
        span: Span,
    },
    Use {
        self_span: Option<Span>,
        tree: UseTree,
        span: Span,
    },
    Struct {
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        fields: Vec<StructField>,
        span: Span,
    },
    Enum {
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        items: Vec<EnumItem>,
        span: Span,
    },
    Type {
        is_pub: bool,
        name: Name,
        generic_params: Option<GenericParams>,
        ty: Ty,
        span: Span,
    },
    Extern {
        items: Vec<ExternItem>,
        span: Span,
    },
    Fn {
        is_pub: bool,
        signature: Signature,
        block: Block,
        span: Span,
    },
    Const {
        is_pub: bool,
        name: Name,
        ty: Ty,
        expr: Expr,
        span: Span,
    },
    Static {
        is_pub: bool,
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
            | Stmt::Type { span, .. }
            | Stmt::Extern { span, .. }
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
                self_span,
                tree,
                span,
            } => Ok(Stmt::Use {
                self_span,
                tree,
                span,
            }),
            UnloadedItem::Struct {
                is_pub,
                name,
                generic_params,
                fields,
                span,
            } => Ok(Stmt::Struct {
                is_pub,
                name,
                generic_params,
                fields,
                span,
            }),
            UnloadedItem::Enum {
                is_pub,
                name,
                generic_params,
                items,
                span,
            } => Ok(Stmt::Enum {
                is_pub,
                name,
                generic_params,
                items,
                span,
            }),
            UnloadedItem::Type {
                is_pub,
                name,
                generic_params,
                ty,
                span,
            } => Ok(Stmt::Type {
                is_pub,
                name,
                generic_params,
                ty,
                span,
            }),
            UnloadedItem::Extern { items, span } => Ok(Stmt::Extern { items, span }),
            UnloadedItem::Fn {
                is_pub,
                signature,
                block,
                span,
            } => Ok(Stmt::Fn {
                is_pub,
                signature,
                block,
                span,
            }),
            UnloadedItem::Const {
                is_pub,
                name,
                ty,
                expr,
                span,
            } => Ok(Stmt::Const {
                is_pub,
                name,
                ty,
                expr,
                span,
            }),
            UnloadedItem::Static {
                is_pub,
                name,
                ty,
                expr,
                span,
            } => Ok(Stmt::Static {
                is_pub,
                name,
                ty,
                expr,
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
    Field {
        expr: Box<Expr>,
        name: Name,
        span: Span,
    },
    TupleField {
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
        fields: Vec<ExprField>,
        span: Span,
    },
    Repeat {
        expr: Box<Expr>,
        count: Span,
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
            | Expr::Field { span, .. }
            | Expr::TupleField { span, .. }
            | Expr::Index { span, .. }
            | Expr::Range { span, .. }
            | Expr::Path { span, .. }
            | Expr::Break { span, .. }
            | Expr::Continue { span, .. }
            | Expr::Return { span, .. }
            | Expr::Struct { span, .. }
            | Expr::Repeat { span, .. }
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

    pub fn is_cmp(&self) -> bool {
        matches!(
            self,
            Expr::Binary {
                op: BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge,
                ..
            }
        )
    }
}

#[derive(Debug)]
pub struct GenericPath {
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
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug)]
pub enum UnOp {
    Deref,
    Not,
    LogicalNot,
    Neg,
    AddrOf,
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
pub enum ExprField {
    Name { name: Name, span: Span },
    Expr { name: Name, expr: Expr, span: Span },
}

impl ExprField {
    pub fn span(&self) -> Span {
        match self {
            ExprField::Name { span, .. } | ExprField::Expr { span, .. } => *span,
        }
    }
}

#[derive(Debug)]
pub enum Pattern {
    Wild {
        span: Span,
    },
    Path {
        path: Path,
        span: Span,
    },
    Struct {
        path: Path,
        fields: Vec<FieldPattern>,
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
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Wild { span, .. }
            | Pattern::Path { span, .. }
            | Pattern::Struct { span, .. }
            | Pattern::Enum { span, .. }
            | Pattern::Tuple { span, .. }
            | Pattern::Array { span, .. } => *span,
        }
    }

    pub fn into_name(self) -> Result<Name, Span> {
        let span = self.span();
        if let Pattern::Path { mut path, .. } = self {
            if path.path.len() == 1 {
                Ok(path.path.pop().unwrap())
            } else {
                Err(span)
            }
        } else {
            Err(span)
        }
    }
}

#[derive(Debug)]
pub struct FieldPattern {
    pub name: Name,
    pub pattern: Option<Pattern>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ImplFn {
    pub is_pub: bool,
    pub signature: Signature,
    pub block: Block,
    pub span: Span,
}
