use std::fmt::{self, Debug};

use crate::span::Span;

#[derive(Clone, Copy)]
pub struct Name<'a, 'b> {
    pub name: &'a str,
    pub span: Span<'a, 'b>,
}

impl Debug for Name<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Name({:?})", self.name)
    }
}

#[derive(Debug)]
pub struct Path<'a, 'b> {
    pub path: Vec<Name<'a, 'b>>,
    pub span: Span<'a, 'b>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct GenericArgs<'a, 'b> {
    pub args: Vec<Ty<'a, 'b>>,
    pub span: Span<'a, 'b>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct UseTree<'a, 'b> {
    pub prefix: Path<'a, 'b>,
    pub kind: UseTreeKind<'a, 'b>,
    pub span: Span<'a, 'b>,
}

#[derive(Debug)]
pub enum UseTreeKind<'a, 'b> {
    Simple,
    Rename(Name<'a, 'b>),
    Nested(Vec<UseTree<'a, 'b>>),
}

#[derive(Debug)]
pub struct StructField<'a, 'b> {
    pub is_pub: bool,
    pub name: Name<'a, 'b>,
    pub ty: Ty<'a, 'b>,
    pub span: Span<'a, 'b>,
}

#[derive(Debug)]
pub struct EnumItem<'a, 'b> {
    pub name: Name<'a, 'b>,
    pub tuple: Option<Vec<Ty<'a, 'b>>>,
    pub span: Span<'a, 'b>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Signature<'a, 'b> {
    pub name: Name<'a, 'b>,
    pub generic_params: Option<GenericParams<'a, 'b>>,
    pub self_kind: SelfKind<'a, 'b>,
    pub params: Vec<Param<'a, 'b>>,
    pub dots: Option<Span<'a, 'b>>,
    pub ret: Option<Ty<'a, 'b>>,
    pub where_clause: Option<WhereClause<'a, 'b>>,
    pub span: Span<'a, 'b>,
}

#[derive(Debug)]
pub struct GenericParams<'a, 'b> {
    pub params: Vec<Name<'a, 'b>>,
    pub span: Span<'a, 'b>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Param<'a, 'b> {
    pub pattern: Pattern<'a, 'b>,
    pub ty: Ty<'a, 'b>,
    pub span: Span<'a, 'b>,
}

#[derive(Debug)]
pub struct WhereClause<'a, 'b> {
    pub items: Vec<WhereItem<'a, 'b>>,
    pub span: Span<'a, 'b>,
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Block<'a, 'b> {
    pub stmts: Vec<Stmt<'a, 'b>>,
    pub expr: Option<Box<Expr<'a, 'b>>>,
    pub span: Span<'a, 'b>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Expr<'a, 'b> {
    Array {
        exprs: Vec<Expr<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Call {
        func: Box<Expr<'a, 'b>>,
        args: Vec<Expr<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    MethodCall {
        receiver: Box<Expr<'a, 'b>>,
        name: GenericSegment<'a, 'b>,
        args: Vec<Expr<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Tuple {
        exprs: Vec<Expr<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Binary {
        op: BinOp,
        lhs: Box<Expr<'a, 'b>>,
        rhs: Box<Expr<'a, 'b>>,
        op_span: Span<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Unary {
        op: UnOp,
        expr: Box<Expr<'a, 'b>>,
        op_span: Span<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Literal {
        kind: LiteralKind,
        span: Span<'a, 'b>,
    },
    SelfValue {
        span: Span<'a, 'b>,
    },
    Cast {
        expr: Box<Expr<'a, 'b>>,
        ty: Ty<'a, 'b>,
        span: Span<'a, 'b>,
    },
    If {
        test: Box<Expr<'a, 'b>>,
        block: Block<'a, 'b>,
        else_kind: ElseKind<'a, 'b>,
        span: Span<'a, 'b>,
    },
    IfLet {
        pattern: Pattern<'a, 'b>,
        expr: Box<Expr<'a, 'b>>,
        block: Block<'a, 'b>,
        else_kind: ElseKind<'a, 'b>,
        span: Span<'a, 'b>,
    },
    While {
        test: Box<Expr<'a, 'b>>,
        block: Block<'a, 'b>,
        span: Span<'a, 'b>,
    },
    WhileLet {
        pattern: Pattern<'a, 'b>,
        expr: Box<Expr<'a, 'b>>,
        block: Block<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Match {
        expr: Box<Expr<'a, 'b>>,
        arms: Vec<MatchArm<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Block {
        block: Block<'a, 'b>,
        span: Span<'a, 'b>,
    },
    For {
        pattern: Pattern<'a, 'b>,
        iter: Box<Expr<'a, 'b>>,
        block: Block<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Loop {
        block: Block<'a, 'b>,
        span: Span<'a, 'b>,
    },
    TryBlock {
        block: Block<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Label {
        label: Label<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Goto {
        label: Label<'a, 'b>,
        expr: Option<Box<Expr<'a, 'b>>>,
        span: Span<'a, 'b>,
    },
    Try {
        expr: Box<Expr<'a, 'b>>,
        qmark_span: Span<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Assign {
        target: Box<Expr<'a, 'b>>,
        rhs: Box<Expr<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    AssignOp {
        op: AssignOp,
        target: Box<Expr<'a, 'b>>,
        rhs: Box<Expr<'a, 'b>>,
        op_span: Span<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Field {
        expr: Box<Expr<'a, 'b>>,
        name: Name<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Index {
        expr: Box<Expr<'a, 'b>>,
        index: Box<Expr<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Range {
        low: Option<Box<Expr<'a, 'b>>>,
        high: Option<Box<Expr<'a, 'b>>>,
        range_span: Span<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Path {
        path: GenericPath<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Break {
        expr: Option<Box<Expr<'a, 'b>>>,
        span: Span<'a, 'b>,
    },
    Continue {
        span: Span<'a, 'b>,
    },
    Return {
        expr: Option<Box<Expr<'a, 'b>>>,
        span: Span<'a, 'b>,
    },
    Struct {
        path: GenericPath<'a, 'b>,
        fields: Vec<ExprField<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Repeat {
        expr: Box<Expr<'a, 'b>>,
        count: Span<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Paren {
        expr: Box<Expr<'a, 'b>>,
        span: Span<'a, 'b>,
    },
}

impl<'a, 'b> Expr<'a, 'b> {
    pub fn span(&self) -> Span<'a, 'b> {
        match self {
            Expr::Array { span, .. }
            | Expr::Call { span, .. }
            | Expr::MethodCall { span, .. }
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
pub struct GenericPath<'a, 'b> {
    pub segments: Vec<GenericSegment<'a, 'b>>,
    pub span: Span<'a, 'b>,
}

#[derive(Debug)]
pub struct GenericSegment<'a, 'b> {
    pub name: Name<'a, 'b>,
    pub generic_args: Option<GenericArgs<'a, 'b>>,
    pub span: Span<'a, 'b>,
}

#[derive(Debug)]
pub struct MatchArm<'a, 'b> {
    pub pattern: Pattern<'a, 'b>,
    pub body: Expr<'a, 'b>,
    pub span: Span<'a, 'b>,
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
pub struct Label<'a, 'b> {
    pub name: Name<'a, 'b>,
    pub span: Span<'a, 'b>,
}

#[derive(Debug)]
pub enum ElseKind<'a, 'b> {
    Else {
        block: Block<'a, 'b>,
        span: Span<'a, 'b>,
    },
    ElseIf {
        test: Box<Expr<'a, 'b>>,
        block: Block<'a, 'b>,
        else_kind: Box<ElseKind<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    ElseIfLet {
        pattern: Pattern<'a, 'b>,
        expr: Box<Expr<'a, 'b>>,
        block: Block<'a, 'b>,
        else_kind: Box<ElseKind<'a, 'b>>,
        span: Span<'a, 'b>,
    },
    Nothing {
        span: Span<'a, 'b>,
    },
}

impl<'a, 'b> ElseKind<'a, 'b> {
    pub fn span(&self) -> Span<'a, 'b> {
        match self {
            ElseKind::Else { span, .. }
            | ElseKind::ElseIf { span, .. }
            | ElseKind::ElseIfLet { span, .. }
            | ElseKind::Nothing { span, .. } => *span,
        }
    }
}

#[derive(Debug)]
pub enum ExprField<'a, 'b> {
    Name {
        name: Name<'a, 'b>,
        span: Span<'a, 'b>,
    },
    Expr {
        name: Name<'a, 'b>,
        expr: Expr<'a, 'b>,
        span: Span<'a, 'b>,
    },
}

impl<'a, 'b> ExprField<'a, 'b> {
    pub fn span(&self) -> Span<'a, 'b> {
        match self {
            ExprField::Name { span, .. } | ExprField::Expr { span, .. } => *span,
        }
    }
}

#[derive(Debug)]
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
    Array {
        array: Vec<Pattern<'a, 'b>>,
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
            | Pattern::Tuple { span, .. }
            | Pattern::Array { span, .. } => *span,
        }
    }
}

#[derive(Debug)]
pub struct FieldPattern<'a, 'b> {
    pub name: Name<'a, 'b>,
    pub pattern: Option<Pattern<'a, 'b>>,
    pub span: Span<'a, 'b>,
}

#[derive(Debug)]
pub struct ImplFn<'a, 'b> {
    pub is_pub: bool,
    pub signature: Signature<'a, 'b>,
    pub block: Block<'a, 'b>,
    pub span: Span<'a, 'b>,
}
