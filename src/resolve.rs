use std::collections::HashMap;

use crate::ast::*;
use crate::error::Error;
use crate::hir::HirId;
use crate::io::{FileId, FileMap};
use crate::span::Span;

#[derive(Debug)]
enum NameNode {
    Mod {
        visibility: Visibility,
        items: HashMap<String, NameNode>,
        id: HirId,
    },
    Fn {
        visibility: Visibility,
        id: HirId,
    },
    ExternFn {
        visibility: Visibility,
        id: HirId,
    },
    ExternType {
        visibility: Visibility,
        id: HirId,
        impl_fns: HashMap<String, ImplFnInfo>,
    },
    ExternStatic {
        visibility: Visibility,
        id: HirId,
    },
    Struct {
        visibility: Visibility,
        id: HirId,
        impl_fns: HashMap<String, ImplFnInfo>,
    },
    Enum {
        visibility: Visibility,
        id: HirId,
        variants: HashMap<String, HirId>,
        impl_fns: HashMap<String, ImplFnInfo>,
    },
    Trait {
        visibility: Visibility,
        id: HirId,
        trait_fns: HashMap<String, HirId>,
    },
    Const {
        visibility: Visibility,
        id: HirId,
    },
    Static {
        visibility: Visibility,
        id: HirId,
    },
}

impl NameNode {
    fn insert_impl_fn_id(&mut self, name: String, visibility: Visibility, id: HirId) {
        match self {
            NameNode::ExternType { impl_fns, .. }
            | NameNode::Struct { impl_fns, .. }
            | NameNode::Enum { impl_fns, .. } => {
                impl_fns.insert(name, ImplFnInfo { visibility, id });
            }
            _ => unreachable!(),
        }
    }

    fn id(&self) -> HirId {
        match self {
            NameNode::Mod { id, .. }
            | NameNode::Fn { id, .. }
            | NameNode::ExternFn { id, .. }
            | NameNode::ExternType { id, .. }
            | NameNode::ExternStatic { id, .. }
            | NameNode::Struct { id, .. }
            | NameNode::Enum { id, .. }
            | NameNode::Trait { id, .. }
            | NameNode::Const { id, .. }
            | NameNode::Static { id, .. } => *id,
        }
    }

    fn visibility(&self) -> Visibility {
        match self {
            NameNode::Mod { visibility, .. }
            | NameNode::Fn { visibility, .. }
            | NameNode::ExternFn { visibility, .. }
            | NameNode::ExternType { visibility, .. }
            | NameNode::ExternStatic { visibility, .. }
            | NameNode::Struct { visibility, .. }
            | NameNode::Enum { visibility, .. }
            | NameNode::Trait { visibility, .. }
            | NameNode::Const { visibility, .. }
            | NameNode::Static { visibility, .. } => *visibility,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Visibility {
    Pub,
    File(FileId),
}

impl Visibility {
    fn is_pub(&self) -> bool {
        matches!(self, Visibility::Pub)
    }
}

#[derive(Debug)]
struct ImplFnInfo {
    visibility: Visibility,
    id: HirId,
}

struct ImplTypeInfo<'a> {
    type_visibility: Visibility,
    fn_spans: HashMap<&'a String, Span>,
}

macro_rules! visibility {
    ($is_pub:expr, $span:expr) => {
        if $is_pub {
            Visibility::Pub
        } else {
            Visibility::File($span.location.file_id)
        }
    };
}

fn make_tree(items: &[Item]) -> Result<HashMap<String, NameNode>, Error> {
    let mut map = HashMap::new();
    let mut prev_spans = HashMap::new();
    let mut impl_spans = HashMap::new();

    macro_rules! check_redef {
        ($name:expr) => {{
            if let Some(&prev_span) = prev_spans.get(&$name.name) {
                return Err(Error::new(
                    format!("the name `{}` is defined multiple times", $name.name),
                    Some($name.span),
                )
                .note(
                    format!("previous definition of `{}` is here", $name.name),
                    Some(prev_span),
                ));
            }
            prev_spans.insert(&$name.name, $name.span);
        }};
    }

    for item in items {
        match item {
            Item::Use { .. } => {}
            Item::Struct { is_pub, name, .. } => {
                check_redef!(name);
                map.insert(
                    name.name.clone(),
                    NameNode::Struct {
                        visibility: visibility!(*is_pub, name.span),
                        id: name.id,
                        impl_fns: HashMap::new(),
                    },
                );

                impl_spans.insert(
                    &name.name,
                    ImplTypeInfo {
                        type_visibility: visibility!(*is_pub, name.span),
                        fn_spans: HashMap::new(),
                    },
                );
            }
            Item::Enum {
                is_pub,
                name,
                items,
                ..
            } => {
                check_redef!(name);

                let mut variant_spans = HashMap::new();
                let mut variants = HashMap::new();
                for item in items {
                    if let Some(span) = variant_spans.get(&item.name.name) {
                        return Err(Error::new(
                            format!(
                                "the name `{}` is defined multiple times in this enum",
                                item.name.name
                            ),
                            Some(item.name.span),
                        )
                        .note(
                            format!("previous definition of `{}` is here", item.name.name),
                            Some(*span),
                        ));
                    }
                    variant_spans.insert(&item.name.name, item.name.span);
                    variants.insert(item.name.name.clone(), item.name.id);
                }

                map.insert(
                    name.name.clone(),
                    NameNode::Enum {
                        visibility: visibility!(*is_pub, name.span),
                        id: name.id,
                        variants,
                        impl_fns: HashMap::new(),
                    },
                );
                impl_spans.insert(
                    &name.name,
                    ImplTypeInfo {
                        type_visibility: visibility!(*is_pub, name.span),
                        fn_spans: HashMap::new(),
                    },
                );
            }
            Item::Mod {
                is_pub,
                name,
                items,
                ..
            } => {
                check_redef!(name);
                let items = make_tree(&items[..])?;
                map.insert(
                    name.name.clone(),
                    NameNode::Mod {
                        visibility: visibility!(*is_pub, name.span),
                        items,
                        id: name.id,
                    },
                );
            }
            Item::Extern { items, .. } => {
                for item in items {
                    match item {
                        ExternItem::Fn {
                            is_pub, signature, ..
                        } => {
                            check_redef!(signature.name);
                            map.insert(
                                signature.name.name.clone(),
                                NameNode::ExternFn {
                                    visibility: visibility!(*is_pub, signature.name.span),
                                    id: signature.name.id,
                                },
                            );
                        }
                        ExternItem::Type { is_pub, name, .. } => {
                            check_redef!(name);
                            map.insert(
                                name.name.clone(),
                                NameNode::ExternType {
                                    visibility: visibility!(*is_pub, name.span),
                                    id: name.id,
                                    impl_fns: HashMap::new(),
                                },
                            );
                            impl_spans.insert(
                                &name.name,
                                ImplTypeInfo {
                                    type_visibility: visibility!(*is_pub, name.span),
                                    fn_spans: HashMap::new(),
                                },
                            );
                        }
                        ExternItem::Static { is_pub, name, .. } => {
                            check_redef!(name);
                            map.insert(
                                name.name.clone(),
                                NameNode::ExternStatic {
                                    visibility: visibility!(*is_pub, name.span),
                                    id: name.id,
                                },
                            );
                        }
                    }
                }
            }
            Item::Trait {
                is_pub,
                name,
                items,
                ..
            } => {
                check_redef!(name);

                let mut fn_spans = HashMap::new();
                let mut trait_fns = HashMap::new();
                for item in items {
                    match item {
                        TraitItem::Required { signature, .. }
                        | TraitItem::Provided { signature, .. } => {
                            if let Some(span) = fn_spans.get(&signature.name.name) {
                                return Err(Error::new(
                                    format!(
                                        "the function `{}` is defined multiple times in this trait",
                                        signature.name.name
                                    ),
                                    Some(signature.name.span),
                                )
                                .note(
                                    format!(
                                        "previous definition of `{}` is here",
                                        signature.name.name
                                    ),
                                    Some(*span),
                                ));
                            }
                            fn_spans.insert(&signature.name.name, signature.name.span);
                            trait_fns.insert(signature.name.name.clone(), signature.name.id);
                        }
                    }
                }

                map.insert(
                    name.name.clone(),
                    NameNode::Trait {
                        visibility: visibility!(*is_pub, name.span),
                        id: name.id,
                        trait_fns,
                    },
                );
            }
            Item::Fn {
                is_pub, signature, ..
            } => {
                check_redef!(signature.name);
                map.insert(
                    signature.name.name.clone(),
                    NameNode::Fn {
                        visibility: visibility!(*is_pub, signature.name.span),
                        id: signature.name.id,
                    },
                );
            }
            Item::Impl { .. } => {}
            Item::Const { is_pub, name, .. } => {
                check_redef!(name);
                map.insert(
                    name.name.clone(),
                    NameNode::Const {
                        visibility: visibility!(*is_pub, name.span),
                        id: name.id,
                    },
                );
            }
            Item::Static { is_pub, name, .. } => {
                check_redef!(name);
                map.insert(
                    name.name.clone(),
                    NameNode::Static {
                        visibility: visibility!(*is_pub, name.span),
                        id: name.id,
                    },
                );
            }
        }
    }

    for item in items {
        if let Item::Impl {
            name,
            as_trait: None,
            fns,
            ..
        } = item
        {
            if let Some(ImplTypeInfo {
                type_visibility,
                fn_spans,
            }) = impl_spans.get_mut(&name.name)
            {
                for ImplFn {
                    is_pub, signature, ..
                } in fns
                {
                    if let Some(span) = fn_spans.get(&signature.name.name) {
                        return Err(Error::new(
                            format!("duplicate definitions with name `{}`", signature.name.name),
                            Some(signature.name.span),
                        )
                        .note(
                            format!("previous definition of `{}` is here", signature.name.name),
                            Some(*span),
                        ));
                    }
                    if *is_pub && !type_visibility.is_pub() {
                        return Err(Error::new(
                            format!(
                                "function `{}` is declared pub on non-pub type `{}`",
                                signature.name.name, name.name
                            ),
                            Some(signature.name.span),
                        ));
                    }
                    fn_spans.insert(&signature.name.name, signature.name.span);
                    map.get_mut(&name.name).unwrap().insert_impl_fn_id(
                        signature.name.name.clone(),
                        visibility!(*is_pub, signature.name.span),
                        signature.name.id,
                    );
                }
            } else {
                return Err(Error::new(
                    format!("no type `{}` in this module", name.name),
                    Some(name.span),
                ));
            }
        }
    }

    Ok(map)
}

fn populate_id_map<'a>(
    id_map: &mut HashMap<HirId, &'a NameNode>,
    tree: &'a HashMap<String, NameNode>,
) {
    for node in tree.values() {
        match node {
            NameNode::Mod { id, items, .. } => {
                id_map.insert(*id, node);
                populate_id_map(id_map, items);
            }
            NameNode::ExternType { id, .. } => {
                id_map.insert(*id, node);
            }
            NameNode::Struct { id, .. } => {
                id_map.insert(*id, node);
            }
            NameNode::Enum { id, .. } => {
                id_map.insert(*id, node);
            }
            NameNode::Trait { id, .. } => {
                id_map.insert(*id, node);
            }
            _ => {}
        };
    }
}

struct Walker<'a> {
    global: &'a HashMap<String, NameNode>,
    id_map: HashMap<HirId, &'a NameNode>,
    stack: Vec<HashMap<&'a String, HirId>>,
    map: &'a FileMap,
}

impl<'a> Walker<'a> {
    fn walk(&mut self, items: &'a [Item]) -> Result<(), Error> {
        let mut frame = HashMap::new();
        let mut prev_spans = HashMap::new();

        macro_rules! handle {
            ($name:expr) => {{
                if let Some(&prev_span) = prev_spans.get(&$name.name) {
                    return Err(Error::new(
                        format!("the name `{}` is defined multiple times", $name.name),
                        Some($name.span),
                    )
                    .note(
                        format!("previous definition of `{}` is here", $name.name),
                        Some(prev_span),
                    ));
                }
                prev_spans.insert(&$name.name, $name.span);
                frame.insert(&$name.name, $name.id);
            }};
        }

        for item in items {
            match item {
                Item::Use {
                    crate_span: Some(crate_span),
                    tree: UseTree { prefix, kind, .. },
                    ..
                } => {
                    let crate_name = self.map.get_crate_name(crate_span.location.file_id);
                    let id = self.global[crate_name].id();
                    self.walk_use(
                        *crate_span,
                        id,
                        &prefix.path,
                        kind,
                        &mut frame,
                        &mut prev_spans,
                    )?;
                }
                Item::Use {
                    tree: UseTree { prefix, kind, .. },
                    ..
                } => {
                    let first_span = prefix.path[0].span;
                    let id = if let Some(node) = self.global.get(&prefix.path[0].name) {
                        node.id()
                    } else {
                        return Err(Error::new(
                            format!("no external crate `{}`", prefix.path[0].name),
                            Some(first_span),
                        ));
                    };
                    self.walk_use(
                        first_span,
                        id,
                        &prefix.path[1..],
                        kind,
                        &mut frame,
                        &mut prev_spans,
                    )?;
                }
                Item::Struct { name, .. }
                | Item::Enum { name, .. }
                | Item::Const { name, .. }
                | Item::Static { name, .. }
                | Item::Trait { name, .. }
                | Item::Mod { name, .. } => handle!(name),
                Item::Fn { signature, .. } => handle!(signature.name),
                Item::Extern { items, .. } => {
                    for item in items {
                        match item {
                            ExternItem::Fn { signature, .. } => handle!(signature.name),
                            ExternItem::Type { name, .. } | ExternItem::Static { name, .. } => {
                                handle!(name)
                            }
                        }
                    }
                }
                Item::Impl { .. } => {}
            }
        }
        self.stack.push(dbg!(frame));

        for item in items {
            match item {
                Item::Use { .. } => {}
                Item::Struct {
                    generic_params,
                    fields,
                    ..
                } => {
                    self.walk_generic_params(generic_params)?;
                    for field in fields {
                        self.walk_ty(&field.ty)?;
                    }
                    self.stack.pop();
                }
                Item::Enum {
                    generic_params,
                    items,
                    ..
                } => {
                    self.walk_generic_params(generic_params)?;
                    for item in items {
                        for ty in item.tuple.iter().flatten() {
                            self.walk_ty(ty)?;
                        }
                    }
                    self.stack.pop();
                }
                Item::Mod { items, .. } => {
                    let frame = self.stack.pop().unwrap();
                    self.walk(items)?;
                    self.stack.push(frame);
                }
                Item::Extern { items, .. } => {
                    for item in items {
                        match item {
                            ExternItem::Fn { signature, .. } => {
                                self.walk_signature(signature)?;
                                self.stack.pop();
                            }
                            ExternItem::Type { .. } => {}
                            ExternItem::Static { ty, .. } => self.walk_ty(ty)?,
                        }
                    }
                }
                Item::Trait {
                    generic_params,
                    self_bounds,
                    where_clause,
                    items,
                    ..
                } => {
                    self.walk_generic_params(generic_params)?;
                    for bound in self_bounds {
                        self.walk_trait_bound(bound)?;
                    }
                    self.walk_where_clause(where_clause)?;
                    for item in items {
                        match item {
                            TraitItem::Required { signature, .. } => {
                                self.walk_signature(signature)?;
                                self.stack.pop();
                            }
                            TraitItem::Provided {
                                signature, block, ..
                            } => {
                                self.walk_signature(signature)?;
                                self.walk_block(block)?;
                                self.stack.pop();
                            }
                        }
                    }
                    self.stack.pop();
                }
                Item::Fn {
                    signature, block, ..
                } => {
                    self.walk_signature(signature)?;
                    self.walk_block(block)?;
                    self.stack.pop();
                }
                Item::Impl {
                    generic_params,
                    as_trait,
                    where_clause,
                    fns,
                    ..
                } => {
                    self.walk_generic_params(generic_params)?;
                    if let Some(as_trait) = as_trait {
                        self.walk_trait_bound(as_trait)?;
                    }
                    self.walk_where_clause(where_clause)?;
                    for ImplFn {
                        signature, block, ..
                    } in fns
                    {
                        self.walk_signature(signature)?;
                        self.walk_block(block)?;
                        self.stack.pop();
                    }
                    self.stack.pop();
                }
                Item::Const { ty, expr, .. } => {
                    self.walk_ty(ty)?;
                    self.walk_expr(expr)?;
                }
                Item::Static { ty, expr, .. } => {
                    self.walk_ty(ty)?;
                    if let Some(expr) = expr {
                        self.walk_expr(expr)?;
                    }
                }
            }
        }

        self.stack.pop();

        Ok(())
    }

    fn walk_use(
        &mut self,
        mut prev_segment_span: Span,
        mut id: HirId,
        prefix: &'a [Name],
        kind: &'a UseTreeKind,
        frame: &mut HashMap<&'a String, HirId>,
        prev_spans: &mut HashMap<&'a String, Span>,
    ) -> Result<(), Error> {
        for segment in prefix {
            match self.id_map.get(&id) {
                Some(NameNode::Mod { items, .. }) => {
                    if let Some(node) = items.get(&segment.name) {
                        if let Visibility::File(id) = node.visibility() {
                            if id != segment.span.location.file_id {
                                return Err(Error::new(
                                    format!("`{}` is private", segment.name),
                                    Some(segment.span),
                                ));
                            }
                        }
                        id = node.id();
                        prev_segment_span = segment.span;
                    } else {
                        return Err(Error::new(
                            format!(
                                "cannot find `{}` in `{}`",
                                segment.name,
                                self.map.text_at(prev_segment_span)
                            ),
                            Some(segment.span),
                        ));
                    }
                }
                Some(NameNode::ExternType { impl_fns, .. } | NameNode::Struct { impl_fns, .. }) => {
                    if let Some(info) = impl_fns.get(&segment.name) {
                        if let Visibility::File(id) = info.visibility {
                            if id != segment.span.location.file_id {
                                return Err(Error::new(
                                    format!("function `{}` is private", segment.name),
                                    Some(segment.span),
                                ));
                            }
                        }
                        id = info.id;
                        prev_segment_span = segment.span;
                    } else {
                        return Err(Error::new(
                            format!(
                                "function `{}` not found in type `{}`",
                                segment.name,
                                self.map.text_at(prev_segment_span)
                            ),
                            Some(segment.span),
                        ));
                    }
                }
                Some(NameNode::Enum {
                    variants, impl_fns, ..
                }) => {
                    if let Some(variant_id) = variants.get(&segment.name) {
                        id = *variant_id;
                        prev_segment_span = segment.span;
                    } else if let Some(info) = impl_fns.get(&segment.name) {
                        if let Visibility::File(id) = info.visibility {
                            if id != segment.span.location.file_id {
                                return Err(Error::new(
                                    format!("function `{}` is private", segment.name),
                                    Some(segment.span),
                                ));
                            }
                        }
                        id = info.id;
                        prev_segment_span = segment.span;
                    } else {
                        return Err(Error::new(
                            format!(
                                "function `{}` not found in type `{}`",
                                segment.name,
                                self.map.text_at(prev_segment_span)
                            ),
                            Some(segment.span),
                        ));
                    }
                }
                Some(NameNode::Trait { trait_fns, .. }) => {
                    if let Some(trait_fn_id) = trait_fns.get(&segment.name) {
                        id = *trait_fn_id;
                        prev_segment_span = segment.span;
                    } else {
                        return Err(Error::new(
                            format!(
                                "function `{}` not found in trait `{}`",
                                segment.name,
                                self.map.text_at(prev_segment_span)
                            ),
                            Some(segment.span),
                        ));
                    }
                }
                Some(_) => unreachable!(),
                None => {
                    return Err(Error::new(
                        format!(
                            "`{}` is not a type or module",
                            self.map.text_at(prev_segment_span)
                        ),
                        Some(prev_segment_span),
                    ))
                }
            }
        }

        match kind {
            UseTreeKind::Simple => {
                if !prefix.is_empty() {
                    let last = &prefix[prefix.len() - 1];
                    if let Some(&prev_span) = prev_spans.get(&last.name) {
                        return Err(Error::new(
                            format!("the name `{}` is defined multiple times", last.name),
                            Some(last.span),
                        )
                        .note(
                            format!("previous definition of `{}` is here", last.name),
                            Some(prev_span),
                        ));
                    }
                    prev_spans.insert(&last.name, last.span);
                    frame.insert(&last.name, id);
                }
            }
            UseTreeKind::Rename(name) => {
                if let Some(&prev_span) = prev_spans.get(&name.name) {
                    return Err(Error::new(
                        format!("the name `{}` is defined multiple times", name.name),
                        Some(name.span),
                    )
                    .note(
                        format!("previous definition of `{}` is here", name.name),
                        Some(prev_span),
                    ));
                }
                prev_spans.insert(&name.name, name.span);
                frame.insert(&name.name, id);
            }
            UseTreeKind::Nested(trees) => {
                for UseTree { prefix, kind, .. } in trees {
                    self.walk_use(prev_segment_span, id, &prefix.path, kind, frame, prev_spans)?;
                }
            }
        }

        Ok(())
    }

    fn walk_generic_params(
        &mut self,
        generic_params: &'a Option<GenericParams>,
    ) -> Result<(), Error> {
        let mut frame = HashMap::new();
        if let Some(params) = generic_params {
            let mut prev_spans = HashMap::new();
            for param in &params.params {
                if let Some(&prev_span) = prev_spans.get(&param.name) {
                    return Err(Error::new(
                        format!("the generic parameter `{}` already exists", param.name),
                        Some(param.span),
                    )
                    .note(
                        format!("first use of `{}` is here", param.name),
                        Some(prev_span),
                    ));
                }
                prev_spans.insert(&param.name, param.span);
                frame.insert(&param.name, param.id);
            }
        }
        self.stack.push(frame);
        Ok(())
    }

    fn walk_ty(&mut self, ty: &Ty) -> Result<(), Error> {
        match ty {
            Ty::Path {
                path, generic_args, ..
            } => {
                self.walk_path(path)?;
                self.walk_generic_args(generic_args)?;
            }
            Ty::Ptr { ty, .. } | Ty::Slice { ty, .. } | Ty::Array { ty, .. } => self.walk_ty(ty)?,
            Ty::Tuple { tys, .. } => {
                for ty in tys {
                    self.walk_ty(ty)?;
                }
            }
            Ty::Fn { params, ret, .. } => {
                for param in params {
                    self.walk_ty(param)?;
                }
                if let Some(ret) = ret {
                    self.walk_ty(ret)?;
                }
            }
            Ty::SelfType { .. } | Ty::Never { .. } => {}
        }
        Ok(())
    }

    fn walk_trait_bound(&mut self, bound: &TraitBound) -> Result<(), Error> {
        match bound {
            TraitBound::Trait {
                path, generic_args, ..
            } => {
                self.walk_path(path)?;
                self.walk_generic_args(generic_args)?;
            }
            TraitBound::Fn {
                path, params, ret, ..
            } => {
                self.walk_path(path)?;
                for param in params {
                    self.walk_ty(param)?;
                }
                if let Some(ret) = ret {
                    self.walk_ty(ret)?;
                }
            }
        }
        Ok(())
    }

    fn walk_where_clause(&mut self, where_clause: &Option<WhereClause>) -> Result<(), Error> {
        if let Some(clause) = where_clause {
            for WhereItem { param, bounds, .. } in &clause.items {
                self.walk_where_param(param)?;
                for bound in bounds {
                    self.walk_trait_bound(bound)?;
                }
            }
        }
        Ok(())
    }

    fn walk_where_param(&mut self, _param: &Name) -> Result<(), Error> {
        todo!()
    }

    fn walk_generic_args(&mut self, generic_args: &Option<GenericArgs>) -> Result<(), Error> {
        if let Some(args) = generic_args {
            for arg in &args.args {
                self.walk_ty(arg)?;
            }
        }
        Ok(())
    }

    fn walk_signature(&mut self, _signature: &Signature) -> Result<(), Error> {
        todo!()
    }

    fn walk_block(&mut self, _block: &Block) -> Result<(), Error> {
        todo!()
    }

    fn walk_expr(&mut self, _expr: &Expr) -> Result<(), Error> {
        todo!()
    }

    fn walk_path(&mut self, _path: &Path) -> Result<(), Error> {
        todo!()
    }

    fn resolved(self) -> Resolved {
        Resolved
    }
}

#[derive(Debug)]
pub struct Resolved;

pub fn resolve(crates: HashMap<String, Vec<Item>>, map: &FileMap) -> Result<Resolved, Error> {
    let mut tree = HashMap::new();

    for (name, items) in &crates {
        tree.insert(
            name.clone(),
            NameNode::Mod {
                visibility: Visibility::Pub,
                items: make_tree(&items[..])?,
                id: HirId::new(),
            },
        );
    }

    let mut id_map = HashMap::new();
    populate_id_map(&mut id_map, &tree);

    println!("crates: {crates:#?}");
    println!("id_map: {id_map:#?}");
    println!("tree: {tree:#?}");

    let mut walker = Walker {
        global: &tree,
        id_map,
        stack: Vec::new(),
        map,
    };

    for items in crates.values() {
        walker.walk(items)?;
    }

    Ok(walker.resolved())
}
