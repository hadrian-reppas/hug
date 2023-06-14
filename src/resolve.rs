use std::collections::{HashMap, HashSet};

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
    local_enum_id_map: Vec<HashMap<HirId, HashMap<&'a String, HirId>>>,
    stack: Vec<HashMap<&'a String, HirId>>,
    locals: HashSet<HirId>,
    min_local_search_index: Vec<usize>,
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
                    crate_span, tree, ..
                } => self.walk_full_use(crate_span, tree, &mut frame, &mut prev_spans)?,
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

        self.stack.pop().unwrap();
        assert!(self.stack.is_empty());

        Ok(())
    }

    fn walk_full_use(
        &mut self,
        crate_span: &Option<Span>,
        tree: &'a UseTree,
        frame: &mut HashMap<&'a String, HirId>,
        prev_spans: &mut HashMap<&'a String, Span>,
    ) -> Result<(), Error> {
        if let Some(crate_span) = crate_span {
            let crate_name = self.map.get_crate_name(crate_span.location.file_id);
            let id = self.global[crate_name].id();
            self.walk_use(
                *crate_span,
                id,
                &tree.prefix.path,
                &tree.kind,
                frame,
                prev_spans,
            )
        } else {
            let first_span = tree.prefix.path[0].span;
            let id = if let Some(node) = self.global.get(&tree.prefix.path[0].name) {
                node.id()
            } else {
                return Err(Error::new(
                    format!("no external crate `{}`", tree.prefix.path[0].name),
                    Some(first_span),
                ));
            };
            self.walk_use(
                first_span,
                id,
                &tree.prefix.path[1..],
                &tree.kind,
                frame,
                prev_spans,
            )
        }
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

    fn walk_where_param(&mut self, param: &Name) -> Result<(), Error> {
        if let Some(id) = self.stack.last().unwrap().get(&param.name) {
            param.res.set(*id);
            Ok(())
        } else {
            Err(Error::new(
                format!(
                    "cannot find type parameter `{}` in this function signature",
                    param.name
                ),
                Some(param.span),
            ))
        }
    }

    fn walk_generic_args(&mut self, generic_args: &Option<GenericArgs>) -> Result<(), Error> {
        if let Some(args) = generic_args {
            for arg in &args.args {
                self.walk_ty(arg)?;
            }
        }
        Ok(())
    }

    fn walk_signature(&mut self, signature: &'a Signature) -> Result<(), Error> {
        self.walk_generic_params(&signature.generic_params)?;
        let mut prev_spans = HashMap::new();
        for param in &signature.params {
            self.walk_ty(&param.ty)?;
            self.walk_pattern(&param.pattern, &mut prev_spans, true)?;
        }
        if let Some(ret) = &signature.ret {
            self.walk_ty(&ret)?;
        }
        self.walk_where_clause(&signature.where_clause)?;

        if let Some(variadic) = &signature.variadic {
            if let Some(prev_span) = prev_spans.get(&variadic.name.name) {
                return Err(Error::new(
                    format!(
                        "`{}` is bound more than once in this parameter list",
                        variadic.name.name
                    ),
                    Some(variadic.name.span),
                )
                .note(
                    format!("first use of `{}` is here", variadic.name.name),
                    Some(*prev_span),
                ));
            }
        }
        Ok(())
    }

    fn walk_pattern(
        &mut self,
        pattern: &'a Pattern,
        prev_spans: &mut HashMap<&'a String, Span>,
        is_signature: bool,
    ) -> Result<(), Error> {
        match pattern {
            Pattern::Wild { .. } => {}
            Pattern::Path { path, .. } => {
                if path.path.len() == 1 {
                    if let Err(_) = self.walk_path(path) {
                        let name = &path.path[0];
                        if let Some(prev_span) = prev_spans.get(&name.name) {
                            return Err(Error::new(
                                if is_signature {
                                    format!(
                                        "`{}` is bound more than once in this parameter list",
                                        name.name
                                    )
                                } else {
                                    format!(
                                        "`{}` is bound more than once in the same pattern",
                                        name.name
                                    )
                                },
                                Some(name.span),
                            )
                            .note(
                                format!("first use of `{}` is here", name.name),
                                Some(*prev_span),
                            ));
                        } else {
                            self.define(&name.name, name.id);
                        }
                    }
                } else {
                    self.walk_path(path)?;
                }
            }
            Pattern::Struct { path, fields, .. } => {
                self.walk_path(path)?;
                for field in fields {
                    if let Some(pat) = &field.pattern {
                        self.walk_pattern(pat, prev_spans, is_signature)?;
                    }
                }
            }
            Pattern::Enum { path, tuple, .. } => {
                self.walk_path(path)?;
                for pat in tuple {
                    self.walk_pattern(pat, prev_spans, is_signature)?;
                }
            }
            Pattern::Tuple { tuple, .. } => {
                for pat in tuple {
                    self.walk_pattern(pat, prev_spans, is_signature)?;
                }
            }
            Pattern::Array { array, .. } => {
                for pat in array {
                    self.walk_pattern(pat, prev_spans, is_signature)?;
                }
            }
        }
        Ok(())
    }

    fn walk_block(&mut self, block: &'a Block) -> Result<(), Error> {
        const WILD_PATTERN: Pattern = Pattern::Wild {
            span: Span::empty(),
        };
        self.walk_block_with_pattern(block, &WILD_PATTERN)
    }

    fn walk_block_with_pattern(
        &mut self,
        block: &'a Block,
        pattern: &'a Pattern,
    ) -> Result<(), Error> {
        // TODO: check `Expr`s for `Label`s
        let mut frame = HashMap::new();
        let mut prev_spans = HashMap::new();
        let mut enum_id_map = HashMap::new();

        macro_rules! handle {
            ($name:expr) => {{
                if let Some(&prev_span) = prev_spans.get(&$name.name) {
                    return Err(Error::new(
                        format!(
                            "the name `{}` is defined multiple times in this block",
                            $name.name
                        ),
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

        for stmt in &block.stmts {
            match stmt {
                Stmt::Local { .. } | Stmt::Expr { .. } => {}
                Stmt::Use {
                    crate_span, tree, ..
                } => self.walk_full_use(crate_span, tree, &mut frame, &mut prev_spans)?,
                Stmt::Struct { name, .. }
                | Stmt::Const { name, .. }
                | Stmt::Static { name, .. } => handle!(name),
                Stmt::Enum { name, items, .. } => {
                    handle!(name);
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
                        variants.insert(&item.name.name, item.name.id);
                    }
                    enum_id_map.insert(name.id, variants);
                }
                Stmt::Extern { items, .. } => {
                    for item in items {
                        match item {
                            ExternItem::Fn { signature, .. } => handle!(signature.name),
                            ExternItem::Type { name, .. } | ExternItem::Static { name, .. } => {
                                handle!(name)
                            }
                        }
                    }
                }
                Stmt::Fn { signature, .. } => handle!(signature.name),
            }
        }
        self.stack.push(frame);
        self.local_enum_id_map.push(enum_id_map);

        self.walk_pattern(pattern, &mut HashMap::new(), false)?;

        for stmt in &block.stmts {
            self.walk_stmt(stmt)?;
        }

        self.stack.pop().unwrap();
        self.local_enum_id_map.pop();
        Ok(())
    }

    fn define(&mut self, name: &'a String, id: HirId) {
        self.stack.last_mut().unwrap().insert(name, id);
        self.locals.insert(id);
    }

    fn walk_stmt(&mut self, stmt: &'a Stmt) -> Result<(), Error> {
        // make sure to resolve types in structs/enums/fns
        // also make sure to update `self.local_search_depth` in function bodies
        match stmt {
            Stmt::Local {
                pattern, ty, expr, ..
            } => {
                if let Some(expr) = expr {
                    self.walk_expr(expr)?;
                }
                if let Some(ty) = ty {
                    self.walk_ty(ty)?;
                }
                self.walk_pattern(pattern, &mut HashMap::new(), false)?;
            }
            Stmt::Expr { expr, .. } => self.walk_expr(expr)?,
            Stmt::Use { .. } => {}
            Stmt::Struct {
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
            Stmt::Enum {
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
            Stmt::Extern { items, .. } => {
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
            Stmt::Fn {
                signature, block, ..
            } => {
                self.walk_signature(signature)?;
                self.min_local_search_index.push(self.stack.len());
                self.walk_block(block)?;
                self.min_local_search_index.pop();
                self.stack.pop();
            }
            Stmt::Const { ty, expr, .. } => {
                self.walk_ty(ty)?;
                self.walk_expr(expr)?;
            }
            Stmt::Static { ty, expr, .. } => {
                self.walk_ty(ty)?;
                if let Some(expr) = expr {
                    self.walk_expr(expr)?;
                }
            }
        }
        Ok(())
    }

    fn walk_expr(&mut self, expr: &'a Expr) -> Result<(), Error> {
        match expr {
            Expr::Array { exprs, .. } => {
                for expr in exprs {
                    self.walk_expr(expr)?;
                }
            }
            Expr::Call { func, args, .. } => {
                self.walk_expr(func)?;
                for arg in args {
                    self.walk_expr(arg)?;
                }
            }
            Expr::MethodCall {
                receiver,
                name,
                args,
                ..
            } => {
                self.walk_expr(receiver)?;
                self.walk_generic_args(&name.generic_args)?;
                for arg in args {
                    self.walk_expr(arg)?;
                }
            }
            Expr::Macro { args, .. } => {
                for arg in args {
                    self.walk_expr(arg)?;
                }
            }
            Expr::Tuple { exprs, .. } => {
                for expr in exprs {
                    self.walk_expr(expr)?;
                }
            }
            Expr::Binary { lhs, rhs, .. } => {
                self.walk_expr(lhs)?;
                self.walk_expr(rhs)?;
            }
            Expr::Unary { expr, .. } => self.walk_expr(expr)?,
            Expr::Literal { .. } | Expr::SelfValue { .. } => {}
            Expr::Cast { expr, ty, .. } => {
                self.walk_expr(expr)?;
                self.walk_ty(ty)?;
            }
            Expr::If {
                test,
                block,
                else_kind,
                ..
            } => {
                self.walk_expr(test)?;
                self.walk_block(block)?;
                self.walk_else_kind(else_kind)?;
            }
            Expr::IfLet {
                pattern,
                expr,
                block,
                ..
            } => {
                self.walk_expr(expr)?;
                self.walk_block_with_pattern(block, pattern)?;
            }
            Expr::While { test, block, .. } => {
                self.walk_expr(test)?;
                self.walk_block(block)?;
            }
            Expr::WhileLet {
                pattern,
                expr,
                block,
                ..
            } => {
                self.walk_expr(expr)?;
                self.walk_block_with_pattern(block, pattern)?;
            }
            Expr::Match { expr, arms, .. } => {
                self.walk_expr(expr)?;
                for arm in arms {
                    self.walk_expr_with_pattern(expr, &arm.pattern)?;
                }
            }
            Expr::Block { block, .. } => self.walk_block(block)?,
            Expr::For {
                pattern,
                iter,
                block,
                ..
            } => {
                self.walk_expr(iter)?;
                self.walk_block_with_pattern(block, pattern)?;
            }
            Expr::Loop { block, .. } | Expr::TryBlock { block, .. } => self.walk_block(block)?,
            Expr::Label { .. } => {}
            Expr::Goto { label, expr, .. } => {
                self.walk_label(label)?;
                if let Some(expr) = expr {
                    self.walk_expr(expr)?;
                }
            }
            Expr::Try { expr, .. } => self.walk_expr(expr)?,
            Expr::Assign { target, rhs, .. } => {
                self.walk_expr(target)?;
                self.walk_expr(rhs)?;
            }
            Expr::AssignOp { target, rhs, .. } => {
                self.walk_expr(target)?;
                self.walk_expr(rhs)?;
            }
            Expr::Field { expr, .. } => self.walk_expr(expr)?,
            Expr::TupleField { expr, .. } => self.walk_expr(expr)?,
            Expr::Index { expr, index, .. } => {
                self.walk_expr(expr)?;
                self.walk_expr(index)?;
            }
            Expr::Range { low, high, .. } => {
                if let Some(low) = low {
                    self.walk_expr(low)?;
                }
                if let Some(high) = high {
                    self.walk_expr(high)?;
                }
            }
            Expr::Path { path, .. } => self.walk_generic_path(path)?,
            Expr::Break { .. } | Expr::Continue { .. } => {}
            Expr::Return { expr, .. } => {
                if let Some(expr) = expr {
                    self.walk_expr(expr)?;
                }
            }
            Expr::Struct { path, fields, .. } => {
                self.walk_generic_path(path)?;
                for field in fields {
                    self.walk_expr_field(field)?;
                }
            }
            Expr::Repeat { expr, .. } | Expr::Paren { expr, .. } => self.walk_expr(expr)?,
        }
        Ok(())
    }

    fn walk_expr_field(&mut self, field: &'a ExprField) -> Result<(), Error> {
        match field {
            ExprField::Name { name, .. } => name.res.set(self.resolve_name(name, false)?),
            ExprField::Expr { expr, .. } => self.walk_expr(expr)?,
        }
        Ok(())
    }

    fn walk_expr_with_pattern(
        &mut self,
        expr: &'a Expr,
        pattern: &'a Pattern,
    ) -> Result<(), Error> {
        self.stack.push(HashMap::new());
        self.walk_pattern(pattern, &mut HashMap::new(), false)?;
        self.walk_expr(expr)?;
        self.stack.pop();
        Ok(())
    }

    fn walk_else_kind(&mut self, else_kind: &'a ElseKind) -> Result<(), Error> {
        match else_kind {
            ElseKind::Else { block, .. } => self.walk_block(block),
            ElseKind::ElseIf {
                test,
                block,
                else_kind,
                ..
            } => {
                self.walk_expr(test)?;
                self.walk_block(block)?;
                self.walk_else_kind(else_kind)
            }
            ElseKind::ElseIfLet {
                pattern,
                expr,
                block,
                else_kind,
                ..
            } => {
                self.walk_expr(expr)?;
                self.walk_block_with_pattern(block, pattern)?;
                self.walk_else_kind(else_kind)
            }
            ElseKind::Nothing { .. } => Ok(()),
        }
    }

    fn walk_path(&mut self, path: &Path) -> Result<(), Error> {
        let (mut id, mut prev_segment_span, suffix) = if let Some(crate_span) = path.crate_span {
            let crate_name = self.map.get_crate_name(crate_span.location.file_id);
            (self.global[crate_name].id(), crate_span, &path.path[..])
        } else if let Ok((id, _)) = self.resolve_name_with_index(&path.path[0]) {
            path.path[0].res.set(id);
            (id, path.path[0].span, &path.path[1..])
        } else if let Some(node) = self.global.get(&path.path[0].name) {
            path.path[0].res.set(node.id());
            (node.id(), path.path[0].span, &path.path[1..])
        } else {
            return Err(Error::new(
                format!("undeclared module or type `{}`", path.path[0].name),
                Some(path.path[0].span),
            ));
        };

        for segment in suffix {
            (id, prev_segment_span) = self.step_path(segment, id, prev_segment_span)?;
        }

        Ok(())
    }

    fn walk_generic_path(&mut self, path: &GenericPath) -> Result<(), Error> {
        if path.crate_span.is_none() && path.segments.len() == 1 {
            let id = self.resolve_name(&path.segments[0].name, false)?;
            path.segments[0].name.res.set(id);
            return Ok(());
        }

        let (mut id, mut prev_segment_span, suffix) = if let Some(crate_span) = path.crate_span {
            let crate_name = self.map.get_crate_name(crate_span.location.file_id);
            (self.global[crate_name].id(), crate_span, &path.segments[..])
        } else if let Ok((id, _)) = self.resolve_name_with_index(&path.segments[0].name) {
            self.walk_generic_args(&path.segments[0].generic_args)?;
            path.segments[0].name.res.set(id);
            (id, path.segments[0].name.span, &path.segments[1..])
        } else if let Some(node) = self.global.get(&path.segments[0].name.name) {
            self.walk_generic_args(&path.segments[0].generic_args)?;
            path.segments[0].name.res.set(node.id());
            (node.id(), path.segments[0].name.span, &path.segments[1..])
        } else {
            return Err(Error::new(
                format!("undeclared module or type `{}`", path.segments[0].name.name),
                Some(path.segments[0].name.span),
            ));
        };

        for segment in suffix {
            self.walk_generic_args(&segment.generic_args)?;
            (id, prev_segment_span) = self.step_path(&segment.name, id, prev_segment_span)?;
        }

        Ok(())
    }

    fn step_path(
        &mut self,
        segment_name: &Name,
        id: HirId,
        prev_segment_span: Span,
    ) -> Result<(HirId, Span), Error> {
        match self.id_map.get(&id) {
            Some(NameNode::Mod { items, .. }) => {
                if let Some(node) = items.get(&segment_name.name) {
                    if let Visibility::File(id) = node.visibility() {
                        if id != prev_segment_span.location.file_id {
                            return Err(Error::new(
                                format!("`{}` is private", segment_name.name),
                                Some(segment_name.span),
                            ));
                        }
                    }
                    segment_name.res.set(node.id());
                    Ok((node.id(), segment_name.span))
                } else {
                    Err(Error::new(
                        format!(
                            "cannot find `{}` in `{}`",
                            segment_name.name,
                            self.map.text_at(prev_segment_span)
                        ),
                        Some(segment_name.span),
                    ))
                }
            }
            Some(NameNode::ExternType { impl_fns, .. } | NameNode::Struct { impl_fns, .. }) => {
                if let Some(info) = impl_fns.get(&segment_name.name) {
                    if let Visibility::File(id) = info.visibility {
                        if id != prev_segment_span.location.file_id {
                            return Err(Error::new(
                                format!("function `{}` is private", segment_name.name),
                                Some(segment_name.span),
                            ));
                        }
                    }
                    segment_name.res.set(info.id);
                    Ok((info.id, segment_name.span))
                } else {
                    Err(Error::new(
                        format!(
                            "function `{}` not found in type `{}`",
                            segment_name.name,
                            self.map.text_at(prev_segment_span)
                        ),
                        Some(segment_name.span),
                    ))
                }
            }
            Some(NameNode::Enum {
                variants, impl_fns, ..
            }) => {
                if let Some(variant_id) = variants.get(&segment_name.name) {
                    segment_name.res.set(*variant_id);
                    return Ok((*variant_id, segment_name.span));
                } else if let Some(info) = impl_fns.get(&segment_name.name) {
                    if let Visibility::File(id) = info.visibility {
                        if id != prev_segment_span.location.file_id {
                            return Err(Error::new(
                                format!("function `{}` is private", segment_name.name),
                                Some(segment_name.span),
                            ));
                        }
                    }
                    segment_name.res.set(info.id);
                    Ok((info.id, segment_name.span))
                } else {
                    Err(Error::new(
                        format!(
                            "function `{}` not found in type `{}`",
                            segment_name.name,
                            self.map.text_at(prev_segment_span)
                        ),
                        Some(segment_name.span),
                    ))
                }
            }
            Some(NameNode::Trait { trait_fns, .. }) => {
                if let Some(trait_fn_id) = trait_fns.get(&segment_name.name) {
                    segment_name.res.set(*trait_fn_id);
                    Ok((*trait_fn_id, segment_name.span))
                } else {
                    Err(Error::new(
                        format!(
                            "function `{}` not found in trait `{}`",
                            segment_name.name,
                            self.map.text_at(prev_segment_span)
                        ),
                        Some(segment_name.span),
                    ))
                }
            }
            Some(_) => unreachable!(),
            None => {
                if let Some(local_enums) = self.local_enum_id_map.last() {
                    if let Some(variants) = local_enums.get(&id) {
                        if let Some(variant_id) = variants.get(&segment_name.name) {
                            segment_name.res.set(*variant_id);
                            Ok((*variant_id, segment_name.span))
                        } else {
                            Err(Error::new(
                                format!(
                                    "no variant `{}` in enum `{}`",
                                    segment_name.name,
                                    self.map.text_at(prev_segment_span)
                                ),
                                Some(segment_name.span),
                            ))
                        }
                    } else {
                        Err(Error::new(
                            format!(
                                "`{}` is not a type or module",
                                self.map.text_at(prev_segment_span)
                            ),
                            Some(prev_segment_span),
                        ))
                    }
                } else {
                    Err(Error::new(
                        format!(
                            "`{}` is not a type or module",
                            self.map.text_at(prev_segment_span)
                        ),
                        Some(prev_segment_span),
                    ))
                }
            }
        }
    }

    fn walk_label(&mut self, label: &Label) -> Result<(), Error> {
        label.name.res.set(self.resolve_name(&label.name, true)?);
        Ok(())
    }

    fn resolve_name_with_index(&self, name: &Name) -> Result<(HirId, usize), Error> {
        for (index, frame) in self.stack.iter().enumerate().rev() {
            if let Some(id) = frame.get(&name.name) {
                return Ok((*id, index));
            }
        }
        Err(Error::new(
            format!("cannot find `{}` in this scope", name.name),
            Some(name.span),
        ))
    }

    fn resolve_name(&self, name: &Name, is_label: bool) -> Result<HirId, Error> {
        let (id, index) = self.resolve_name_with_index(name)?;
        if let Some(min) = self.min_local_search_index.last() {
            if index < *min {
                return Err(Error::new(
                    if is_label {
                        format!("no label `{}` in this fn item", name.name)
                    } else {
                        "cannot capture dynamic environment".to_string()
                    },
                    Some(name.span),
                ));
            }
        }
        Ok(id)
    }

    fn resolved(self) -> Resolved {
        Resolved
    }
}

#[derive(Debug)]
pub struct Resolved;

pub fn resolve(crates: &HashMap<String, Vec<Item>>, map: &FileMap) -> Result<Resolved, Error> {
    let mut tree = HashMap::new();

    for (name, items) in crates {
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
        local_enum_id_map: Vec::new(),
        stack: Vec::new(),
        locals: HashSet::new(),
        min_local_search_index: Vec::new(),
        map,
    };

    for items in crates.values() {
        walker.walk(items)?;
    }

    println!("resolved: {crates:#?}");

    Ok(walker.resolved())
}
