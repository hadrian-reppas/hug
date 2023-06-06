use std::collections::HashMap;

use crate::ast;
use crate::error::{Error, Note};
use crate::hir::HirId;
use crate::io::FileId;
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

#[derive(Debug)]
enum Visibility {
    Pub,
    File(FileId),
}

impl Visibility {
    fn is_pub(&self) -> bool {
        matches!(self, Visibility::Pub)
    }
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
}

#[derive(Debug)]
struct ImplFnInfo {
    visibility: Visibility,
    id: HirId,
}

struct NameTree {
    crates: HashMap<String, HirId>,
    ids: HashMap<HirId, NameNode>,
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

fn make_map(items: &[ast::Item]) -> Result<HashMap<String, NameNode>, Error> {
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
            ast::Item::Use { .. } => {}
            ast::Item::Struct { is_pub, name, .. } => {
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
            ast::Item::Enum {
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
            ast::Item::Mod {
                is_pub,
                name,
                items,
                ..
            } => {
                check_redef!(name);
                let items = make_map(&items[..])?;
                map.insert(
                    name.name.clone(),
                    NameNode::Mod {
                        visibility: visibility!(*is_pub, name.span),
                        items,
                        id: name.id,
                    },
                );
            }
            ast::Item::Extern { items, .. } => {
                for item in items {
                    match item {
                        ast::ExternItem::Fn {
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
                        ast::ExternItem::Type { is_pub, name, .. } => {
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
                        ast::ExternItem::Static { is_pub, name, .. } => {
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
            ast::Item::Trait {
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
                        ast::TraitItem::Required { signature, .. }
                        | ast::TraitItem::Provided { signature, .. } => {
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
            ast::Item::Fn {
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
            ast::Item::Impl { .. } => {}
            ast::Item::Const { is_pub, name, .. } => {
                check_redef!(name);
                map.insert(
                    name.name.clone(),
                    NameNode::Const {
                        visibility: visibility!(*is_pub, name.span),
                        id: name.id,
                    },
                );
            }
            ast::Item::Static { is_pub, name, .. } => {
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
        if let ast::Item::Impl {
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
                for ast::ImplFn {
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

/*
struct Resolver<'a> {
    global: NameTree,
    stack: Vec<HashMap<&'a str, NameNode>>,
}

fn resolve(
    items: Vec<ast::Item>,
    resolver: &mut Resolver,
    current_name: &str,
) -> Result<Vec<hir::Item>, Error> {
    let mut hir_items = Vec::new();

    for item in &items {
        match item {
            _ => todo!(),
        }
    }

    for item in items {
        match item {
            _ => todo!(),
        }
    }

    Ok(hir_items)
}
*/

#[derive(Debug)]
pub struct Lowered;

pub fn resolve(crates: HashMap<String, Vec<ast::Item>>) -> Result<Lowered, Error> {
    let mut map = HashMap::new();

    for (name, items) in &crates {
        map.insert(
            name.clone(),
            NameNode::Mod {
                visibility: Visibility::Pub,
                items: make_map(&items[..])?,
                id: HirId::new(),
            },
        );
    }

    println!("crates: {crates:#?}");
    println!("map: {map:#?}");

    /*
    let mut resolver = Resolver {
        global: NameTree(map),
        stack: Vec::new(),
    };

    let mut lowered_items = resolve(items, &mut resolver, "crate")?;
    for (name, items) in other_items {
        lowered_items.append(&mut resolve(items, &mut resolver, &name)?);
    }

    println!("lowered_items: {lowered_items:#?}");
    */
    todo!()
}
