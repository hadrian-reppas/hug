use std::collections::HashMap;

use crate::ast;
use crate::error::{Error, Note};
use crate::hir::HirId;
use crate::span::Span;

enum NameNode {
    Mod {
        is_pub: bool,
        items: HashMap<String, HirId>,
    },
    Fn {
        is_pub: bool,
        id: HirId,
    },
    ExternFn {
        is_pub: bool,
        id: HirId,
    },
    ExternType {
        is_pub: bool,
        id: HirId,
        impl_fns: HashMap<String, ImplFnInfo>,
    },
    ExternStatic {
        is_pub: bool,
        id: HirId,
    },
    Struct {
        is_pub: bool,
        id: HirId,
        impl_fns: HashMap<String, ImplFnInfo>,
    },
    Enum {
        is_pub: bool,
        id: HirId,
        variants: HashMap<String, HirId>,
        impl_fns: HashMap<String, ImplFnInfo>,
    },
    Trait {
        is_pub: bool,
        id: HirId,
        trait_fns: HashMap<String, HirId>,
    },
    Const {
        is_pub: bool,
        id: HirId,
    },
    Static {
        is_pub: bool,
        id: HirId,
    },
}

impl NameNode {
    fn insert_impl_fn_id(&mut self, name: String, is_pub: bool, id: HirId) {
        match self {
            NameNode::ExternType { impl_fns, .. }
            | NameNode::Struct { impl_fns, .. }
            | NameNode::Enum { impl_fns, .. } => {
                impl_fns.insert(name, ImplFnInfo { is_pub, id });
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
struct ImplFnInfo {
    is_pub: bool,
    id: HirId,
}

/*
struct ImplTypeInfo<'a> {
    type_is_pub: bool,
    fn_spans: HashMap<&'a String, Span>,
    constructor: &'static dyn Fn(usize) -> hir::ImplFnId,
}

fn extern_constructor(id: usize) -> hir::ImplFnId {
    hir::ImplFnId::Extern(id.into())
}

fn struct_constructor(id: usize) -> hir::ImplFnId {
    hir::ImplFnId::Struct(id.into())
}

fn enum_constructor(id: usize) -> hir::ImplFnId {
    hir::ImplFnId::Enum(id.into())
}

fn next_id<T: From<usize>>() -> T {
    use std::sync::atomic::{AtomicUsize, Ordering};
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    COUNTER.fetch_add(1, Ordering::Relaxed).into()
}

#[derive(Debug)]
struct NameTree(HashMap<String, NameNode>);

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
            ast::Item::Struct {
                is_pub, name, id, ..
            } => {
                check_redef!(name);
                let new_id = next_id();
                id.set(new_id);
                map.insert(
                    name.name.clone(),
                    NameNode::Struct {
                        is_pub: *is_pub,
                        id: new_id,
                        impl_fns: HashMap::new(),
                    },
                );
                impl_spans.insert(
                    &name.name,
                    ImplTypeInfo {
                        type_is_pub: *is_pub,
                        fn_spans: HashMap::new(),
                        constructor: &struct_constructor,
                    },
                );
            }
            ast::Item::Enum {
                is_pub,
                name,
                items,
                id,
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
                    let new_id = next_id();
                    variant_spans.insert(&item.name.name, item.name.span);
                    item.id.set(new_id);
                    variants.insert(item.name.name.clone(), new_id);
                }

                let new_id = next_id();
                id.set(new_id);
                map.insert(
                    name.name.clone(),
                    NameNode::Enum {
                        is_pub: *is_pub,
                        id: new_id,
                        variants,
                        impl_fns: HashMap::new(),
                    },
                );
                impl_spans.insert(
                    &name.name,
                    ImplTypeInfo {
                        type_is_pub: *is_pub,
                        fn_spans: HashMap::new(),
                        constructor: &enum_constructor,
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
                        is_pub: *is_pub,
                        items,
                    },
                );
            }
            ast::Item::Extern { items, .. } => {
                for item in items {
                    match item {
                        ast::ExternItem::Fn {
                            is_pub,
                            signature,
                            id,
                            ..
                        } => {
                            check_redef!(signature.name);
                            let new_id = next_id();
                            id.set(new_id);
                            map.insert(
                                signature.name.name.clone(),
                                NameNode::ExternFn {
                                    is_pub: *is_pub,
                                    id: new_id,
                                },
                            );
                        }
                        ast::ExternItem::Type {
                            is_pub, name, id, ..
                        } => {
                            check_redef!(name);
                            let new_id = next_id();
                            id.set(new_id);
                            map.insert(
                                name.name.clone(),
                                NameNode::ExternType {
                                    is_pub: *is_pub,
                                    id: new_id,
                                    impl_fns: HashMap::new(),
                                },
                            );
                            impl_spans.insert(
                                &name.name,
                                ImplTypeInfo {
                                    type_is_pub: *is_pub,
                                    fn_spans: HashMap::new(),
                                    constructor: &extern_constructor,
                                },
                            );
                        }
                        ast::ExternItem::Static {
                            is_pub, name, id, ..
                        } => {
                            check_redef!(name);
                            let new_id = next_id();
                            id.set(new_id);
                            map.insert(
                                name.name.clone(),
                                NameNode::ExternStatic {
                                    is_pub: *is_pub,
                                    id: new_id,
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
                id,
                ..
            } => {
                check_redef!(name);

                let mut fn_spans = HashMap::new();
                let mut trait_fns = HashMap::new();
                for item in items {
                    match item {
                        ast::TraitItem::Required { signature, id, .. }
                        | ast::TraitItem::Provided { signature, id, .. } => {
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
                            let new_id = next_id();
                            id.set(new_id);
                            fn_spans.insert(&signature.name.name, signature.name.span);
                            trait_fns.insert(signature.name.name.clone(), new_id);
                        }
                    }
                }

                let new_id = next_id();
                id.set(new_id);
                map.insert(
                    name.name.clone(),
                    NameNode::Trait {
                        is_pub: *is_pub,
                        id: new_id,
                        trait_fns,
                    },
                );
            }
            ast::Item::Fn {
                is_pub,
                signature,
                id,
                ..
            } => {
                check_redef!(signature.name);
                let new_id = next_id();
                id.set(new_id);
                map.insert(
                    signature.name.name.clone(),
                    NameNode::Fn {
                        is_pub: *is_pub,
                        id: new_id,
                    },
                );
            }
            ast::Item::Impl { .. } => {}
            ast::Item::Const {
                is_pub, name, id, ..
            } => {
                check_redef!(name);
                let new_id = next_id();
                id.set(new_id);
                map.insert(
                    name.name.clone(),
                    NameNode::Const {
                        is_pub: *is_pub,
                        id: new_id,
                    },
                );
            }
            ast::Item::Static {
                is_pub, name, id, ..
            } => {
                check_redef!(name);
                let new_id = next_id();
                id.set(new_id);
                map.insert(
                    name.name.clone(),
                    NameNode::Static {
                        is_pub: *is_pub,
                        id: new_id,
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
                type_is_pub,
                fn_spans,
                constructor,
            }) = impl_spans.get_mut(&name.name)
            {
                for ast::ImplFn {
                    is_pub,
                    signature,
                    id,
                    ..
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
                    if *is_pub && !*type_is_pub {
                        return Err(Error::new(
                            format!(
                                "function `{}` is declared pub on non-pub type `{}`",
                                signature.name.name, name.name
                            ),
                            Some(signature.name.span),
                        ));
                    }
                    let new_id = constructor(next_id());
                    id.set(new_id);
                    fn_spans.insert(&signature.name.name, signature.name.span);
                    map.get_mut(&name.name).unwrap().insert_impl_fn_id(
                        signature.name.name.clone(),
                        *is_pub,
                        new_id.into(),
                    );
                }
            } else {
                return Err(Error::new(
                    format!("no type `{}` in this file", name.name),
                    Some(name.span),
                ));
            }
        }
    }

    Ok(map)
}

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

pub fn resolve(
    items: Vec<ast::Item>,
    other_items: HashMap<String, Vec<ast::Item>>,
) -> Result<Lowered, Error> {
    /*
    let mut map = HashMap::new();

    map.insert(
        "crate".to_string(),
        NameNode::Mod {
            is_pub: true,
            items: make_map(&items[..])?,
        },
    );

    for (name, items) in &other_items {
        map.insert(
            name.clone(),
            NameNode::Mod {
                is_pub: true,
                items: make_map(&items[..])?,
            },
        );
    }

    println!("map: {map:#?}");
    println!("items: {items:#?}");

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
