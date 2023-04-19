use std::collections::HashMap;

use crate::error::{Error, Note};
use crate::span::Span;
use crate::{ast, hir};

#[derive(Debug)]
enum NameNode {
    Mod {
        is_pub: bool,
        items: HashMap<String, NameNode>,
    },
    Fn {
        is_pub: bool,
        id: hir::FnId,
    },
    ExternFn {
        is_pub: bool,
        id: hir::ExternFnId,
    },
    ExternType {
        is_pub: bool,
        id: hir::ExternTypeId,
        impl_fns: HashMap<String, ImplFnInfo<hir::ExternImplFnId>>,
    },
    ExternStatic {
        is_pub: bool,
        id: hir::ExternStaticId,
    },
    Struct {
        is_pub: bool,
        id: hir::StructId,
        impl_fns: HashMap<String, ImplFnInfo<hir::StructImplFnId>>,
    },
    Enum {
        is_pub: bool,
        id: hir::EnumId,
        variants: HashMap<String, hir::EnumVariantId>,
        impl_fns: HashMap<String, ImplFnInfo<hir::EnumImplFnId>>,
    },
    Trait {
        is_pub: bool,
        id: hir::TraitId,
        trait_fns: HashMap<String, hir::TraitFnId>,
    },
    Const {
        is_pub: bool,
        id: hir::ConstId,
    },
    Static {
        is_pub: bool,
        id: hir::StaticId,
    },
}

impl NameNode {
    fn insert_impl_fn_id(&mut self, name: String, is_pub: bool, id: usize) {
        match self {
            NameNode::ExternType { impl_fns, .. } => {
                impl_fns.insert(name, ImplFnInfo::new(is_pub, id));
            }
            NameNode::Struct { impl_fns, .. } => {
                impl_fns.insert(name, ImplFnInfo::new(is_pub, id));
            }
            NameNode::Enum { impl_fns, .. } => {
                impl_fns.insert(name, ImplFnInfo::new(is_pub, id));
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
struct ImplFnInfo<T> {
    is_pub: bool,
    id: T,
}

impl<T: From<usize>> ImplFnInfo<T> {
    fn new(is_pub: bool, id: usize) -> Self {
        ImplFnInfo {
            is_pub,
            id: id.into(),
        }
    }
}

struct ImplTypeInfo<'a> {
    type_is_pub: bool,
    fn_spans: HashMap<&'a String, Span>,
    counter: Counter,
}

struct Counter(usize);

impl Counter {
    fn new() -> Self {
        Counter(0)
    }

    fn next<T: From<usize>>(&mut self) -> T {
        self.0 += 1;
        (self.0 - 1).into()
    }
}

#[derive(Debug)]
struct NameTree(HashMap<String, NameNode>);

fn make_tree(items: &[ast::Item]) -> Result<NameTree, Error> {
    let mut map = HashMap::new();
    let mut prev_spans = HashMap::new();
    let mut counter = Counter::new();
    let mut impl_spans = HashMap::new();

    macro_rules! check_redef {
        ($name:expr) => {{
            if let Some(&prev_span) = prev_spans.get(&$name.name) {
                return Err(Error::Name(
                    format!("the name `{}` is defined multiple times", $name.name),
                    $name.span,
                    vec![Note::new(
                        format!("previous definition of `{}` is here", $name.name),
                        Some(prev_span),
                    )],
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
                        is_pub: *is_pub,
                        id: counter.next(),
                        impl_fns: HashMap::new(),
                    },
                );
                impl_spans.insert(
                    &name.name,
                    ImplTypeInfo {
                        type_is_pub: *is_pub,
                        fn_spans: HashMap::new(),
                        counter: Counter::new(),
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

                let mut variant_counter = Counter::new();
                let mut variant_spans = HashMap::new();
                let mut variants = HashMap::new();
                for item in items {
                    if let Some(span) = variant_spans.get(&item.name.name) {
                        return Err(Error::Name(
                            format!(
                                "the name `{}` is defined multiple times in this enum",
                                item.name.name
                            ),
                            item.name.span,
                            vec![Note::new(
                                format!("previous definition of `{}` is here", item.name.name),
                                Some(*span),
                            )],
                        ));
                    }
                    variant_spans.insert(&item.name.name, item.name.span);
                    variants.insert(item.name.name.clone(), variant_counter.next());
                }

                map.insert(
                    name.name.clone(),
                    NameNode::Enum {
                        is_pub: *is_pub,
                        id: counter.next(),
                        variants,
                        impl_fns: HashMap::new(),
                    },
                );
                impl_spans.insert(
                    &name.name,
                    ImplTypeInfo {
                        type_is_pub: *is_pub,
                        fn_spans: HashMap::new(),
                        counter: Counter::new(),
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
                let NameTree(items) = make_tree(&items[..])?;
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
                            is_pub, signature, ..
                        } => {
                            check_redef!(signature.name);
                            map.insert(
                                signature.name.name.clone(),
                                NameNode::ExternFn {
                                    is_pub: *is_pub,
                                    id: counter.next(),
                                },
                            );
                        }
                        ast::ExternItem::Type { is_pub, name, .. } => {
                            check_redef!(name);
                            map.insert(
                                name.name.clone(),
                                NameNode::ExternType {
                                    is_pub: *is_pub,
                                    id: counter.next(),
                                    impl_fns: HashMap::new(),
                                },
                            );
                            impl_spans.insert(
                                &name.name,
                                ImplTypeInfo {
                                    type_is_pub: *is_pub,
                                    fn_spans: HashMap::new(),
                                    counter: Counter::new(),
                                },
                            );
                        }
                        ast::ExternItem::Static { is_pub, name, .. } => {
                            check_redef!(name);
                            map.insert(
                                name.name.clone(),
                                NameNode::Static {
                                    is_pub: *is_pub,
                                    id: counter.next(),
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

                let mut fn_counter = Counter::new();
                let mut fn_spans = HashMap::new();
                let mut trait_fns = HashMap::new();
                for item in items {
                    match item {
                        ast::TraitItem::Required { signature, .. }
                        | ast::TraitItem::Provided { signature, .. } => {
                            if let Some(span) = fn_spans.get(&signature.name.name) {
                                return Err(Error::Name(
                                    format!(
                                        "the function `{}` is defined multiple times in this trait",
                                        signature.name.name
                                    ),
                                    signature.name.span,
                                    vec![Note::new(
                                        format!(
                                            "previous definition of `{}` is here",
                                            signature.name.name
                                        ),
                                        Some(*span),
                                    )],
                                ));
                            }
                            fn_spans.insert(&signature.name.name, signature.name.span);
                            trait_fns.insert(signature.name.name.clone(), fn_counter.next());
                        }
                    }
                }

                map.insert(
                    name.name.clone(),
                    NameNode::Trait {
                        is_pub: *is_pub,
                        id: counter.next(),
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
                        is_pub: *is_pub,
                        id: counter.next(),
                    },
                );
            }
            ast::Item::Impl { .. } => {}
            ast::Item::Const { is_pub, name, .. } => {
                check_redef!(name);
                map.insert(
                    name.name.clone(),
                    NameNode::Const {
                        is_pub: *is_pub,
                        id: counter.next(),
                    },
                );
            }
            ast::Item::Static { is_pub, name, .. } => {
                check_redef!(name);
                map.insert(
                    name.name.clone(),
                    NameNode::Static {
                        is_pub: *is_pub,
                        id: counter.next(),
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
                counter,
            }) = impl_spans.get_mut(&name.name)
            {
                for ast::ImplFn {
                    is_pub, signature, ..
                } in fns
                {
                    if let Some(span) = fn_spans.get(&signature.name.name) {
                        return Err(Error::Name(
                            format!("duplicate definitions with name `{}`", signature.name.name),
                            signature.name.span,
                            vec![Note::new(
                                format!("previous definition of `{}` is here", signature.name.name),
                                Some(*span),
                            )],
                        ));
                    } else {
                        if *is_pub && !*type_is_pub {
                            return Err(Error::Name(
                                format!(
                                    "function `{}` is declared pub on non-pub type `{}`",
                                    signature.name.name, name.name
                                ),
                                signature.name.span,
                                vec![],
                            ));
                        }
                        fn_spans.insert(&signature.name.name, signature.name.span);
                        map.get_mut(&name.name).unwrap().insert_impl_fn_id(
                            signature.name.name.clone(),
                            *is_pub,
                            counter.next(),
                        );
                    }
                }
            } else {
                return Err(Error::Name(
                    format!("no type `{}` in this file", name.name),
                    name.span,
                    vec![],
                ));
            }
        }
    }

    Ok(NameTree(map))
}

#[derive(Debug)]
pub struct Lowered {
    items: Vec<hir::Item>,
    info: (),
}

pub fn lower(
    items: Vec<ast::Item>,
    other_items: HashMap<String, Vec<ast::Item>>,
) -> Result<Lowered, Error> {
    let tree = make_tree(&items[..])?;
    println!("tree: {tree:#?}");
    todo!()
}
