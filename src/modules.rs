use std::path::{Path, PathBuf};

use crate::ast::{Item, Name, UnloadedItem};
use crate::error::{Error, Note};
use crate::io::FileMap;
use crate::span::Span;

pub fn collect(
    unloaded_items: Vec<UnloadedItem>,
    prefix: PathBuf,
    map: &mut FileMap,
) -> Result<Vec<Item>, Error> {
    let mut items = Vec::new();
    for item in unloaded_items {
        match Item::try_from(item) {
            Ok(item) => items.push(item),
            Err((is_pub, name, span)) => items.push(handle_mod(is_pub, name, span, &prefix, map)?),
        }
    }
    Ok(items)
}

fn handle_mod(
    is_pub: bool,
    name: Name,
    span: Span,
    prefix: &Path,
    map: &mut FileMap,
) -> Result<Item, Error> {
    if name.name == "std" {
        return Err(Error::Parse(
            "module name `std` is reserved".to_string(),
            name.span,
            vec![],
        ));
    }

    let mut file_path = prefix.to_path_buf();
    file_path.push(&name.name);
    file_path.set_extension("hug");
    let mut mod_path = prefix.to_path_buf();
    mod_path.push(&name.name);
    mod_path.push("mod.hug");

    if file_path.is_file() && mod_path.is_file() {
        Err(Error::Io(
            format!(
                "file for module `{}` found at both {:?} and {:?}",
                name.name, file_path, mod_path
            ),
            Some(name.span),
            vec![],
        ))
    } else if file_path.is_file() {
        let mut file_items = Vec::new();
        for item in map.parse(file_path)? {
            match Item::try_from(item) {
                Ok(item) => file_items.push(item),
                Err((_, _, span)) => {
                    return Err(Error::Io(
                        "mod items are only allowed in the main file and \"mod.hug\" files"
                            .to_string(),
                        Some(span),
                        vec![],
                    ))
                }
            }
        }
        Ok(Item::Mod {
            is_pub,
            name,
            items: file_items,
            span,
        })
    } else if mod_path.is_file() {
        let unloaded_items = map.parse(mod_path.clone())?;
        mod_path.pop();
        let mod_items = collect(unloaded_items, mod_path, map)?;
        Ok(Item::Mod {
            is_pub,
            name,
            items: mod_items,
            span,
        })
    } else {
        Err(Error::Io(
            format!("file not found for module `{}`", name.name),
            Some(name.span),
            vec![Note::new(
                format!(
                    "to create the module `{}`, create file {file_path:?} or {mod_path:?}",
                    name.name
                ),
                None,
            )],
        ))
    }
}

pub fn get_std(map: &mut FileMap) -> Result<Vec<Item>, Error> {
    let mut items = Vec::new();
    for item in map.parse_std(&["lib"])?.0 {
        match Item::try_from(item) {
            Ok(item) => items.push(item),
            Err((is_pub, name, span)) => items.push(handle_std_mod(is_pub, name, span, &[], map)?),
        }
    }
    Ok(items)
}

fn handle_std_mod(
    is_pub: bool,
    name: Name,
    span: Span,
    prefix: &[&str],
    map: &mut FileMap,
) -> Result<Item, Error> {
    let mut path = prefix.to_vec();
    path.push(&name.name);
    let (unloaded_items, is_mod) = map.parse_std(&path)?;

    let mut items = Vec::new();
    for unloaded_item in unloaded_items {
        match Item::try_from(unloaded_item) {
            Ok(item) => items.push(item),
            Err((is_pub, name, span)) => {
                if is_mod {
                    items.push(handle_std_mod(is_pub, name, span, &path, map)?)
                } else {
                    return Err(Error::Io(
                        "mod items are only allowed in the main file and \"mod.hug\" files"
                            .to_string(),
                        Some(span),
                        vec![],
                    ));
                }
            }
        }
    }

    Ok(Item::Mod {
        is_pub,
        name,
        items,
        span,
    })
}
