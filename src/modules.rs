use std::path::PathBuf;

use crate::ast::Item;
use crate::error::Error;
use crate::io::FileMap;

#[derive(Debug)]
pub enum FileTree {
    File {
        name: String,
        items: Vec<Item>,
        is_main: bool,
    },
    Dir {
        name: String,
        items: Vec<Item>,
        files: Vec<FileTree>,
    },
}

pub fn collect(items: Vec<Item>, name: String, prefix: PathBuf, map: &mut FileMap) -> Result<Vec<FileTree>, Error> {
    let mut tree = make_file_tree(&items, prefix, map)?;
    tree.push(FileTree::File { name, items, is_main: true });
    Ok(tree)
}

fn make_file_tree(
    items: &[Item],
    prefix: PathBuf,
    map: &mut FileMap,
) -> Result<Vec<FileTree>, Error> {
    let mut tree = Vec::new();
    for item in items {
        if let Item::Mod { name, .. } = item {
            let mut file = prefix.clone();
            file.push(&name.name);
            file.set_extension("hug");
            let mut mod_file = prefix.clone();
            mod_file.push(&name.name);
            mod_file.push("mod.hug");

            if file.is_file() && mod_file.is_file() {
                return Err(Error::Io(
                    format!(
                        "file for module `{}` found at both {:?} and {:?}",
                        name.name, file, mod_file
                    ),
                    Some(name.span),
                    vec![],
                ));
            } else if !file.is_file() && !mod_file.is_file() {
                return Err(Error::Io(
                    format!("file not found for module `{}`", name.name),
                    Some(name.span),
                    vec![],
                ));
            } else if map.contains(&file) || map.contains(&mod_file) {
                continue;
            }


            if file.is_file() {
                tree.push(FileTree::File {
                    name: name.name.clone(),
                    items: map.get_unit(file)?,
                    is_main: false,
                });
            } else {
                let items = map.get_unit(mod_file)?;
                file.set_extension("");
                let files = make_file_tree(&items, file, map)?;
                tree.push(FileTree::Dir {
                    name: name.name.clone(),
                    items,
                    files,
                });
            }
        }
    }
    Ok(tree)
}
