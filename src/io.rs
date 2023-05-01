use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use crate::ast::{Item, Name, UnloadedItem};
use crate::error::{Error, Note};
use crate::parse;
use crate::span::Span;

macro_rules! std_file {
    ($($path:expr),* $(,)?) => {
        (vec![$($path),*], (include_str!(concat!("../std", $("/", $path),*, ".hug")), false))
    };
}

macro_rules! std_mod {
    ($($path:expr),* $(,)?) => {
        (vec![$($path),*], (include_str!(concat!("../std", $("/", $path),*, "/mod.hug")), true))
    };
}

lazy_static::lazy_static! {
    static ref STD_MAP: HashMap<Vec<&'static str>, (&'static str, bool)> = HashMap::from([
        std_file!("lib"),
        std_file!("option"),
        std_file!("result"),
        std_mod!("collections"),
        std_file!("collections", "hash_map"),
        std_file!("fmt"),
        std_file!("slice"),
    ]);
}

pub struct FileMap(Vec<FileInfo>);

impl FileMap {
    pub fn new() -> Self {
        FileMap(Vec::new())
    }

    fn load(&mut self, path: PathBuf) -> Result<FileId, Error> {
        let code = fs::read_to_string(&path)
            .map_err(|_| Error::Io(format!("couldn't read file {:?}", path), None, vec![]))?;
        let id = FileId(self.0.len());
        self.0.push(FileInfo { code, path });
        Ok(id)
    }

    pub fn get_code(&self, id: FileId) -> &str {
        &self.0[id.0].code
    }

    pub fn get_path(&self, id: FileId) -> &Path {
        &self.0[id.0].path
    }

    pub fn text_at(&self, span: Span) -> &str {
        let code = self.get_code(span.location.file_id);
        &code[span.start..span.end]
    }

    fn parse(&mut self, path: PathBuf) -> Result<Vec<UnloadedItem>, Error> {
        let id = self.load(path)?;
        let code = self.get_code(id);
        parse::parse(code, id)
    }

    fn load_std(&mut self, path: &[&str]) -> Result<(FileId, bool), Error> {
        let mut path_buf = PathBuf::from("std");
        path_buf.extend(path);
        path_buf.set_extension("hug");
        let (code, is_mod) = if let Some((code, is_mod)) = STD_MAP.get(path) {
            (code.to_string(), *is_mod)
        } else {
            return Err(Error::Io(
                format!("no standard library file {path_buf:?}"),
                None,
                vec![],
            ));
        };
        let id = FileId(self.0.len());
        self.0.push(FileInfo {
            code,
            path: path_buf,
        });
        Ok((id, is_mod))
    }

    fn parse_std_file(&mut self, path: &[&str]) -> Result<(Vec<UnloadedItem>, bool), Error> {
        let (id, is_mod) = self.load_std(path)?;
        let code = self.get_code(id);
        Ok((parse::parse(code, id)?, is_mod))
    }

    pub fn parse_all(&mut self, path: PathBuf) -> Result<Vec<Item>, Error> {
        let mut prefix = path.clone();
        prefix.pop();
        let items = self.parse(path)?;
        self.collect(items, prefix)
    }

    pub fn parse_std(&mut self) -> Result<Vec<Item>, Error> {
        let mut items = Vec::new();
        for item in self.parse_std_file(&["lib"])?.0 {
            match Item::try_from(item) {
                Ok(item) => items.push(item),
                Err((is_pub, name, span)) => {
                    items.push(self.handle_std_mod(is_pub, name, span, &[])?)
                }
            }
        }
        Ok(items)
    }

    fn collect(
        &mut self,
        unloaded_items: Vec<UnloadedItem>,
        prefix: PathBuf,
    ) -> Result<Vec<Item>, Error> {
        let mut items = Vec::new();
        for item in unloaded_items {
            match Item::try_from(item) {
                Ok(item) => items.push(item),
                Err((is_pub, name, span)) => {
                    items.push(self.handle_mod(is_pub, name, span, &prefix)?)
                }
            }
        }
        Ok(items)
    }

    fn handle_mod(
        &mut self,
        is_pub: bool,
        name: Name,
        span: Span,
        prefix: &Path,
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
            for item in self.parse(file_path)? {
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
            let unloaded_items = self.parse(mod_path.clone())?;
            mod_path.pop();
            let mod_items = self.collect(unloaded_items, mod_path)?;
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

    fn handle_std_mod(
        &mut self,
        is_pub: bool,
        name: Name,
        span: Span,
        prefix: &[&str],
    ) -> Result<Item, Error> {
        let mut path = prefix.to_vec();
        path.push(&name.name);
        let (unloaded_items, is_mod) = self.parse_std_file(&path)?;

        let mut items = Vec::new();
        for unloaded_item in unloaded_items {
            match Item::try_from(unloaded_item) {
                Ok(item) => items.push(item),
                Err((is_pub, name, span)) => {
                    if is_mod {
                        items.push(self.handle_std_mod(is_pub, name, span, &path)?)
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
}

struct FileInfo {
    code: String,
    path: PathBuf,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct FileId(usize);
