use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use crate::ast::{Annotation, Item, Name, UnloadedItem};
use crate::error::Error;
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

pub struct FileMap {
    files: Vec<FileInfo>,
    crates: Vec<CrateInfo>,
}

impl FileMap {
    pub fn new() -> Self {
        FileMap {
            files: Vec::new(),
            crates: vec![CrateInfo {
                name: "std".to_string(),
            }],
        }
    }

    fn add_crate(&mut self, crate_name: String) -> Result<CrateId, Error> {
        for CrateInfo { name } in &self.crates {
            if name == &crate_name {
                return Err(Error::new(format!("multiple crates named `{name}`"), None));
            }
        }
        let id = CrateId(self.crates.len());
        self.crates.push(CrateInfo { name: crate_name });
        Ok(id)
    }

    fn load(&mut self, path: PathBuf, crate_id: CrateId) -> Result<FileId, Error> {
        let code = fs::read_to_string(&path)
            .map_err(|_| Error::new(format!("couldn't read file {path:?}"), None))?;
        let id = FileId(self.files.len());
        self.files.push(FileInfo {
            code,
            path,
            crate_id,
        });
        Ok(id)
    }

    pub fn get_code(&self, id: FileId) -> &str {
        &self.files[id.0].code
    }

    pub fn get_path(&self, id: FileId) -> &Path {
        &self.files[id.0].path
    }

    pub fn get_crate_id(&self, id: FileId) -> CrateId {
        self.files[id.0].crate_id
    }

    pub fn text_at(&self, span: Span) -> &str {
        let code = self.get_code(span.file_id);
        &code[span.start..span.end]
    }

    fn parse(&mut self, path: PathBuf, crate_id: CrateId) -> Result<Vec<UnloadedItem>, Error> {
        let id = self.load(path, crate_id)?;
        let code = self.get_code(id);
        parse::parse(code, id, crate_id)
    }

    fn load_std(&mut self, path: &[&str]) -> Result<(FileId, bool), Error> {
        let mut path_buf = PathBuf::from("std");
        path_buf.extend(path);
        path_buf.set_extension("hug");
        let (code, is_mod) = if let Some((code, is_mod)) = STD_MAP.get(path) {
            (code.to_string(), *is_mod)
        } else {
            return Err(Error::new(
                format!("no standard library file {path_buf:?}"),
                None,
            ));
        };
        let id = FileId(self.files.len());
        self.files.push(FileInfo {
            code,
            path: path_buf,
            crate_id: CrateId::std_id(),
        });
        Ok((id, is_mod))
    }

    fn parse_std_file(&mut self, path: &[&str]) -> Result<(Vec<UnloadedItem>, bool), Error> {
        let (id, is_mod) = self.load_std(path)?;
        let code = self.get_code(id);
        Ok((parse::parse(code, id, CrateId::std_id())?, is_mod))
    }

    pub fn parse_all(&mut self, path: PathBuf, crate_name: String) -> Result<Vec<Item>, Error> {
        let mut prefix = path.clone();
        prefix.pop();
        let crate_id = self.add_crate(crate_name)?;
        let items = self.parse(path, crate_id)?;
        self.collect(items, prefix, crate_id)
    }

    pub fn parse_std(&mut self) -> Result<Vec<Item>, Error> {
        let mut items = Vec::new();
        for item in self.parse_std_file(&["lib"])?.0 {
            match Item::try_from(item) {
                Ok(item) => items.push(item),
                Err((annotations, is_pub, name, span)) => {
                    items.push(self.handle_std_mod(annotations, is_pub, name, span, &[])?)
                }
            }
        }
        Ok(items)
    }

    fn collect(
        &mut self,
        unloaded_items: Vec<UnloadedItem>,
        prefix: PathBuf,
        crate_id: CrateId,
    ) -> Result<Vec<Item>, Error> {
        let mut items = Vec::new();
        for item in unloaded_items {
            match Item::try_from(item) {
                Ok(item) => items.push(item),
                Err((annotations, is_pub, name, span)) => items.push(self.handle_mod(
                    annotations,
                    is_pub,
                    name,
                    span,
                    &prefix,
                    crate_id,
                )?),
            }
        }
        Ok(items)
    }

    fn handle_mod(
        &mut self,
        annotations: Vec<Annotation>,
        is_pub: bool,
        name: Name,
        span: Span,
        prefix: &Path,
        crate_id: CrateId,
    ) -> Result<Item, Error> {
        if name.name == "std" {
            return Err(Error::new("module name `std` is reserved", Some(name.span)));
        }

        let mut file_path = prefix.to_path_buf();
        file_path.push(&name.name);
        file_path.set_extension("hug");
        let mut mod_path = prefix.to_path_buf();
        mod_path.push(&name.name);
        mod_path.push("mod.hug");

        if file_path.is_file() && mod_path.is_file() {
            Err(Error::new(
                format!(
                    "file for module `{}` found at both {file_path:?} and {mod_path:?}",
                    name.name,
                ),
                Some(name.span),
            ))
        } else if file_path.is_file() {
            let mut file_items = Vec::new();
            for item in self.parse(file_path, crate_id)? {
                match Item::try_from(item) {
                    Ok(item) => file_items.push(item),
                    Err((_, _, _, span)) => {
                        return Err(Error::new(
                            "mod items are only allowed in the main file and \"mod.hug\" files",
                            Some(span),
                        ))
                    }
                }
            }
            Ok(Item::Mod {
                annotations,
                is_pub,
                name,
                items: file_items,
                span,
            })
        } else if mod_path.is_file() {
            let unloaded_items = self.parse(mod_path.clone(), crate_id)?;
            mod_path.pop();
            let mod_items = self.collect(unloaded_items, mod_path, crate_id)?;
            Ok(Item::Mod {
                annotations,
                is_pub,
                name,
                items: mod_items,
                span,
            })
        } else {
            Err(Error::new(
                format!("file not found for module `{}`", name.name),
                Some(name.span),
            )
            .note(
                format!(
                    "to create the module `{}`, create file {file_path:?} or {mod_path:?}",
                    name.name
                ),
                None,
            ))
        }
    }

    fn handle_std_mod(
        &mut self,
        annotations: Vec<Annotation>,
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
                Err((annotations, is_pub, name, span)) => {
                    if is_mod {
                        items.push(self.handle_std_mod(annotations, is_pub, name, span, &path)?)
                    } else {
                        return Err(Error::new(
                            "mod items are only allowed in the main file and \"mod.hug\" files",
                            Some(span),
                        ));
                    }
                }
            }
        }

        Ok(Item::Mod {
            annotations,
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
    crate_id: CrateId,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct FileId(usize);

impl FileId {
    pub const fn first() -> Self {
        FileId(0)
    }
}

struct CrateInfo {
    name: String,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct CrateId(usize);

impl CrateId {
    pub const fn std_id() -> Self {
        CrateId(0)
    }
}
