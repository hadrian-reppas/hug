use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use crate::ast::UnloadedItem;
use crate::error::Error;
use crate::parse;
use crate::span::Span;

lazy_static::lazy_static! {
    static ref STD_MAP: HashMap<Vec<&'static str>, (&'static str, bool)> = HashMap::from([
        (vec!["lib"], (include_str!("../std/lib.hug"), false)),
        (vec!["option"], (include_str!("../std/option.hug"), false)),
        (vec!["result"], (include_str!("../std/result.hug"), false)),
        (vec!["collections"], (include_str!("../std/collections/mod.hug"), true)),
        (vec!["collections", "hash_map"], (include_str!("../std/collections/hash_map.hug"), false)),
        (vec!["fmt"], (include_str!("../std/fmt.hug"), false)),
    ]);
}

pub struct FileMap(Vec<FileInfo>);

impl FileMap {
    pub fn new() -> Self {
        FileMap(Vec::new())
    }

    pub fn load(&mut self, path: PathBuf) -> Result<FileId, Error> {
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

    pub fn parse(&mut self, path: PathBuf) -> Result<Vec<UnloadedItem>, Error> {
        let id = self.load(path)?;
        let code = self.get_code(id);
        parse::parse(code, id)
    }

    pub fn load_std(&mut self, path: &[&str]) -> Result<(FileId, bool), Error> {
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

    pub fn parse_std(&mut self, path: &[&str]) -> Result<(Vec<UnloadedItem>, bool), Error> {
        let (id, is_mod) = self.load_std(path)?;
        let code = self.get_code(id);
        Ok((parse::parse(code, id)?, is_mod))
    }
}

struct FileInfo {
    code: String,
    path: PathBuf,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct FileId(usize);
