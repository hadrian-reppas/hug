use std::fs;
use std::path::{Path, PathBuf};

use crate::error::Error;
use crate::span::Span;

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
}

struct FileInfo {
    code: String,
    path: PathBuf,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct FileId(usize);
