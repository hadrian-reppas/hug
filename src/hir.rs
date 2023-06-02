use std::fmt::{self, Debug};
use std::num::NonZeroUsize;

use once_cell::unsync::OnceCell;

use crate::ast::{Name, Path};
use crate::span::Span;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct HirId(NonZeroUsize);

impl HirId {
    pub fn new() -> Self {
        use std::sync::atomic::{AtomicUsize, Ordering};

        static COUNTER: AtomicUsize = AtomicUsize::new(1);
        let id = COUNTER.fetch_add(1, Ordering::Relaxed);

        HirId(NonZeroUsize::new(id).unwrap())
    }
}

impl Debug for HirId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

pub struct IdCell(OnceCell<HirId>);

impl IdCell {
    pub fn uninit() -> Self {
        IdCell(OnceCell::new())
    }

    pub fn set(&self, id: HirId) {
        self.0.set(id).unwrap();
    }

    pub fn get(&self) -> HirId {
        *self.0.get().unwrap()
    }
}

impl Debug for IdCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(&id) = self.0.get() {
            write!(f, "{id:?}")
        } else {
            write!(f, "Uninit")
        }
    }
}
