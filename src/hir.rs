use std::fmt::Debug;

use once_cell::unsync::OnceCell;

use crate::ast::{Name, Path};
use crate::span::Span;

macro_rules! id {
    ($Name:ident) => {
        #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
        pub struct $Name(usize);

        impl From<$Name> for usize {
            fn from(x: $Name) -> usize {
                x.0
            }
        }

        impl From<usize> for $Name {
            fn from(x: usize) -> Self {
                $Name(x)
            }
        }
    };
}

id!(FnId);
id!(ExternFnId);
id!(ExternTypeId);
id!(ExternImplFnId);
id!(ExternStaticId);
id!(StructId);
id!(StructImplFnId);
id!(EnumId);
id!(EnumVariantId);
id!(EnumImplFnId);
id!(TraitId);
id!(TraitFnId);
id!(ConstId);
id!(StaticId);

#[derive(Debug)]
pub struct HirId<T>(OnceCell<T>);

impl<T: Copy + Debug> HirId<T> {
    pub fn new() -> Self {
        HirId(OnceCell::new())
    }

    pub fn set(&self, id: T) {
        self.0.set(id).unwrap();
    }

    pub fn get(&self) -> T {
        *self.0.get().unwrap()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TypeId {
    Extern(ExternTypeId),
    Struct(StructId),
    Enum(EnumId),
}

#[derive(Debug, Clone, Copy)]
pub enum ValueId {
    Fn(FnId),
    ExternFn(ExternFnId),
    ExternImplFn(ExternTypeId, ExternImplFnId),
    StructImplFn(StructId, StructImplFnId),
    EnumImplFn(EnumId, EnumImplFnId),
    TraitFn(TraitId, TraitFnId),
    Const(ConstId),
    Static(StaticId),
}

#[derive(Debug, Clone, Copy)]
pub enum ImplFnId {
    Extern(ExternImplFnId),
    Struct(StructImplFnId),
    Enum(EnumImplFnId),
}

impl From<ImplFnId> for usize {
    fn from(id: ImplFnId) -> usize {
        match id {
            ImplFnId::Extern(id) => id.into(),
            ImplFnId::Struct(id) => id.into(),
            ImplFnId::Enum(id) => id.into(),
        }
    }
}

#[derive(Debug)]
pub struct Item;
