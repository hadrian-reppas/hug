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

pub enum TypeId {
    Extern(ExternTypeId),
    Struct(StructId),
    Enum(EnumId),
}

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

#[derive(Debug)]
pub struct Item;
