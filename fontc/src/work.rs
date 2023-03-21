//! Helps fontc manage workloads that span FE and BE.
//!
//! Basically enums that can be a FeWhatever or a BeWhatever.

use std::{collections::HashSet, fmt::Display};

use fontbe::{
    error::Error as BeError,
    orchestration::{AnyWorkId, BeWork, Context as BeContext},
};
use fontdrasil::orchestration::Access;
use fontir::{
    error::WorkError as FeError,
    orchestration::{Context as FeContext, IrWork, WorkId},
};

#[derive(Debug)]
pub enum AnyWorkError {
    Fe(FeError),
    Be(BeError),
}

impl From<BeError> for AnyWorkError {
    fn from(e: BeError) -> Self {
        AnyWorkError::Be(e)
    }
}

impl From<FeError> for AnyWorkError {
    fn from(e: FeError) -> Self {
        AnyWorkError::Fe(e)
    }
}

impl Display for AnyWorkError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnyWorkError::Be(e) => e.fmt(f),
            AnyWorkError::Fe(e) => e.fmt(f),
        }
    }
}

// Work of any type, FE, BE, ... some future pass, w/e
pub enum AnyWork {
    Fe(Box<IrWork>),
    Be(Box<BeWork>),
}

impl From<Box<IrWork>> for AnyWork {
    fn from(work: Box<IrWork>) -> Self {
        AnyWork::Fe(work)
    }
}

impl From<Box<BeWork>> for AnyWork {
    fn from(work: Box<BeWork>) -> Self {
        AnyWork::Be(work)
    }
}

impl AnyWork {
    pub fn exec(&self, context: AnyContext) -> Result<(), AnyWorkError> {
        match self {
            AnyWork::Be(work) => work.exec(context.unwrap_be()).map_err(|e| e.into()),
            AnyWork::Fe(work) => work.exec(context.unwrap_fe()).map_err(|e| e.into()),
        }
    }
}

pub enum AnyContext {
    Fe(FeContext),
    Be(BeContext),
}

pub enum ReadAccess {
    Dependencies,
    Custom(Access<AnyWorkId>),
}

impl ReadAccess {
    pub fn custom(func: impl Fn(&AnyWorkId) -> bool + Send + Sync + 'static) -> Self {
        Self::Custom(Access::custom(func))
    }
}

impl AnyContext {
    pub fn for_work(
        fe_root: &FeContext,
        be_root: &BeContext,
        work_id: &AnyWorkId,
        dependencies: HashSet<AnyWorkId>,
        read_access: ReadAccess,
        write_access: Access<AnyWorkId>,
    ) -> AnyContext {
        let read_access = match read_access {
            ReadAccess::Dependencies => Access::custom(move |id| dependencies.contains(id)),
            ReadAccess::Custom(access_fn) => access_fn,
        };
        match work_id {
            AnyWorkId::Be(..) => AnyContext::Be(be_root.copy_for_work(read_access, write_access)),
            AnyWorkId::Fe(..) => AnyContext::Fe(fe_root.copy_for_work(
                Access::custom(move |id: &WorkId| read_access.check(&AnyWorkId::Fe(id.clone()))),
                Access::custom(move |id: &WorkId| write_access.check(&AnyWorkId::Fe(id.clone()))),
            )),
        }
    }

    pub fn unwrap_be(&self) -> &BeContext {
        match self {
            AnyContext::Fe(..) => panic!("Not a BE context"),
            AnyContext::Be(ctx) => ctx,
        }
    }

    pub fn unwrap_fe(&self) -> &FeContext {
        match self {
            AnyContext::Fe(ctx) => ctx,
            AnyContext::Be(..) => panic!("Not a FE context"),
        }
    }
}
