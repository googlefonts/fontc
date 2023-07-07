//! Helps fontc manage workloads that span FE and BE.
//!
//! Basically enums that can be a FeWhatever or a BeWhatever.

use std::fmt::Display;

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
#[derive(Debug)]
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
    pub fn id(&self) -> AnyWorkId {
        match self {
            AnyWork::Be(work) => work.id(),
            AnyWork::Fe(work) => work.id().into(),
        }
    }

    pub fn read_access(&self) -> AnyAccess {
        match self {
            AnyWork::Be(work) => work.read_access().into(),
            AnyWork::Fe(work) => work.read_access().into(),
        }
    }

    pub fn write_access(&self) -> AnyAccess {
        match self {
            AnyWork::Be(work) => work.write_access().into(),
            AnyWork::Fe(work) => work.write_access().into(),
        }
    }

    pub fn also_completes(&self) -> Vec<AnyWorkId> {
        match self {
            AnyWork::Be(work) => work.also_completes().into_iter().collect(),
            AnyWork::Fe(work) => work
                .also_completes()
                .into_iter()
                .map(|id| id.into())
                .collect(),
        }
    }

    pub fn exec(&self, context: AnyContext) -> Result<(), AnyWorkError> {
        match self {
            AnyWork::Be(work) => work.exec(context.unwrap_be()).map_err(|e| e.into()),
            AnyWork::Fe(work) => work.exec(context.unwrap_fe()).map_err(|e| e.into()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum AnyAccess {
    Be(Access<AnyWorkId>),
    Fe(Access<WorkId>),
}

impl From<Access<AnyWorkId>> for AnyAccess {
    fn from(value: Access<AnyWorkId>) -> Self {
        AnyAccess::Be(value)
    }
}

impl From<Access<WorkId>> for AnyAccess {
    fn from(value: Access<WorkId>) -> Self {
        AnyAccess::Fe(value)
    }
}

impl AnyAccess {
    pub fn check(&self, id: &AnyWorkId) -> bool {
        match self {
            AnyAccess::Be(access) => access.check(id),
            AnyAccess::Fe(access) => {
                let AnyWorkId::Fe(id) = id else {
                    return false;
                };
                access.check(id)
            }
        }
    }

    pub fn unwrap_be(&self) -> Access<AnyWorkId> {
        match self {
            AnyAccess::Fe(..) => panic!("Not BE access"),
            AnyAccess::Be(access) => access.clone(),
        }
    }

    pub fn unwrap_fe(&self) -> Access<WorkId> {
        match self {
            AnyAccess::Fe(access) => access.clone(),
            AnyAccess::Be(..) => panic!("Not FE access"),
        }
    }
}

pub enum AnyContext {
    Fe(FeContext),
    Be(BeContext),
}

impl AnyContext {
    pub fn for_work(
        fe_root: &FeContext,
        be_root: &BeContext,
        work_id: &AnyWorkId,
        read_access: AnyAccess,
        write_access: AnyAccess,
    ) -> AnyContext {
        match work_id {
            AnyWorkId::Be(..) => AnyContext::Be(
                be_root.copy_for_work(read_access.unwrap_be(), write_access.unwrap_be()),
            ),
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
