//! Helps fontc manage workloads that span FE and BE.
//!
//! Basically enums that can be a FeWhatever or a BeWhatever.

use std::{collections::HashSet, fmt::Display, sync::Arc};

use fontbe::{
    error::Error as BeError,
    orchestration::{AnyWorkId, BeWork, Context as BeContext},
};
use fontir::{
    error::WorkError as FeError,
    orchestration::{Context as FeContext, IrWork},
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

impl AnyContext {
    pub fn for_work(
        fe_root: &FeContext,
        be_root: &BeContext,
        id: &AnyWorkId,
        dependencies: HashSet<AnyWorkId>,
        write_access: Arc<dyn Fn(&AnyWorkId) -> bool + Send + Sync>,
    ) -> AnyContext {
        match id {
            AnyWorkId::Be(..) => {
                AnyContext::Be(be_root.copy_for_work(Some(dependencies), write_access))
            }
            AnyWorkId::Fe(..) => AnyContext::Fe(
                fe_root.copy_for_work(
                    Some(
                        dependencies
                            .into_iter()
                            .map(|a| a.unwrap_fe().clone())
                            .collect(),
                    ),
                    Arc::new(move |id| (write_access)(&AnyWorkId::Fe(id.clone()))),
                ),
            ),
        }
    }
}

impl AnyContext {
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
