use crate::{
    resource::{AnyResource, ResourceIdentifier, ResourceRequest},
    AnyTask, CompilationContext, Dependencies, Error, Provider, Task,
};

use norad::designspace::DesignSpaceDocument;

struct UfoSourceProvider {
    // hi
}

impl Provider for UfoSourceProvider {
    fn provide_task(&self, for_resource: &ResourceIdentifier) -> Option<AnyTask> {
        if for_resource.matches_type::<DesignSpaceDocument>() {
            Some(LoadDesignSpace.into())
        } else {
            None
        }
    }
}

crate::resource!(DesignSpaceResource => DesignSpaceDocument);

struct LoadDesignSpace;

impl Task for LoadDesignSpace {
    fn dependencies(&self) -> Dependencies {
        Default::default()
    }

    fn exec(&self, ctx: &CompilationContext) -> Result<crate::resource::AnyResource, crate::Error> {
        DesignSpaceDocument::load(&ctx.args.source_path)
            .map_err(|inner| Error::CantReadFile {
                path: ctx.args.source_path.clone(),
                inner: inner.into(),
            })
            .map(AnyResource::new)
    }
}

struct StaticMetaFromDesignSpace;

impl Task for StaticMetaFromDesignSpace {
    fn dependencies(&self) -> Dependencies {
        Dependencies::Slice(&[DesignSpaceResource.identifier()])
    }

    fn exec(&self, ctx: &CompilationContext) -> Result<AnyResource, Error> {
        let dspace = ctx.resources.get(&DesignSpaceResource);
        todo!()
    }
}
