use fontir::ir;
use norad::designspace::{self, DesignSpaceDocument};

use crate::error::Error;

pub fn designspace_to_ir(designspace: DesignSpaceDocument) -> Result<Vec<ir::Axis>, Error> {
    // Truly we have done something amazing here today
    let ir_axes: Vec<ir::Axis> = designspace.axes.into_iter().map(to_ir_axis).collect();

    // Someday we will return something useful! But ... not today.
    Ok(ir_axes)
}

fn to_ir_axis(axis: designspace::Axis) -> ir::Axis {
    ir::Axis {
        tag: axis.tag,
        min: axis.minimum.expect("Discrete axes not supported yet"),
        default: axis.default,
        max: axis.maximum.expect("Discrete axes not supported yet"),
        hidden: axis.hidden,
    }
}

#[cfg(test)]
mod tests {
    use norad::designspace::DesignSpaceDocument;
    use std::path::Path;

    use crate::toir::designspace_to_ir;
    use fontir::ir;

    #[test]
    fn simple_wght_variable() {
        let ds = DesignSpaceDocument::load(Path::new("testdata/wght_var.designspace")).unwrap();
        assert_eq!(
            vec![ir::Axis {
                tag: "wght".to_string(),
                min: 400.,
                default: 400.,
                max: 700.,
                hidden: false
            }],
            designspace_to_ir(ds).unwrap()
        );
    }
}
