use ir::ir;
use norad::designspace;

fn to_ir_axis(axis: designspace::Axis) -> ir::Axis {
    ir::Axis {
        tag: axis.tag,
        min: axis.minimum.expect("Discrete axes not supported yet"),
        default: axis.default,
        max: axis.maximum.expect("Discrete axes not supported yet"),
        hidden: axis.hidden.unwrap_or(false),
    }
}

#[cfg(test)]
mod tests {
    use norad::designspace::DesignSpaceDocument;
    use std::path::Path;

    use crate::toir::to_ir_axis;
    use ir::ir;

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
            ds.axes
                .axis
                .into_iter()
                .map(to_ir_axis)
                .collect::<Vec<ir::Axis>>()
        );
    }
}
