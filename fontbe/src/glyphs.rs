//! 'glyf' Glyph binary compilation

use std::collections::{BTreeSet, HashMap, HashSet};

use fontdrasil::{orchestration::Work, types::GlyphName};
use fontir::{coords::NormalizedLocation, ir};
use kurbo::{cubics_to_quadratic_splines, BezPath, CubicBez, PathEl};
use log::{error, trace, warn};

use write_fonts::tables::glyf::SimpleGlyph;

use crate::{
    error::{Error, GlyphProblem},
    orchestration::{BeWork, Context},
};

struct GlyphWork {
    glyph_name: GlyphName,
}

pub fn create_glyph_work(glyph_name: GlyphName) -> Box<BeWork> {
    Box::new(GlyphWork { glyph_name })
}

impl Work<Context, Error> for GlyphWork {
    fn exec(&self, context: &Context) -> Result<(), Error> {
        trace!("BE glyph work for {}", self.glyph_name);

        let static_metadata = context.ir.get_static_metadata();
        let var_model = &static_metadata.variation_model;
        let default_location = var_model.default_location();
        let ir_glyph = &*context.ir.get_glyph_ir(&self.glyph_name);
        let glyph: CheckedGlyph = ir_glyph.try_into()?;

        // Hopefully in time https://github.com/harfbuzz/boring-expansion-spec means we can drop this
        let glyph = cubics_to_quadratics(glyph);

        // TODO refine (submodel) var model if glyph locations is a subset of var model locations

        match glyph {
            CheckedGlyph::Composite { name, .. } => {
                error!("setting no glyph for {name}; composites not implemented yet");
            }
            CheckedGlyph::Contour { name, contours } => {
                // Draw the default outline of our simple glyph
                let Some(path) = contours.get(default_location) else {
                    return Err(Error::GlyphError(ir_glyph.name.clone(), GlyphProblem::MissingDefault));
                };
                let base_glyph = SimpleGlyph::from_kurbo(path).map_err(|e| Error::KurboError {
                    glyph_name: self.glyph_name.clone(),
                    kurbo_problem: e,
                    context: path.to_svg(),
                })?;
                context.set_glyph(name, base_glyph.into());
            }
        }

        Ok(())
    }
}

fn cubics_to_quadratics(glyph: CheckedGlyph) -> CheckedGlyph {
    let CheckedGlyph::Contour { name, contours } = glyph else {
        return glyph;  // nop for composite
    };

    trace!("Convert '{name}' to quadratic");

    // put all the loc + path iters into a vec
    let mut loc_iters: Vec<_> = contours
        .iter()
        .map(|(loc, path)| (loc, path.iter()))
        .collect();
    let mut new_contours = HashMap::<NormalizedLocation, BezPath>::new();

    let mut subpath_start_pts: Vec<_> = loc_iters
        .iter_mut()
        .filter_map(|(loc, iter)| match iter.next() {
            Some(PathEl::MoveTo(p)) => {
                new_contours.entry((*loc).clone()).or_default().move_to(p);
                Some(p)
            }
            None => None,
            Some(other) => panic!("'{name}': illegal start of path: {other:?}"),
        })
        .collect();
    let mut prev_el_end_pts = subpath_start_pts.clone();
    loop {
        let elements: Vec<_> = loc_iters
            .iter_mut()
            .filter_map(|(_, iter)| iter.next())
            .collect();
        if elements.is_empty() {
            break; // normal termination, we exhausted all the element iterators
        }

        // No work to be done if the segment isn't cubic
        if let PathEl::CurveTo(..) = elements[0] {
            if prev_el_end_pts.len() != elements.len() {
                panic!("'{name}': not enough end points");
            }
            let cubics: Vec<_> = elements
                .iter()
                .zip(&prev_el_end_pts)
                .map(|(el, p0)| match el {
                    PathEl::CurveTo(p1, p2, p3) => CubicBez {
                        p0: *p0,
                        p1: *p1,
                        p2: *p2,
                        p3: *p3,
                    },
                    _ => unreachable!("'{name}': *must* all be cubic"),
                })
                .collect();

            // At long last, actually convert something to quadratic
            // TODO what should we pass for accuracy
            let Some(quad_splines) = cubics_to_quadratic_splines(&cubics, 1.0) else {
                panic!("'{name}': unable to convert to quadratic {cubics:?}");
            };
            if quad_splines.len() != loc_iters.len() {
                panic!(
                    "'{name}': needed {} splines, got {}",
                    quad_splines.len(),
                    loc_iters.len()
                );
            }

            loc_iters
                .iter()
                .zip(&quad_splines)
                .for_each(|((loc, _), quad_spline)| {
                    let bez = new_contours.entry((*loc).clone()).or_default();
                    quad_spline.to_quads().for_each(|quad| {
                        bez.quad_to(quad.p1, quad.p2);
                    })
                });
        } else {
            loc_iters.iter().zip(&elements).for_each(|((loc, _), el)| {
                new_contours.entry((*loc).clone()).or_default().push(*el)
            });
        };

        // Update element end points
        prev_el_end_pts.clear();
        elements
            .iter()
            .zip(&subpath_start_pts)
            .map(|(e, subpath_start)| match e {
                PathEl::MoveTo(p)
                | PathEl::LineTo(p)
                | PathEl::QuadTo(_, p)
                | PathEl::CurveTo(_, _, p) => p,
                PathEl::ClosePath => subpath_start,
            })
            .for_each(|p| prev_el_end_pts.push(*p));

        // If we are at a move then these are also our new subpath start points
        if let PathEl::MoveTo(..) = elements[0] {
            subpath_start_pts = prev_el_end_pts.clone();
        }
    }

    CheckedGlyph::Contour {
        name,
        contours: new_contours,
    }
}

/// An [ir::Glyph] that has been confirmed to maintain invariants:
///
/// <ul>
///     <li>Components are consistent across the design space</li>
///     <li>Paths are interpolation compatible</li>
/// </ul>
enum CheckedGlyph {
    Composite {
        name: GlyphName,
    },
    Contour {
        name: GlyphName,
        contours: HashMap<NormalizedLocation, BezPath>,
    },
}

impl TryFrom<&ir::Glyph> for CheckedGlyph {
    type Error = Error;

    fn try_from(glyph: &ir::Glyph) -> Result<Self, Self::Error> {
        // every instance must have consistent component glyphs
        let components: HashSet<BTreeSet<GlyphName>> = glyph
            .sources
            .values()
            .map(|s| s.components.iter().map(|c| c.base.clone()).collect())
            .collect();
        if components.len() > 1 {
            warn!("{} has inconsistent component glyph sequences; fontir is supposed to fix that for us", glyph.name);
            return Err(Error::GlyphError(
                glyph.name.clone(),
                GlyphProblem::InconsistentComponents,
            ));
        }

        // every instance must have consistent path element types
        let path_els: HashSet<String> = glyph
            .sources
            .values()
            .map(|s| {
                s.contours
                    .iter()
                    .map(|c| c.elements().iter().map(path_el_type).collect::<String>())
                    .collect()
            })
            .collect();
        if path_els.len() > 1 {
            return Err(Error::GlyphError(
                glyph.name.clone(),
                GlyphProblem::InconsistentPathElements,
            ));
        }
        let components = components.into_iter().next().unwrap_or_default();
        let path_els = path_els.into_iter().next().unwrap_or_default();
        trace!(
            "'{}' consistent: components '{:?}', paths '{}'",
            glyph.name,
            components,
            path_els
        );

        if !components.is_empty() && !path_els.is_empty() {
            warn!(
                "{} has component *and* paths; fontir is supposed to fix that for us",
                glyph.name
            );
            return Err(Error::GlyphError(
                glyph.name.clone(),
                GlyphProblem::HasComponentsAndPath,
            ));
        }

        // All is well, build the result
        let name = glyph.name.clone();
        Ok(if components.is_empty() {
            let contours = glyph
                .sources
                .iter()
                .map(|(location, instance)| {
                    if instance.contours.len() > 1 {
                        trace!(
                            "Merging {} contours to form '{}' at {:?}",
                            instance.contours.len(),
                            glyph.name,
                            location
                        );
                    }
                    let mut path = instance.contours.first().cloned().unwrap_or_default();
                    for contour in instance.contours.iter().skip(1) {
                        for el in contour.elements() {
                            path.push(*el);
                        }
                    }
                    (location.clone(), path)
                })
                .collect();
            CheckedGlyph::Contour { name, contours }
        } else {
            CheckedGlyph::Composite { name }
        })
    }
}

fn path_el_type(el: &PathEl) -> &'static str {
    match el {
        PathEl::MoveTo(..) => "M",
        PathEl::LineTo(..) => "L",
        PathEl::QuadTo(..) => "Q",
        PathEl::CurveTo(..) => "C",
        PathEl::ClosePath => "Z",
    }
}
