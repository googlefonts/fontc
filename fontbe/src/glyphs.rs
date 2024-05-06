//! 'glyf' and 'gvar' compilation
//!
//! Each glyph is built in isolation and then the fragments are collected
//! and glued together to form a final table.

use std::collections::{BTreeSet, HashMap, HashSet};

use fontdrasil::{
    coords::NormalizedLocation,
    orchestration::{Access, AccessBuilder, Work},
    types::GlyphName,
};
use fontir::{
    ir,
    orchestration::{Flags, WorkId as FeWorkId},
    variations::{VariationModel, VariationRegion},
};
use kurbo::{cubics_to_quadratic_splines, Affine, BezPath, CubicBez, PathEl, Point, Rect, Vec2};
use log::{log_enabled, trace, warn};

use write_fonts::{
    read::{
        tables::glyf::{self, Anchor, Transform},
        types::F2Dot14,
    },
    tables::{
        glyf::{
            Bbox, Component, ComponentFlags, CompositeGlyph, GlyfLocaBuilder, Glyph as RawGlyph,
            SimpleGlyph,
        },
        gvar::{iup::iup_delta_optimize, GlyphDelta},
    },
    OtRound,
};

use crate::{
    error::{Error, GlyphProblem},
    orchestration::{AnyWorkId, BeWork, Context, Glyph, GvarFragment, WorkId},
};

type Deltas = Vec<(VariationRegion, Vec<GlyphDelta>)>;

#[derive(Debug)]
struct GlyphWork {
    glyph_name: GlyphName,
}

pub fn create_glyf_work(glyph_name: GlyphName) -> Box<BeWork> {
    Box::new(GlyphWork { glyph_name })
}

/// Can glyph instance reuse the metrics of other?
///
/// To be safe the component should have:
///
/// * The same advance width as glyph
/// * A 2x2 transform that does nothing (basis vectors do not change)
/// * No x-translation
///    * y-translation is OK
///
/// This forces the composite glyph to use the possibly hinted horizontal
/// metrics of the sub-glyph, instead of those from the "hmtx" table.
///
/// See <https://github.com/googlefonts/ufo2ft/blob/0c0a570b84d1351ab704ba1fa5ae03aeef51179f/Lib/ufo2ft/instructionCompiler.py#L151-L173>
fn can_reuse_metrics(
    glyph: &ir::GlyphInstance,
    component_glyph: &ir::GlyphInstance,
    transform: &Affine,
) -> bool {
    let width: u16 = glyph.width.ot_round();
    if width != component_glyph.width.ot_round() {
        return false;
    }
    // transform needs to be identity ignoring dy
    let mut coeffs = transform.as_coeffs();
    coeffs[5] = 0.0;
    coeffs == Affine::IDENTITY.as_coeffs()
}

fn create_component(
    context: &Context,
    ref_glyph_name: &GlyphName,
    transform: &Affine,
) -> Result<(Component, Bbox), GlyphProblem> {
    // Obtain glyph id from static metadata
    let gid = context
        .ir
        .glyph_order
        .get()
        .glyph_id(ref_glyph_name)
        .ok_or(GlyphProblem::NotInGlyphOrder)?;

    // No known source does point anchoring so we just turn transform into a 2x2 + offset
    let [a, b, c, d, e, f] = transform.as_coeffs();
    let flags = ComponentFlags {
        round_xy_to_grid: true, // ufo2ft defaults to this, match it
        ..Default::default()
    };
    let component = Component::new(
        gid,
        Anchor::Offset {
            x: e as i16,
            y: f as i16,
        },
        Transform {
            xx: F2Dot14::from_f32(a as f32),
            yx: F2Dot14::from_f32(b as f32),
            xy: F2Dot14::from_f32(c as f32),
            yy: F2Dot14::from_f32(d as f32),
        },
        flags,
    );

    // Bbox computation is postponed to glyph merge to ensure all glyphs are available to query
    Ok((component, Bbox::default()))
}

fn create_composite(
    context: &Context,
    glyph: &ir::Glyph,
    default_location: &NormalizedLocation,
    components: &[(GlyphName, NormalizedLocation, Affine)],
) -> Result<CompositeGlyph, Error> {
    let mut errors = vec![];
    let mut set_use_my_metrics = false;
    let Some(default_glyph) = glyph.sources().get(default_location) else {
        return Err(Error::GlyphError(
            glyph.name.clone(),
            GlyphProblem::MissingDefault,
        ));
    };
    let components_at_default = components
        .iter()
        .filter_map(|(ref_glyph_name, loc, transform)| {
            if default_location == loc {
                Some((ref_glyph_name, transform))
            } else {
                None
            }
        })
        .filter_map(|(ref_glyph_name, transform)| {
            create_component(context, ref_glyph_name, transform)
                .map_err(|problem| {
                    errors.push(Error::ComponentError {
                        glyph: glyph.name.clone(),
                        referenced_glyph: ref_glyph_name.clone(),
                        problem,
                    })
                })
                .map(|(mut component, bbox)| {
                    if !set_use_my_metrics {
                        let component_glyph = context
                            .ir
                            .glyphs
                            .get(&FeWorkId::Glyph(ref_glyph_name.clone()));
                        if let Some(default_component) =
                            component_glyph.sources().get(default_location)
                        {
                            if can_reuse_metrics(default_glyph, default_component, transform) {
                                set_use_my_metrics = true;
                                component.flags.use_my_metrics = true;
                            }
                        }
                    }
                    (component, bbox)
                })
                .ok()
        });

    let composite = CompositeGlyph::try_from_iter(components_at_default)
        .map_err(|_| {
            errors.push(Error::GlyphError(
                glyph.name.clone(),
                GlyphProblem::NoComponents,
            ))
        })
        .ok();

    if !errors.is_empty() {
        return Err(Error::ComponentErrors {
            glyph: glyph.name.clone(),
            errors,
        });
    }
    Ok(composite.unwrap())
}

/// * <https://github.com/fonttools/fonttools/blob/3b9a73ff8379ab49d3ce35aaaaf04b3a7d9d1655/Lib/fontTools/ttLib/tables/_g_l_y_f.py#L335-L367>
/// * <https://docs.microsoft.com/en-us/typography/opentype/spec/tt_instructing_glyphs#phantoms>
fn add_phantom_points(advance: u16, points: &mut Vec<Point>) {
    // FontTools says
    //      leftSideX = glyph.xMin - leftSideBearing
    //      rightSideX = leftSideX + horizontalAdvanceWidth
    // We currently always set lsb to xMin so leftSideX = 0, rightSideX = advance.
    points.push(Point::new(0.0, 0.0)); // leftSideX, 0
    points.push(Point::new(advance as f64, 0.0)); // rightSideX, 0

    // TODO: vertical phantom points
    points.push(Point::new(0.0, 0.0));
    points.push(Point::new(0.0, 0.0));
}

/// See <https://github.com/fonttools/fonttools/blob/86291b6ef62ad4bdb48495a4b915a597a9652dcf/Lib/fontTools/ttLib/tables/_g_l_y_f.py#L369>
fn point_seqs_for_simple_glyph(
    ir_glyph: &ir::Glyph,
    instances: HashMap<NormalizedLocation, SimpleGlyph>,
) -> HashMap<NormalizedLocation, Vec<Point>> {
    instances
        .into_iter()
        .map(|(loc, glyph)| {
            let mut points = glyph
                .contours()
                .iter()
                .flat_map(|c| c.iter())
                .map(|cp| Point::new(cp.x as f64, cp.y as f64))
                .collect();

            add_phantom_points(ir_glyph.sources()[&loc].width.ot_round(), &mut points);

            (loc, points)
        })
        .collect()
}

/// See <https://github.com/fonttools/fonttools/blob/86291b6ef62ad4bdb48495a4b915a597a9652dcf/Lib/fontTools/ttLib/tables/_g_l_y_f.py#L369>
fn point_seqs_for_composite_glyph(ir_glyph: &ir::Glyph) -> HashMap<NormalizedLocation, Vec<Point>> {
    ir_glyph
        .sources()
        .iter()
        .map(|(loc, inst)| {
            // We need 1 point per component for it's X/Y, plus phantoms
            // See https://github.com/fonttools/fonttools/blob/1c283756a5e39d69459eea80ed12792adc4922dd/Lib/fontTools/ttLib/tables/_g_v_a_r.py#L243
            let mut points = Vec::new();
            for component in inst.components.iter() {
                let [.., dx, dy] = component.transform.as_coeffs();
                points.push((dx, dy).into());
            }
            add_phantom_points(inst.width.ot_round(), &mut points);

            (loc.clone(), points)
        })
        .collect()
}

fn compute_deltas(
    glyph_name: &GlyphName,
    var_model: &VariationModel,
    should_iup: bool,
    point_seqs: &HashMap<NormalizedLocation, Vec<Point>>,
    coords: &Vec<Point>,
    contour_ends: &Vec<usize>,
) -> Result<Deltas, Error> {
    // FontTools hard-codes 0.5
    //https://github.com/fonttools/fonttools/blob/65bc6105f7aec3478427525d23ddf2e3c8c4b21e/Lib/fontTools/varLib/__init__.py#L239
    let tolerance = 0.5;

    // Contour (aka Simple) and Composite both need gvar
    var_model
        .deltas(point_seqs)
        .map_err(|e| Error::GlyphDeltaError(glyph_name.clone(), e))?
        .into_iter()
        .map(|(region, deltas)| {
            // Spec: inferring of deltas for un-referenced points applies only
            // to simple glyphs, not to composite glyphs.
            if should_iup {
                // Doing IUP optimization here conveniently means it threads
                // per-glyph
                if log_enabled!(log::Level::Trace) {
                    // I like the point string better than the vec2
                    let deltas = deltas.iter().map(|d| d.to_point()).collect::<Vec<_>>();
                    trace!("IUP '{}', tolerance {tolerance}\n  {} contour ends {contour_ends:?}\n  {} deltas {deltas:?}\n  {} coords {coords:?}", glyph_name, contour_ends.len(), deltas.len(), coords.len());
                }
                iup_delta_optimize(deltas, coords.clone(), tolerance, contour_ends)
                    .map(|iup_deltas| (region.clone(), iup_deltas))
            } else {
                let deltas = process_composite_deltas(deltas);
                Ok((region, deltas))
            }
        })
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| Error::IupError(glyph_name.clone(), e))
}

/// convert raw deltas to the write-fonts representation (composite glyphs only)
fn process_composite_deltas(deltas: Vec<Vec2>) -> Vec<GlyphDelta> {
    let mut deltas = deltas
        .into_iter()
        .map(|delta| match delta.to_point().ot_round() {
            // IUP only applies to simple glyphs; for composites we
            // just mark the zero deltas as being interpolatable.
            (0, 0) => GlyphDelta::optional(0, 0),
            (x, y) => GlyphDelta::required(x, y),
        })
        .collect::<Vec<_>>();

    // match fonttools behaviour, ensuring there is at least one
    // delta required, to ensure we write an entry for the glyf.
    // <https://github.com/fonttools/fonttools/blob/0a3360e52727cdefce2e9b28286b074faf99033c/Lib/fontTools/varLib/__init__.py#L351>
    // <https://github.com/fonttools/fonttools/issues/1381>
    if deltas.iter().all(|delta| !delta.required) {
        if let Some(first) = deltas.first_mut() {
            first.required = true;
        }
    }
    deltas
}

impl Work<Context, AnyWorkId, Error> for GlyphWork {
    fn id(&self) -> AnyWorkId {
        WorkId::GlyfFragment(self.glyph_name.clone()).into()
    }

    /// We need to block on all our components, but we don't know them yet.
    ///
    /// We could block on ALL IR glyphs, but that triggers inefficient behavior in workload.rs.
    /// Instead, start in a hard block and update upon success of the corresponding IR job.
    /// See fontc, workload.rs, handle_success.
    fn read_access(&self) -> Access<AnyWorkId> {
        Access::Unknown
    }

    fn write_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .specific_instance(WorkId::GlyfFragment(self.glyph_name.clone()))
            .specific_instance(WorkId::GvarFragment(self.glyph_name.clone()))
            .build()
    }

    fn also_completes(&self) -> Vec<AnyWorkId> {
        vec![WorkId::GvarFragment(self.glyph_name.clone()).into()]
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        trace!("BE glyph work for '{}'", self.glyph_name);

        let static_metadata = context.ir.static_metadata.get();
        let default_location = static_metadata.default_location();
        let ir_glyph = &*context
            .ir
            .glyphs
            .get(&FeWorkId::Glyph(self.glyph_name.clone()));
        let glyph = CheckedGlyph::new(ir_glyph)?;

        // Hopefully in time https://github.com/harfbuzz/boring-expansion-spec means we can drop this
        let mut glyph = cubics_to_quadratics(glyph, static_metadata.units_per_em);

        if !context.flags.contains(Flags::KEEP_DIRECTION) {
            glyph.reverse_contour_direction();
        }

        let should_iup = glyph.should_iup(); // we partially borrow it later

        let (name, point_seqs, contour_ends) = match glyph {
            CheckedGlyph::Composite { name, components } => {
                let composite = create_composite(context, ir_glyph, default_location, &components)?;
                context
                    .glyphs
                    .set_unconditionally(Glyph::new(name.clone(), composite));
                let point_seqs = point_seqs_for_composite_glyph(ir_glyph);
                (name, point_seqs, Vec::new())
            }
            CheckedGlyph::Contour { name, paths } => {
                // Convert paths to SimpleGlyphs in parallel so we can get consistent point streams
                let (locations, bezpaths): (Vec<_>, Vec<_>) = paths.into_iter().unzip();
                let simple_glyphs = SimpleGlyph::interpolatable_glyphs_from_bezpaths(&bezpaths)
                    .map_err(|e| Error::KurboError {
                        glyph_name: self.glyph_name.clone(),
                        kurbo_problem: e,
                        context: bezpaths
                            .into_iter()
                            .map(|p| p.to_svg())
                            .collect::<Vec<_>>()
                            .join("\n"),
                    })?;
                let mut instances = HashMap::new();
                for (loc, glyph) in locations.into_iter().zip(simple_glyphs.into_iter()) {
                    instances.insert(loc, glyph);
                }

                // Establish the default outline of our simple glyph
                let Some(base_glyph) = instances.get(default_location) else {
                    return Err(Error::GlyphError(
                        ir_glyph.name.clone(),
                        GlyphProblem::MissingDefault,
                    ));
                };
                context
                    .glyphs
                    .set_unconditionally(Glyph::new(name.clone(), base_glyph.clone()));

                let mut num_points = 0;
                let mut contour_ends = Vec::with_capacity(base_glyph.contours().len());
                for contour in base_glyph.contours() {
                    assert!(!contour.is_empty());
                    num_points += contour.len();
                    contour_ends.push(num_points - 1);
                }
                (
                    name,
                    point_seqs_for_simple_glyph(ir_glyph, instances),
                    contour_ends,
                )
            }
        };

        let coords = point_seqs.get(default_location).ok_or_else(|| {
            Error::GlyphError(ir_glyph.name.clone(), GlyphProblem::MissingDefault)
        })?;

        // If our glyph is not sparse it will have the same set of locations as the global variation model
        // and we can use that. If it does not we must build a model specific to this glyph's master locations,
        // upon which the region of influence and the delta weights associated to each master in turn depend.
        let global_model = &static_metadata.variation_model;
        let deltas = if global_model.num_locations() == ir_glyph.sources().len()
            && global_model
                .locations()
                .all(|l| ir_glyph.sources().contains_key(l))
        {
            compute_deltas(
                &self.glyph_name,
                global_model,
                should_iup,
                &point_seqs,
                coords,
                &contour_ends,
            )?
        } else {
            let locations: HashSet<_> = ir_glyph.sources().keys().cloned().collect();
            let sub_model = VariationModel::new(locations, static_metadata.axes.clone())
                .map_err(|e| Error::VariationModelError(self.glyph_name.clone(), e))?;
            compute_deltas(
                &self.glyph_name,
                &sub_model,
                should_iup,
                &point_seqs,
                coords,
                &contour_ends,
            )?
        };

        context.gvar_fragments.set_unconditionally(GvarFragment {
            glyph_name: name,
            deltas,
        });

        Ok(())
    }
}

fn cubics_to_quadratics(glyph: CheckedGlyph, units_per_em: u16) -> CheckedGlyph {
    let CheckedGlyph::Contour {
        name,
        paths: contours,
    } = glyph
    else {
        return glyph; // nop for composite
    };

    trace!("Convert '{name}' to quadratic");

    // match fontTools.cu2qu default tolerance (i.e 1/1000th of UPEM):
    // https://github.com/fonttools/fonttools/blob/f99774a/Lib/fontTools/cu2qu/ufo.py#L43-L46
    let tolerance = units_per_em as f64 / 1000.0;

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
            None => {
                // keep the empty path for this location, but no start point
                new_contours.entry((*loc).clone()).or_default();
                None
            }
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
            let Some(quad_splines) = cubics_to_quadratic_splines(&cubics, tolerance) else {
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
            subpath_start_pts.clone_from(&prev_el_end_pts);
        }
    }

    CheckedGlyph::Contour {
        name,
        paths: new_contours,
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
        components: Vec<(GlyphName, NormalizedLocation, Affine)>,
    },
    Contour {
        name: GlyphName,
        paths: HashMap<NormalizedLocation, BezPath>,
    },
}

impl CheckedGlyph {
    fn new(glyph: &ir::Glyph) -> Result<Self, Error> {
        // every instance must have consistent component glyphs
        let components: HashSet<BTreeSet<GlyphName>> = glyph
            .sources()
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
            .sources()
            .values()
            .map(|s| {
                s.contours
                    .iter()
                    .map(|c| c.elements().iter().map(path_el_type).collect::<String>())
                    .collect()
            })
            .collect();
        if path_els.len() > 1 {
            warn!(
                "{} has inconsistent path elements: {path_els:?}",
                glyph.name
            );
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
                .sources()
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
            CheckedGlyph::Contour {
                name,
                paths: contours,
            }
        } else {
            let components = glyph
                .sources()
                .iter()
                .flat_map(|(location, instance)| {
                    trace!("{} {:?}", glyph.name, instance.components);
                    instance
                        .components
                        .iter()
                        .map(|c| (c.base.clone(), location.clone(), c.transform))
                })
                .collect();
            CheckedGlyph::Composite { name, components }
        })
    }

    fn should_iup(&self) -> bool {
        match self {
            CheckedGlyph::Composite { .. } => false,
            CheckedGlyph::Contour { .. } => true,
        }
    }

    /// Flip the glyph contours' direction, or do nothing if the glyph is a composite.
    ///
    /// The source contours are normally drawn with cubic curves thus are expected to be
    /// in counter-clockwise winding direction as recommended for PostScript outlines.
    /// When converting to TrueType quadratic splines, we reverse them so that they
    /// follow the clockwise direction as recommeded for TrueType outlines.
    fn reverse_contour_direction(&mut self) {
        if let CheckedGlyph::Contour { name, paths } = self {
            trace!("Reverse '{name}' contour direction");
            for contour in paths.values_mut() {
                *contour = contour.reverse_subpaths();
            }
        }
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

fn affine_for(component: &Component) -> Affine {
    let glyf::Anchor::Offset { x: dx, y: dy } = component.anchor else {
        panic!("Only offset anchor is supported");
    };
    Affine::new([
        component.transform.xx.to_f32().into(),
        component.transform.yx.to_f32().into(),
        component.transform.xy.to_f32().into(),
        component.transform.yy.to_f32().into(),
        dx.into(),
        dy.into(),
    ])
}

fn bbox2rect(bbox: Bbox) -> Rect {
    Rect {
        x0: bbox.x_min.into(),
        y0: bbox.y_min.into(),
        x1: bbox.x_max.into(),
        y1: bbox.y_max.into(),
    }
}

#[derive(Debug)]
struct GlyfLocaWork {}

pub fn create_glyf_loca_work() -> Box<BeWork> {
    Box::new(GlyfLocaWork {})
}

fn compute_composite_bboxes(context: &Context) -> Result<(), Error> {
    let glyph_order = context.ir.glyph_order.get();

    let glyphs: HashMap<_, _> = glyph_order
        .iter()
        .map(|gn| {
            (
                gn,
                context.glyphs.get(&WorkId::GlyfFragment(gn.clone()).into()),
            )
        })
        .collect();

    // Simple glyphs have bbox set. Composites don't.
    // Ultimately composites are made up of simple glyphs, lets figure out the boxes
    let mut bbox_acquired: HashMap<GlyphName, Rect> = HashMap::new();
    let mut composites = glyphs
        .values()
        .filter(|glyph| glyph.is_composite())
        .collect::<Vec<_>>();

    trace!("Resolve bbox for {} composites", composites.len());
    while !composites.is_empty() {
        let pending = composites.len();

        // Hopefully we can figure out some of those bboxes!
        for composite in composites.iter() {
            let glyph_name = &composite.name;
            let RawGlyph::Composite(composite) = &composite.data else {
                unreachable!("we just checked that these are all composites");
            };

            let mut missing_boxes = false;
            let boxes: Vec<_> = composite
                .components()
                .iter()
                .filter_map(|c| {
                    if missing_boxes {
                        return None; // can't succeed
                    }
                    let ref_glyph_name = glyph_order.glyph_name(c.glyph.to_u16() as usize).unwrap();
                    let bbox = bbox_acquired.get(ref_glyph_name).copied().or_else(|| {
                        glyphs
                            .get(ref_glyph_name)
                            .map(|g| g.as_ref().clone())
                            .and_then(|g| match &g.data {
                                RawGlyph::Composite(..) => None,
                                RawGlyph::Empty => None,
                                RawGlyph::Simple(simple_glyph) => Some(bbox2rect(simple_glyph.bbox)),
                            })
                    });
                    if bbox.is_none() {
                        trace!("Can't compute bbox for {glyph_name} because bbox for {ref_glyph_name} isn't ready yet");
                        missing_boxes = true;
                        return None; // maybe next time?
                    };

                    // The transform we get here has changed because it got turned into F2Dot14 and i16 parts
                    // We could go get the "real" transform from IR...?
                    let affine = affine_for(c);
                    let transformed_box = affine.transform_rect_bbox(bbox.unwrap());
                    Some(transformed_box)
                })
                .collect();
            if missing_boxes {
                trace!("bbox for {glyph_name} not yet resolveable");
                continue;
            }

            let bbox = boxes.into_iter().reduce(|acc, e| acc.union(e)).unwrap();
            trace!("bbox for {glyph_name} {bbox:?}");
            bbox_acquired.insert(glyph_name.clone(), bbox);
        }

        // Kerplode if we didn't make any progress this spin
        composites.retain(|composite| !bbox_acquired.contains_key(&composite.name));
        if pending == composites.len() {
            panic!("Unable to make progress on composite bbox, stuck at\n{composites:?}");
        }
    }

    // It'd be a shame to just throw away those nice boxes
    for (glyph_name, bbox) in bbox_acquired.into_iter() {
        let mut glyph = (*context
            .glyphs
            .get(&WorkId::GlyfFragment(glyph_name.clone()).into()))
        .clone();
        let RawGlyph::Composite(composite) = &mut glyph.data else {
            panic!("{glyph_name} is not a composite; we shouldn't be trying to update it");
        };
        composite.bbox = bbox.into(); // delay conversion to Bbox to avoid accumulating rounding error
        context.glyphs.set_unconditionally(glyph);
    }

    Ok(())
}

impl Work<Context, AnyWorkId, Error> for GlyfLocaWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Glyf.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::StaticMetadata)
            .variant(FeWorkId::GlyphOrder)
            .variant(WorkId::ALL_GLYF_FRAGMENTS)
            .build()
    }

    fn write_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(AnyWorkId::Be(WorkId::Glyf))
            .variant(AnyWorkId::Be(WorkId::Loca))
            .variant(AnyWorkId::Be(WorkId::LocaFormat))
            .variant(WorkId::ALL_GLYF_FRAGMENTS)
            .build()
    }

    fn also_completes(&self) -> Vec<AnyWorkId> {
        vec![WorkId::Loca.into(), WorkId::LocaFormat.into()]
    }

    /// Generate [glyf](https://learn.microsoft.com/en-us/typography/opentype/spec/glyf)
    /// and [loca](https://learn.microsoft.com/en-us/typography/opentype/spec/loca).
    ///
    /// We've already generated all the binary glyphs so all we have to do here is glue everything together.
    fn exec(&self, context: &Context) -> Result<(), Error> {
        compute_composite_bboxes(context)?;

        let glyph_order = context.ir.glyph_order.get();
        let mut builder = GlyfLocaBuilder::new();

        for name in glyph_order.iter() {
            let glyph = context
                .glyphs
                .get(&WorkId::GlyfFragment(name.clone()).into());
            builder.add_glyph(&glyph.data).unwrap();
        }

        let (glyf, loca, loca_format) = builder.build();
        let raw_loca = write_fonts::dump_table(&loca).unwrap();
        let raw_glyf = write_fonts::dump_table(&glyf).unwrap();
        context.loca_format.set(loca_format.into());
        context.glyf.set(raw_glyf.into());
        context.loca.set(raw_loca.into());

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use fontdrasil::{
        coords::{NormalizedCoord, NormalizedLocation},
        types::GlyphName,
    };
    use fontir::ir;
    use kurbo::{Affine, BezPath, PathEl};
    use rstest::rstest;
    use write_fonts::types::Tag;

    /// Returns a glyph instance and another one that can be its component
    fn create_reusable_component() -> (ir::GlyphInstance, ir::GlyphInstance) {
        let parent = ir::GlyphInstance {
            width: 42.5,
            ..Default::default()
        };
        let child = parent.clone();
        (parent, child)
    }

    #[test]
    fn can_reuse_metrics_no_transform() {
        let (glyph, component) = create_reusable_component();
        assert!(can_reuse_metrics(&glyph, &component, &Affine::IDENTITY));
    }

    #[test]
    fn cannot_reuse_metrics_if_width_mismatch() {
        let (glyph, mut component) = create_reusable_component();
        component.width += 1.0;
        assert!(!can_reuse_metrics(&glyph, &component, &Affine::IDENTITY));
    }

    #[test]
    fn can_reuse_metrics_ignores_dy() {
        let (glyph, component) = create_reusable_component();
        assert!(can_reuse_metrics(
            &glyph,
            &component,
            &Affine::translate((0.0, 1.0))
        ));
    }

    #[test]
    fn cannot_reuse_metrics_for_non_dy_transform() {
        let (glyph, component) = create_reusable_component();
        // [5], which we don't reach, is dy
        for i in 0..5 {
            let mut coeffs = Affine::IDENTITY.as_coeffs();
            coeffs[i] = 2.0;
            assert!(!can_reuse_metrics(&glyph, &component, &Affine::new(coeffs)));
        }
    }

    #[test]
    fn all_zero_composite_deltas() {
        let zeros = vec![Vec2::ZERO; 6];
        // if a composite glyph has all zero deltas we want to finish with a single
        // required zero delta, and skip the rest:
        let deltas = process_composite_deltas(zeros);
        assert!(deltas[0].required);
        assert!(deltas.iter().skip(1).all(|d| !d.required));
    }

    #[test]
    fn elide_composite_zero_deltas() {
        // if a composite glyph has some non-zero deltas than all zero deltas
        // become optional
        let deltas = vec![
            Vec2::ZERO,
            Vec2::new(1., 0.),
            Vec2::ZERO,
            Vec2::ZERO,
            Vec2::new(0., 5.),
        ];

        let processed = process_composite_deltas(deltas.clone());
        for (pre, post) in deltas.iter().zip(processed.iter()) {
            let should_be_required = *pre != Vec2::ZERO;
            assert_eq!(should_be_required, post.required)
        }
    }

    fn simple_static_contour_glyph() -> CheckedGlyph {
        // Contains one default instance with one contour comprising two segments, i.e.
        // a cubic curve and a closing line
        let mut paths = HashMap::new();
        paths.insert(
            NormalizedLocation::from(vec![(Tag::new(b"wght"), NormalizedCoord::new(0.0))]),
            BezPath::from_vec(vec![
                PathEl::MoveTo((0.0, 500.0).into()),
                PathEl::CurveTo(
                    (200.0, 500.0).into(),
                    (500.0, 200.0).into(),
                    (500.0, 0.0).into(),
                ),
                PathEl::ClosePath,
            ]),
        );
        CheckedGlyph::Contour {
            name: GlyphName::from("test"),
            paths,
        }
    }

    #[rstest]
    #[case::small_upem(500, 8)]
    #[case::default_upem(1000, 7)]
    #[case::large_upem(2000, 6)]
    fn cubics_to_quadratics_at_various_upems(#[case] upem: u16, #[case] expected_segments: usize) {
        // The default conversion accuracy/tolerance is set to 1/1000th of the UPEM.
        // Therefore, the number of converted quadratic segments increases as the UPEM
        // decreases, or decreases as the UPEM increases.
        let CheckedGlyph::Contour { paths, .. } =
            cubics_to_quadratics(simple_static_contour_glyph(), upem)
        else {
            panic!("Expected a contour glyph");
        };

        assert_eq!(
            paths.values().next().unwrap().segments().count(),
            expected_segments
        );
    }
}
