//! 'glyf' and 'gvar' compilation
//!
//! Each glyph is built in isolation and then the fragments are collected
//! and glued together to form a final table.

use std::{
    collections::{BTreeSet, HashMap, HashSet},
    sync::Arc,
};

use fontdrasil::{
    coords::NormalizedLocation,
    orchestration::{Access, AccessBuilder, Work},
    types::GlyphName,
    variations::{VariationModel, VariationRegion},
};
use fontir::{
    ir::{self, GlobalMetrics, GlyphOrder},
    orchestration::{Flags, WorkId as FeWorkId},
};
use kurbo::{Affine, BezPath, CubicBez, PathEl, Point, Rect, Vec2, cubics_to_quadratic_splines};
use log::{log_enabled, trace, warn};

use write_fonts::{
    OtRound,
    read::{
        tables::glyf::{self, Anchor, Transform},
        types::F2Dot14,
    },
    tables::{
        glyf::{
            Bbox, Component, ComponentFlags, CompositeGlyph, GlyfLocaBuilder, Glyph as RawGlyph,
            SimpleGlyph,
        },
        gvar::{GlyphDelta, iup::iup_delta_optimize},
    },
    types::GlyphId16,
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
/// * No x-translation that will survive rounding
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
    let component_width: u16 = component_glyph.width.ot_round();
    if width != component_width {
        return false;
    }
    // transform needs to be identity ignoring dy, and dx if it will be rounded away
    let mut coeffs = transform.as_coeffs();
    coeffs[4] = coeffs[4].ot_round();
    coeffs[5] = 0.0;
    coeffs == Affine::IDENTITY.as_coeffs()
}

fn create_component_ref_gid(
    gid: GlyphId16,
    transform: &Affine,
) -> Result<(Component, Bbox), GlyphProblem> {
    // No known source does point anchoring so we just turn transform into a 2x2 + offset
    let [a, b, c, d, e, f] = transform.as_coeffs();
    let flags = ComponentFlags {
        round_xy_to_grid: true, // ufo2ft defaults to this, match it
        ..Default::default()
    };

    // By this point, fontir should have decomposed any components with transforms
    // outside the -2.0 to +2.0 range. For values between MAX_F2DOT14 and 2.0,
    // F2Dot14::from_f32() will saturate to MAX_F2DOT14, matching fonttools behavior:
    // https://github.com/googlefonts/fontc/issues/1638

    let component = Component::new(
        gid,
        Anchor::Offset {
            x: e.ot_round(),
            y: f.ot_round(),
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

fn create_component_ref_name(
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

    create_component_ref_gid(gid, transform)
}

fn create_composite(
    context: &Context,
    glyph: &ir::Glyph,
    default_location: &NormalizedLocation,
    components: &[(GlyphName, NormalizedLocation, Affine)],
    is_variable: bool,
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
            create_component_ref_name(context, ref_glyph_name, transform)
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

                        // Only set USE_MY_METRICS for static fonts (no variable axes). The flag
                        // is only useful for hinted fonts where one wants to reuse the hinted metrics
                        // for composites, and as such should be set by the hinting editor rather than
                        // fontc. We keep setting the flag on statics to match the old behavior of
                        // fontmake. For variable fonts, the presence of this flag can be dangerous
                        // when the composite and component glyph metrics aren't equal in all the
                        // masters. It could cause the wrong metrics to be used depending on whether
                        // the advance width is computed from HVAR (for which the flag is normally
                        // ignored) or from the glyf+gvar phantom points of the flagged component.
                        // While we could detect this inconsistency and only set the flag when it is
                        // safe (basically no-op), it's not worth the extra complexity.
                        if !is_variable
                            && let Some(default_component) =
                                component_glyph.sources().get(default_location)
                            && can_reuse_metrics(default_glyph, default_component, transform)
                        {
                            set_use_my_metrics = true;
                            component.flags.use_my_metrics = true;
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

/// See <https://github.com/fonttools/fonttools/blob/86291b6ef6/Lib/fontTools/ttLib/tables/_g_l_y_f.py#L369>
fn point_seqs_for_simple_glyph(
    ir_glyph: &ir::Glyph,
    instances: HashMap<NormalizedLocation, SimpleGlyph>,
    global_metrics: &GlobalMetrics,
    build_vertical: bool,
) -> HashMap<NormalizedLocation, Vec<Point>> {
    instances
        .into_iter()
        .map(|(loc, glyph)| {
            let mut points = glyph
                .contours
                .iter()
                .flat_map(|c| c.iter())
                .map(|cp| Point::new(cp.x as f64, cp.y as f64))
                .collect();

            let instance = &ir_glyph.sources()[&loc];
            let metrics = global_metrics.at(&loc);

            instance.add_phantom_points(&metrics, build_vertical, &mut points);

            (loc, points)
        })
        .collect()
}

/// <https://github.com/fonttools/fonttools/blob/86291b6ef6/Lib/fontTools/ttLib/tables/_g_l_y_f.py#L369>
fn point_seqs_for_composite_glyph(
    ir_glyph: &ir::Glyph,
    global_metrics: &GlobalMetrics,
    build_vertical: bool,
) -> HashMap<NormalizedLocation, Vec<Point>> {
    ir_glyph
        .sources()
        .iter()
        .map(|(loc, inst)| {
            // We need 1 point per component for it's X/Y, plus phantoms
            // See https://github.com/fonttools/fonttools/blob/1c283756a5/Lib/fontTools/ttLib/tables/_g_v_a_r.py#L243
            let mut points = Vec::new();
            for component in inst.components.iter() {
                let [.., dx, dy] = component.transform.as_coeffs();
                // ensure we round now, before iup or gvar generation:
                // https://github.com/fonttools/fonttools/blob/5ae2943a43/Lib/fontTools/pens/ttGlyphPen.py#L110
                let point = Point::new(dx.ot_round(), dy.ot_round());
                points.push(point);
            }
            let metrics = global_metrics.at(loc);
            inst.add_phantom_points(&metrics, build_vertical, &mut points);

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
    //https://github.com/fonttools/fonttools/blob/65bc6105f7/Lib/fontTools/varLib/__init__.py#L239
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
    deltas
        .into_iter()
        .map(|delta| match delta.to_point().ot_round() {
            // IUP only applies to simple glyphs; for composites we
            // just mark the zero deltas as being interpolatable.
            (0, 0) => GlyphDelta::optional(0, 0),
            (x, y) => GlyphDelta::required(x, y),
        })
        .collect()
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
        let global_metrics = context.ir.global_metrics.get();
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
                let is_variable = !static_metadata.axes.is_empty();
                let composite = create_composite(
                    context,
                    ir_glyph,
                    default_location,
                    &components,
                    is_variable,
                )?;
                context
                    .glyphs
                    .set_unconditionally(Glyph::new(name.clone(), composite));
                let point_seqs = point_seqs_for_composite_glyph(
                    ir_glyph,
                    &global_metrics,
                    static_metadata.build_vertical,
                );
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
                let mut contour_ends = Vec::with_capacity(base_glyph.contours.len());
                for contour in &base_glyph.contours {
                    assert!(!contour.is_empty());
                    num_points += contour.len();
                    contour_ends.push(num_points - 1);
                }
                (
                    name,
                    point_seqs_for_simple_glyph(
                        ir_glyph,
                        instances,
                        &global_metrics,
                        static_metadata.build_vertical,
                    ),
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
            let sub_model = VariationModel::new(locations, static_metadata.axes.axis_order());

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
        let name = &glyph.name;
        // every instance must have consistent component glyphs
        let components: HashSet<BTreeSet<GlyphName>> = glyph
            .sources()
            .values()
            .map(|s| s.components.iter().map(|c| c.base.clone()).collect())
            .collect();
        if components.len() > 1 {
            warn!(
                "{name} has inconsistent component glyph sequences; fontir is supposed to fix that for us"
            );
            return Err(Error::GlyphError(
                name.clone(),
                GlyphProblem::InconsistentComponents,
            ));
        }

        // every instance must have consistent path element types
        let path_els: HashSet<String> = glyph
            .sources()
            .values()
            .map(|g| g.path_elements())
            .collect();
        if path_els.len() > 1 {
            warn!("{name} has inconsistent path elements: {path_els:?}",);
            return Err(Error::GlyphError(
                name.clone(),
                GlyphProblem::InconsistentPathElements,
            ));
        }
        let components = components.into_iter().next().unwrap_or_default();
        let path_els = path_els.into_iter().next().unwrap_or_default();
        trace!("'{name}' consistent: components '{components:?}', paths '{path_els}'",);

        if !components.is_empty() && !path_els.is_empty() {
            warn!("{name} has component *and* paths; fontir is supposed to fix that for us",);
            return Err(Error::GlyphError(
                name.clone(),
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
                    let n_contours = instance.contours.len();
                    if n_contours > 1 {
                        trace!("Merging {n_contours} contours to form '{name}' at {location:?}",);
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
                    trace!("{name} {:?}", instance.components);
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

#[derive(Debug)]
struct GlyfLocaWork {}

pub fn create_glyf_loca_work() -> Box<BeWork> {
    Box::new(GlyfLocaWork {})
}

/// See <https://github.com/fonttools/fonttools/blob/42c1a52c5facd0edbc9c685787b084af44f6f607/Lib/fontTools/ttLib/tables/_g_l_y_f.py#L1244>
fn bbox_of_composite(
    glyph_order: &GlyphOrder,
    glyphs: &HashMap<&GlyphName, Arc<Glyph>>,
    composite: &CompositeGlyph,
    affine: Affine,
) -> Result<Option<Rect>, Error> {
    // For simple scale+translate transforms, which seem to be common, we could just transform a bbox
    // Let's wait to see if that pops out in a profile and do the simple solution for now
    // Because transforms can skew/rotate the control box computed for the simple glyph isn't always reusable

    let mut bbox: Option<Rect> = None;
    for component in composite.components() {
        // The transform we get here has changed because it got turned into F2Dot14 and i16 parts
        // We could go get the "real" transform from IR but ... this seems to match fontmake so far
        let affine = affine * affine_for(component);

        let ref_glyph_name = glyph_order
            .glyph_name(component.glyph.to_u16() as usize)
            .unwrap();
        let Some(ref_glyph) = glyphs.get(ref_glyph_name) else {
            return Err(Error::MissingGlyphId(ref_glyph_name.clone()));
        };
        match &ref_glyph.data {
            RawGlyph::Empty => continue, // no impact on our bbox
            RawGlyph::Simple(ref_simple) => {
                // Update our bbox to include the transformed points
                for pt in ref_simple.contours.iter().flat_map(|c| c.iter()) {
                    let pt = affine * Point::new(pt.x as f64, pt.y as f64);
                    bbox = Some(if let Some(current) = bbox {
                        current.union_pt(pt)
                    } else {
                        Rect::from_points(pt, pt)
                    });
                }
            }
            RawGlyph::Composite(ref_composite) => {
                // Chase our components using an updated transform
                if let Some(child_bbox) =
                    bbox_of_composite(glyph_order, glyphs, ref_composite, affine)?
                {
                    bbox = bbox.map(|bbox| bbox.union(child_bbox)).or(Some(child_bbox));
                }
            }
        }
    }
    Ok(bbox)
}

fn compute_composite_bboxes(context: &Context) -> Result<(), Error> {
    let glyph_order = context.ir.glyph_order.get();

    let glyphs: HashMap<_, _> = glyph_order
        .names()
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

    for (glyph_name, glyph) in glyphs.iter().filter_map(|(gn, g)| match &g.data {
        RawGlyph::Composite(composite) => Some((*gn, composite)),
        RawGlyph::Simple(..) | RawGlyph::Empty => None,
    }) {
        let bbox = bbox_of_composite(&glyph_order, &glyphs, glyph, Affine::IDENTITY)?;
        bbox_acquired.insert(glyph_name.clone(), bbox.unwrap_or_default());
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

    /// We need to block on all `GlyfFragment`s to build glyf and loca, but the final glyph
    /// order may not be known yet as it may be extended with e.g. a generated '.notdef'.
    ///
    /// A generic `variant(WorkId::ALL_GLYF_FRAGMENTS)` dependency can occasionally be racy
    /// and lead to a panic if the `GlyfLocaWork` is started too early.
    /// For a `variant` access type, the dependency is deemed "fullfilled" if the pending work
    /// count drops to 0, which may occur when all the "static" glyph fragments have been
    /// generated before the dynamic ones have yet to be scheduled.
    ///
    /// So here instead we use `Access::Unknown` to start in a hard block and update our
    /// `read_access` with `specific_instance` GlyfFragments once the final `GlyphOrder`
    /// work completes (see `fontc::workload::Workload::handle_success`).
    ///
    /// Also see <https://github.com/googlefonts/fontc/issues/1436>
    fn read_access(&self) -> Access<AnyWorkId> {
        // AccessBuilder::new()
        //     .variant(FeWorkId::StaticMetadata)
        //     .variant(FeWorkId::GlyphOrder)
        //     .variant(WorkId::ALL_GLYF_FRAGMENTS)
        //     .build()
        Access::Unknown
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

        for name in glyph_order.names() {
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

    #[derive(Debug, Clone, Copy)]
    enum GlyphType {
        Simple,
        Composite,
    }

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
    fn can_reuse_metrics_ignores_negligible_dx() {
        let (glyph, component) = create_reusable_component();
        assert!(can_reuse_metrics(
            &glyph,
            &component,
            &Affine::translate((0.4, 0.0))
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
        // if a composite glyph has all zero deltas we want to skip them all
        let deltas = process_composite_deltas(zeros);
        assert!(deltas.iter().all(|d| !d.required));
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

    // Contributor to https://github.com/googlefonts/fontc/pull/1050
    #[test]
    fn component_translation_otrounds() {
        let (c, _) = create_component_ref_gid(
            GlyphId16::new(42),
            &Affine::new([1.0, 0.0, 0.0, 1.0, 0.4, 0.9]),
        )
        .unwrap();
        let Anchor::Offset { x, y } = c.anchor else {
            panic!("Must be an offset");
        };
        assert_eq!((0, 1), (x, y));
    }

    #[test]
    fn test_component_transform_saturation() {
        // Test that component 2x2 transforms with values exceeding F2Dot14's range
        // get saturated to min/max by font-types' F2Dot14::from_f32.
        // We are interested in particular to values > MAX_F2DOT14 but <= 2.0 (e.g.
        // 'xx' below), which fonttools TTGlyphPen clamps to MAX_F2DOT14.
        // Components with transform values < -2.0 or > 2.0 are always decomposed
        // in fontir so they should never reach here. I include them here to
        // show what would happen if they did.
        let transform = Affine::new([1.99995, -2.0001, 1.0, 2.5, 100.0, -200.0]);
        let (c, _) = create_component_ref_gid(GlyphId16::new(42), &transform).unwrap();

        // Both xx and yy are > MAX_F2DOT14 thus get clamped to MAX_F2DOT14
        assert_eq!(c.transform.xx, F2Dot14::MAX);
        assert_eq!(c.transform.yy, F2Dot14::MAX);
        // yx < MIN_F2DOT14 and gets clamped to MIN_F2DOT14
        assert_eq!(c.transform.yx, F2Dot14::MIN);
        // xy is within the valid range
        assert_eq!(c.transform.xy.to_f32(), 1.0);
        // translation offsets are encoded as Fixed16.16 so stay the same
        assert_eq!(c.anchor, Anchor::Offset { x: 100, y: -200 });
    }

    #[rstest]
    #[case::empty(GlyphType::Simple, 0)]
    #[case::simple(GlyphType::Simple, 4)]
    #[case::composite(GlyphType::Composite, 2)]
    fn point_seqs_interpolates_global_metrics_for_vertical_phantoms(
        #[case] glyph_type: GlyphType,
        #[case] num_points_or_components: usize,
    ) {
        // Test point_seqs_for_{simple,composite}_glyph with "sparse" GlobalMetrics sources.
        //
        // When computing vertical phantom points of glyph instances that exist at
        // locations not explicitly defined in GlobalMetrics (e.g. medium master between
        // regular and bold), and the glyphs don't themselves define explicit height and
        // vertical_origin, the ascender/descender that are used as fallback should be
        // interpolated via GlobalMetrics::at().

        use write_fonts::tables::glyf::SimpleGlyph;

        let regular = NormalizedLocation::for_pos(&[("wght", 0.0)]);
        let bold = NormalizedLocation::for_pos(&[("wght", 1.0)]);
        let medium = NormalizedLocation::for_pos(&[("wght", 0.5)]);

        // Build GlobalMetrics with SPARSE locations (only regular and bold, NOT medium)
        let mut metrics_builder = ir::GlobalMetricsBuilder::new();

        // Populate defaults with varying ascender/descender metrics to create variation
        metrics_builder.populate_defaults(&regular, 1000, None, Some(800.0), Some(-200.0), None);
        metrics_builder.populate_defaults(&bold, 1000, None, Some(820.0), Some(-220.0), None);

        let axes = fontdrasil::types::Axes::new(vec![fontdrasil::types::Axis::for_test("wght")]);
        let global_metrics = metrics_builder.build(&axes).unwrap();

        // Create an ir::Glyph with instances at all 3 locations (including medium)
        let mut glyph_builder = ir::GlyphBuilder::new("test".into());

        for loc in [&regular, &bold, &medium] {
            let (contours, components) = match glyph_type {
                GlyphType::Simple => {
                    let contours = if num_points_or_components > 0 {
                        // Create a simple path with the specified number of points
                        // Coordinates don't matter - we only care about phantom points
                        let mut path = BezPath::new();
                        path.move_to((0.0, 0.0));
                        for i in 1..num_points_or_components {
                            path.line_to((i as f64, i as f64));
                        }
                        path.close_path();
                        vec![path]
                    } else {
                        vec![]
                    };
                    (contours, vec![])
                }
                GlyphType::Composite => (
                    vec![],
                    vec![
                        ir::Component {
                            base: "base".into(),
                            transform: kurbo::Affine::translate((100.0, 0.0)),
                        },
                        ir::Component {
                            base: "accent".into(),
                            transform: kurbo::Affine::translate((100.0, 600.0)),
                        },
                    ],
                ),
            };
            // All instances have height: None, vertical_origin: None to trigger fallback
            let instance = ir::GlyphInstance {
                width: 200.0,
                height: None,
                vertical_origin: None,
                contours,
                components,
            };
            glyph_builder.try_add_source(loc, instance).unwrap();
        }
        let ir_glyph = glyph_builder.build().unwrap();

        // Call the appropriate `point_seqs_for_*_glyph` function based on glyph type
        let point_seqs = match glyph_type {
            GlyphType::Simple => {
                let mut simple_glyphs = HashMap::new();
                for loc in [regular.clone(), bold.clone(), medium.clone()] {
                    let simple_glyph = if num_points_or_components > 0 {
                        let instance = &ir_glyph.sources()[&loc];
                        SimpleGlyph::from_bezpath(&instance.contours[0]).unwrap()
                    } else {
                        SimpleGlyph::default()
                    };
                    simple_glyphs.insert(loc, simple_glyph);
                }
                point_seqs_for_simple_glyph(&ir_glyph, simple_glyphs, &global_metrics, true)
            }
            GlyphType::Composite => {
                point_seqs_for_composite_glyph(&ir_glyph, &global_metrics, true)
            }
        };

        assert_eq!(point_seqs.len(), 3);

        let expected_total_points = num_points_or_components + 4;
        let top_phantom_idx = num_points_or_components + 2;
        let bottom_phantom_idx = num_points_or_components + 3;

        let regular_points = &point_seqs[&regular];
        let bold_points = &point_seqs[&bold];
        let medium_points = &point_seqs[&medium];

        assert_eq!(regular_points.len(), expected_total_points);
        assert_eq!(bold_points.len(), expected_total_points);
        assert_eq!(medium_points.len(), expected_total_points);

        let regular_top = regular_points[top_phantom_idx].y;
        let regular_bottom = regular_points[bottom_phantom_idx].y;
        let bold_top = bold_points[top_phantom_idx].y;
        let bold_bottom = bold_points[bottom_phantom_idx].y;
        let medium_top = medium_points[top_phantom_idx].y;
        let medium_bottom = medium_points[bottom_phantom_idx].y;

        // Verify existing metrics at master locations:
        // Regular: top=vertical_origin=800, bottom=800-1000=-200
        assert_eq!(regular_top, 800.0);
        assert_eq!(regular_bottom, -200.0);

        // Bold: top=vertical_origin=820, bottom=820-1040=-220
        assert_eq!(bold_top, 820.0);
        assert_eq!(bold_bottom, -220.0);

        // Verify that INTERPOLATED metrics at medium location (wght=0.5)
        // are halfway between regular and bold
        assert_eq!(
            medium_top, 810.0,
            "Medium top should be interpolated: (800+820)/2 = 810"
        );
        assert_eq!(
            medium_bottom, -210.0,
            "Medium bottom should be interpolated: (-200+(-220))/2 = -210"
        );
    }
}
