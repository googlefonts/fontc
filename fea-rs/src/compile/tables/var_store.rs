//! Building the ItemVariationStore

use std::collections::HashMap;

use indexmap::IndexMap;
use write_fonts::tables::{
    layout::VariationIndex,
    variations::{ItemVariationData, ItemVariationStore, VariationRegion, VariationRegionList},
};

/// A builder for the [ItemVariationStore].
///
/// This handles assigning VariationIndex values to unique sets of deltas and
/// grouping delta sets into [ItemVariationData] subtables.
#[derive(Clone, Default, Debug)]
pub(crate) struct VariationStoreBuilder {
    // region -> index map
    all_regions: HashMap<VariationRegion, usize>,
    // we use an index map so that we have a deterministic ordering,
    // which lets us write better tests
    delta_sets: IndexMap<RegionSet, IndexMap<DeltaSet, DeltaKey>>,
    next_id: DeltaKey,
}

/// A map from the temporary delta set identifiers to the final values.
///
/// This is generated when the [ItemVariationStore] is built; afterwards
/// any tables or records that contain VariationIndex tables need to be remapped.
#[derive(Clone, Debug, Default)]
pub(crate) struct VariationIndexRemapping {
    map: HashMap<DeltaKey, DeltaKey>,
}

/// a subset of regions, represented as indicies into the region map
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct RegionSet {
    indices: Vec<u16>,
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
struct DeltaSet {
    values: Vec<i32>,
}

/// A temporary value that uniquely describes a particular delta set.
///
/// When adding a delta set to the [VariationStoreBuilder], the final position
/// of that data is not known until the whole table is compiled. We use these
/// temporary keys to identify delta sets before the builder is finalized.
///
/// After the [ItemVariationStore] is built, we remap keys to the final value.
///
/// This is intended to be temporarily stored in a value record or anchor table's
/// VariationIndex table.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct DeltaKey {
    pub(crate) outer: u16,
    pub(crate) inner: u16,
}

impl DeltaKey {
    #[allow(dead_code)]
    pub const NO_DELTAS: DeltaKey = DeltaKey {
        outer: 0xFFFF,
        inner: 0xFFFF,
    };

    /// Construct the next identifier.
    ///
    /// Identifiers start (0xfffe, 0xfffe) and decrease (this makes them easier
    /// to identify if they show up in a compiled table)
    const fn next(self) -> Self {
        let DeltaKey { outer, inner } = self;
        match inner.checked_sub(1) {
            Some(inner) => DeltaKey { outer, inner },
            None => DeltaKey {
                outer: outer.saturating_sub(1),
                inner: 0xFFFF,
            },
        }
    }

    /// Return the current key and update self
    fn bump(&mut self) -> Self {
        let value = *self;
        *self = self.next();
        value
    }
}

impl Default for DeltaKey {
    fn default() -> Self {
        Self {
            outer: 0xFFFE,
            inner: 0xFFFE,
        }
    }
}

impl VariationStoreBuilder {
    pub(crate) fn add_deltas(&mut self, deltas: Vec<(VariationRegion, i16)>) -> DeltaKey {
        let mut delta_set = DeltaSet::default();
        let mut regions = Vec::with_capacity(deltas.len());
        for (region, delta) in deltas {
            let region_idx = self.index_for_region(region);
            delta_set.push(delta as _);
            regions.push(region_idx as u16);
        }
        *self
            .delta_sets
            .entry(RegionSet { indices: regions })
            .or_default()
            .entry(delta_set)
            .or_insert_with(|| self.next_id.bump())
    }

    fn index_for_region(&mut self, region: VariationRegion) -> usize {
        let next_idx = self.all_regions.len();
        *self.all_regions.entry(region).or_insert(next_idx)
    }

    pub(crate) fn build(self) -> (ItemVariationStore, VariationIndexRemapping) {
        let mut key_lookup = VariationIndexRemapping::default();
        let mut region_list = self
            .all_regions
            .into_iter()
            .map(|(reg, idx)| (idx, reg))
            .collect::<Vec<_>>();
        region_list.sort_unstable();
        let region_list = region_list.into_iter().map(|(_, reg)| reg).collect();
        let mut item_variations = Vec::new();

        // Now: for each region set, we will produce one or more subtables
        for (region, deltas) in self.delta_sets {
            add_deltas(region, deltas, &mut item_variations, &mut key_lookup);
        }

        let varstore =
            ItemVariationStore::new(VariationRegionList::new(region_list), item_variations);
        (varstore, key_lookup)
    }
}

/// create one or more subtables for the deltas in a particular region
fn add_deltas(
    regionset: RegionSet,
    deltas: IndexMap<DeltaSet, DeltaKey>,
    out: &mut Vec<Option<ItemVariationData>>,
    keys: &mut VariationIndexRemapping,
) {
    // if there are no deltas, we push an empty subtable
    if deltas.is_empty() {
        out.push(None);
        return;
    }

    let n_regions = regionset.indices.len();

    // for now we just always use long indices
    let word_delta_count: u16 = n_regions as _;

    // (item_count, BE bytes)
    let mut finished = Vec::new();
    let mut current = Vec::new();

    // the index of the current subtable
    let mut outer = out.len() as u16;
    // the index of the deltaset, in that subtable
    let mut inner = 0u16;
    for (delta, key) in deltas {
        if inner == 0 {
            // next_index wraps, so if it's zero and this is non-empty it means
            // we just filled a subtable
            if !current.is_empty() {
                finished.push((0xffff, std::mem::take(&mut current)));
                outer += 1;
            }
        }
        let real_key = DeltaKey { outer, inner };
        current.extend(delta.values.iter().flat_map(|v| (*v as i16).to_be_bytes()));
        keys.set(key, real_key);
        inner = inner.wrapping_add(1);
    }

    // add all subtables, or null subtables if they were empty
    // (empty is only possible when the number of deltasets is divisible by 0xffff)
    for (item_count, deltas) in finished.into_iter().chain(Some((inner, current))) {
        if deltas.is_empty() {
            out.push(None);
        } else {
            out.push(Some(ItemVariationData::new(
                item_count,
                word_delta_count,
                regionset.indices.clone(),
                deltas,
            )));
        }
    }
}

impl DeltaSet {
    fn push(&mut self, val: i32) {
        self.values.push(val)
    }
}

impl VariationIndexRemapping {
    fn set(&mut self, from: DeltaKey, to: DeltaKey) {
        self.map.insert(from, to);
    }

    fn get(&self, from: DeltaKey) -> Option<DeltaKey> {
        self.map.get(&from).copied()
    }

    /// remap a variation index table to its final position
    pub(crate) fn remap(&self, table: &mut VariationIndex) {
        let key = DeltaKey {
            outer: table.delta_set_outer_index,
            inner: table.delta_set_inner_index,
        };

        let resolved = self.get(key).unwrap();
        table.delta_set_outer_index = resolved.outer;
        table.delta_set_inner_index = resolved.inner;
    }
}

impl PartialEq<(u16, u16)> for DeltaKey {
    fn eq(&self, other: &(u16, u16)) -> bool {
        (self.outer, self.inner) == *other
    }
}

#[cfg(test)]
mod tests {
    use write_fonts::{read::FontRead, tables::variations::RegionAxisCoordinates, types::F2Dot14};

    use super::*;

    fn reg_coords(min: f32, default: f32, max: f32) -> RegionAxisCoordinates {
        RegionAxisCoordinates {
            start_coord: F2Dot14::from_f32(min),
            peak_coord: F2Dot14::from_f32(default),
            end_coord: F2Dot14::from_f32(max),
        }
    }

    fn test_regions() -> [VariationRegion; 3] {
        [
            VariationRegion::new(vec![reg_coords(0.0, 0.2, 1.0), reg_coords(0.0, 0.0, 1.0)]),
            VariationRegion::new(vec![reg_coords(0.0, 0.1, 0.3), reg_coords(0.0, 0.1, 0.3)]),
            VariationRegion::new(vec![reg_coords(0.0, 0.1, 0.5), reg_coords(0.0, 0.1, 0.3)]),
        ]
    }

    #[test]
    fn delta_keys() {
        impl DeltaKey {
            fn new(outer: u16, inner: u16) -> Self {
                Self { outer, inner }
            }
        }
        assert_eq!(
            DeltaKey::new(0xFFFE, 0xFFFE,).next(),
            DeltaKey::new(0xFFFE, 0xFFFD,)
        );

        assert_eq!(
            DeltaKey::new(0xFFFE, 0).next(),
            DeltaKey::new(0xFFFD, 0xFFFF)
        );
    }

    #[test]
    fn smoke_test() {
        let [r1, r2, r3] = test_regions();

        let mut builder = VariationStoreBuilder::default();
        builder.add_deltas(vec![(r1.clone(), 5), (r2, 10), (r3.clone(), 15)]);
        builder.add_deltas(vec![(r1, -3), (r3, 20)]);

        // we should have three regions, and two subtables
        let (store, _) = builder.build();
        assert_eq!(store.variation_region_list.variation_regions.len(), 3);
        assert_eq!(store.item_variation_data.len(), 2);
        assert_eq!(
            store.item_variation_data[1]
                .as_ref()
                .unwrap()
                .region_indexes,
            vec![0, 2]
        );
    }

    #[test]
    fn key_mapping() {
        let [r1, r2, r3] = test_regions();

        let mut builder = VariationStoreBuilder::default();
        let k1 = builder.add_deltas(vec![(r1.clone(), 5), (r2, 10), (r3.clone(), 15)]);
        let k2 = builder.add_deltas(vec![(r1.clone(), -3), (r3.clone(), 20)]);
        let k3 = builder.add_deltas(vec![(r1, -12), (r3, 7)]);

        // we should have three regions, and two subtables
        let (_, key_lookup) = builder.build();

        // first subtable has only one item
        assert_eq!(key_lookup.get(k1).unwrap(), (0, 0),);
        // next two items are in the next subtable (different outer index)
        assert_eq!(key_lookup.get(k2).unwrap(), (1, 0),);
        assert_eq!(key_lookup.get(k3).unwrap(), (1, 1),);

        assert_eq!(key_lookup.map.len(), 3);
    }

    #[test]
    fn to_binary() {
        let [r1, r2, r3] = test_regions();

        let mut builder = VariationStoreBuilder::default();
        builder.add_deltas(vec![(r1.clone(), 5), (r2, 10), (r3.clone(), 15)]);
        builder.add_deltas(vec![(r1.clone(), -3), (r3.clone(), 20)]);
        builder.add_deltas(vec![(r1, -12), (r3, 7)]);
        let (table, _) = builder.build();
        let bytes = write_fonts::dump_table(&table).unwrap();
        let data = write_fonts::read::FontData::new(&bytes);

        let reloaded =
            write_fonts::read::tables::variations::ItemVariationStore::read(data).unwrap();

        assert_eq!(reloaded.item_variation_data_count(), 2);
        let var_data_array = reloaded.item_variation_data();

        let var_data = var_data_array.get(0).unwrap().unwrap();
        assert_eq!(var_data.region_indexes(), &[0, 1, 2]);
        assert_eq!(var_data.item_count(), 1);
        assert_eq!(var_data.delta_set(0).collect::<Vec<_>>(), vec![5, 10, 15]);

        let var_data = var_data_array.get(1).unwrap().unwrap();
        assert_eq!(var_data.region_indexes(), &[0, 2]);
        assert_eq!(var_data.item_count(), 2);
        assert_eq!(var_data.delta_set(0).collect::<Vec<_>>(), vec![-3, 20]);
        assert_eq!(var_data.delta_set(1).collect::<Vec<_>>(), vec![-12, 7]);
    }

    #[test]
    fn reuse_identical_variation_data() {
        let [r1, r2, r3] = test_regions();

        let mut builder = VariationStoreBuilder::default();
        let k1 = builder.add_deltas(vec![(r1.clone(), 5), (r2, 10), (r3.clone(), 15)]);
        let k2 = builder.add_deltas(vec![(r1.clone(), -12), (r3.clone(), 7)]);
        let k3 = builder.add_deltas(vec![(r1.clone(), -12), (r3.clone(), 7)]);
        let k4 = builder.add_deltas(vec![(r1, 322), (r3, 532)]);

        // we should have three regions, and two subtables
        let (_, key_lookup) = builder.build();
        assert_eq!(k2, k3);
        assert_ne!(k1, k2);
        assert_ne!(k1, k4);
        assert_eq!(key_lookup.map.len(), 3);
    }
}
