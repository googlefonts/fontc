//! Building the ItemVariationStore

use std::collections::{BinaryHeap, HashMap};

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
    //TODO: we could store IndexMap<RowShape, IndexMap<DeltaSet, DeltaKey>> here,
    //if desired?
    delta_sets: IndexMap<DeltaSet, DeltaKey>,
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

/// Always sorted, so we can ensure equality
///
/// Each tuple is (region index, delta value)
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
struct DeltaSet(Vec<(u16, i32)>);

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
        let mut delta_set = Vec::with_capacity(deltas.len());
        for (region, delta) in deltas {
            let region_idx = self.index_for_region(region) as u16;
            delta_set.push((region_idx, delta as i32));
        }
        delta_set.sort_unstable();
        *self
            .delta_sets
            .entry(DeltaSet(delta_set))
            .or_insert_with(|| self.next_id.bump())
    }

    fn index_for_region(&mut self, region: VariationRegion) -> usize {
        let next_idx = self.all_regions.len();
        *self.all_regions.entry(region).or_insert(next_idx)
    }

    fn make_region_list(&self) -> VariationRegionList {
        let mut region_list = self
            .all_regions
            .iter()
            .map(|(reg, idx)| (idx, reg.to_owned()))
            .collect::<Vec<_>>();
        region_list.sort_unstable();
        let region_list = region_list.into_iter().map(|(_, reg)| reg).collect();
        VariationRegionList::new(region_list)
    }

    pub(crate) fn build(self) -> (ItemVariationStore, VariationIndexRemapping) {
        let mut shape = RowShape::default();
        let mut encodings: IndexMap<_, Vec<_>> = Default::default();
        let n_regions = self.all_regions.len() as u16;

        for delta in self.delta_sets.keys() {
            shape.reuse(delta, n_regions);
            match encodings.get_mut(&shape) {
                Some(items) => items.push(delta),
                None => {
                    encodings.insert(shape.clone(), vec![delta]);
                }
            }
        }
        let mut to_process = encodings
            .into_iter()
            .map(|(shape, deltas)| Some(Encoding { shape, deltas }))
            .collect::<Vec<_>>();

        // build up a priority list of the space savings from combining each pair
        // of encodings
        let mut queue = BinaryHeap::with_capacity(to_process.len());

        for ((i, red), (j, blue)) in to_process
            .iter()
            .enumerate()
            .zip(to_process.iter().enumerate().cycle().skip(1))
        {
            let gain = red.as_ref().unwrap().compute_gain(blue.as_ref().unwrap());
            if gain > 0 {
                queue.push((gain, i, j));
            }
        }

        // iteratively process each item in the queue
        while let Some((_, i, j)) = queue.pop() {
            // as items are combined, we leave `None` in the to_process list.
            // This ensures that indicies are stable.
            let (Some(mut to_update), Some(to_add)) = (
                to_process.get_mut(i).and_then(Option::take),
                to_process.get_mut(j).and_then(Option::take)
            ) else { continue };

            //NOTE: it is now possible that we have duplicate data. I'm not sure
            //how likely this is? probably not likely?
            to_update.merge_with(to_add);
            let n = to_process.len();
            let mut maybe_existing_encoding = None;
            for (ii, opt_encoding) in to_process.iter_mut().enumerate() {
                // does two things: skips empty indices, and also temporarily
                // removes the item (we'll put it back unless we merge, below)
                let Some(encoding) = opt_encoding.take() else { continue };

                if encoding.shape == to_update.shape {
                    // if an identical encoding exists in the list, we will just
                    // merge it with the newly created one. We do this after
                    // calculating the new gains, though, so we aren't changing
                    // anything mid-stream
                    maybe_existing_encoding = Some(encoding);
                    continue;
                }
                let gain = to_update.compute_gain(&encoding);
                if gain > 0 {
                    queue.push((gain, n, ii));
                }
                *opt_encoding = Some(encoding);
            }
            if let Some(existing) = maybe_existing_encoding.take() {
                to_update.deltas.extend(existing.deltas);
            }
            to_process.push(Some(to_update));
        }
        // we now have a vec of optimized encodings.
        // we need to turn these into ItemVariationData tables.
        // (probably one table per encoding, unless the coding contains > u16::max
        // items, in which case we'll split it)
        let mut key_map = VariationIndexRemapping::default();
        let subtables: Vec<_> = to_process
            .into_iter()
            .flatten()
            .flat_map(Encoding::iter_split_into_table_size_chunks)
            .enumerate()
            .map(|(i, encoding)| encoding.encode(&self.delta_sets, &mut key_map, i as u16))
            .collect();
        let region_list = self.make_region_list();
        (ItemVariationStore::new(region_list, subtables), key_map)
    }
}

struct Encoding<'a> {
    shape: RowShape,
    deltas: Vec<&'a DeltaSet>,
}

/// A type for remapping delta sets during encoding.
struct RegionMap {
    map: Vec<(u16, ColumnBits)>,
    n_active_regions: u16,
    n_long_regions: u16,
    long_words: bool,
}

/// Describes the compressability of a row of deltas across all variation regions.
///
/// fonttools calls this the 'characteristic' of a row.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
struct RowShape(Vec<ColumnBits>);

//NOTE: we could do fancier bit packing here (fonttools uses four bits per
//column but I think the gains will be marginal)
/// The number of bits required to represent a given delta column.
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ColumnBits {
    /// i.e. the value is zero
    None = 0,
    /// an i8
    One = 1,
    /// an i16
    Two = 2,
    /// an i32
    Four = 4,
}

impl ColumnBits {
    fn for_val(val: i32) -> Self {
        if val == 0 {
            Self::None
        } else if i8::try_from(val).is_ok() {
            Self::One
        } else if i16::try_from(val).is_ok() {
            Self::Two
        } else {
            Self::Four
        }
    }

    /// The number of bytes required to store this column
    fn cost(self) -> usize {
        self as u8 as _
    }
}

impl RowShape {
    /// Reuse this types storage for a new delta set.
    ///
    /// This might be premature optimization.
    ///
    /// The rationale is that many of these are identical, so this saves us
    /// from constantly allocating and throwing away.
    fn reuse(&mut self, deltas: &DeltaSet, n_regions: u16) {
        self.0.clear();
        self.0.resize(n_regions as _, ColumnBits::None);
        for (region, delta) in &deltas.0 {
            self.0[*region as usize] = ColumnBits::for_val(*delta);
        }
    }

    /// Returns a shape that can fit both self and other.
    ///
    /// In practice this means taking the max of each column.
    fn merge(&self, other: &Self) -> Self {
        Self(
            self.0
                .iter()
                .zip(other.0.iter())
                .map(|(us, them)| *us.max(them))
                .collect(),
        )
    }

    /// the cost in bytes of a row in this encoding
    fn row_cost(&self) -> usize {
        self.0.iter().copied().map(ColumnBits::cost).sum()
    }

    fn overhead(&self) -> usize {
        /// the minimum number of bytes in an ItemVariationData table
        const SUBTABLE_FIXED_COST: usize = 10;
        const COST_PER_REGION: usize = 2;
        SUBTABLE_FIXED_COST + (self.n_non_zero_regions() * COST_PER_REGION)
    }

    fn n_non_zero_regions(&self) -> usize {
        self.0.iter().map(|x| (*x as u8).min(1) as usize).sum()
    }

    /// return a tuple for the counts of (1, 2, 3) byte-encoded items in self
    fn count_lengths(&self) -> (u16, u16, u16) {
        self.0
            .iter()
            .fold((0, 0, 0), |(byte, short, long), this| match this {
                ColumnBits::One => (byte + 1, short, long),
                ColumnBits::Two => (byte, short + 1, long),
                ColumnBits::Four => (byte, short, long + 1),
                _ => (byte, short, long),
            })
    }

    /// we reorder regions so that all the 'word' columns are at the front.
    ///
    /// The returns a vec where for each region `x`, vec[x] is the final position
    /// of that region.
    fn region_map(&self) -> RegionMap {
        let mut with_idx = self.0.iter().copied().enumerate().collect::<Vec<_>>();
        // sort in descending order of bit size, e.g. big first
        with_idx.sort_unstable_by_key(|(idx, bit)| (std::cmp::Reverse(*bit), *idx));
        // now build a map of indexes from the original positions to the new ones.
        let mut map = vec![(0u16, ColumnBits::None); with_idx.len()];
        for (new_idx, (old_idx, bits)) in with_idx.iter().enumerate() {
            map[*old_idx] = (new_idx as _, *bits);
        }

        let (count_8, count_16, count_32) = self.count_lengths();
        let long_words = count_32 > 0;
        let n_long_regions = if long_words { count_32 } else { count_16 };
        let n_active_regions = count_8 + count_16 + count_32;
        RegionMap {
            map,
            n_active_regions,
            n_long_regions,
            long_words,
        }
    }
}

impl<'a> Encoding<'a> {
    fn cost(&self) -> usize {
        self.shape.overhead() + (self.shape.row_cost() * self.deltas.len())
    }

    fn compute_gain(&self, other: &Encoding) -> i64 {
        let current_cost = self.cost() + other.cost();

        let combined = self.shape.merge(&other.shape);
        let combined_cost =
            combined.overhead() + (combined.row_cost() * (self.deltas.len() + other.deltas.len()));
        combined_cost as i64 - current_cost as i64
    }

    fn merge_with(&mut self, other: Encoding<'a>) {
        self.shape = self.shape.merge(&other.shape);
        self.deltas.extend(other.deltas);
    }

    /// Split this item into chunks that fit in an ItemVariationData subtable.
    ///
    /// we can only encode up to u16::MAX items in a single subtable, so if we
    /// have more items than that we split them off now.
    fn iter_split_into_table_size_chunks(self) -> impl Iterator<Item = Encoding<'a>> {
        let mut next = Some(self);
        std::iter::from_fn(move || {
            let mut this = next.take()?;
            next = this.split_off_back();
            Some(this)
        })
    }

    /// If we contain more than the max allowed items, split the extra items off
    ///
    /// This ensures `self` can be encoded.
    fn split_off_back(&mut self) -> Option<Self> {
        const MAX_ITEMS: usize = 0xFFFF;
        if self.deltas.len() <= MAX_ITEMS {
            return None;
        }
        let deltas = self.deltas.split_off(MAX_ITEMS);
        Some(Self {
            shape: self.shape.clone(),
            deltas,
        })
    }

    fn encode(
        self,
        delta_ids: &IndexMap<DeltaSet, DeltaKey>,
        key_map: &mut VariationIndexRemapping,
        subtable_idx: u16,
    ) -> Option<ItemVariationData> {
        assert!(self.deltas.len() <= 0xffff, "call split_off_back first");
        let item_count = self.deltas.len() as u16;
        if item_count == 0 {
            //TODO: figure out when a null subtable is useful?
            return None;
        }

        // what is tricky is that the deltasets can have an arbitrary number
        // of different regionsets, contain a different number of regions, etc.
        // what would be easiest is to encode to an intermediate representation
        // (where all items are the same size) so we can index into it, instead
        // of encoding directly (since we have a variable length encoding, and
        // so can't use indexes with the final data)
        //
        // so... we can actually stay in i32 for this first stage?
        let region_map = self.shape.region_map();
        let n_regions = self.shape.n_non_zero_regions();
        let total_n_delta_values = self.deltas.len() * n_regions;
        let mut raw_deltas = vec![0i32; total_n_delta_values];

        // first we generate a vec of i32s, which represents an uncompressed
        // 2d array where rows are items and columns are per-region values.
        for (i, delta) in self.deltas.iter().enumerate() {
            let pos = i * n_regions;
            for (region, val) in &delta.0 {
                let idx_for_region = region_map.map[*region as usize].0;
                let idx = pos + idx_for_region as usize;
                raw_deltas[idx] = *val;
            }
            let raw_key = delta_ids.get(*delta).unwrap();
            let final_key = DeltaKey {
                outer: subtable_idx,
                inner: i as u16,
            };
            key_map.set(*raw_key, final_key);
        }

        // then
        let delta_sets = region_map.encode_raw_delta_values(raw_deltas);
        let word_delta_count = region_map.word_delta_count();
        let region_indexes = region_map.indices();

        Some(ItemVariationData::new(
            item_count,
            word_delta_count,
            region_indexes,
            delta_sets,
        ))
    }
}

impl RegionMap {
    /// Takes the delta data as a vec of i32s, writes a vec of BigEndian bytes.
    ///
    /// This is mostly boilerplate around whether we are writing i16 and i8, or
    /// i32 and i16.
    fn encode_raw_delta_values(&self, raw_deltas: Vec<i32>) -> Vec<u8> {
        // handles the branching logic of whether long words are 32 or 16 bits.
        fn encode_words<'a>(
            long: &'a [i32],
            short: &'a [i32],
            long_words: bool,
        ) -> impl Iterator<Item = u8> + 'a {
            // dumb trick: the two branches have differnet concrete types,
            // so we need to unify them
            let mut left = None;
            let mut right = None;
            if long_words {
                left = Some(
                    long.iter()
                        .flat_map(|x| x.to_be_bytes().into_iter())
                        .chain(short.iter().flat_map(|x| (*x as i16).to_be_bytes())),
                );
            } else {
                right = Some(
                    long.iter()
                        .flat_map(|x| (*x as i16).to_be_bytes().into_iter())
                        .chain(short.iter().flat_map(|x| (*x as i8).to_be_bytes())),
                );
            }

            // combine the two branches into a single type
            left.into_iter()
                .flatten()
                .chain(right.into_iter().flatten())
        }

        raw_deltas
            .chunks(self.n_active_regions as usize)
            .flat_map(|delta_set| {
                let (long, short) = delta_set.split_at(self.n_long_regions as usize);
                encode_words(long, short, self.long_words)
            })
            .collect()
    }

    /// Compute the 'wordDeltaCount' field
    ///
    /// This is a packed field, with the high bit indicating if we have 2-or-4-bit
    /// words, and the low 15 bits indicating the number of 'long' types
    fn word_delta_count(&self) -> u16 {
        let long_flag = if self.long_words { 0x8000 } else { 0 };
        self.n_long_regions | long_flag
    }

    /// the indexes into the canonical region list of the active regions
    fn indices(&self) -> Vec<u16> {
        self.map
            .iter()
            .enumerate()
            .filter_map(|(i, (_, bits))| (*bits as u8 > 0).then_some(i as _))
            .collect()
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
            store.item_variation_data[0]
                .as_ref()
                .unwrap()
                .region_indexes,
            vec![0, 1, 2]
        );
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
