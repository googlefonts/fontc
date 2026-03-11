use std::collections::BTreeSet;

use write_fonts::read::{
    ReadError,
    tables::{layout::VariationIndex, variations::ItemVariationStore},
    types::F2Dot14,
};

type MasterLocations = Vec<Vec<F2Dot14>>;

pub(crate) struct DeltaComputer<'a> {
    ivs: ItemVariationStore<'a>,
    locations: MasterLocations,
}

impl<'a> DeltaComputer<'a> {
    pub(crate) fn new(
        ivs: ItemVariationStore<'a>,
        used_subtables: Option<&BTreeSet<u16>>,
    ) -> Result<Self, ReadError> {
        let region_list = ivs.variation_region_list()?;

        // Collect the set of global region indices actually referenced by the
        // requested IVS subtables.  When None we keep all regions.
        let used_regions: Option<BTreeSet<usize>> = used_subtables
            .map(|outers| {
                let mut set = BTreeSet::new();
                for &outer in outers {
                    if let Some(data) = ivs.item_variation_data().get(outer as usize) {
                        for idx in data?.region_indexes() {
                            set.insert(idx.get() as usize);
                        }
                    }
                }
                Ok(set)
            })
            .transpose()?;

        let mut locations: MasterLocations = region_list
            .variation_regions()
            .iter()
            .enumerate()
            .filter(|(idx, _)| used_regions.as_ref().is_none_or(|set| set.contains(idx)))
            .map(|(_, region)| {
                let region = region?;
                Ok(region
                    .region_axes()
                    .iter()
                    .map(|axis| axis.peak_coord())
                    .collect())
            })
            .collect::<Result<_, ReadError>>()?;
        locations.sort();
        Ok(DeltaComputer { ivs, locations })
    }

    pub(crate) fn master_values(
        &self,
        coord: i32,
        idx: VariationIndex,
    ) -> Result<Vec<i32>, ReadError> {
        let delta_ix = idx.into();
        self.locations
            .iter()
            .map(|loc| self.ivs.compute_delta(delta_ix, loc).map(|d| d + coord))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use write_fonts::{
        dump_table,
        read::{FontData, FontRead},
        tables::variations::{
            ItemVariationData, ItemVariationStore as WriteIvs, RegionAxisCoordinates,
            VariationRegion, VariationRegionList,
        },
        types::F2Dot14,
    };

    fn region_1axis(peak: f32) -> VariationRegion {
        VariationRegion::new(vec![RegionAxisCoordinates {
            start_coord: F2Dot14::from_f32(0.0),
            peak_coord: F2Dot14::from_f32(peak),
            end_coord: F2Dot14::from_f32(1.0),
        }])
    }

    /// Build an IVS with two subtables referencing different subsets of
    /// a 3-region global VariationRegionList:
    ///   - subtable 0 references regions 0 and 1 (peaks 0.5, 1.0)
    ///   - subtable 1 references region 2 only (peak 0.75)
    fn build_multi_subtable_ivs() -> Vec<u8> {
        let regions = VariationRegionList::new(
            1,
            vec![region_1axis(0.5), region_1axis(1.0), region_1axis(0.75)],
        );
        // subtable 0: 1 item, 2 regions (indices 0, 1), deltas [10, 20]
        let sub0 = ItemVariationData::new(1, 0, vec![0, 1], vec![10, 20]);
        // subtable 1: 1 item, 1 region (index 2), delta [30]
        let sub1 = ItemVariationData::new(1, 0, vec![2], vec![30]);
        let store = WriteIvs::new(regions, vec![Some(sub0), Some(sub1)]);
        dump_table(&store).unwrap()
    }

    #[test]
    fn no_filter_includes_all_regions() {
        let bytes = build_multi_subtable_ivs();
        let ivs = ItemVariationStore::read(FontData::new(&bytes)).unwrap();
        let computer = DeltaComputer::new(ivs, None).unwrap();
        assert_eq!(computer.locations.len(), 3);
    }

    #[test]
    fn filter_to_subtable_0_excludes_unrelated_regions() {
        let bytes = build_multi_subtable_ivs();
        let ivs = ItemVariationStore::read(FontData::new(&bytes)).unwrap();
        let used = BTreeSet::from([0u16]);
        let computer = DeltaComputer::new(ivs, Some(&used)).unwrap();
        assert_eq!(computer.locations.len(), 2);
    }

    #[test]
    fn filter_to_subtable_1_excludes_unrelated_regions() {
        let bytes = build_multi_subtable_ivs();
        let ivs = ItemVariationStore::read(FontData::new(&bytes)).unwrap();
        let used = BTreeSet::from([1u16]);
        let computer = DeltaComputer::new(ivs, Some(&used)).unwrap();
        assert_eq!(computer.locations.len(), 1);
    }

    #[test]
    fn filter_union_of_subtables_includes_all() {
        let bytes = build_multi_subtable_ivs();
        let ivs = ItemVariationStore::read(FontData::new(&bytes)).unwrap();
        let used = BTreeSet::from([0u16, 1u16]);
        let computer = DeltaComputer::new(ivs, Some(&used)).unwrap();
        assert_eq!(computer.locations.len(), 3);
    }
}
