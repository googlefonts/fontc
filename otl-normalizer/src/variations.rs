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
    pub(crate) fn new(ivs: ItemVariationStore<'a>) -> Result<Self, ReadError> {
        let mut locations = vec![];
        let region_list = ivs.variation_region_list()?;
        for region in region_list.variation_regions().iter() {
            let region = region?;
            locations.push(
                region
                    .region_axes()
                    .iter()
                    .map(|axis| axis.peak_coord())
                    .collect(),
            );
        }
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
