use write_fonts::read::{
    tables::{layout::VariationIndex, variations::ItemVariationStore},
    types::F2Dot14,
    ReadError,
};

type MasterLocations = Vec<Vec<F2Dot14>>;

#[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Value {
    pub default: i16,
    /// Values for each master
    pub variations: Option<Vec<i32>>,
}

impl From<i16> for Value {
    fn from(value: i16) -> Self {
        Value {
            default: value,
            variations: None,
        }
    }
}

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

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.default)?;
        if let Some(vars) = &self.variations {
            write!(f, " {{")?;
            for (i, var) in vars.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{var}")?;
            }
            write!(f, "}}")?;
        }
        Ok(())
    }
}
