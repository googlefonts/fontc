//! Extra helper methods on ValueRecord

use write_fonts::tables::{
    gpos::{AnchorTable, ValueFormat},
    layout::{Device, DeviceOrVariationIndex, PendingVariationIndex},
    variations::{ivs_builder::VariationStoreBuilder, VariationRegion},
};

/// A value record, possibly containing raw deltas or device tables
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ValueRecord {
    /// The x advance, plus a possible device table or set of deltas
    pub x_advance: Option<Metric>,
    /// The y advance, plus a possible device table or set of deltas
    pub y_advance: Option<Metric>,
    /// The x placement, plus a possible device table or set of deltas
    pub x_placement: Option<Metric>,
    /// The y placement, plus a possible device table or set of deltas
    pub y_placement: Option<Metric>,
}

/// An Anchor table, possibly containing deltas or a devices
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Anchor {
    /// The x coordinate, plus a possible device table or set of deltas
    pub x: Metric,
    /// The y coordinate, plus a possible device table or set of deltas
    pub y: Metric,
    /// The countourpoint, in a format 2 anchor.
    ///
    /// This is a rarely used format.
    pub contourpoint: Option<u16>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum DeviceOrDeltas {
    Device(Device),
    Deltas(Vec<(VariationRegion, i16)>),
    #[default]
    None,
}

/// A metric with optional device or variation information
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Metric {
    pub default: i16,
    pub device_or_deltas: DeviceOrDeltas,
}

impl ValueRecord {
    pub(crate) fn clear_zeros(mut self) -> Self {
        self.x_advance = self.x_advance.filter(|m| !m.is_zero());
        self.y_advance = self.y_advance.filter(|m| !m.is_zero());
        self.x_placement = self.x_placement.filter(|m| !m.is_zero());
        self.y_placement = self.y_placement.filter(|m| !m.is_zero());
        self
    }

    pub(crate) fn format(&self) -> ValueFormat {
        const EMPTY: ValueFormat = ValueFormat::empty();
        use ValueFormat as VF;

        let get_flags = |field: &Option<Metric>, def_flag, dev_flag| {
            let field = field.as_ref();
            let def_flag = if field.is_some() { def_flag } else { EMPTY };
            let dev_flag = field
                .and_then(|fld| (!fld.device_or_deltas.is_none()).then_some(dev_flag))
                .unwrap_or(EMPTY);
            (def_flag, dev_flag)
        };

        let (x_adv, x_adv_dev) = get_flags(&self.x_advance, VF::X_ADVANCE, VF::X_ADVANCE_DEVICE);
        let (y_adv, y_adv_dev) = get_flags(&self.y_advance, VF::Y_ADVANCE, VF::Y_ADVANCE_DEVICE);
        let (x_place, x_place_dev) =
            get_flags(&self.x_placement, VF::X_PLACEMENT, VF::X_PLACEMENT_DEVICE);
        let (y_place, y_place_dev) =
            get_flags(&self.y_placement, VF::Y_PLACEMENT, VF::Y_PLACEMENT_DEVICE);
        x_adv | y_adv | x_place | y_place | x_adv_dev | y_adv_dev | x_place_dev | y_place_dev
    }

    /// `true` if we are not null, but our set values are all 0
    fn is_all_zeros(&self) -> bool {
        let device_mask = ValueFormat::X_PLACEMENT_DEVICE
            | ValueFormat::Y_PLACEMENT_DEVICE
            | ValueFormat::X_ADVANCE_DEVICE
            | ValueFormat::Y_ADVANCE_DEVICE;

        let format = self.format();
        if format.is_empty() || format.intersects(device_mask) {
            return false;
        }
        let all_values = [
            &self.x_placement,
            &self.y_placement,
            &self.x_advance,
            &self.y_advance,
        ];
        all_values
            .iter()
            .all(|v| v.as_ref().map(|v| v.is_zero()).unwrap_or(true))
    }

    // Modify this value record for the special requirements of pairpos lookups
    //
    // In pair pos tables, if a value record is all zeros (but not null) then
    // we interpret it as a having a single zero advance in the x/y direction,
    // depending on context.
    pub(crate) fn for_pair_pos(self, in_vert_feature: bool) -> Self {
        if !self.is_all_zeros() {
            return self.clear_zeros();
        }
        let mut out = self.clear_zeros();
        if in_vert_feature {
            out.y_advance = Some(0.into());
        } else {
            out.x_advance = Some(0.into());
        }
        out
    }

    pub(crate) fn build(
        self,
        var_store: &mut VariationStoreBuilder,
    ) -> write_fonts::tables::gpos::ValueRecord {
        let mut result = write_fonts::tables::gpos::ValueRecord::new();
        result.x_advance = self.x_advance.as_ref().map(|val| val.default);
        result.y_advance = self.y_advance.as_ref().map(|val| val.default);
        result.x_placement = self.x_placement.as_ref().map(|val| val.default);
        result.y_placement = self.y_placement.as_ref().map(|val| val.default);
        result.x_advance_device = self
            .x_advance
            .and_then(|val| val.device_or_deltas.build(var_store))
            .into();
        result.y_advance_device = self
            .y_advance
            .and_then(|val| val.device_or_deltas.build(var_store))
            .into();
        result.x_placement_device = self
            .x_placement
            .and_then(|val| val.device_or_deltas.build(var_store))
            .into();
        result.y_placement_device = self
            .y_placement
            .and_then(|val| val.device_or_deltas.build(var_store))
            .into();

        result
    }
}

impl Anchor {
    pub(crate) fn build(self, var_store: &mut VariationStoreBuilder) -> AnchorTable {
        let x = self.x.default;
        let y = self.y.default;
        let x_dev = self.x.device_or_deltas.build(var_store);
        let y_dev = self.y.device_or_deltas.build(var_store);
        if x_dev.is_some() || y_dev.is_some() {
            AnchorTable::format_3(x, y, x_dev, y_dev)
        } else if let Some(point) = self.contourpoint {
            AnchorTable::format_2(x, y, point)
        } else {
            AnchorTable::format_1(x, y)
        }
    }
}

impl Metric {
    fn is_zero(&self) -> bool {
        self.default == 0 && !self.has_device_or_deltas()
    }

    pub fn has_device_or_deltas(&self) -> bool {
        !self.device_or_deltas.is_none()
    }
}

impl DeviceOrDeltas {
    fn is_none(&self) -> bool {
        *self == DeviceOrDeltas::None
    }

    fn build(self, var_store: &mut VariationStoreBuilder) -> Option<DeviceOrVariationIndex> {
        match self {
            DeviceOrDeltas::Device(dev) => Some(DeviceOrVariationIndex::Device(dev)),
            DeviceOrDeltas::Deltas(deltas) => {
                let temp_id = var_store.add_deltas(deltas);
                Some(DeviceOrVariationIndex::PendingVariationIndex(
                    PendingVariationIndex::new(temp_id),
                ))
            }
            DeviceOrDeltas::None => None,
        }
    }
}

impl From<i16> for Metric {
    fn from(src: i16) -> Metric {
        Metric {
            default: src,
            device_or_deltas: DeviceOrDeltas::None,
        }
    }
}

impl From<Option<Device>> for DeviceOrDeltas {
    fn from(src: Option<Device>) -> DeviceOrDeltas {
        src.map(DeviceOrDeltas::Device).unwrap_or_default()
    }
}
