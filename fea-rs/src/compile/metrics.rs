//! Variable-first metrics, ValueRecords & Anchors

use write_fonts::tables::{
    gdef::CaretValue as RawCaretValue,
    gpos::{AnchorTable, ValueFormat},
    layout::{Device, DeviceOrVariationIndex, PendingVariationIndex},
    variations::{ivs_builder::VariationStoreBuilder, VariationRegion},
};

/// A value record, possibly containing raw deltas or device tables
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ValueRecord {
    /// The x advance, plus a possible device table or set of deltas
    pub(crate) x_advance: Option<Metric>,
    /// The y advance, plus a possible device table or set of deltas
    pub(crate) y_advance: Option<Metric>,
    /// The x placement, plus a possible device table or set of deltas
    pub(crate) x_placement: Option<Metric>,
    /// The y placement, plus a possible device table or set of deltas
    pub(crate) y_placement: Option<Metric>,
}

/// An Anchor table, possibly containing deltas or a devices
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Anchor {
    /// The x coordinate, plus a possible device table or set of deltas
    pub(crate) x: Metric,
    /// The y coordinate, plus a possible device table or set of deltas
    pub(crate) y: Metric,
    /// The countourpoint, in a format 2 anchor.
    ///
    /// This is a rarely used format.
    pub(crate) contourpoint: Option<u16>,
}

/// Either a `Device` table or a set of deltas
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[allow(missing_docs)]
pub enum DeviceOrDeltas {
    Device(Device),
    Deltas(Vec<(VariationRegion, i16)>),
    #[default]
    None,
}

/// A metric with optional device or variation information
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub(crate) struct Metric {
    /// The value at the default location
    pub default: i16,
    /// An optional device table or delta set
    pub device_or_deltas: DeviceOrDeltas,
}

/// A value in the GDEF ligature caret list
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum CaretValue {
    /// An X or Y value (in design units) with optional deltas
    Coordinate {
        /// The value at the default location
        default: i16,
        /// An optional device table or delta set
        deltas: DeviceOrDeltas,
    },
    /// The index of a contour point to be used as the caret location.
    ///
    /// This format is rarely used.
    PointIndex(u16),
}

impl ValueRecord {
    /// Create a new all-zeros ValueRecord
    pub fn new() -> Self {
        Default::default()
    }

    /// Duplicates the x-advance value to x-placement, required for RTL rules.
    ///
    /// This is only necessary when a record was originally created without
    /// knowledge of the writing direction, and then later needs to be modified.
    pub fn make_rtl_compatible(&mut self) {
        if self.x_placement.is_none() {
            self.x_placement.clone_from(&self.x_advance);
        }
    }

    // these methods just match the existing builder methods on `ValueRecord`
    /// Builder style method to set the default x_placement value
    pub fn with_x_placement(mut self, val: i16) -> Self {
        self.x_placement
            .get_or_insert_with(Default::default)
            .default = val;
        self
    }

    /// Builder style method to set the default y_placement value
    pub fn with_y_placement(mut self, val: i16) -> Self {
        self.y_placement
            .get_or_insert_with(Default::default)
            .default = val;
        self
    }

    /// Builder style method to set the default x_placement value
    pub fn with_x_advance(mut self, val: i16) -> Self {
        self.x_advance.get_or_insert_with(Default::default).default = val;
        self
    }

    /// Builder style method to set the default y_placement value
    pub fn with_y_advance(mut self, val: i16) -> Self {
        self.y_advance.get_or_insert_with(Default::default).default = val;
        self
    }

    /// Builder style method to set the device or deltas for x_placement
    ///
    /// The argument can be a `Device` table or a `Vec<(VariationRegion, i16)>`
    pub fn with_x_placement_device(mut self, val: impl Into<DeviceOrDeltas>) -> Self {
        self.x_placement
            .get_or_insert_with(Default::default)
            .device_or_deltas = val.into();
        self
    }

    /// Builder style method to set the device or deltas for y_placement
    ///
    /// The argument can be a `Device` table or a `Vec<(VariationRegion, i16)>`
    pub fn with_y_placement_device(mut self, val: impl Into<DeviceOrDeltas>) -> Self {
        self.y_placement
            .get_or_insert_with(Default::default)
            .device_or_deltas = val.into();
        self
    }

    /// Builder style method to set the device or deltas for x_advance
    ///
    /// The argument can be a `Device` table or a `Vec<(VariationRegion, i16)>`
    pub fn with_x_advance_device(mut self, val: impl Into<DeviceOrDeltas>) -> Self {
        self.x_advance
            .get_or_insert_with(Default::default)
            .device_or_deltas = val.into();
        self
    }

    /// Builder style method to set the device or deltas for y_advance
    ///
    /// The argument can be a `Device` table or a `Vec<(VariationRegion, i16)>`
    pub fn with_y_advance_device(mut self, val: impl Into<DeviceOrDeltas>) -> Self {
        self.y_advance
            .get_or_insert_with(Default::default)
            .device_or_deltas = val.into();
        self
    }

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
    pub fn is_all_zeros(&self) -> bool {
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
    /// Create a new anchor.
    pub fn new(x: i16, y: i16) -> Self {
        Anchor {
            x: x.into(),
            y: y.into(),
            contourpoint: None,
        }
    }

    /// Builder style method to set the device or deltas for the x value
    ///
    /// The argument can be a `Device` table or a `Vec<(VariationRegion, i16)>`
    pub fn with_x_device(mut self, val: impl Into<DeviceOrDeltas>) -> Self {
        self.x.device_or_deltas = val.into();
        self
    }

    /// Builder style method to set the device or deltas for the y value
    ///
    /// The argument can be a `Device` table or a `Vec<(VariationRegion, i16)>`
    pub fn with_y_device(mut self, val: impl Into<DeviceOrDeltas>) -> Self {
        self.y.device_or_deltas = val.into();
        self
    }

    /// Builder-style method to set the contourpoint.
    ///
    /// This is for the little-used format2 AnchorTable; it will be ignored
    /// if any device or deltas have been set.
    pub fn with_contourpoint(mut self, idx: u16) -> Self {
        self.contourpoint = Some(idx);
        self
    }

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

impl CaretValue {
    // var_store is an Option here because this is built at the very end and
    // lives in GDEF alongside the varstore, so if it's not a variable font we won't
    // have access to a var store builder at that point.
    pub(crate) fn build(self, var_store: &mut VariationStoreBuilder) -> RawCaretValue {
        match self {
            CaretValue::Coordinate { default, deltas } => match deltas.build(var_store) {
                Some(deltas) => RawCaretValue::format_3(default, deltas),
                None => RawCaretValue::format_1(default),
            },
            CaretValue::PointIndex(index) => RawCaretValue::format_2(index),
        }
    }
}

impl Metric {
    fn is_zero(&self) -> bool {
        self.default == 0 && !self.has_device_or_deltas()
    }

    /// `true` if this metric has either a device table or deltas
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

impl From<Device> for DeviceOrDeltas {
    fn from(value: Device) -> Self {
        DeviceOrDeltas::Device(value)
    }
}

impl From<Vec<(VariationRegion, i16)>> for DeviceOrDeltas {
    fn from(src: Vec<(VariationRegion, i16)>) -> DeviceOrDeltas {
        if src.is_empty() {
            DeviceOrDeltas::None
        } else {
            DeviceOrDeltas::Deltas(src)
        }
    }
}
