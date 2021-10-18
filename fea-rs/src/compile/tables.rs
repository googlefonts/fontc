/// The explicit tables allowed in a fea file
#[derive(Clone, Debug, Default)]
struct Tables {
    head: Option<fea_tables::head>,
    hhea: Option<fea_tables::hhea>,
    vhea: Option<fea_tables::vhea>,
    //name: Option<tables::name>,
    //OS2: Option<tables::OS2>,
    //STAT: Option<tables::STAT>,
}
#[derive(Debug, Clone)]
#[allow(non_camel_case_types)]
pub struct head {
    font_revision: u16,
}

#[derive(Debug, Clone)]
#[allow(non_camel_case_types)]
pub struct hhea {
    caret_offset: i32,
    ascender: i32,
    descender: i32,
    line_gap: i32,
}

#[derive(Debug, Clone)]
#[allow(non_camel_case_types)]
pub struct vhea {
    vert_typo_ascender: i32,
    vert_typo_descender: i32,
    vert_typo_line_gap: i32,
}
