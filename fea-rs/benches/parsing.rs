//! A very simple benchmark for the parser

use criterion::{black_box, criterion_group, criterion_main, Criterion};

const DEVA: &str = include_str!("../test-data/real-files/plex_devanagari.fea");
const LATN: &str = include_str!("../test-data/real-files/roboto-regular.fea");
const ARAB: &str = include_str!("../test-data/real-files/tajawal-regular.fea");

fn parse_source(source: &fea_rs::Source) -> fea_rs::Node {
    fea_rs::parse_src(source, None).0
}

fn parsing(c: &mut Criterion) {
    let deva = fea_rs::Source::from_text(DEVA);
    let latn = fea_rs::Source::from_text(LATN);
    let arab = fea_rs::Source::from_text(ARAB);
    c.bench_function("parse plex-devenagari", |b| {
        b.iter(|| parse_source(black_box(&deva)))
    });
    c.bench_function("parse roboto-regular", |b| {
        b.iter(|| parse_source(black_box(&latn)))
    });
    c.bench_function("parse tajawal-regular", |b| {
        b.iter(|| parse_source(black_box(&arab)))
    });
}

criterion_group!(benches, parsing);
criterion_main!(benches);
