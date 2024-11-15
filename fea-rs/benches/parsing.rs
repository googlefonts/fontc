//! A very simple benchmark for the parser

use std::sync::Arc;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

const DEVA: &str = include_str!("../test-data/real-files/plex_devanagari.fea");
const LATN: &str = include_str!("../test-data/real-files/roboto-regular.fea");
const ARAB: &str = include_str!("../test-data/real-files/tajawal-regular.fea");

fn parse_source(source: Arc<str>) -> fea_rs::ParseTree {
    fea_rs::parse::parse_string(source).0
}

fn parsing(c: &mut Criterion) {
    let deva: Arc<str> = DEVA.into();
    let latn: Arc<str> = LATN.into();
    let arab: Arc<str> = ARAB.into();
    c.bench_function("parse plex-devenagari", |b| {
        b.iter(|| parse_source(black_box(deva.clone())))
    });
    c.bench_function("parse roboto-regular", |b| {
        b.iter(|| parse_source(black_box(latn.clone())))
    });
    c.bench_function("parse tajawal-regular", |b| {
        b.iter(|| parse_source(black_box(arab.clone())))
    });
}

criterion_group!(benches, parsing);
criterion_main!(benches);
