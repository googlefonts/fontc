//! A very simple benchmark for the parser

use std::{hint::black_box, sync::Arc};

use criterion::{Criterion, criterion_group, criterion_main};

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
    let mut bench_group = c.benchmark_group("fea-parse");
    bench_group.bench_function("plex-devenagari", |b| {
        b.iter(|| parse_source(black_box(deva.clone())))
    });
    bench_group.bench_function("roboto-regular", |b| {
        b.iter(|| parse_source(black_box(latn.clone())))
    });
    bench_group.bench_function("tajawal-regular", |b| {
        b.iter(|| parse_source(black_box(arab.clone())))
    });
}

criterion_group!(benches, parsing);
criterion_main!(benches);
