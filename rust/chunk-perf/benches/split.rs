
use chunk_perf::*;
use criterion::{criterion_group, criterion_main, Criterion, ParameterizedBenchmark};
fn bench(c: &mut Criterion) {
    c.bench(
        "Split",
        ParameterizedBenchmark::new(
            "iter",
            |b, i| b.iter(|| split_iter(*i)),
            vec!["abcdefghijklmnopqrstuvwxyz",
            "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz",
            ],
        )
        .with_function("chunks", |b, i| b.iter(|| split_chunks(*i))),
    );
}

criterion_group!(benches, bench);
criterion_main!(benches);
