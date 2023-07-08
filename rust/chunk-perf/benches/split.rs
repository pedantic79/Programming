use chunk_perf::*;
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

const ALPHA: &str = "abcdefghijklmnopqrstuvwxyz";

fn bench(c: &mut Criterion) {
    let mut bg = c.benchmark_group("split");

    for input in [ALPHA.to_string(), ALPHA.repeat(3), ALPHA.repeat(10)] {
        bg.bench_with_input(BenchmarkId::new("iter", input.len()), &input, |b, i| {
            b.iter(|| split_iter(i))
        });
        bg.bench_with_input(BenchmarkId::new("chunks", input.len()), &input, |b, i| {
            b.iter(|| split_chunks(i))
        });
    }
}

criterion_group!(benches, bench);
criterion_main!(benches);
