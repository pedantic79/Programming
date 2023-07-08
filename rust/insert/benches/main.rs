use criterion::{
    criterion_group, criterion_main, measurement::Measurement, BenchmarkGroup, BenchmarkId,
    Criterion,
};
use insert::*;
use rand::Rng;

fn random(size: usize) -> Vec<i32> {
    let mut rng = rand::thread_rng();
    (0..size).map(|_| rng.gen()).collect()
}

fn create_bench<T, M: Measurement>(
    c: &mut BenchmarkGroup<M>,
    f: &'static dyn for<'r> Fn(&'r [i32]) -> T,
    data: &[Vec<i32>],
) {
    for d in data {
        let vec = d.to_owned();

        c.bench_function(BenchmarkId::from_parameter(d.len()), move |b| {
            b.iter(|| f(&vec))
        });
    }
}

fn bench<T>(
    c: &mut Criterion,
    name: &str,
    f: &'static dyn for<'r> Fn(&'r [i32]) -> T,
    data: &[Vec<i32>],
) {
    let mut bg = c.benchmark_group(name);
    create_bench(&mut bg, f, data);
    bg.finish();
}

fn criterion_benchmark(c: &mut Criterion) {
    let data = [random(10), random(100), random(1000)];

    bench(c, "vec", &insert_vect, &data);
    bench(c, "list", &insert_list, &data);
    bench(c, "func", &insert_vect_func, &data);
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
