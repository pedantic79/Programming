use criterion::{criterion_group, criterion_main, Criterion};
use insert::*;
use rand::Rng;

fn random(size: usize) -> Vec<i32> {
    let mut rng = rand::thread_rng();
    (0..size).map(|_| rng.gen()).collect()
}

fn create_bench<T>(
    c: &mut Criterion,
    name: &str,
    f: &'static (dyn for<'r> Fn(&'r [i32]) -> T),
    v: &[i32],
) {
    let vec = v.to_owned();
    let vstr = format!("{} {}", name, v.len());

    c.bench_function(&vstr, move |b| b.iter(|| f(&vec)));
}

fn bench(c: &mut Criterion, size: usize) {
    let vec1 = random(size);

    create_bench(c, "vec", &insert_vect, &vec1);
    // create_bench(c, "list", &insert_list, &vec1);
    create_bench(c, "func", &insert_vect_func, &vec1);
}

fn criterion_benchmark(c: &mut Criterion) {
    for n in &[10, 100, 1000] {
        bench(c, *n);
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
