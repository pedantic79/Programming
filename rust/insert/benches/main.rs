#[macro_use]
extern crate criterion;
extern crate linked_list;
extern crate rand;

use criterion::Criterion;
use linked_list::LinkedList;
use rand::Rng;

fn insert_vect(input: &Vec<i32>) -> Vec<i32> {
    let mut output = Vec::new();

    for n in input {
        let mut pos = None;
        for (i, item) in output.iter().enumerate() {
            if item > n {
                pos = Some(i);
                break;
            }
        }

        match pos {
            Some(position) => output.insert(position, *n),
            None => output.push(*n),
        }
    }

    return output;
}

fn insert_list(input: &Vec<i32>) -> LinkedList<i32> {
    let mut output = LinkedList::new();

    for n in input {
        let mut cursor = output.cursor();

        loop {
            match cursor.peek_next() {
                Some(value) => if *value > *n {
                    break;
                },
                None => break,
            }
            cursor.next();
        }
        cursor.insert(*n);
    }

    return output;
}

fn random(size: i32) -> Vec<i32> {
    let mut rng = rand::thread_rng();
    let numbers = (0..size).map(|_| rng.gen::<i32>()).collect();
    return numbers;
}

fn bench(c: &mut Criterion, size: i32) {
    let vec1 = random(size);
    let vec2 = vec1.clone();
    let vstr = format!("vect {}", size);
    let lstr = format!("list {}", size);

    c.bench_function(&vstr, move |b| b.iter(|| insert_vect(&vec1)));
    c.bench_function(&lstr, move |b| b.iter(|| insert_list(&vec2)));
}

fn regression(v: &Vec<i32>) {
    let out = insert_vect(v);
    assert_eq!(out, [1,2,3,4,5]);

    let out = insert_list(v);
    let mut iter = out.iter();
    assert_eq!(iter.next(), Some(&1));
    assert_eq!(iter.next(), Some(&2));
    assert_eq!(iter.next(), Some(&3));
    assert_eq!(iter.next(), Some(&4));
    assert_eq!(iter.next(), Some(&5));
    assert!(iter.next().is_none());
}

fn test(c: &mut Criterion) {
    let vec = vec![5, 4, 3, 2, 1];
    c.bench_function("test", move |b| b.iter(|| regression(&vec)));
}

fn criterion_benchmark(c: &mut Criterion) {
    for n in vec![10, 100, 1000] {
        bench(c, n);
    }
    test(c);
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
