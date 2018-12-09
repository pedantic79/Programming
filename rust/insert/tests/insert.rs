use insert::*;

#[test]
fn regression() {
    let v = &vec![5, 4, 3, 2, 1];

    let out = insert_vect(v);
    assert_eq!(out, [1, 2, 3, 4, 5]);

    let out = insert_list(v);
    let mut iter = out.iter();
    assert_eq!(iter.next(), Some(&1));
    assert_eq!(iter.next(), Some(&2));
    assert_eq!(iter.next(), Some(&3));
    assert_eq!(iter.next(), Some(&4));
    assert_eq!(iter.next(), Some(&5));
    assert!(iter.next().is_none());
}
