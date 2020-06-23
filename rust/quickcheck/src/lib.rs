#![allow(dead_code)]

fn mod10_vanishes(n: u32) -> bool {
    n - ((n / 10) * 10) == 0
}

fn mod10(n: u32, zeroth: bool) -> bool {
    if n == 0 {
        zeroth
    } else {
        n % 10 == 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck_macros::quickcheck;

    #[quickcheck]
    fn same(n: u32) -> bool {
        mod10(n, true) == mod10_vanishes(n)
    }

    #[quickcheck]
    fn fail(n: u32) -> bool {
        mod10(n, false) == mod10_vanishes(n)
    }
}
