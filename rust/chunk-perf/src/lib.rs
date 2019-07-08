use std::iter::once;

pub fn split_iter(s: &str) -> String {
    s.chars()
        .enumerate()
        .flat_map(|(idx, c)| {
            if idx % 5 == 0 && idx > 0 {
                Some(' ')
            } else {
                None
            }
            .into_iter()
            .chain(once(c))
        })
        .collect()
}

pub fn split_chunks(s: &str) -> String {
    s.chars()
        .collect::<Vec<char>>()
        .chunks(5)
        .map(|group| group.iter().collect::<String>())
        .collect::<Vec<String>>()
        .join(" ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(split_iter("abcdef"), String::from("abcde f"));
        assert_eq!(split_chunks("abcdef"), String::from("abcde f"));
    }
}
