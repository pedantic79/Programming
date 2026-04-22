use linked_list::LinkedList;

pub fn insert_vect(input: &[i32]) -> Vec<i32> {
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

    output
}

pub fn insert_vect_func(input: &[i32]) -> Vec<i32> {
    let mut output = Vec::new();

    for n in input {
        match output
            .iter()
            .enumerate()
            .filter(|(_, &item)| item > *n)
            .map(|(i, _)| i)
            .next()
        {
            Some(position) => output.insert(position, *n),
            None => output.push(*n),
        }
    }

    output
}

pub fn insert_list(input: &[i32]) -> LinkedList<i32> {
    let mut output = LinkedList::new();

    for n in input {
        let mut cursor = output.cursor_mut();

        while let Some(value) = cursor.peek_next() {
            if *value > *n {
                cursor.move_next();
                break;
            }
            cursor.move_next();
        }
        let mut l = LinkedList::new();
        l.push_back(*n);
        cursor.splice_before(l);
    }

    output
}
