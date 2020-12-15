use std::collections::HashMap;

pub fn solve_part1(input: String) -> i32 {
    return get_spoken_number(2020, parse_input(input));
}

pub fn solve_part2(input: String) -> i32 {
    return get_spoken_number(30000000, parse_input(input));
}

fn parse_input(input: String) -> Vec<i32> {
    return input
        .trim_end()
        .split(",")
        .map(|x| x.parse().unwrap())
        .collect();
}

fn get_spoken_number(n: i32, input: Vec<i32>) -> i32 {
    let mut m = HashMap::new();
    let mut i = 1;
    let mut last = -1;
    for k in input.into_iter() {
        m.insert(k, i);
        i += 1;
        last = k;
    }
    let len = m.len() as i32;

    for i in len..n {
        if let Some(v) = m.get_mut(&last) {
            last = i - *v;
            *v = i;
        } else {
            m.insert(last, i);
            last = 0;
        }
    }
    return last;
}
