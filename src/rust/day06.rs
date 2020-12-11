use itertools::Itertools;
use std::collections::HashSet;

pub fn solve_part1(input: String) -> usize {
    let iter = parse_input(input).into_iter();
    return iter
        .map(|x| x.concat().chars().unique().collect::<Vec<_>>().len())
        .sum();
}

pub fn solve_part2(input: String) -> usize {
    let iter = parse_input(input).into_iter();
    return iter.map(|x| intersect_strings(x).len()).sum();
}

fn parse_input(input: String) -> Vec<Vec<String>> {
    return input
        .split("\n\n")
        .map(|x| x.lines().map(str::to_string).collect())
        .collect();
}

fn intersect_strings(vec: Vec<String>) -> HashSet<char> {
    let first: HashSet<char> = vec.first().unwrap().chars().collect();
    return vec
        .into_iter()
        .map(|x| x.chars().collect())
        .fold(first, |a, b| a.intersection(&b).copied().collect());
}
