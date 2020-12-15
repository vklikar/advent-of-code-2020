use std::fs;

mod rust {
    pub mod day06;
    pub mod day15;
}

fn main() {
    println!("{}", rust::day06::solve_part1(read_file("data/day06.txt")));
    println!("{}", rust::day06::solve_part2(read_file("data/day06.txt")));
    println!("{}", rust::day15::solve_part1(read_file("data/day15.txt")));
    println!("{}", rust::day15::solve_part2(read_file("data/day15.txt")));
}

fn read_file(filename: &str) -> String {
    return fs::read_to_string(filename).expect("Something went wrong reading the file");
}
