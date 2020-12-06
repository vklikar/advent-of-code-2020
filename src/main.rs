use std::fs;

mod rust {
    pub mod day6;
}

fn main() {
    println!("{}", rust::day6::solve_part1(read_file("data/day6.txt")));
    println!("{}", rust::day6::solve_part2(read_file("data/day6.txt")));
}

fn read_file(filename: &str) -> String {
    return fs::read_to_string(filename).expect("Something went wrong reading the file");
}
