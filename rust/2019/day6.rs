// use std::collections::HashMap;
// use std::collections::HashSet;

mod util;

fn main() {
    let lines = util::get_lines();

    part1(lines.clone());
    part2(lines);
}

fn part1(lines: Vec<String>) {
    let mut ans = 0;

    let mut reg: Vec<i32> = lines[0].split(",").map(|n| n.parse::<i32>().unwrap()).collect();
    let input: Vec<i32> = [1].to_vec();

    println!("Part 1 answer {}", ans);
}

fn part2(lines: Vec<String>) {
    let mut ans = 0;

    println!("Part 2 answer {}", ans);
}