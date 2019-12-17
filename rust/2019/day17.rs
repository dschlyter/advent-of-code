mod util;
mod computer;

// use std::collections::HashSet;
// use std::collections::HashMap;
// use std::collections::LinkedList;

// use std::{thread, time, char};
use std::time::Instant;

extern crate regex;

fn main() {
    let lines = util::get_lines();

    part1(&lines);
    // part2(&lines);
}

fn part1(lines: &Vec<String>) {
    let program: Vec<i64> = lines[0].split(",").map(|n| n.parse::<i64>().unwrap()).collect();
}
