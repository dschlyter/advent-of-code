#![allow(unused_imports)]

use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};
use std::cmp::Reverse;
use std::mem::swap;
use aoc2024::util::{Grid, grid_get};
use regex::Regex;
use std::ops::Add;

use aoc2024::util::{self, run_for_inputs, Point};

pub fn main() {
    run_for_inputs(file!(), solve);
}

// type Int = i64;

pub fn solve(filename: String) {
    let (towel_lines, designs) = util::read_lines_split(&filename);
    let towels: Vec<String> = towel_lines[0].split(", ").map(|s| s.to_string()).collect();

    // Part 1
    let part1_answer = {
        let mut ans = 0;
        for d in &designs {
            if pattern_possible(d, 0, &towels, &mut HashMap::new()) > 0 {
                ans += 1;
            }
        }
        ans
    };
    println!("{}", part1_answer);

    // Part 2 - Slow brute force 24.7 seconds
    let part2_answer = {
        let mut ans = 0;
        for d in &designs {
            ans += pattern_possible(d, 0, &towels, &mut HashMap::new());
        }
        ans
    };
    println!("{}", part2_answer);
}

fn pattern_possible(pattern: &String, i: usize, towels: &Vec<String>, seen: &mut HashMap<usize, i64>) -> i64 {
    if seen.contains_key(&i) {
        return seen[&i];
    }
    if i == pattern.len() {
        return 1;
    }

    let mut count = 0;
    for t in towels {
        if i + t.len() > pattern.len() {
            continue;
        }
        let ni = i + t.len();
        let part = &pattern[i..ni];
        if t != part {
            continue;
        }
        count += pattern_possible(pattern, ni, towels, seen)
    }

    seen.insert(i, count);
    return count;
}