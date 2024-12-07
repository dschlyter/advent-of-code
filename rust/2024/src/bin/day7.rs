#![allow(unused_imports)]

use std::collections::HashMap;
use std::collections::HashSet;
use aoc2024::util::grid_get;
use regex::Regex;

use aoc2024::util::{self, run_for_inputs};

pub fn main() {
    run_for_inputs(file!(), solve);
}

pub fn solve(filename: String) {
    // Input

    let lines = util::read_lines(&filename);

    let targets: Vec<i64> = lines.iter().map(|s| s.split(":").nth(0).unwrap()).map(|s| s.parse::<i64>().unwrap()).collect();
    let args: Vec<Vec<i64>> = lines.iter().map(|s| s.split(":").nth(1).unwrap().trim())
        .map(|s| s.split_whitespace().map(|a| a.parse::<i64>().unwrap()).collect()).collect();

    // Part 1
    {
        let mut res = 0;
        for (i, al) in args.iter().enumerate() {
            let mut possibilities: HashSet<i64> = [1].into();
            for arg in al {
                possibilities = possibilities.iter().flat_map(|f| [f + arg, f * arg]).collect();
            }
            if possibilities.contains(&targets[i]) {
                res += targets[i];
            }
        }
        println!("{}", res);
    }

    // Part 2
    // Slow 33 seconds
    // Possible opt - do it backwards with subtraction, division and suffix trimming. Any non-zero, non-integer or non-suffix can be discarded.
    {
        let mut res = 0;
        for (i, al) in args.iter().enumerate() {
            println!("{}:{}: {:?}", file!(), line!(), (i, targets.len()));

            let mut possibilities: HashSet<i64> = [1].into();
            for arg in al {
                possibilities = possibilities.iter().flat_map(|f| [f + arg, f * arg, (f.to_string() + &arg.to_string()).parse::<i64>().unwrap()]).collect();
            }
            if possibilities.contains(&targets[i]) {
                res += targets[i];
            }
        }
        println!("{}", res);
    }
}