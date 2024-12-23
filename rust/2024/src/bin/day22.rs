#![allow(unused_imports)]

use core::num;
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
    let lines = util::read_lines(&filename);

    // Part 1
    let part1_answer = {
        let mut ans = 0;
        for line in &lines {
            let mut n = line.parse::<i64>().unwrap();
            for _ in 0..2000 {
                n = next(n);
            }
            // println!("{}:{}: {:?}", file!(), line!(), (n));
            ans += n;
        }
        ans
    };
    println!("{}", part1_answer);

    // Part 2
    let part2_answer = {
        let mut change_to_price = HashMap::new();
        for line in &lines {
            let mut n = line.parse::<i64>().unwrap();
            let mut prices = vec![n % 10];
            for _ in 0..2000 {
                let n2 = next(n);
                prices.push(n2 % 10);
                n = n2;
            }
            let mut change_res = HashMap::new();
            for i in 4..2001 {
                if let [a, b, c, d, e] = &prices[(i-4)..i+1] {
                    let key = (b-a, c-b, d-c, e-d);
                    if !change_res.contains_key(&key) {
                        change_res.insert(key, *e);
                    }
                } else {
                    panic!("Unable to fetch array")
                }
            }
            for (change, price) in change_res {
                let p = change_to_price.entry(change).or_insert(vec![]);
                (*p).push(price);
            }
        }
        let best_sum: i64 = change_to_price.values().into_iter().map(|p| p.iter().copied().sum()).max().unwrap();
        best_sum
    };
    println!("{}", part2_answer);
}

fn next(n: i64) -> i64 {
    let p = 16777216;
    let n2 = (n ^(n * 64)) % p;
    let n3 = (n2 ^ (n2 / 32)) % p;
    let n4 = (n3 ^ (n3 * 2048)) % p;
    n4
}