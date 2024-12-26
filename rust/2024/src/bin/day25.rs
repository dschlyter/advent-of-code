#![allow(unused_imports)]

use core::num;
use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};
use std::cmp::Reverse;
use std::mem::swap;
use std::vec;
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
    let keys: Vec<Vec<String>> = lines.split(|line| line.is_empty()).map(|group| group.to_vec()).collect();

    // Part 1
    // Very lazy - don't bother converting the input or distinguishing between keys and locks
    let part1_answer = {
        let mut ans = 0;
        for i in 0..keys.len() {
            for j in i+1..keys.len() {
                let key = &keys[i];
                let lock = &keys[j];

                let mut overlap = false;
                'check: for y in 0..key.len() {
                    for x in 0..key[y].len() {
                        if key[y].chars().nth(x) == Some('#') && lock[y].chars().nth(x) == Some('#') {
                            overlap = true;
                            break 'check;
                        }
                    }
                }
                if !overlap {
                    ans += 1;
                }
            }
        }
        ans
    };
    println!("{}", part1_answer);
}