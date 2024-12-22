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
    let lines = util::read_lines(&filename);

    let falling: Vec<(i32, i32)> = lines.iter().map(|s| {
        let i: Vec<i32> = s.split(",").map(|c| c.parse::<i32>().unwrap()).collect();
        (i[0], i[1])
    }).collect();

    // Part 1
    let part1_answer = {
        let mut ans = -1;
        let (fallen, y_max, x_max) = if filename.contains("_test") {(12, 6, 6)} else {(1024, 70, 70)};
        let corrupted: HashSet<(i32, i32)> = falling.iter().take(fallen).cloned().collect();
        let mut visited = HashSet::new();
        let mut next = VecDeque::new();
        next.push_back((0, 0, 0));


        while let Some((x, y, dist)) = next.pop_front() {
            if x < 0 || y < 0 || x > x_max || y > y_max {
                continue;
            }

            if !visited.insert((x, y)) {
                continue;
            }

            if corrupted.contains(&(x, y)) {
                continue;
            }

            if (x, y) == (x_max, y_max) {
                ans = dist;
                break;
            }

            next.push_back((x+1, y, dist+1));
            next.push_back((x-1, y, dist+1));
            next.push_back((x, y+1, dist+1));
            next.push_back((x, y-1, dist+1));
        }

        ans
    };
    println!("{}", part1_answer);

    // Part 2 - Slow brute force 24.7 seconds
    let part2_answer = {
        let mut byte_index = 9000;
        for bytes_to_take in 1..(falling.len()+1) {
            let mut to_goal = -1;
            let (y_max, x_max) = if filename.contains("_test") {(6, 6)} else {(70, 70)};
            let corrupted: HashSet<(i32, i32)> = falling.iter().take(bytes_to_take).cloned().collect();
            let mut visited = HashSet::new();
            let mut next = VecDeque::new();
            next.push_back((0, 0, 0));


            println!("{}:{}: {:?}", file!(), line!(), (bytes_to_take));
            while let Some((x, y, dist)) = next.pop_front() {
                if x < 0 || y < 0 || x > x_max || y > y_max {
                    continue;
                }

                if !visited.insert((x, y)) {
                    continue;
                }

                if corrupted.contains(&(x, y)) {
                    continue;
                }

                if (x, y) == (x_max, y_max) {
                    to_goal = dist;
                    break;
                }

                next.push_back((x+1, y, dist+1));
                next.push_back((x-1, y, dist+1));
                next.push_back((x, y+1, dist+1));
                next.push_back((x, y-1, dist+1));
            }

            if to_goal == -1 {
                byte_index = bytes_to_take - 1;
                break;
            }
        }
        lines[byte_index].clone()
    };
    println!("{}", part2_answer);

}