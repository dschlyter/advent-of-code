// #![allow(unused_imports)]

#[allow(unused_imports)]
use std::collections::HashMap;
use std::collections::HashSet;

use aoc2024::util;

pub fn main() {
    util::run_for_inputs(file!(), solve);
}

pub fn solve(filename: String) {
    // Input

    let lines = util::read_lines(&filename);
    let num_lines: Vec<Vec<i32>> = lines.iter().map(|line| {
        line.split_whitespace().map(|s| s.parse::<i32>().unwrap()).collect()
    }).collect();

    // dbg!(&numLines);

    // Part 1

    {
        let mut res = 0;

        for nums in num_lines.iter() {
            if is_safe(nums) {
                res += 1;
            }
        }

        println!("{}", res);
    }


    // Part 2

    {
        let mut res = 0;

        for nums in num_lines.iter() {
            if is_safe(nums) {
                res += 1;
            } else {
                for i in 0..nums.len() {
                    let mut n2 = nums.clone(); 
                    n2.remove(i);
                    if is_safe(&n2) {
                        res += 1;
                        break;
                    }
                }
            }
        }

        println!("{}", res);
    }
}

fn is_safe(nums: &Vec<i32>) -> bool {
    let mut diffs = HashSet::new();
    for i in 0..nums.len()-1 {
        diffs.insert(nums[i+1] - nums[i]);
    }

    let target1 = HashSet::from([1, 2, 3]);
    let target2 = HashSet::from([-1, -2, -3]);
    return diffs.is_subset(&target1) || diffs.is_subset(&target2)
}