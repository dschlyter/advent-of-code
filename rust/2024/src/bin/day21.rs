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

    let keypad: Vec<Vec<String>> = vec![
        vec!["7".to_string(), "8".to_string(), "9".to_string()],
        vec!["4".to_string(), "5".to_string(), "6".to_string()],
        vec!["1".to_string(), "2".to_string(), "3".to_string()],
        vec![" ".to_string(), "0".to_string(), "A".to_string()],
    ];

    let dirpad: Vec<Vec<String>> = vec![
        vec![" ".to_string(), "^".to_string(), "A".to_string()],
        vec!["<".to_string(), "v".to_string(), ">".to_string()],
    ];

    let bad_keys = vec![" ".to_string()].into_iter().collect::<HashSet<_>>();
    let dirpad_keys: HashSet<String> = util::grid_mapped_by_cell(&dirpad, &bad_keys).keys().into_iter().map(|s| s.to_string()).collect();
    // For the outer keypad the cost of any keypress is 1
    let mut outer_cost: HashMap<(String, String), i64> = HashMap::new();
    for key1 in &dirpad_keys {
        for key2 in &dirpad_keys {
            outer_cost.insert((key1.to_string(), key2.to_string()), 1);
        }
    }

    println!("{}:{}: {:?}", file!(), line!(), (cheapest_cost_for(&dirpad, &outer_cost, &"A".to_string(), &"<".to_string())));

    // Part 1
    let part1_answer = {
        let cost1 = cost_for_pad(&dirpad, &outer_cost);
        let cost2 = cost_for_pad(&dirpad, &cost1);

        let mut ans = 0;
        for code in &lines {
            let num_part = code.replace("A", "").parse::<i64>().expect("parsed num");
            let mut press_cost = 0;
            let mut last_key = "A".to_string();
            for c in code.chars() {
                let cs = c.to_string();
                press_cost += cheapest_cost_for(&keypad, &cost2, &last_key, &cs);
                last_key = cs;
            }
            ans += press_cost * num_part;
        }
        ans
    };
    println!("{}", part1_answer);

    // Part 2
    let part2_answer = {
        let mut pad_cost = outer_cost;
        for _ in 0..25 {
            pad_cost = cost_for_pad(&dirpad, &pad_cost);
        }

        let mut ans = 0;
        for code in &lines {
            let num_part = code.replace("A", "").parse::<i64>().expect("parsed num");
            let mut press_cost: i64 = 0;
            let mut last_key = "A".to_string();
            for c in code.chars() {
                let cs = c.to_string();
                press_cost += cheapest_cost_for(&keypad, &pad_cost, &last_key, &cs);
                last_key = cs;
            }
            ans += press_cost * num_part;
        }
        ans
    };
    println!("{}", part2_answer);
}

pub fn cost_for_pad(pad: &Grid, cost: &HashMap<(String, String), i64>) -> HashMap<(String, String), i64> {
    let bad_keys = vec![" ".to_string()].into_iter().collect::<HashSet<_>>();
    let dirpad_keys: HashSet<String> = util::grid_mapped_by_cell(&pad, &bad_keys).keys().into_iter().map(|s| s.to_string()).collect();

    let mut inner_cost: HashMap<(String, String), i64> = HashMap::new();
    for key1 in &dirpad_keys {
        for key2 in &dirpad_keys {
            let cost = cheapest_cost_for(pad, cost, key1, key2);
            inner_cost.insert((key1.to_string(), key2.to_string()), cost);
        }
    }
    inner_cost
}

pub fn cheapest_cost_for(pad: &Grid, cost: &HashMap<(String, String), i64>, start: &String, target: &String) -> i64 {
    let mut seen = HashSet::new();
    let mut q = BinaryHeap::new();
    let s_pos = util::grid_find_cell(&pad, start);

    q.push((0, "A".to_string(), s_pos));

    while let Some((neg_dist, prev_press, pos)) = q.pop() {
        let dist = -neg_dist;
        if !seen.insert((prev_press.to_string(), pos)) {
            continue;
        }

        for press in vec!["^", "<", ">", "v", "A"] {
            if let Some(new_pos) = press_key_pad(pad, pos, &press.to_string()) {
                let next_key = &pad[new_pos.0 as usize][new_pos.1 as usize];
                let new_dist = dist + cost[&(prev_press.to_string(), press.to_string())];
                if press == "A" {
                    if next_key == target {
                        return new_dist;
                    } else {
                        // Wrong key
                        continue;
                    }
                }
                q.push((-new_dist, press.to_string(), new_pos));
            }
        }
    }
    panic!("No path found")
}

pub fn press_key_pad(pad: &Vec<Vec<String>>, pos: (i32, i32), key: &String) -> Option<(i32, i32)> {
    if key == "A" {
        return Some(pos)
    } else {
        let mut dir_map = HashMap::new();
        dir_map.insert("^", (-1, 0));
        dir_map.insert("<", (0, -1));
        dir_map.insert(">", (0, 1));
        dir_map.insert("v", (1, 0));

        let (yd, xd) = dir_map[key.as_str()];
        let new_pos = (pos.0 + yd, pos.1 + xd);
        let (y, x) = new_pos;

        if y < 0 || x < 0 || y >= pad.len() as i32 || x >= pad[y as usize].len() as i32 || pad[y as usize][x as usize] == " " {
            return None;
        }
        return Some(new_pos);
    }
}