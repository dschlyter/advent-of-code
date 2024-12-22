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
        for code in &lines {
            let num_part = code.replace("A", "").parse::<i32>().expect("parsed num");
            let seq_len = path_to_seq(2, &code.to_string());
            ans += seq_len * num_part;
            println!("{}:{}: {:?}", file!(), line!(), (num_part, seq_len, ans));
        }
        ans
    };
    println!("{}", part1_answer);

    // Part 2 - Slow brute force 24.7 seconds
    let part2_answer = {
        0
    };
    println!("{}", part2_answer);
}

pub fn path_to_seq(key_pads: usize, target: &String) -> i32 {
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

    let mut seen = HashSet::new();
    let mut q = VecDeque::new();
    let s_pos_key = util::grid_find_cell(&keypad, &"A".to_string());
    let s_pos_dir = util::grid_find_cell(&dirpad, &"A".to_string());

    let mut start = Vec::new();
    for _ in 0..key_pads {
        start.push(s_pos_dir);
    }
    start.push(s_pos_key);

    q.push_back((start, 1, "".to_string()));
    // q.push_back((vec![s_pos_key], 1, "".to_string()));

    while let Some((state, dist, pressed)) = q.pop_front() {
        let key = (state.clone(), dist, pressed.clone());
        if !seen.insert(key) {
            continue;
        }

        'outer: for press in vec!["^", "<", ">", "v", "A"] {
            let mut key_press = press.to_string();
            let mut new_state = vec![];
            for i in 0..state.len() {
                let pos = state[i];
                let pad = match i {
                    _ if i < state.len() - 1 => &dirpad,
                    _ => &keypad
                };
                match press_key_pad(pad, pos, &key_press) {
                    Some((p, key)) => {
                        new_state.push(p);
                        key_press = key;
                    }
                    None => continue 'outer
                }
            }

            let mut new_pressed = pressed.to_string();
            if key_press != "" {
                new_pressed = new_pressed + &key_press;
            }
            if &new_pressed == target {
                return dist;
            }
            if !target.starts_with(&new_pressed) {
                continue;
            }
            q.push_back((new_state, dist + 1, new_pressed));
        }
    }
    panic!("No path found")
}

// Press a direction on a keypad
// Return the next pos, and if applicable the key that was pressed on the keypad
pub fn press_key_pad(pad: &Vec<Vec<String>>, pos: (i32, i32), key: &String) -> Option<((i32, i32), String)> {
    if key == "" {
        return Some((pos, "".to_string()));
    } else if key == "A" {
        let (y, x) = pos;
        let key = &pad[y as usize][x as usize];
        return Some((pos, key.clone()))
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
        return Some((new_pos, "".to_string()));
    }
}