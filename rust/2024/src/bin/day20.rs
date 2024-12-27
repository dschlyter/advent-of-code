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
    let grid = util::to_grid(&lines);
    let start = util::grid_mapped_by_cell(&grid, &HashSet::new())[&"S".to_string()][0];

    let mut dist_map = HashMap::new();
    map_dist(&grid, start, &mut dist_map);

    // Part 1
    let part1_answer = {
        let mut ans = 0;
        // println!("{}:{}: {:?}", file!(), line!(), (&dist_map));

        for s_pos in dist_map.keys() {
            let start_dist = dist_map[s_pos];
            for (yd, xd) in vec![
                (-2, 0), (0, -2), (0, 2), (2, 0), 
                (-1, -1), (-1, 1), (1, -1), (1, 1), 
                ] {
                    let e_pos = (s_pos.0 + yd, s_pos.1 + xd);

                    if let Some(end_dist) = dist_map.get(&e_pos) {
                        let savings = end_dist - start_dist - yd.abs() - xd.abs();
                        if savings > 0 {
                            // println!("{}:{}: {:?}", file!(), line!(), (savings));
                        }
                        if savings >= 100 {
                            ans += 1;
                        }
                    }
            }
        }
        ans
    };
    println!("{}", part1_answer);

    // Part 2
    let part2_answer = {
        let cheat_dist = 20;
        let mut ans = 0;

        for s_pos in dist_map.keys() {
            let start_dist = dist_map[s_pos];
            for yd in -cheat_dist..(cheat_dist+1) {
                for xd in -cheat_dist..(cheat_dist+1) {
                    let dist = (yd as i32).abs() + (xd as i32).abs();
                    if dist > cheat_dist {
                        continue;
                    }
                    let e_pos = (s_pos.0 + yd, s_pos.1 + xd);
                    if let Some(end_dist) = dist_map.get(&e_pos) {
                        let savings = end_dist - start_dist - dist;
                        if savings > 0 {
                            // println!("{}:{}: {:?}", file!(), line!(), (savings));
                        }
                        if savings >= 100 {
                            ans += 1;
                        }
                    }
                }
            }
        }
        ans
    };
    println!("{}", part2_answer);
}

fn map_dist(grid: &Vec<Vec<String>>, pos: (i32, i32), dist_map: &mut HashMap<(i32, i32), i32>) -> i32 {
    let (y, x) = pos;
    if y < 0 || x < 0 || y >= grid.len() as i32 || x >= grid[y as usize].len() as i32 || grid[y as usize][x as usize] == "#" {
        return -1;
    }
    if dist_map.contains_key(&pos) {
        return -1;
    }
    dist_map.insert(pos, -1);

    if grid[y as usize][x as usize] == "E".to_string() {
        dist_map.insert(pos, 0);
        return 0;
    }
    for (yd, xd) in vec![(-1, 0), (0, -1), (0, 1), (1, 0)] {
        let new_pos = (y + yd, x + xd);
        let d = map_dist(grid, new_pos, dist_map);
        if d != -1 {
            let nd = d + 1;
            dist_map.insert(pos, nd);
            return nd;
        }
    }
    panic!("Found no progress")
}
