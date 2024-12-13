#![allow(unused_imports)]

use std::collections::HashMap;
use std::collections::HashSet;
use aoc2024::util::grid_get;
use regex::Regex;
use std::ops::Add;

use aoc2024::util::{self, run_for_inputs};

pub fn main() {
    run_for_inputs(file!(), solve);
}

pub fn solve(filename: String) {
    let lines = util::read_lines(&filename);
    let grid = util::to_grid(&lines);
    
    let part1_answer = {
        let mut visited = HashSet::new();
        let mut ans = 0;
        for y in 0..grid.len() {
            for x in 0..grid[y].len() {
                let target = &grid[y][x];
                let res = visit2(y as i32, x as i32, target, &grid, &mut visited);
                let price = res.area * res.perimiter;
                ans += price;
            }
        }
        ans
    };
    println!("{}", part1_answer);


    let part2_answer = {
        let mut visited = HashSet::new();
        let mut ans = 0;
        for y in 0..grid.len() {
            for x in 0..grid[y].len() {
                let target = &grid[y][x];
                let res = visit2(y as i32, x as i32, target, &grid, &mut visited);
                // Border is a pair of (inside, outside) points - remove all points that have another to the right or below to get unique
                let unique_border: HashSet<_> = res.border.iter().filter(|(y1, x1, y2, x2)| {
                    !res.border.contains(&(y1+1, *x1, y2+1, *x2)) && !res.border.contains(&(*y1, x1+1, *y2, x2+1))
                }).collect();
                let price = res.area * unique_border.len() as i32;
                ans += price;
            }
        }
        ans
    };
    println!("{}", part2_answer);
}

fn visit2(y: i32, x: i32, target: &String, grid: &Vec<Vec<String>>, visited: &mut HashSet<(i32, i32)>) -> Region {
    let mut ret = Region { area: 0, perimiter: 0, border: HashSet::new() };

    if y >= 0 && x >= 0 && y < grid.len() as i32 && x < grid[y as usize].len() as i32 && grid[y as usize][x as usize] == *target {
        // Only add visited for in-region nodes, we need to process out-of-region nodes twice for perimiters
        let key = (y, x);
        if visited.insert(key) {
            ret.area += 1;
            for (yd, xd) in [(1, 0), (-1, 0), (0, -1), (0, 1)] {
                let neigh = visit2(y + yd, x + xd, target, grid, visited);
                if neigh.area > 0 {
                    ret.area += neigh.area;
                    ret.perimiter += neigh.perimiter;
                    ret.border.extend(neigh.border);
                } else if neigh.area == 0 {
                    ret.perimiter += 1;
                    ret.border.insert((y, x, y + yd, x + xd));
                }
            }
        } else {
            // Signal this node should not be counted as perimiter
            ret.area = -1;
        }
    }

    ret
}

struct Region {
    area: i32,
    perimiter: i32,
    border: HashSet<(i32, i32, i32, i32)>
}