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
    let lines = util::read_lines(&filename);
    let grid = util::to_grid_typed::<i32>(&lines);
    
    let part1_answer = {
        let mut score = 0;
        for y in 0..grid.len() {
            for x in 0..grid[y].len() {
                if grid[y][x] == 0 {
                    let reachable = score_dfs(&grid, y, x, 0);
                    score += reachable.len();
                }
            }
        }
        score
    };
    println!("{}", part1_answer);


    let part2_answer = {
        let mut score = 0;
        for y in 0..grid.len() {
            for x in 0..grid[y].len() {
                if grid[y][x] == 0 {
                    score += score_dfs2(&grid, y, x, 0);
                }
            }
        }
        score
    };
    println!("{}", part2_answer);
}

pub fn score_dfs(grid: &Vec<Vec<i32>>, y: usize, x: usize, target: i32) -> HashSet<(usize, usize)> {
    let mut targets = HashSet::new();
    let cell = grid[y][x];

    if cell == target {
        if target == 9 {
            targets.insert((y, x));
        } else {
            if y > 0 {
                targets.extend(score_dfs(grid, y-1, x, target+1));
            }
            if x > 0 {
                targets.extend(score_dfs(grid, y, x-1, target+1));
            }
            if y < grid.len() - 1 {
                targets.extend(score_dfs(grid, y+1, x, target+1));
            }
            if x < grid[y].len() - 1 {
                targets.extend(score_dfs(grid, y, x+1, target+1));
            }
        }
    }

    targets
}

pub fn score_dfs2(grid: &Vec<Vec<i32>>, y: usize, x: usize, target: i32) -> i32 {
    let mut score = 0;
    let cell = grid[y][x];

    if cell == target {
        if target == 9 {
            score += 1
        } else {
            if y > 0 {
                score += score_dfs2(grid, y-1, x, target+1);
            }
            if x > 0 {
                score += score_dfs2(grid, y, x-1, target+1);
            }
            if y < grid.len() - 1 {
                score += score_dfs2(grid, y+1, x, target+1);
            }
            if x < grid[y].len() - 1 {
                score += score_dfs2(grid, y, x+1, target+1);
            }
        }
    }

    score
}