#![allow(unused_imports)]

use std::collections::{HashMap, HashSet, BinaryHeap};
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
    let index = util::grid_mapped_by_cell(&grid, &HashSet::new());

    let dirs = vec![(-1, 0), (0, 1), (1, 0), (0, -1)];
    let start_pos = index[&"S".to_string()][0];
    let east = 1;

    // Part 1
    let part1_answer = {
        let mut ans = 0;
        let mut pq = BinaryHeap::new();
        let mut visited = HashSet::new();
        pq.push(Reverse((0, start_pos, east)));
        while let Some(Reverse((score, pos, dir))) = pq.pop() {
            let key = (pos, dir) ;
            if !visited.insert(key) {
                continue;
            }
            // println!("{}:{}: {:?}", file!(), line!(), (score, pos, dir));
            if pos.0 < 0 || pos.1 < 0 || pos.0 as usize >= grid.len() || pos.1 as usize >= grid[pos.0 as usize].len() {
                continue;
            }
            match grid[pos.0 as usize][pos.1 as usize].as_str() {
                "#" => (),
                "S" | "." => {
                    let forward = (pos.0 + dirs[dir].0, pos.1 + dirs[dir].1);
                    pq.push(Reverse((score + 1, forward, dir)));
                    pq.push(Reverse((score + 1000, pos, (dir + 1) % 4)));
                    pq.push(Reverse((score + 1000, pos, (dir + 3) % 4)));
                },
                "E" => {
                    ans = score;
                    break;
                },
                _ => panic!("Bad cell!")
            }
        }
        ans
    };
    println!("{}", part1_answer);

    // Part 2 - solve the equation system
    let part2_answer = {
        let mut shortest_path = i32::MAX;
        let mut pq = BinaryHeap::new();
        let mut visited = HashMap::new();
        let mut best_path = HashSet::new();

        pq.push(Reverse((0, start_pos, east, Vec::new())));
        while let Some(Reverse((score, pos, dir, pos_history))) = pq.pop() {
            let key = (pos, dir);
            let prev_score = *visited.get(&key).unwrap_or(&i32::MAX);
            if prev_score < score {
                continue;
            }
            visited.insert(key, score);
            // println!("{}:{}: {:?}", file!(), line!(), (score, pos, dir));
            if pos.0 < 0 || pos.1 < 0 || pos.0 as usize >= grid.len() || pos.1 as usize >= grid[pos.0 as usize].len() {
                continue;
            }
            match grid[pos.0 as usize][pos.1 as usize].as_str() {
                "#" => (),
                "S" | "." => {
                    // A lot of cloning here - not very opt!
                    let mut new_history = pos_history.clone();
                    new_history.push(pos);

                    let forward = (pos.0 + dirs[dir].0, pos.1 + dirs[dir].1);
                    pq.push(Reverse((score + 1, forward, dir, new_history.clone())));
                    pq.push(Reverse((score + 1000, pos, (dir + 1) % 4, new_history.clone())));
                    pq.push(Reverse((score + 1000, pos, (dir + 3) % 4, new_history)));
                },
                "E" => {
                    if shortest_path >= score {
                        shortest_path = score;
                        best_path.insert(pos);
                        for prev_pos in pos_history {
                            best_path.insert(prev_pos);
                        }
                    } else {
                        // break;
                    }
                },
                _ => panic!("Bad cell!")
            }
        }
        let mut grid2 = grid.clone();
        for pos in &best_path {
            grid2[pos.0 as usize][pos.1 as usize] = "O".to_string();
        }
        util::print_grid(&grid2, &HashMap::new());
        best_path.len()
    };
    println!("{}", part2_answer);
}