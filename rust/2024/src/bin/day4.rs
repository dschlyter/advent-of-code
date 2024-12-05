// #![allow(unused_imports)]

#[allow(unused_imports)]
use std::collections::HashMap;
#[allow(unused_imports)]
use std::collections::HashSet;
#[allow(unused_imports)]
use regex::Regex;

use aoc2024::util::{self, run_for_inputs};

pub fn main() {
    run_for_inputs(file!(), solve);
}

pub fn solve(filename: String) {
    // Input

    let lines = util::read_lines(&filename);
    let grid = util::to_grid(&lines);
    // dbg!(&grid);

    // Part 1

    {
        let mut res = 0;
        let target = (vec!["X", "M", "A", "S"]).iter().map(|s| s.to_string()).collect();

        for y in 0..grid.len() {
            for x in 0..grid[y].len() {
                // dbg!(y, x, res);
                let ref steps: Vec<i32> = vec![-1, 0, 1];
                for yd in steps {
                    for xd in steps {
                        if *yd == 0 && *xd == 0 {
                            continue;
                        }
                        res += dfs(&grid, y, x, *yd, *xd, &target);
                    }
                }
            }
        }

        // dbg!(&lines[0].chars().nth());
        println!("{}", res);
    }


    // Part 2
   {
        let mut res = 0;

        for y in 0..grid.len() {
            for x in 0..grid[y].len() {
                let yi = y as i32;
                let xi = x as i32;

                let mid = util::grid_get(&grid, yi, xi);
                let ul = util::grid_get(&grid, yi-1, xi-1);
                let ur = util::grid_get(&grid, yi-1, xi+1);
                let ll = util::grid_get(&grid, yi+1, xi-1);
                let lr = util::grid_get(&grid, yi+1, xi+1);

                let mcount = vec![ul, ur, ll, lr].iter().filter(|c| **c == Some(&String::from("M"))).count();
                let scount = vec![ul, ur, ll, lr].iter().filter(|c| **c == Some(&String::from("S"))).count();

                // If mid is A, we have two M and two S, and there is a difference across the diagonal - then we have X-MAS cross
                println!("{}:{}: {:?}", file!(), line!(), (y, x, mid, ul, ur, ll, lr, mid == Some(&String::from("A")), mcount, scount, ul != lr));

                if mid == Some(&String::from("A")) && mcount == 2 && scount == 2 && ul != lr {
                    res += 1;
                }
            }
        }

        // dbg!(&lines[0].chars().nth());
        println!("{}", res);
    }
}

pub fn dfs(grid: &Vec<Vec<String>>, y: usize, x: usize, yd: i32, xd: i32, target: &Vec<String>) -> i32 {
    if y >= grid.len() || x >= grid[y].len() {
        return 0
    }

    let curr = &grid[y][x];
    if curr == &target[0] {
        if target.len() == 1 {
            return 1;
        } else {
            let tail = target.clone().split_off(1);

            let y2 = y as i32 + yd;
            let x2 = x as i32 + xd;

            return dfs(grid, y2 as usize, x2 as usize, yd, xd, &tail);
        }
    }

    return 0;
}