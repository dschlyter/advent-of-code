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
    // Input

    let lines = util::read_lines(&filename);
    let grid = util::to_grid(&lines);

    let dirs = vec![(-1, 0), (0, 1), (1, 0), (0, -1)];
    let mut start_x: i32 = 0;
    let mut start_y: i32  = 0;

    for i in 0..grid.len() {
        let p = grid[i].iter().position(|x| x == "^");
        match p {
            Some(j) => {
                start_y = i as i32;
                start_x = j as i32;
            }
            _ => ()
        }
    }

    // Part 1
    let mut visited = HashSet::new();

    {
        let mut y = start_y;
        let mut x = start_x;
        let mut dir = 0;

        loop {
            visited.insert((y, x));
            
            let (yd, xd) = dirs[dir];
            let ny = y + yd;
            let nx = x + xd;

            match grid_get(&grid, ny, nx) {
                Some(s) => {
                    match s.as_str() {
                        "#" => {
                            dir = (dir + 1) % dirs.len();
                        }
                        _ => {
                            y = ny;
                            x = nx;
                        }
                    }
                }
                None => break
            }
        }

        println!("{}", visited.len());
    }


    // Part 2 - reuse visited from part 1
    // A bit slow 17s - probably possible to opt
    {
        let mut res = 0;

        for (yv, xv) in visited {
            if yv == start_y && xv == start_x {
                continue;
            }

            let mut visited2 = HashSet::new();
            let mut y = start_y;
            let mut x = start_x;
            let mut dir = 0;

            println!("{}:{}: {:?}", file!(), line!(), (yv, xv));

            loop {
                let key = (y, x, dir);

                let new = visited2.insert(key);
                if !new {
                    res += 1;
                    break;
                }
                
                let (yd, xd) = dirs[dir];
                let ny = y + yd;
                let nx = x + xd;

                match grid_get(&grid, ny, nx) {
                    Some(s) => {
                        if s == "#" || (ny == yv && nx == xv) {
                            dir = (dir + 1) % dirs.len();
                        } else {
                            y = ny;
                            x = nx;

                        }
                    }
                    None => break
                }
            }
        }

        println!("{}", res);

    }
}