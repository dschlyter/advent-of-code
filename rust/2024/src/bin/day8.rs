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

    let antennas = util::grid_mapped_by_cell(&grid, &[".".to_string()].into());

    let part1_answer = {
        let mut affected: HashSet<(i32, i32)> = HashSet::new();

        for (_, positions) in antennas.iter() {
            for a1 in positions {
                for a2 in positions {
                    if a1 == a2 {
                        continue;
                    }

                    let (y1, x1 )= a1;
                    let (y2, x2 )= a2;
                    let y3 = y2 + (y2 - y1);
                    let x3 = x2 + (x2 - x1);
                    if y3 >= 0 && y3 < lines.len() as i32 && x3 >= 0 && x3 < lines[0].len() as i32 {
                        affected.insert((y3, x3));
                    }
                }
            }

        }

        affected.len()
    };
    println!("{}", part1_answer);

    let part2_answer = {
        let mut affected: HashSet<(i32, i32)> = HashSet::new();

        for (_, positions) in antennas.iter() {
            for a1 in positions {
                for a2 in positions {
                    if a1 == a2 {
                        continue;
                    }

                    let (y1, x1) = a1;
                    let (y2, x2) = a2;
                    let yd = y2 - y1;
                    let xd = x2 - x1;

                    let mut y3: i32 = *y2;
                    let mut x3: i32 = *x2;


                    loop {
                        if y3 < 0 || y3 >= lines.len() as i32 || x3 < 0 || x3 >= lines[0].len() as i32 {
                            break;
                        }
                        affected.insert((y3, x3));

                        y3 += yd;
                        x3 += xd;
                    }
                }
            }

        }

        let mut print_grid = grid.clone();
        for (y3, x3) in affected.iter() {
            print_grid[*y3 as usize][*x3 as usize] = "#".to_string();
        }
        for row in print_grid {
            println!("{}:{}: {:?}", file!(), line!(), (row.join("")));
        }
        affected.len()

    };
    println!("{}", part2_answer);
}