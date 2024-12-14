#![allow(unused_imports)]

use std::collections::HashMap;
use std::collections::HashSet;
use aoc2024::util::grid_get;
use regex::Regex;
use std::ops::Add;

use aoc2024::util::{self, run_for_inputs, Point};

pub fn main() {
    run_for_inputs(file!(), solve);
}

type Int = i64;

pub fn solve(filename: String) {
    let lines = util::read_lines(&filename);

    // Input
    let mut robots: Vec<Robot> = Vec::new();
    for line in lines {
        let p: Vec<_> = line.split_whitespace().collect();
        robots.push(
            Robot {
                p: parse_point(&p[0].to_string(), "="),
                v: parse_point(&p[1].to_string(), "=")
            }
        );
    }
    let (ys, xs) = if filename.contains("_test") {
        (7, 11)
    } else {
        (103, 101)
    };
    
    // Part 1
    let part1_answer = {
        let mut quad_count = HashMap::new();

        for r in &robots {
            let ny = pos_mod(r.p.y + r.v.y * 100, ys);
            let nx = pos_mod(r.p.x + r.v.x * 100, xs);

            let y_side = (ny - (ys / 2)).signum();
            let x_side = (nx - (xs / 2)).signum();

            if y_side == 0 || x_side == 0 {
                continue;
            }
            *(quad_count.entry((y_side, x_side)).or_insert(0)) += 1
        }
        println!("{}:{}: {:?}", file!(), line!(), (quad_count));
        let s: i32 = quad_count.values().into_iter().product();
        s
    };
    println!("{}", part1_answer);

    // Part 2 - solve the equation system
    let part2_answer = {
        let mut r2 = robots.clone();

        let mut best_balance = i32::MAX;
        for tick in 1..10_000 {
            let mut quad_count = HashMap::new();

            for robot in &mut r2 {
                robot.p.y = pos_mod(robot.p.y + robot.v.y, ys);
                robot.p.x = pos_mod(robot.p.x + robot.v.x, xs);

                let y_side = (robot.p.y - (ys / 2)).signum();
                let x_side = (robot.p.x - (xs / 2)).signum();

                if y_side == 0 || x_side == 0 {
                    continue;
                }
                *(quad_count.entry((y_side, x_side)).or_insert(0)) += 1
            }

            let safety_factor: i32 = quad_count.values().into_iter().product();

            if safety_factor < best_balance {
                best_balance = safety_factor;
            } else {
                continue;
            }

            let r_pos: HashSet<Point> = r2.iter().map(|r| r.p.clone()).collect();
            println!();
            println!("tick {} {}", tick, safety_factor);
            for y in 0..ys {
                for x in 0..xs {
                    if r_pos.contains(&Point { x: x, y: y }) {
                        print!("#")
                    } else {
                        print!(".")
                    }
                }
                println!();
            }
        }
        0
    };
    println!("{}", part2_answer);
}

fn pos_mod(n: i64, m: i64) -> i64 {
    return ((n % m) + m) % m;
}

fn parse_point(line: &String, sign: &str) -> Point {
    let pos = line.split(sign).nth(1).unwrap().trim();
    let coords: Vec<Int> = pos.split(",").map(|s| s.parse::<Int>().unwrap()).collect();
    Point { x: coords[0], y: coords[1] }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Robot {
    p: Point,
    v: Point,
}