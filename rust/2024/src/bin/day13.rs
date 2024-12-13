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

type Int = i64;

pub fn solve(filename: String) {
    let lines = util::read_lines(&filename);

    // Input
    let mut games: Vec<Game> = Vec::new();
    let mut i = 0;
    while i+2 < lines.len() {
        games.push(
            Game {
                a: parse_point(&lines[i], "+"),
                b: parse_point(&lines[i+1], "+"),
                prize: parse_point(&lines[i+2], "="),
            }
        );
        i += 4;
    }
    
    // Part 1 - lazy brute force
    let part1_answer = {
        let mut ans = 0;
        for game in games.iter() {
            let mut best = 9000;
            for a in 0..100 {
                for b in 0..100 {
                    let cost = 3 * a + 1 * b;
                    if a * game.a.x + b * game.b.x == game.prize.x && a * game.a.y + b * game.b.y == game.prize.y {
                        if cost < best {
                            best = cost;
                        }
                    }
                }
            }
            if best < 9000 {
                ans += best;
            }
        }
        ans
    };
    println!("{}", part1_answer);

    // Part 2 - solve the equation system
    let part2_answer = {
        let mut ans = 0;
        for game in games.iter() {
            let new_price = Point { x: game.prize.x + 10000000000000, y: game.prize.y + 10000000000000 };

            let af = game.a.to_f();
            let bf = game.b.to_f();
            let pf = new_price.to_f();

            let t1 = pf.x - (af.x * pf.y) / (af.y);
            let t2 = bf.x - (bf.y * af.x) / af.y;

            let b = (t1 / t2).round() as Int;
            let a = ((pf.x - (b as f64) * bf.x) / af.x).round() as Int;

            if a * game.a.x + b * game.b.x == new_price.x && a * game.a.y + b * game.b.y == new_price.y {
                let cost = 3 * a + 1 * b;
                ans += cost;
                // println!("{}:{}: {:?}", file!(), line!(), (a, b, cost));
            } else {
                // println!("{}:{}: {:?}", file!(), line!(), ("fail"));
            }
        }
        ans
    };
    println!("{}", part2_answer);
}

fn parse_point(line: &String, sign: &str) -> Point {
    let pos = line.split(":").nth(1).unwrap().trim();
    let coords: Vec<Int> = pos.split(",").map(|s| s.split(sign).nth(1).unwrap().parse::<Int>().unwrap()).collect();
    Point { x: coords[0], y: coords[1] }
}

struct PointF {
    x: f64,
    y: f64
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Point {
    x: Int,
    y: Int
}

impl Point {
    fn to_f(&self) -> PointF {
        return PointF { x: self.x as f64, y: self.y as f64 }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Game {
    a: Point,
    b: Point,
    prize: Point
}