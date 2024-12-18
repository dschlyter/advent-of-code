#![allow(unused_imports)]

use std::collections::HashMap;
use std::collections::HashSet;
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

    // Input
    let input_parts: Vec<Vec<String>> = lines.split(|line| line.is_empty()).map(|slice| slice.to_vec()).collect();
    let mut map_mut = util::to_grid(&input_parts[0]);
    let robot = util::grid_mapped_by_cell(&map_mut, &HashSet::new())[&"@".to_string()][0].clone();
    map_mut[robot.0 as usize][robot.1 as usize] = ".".to_string();
    let map = map_mut;
    let moves: Vec<String> = input_parts[1].join("").chars().map(|c| c.to_string()).collect();
    
    // Part 1
    let part1_answer = {
        let mut pos = robot;
        let mut map2 = map.clone();

        for m in &moves {
            let dir = parse_move(m);

            // let old_pos = pos;
            if sokoban(pos, dir, &mut map2) {
                pos = (pos.0 + dir.0, pos.1 + dir.1);
            }

            // print_grid(old_pos, m, pos, &map2);
        }

        let mut ans = 0;
        for (y, _) in map2.iter().enumerate() {
            for (x, _) in map2[y].iter().enumerate() {
                if map2[y as usize][x as usize] == "O".to_string() {
                    ans += 100 * y + x;
                }
            }
        }
        ans
    };
    println!("{}", part1_answer);

    // Part 2
    let part2_answer = {
        let mut pos = (robot.0, robot.1 * 2);
        let mut big_map: Vec<Vec<String>> = map.iter().map(|row| row.iter().flat_map(|cell| {
            let res: Vec<String> = match cell.as_str() {
                "#" => vec!["#", "#"],
                "O" => vec!["[", "]"],
                "." => vec![".", "."],
                _ => panic!("Bad cell in map2")
            }.iter().map(|r| r.to_string()).collect();
            res
        }).collect()).collect();

        for (_, m) in moves.iter().enumerate() {
            let dir = parse_move(m);

            // let old_pos = pos;
            if sokoban2(pos, dir, &mut big_map, &mut HashSet::new(), true) {
                sokoban2(pos, dir, &mut big_map, &mut HashSet::new(), false);
                pos = (pos.0 + dir.0, pos.1 + dir.1);
            }

            // println!("{}:{}: {:?}", file!(), line!(), (i));
            // print_grid(old_pos, m, pos, &big_map);
        }

        let mut ans = 0;
        for (y, _) in big_map.iter().enumerate() {
            for (x, _) in big_map[y].iter().enumerate() {
                if big_map[y as usize][x as usize] == "[".to_string() {
                    ans += 100 * y + x;
                }
            }
        }
        ans
    };
    println!("{}", part2_answer);
}

fn parse_move(m: &String) -> (i32, i32) {
    match m.as_str() {
        "^" => (-1, 0),
        ">" => (0, 1),
        "v" => (1, 0),
        "<" => (0, -1),
        _ => panic!("Bad move")
    }
}

pub fn sokoban(pos: (i32, i32), dir: (i32, i32), map: &mut Grid) -> bool {
    let new_pos = (pos.0 + dir.0, pos.1 + dir.1);
    let new_cell = map[new_pos.0 as usize][new_pos.1 as usize].as_str();
    let can_move = match new_cell {
        "." => true,
        "#" => false,
        "O" => sokoban(new_pos, dir, map),
        _ => panic!("Bad cell")
    };
    if can_move {
        map[new_pos.0 as usize][new_pos.1 as usize] = map[pos.0 as usize][pos.1 as usize].clone();
    }
    can_move
}

pub fn sokoban2(pos: (i32, i32), dir: (i32, i32), map: &mut Grid, checked: &mut HashSet<(i32, i32)>, dry: bool) -> bool {
    // This surpisingly does not seem to be required, but an opt to avoid wasted work
    if checked.contains(&pos) {
        return true;
    }
    checked.insert(pos);

    let new_pos = (pos.0 + dir.0, pos.1 + dir.1);
    let new_cell = map[new_pos.0 as usize][new_pos.1 as usize].as_str();
    let can_move = match new_cell {
        "." => true,
        "#" => false,
        "[" => {
            let mut success = sokoban2(new_pos, dir, map, checked, dry);
            if dir.0 != 0 {
                success = success && sokoban2((new_pos.0, new_pos.1 + 1), dir, map, checked, dry);
            }
            success
        }
        "]" => {
            let mut success = sokoban2(new_pos, dir, map, checked, dry);
            if dir.0 != 0 {
                success = success && sokoban2((new_pos.0, new_pos.1 - 1), dir, map, checked, dry);
            }
            success
        }
        _ => panic!("Bad cell")
    };
    if can_move && !dry {
        map[new_pos.0 as usize][new_pos.1 as usize] = map[pos.0 as usize][pos.1 as usize].clone();
        map[pos.0 as usize][pos.1 as usize] = ".".to_string();
    }
    can_move
}

#[allow(unused)]
fn print_grid(old_pos: (i32, i32), m: &String, pos: (i32, i32), map2: &Vec<Vec<String>>) {
    let mut overlay: HashMap<_, _> = HashMap::new();
    overlay.insert((old_pos.0 as usize, old_pos.1 as usize), m.clone());
    overlay.insert((pos.0 as usize, pos.1 as usize), m.clone());
    util::print_grid(map2, &overlay);
    println!();
}