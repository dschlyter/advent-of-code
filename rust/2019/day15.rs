mod util;
mod computer;

use std::collections::HashSet;
use std::collections::HashMap;
use std::collections::LinkedList;

use std::{thread, time};

extern crate regex;

fn main() {
    let lines = util::get_lines();

    part1(&lines);
    part2(&lines);
}

fn part1(lines: &Vec<String>) {
    let program: Vec<i64> = lines[0].split(",").map(|n| n.parse::<i64>().unwrap()).collect();

    let mut c = computer::Computer::new(&program);

    let dirs = [(0, 0), (-1,0), (1,0), (0,-1), (0,1)].to_vec();
    let mut rev_dir: HashMap<i64, i64> = HashMap::new();
    rev_dir.insert(1, 2);
    rev_dir.insert(2, 1);
    rev_dir.insert(3, 4);
    rev_dir.insert(4, 3);

    let mut queue = LinkedList::new();
    let mut visited = HashSet::new();

    let mut search_done = false;
    let mut steps = 0;

    queue.push_back((0, 0, Vec::new()));

    // BFS

    while !search_done {
        let (y, x, moves) = queue.pop_front().unwrap();

        // dbg!((y, x, &moves));

        if visited.contains(&(y, x)) {
            continue;
        }

        visited.insert((y, x));

        for robot_move in &moves {
            c.process(*robot_move);
            if c.read() != 1 {
                panic!("Error entering position")
            }
        }

        for i in 1..5 {
            c.process(i);
            let res = c.read();

            if res == 0 {
                continue;
            }

            if res == 2 {
                if steps == 0 {
                    steps = moves.len() + 1;
                    search_done = true;
                }
            }

            let mut new_moves = moves.clone();
            new_moves.push(i);

            queue.push_back(((y + dirs[i as usize].0), (x + dirs[i as usize].1), new_moves));

            // Move back by xoring with 1
            c.process(rev_dir[&i]);
            if c.read() != 1 {
                panic!("Error moving back");
            }
        }

        if search_done {
            break;
        }

        for robot_move in moves.iter().rev() {
            c.process(rev_dir[robot_move]);
            if c.read() != 1 {
                panic!("Error returning to 0,0")
            }
        }
    }

    println!("Part 1 answer {}", steps);
}

fn part2(lines: &Vec<String>) {
}
