mod util;
mod computer;

use std::collections::HashSet;
use std::collections::HashMap;
use std::collections::LinkedList;

use std::{thread, time, char};

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

    let mut steps = 0;
    let mut oxygen_pos = (0, 0);

    queue.push_back((0, 0, Vec::new()));

    // BFS

    while queue.len() > 0 {
        let (y, x, moves) = queue.pop_front().unwrap();

        // dbg!((y, x, &moves));

        if visited.contains(&(y, x)) {
            continue;
        }

        visited.insert((y, x));

        let mut i = 0;
        for robot_move in &moves {
            c.process(*robot_move);
            i += 1;
            if c.read() == 0 {
                dbg!(i);
                panic!("Error entering position")
            }
        }

        for i in 1..5 {
            c.process(i);
            let res = c.read();

            if res == 0 {
                continue;
                let new_pos = ((y + dirs[i as usize].0), (x + dirs[i as usize].1));
                if visited.contains(&new_pos) {
                    dbg!("tiny walls!");
                }
            }

            if res == 2 {
                dbg!((y, x));
                oxygen_pos = (y, x);
                if steps == 0 {
                    steps = moves.len() + 1;
                }
            }

            let mut new_moves = moves.clone();
            new_moves.push(i);

            queue.push_back(((y + dirs[i as usize].0), (x + dirs[i as usize].1), new_moves));

            // Move back by xoring with 1
            c.process(rev_dir[&i]);
            if c.read() == 0 {
                panic!("Error moving back");
            }
        }

        for robot_move in moves.iter().rev() {
            c.process(rev_dir[robot_move]);
            if c.read() == 0 {
                panic!("Error returning to 0,0")
            }
        }
    }

    println!("Part 1 answer {}", steps);

    let rooms = visited;

    let mut queue2 = LinkedList::new();
    let mut visited2 = HashSet::new();
    let mut max_dist = 0;

    queue2.push_back((oxygen_pos.0, oxygen_pos.1, 0));

    let mut d = util::debug_map(45, 45, ' ');

    while queue2.len() > 0 {
        let (y, x, dist) = queue2.pop_front().unwrap();

        if !rooms.contains(&(y, x)) {
            continue;
        }

        if visited2.contains(&(y, x)) {
            continue;
        }

        // d[(y+25) as usize][(x+25) as usize] = 'X';
        // util::debug_map_print(&d);
        // d[(y+25) as usize][(x+25) as usize] = char::from_digit(dist / 100 % 10, 10).unwrap();
        // d[(oxygen_pos.0+25) as usize][(oxygen_pos.1+25) as usize] = '0';

        visited2.insert((y, x));

        if dist > max_dist {
            max_dist = dist;
        }

        for i in 1..5 {
            queue2.push_back(((y + dirs[i as usize].0), (x + dirs[i as usize].1), dist+1));
        }

    }


    let idontknowwhy = 1;
    println!("Part 2 answer {}", max_dist + idontknowwhy);
}

fn part2(lines: &Vec<String>) {
}
