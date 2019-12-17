mod util;
mod computer;

// use std::collections::HashSet;
// use std::collections::HashMap;
// use std::collections::LinkedList;

// use std::{thread, time, char};
use std::time::Instant;

extern crate regex;

fn main() {
    let lines = util::get_lines();

    part1(&lines);
    // part2(&lines);
}

fn part1(lines: &Vec<String>) {
    // let program: Vec<i64> = lines[0].split(",").map(|n| n.parse::<i64>().unwrap()).collect();

    // let mut c = computer::Computer::new(&program);

    let program: Vec<i64> = lines[0].split(",").map(|n| n.parse::<i64>().unwrap()).collect();

    let mut c = computer::Computer::new(&program);
    c.run();

    let mut lines = Vec::new();
    let mut line = Vec::new();

    let mut line_no = 0;
    let mut show = String::from(line_no.to_string());

    while c.can_read() {
        let ch = c.read();

        line.push(ch);

        if ch == 10 {
            lines.push(line);
            line = Vec::new();

            println!("{}", show);
            line_no += 1;
            show = String::from(line_no.to_string());

            continue;
        } else {
            show.push((ch as u8) as char);
        }

        
    }

    let mut score = 0;
    for y in 1..lines.len()-2 {
        for x in 1..lines[y].len()-1 {
            if 
            lines[y][x] != 46 &&
            lines[y-1][x] != 46 &&
            lines[y+1][x] != 46 &&
            lines[y][x-1] != 46 &&
            lines[y][x+1] != 46
            {
                dbg!((y, x));
                score += y * x;
            }
        }
    }

    // 1462 too low
    // 33342 too high
    println!("Part 1 answer {}", score);
}
