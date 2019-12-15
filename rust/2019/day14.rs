mod util;
mod computer;

use std::collections::HashMap;

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
    c.run();

    let mut blocks = 0;

    while c.can_read() {
        c.read();
        c.read();
        if c.read() == 2 {
            blocks += 1;
        }
    }

    println!("Part 1 answer {}", blocks);
}

fn part2(lines: &Vec<String>) {
    let mut program: Vec<i64> = lines[0].split(",").map(|n| n.parse::<i64>().unwrap()).collect();
    program[0] = 2;

    let mut score = 0;

    let mut c = computer::Computer::new(&program);

    let mut screen = Vec::new();
    for y in 0..30 {
        screen.push(Vec::new());
        for x in 0..50 {
            screen[y].push(0);
        }
    }

    let mut ball_pos = 0;
    let mut paddle_pos = 0;

    loop {
        c.run_to_input();

        while c.can_read() {
            let x = c.read();
            let y = c.read();
            if x == -1 && y == 0 {
                score = c.read();
            } else {
                let tile = c.read();
                screen[y as usize][x as usize] = tile;
                if tile == 3 {
                    paddle_pos = x;
                }
                if tile == 4 {
                    ball_pos = x;
                }
            }
        }

        if ball_pos < paddle_pos {
            c.write(-1);
        } else if ball_pos > paddle_pos {
            c.write(1);
        } else {
            c.write(0);
        }
        dbg!(ball_pos, paddle_pos);

        print!("{}[2J", 27 as char);
        for y in 0..30 {
            let mut line = String::new();
            for x in 0..50 {
                line.push(match screen[y][x] {
                    1 => '#',
                    2 => 'B',
                    3 => '-',
                    4 => '0',
                    _ => ' '
                })
            }
            println!("{}", line);
        }

        if c.done() {
            break;
        }

        // let sleep_time = time::Duration::from_millis(50);
        // thread::sleep(sleep_time);
    }

    println!("Part 2 answer {}", score);
}
