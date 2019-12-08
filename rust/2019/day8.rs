// use std::collections::HashMap;
// use std::collections::HashSet;

mod util;
mod computer;

fn main() {
    let lines = util::get_lines();

    part1(&lines);
    part2(&lines);
}

fn part1(lines: &Vec<String>) {
    let digits: Vec<u32> = lines[0].chars().map(|n| n.to_digit(10).unwrap()).collect();

    let mut best = 999999999;
    let mut ans = 0;

    let width = 25;
    let height = 6;
    let layer_size = width * height;
    let layers = digits.len() / layer_size;

    for l in 0..layers {
        let mut z = 0;
        let mut o = 0;
        let mut t = 0;

        for y in 0..height {
            for x in 0..width {
                let i = l * layer_size + y * width + x;

                let d = digits[i];
                if d == 0 { z += 1; }
                else if d == 1 { o += 1; }
                else if d == 2 { t += 1; }
                else {panic!(d); }
            }
        }

        if (z < best) {
            best = z;
            ans = o * t;
        }
    }

    println!("Part 1 answer {}", ans);
}

fn part2(lines: &Vec<String>) {
    let digits: Vec<u32> = lines[0].chars().map(|n| n.to_digit(10).unwrap()).collect();

    let mut best = 999999999;
    let mut ans = 0;

    let width = 25;
    let height = 6;
    let layer_size = width * height;
    let layers = digits.len() / layer_size;

    for y in 0..height {
        let mut line = "".to_string();
        for x in 0..width {
            for l in 0..layers {
                let i = l * layer_size + y * width + x;
                let d = digits[i];

                if d == 2 {
                    continue;
                }
                
                let s = match d {
                    0 => " ",
                    1 => "W",
                    _ => panic!("")
                };

                line = format!("{}{}", line, s);
                break;
            }
        }
        println!("{}", line);
    }

    println!("Part 2 answer {}", ans);
}

fn test_program(program: &Vec<i32>, input: &Vec<i32>) -> i32 {
    return computer::run_program(&mut program.to_vec(), &input)[0];
}