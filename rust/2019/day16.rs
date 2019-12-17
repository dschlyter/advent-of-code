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

    // part1(&lines);
    part2(&lines);
}

fn part1(lines: &Vec<String>) {
    let now = Instant::now();
    let seq: Vec<i64> = lines[0].chars().map(|c| c.to_string().parse::<i64>().unwrap()).collect();

    let pattern = [0, 1, 0, -1].to_vec();

    let mut input = seq;
    let mut output: Vec<i64> = Vec::new();

    for phase in 0..100 {
        for pos in 0..input.len() {
            let mut pattern_i = 1;
            let mut res: i64 = 0;

            for input_pos in 0..input.len() {
                let pattern_digit = pattern[(pattern_i / (pos+1)) % pattern.len()];
                res += pattern_digit as i64 * input[input_pos];
                pattern_i += 1;
            }
            output.push(res.abs() % 10);
        }
        input = output;
        output = Vec::new();
    }

    dbg!(&input[0..8]);
    dbg!(now.elapsed());
}

fn part2(lines: &Vec<String>) {
    let now = Instant::now();

    let seq: Vec<i64> = lines[0].chars().map(|c| c.to_string().parse::<i64>().unwrap()).collect();

    let mut input: Vec<i64> = Vec::new();
    let mut output: Vec<i64> = Vec::new();

    for i in 0..10000 {
        for x in &seq {
            input.push(*x);
            output.push(0);
        }
    }

    // let pos = 303673; // test input
    // let pos = 5976277;
    let pos = lines[0][0..7].parse::<usize>().unwrap();
    dbg!(input.len());

    for phase in 0..100 {
        let mut cumsum = 0;
        for i in (pos..input.len()).rev() {
            cumsum = (cumsum + input[i]) % 10;
            output[i] = cumsum;
        }

        let tmp = input;
        input = output;
        output = tmp;
    }

    dbg!(&input[pos..pos+8]);
    dbg!(now.elapsed());

    dbg!("what???");
}
