#![allow(unused_imports)]

use std::collections::{HashMap, HashSet, BinaryHeap};
use std::cmp::Reverse;
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
    let (register_lines, program_lines) = util::read_lines_split(&filename);

    let registers: Vec<i64> = register_lines.iter().map(|s| s.split(": ").nth(1).unwrap().parse::<i64>().unwrap()).collect();
    let program: Vec<i64> = program_lines[0].replace("Program: ", "").split(",").map(|s| s.parse::<i64>().unwrap()).collect();

    // Part 1
    let part1_answer = {
        let mut r = registers.clone();
        let mut ip = 0;
        let mut output = Vec::new();

        while ip < program.len() {
            let inst = program[ip];
            let op = program[ip + 1];
            let combo_val = match op {
                0 | 1 | 2 | 3 => op,
                4 => r[0],
                5 => r[1],
                6 => r[2],
                _  if inst != 1 && inst != 3 && inst != 4 => panic!("Invalid op"),
                _ => -1 // Won't be read
            };

            match inst {
                0 => r[0] = r[0] >> combo_val,
                1 => r[1] = r[1] ^ op,
                2 => r[1] = combo_val % 8,
                3 => if r[0] != 0 {
                    ip = op as usize;
                    continue;
                }
                4 => r[1] = r[1] ^ r[2],
                5 => output.push(format!("{}", combo_val % 8)),
                6 => r[1] = r[0] >> combo_val,
                7 => r[2] = r[0] >> combo_val,
                _ => panic!("Invalid op")
            }
            ip += 2;
        }

        // println!("{}:{}: {:?}", file!(), line!(), (&r));
        output.join(",")
    };
    println!("{}", part1_answer);

    // Part 2
    // Answer is too big to brute force
    // Since program seem to be shifting the input - maintain a list of successful numbers for producing a subsequence of the target output
    // Then find newer numbers by extending those numbers
    let part2_answer = {
        let mut success_numbers = HashSet::new();
        for n in 0..(2i64.pow(7)) {
            success_numbers.insert(n);
        }

        for target_length in 1..(program.len()+1) {
            let mut next_success = HashSet::new();
            for prev_number in &success_numbers {
                for next_number in 0..(2i64.pow(7)) {
                    let a_start = (next_number << (target_length * 3)) + prev_number;

                    let mut r: Vec<i64> = registers.iter().map(|n| *n as i64).collect();
                    r[0] = a_start;
                    let mut ip = 0;
                    let mut output_i = 0;

                    while ip < program.len() {
                        let inst = program[ip] as i64;
                        let op = program[ip + 1] as i64;
                        let combo_val: i64 = match op {
                            0 | 1 | 2 | 3 => op,
                            4 => r[0],
                            5 => r[1],
                            6 => r[2],
                            _  if inst != 1 && inst != 3 && inst != 4 => panic!("Invalid op"),
                            _ => -1 // Won't be read
                        };

                        match inst {
                            0 => r[0] = r[0] >> combo_val,
                            1 => r[1] = r[1] ^ op,
                            2 => r[1] = combo_val % 8,
                            3 => if r[0] != 0 {
                                ip = op as usize;
                                continue;
                            }
                            4 => r[1] = r[1] ^ r[2],
                            5 => {
                                let out = combo_val % 8;
                                if program[output_i] as i64 != out {
                                    break;
                                }
                                output_i += 1;
                                if output_i >= target_length {
                                    // println!("{}:{}: {:?}", file!(), line!(), ("output", a_start, out));
                                    next_success.insert(a_start);
                                    break;
                                }
                            },
                            6 => r[1] = r[0] >> combo_val,
                            7 => r[2] = r[0] >> combo_val,
                            _ => panic!("Invalid op")
                        }
                        ip += 2;
                    }
                }
            }
            success_numbers = next_success;
        }

        *(success_numbers.iter().min().unwrap_or(&-1))
    };
    println!("{}", part2_answer);

    // Part 2 - OLD
    let part2_answer_old = {
        // First attempt - full simulation brute force, too slow
        /*
        loop {
            if filename.contains("_test.txt") {
                // Test input not supported ??
                break -1;
            }

            if a_start > 1_000_000 {
                break -1;
            }

            if a_start % 10_000 == 0 {
                println!("{}:{}: {:?}", file!(), line!(), ("checking", a_start));
            }

            let mut r = registers.clone();
            r[0] = a_start;
            let mut ip = 0;
            let mut output_i = 0;

            while ip < program.len() {
                let inst = program[ip];
                let op = program[ip + 1];
                let combo_val = match op {
                    0 | 1 | 2 | 3 => op,
                    4 => r[0],
                    5 => r[1],
                    6 => r[2],
                    _  if inst != 1 && inst != 3 && inst != 4 => panic!("Invalid op"),
                    _ => -1 // Won't be read
                };

                match inst {
                    0 => r[0] = r[0] / 2i32.pow(combo_val as u32),
                    1 => r[1] = r[1] ^ op,
                    2 => r[1] = combo_val % 8,
                    3 => if r[0] != 0 {
                        ip = op as usize;
                        continue;
                    }
                    4 => r[1] = r[1] ^ r[2],
                    5 => {
                        let out = combo_val % 8;
                        if output_i >= program.len() || program[output_i] != out {
                            output_i = 0;
                            break;
                        }
                        output_i += 1;
                    },
                    6 => r[1] = r[0] / 2i32.pow(combo_val as u32),
                    7 => r[2] = r[0] / 2i32.pow(combo_val as u32),
                    _ => panic!("Invalid op")
                }
                ip += 2;
            }

            if output_i == program.len() {
                break a_start;
            }
            a_start += 1;
        }
        */

        // Manually compile the program and run it
        // This works for the test input, but too slow for the full one
        let mut a_start: i64 = 0;
        loop {
            if filename != "input/day17_test2.txt".to_string() {
                // Wrong input
                break -1;
            }

            let mut output_i = 0;
            let mut a = a_start;
            // let mut b = 0;
            // let mut c = 0;

            loop {
                a = a / 2i64.pow(3);
                let out = a % 8;
                if output_i >= program.len() || out != program[output_i] {
                    output_i = 0;
                    break;
                }
                output_i += 1;
                if a == 0 {
                    break;
                }
            }

            if output_i == program.len() {
                println!("{}:{}: {:?}", file!(), line!(), ("res _test", a_start));
                break a_start;
            }
            a_start += 1;
        }

        // This is too slow
        /*
        a_start = 0;
        let mut best_i = 0;
        loop {
            if filename != "input/day17.txt".to_string() {
                // Wrong input
                break -1;
            }

            if a_start % 1000_000 == 0 {
                println!("{}:{}: {:?}", file!(), line!(), ("checking main input", a_start));
            }

            let mut output_i = 0;
            let mut a: i64 = a_start;
            // let mut a: i64 = 37283687;
            let mut b: i64 = 0;
            let mut c: i64 = 0;

            loop {
                b = a % 8;
                b = b ^ 3;
                c = a >> b;
                b = b ^ c;
                b = b ^ 3;
                a = a >> 3;
                let out = b % 8;
                // println!("{}:{}: {:?}", file!(), line!(), (out));
                if output_i >= program.len() || out != program[output_i] as i64 {
                    output_i = 0;
                    break;
                }
                output_i += 1;
                if output_i > best_i {
                    println!("{}:{}: {:?}", file!(), line!(), ("best i", best_i, &a_start));
                    println!("The binary representation of {} is {:b}", a_start, a_start);
                    best_i = output_i;
                }
                if a == 0 {
                    break;
                }

                X = (b ^ (a >> (b ^ 3)) % 8
            }

            if output_i == program.len() {
                break a_start;
            }
            a_start += 1;
        }

        a = 16

        b = 0
        b = 3
        c = 2
        b = 3 ^ 2 = 1
        b = 1 ^ 3 = 2
        out = 2 % 8

        a = 23

        b = 


        */



    };
    println!("{}", part2_answer_old);
}