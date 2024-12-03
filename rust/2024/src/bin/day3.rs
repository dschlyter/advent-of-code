// #![allow(unused_imports)]

#[allow(unused_imports)]
use std::collections::HashMap;
#[allow(unused_imports)]
use std::collections::HashSet;
use regex::Regex;

use aoc2024::util::{self, run_for_inputs};

pub fn main() {
    run_for_inputs(file!(), solve);
}

pub fn solve(filename: String) {
    // Input

    let lines = util::read_lines(&filename);
    let line = lines.join("");

    // Part 1

    {
        let mut res = 0;
        let reg = Regex::new(r"mul\(\d+,\d+\)").unwrap();
        let reg_num = Regex::new(r"\d+").unwrap();

        for mat in reg.find_iter(&line) {
            let mul = mat.as_str();
            let p: i32 = reg_num.find_iter(mul).map(|m| m.as_str().parse::<i32>().unwrap()).product();
            res += p;
        }

        println!("{}", res);
    }


    // Part 2

    {
        let mut res = 0;
        let reg = Regex::new(r"mul\(\d+,\d+\)|do\(\)|don't\(\)").unwrap();
        let reg_num = Regex::new(r"\d+").unwrap();

        let mut doit = true;
        for mat in reg.find_iter(&line) {
            match mat.as_str() {
                "do()" => { 
                    doit = true; 
                }
                "don't()" => {
                    doit = false;
                }
                mul if doit => {
                    let p: i32 = reg_num.find_iter(mul).map(|m| m.as_str().parse::<i32>().unwrap()).product();
                    res += p;
                }
                _ => ()
            }
        }

        println!("{}", res);
    }
}