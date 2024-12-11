#![allow(unused_imports)]

use std::collections::HashMap;
use std::collections::HashSet;
use aoc2024::util::grid_get;
use regex::Regex;

use aoc2024::util::{self, run_for_inputs};

pub fn main() {
    run_for_inputs(file!(), solve);
}

pub fn solve(filename: String) {
    let lines = util::read_lines(&filename);
    let stones: Vec<String> = lines[0].split_whitespace().map(|s| s.to_string()).collect();

    let mut mem = HashMap::new();
    
    let part1_answer = {
        let mut ans = 0;
        for stone in stones.iter() {
            ans += number_of_stones(stone, 25, &mut mem);
        }
        ans
    };
    println!("{}", part1_answer);


    let part2_answer = {
        let mut ans = 0;
        for stone in stones.iter() {
            ans += number_of_stones(&stone, 75, &mut mem);
        }
        ans
    };
    println!("{}", part2_answer);
}

pub fn number_of_stones(stone_value: &String, blinks: i32, mem: &mut HashMap<(String, i32), i64>) -> i64 {
    let key = (stone_value.to_string(), blinks);

    if let Some(memed) = mem.get(&key) {
        return *memed;
    }

    let ret = if blinks == 0 {
        // println!("{}:{}: {:?}", file!(), line!(), (&stone_value));
        1 
    } else if stone_value == "0" {
        number_of_stones(&"1".into(), blinks-1, mem)
    } else if stone_value.len() % 2 == 0 {
        let (p1, p2) = stone_value.split_at(stone_value.len() / 2);
        let s1 = p1.parse::<i64>().unwrap().to_string();
        let s2 = p2.parse::<i64>().unwrap().to_string();
        number_of_stones(&s1, blinks-1, mem) + number_of_stones(&s2, blinks-1, mem)
    } else {
        let new_value =  stone_value.parse::<i64>().unwrap() * 2024;
        number_of_stones(&new_value.to_string(), blinks-1, mem)
    };

    mem.insert(key, ret);

    ret
}