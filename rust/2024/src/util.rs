use std::{env, fs::read_to_string, hash::Hash};
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::str::FromStr;

pub fn run_for_inputs(code_filename: &str, solve: fn(String)) {
    let arg = suffix_arg();
    match arg {
        None => {
            solve_if_exists(input_with_suffix(&code_filename, "_test".into()), solve);
            solve_if_exists(input_with_suffix(&code_filename, "_test2".into()), solve);
            solve_if_exists(input_with_suffix(&code_filename, "".into()), solve);
        }
        Some(suffix) => solve_if_exists(input_with_suffix(&code_filename, suffix), solve)
    }
}

fn solve_if_exists(input_file: String, solve: fn(String)) {
    if Path::new(&input_file).exists() {
        solve(input_file);
    } else {
        println!("Input file {} not found, skipping...", &input_file);
    }
}

fn suffix_arg() -> Option<String> {
    let args: Vec<String> = env::args().collect();
    return args.get(1).cloned();
}

fn input_with_suffix(filename: &str, suffix: String) -> String {
    let day_name = filename.replace("src/bin/", "").replace(".rs", "");
    let input_file = format!("input/{}{}.txt", day_name, suffix);
    return input_file
}

pub fn input_for(filename: &str) -> String {
    return input_with_suffix(filename, suffix_arg().unwrap_or(String::default()));
}

// https://doc.rust-lang.org/rust-by-example/std_misc/file/read_lines.html
// Simple but a bit inefficient due to reallocation
pub fn read_lines(filename: &str) -> Vec<String> {
    dbg!(filename);
    read_to_string(filename) 
        .unwrap()
        .lines()
        .map(String::from)
        .collect()
}

pub fn count<T: Eq + Hash>(map: &mut HashMap<T, i32>, key: T) {
    let counter = map.entry(key).or_insert(0);
    *counter += 1;
}

// Grid utils

pub fn to_grid(lines: &Vec<String>) -> Vec<Vec<String>> {
    lines.into_iter().map(|s| s.chars().map(|c| c.to_string()).collect()).collect()
}

pub fn to_grid_typed<T: FromStr>(lines: &Vec<String>) -> Vec<Vec<T>> where <T as FromStr>::Err: std::fmt::Debug {
    lines.into_iter().map(|s| s.chars().map(|c| c.to_string().parse::<T>().unwrap()).collect()).collect()
}

pub fn grid_get(grid: &Vec<Vec<String>>, y: i32, x: i32) -> Option<&String> {
    return grid.get(y as usize).and_then(|line| line.get(x as usize));
}

pub fn grid_mapped_by_cell<'a>(grid: &'a Vec<Vec<String>>, ignore: &HashSet<String>) -> HashMap<&'a String, Vec<(i32, i32)>> {
    let mut ret = HashMap::new();

    for (y, row) in grid.iter().enumerate() {
        for (x, cell) in row.iter().enumerate() {
            if !ignore.contains(cell) {
                ret.entry(cell).or_insert(Vec::new()).push((y as i32,x as i32));
            }
        }
    }
    
    ret
}



// ignore unused
// #[allow(dead_code)]