use std::io::{self, BufRead};

pub fn get_lines() -> Vec<String> {
    io::stdin().lock().lines().map(|l| l.expect("failed to parse line")).collect()
}