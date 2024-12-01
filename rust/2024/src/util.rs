use std::{collections::HashMap, env, fs::read_to_string, hash::Hash};

pub fn input_for(filename: &str) -> String {
    let day_name = filename.replace("src/bin", "").replace(".rs", "");
    let args: Vec<String> = env::args().collect();
    let suffix = args.get(1).cloned().unwrap_or(String::default());
    let input_file = format!("input/{}{}.txt", day_name, suffix);
    return input_file
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

// ignore unused
#[allow(dead_code)]
pub fn hello() {
    println!("Hello World Util!"); 
}