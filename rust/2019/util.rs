use std::io::{self, BufRead};

pub fn get_lines() -> Vec<String> {
    io::stdin().lock().lines().map(|l| l.expect("failed to parse line")).collect()
}

pub fn debug_map(y: i32, x: i32, c: char) -> Vec<Vec<char>> {
    let mut ret = Vec::new();

    for i in 0..y {
        let mut line = Vec::new();
        for j in 0..x {
            line.push(c);
        }
        ret.push(line);
    }

    ret
}

pub fn debug_map_print(map: &Vec<Vec<char>>) {
    for y in 0..map.len() {
        let mut line = String::new();
        for x in 0..map[y].len() {
            line.push(map[y][x])
        }
        println!("{}", line);
    }
}