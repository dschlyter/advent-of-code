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
    let mut ans = 0;
    let program: Vec<i64> = lines[0].split(",").map(|n| n.parse::<i64>().unwrap()).collect();

    let mut c = computer::Computer::new(&program);

    c.write(1);
    c.run();

    println!("Part 1 answer {}", c.read());
}

fn part2(lines: &Vec<String>) {
    let mut ans = 0;

    println!("Part 2 answer {}", ans);
}