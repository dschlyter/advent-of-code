use std::collections::HashMap;
use std::collections::HashSet;

mod util;

fn main() {
    let lines = util::get_lines();

    part1(lines.clone());
    part2(lines);
}

fn part1(lines: Vec<String>) {
    let mut orbits: HashMap<String, String> = HashMap::new();

    for line in &lines {
        let s: Vec<&str> = line.split(")").collect();
        let center = s[0].to_string();
        let satellite = s[1].to_string();

        orbits.insert(satellite, center);
    }

    let mut ans = 0;

    for key in orbits.keys() {
        ans += count_orbits(&orbits, &key);
    }

    println!("Part 1 answer {}", ans);
}

fn count_orbits(orbits: &HashMap<String, String>, target: &String) -> i32 {
    if !orbits.contains_key(target) {
        return 0;
    }

    return 1 + count_orbits(&orbits, &(orbits[target]));
}

fn part2(lines: Vec<String>) {
    let mut orbits: HashMap<String, String> = HashMap::new();

    for line in &lines {
        let s: Vec<&str> = line.split(")").collect();
        let center = s[0].to_string();
        let satellite = s[1].to_string();

        orbits.insert(satellite, center);
    }

    let targets = orbiting(&orbits, &"SAN".to_string(), 0);
    dbg!(&targets);
    let ans = count_distance(&orbits, &targets, &"YOU".to_string()) - 2;

    // 170 too low, 389 high
    println!("Part 2 answer {}", ans);
}

fn orbiting(orbits: &HashMap<String, String>, target: &String, dist: i32) -> HashMap<String, i32> {
    if !orbits.contains_key(target) {
        return HashMap::new();
    }

    let mut children = orbiting(&orbits, &orbits[target], dist+1);
    children.insert(target.clone(), dist);
    return children;
}

fn count_distance(orbits: &HashMap<String, String>, targets: &HashMap<String, i32>, me: &String) -> i32 {
    if targets.contains_key(me) {
        return *targets.get(me).unwrap();
    }

    return 1 + count_distance(orbits, targets, &orbits[me]);
}