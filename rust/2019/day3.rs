use std::io::{self, BufRead};
use std::collections::HashMap;
use std::collections::HashSet;

fn main() {
    let lines = get_lines();

    part1(lines.clone());
    part2(lines);
}

fn part1(lines: Vec<String>) {
    let mut moves1: Vec<String> = lines[0].split(",").map(|n| n.parse::<String>().unwrap()).collect();
    let mut moves2: Vec<String> = lines[1].split(",").map(|n| n.parse::<String>().unwrap()).collect();

    let line1 = run_line(moves1);
    let line2 = run_line(moves2);

    let p1: HashSet<(i32, i32)> = line1.keys().map(|k| k.clone()).collect();
    let p2: HashSet<(i32, i32)> = line2.keys().map(|k| k.clone()).collect();
    let crossings = p1.intersection(&p2);

    let mut min_dist = 999999999;
    for cross in crossings {
        let mdist = cross.0.abs() + cross.1.abs();
        if mdist < min_dist {
            min_dist = mdist;
        }
    }

    println!("Part 1 answer {}", min_dist);
}

fn run_line(moves: Vec<String>) -> HashMap<(i32, i32), i32> {
    let mut ret = HashMap::new();
    let mut pos = (0, 0);
    let mut travelled = 0;

    for line_move in moves {
        let dir = &line_move[0..1];
        let dist = line_move[1..].parse::<i32>().unwrap();

        for _ in 0..dist {
            travelled += 1;
            let new_pos = match dir {
                "U" => (pos.0, pos.1-1),
                "D" => (pos.0, pos.1+1),
                "L" => (pos.0-1, pos.1),
                "R" => (pos.0+1, pos.1),
                _ => panic!(format!("unsupported dir {}", dir)),
            };
            ret.insert(new_pos, travelled);
            pos = new_pos;
        }
    }

    return ret
}

fn part2(lines: Vec<String>) {
    let mut moves1: Vec<String> = lines[0].split(",").map(|n| n.parse::<String>().unwrap()).collect();
    let mut moves2: Vec<String> = lines[1].split(",").map(|n| n.parse::<String>().unwrap()).collect();

    let line1 = run_line(moves1);
    let line2 = run_line(moves2);

    let p1: HashSet<(i32, i32)> = line1.keys().map(|k| k.clone()).collect();
    let p2: HashSet<(i32, i32)> = line2.keys().map(|k| k.clone()).collect();
    let crossings = p1.intersection(&p2);

    let mut min_dist = 999999999;
    for cross in crossings {
        let dist = *line1.get(cross).unwrap() + *line2.get(cross).unwrap();
        if dist < min_dist {
            min_dist = dist;
        }
    }

    println!("Part 2 answer {}", min_dist);
}


fn get_lines() -> Vec<String> {
    io::stdin().lock().lines().map(|l| l.expect("failed to parse line")).collect()
}