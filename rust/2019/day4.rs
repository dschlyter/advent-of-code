// use std::collections::HashMap;
// use std::collections::HashSet;

mod util;

fn main() {
    let lines = util::get_lines();

    part1(lines.clone());
    part2(lines);
}

fn part1(lines: Vec<String>) {
    let mut ans = 0;

    let range: Vec<i32> = lines[0].split("-").map(|n| n.parse::<i32>().unwrap()).collect();

    let start = range[0];
    let stop = range[1];

    for n in start..(stop+1) {
        let s = n.to_string();

        let mut double = false;
        let mut no_decrease = true;

        let mut last: char = '-';
        for c in s.chars() {
            if last == c {
                double = true;
            }

            if last != '-' && last > c {
                no_decrease = false;
            }

            last = c;
        }

        if double && no_decrease {
            ans += 1;
        }
    }

    println!("Part 1 answer {}", ans);
}

fn part2(lines: Vec<String>) {
    let mut ans = 0;

    let range: Vec<i32> = lines[0].split("-").map(|n| n.parse::<i32>().unwrap()).collect();

    let start = range[0];
    let stop = range[1];

    for n in start..(stop+1) {
        let s = n.to_string();

        let mut same = 0;
        let mut double = false;
        let mut no_decrease = true;

        let mut last: char = '-';
        for c in s.chars() {
            if last == c {
                same += 1;
            } else {
                if same == 1 {
                    double = true;
                }
                same = 0;
            }

            if last != '-' && last > c {
                no_decrease = false;
            }

            last = c;
        }

        if same == 1 {
            double = true;
        }

        if double && no_decrease {
            ans += 1;
        }
    }

    println!("Part 2 answer {}", ans);
}