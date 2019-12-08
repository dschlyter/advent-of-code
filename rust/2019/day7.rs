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
    let program: Vec<i32> = lines[0].split(",").map(|n| n.parse::<i32>().unwrap()).collect();
    let mut best = 0;

    for a in 0..5 {
        let out1 = test_program(&program, &([a, 0].to_vec()));
        for b in 0..5 {
            if a == b {continue;}
            let out2 = test_program(&program, &([b, out1].to_vec()));
            for c in 0..5 {
                if c == b || c == a {continue;}
                let out3 = test_program(&program, &([c, out2].to_vec()));
                for d in 0..5 {
                    if d == c || d == b || d == a {continue;}
                    let out4 = test_program(&program, &([d, out3].to_vec()));
                    for e in 0..5 {
                        if e == d || e == c || e == b || e == a {continue;}
                        let out5 = test_program(&program, &([e, out4].to_vec()));
                        best = i32::max(best, out5);
                    }
                }
            }
        }
    }

    println!("Part 1 answer {}", best);
}

fn part2(lines: &Vec<String>) {
    let mut ans = 0;

    println!("Part 2 answer {}", ans);
}

fn test_program(program: &Vec<i32>, input: &Vec<i32>) -> i32 {
    return computer::run_program(&mut program.to_vec(), &input)[0];
}