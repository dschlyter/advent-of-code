use std::io::{self, BufRead};

fn main() {
    let lines = get_lines();

    part1(lines.clone());
    part2(lines);
}

fn part1(lines: Vec<String>) {
    let mut reg: Vec<usize> = lines[0].split(",").map(|n| n.parse::<usize>().unwrap()).collect();

    reg[1] = 12;
    reg[2] = 2;

    let res = run_program(&mut reg);

    println!("Part 1 answer {}", res);
}

fn run_program(reg: &mut Vec<usize>) -> usize {
    let mut ip = 0;

    loop {
        let (op, a, b, c) = (reg[ip], reg[ip+1], reg[ip+2], reg[ip+3]);

        if op == 1 {
            reg[c] = reg[a] + reg[b];
        } else if op == 2 {
            reg[c] = reg[a] * reg[b];
        } else if op == 99 {
            break;
        } else {
            panic!(format!("Unsupported opcode! {}", op));
        }

        ip += 4;
    }

    return reg[0]
}

fn part2(lines: Vec<String>) {
    for i in 1..100 {
        for j in 1..100 {
            let mut reg: Vec<usize> = lines[0].split(",").map(|n| n.parse::<usize>().unwrap()).collect();

            reg[1] = i;
            reg[2] = j;

            let res = run_program(&mut reg);

            if res == 19690720 {
                let ans = 100 * i + j;
                println!("Part 2 answer {}", ans);
            }
        }
    }

}


fn get_lines() -> Vec<String> {
    io::stdin().lock().lines().map(|l| l.expect("failed to parse line")).collect()
}