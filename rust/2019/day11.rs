use std::collections::HashMap;

mod util;
mod computer;

fn main() {
    let lines = util::get_lines();

    part1(&lines);
    part2(&lines);
}

fn part1(lines: &Vec<String>) {
    let program: Vec<i64> = lines[0].split(",").map(|n| n.parse::<i64>().unwrap()).collect();

    let mut c = computer::Computer::new(&program);
    let mut colors: HashMap<(i32, i32), i32> = HashMap::new();

    let dirs = [(-1, 0), (0, 1), (1, 0), (0, -1)];
    let mut dir = 0;
    let mut pos = (0, 0);

    loop {
        let color: i32 = *colors.get(&pos).unwrap_or(&0);
        c.process(color as i64);

        if c.done() {
            break;
        }

        let paint = c.read() as i32;
        colors.insert(pos, paint);

        c.run_next();
        let turn = c.read() as i32;
        if turn == 0 {
            dir = (dir+3) % 4;
        } else {
            dir = (dir+1) % 4;
        }

        let move_dir = dirs[dir];
        pos = (pos.0+move_dir.0, pos.1+move_dir.1);
    }

    println!("Part 1 answer {}", colors.keys().len());
}

fn part2(lines: &Vec<String>) {
    let program: Vec<i64> = lines[0].split(",").map(|n| n.parse::<i64>().unwrap()).collect();

    let mut c = computer::Computer::new(&program);
    let mut colors: HashMap<(i32, i32), i32> = HashMap::new();

    let dirs = [(-1, 0), (0, 1), (1, 0), (0, -1)];
    let mut dir = 0;
    let mut pos = (0, 0);

    colors.insert(pos, 1);

    loop {
        let color: i32 = *colors.get(&pos).unwrap_or(&0);
        c.process(color as i64);

        if c.done() {
            break;
        }

        let paint = c.read() as i32;
        colors.insert(pos, paint);

        c.run_next();
        let turn = c.read() as i32;
        if turn == 0 {
            dir = (dir+3) % 4;
        } else {
            dir = (dir+1) % 4;
        }

        let move_dir = dirs[dir];
        pos = (pos.0+move_dir.0, pos.1+move_dir.1);
    }

    for y in 0..10 {
        let mut line = String::new();
        for x in 0..50 {
            if *colors.get(&(y, x)).unwrap_or(&0) == 1 {
                line.push('#');
            } else {
                line.push(' ');
            }
        }
        println!("{}", line);
    }

    println!("Part 2 answer {}", colors.keys().len());
}