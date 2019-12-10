// use std::collections::HashMap;
// use std::collections::HashSet;

mod util;

fn main() {
    let lines = util::get_lines();

    part1(&lines);
    part2(&lines);
}

fn part1(lines: &Vec<String>) {
    let map: Vec<Vec<char>> = lines.into_iter().map(|l| l.chars().collect()).collect();

    // let mut dm = util::debug_map(lines.len() as i32, lines[0].len() as i32, ' ');

    let mut best = 0;
    for y in 0..lines.len() {
        for x in 0..map[y].len() {
            let mut count = 0;
            if map[y][x] != '#' {
                continue;
            }
            // dbg!(format!("Checking {} {}", y, x));
            for y2 in 0..lines.len() {
                for x2 in 0..map[y].len() {
                    if map[y2][x2] != '#' {
                        continue;
                    }
                    if x == x2 && y == y2 {
                        continue;
                    }

                    let mut line_of_sight = true;

                    let xstart = std::cmp::min(x, x2);
                    let xstop = std::cmp::max(x, x2);
                    let ystart = std::cmp::min(y, y2);
                    let ystop = std::cmp::max(y, y2);

                    if x == x2 {
                        for ymiddle in (ystart+1)..ystop {
                            if map[ymiddle][x] == '#' {
                                line_of_sight = false;
                            }
                        }
                    } else {
                        let k = (y2 as f64 - y as f64) / (x2 as f64 - x as f64);
                        let m = y as f64 - k * x as f64;

                        for xmiddle in (xstart+1)..xstop {
                            let ypos = k * (xmiddle as f64) + m;

                            if (ypos.round() - ypos).abs() > 1e-8 {
                                // Not on even square
                                continue;
                            }

                            if map[ypos.round() as usize][xmiddle] == '#' {
                                if y == 1 && x == 14 {
                                    let p: usize = ypos.round() as usize;
                                }
                                line_of_sight = false;
                            }
                        }
                    }

                    if line_of_sight {
                        count += 1;
                    }
                }
            }

            if count > best {
                dbg!(format!("New best {} {} {}", y, x, count));
                best = count;
            }
        }
    }

    println!("Part 1 answer {}", best);
}

fn part2(lines: &Vec<String>) {
    let mut ans = 0;
    println!("Part 2 answer {}", ans);
}