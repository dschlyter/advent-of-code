// use std::collections::HashMap;
use std::collections::HashSet;

mod util;

fn main() {
    let lines = util::get_lines();

    part1(&lines);
}

fn part1(lines: &Vec<String>) {
    let map: Vec<Vec<char>> = lines.into_iter().map(|l| l.chars().collect()).collect();

    // let mut dm = util::debug_map(lines.len() as i32, lines[0].len() as i32, ' ');


    let mut best = 0;
    let mut bestPos = (0, 0);
    let mut bestVisible = HashSet::new();


    for y in 0..lines.len() {
        for x in 0..map[y].len() {
            let mut count = 0;
            let mut visible = HashSet::new();

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
                                line_of_sight = false;
                            }
                        }
                    }

                    if line_of_sight {
                        count += 1;
                        visible.insert((y2, x2));
                    }
                }
            }

            if count > best {
                dbg!(format!("New best {} {} {}", y, x, count));
                best = count;
                bestPos = (y, x);
                bestVisible = visible;
            }
        }
    }

    println!("Part 1 answer {}", best);

    // Note: Since answer was 299 for part 1, and part 2 asks for the 200th destroyed asteroid we do not rotate more than a full round

    let (y, x) = bestPos;
    let mut positions: Vec<(f64, usize, usize)> = bestVisible.into_iter().map(|(y2, x2)| (rot_pos(y as i32, x as i32, y2 as i32, x2 as i32), y2, x2)).collect();

    positions.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

    if positions.len() >= 200 {
        println!("Part 2 answer {}", positions[199].2 * 100 + positions[199].1);
    }
}

fn rot_pos(y: i32, x: i32, y2: i32, x2: i32) -> f64 {
    let yd = (y2 - y) as f64;
    let xd = (x2 - x) as f64;
    let pi = std::f64::consts::PI;

    return (yd.atan2(xd) + 1e-8 + (pi/2.0) + 2.0*pi) % (2.0*pi);
}