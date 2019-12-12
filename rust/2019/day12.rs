mod util;
// mod computer;

use std::collections::HashMap;

extern crate regex;

fn main() {
    let lines = util::get_lines();

    part1(&lines);
    part2(&lines);
}

fn part1(lines: &Vec<String>) {
    let mut moons: Vec<Vec<i64>> = Vec::new();
    let mut vel = Vec::new();

    for line in lines {
        let filter = regex::Regex::new(r"[^0-9,-]");
        let new_line: String = filter.unwrap().replace_all(&line, "").to_string();

        let pos: Vec<i64> = new_line.split(",").map(|n| n.parse::<i64>().unwrap()).collect();
        moons.push(pos);
        vel.push([0, 0, 0].to_vec());
    }

    let steps = 1000;

    for step in 0..steps {

        for pos in 0..3 {
            for moon1 in 0..moons.len() {
                for moon2 in 0..moons.len() {
                    if moon1 == moon2 {
                        continue;
                    }
                    if moons[moon1][pos] < moons[moon2][pos] {
                        vel[moon1][pos] += 1;
                    }
                    if moons[moon1][pos] > moons[moon2][pos] {
                        vel[moon1][pos] -= 1;
                    }
                }
            }
        }

        for pos in 0..3 {
            for moon1 in 0..moons.len() {
                moons[moon1][pos] += vel[moon1][pos];
            }
        }

        // dbg!(&moons, &vel);
    }

    let mut sum = 0;
    for moon1 in 0..moons.len() {
        let (mut potential, mut kinetic) = (0,0);
        for pos in 0..3 {
            potential += moons[moon1][pos].abs();
        }

        for pos in 0..3 {
            kinetic += vel[moon1][pos].abs();
        }

        sum += potential * kinetic;
    }

    println!("Part 1 answer {}", sum);
}

fn part2(lines: &Vec<String>) {
    for trypos in 0..3 {
        let mut moons: Vec<Vec<i64>> = Vec::new();
        let mut vel = Vec::new();

        let mut used: HashMap<(Vec<Vec<i64>>, Vec<Vec<i64>>), i64> = HashMap::new();

        for line in lines {
            let filter = regex::Regex::new(r"[^0-9,-]");
            let new_line: String = filter.unwrap().replace_all(&line, "").to_string();

            let pos: Vec<i64> = new_line.split(",").map(|n| n.parse::<i64>().unwrap()).collect();
            moons.push(pos);
            vel.push([0, 0, 0].to_vec());
        }

        let steps = 1000000;
        let mut used_steps = 0;

        for step in 0..steps {
            let state = (moons.clone(), vel.clone());
            if used.contains_key(&state) {
                // Manually take LCM of the outputs here
                println!("{}", used_steps - used[&state]);
                break;
            }
            used.insert(state, used_steps);

            for pos in trypos..trypos+1 {
                for moon1 in 0..moons.len() {
                    for moon2 in 0..moons.len() {
                        if moon1 == moon2 {
                            continue;
                        }
                        if moons[moon1][pos] < moons[moon2][pos] {
                            vel[moon1][pos] += 1;
                        }
                        if moons[moon1][pos] > moons[moon2][pos] {
                            vel[moon1][pos] -= 1;
                        }
                    }
                }
            }

            for pos in trypos..trypos+1 {
                for moon1 in 0..moons.len() {
                    moons[moon1][pos] += vel[moon1][pos];
                }
            }
            used_steps += 1;
        }

        

        // 2029, 5899, 4703

        // 268297, 231615, 23327
                
    }

    
}
