#![allow(unused_imports)]

use core::num;
use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};
use std::cmp::Reverse;
use std::mem::swap;
use std::vec;
use aoc2024::util::{Grid, grid_get};
use regex::Regex;
use std::ops::Add;

use aoc2024::util::{self, run_for_inputs, Point};

pub fn main() {
    run_for_inputs(file!(), solve);
}

type Int = i64;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Gate {
    left: String,
    op: String,
    right: String
}

pub fn solve(filename: String) {
    let (wire_lines, gate_lines) = util::read_lines_split(&filename);

    let wires: HashMap<String, Int> = wire_lines.iter().map(|s| {
        let sp: Vec<_> = s.split(": ").collect();
        (sp[0].to_string(), sp[1].parse::<Int>().unwrap())
    }).into_iter().collect();
    let gates: HashMap<String, Gate> = gate_lines.iter().map(|s| {
        let sp: Vec<_> = s.split(" -> ").collect();
        let left_sp: Vec<_> = sp[0].split(" ").collect();
        (sp[1].to_string(), Gate{left: left_sp[0].to_string(), op: left_sp[1].to_string(), right: left_sp[2].to_string()})
    }).into_iter().collect();

    // Part 1
    let part1_answer = {
        let mut ans = 0;
        for i in 0..64 {
            let pos = 63 - i;
            let key = "z".to_string() + (if pos < 10 {"0"} else {""}) + &pos.to_string();
            if gates.contains_key(&key) {
                ans = ans << 1 | eval(&key, &gates, &wires, &mut HashSet::new());
            }
        }
        ans
    };
    println!("{}", part1_answer);

    // Part 2
    let part2_answer = {
        if filename.contains("_test") {
            println!("{}:{}: {:?}", file!(), line!(), ("Skipping part 2 - not defined for tests"));
            return;
        }

        let mut tests: Vec<Int> = vec![1, 2, 3, 4];
        loop {
            let t = *tests.last().unwrap();
            if t > (1 << 60) {
                break;
            }
            tests.push(t + 1 + t/10);
        }


        let mut fixed_gates = gates.clone();
        'testing: for t in &tests {
            let r = run(*t, *t, &gates);
            if r == t + t {
                println!("{}:{}: {:?}", file!(), line!(), (t, "is OK!"));
            } else {
                println!("{}:{}: {:?}", file!(), line!(), (t, "did not equal", t+t, "but", r));

                let diffing_bits = r ^ (t + t);
                println!("Diffing bits {:b}", diffing_bits);
                let mut problem_wires = HashSet::new();
                for i in 0..64 {
                    if (diffing_bits >> i) % 2 == 0 {
                        continue;
                    }

                    let key = "z".to_string() + (if i < 10 {"0"} else {""}) + &i.to_string();
                    if gates.contains_key(&key) {
                        let mut nodes = HashSet::new();
                        child_node_set(&key, &fixed_gates, &wires, &mut HashSet::new(), &mut nodes);
                        problem_wires.extend(nodes.into_iter());
                    }
                }
                println!("{}:{}: {:?}", file!(), line!(), ("Suspected nodes", &problem_wires));


                for key1 in &problem_wires {
                    for key2 in &problem_wires {
                        if key1 >= key2 {
                            continue;
                        }
                        println!("{}:{}: {:?}", file!(), line!(), ("Swapping", key1, key2));

                        swap_keys(&mut fixed_gates, key1, key2);
                        let mut good_swap = true;
                        for t2 in &tests {
                            if t2 > t {
                                break;
                            }
                            if run(*t2, *t2, &fixed_gates) != t2 + t2 {
                                good_swap = false;
                                break;
                            }
                        }
                        if good_swap {
                            println!("{}:{}: {:?}", file!(), line!(), ("Good swap found"));
                            continue 'testing;
                        }
                        // Restore the swap and try another
                        swap_keys(&mut fixed_gates, key1, key2);
                    }
                }
                panic!("A single swap did not fix the issue for this candidate")
            }
        }

        // Simple verification
        for &t in &tests {
            println!("{}:{}: {:?}", file!(), line!(), (t, run(t, t, &fixed_gates) == t + t));
        }
        0
    };
    println!("{}", part2_answer);
}

fn eval(arg: &String, gates: &HashMap<String, Gate>, wires: &HashMap<String, i64>, seen: &mut HashSet<String>) -> i64 {
    // Terminating on loops - instead of tracking visited keys check if depth has exceeded the number of nodes 
    if seen.contains(arg) {
        return -1;
    }

    if let Some(v) = wires.get(arg) {
        return *v;
    }

    if let Some(gate) = gates.get(arg) {
        seen.insert(arg.to_string());
        let l = eval(&gate.left, gates, wires, seen);
        let r = eval(&gate.right, gates, wires, seen);
        seen.remove(&arg.to_string());
        if l == -1 || r == -1 {
            return -1;
        }
        return match gate.op.as_str() {
            "AND" => l & r,
            "OR" => l | r,
            "XOR" => l ^ r,
            _ => panic!("Op {} not found", gate.op)
        }
    }
    
    panic!("Arg {} not found", arg)
}

fn child_node_set(arg: &String, gates: &HashMap<String, Gate>, wires: &HashMap<String, i64>, seen: &mut HashSet<String>, child_set: &mut HashSet<String>) {
    // Terminating on loops - instead of tracking visited keys check if depth has exceeded the number of nodes 
    if seen.contains(arg) {
        return;
    }

    if let Some(v) = wires.get(arg) {
        return;
    }

    if let Some(gate) = gates.get(arg) {
        // Only consider gates
        // TODO good enough ??
        child_set.insert(arg.to_string());

        seen.insert(arg.to_string());
        let l = child_node_set(&gate.left, gates, wires, seen, child_set);
        let r = child_node_set(&gate.right, gates, wires, seen, child_set);
        seen.remove(&arg.to_string());
        return;
    }
    
    panic!("Arg {} not found", arg)
}

fn run(x: Int, y: Int, gates: &HashMap<String, Gate>) -> i64 {
    let mut mx = x;
    let mut my = y;

    let mut wires: HashMap<String, Int> = HashMap::new();
    for pos in 0..64 {
        let key_suffix = (if pos < 10 {"0"} else {""}).to_string() + &pos.to_string();
        wires.insert("x".to_string() + &key_suffix, mx % 2);
        wires.insert("y".to_string() + &key_suffix, my % 2);
        mx = mx >> 1;
        my = my >> 1;
    }

    let mut ans = 0;
    for i in 0..64 {
        let pos = 63 - i;
        let key = "z".to_string() + (if pos < 10 {"0"} else {""}) + &pos.to_string();
        if gates.contains_key(&key) {
            let res = eval(&key, &gates, &wires, &mut HashSet::new());
            if res == -1 {
                return -1;
            }
            ans = ans << 1 | res;
        }
    }
    ans
}

fn swap_keys(gates: &mut HashMap<String, Gate>, key1: &String, key2: &String) {
    if let (Some(gate1), Some(gate2)) = (gates.remove(key1), gates.remove(key2)) {
        gates.insert(key1.clone(), gate2);
        gates.insert(key2.clone(), gate1);
    }
}

fn inspect(indent: &String, arg: &String, gates: &HashMap<String, Gate>) {
    if let Some(gate) = gates.get(arg) {
        println!("{} {} = {} {} {}", indent, arg, gate.left, gate.op, gate.right);
        inspect(&(indent.clone() + "  "), &gate.left, gates);
        inspect(&(indent.clone() + "  "), &gate.right, gates);
    }
}