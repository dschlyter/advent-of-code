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
const MAX_GATE: usize = 45;

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
                ans = ans << 1 | eval(&key, &gates, &wires, &mut HashMap::new());
            }
        }
        ans
    };
    println!("{}", part1_answer);

    // Part 2
    // 2s runtime after some opts - 30 seconds runtime originally
    // Very messy and iterative process :(
    let part2_answer = {
        if filename.contains("_test") {
            println!("{}:{}: {:?}", file!(), line!(), ("Skipping part 2 - not defined for tests"));
            return;
        }
        inspect_to(5, &gates);

        // Idea:
        // Start testing the lowest gates with low numbers and fix them, then build up towards a full solution
        // Assuming that every "bad bit" in a addition can be fixed with a single swap

        let mut ans = Vec::new();
        let gate_labels: HashSet<String> = gates.keys().cloned().collect();
        let mut gates2 = gates.clone();

        'testing: for offset in 0..(MAX_GATE+1) {
            if test_n(offset, &gates2) {
                println!("{} - all good!", offset);
                continue;
            }

            // Opt - one of the swapped nodes must be a child of the bad output gate - makes no sense to swap two unrelated gates
            // Also assume that all gates involved with previous bits are good and valid
            let mut bad_gate_children = HashSet::new();
            child_node_set(z_labels().iter().nth(offset).unwrap(), &gates, &mut bad_gate_children);
            if offset > 0 {
                let mut good_gate_children = HashSet::new();
                child_node_set(z_labels().iter().nth(offset - 1).unwrap(), &gates, &mut good_gate_children);
                bad_gate_children.retain(|c| !good_gate_children.contains(c));
            }
            println!("{} swapping... with {} bad gates", offset, bad_gate_children.len());

            for key1 in &gate_labels {
                // println!(" > {}", key1);
                for key2 in &gate_labels {
                    if key1 >= key2 {
                        continue;
                    }
                    if !bad_gate_children.contains(key1) && !bad_gate_children.contains(key2) {
                        continue;
                    }

                    swap_keys(&mut gates2, key1, key2);
                    // Check some basic invariants on the swap
                    // This is both an opt for time, and also rules out bad swaps that might accidentally work for lower numbers
                    if check_validity(offset, &gates2) {
                        let mut valid = true;
                        for recheck in 0..(offset+1) {
                            if !test_n(offset - recheck, &gates2) {
                                valid = false;
                                break;
                            }
                        }
                        if valid {
                            println!("Found swap for {} and {}", &key1, &key2);
                            ans.push(key1.clone());
                            ans.push(key2.clone());
                            continue 'testing;
                        }
                    }
                    swap_keys(&mut gates2, key1, key2);
                }
            }

            panic!("No good swaps found!")
        }

        ans.sort();
        ans.join(",")
    };
    println!("{}", part2_answer);
}

fn test_n(offset: usize, gates: &HashMap<String, Gate>) -> bool {
    let test = 1 << offset;
    let t1 = test - 1;
    let t2 = 1;
    return run(t1, t2, &gates) == t1 + t2;
}

fn z_labels() -> Vec<String> {
    let mut ret = Vec::new();
    for i in 0..(MAX_GATE+1) {
        ret.push("z".to_string() + (if i < 10 {"0"} else {""}) + &i.to_string());
    }
    ret
}

fn check_validity(offset: usize, gates: &HashMap<String, Gate>) -> bool {
    let zl: Vec<_> = z_labels().into_iter().take(offset+1).collect();

    // Invariant 1 - every top-level is an XOR
    for l in &zl {
        if gates[l].op != "XOR" {
            return false;
        }
    }

    // Invariant 2 - z(N) should not depend on z(N+1)
    for (i, l) in zl.iter().enumerate() {
        let m = max_input(l, &gates, &mut HashSet::new());
        if m > i as i64 {
            return false;
        }
    }

    true
}

fn eval(wire: &String, gates: &HashMap<String, Gate>, inputs: &HashMap<String, i64>, mem: &mut HashMap<String, i64>) -> i64 {
    // Terminating on loops - instead of tracking visited keys check if depth has exceeded the number of nodes 
    if let Some(x) = mem.get(wire) {
        return *x;
    }

    if let Some(v) = inputs.get(wire) {
        return *v;
    }

    if let Some(gate) = gates.get(wire) {
        // If we enounter this wire again before resolving it, then we have a loop and return -1
        mem.insert(wire.to_string(), -1);

        let l = eval(&gate.left, gates, inputs, mem);
        let r = eval(&gate.right, gates, inputs, mem);
        if l == -1 || r == -1 {
            return -1;
        }
        let res = match gate.op.as_str() {
            "AND" => l & r,
            "OR" => l | r,
            "XOR" => l ^ r,
            _ => panic!("Op {} not found", gate.op)
        };

        mem.insert(wire.to_string(), res);
        return res;
    }
    
    panic!("Arg {} not found", wire)
}

fn max_input(wire: &String, gates: &HashMap<String, Gate>, seen: &mut HashSet<String>) -> i64 {
    // Loop detected
    if seen.contains(wire) {
        return 9000;
    }

    if wire.starts_with("x") || wire.starts_with("y") {
        return wire[1..].parse::<i64>().unwrap();
    }

    if let Some(gate) = gates.get(wire) {
        seen.insert(wire.to_string());
        let l = max_input(&gate.left, gates, seen);
        let r = max_input(&gate.right, gates, seen);
        seen.remove(wire);
        return l.max(r);
    }
    -1
}

fn child_node_set(wire: &String, gates: &HashMap<String, Gate>, children: &mut HashSet<String>) {
    if let Some(gate) = gates.get(wire) {
        children.insert(wire.to_string());
        child_node_set(&gate.left, gates, children);
        child_node_set(&gate.right, gates, children);
    }
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
            let res = eval(&key, &gates, &wires, &mut HashMap::new());
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

fn inspect_to(offset: usize, gates: &HashMap<String, Gate>) {
    for pos in 0..(offset+1) {
        let key = "z".to_string() + (if pos < 10 {"0"} else {""}) + &pos.to_string();
        if gates.contains_key(&key) {
            inspect(&"".to_string(), &key, &gates);
        }
    }
}

fn inspect(indent: &String, arg: &String, gates: &HashMap<String, Gate>) {
    if let Some(gate) = gates.get(arg) {
        println!("{} {} = {} {} {}", indent, arg, gate.left, gate.op, gate.right);
        inspect(&(indent.clone() + "  "), &gate.left, gates);
        inspect(&(indent.clone() + "  "), &gate.right, gates);
    }
}