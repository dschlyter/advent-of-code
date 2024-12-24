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

// type Int = i64;

pub fn solve(filename: String) {
    let lines = util::read_lines(&filename);
    let mut neigh: HashMap<String, Vec<String>> = HashMap::new();
    let mut neigh2: HashSet<(String, String)> = HashSet::new();
    for s in lines {
        let parts: Vec<&str> = s.split("-").collect();
        let [a, b] = &parts[..] else { panic!("Expected exactly two parts"); };
        let ass = a.to_string();
        let bs = b.to_string();

        neigh.entry(ass.clone()).or_insert(Vec::new()).push(bs.clone());
        neigh.entry(bs.clone()).or_insert(Vec::new()).push(ass.clone());

        neigh2.insert((bs.clone(), ass.clone()));
        neigh2.insert((ass, bs));
    };

    // Part 1
    let part1_answer = {
        let mut ans = 0;
        for k1 in neigh.keys() {
            for k2 in &neigh[k1] {
                if k1 >= k2 {
                    continue;
                }
                for k3 in &neigh[k1] {
                    if k2 >= k3 {
                        continue;
                    }
                    if !k1.starts_with("t") && !k2.starts_with("t") && !k3.starts_with("t") {
                        continue;
                    }
                    if !neigh[k2].contains(k3) {
                        continue;
                    }
                    ans += 1;
                }
            }
        }
        ans
    };
    println!("{}", part1_answer);

    // Part 2
    // NP complete!
    // but lets assume there is a naive solution
    let part2_answer = {
        let mut best = HashSet::new();
        let keys: HashSet<String> = neigh.keys().cloned().collect();

        for start_candidate in keys.iter() {
            let mut clique = HashSet::new();
            clique.insert(start_candidate.to_string());
            let grow = enlarge_clique(clique, &keys, &neigh2);
            if grow.len() > best.len() {
                best = grow;
            }
            // println!("Best candidate has size: {}", best.len());
        }

        let mut vec_best: Vec<String> = best.into_iter().collect();
        vec_best.sort();
        vec_best.join(",")
    };
    println!("{}", part2_answer);
}

fn enlarge_clique(clique: HashSet<String>, keys: &HashSet<String>, neigh: &HashSet<(String, String)>) -> HashSet<String> {
    let mut ret = clique;
    'cand: for candidate in keys {
        for existing in ret.iter() {
            // Nasty copying here... :(
            if !neigh.contains(&(candidate.clone(), existing.clone())) {
                continue 'cand;
            }
        }
        ret.insert(candidate.clone());
    }
    ret
}