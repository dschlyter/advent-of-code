// #![allow(unused_imports)]

#[allow(unused_imports)]
use std::collections::HashMap;
#[allow(unused_imports)]
use std::collections::HashSet;
#[allow(unused_imports)]
use regex::Regex;

use aoc2024::util::{self, run_for_inputs};

pub fn main() {
    run_for_inputs(file!(), solve);
}

pub fn solve(filename: String) {
    // Input

    let lines = util::read_lines(&filename);
    let s = lines.split(|s| s == "").collect::<Vec<_>>();

    let page_ordering = s[0];
    let update = s[1];
    let mut ordering: HashMap<String, HashSet<String>> = HashMap::new();

    for order in page_ordering {
        let parts: Vec<&str> = order.split('|').collect();
        let before = parts[0];
        let after = parts[1];

        ordering.entry(before.to_string())
            .or_insert_with(HashSet::new)
            .insert(after.to_string());
    }

    // Part 1

    {
        let mut res = 0;

        for line in update {
            let pages: Vec<&str> = line.split(",").collect();
            let mut pages_before: HashSet<String> = HashSet::new();
            let mut valid = true;

            for p in pages.iter() {
                let default_set = HashSet::default();
                let needs_to_be_after = ordering.get(*p).unwrap_or(&default_set);
                if !pages_before.is_disjoint(&needs_to_be_after) {
                    valid = false;
                    break;
                }
                pages_before.insert(p.to_string());
            }

            if valid {
                if pages.len() % 2 == 0 {
                    panic!("Even number of pages")
                }
                let page = pages[pages.len() / 2].parse::<i32>().unwrap();
                res += page;
            }
        }

        println!("{}", res);
    }


    // Part 2
    {
        let mut res = 0;

        for line in update {
            let mut pages: Vec<&str> = line.split(",").collect();
            let mut pages_before: HashSet<&str> = HashSet::new();
            let mut valid = true;

            for i in 0..pages.len() {
                let p = pages[i];
                let default_set = HashSet::default();
                let needs_to_be_after: HashSet<&str> = ordering.get(p).unwrap_or(&default_set).iter().map(|s| s.as_str()).collect();

                if !pages_before.is_disjoint(&needs_to_be_after) {
                    valid = false;

                    let mut j = i;
                    let mut before_set = pages_before.clone();

                    while !before_set.is_disjoint(&needs_to_be_after) {
                        let prev_p = pages[j-1];
                        before_set.remove(prev_p);
                        pages.swap(j-1, j);
                        j -= 1;
                    }
                }

                pages_before.insert(p);
            }

            if !valid {
                let page = pages[pages.len() / 2].parse::<i32>().unwrap();
                res += page;
            }
        }

        println!("{}", res);
    }
}