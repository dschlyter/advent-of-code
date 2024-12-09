#![allow(unused_imports)]

use std::collections::HashMap;
use std::collections::HashSet;
use aoc2024::util::grid_get;
use regex::Regex;

use aoc2024::util::{self, run_for_inputs};

pub fn main() {
    run_for_inputs(file!(), solve);
}

pub fn solve(filename: String) {
    // Input

    let lines = util::read_lines(&filename);
    let disk_map: Vec<String> = lines[0].chars().map(|c| c.to_string()).collect();

    // Calc disk - shared between steps
    let mut disk : Vec<i32> = Vec::new();
    let mut i: usize = 0;
    loop {
        // File with index
        for j in 0..(disk_map[i*2].parse::<i32>().unwrap()) {
            disk.push(i as i32);
        }
        if i*2+1 >= disk_map.len() {
            break;
        }
        // Free space on disk
        for j in 0..(disk_map[i*2+1].parse::<i32>().unwrap()) {
            disk.push(-1);
        }

        i += 1;
    }
    // println!("{}:{}: {:?}", file!(), line!(), (disk));
    
    let part1_answer = {
        let mut disk1 = disk.clone();
        let mut head = 0;
        let mut tail = disk1.len() - 1;

        while head < tail {
            while disk1[head] != -1 && head < disk1.len()-1 {
                head += 1;
            }
            while disk1[tail] == -1 && tail > 0 {
                tail -= 1;
            }
            if (head < tail) {
                disk1[head] = disk1[tail];
                disk1[tail] = -1;
            }
        }

        let mut checksum: i64 = 0;
        for (i, file) in disk1.iter().enumerate() {
            if *file >= 0 {
                checksum += (i as i64) * (*file as i64);
            }
        }

        checksum
    };
    println!("{}", part1_answer);


    // Slow 10 seconds
    // More opt would be to maintain a list of free space of course
    let part2_answer = {
        let mut disk1 = disk.clone();

        let mut file = (disk_map.len() / 2) as i32;
        i = disk1.len() - 1;
        while file > 0 {
            println!("{}:{}: {:?}", file!(), line!(), (file));

            // Find the target file and size
            while disk1[i] != file {
                i -= 1;
            }
            let mut file_size = 0;
            while disk1[i] == file {
                i -= 1;
                file_size += 1;
            }

            // Find the best gap O(n^2)
            let file_start = i + 1;
            let mut j = i;
            let mut gap_size = 0;
            let mut best_pos = file_start;
            while j > 0 {
                if disk1[j] == -1 {
                    gap_size += 1;
                } else {
                    gap_size = 0;
                }
                if gap_size >= file_size {
                    best_pos = j;
                }
                j -= 1;
            }

            // Move file if a gap exists
            if best_pos < file_start {
                // println!("{}:{}: {:?}", file!(), line!(), ("move to", best_pos));
                for k in 0..file_size {
                    disk1[best_pos+k] = file;
                    disk1[file_start+k] = -1;
                }
            }

            file -= 1;
        }

        println!("{}:{}: {:?}", file!(), line!(), (disk1));

        let mut checksum: i64 = 0;
        for (i, file) in disk1.iter().enumerate() {
            if *file >= 0 {
                checksum += (i as i64) * (*file as i64);
            }
        }

        checksum
    };
    println!("{}", part2_answer);
}