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


    // First attempt, scan through entire list each time - slow 10 seconds
    // Second attempt, maintain a list of free space - 1 second
    let part2_answer = {
        let mut disk1 = disk.clone();

        let mut free_list: Vec<(usize, usize)> = Vec::new();

        let mut start_pos = 0;
        let mut gap_size = 0;
        for (pos, file) in disk1.iter().enumerate() {
            if *file == -1 {
                if gap_size == 0 {
                    start_pos = pos;
                }
                gap_size += 1;
            } else {
                if gap_size > 0 {
                    free_list.push((start_pos, gap_size));
                }
                gap_size = 0;
            }

        }

        let mut file = (disk_map.len() / 2) as i32;
        i = disk1.len() - 1;
        while file > 0 {
            // Find the target file and size
            while disk1[i] != file {
                i -= 1;
            }
            let mut file_size = 0;
            while disk1[i] == file {
                i -= 1;
                file_size += 1;
            }
            let file_start = i + 1;

            for gap in free_list.iter_mut() {
                if gap.1 >= file_size && gap.0 < file_start {
                    for offset in 0..file_size {
                        disk1[gap.0 + offset] = file;
                        disk1[file_start + offset] = -1;
                    }
                    gap.0 += file_size;
                    gap.1 -= file_size;
                    break
                }
            }

            file -= 1;
        }

        // println!("{}:{}: {:?}", file!(), line!(), (disk1));

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