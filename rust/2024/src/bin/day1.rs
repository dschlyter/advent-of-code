// #![allow(unused_imports)]

use std::collections::HashMap;

use aoc2024::util;

pub fn main() {
    let lines = util::read_lines(&util::input_for(file!()));

    // Part 1 fail - the problem was not as complex as I thought

    /* 
    Wrong idea - not a position comparison
    type NumWithLine = (u32, usize);
    let (mut list1, mut list2): (Vec<NumWithLine>, Vec<NumWithLine>) = lines.iter().enumerate().map(|(index, line)| {
        let nums: Vec<u32> = line.split_whitespace().map(|s| s.parse::<u32>().unwrap()).collect();
        let r = ((nums[0], index), (nums[1], index));
        r
    }).unzip();
    */

    // Input

    let (list1, list2): (Vec<i32>, Vec<i32>) = lines.iter().map(|line| {
        let nums: Vec<i32> = line.split_whitespace().map(|s| s.parse::<i32>().unwrap()).collect();
        let r = (nums[0], nums[1]);
        r
    }).unzip();

    // Part 1

    let mut list1s = list1.clone();
    let mut list2s = list2.clone();

    list1s.sort();
    list2s.sort();

    let mut res = 0;
    for (a, b) in list1s.iter().zip(list2s.iter()) {
        let diff = (a - b).abs();
        // dbg!(diff);
        res += diff;
    }

    println!("{}", res);

    // Part 2

    let mut map2: HashMap<&i32, i32> = HashMap::new();
    list2.iter().for_each(|n| util::count(&mut map2, n));

    let mut res2 = 0;
    for n in list1.iter() {
        let right = map2.get(&n).unwrap_or(&0);
        let similarity = n * right;
        // dbg!(n, right, similarity);
        res2 += similarity
    }

    println!("{}", res2);
}