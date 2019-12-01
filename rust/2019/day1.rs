use std::io::{self, BufRead};

fn main() {
    let stdin = io::stdin();

    let mut sum = 0;
    let mut sum2 = 0;

    for line in stdin.lock().lines() {
        let mass = line.unwrap().parse::<i32>().unwrap();
        let fuel = mass / 3 - 2;

        sum += fuel;

        sum2 += fuel;
        let mut fuelfuel = fuel;

        // Hacky do-while - bad practice
        loop {
            fuelfuel = fuelfuel / 3 - 2;
            if fuelfuel <= 0 { break; }
            sum2 += fuelfuel;

        }
    }

    println!("Part 1 {}", sum);
    println!("Part 2 {}", sum2);
}