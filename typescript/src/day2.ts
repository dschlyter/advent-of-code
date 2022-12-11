import { readLines } from './util'
import * as _ from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day2.txt")

  await part1(input)
  await part2(input)
}

async function part1(input: string[]): Promise<void> {
  let s = 0
  for (const line of input) {
    let [m, e] = line.split(" ")

    let shape = {
      'X': 1,
      'Y': 2,
      'Z': 3
    }[e]

    let win = {
      'A X': 3,
      'A Y': 6,
      'A Z': 0,
      'B X': 0,
      'B Y': 3,
      'B Z': 6,
      'C X': 6,
      'C Y': 0,
      'C Z': 3,
    }[line]

    let score = shape + win
    s += score
  }
  console.log(s)
}

async function part2(input: string[]): Promise<void> {
  let s = 0
  for (const line of input) {
    let [m, e] = line.split(" ")

    let base = {
      'A': 0,
      'B': 1,
      'C': 2
    }[m]

    let offset = {
      'X': 2,
      'Y': 0,
      'Z': 1
    }[e]

    let win = {
      'X': 0,
      'Y': 3,
      'Z': 6
    }[e]

    let shape = 1 + (base + offset) % 3

    let score = shape + win
    s += score
  }
  console.log(s)
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null