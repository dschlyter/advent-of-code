import { readLines } from './util'
import * as _ from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day5.txt")

  await part1(input)
  await part2(input)
}

async function part1(input: string[]): Promise<void> {
  let row = 0
  while (input[row+1] != "") {
    row++
  }
  let col = 1
  let stacks: Array<Array<string>> = []
  while (col < input[row].length) {
    let s: Array<string> = []
    stacks.push(s)
    for (let r=row-1; r>=0; r--) {
      if (input[r][col] && input[r][col] != ' ') {
        s.push(input[r][col])
      }
    }
    col += 4
  }

  row += 2
  while (row < input.length) {
    let line = input[row]
    let [move, amount, from, f, to, t] = line.split(" ")
    row += 1
    for (let i=0; i<parseInt(amount); i++) {
      let crate = stacks[parseInt(f)-1].pop()!
      stacks[parseInt(t)-1].push(crate)
    }
  }

  let msg = ""
  for (let s of stacks) {
    msg += s[s.length-1]
  }
  console.log(msg)
}

async function part2(input: string[]): Promise<void> {
  let row = 0
  while (input[row+1] != "") {
    row++
  }
  let col = 1
  let stacks: Array<Array<string>> = []
  while (col < input[row].length) {
    let s: Array<string> = []
    stacks.push(s)
    for (let r=row-1; r>=0; r--) {
      if (input[r][col] && input[r][col] != ' ') {
        s.push(input[r][col])
      }
    }
    col += 4
  }

  row += 2
  while (row < input.length) {
    let line = input[row]
    let [move, amount, from, f, to, t] = line.split(" ")
    row += 1
    let moved: Array<string> = []
    for (let i=0; i<parseInt(amount); i++) {
      moved.push(stacks[parseInt(f)-1].pop()!)
    }
    for (let i=0; i<parseInt(amount); i++) {
      stacks[parseInt(t)-1].push(moved.pop()!)
    }
  }

  let msg = ""
  for (let s of stacks) {
    msg += s[s.length-1]
  }
  console.log(msg)
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null