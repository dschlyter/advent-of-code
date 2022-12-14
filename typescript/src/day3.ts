import { readLines } from './util'
import * as _ from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day3.txt")

  await part1(input)
  await part2(input)
}

async function part1(input: string[]): Promise<void> {
  let sum = 0
  for (let line of input) {
    let p1 = line.substring(0, line.length/2)
    let p2 = line.substring(line.length/2, line.length)

    let items = toSet(p1);
    let dup: string = ""
    for (let c of p2) {
      if (items[c]) {
        dup = c
      }
    }
    let prio: number = getPrio(dup);
    sum += prio
  }
  console.log(sum)
}

function toSet(p1: string): {[a: string]: boolean} {
  let items = {}
  for (let c of p1) {
    items[c] = true;
  }
  return items
}

function getPrio(dup: string): number {
  let prio = dup.charCodeAt(0) - 'a'.charCodeAt(0) + 1;
  if (prio <= 0) {
    prio = dup.charCodeAt(0) - 'A'.charCodeAt(0) + 27;
  }
  return prio;
}

async function part2(input: string[]): Promise<void> {
  let sum = 0
  let i = 0
  while (i < input.length) {
    let e1 = toSet(input[i])
    let e2 = toSet(input[i+1])
    let e3 = input[i+2]
    for (let c of e3) {
      if (e1[c] && e2[c]) {
        sum += getPrio(c)
        break
      }
    }
    i += 3
  }
  console.log(sum)
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null