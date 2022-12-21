import { readLines, ints } from './util'
import * as _ from 'lodash';
import { times } from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day21.txt")

  await part1(input)
  await part2(input)
}

type BaseMonkey = {
  number: number
}

type BiMonkey = {
  dep1: string,
  dep2: string,
  op: string
}

async function part1(input: string[]): Promise<void> {
  let monkeys: {[a: string]: BaseMonkey | BiMonkey} = {}
  for (let row of input) {
    let s = row.split(":")
    let s1 = s[0]
    let s2 = s[1].trim()
    if (s2.includes(" ")) {
      let [dep1, op, dep2] = s2.split(" ")
      monkeys[s1] = {dep1, op, dep2}
    } else {
      monkeys[s1] = {number: parseInt(s2)}
    }
  }

  function find(name: string) {
    let m = monkeys[name]
    if ("number" in m) {
      return m.number
    } else {
      let r1 = find(m.dep1)
      let r2 = find(m.dep2)
      switch (m.op) {
        case '+': return r1 + r2
        case '-': return r1 - r2
        case '*': return r1 * r2
        case '/': return r1 / r2
      }
    }
  }
  console.log(find("root"))
}

async function part2(input: string[]): Promise<void> {
  let monkeys: {[a: string]: BaseMonkey | BiMonkey} = {}
  for (let row of input) {
    let s = row.split(":")
    let s1 = s[0]
    let s2 = s[1].trim()
    if (s2.includes(" ")) {
      let [dep1, op, dep2] = s2.split(" ")
      monkeys[s1] = {dep1, op, dep2}
    } else {
      monkeys[s1] = {number: parseInt(s2)}
    }
  }

  function find(name: string) {
    let m = monkeys[name]
    if ("number" in m) {
      return m.number
    } else {
      let r1 = find(m.dep1)
      let r2 = find(m.dep2)
      switch (m.op) {
        case '+': return r1 + r2
        case '-': return r1 - r2
        case '*': return r1 * r2
        case '/': return r1 / r2
      }
    }
  }

  function diff(n: number) {
    let root = monkeys['root']
    if (!("dep1" in root)) {
      throw Error("Invalid root")
    }

    monkeys['humn'] = {number: n}
    let r1 = find(root.dep1)
    let r2 = find(root.dep2)
    return r1 - r2
  }

  function norm(n: number) {
    return n / Math.abs(n)
  }

  // Do a hacky binary search for the number
  let start = 1, end = 1_000_000_000_000_000
  if (norm(diff(start)) == norm(diff(end))) {
    throw Error("End is too small")
  }
  while (true) {
    let check = (start + end) / 2
    let newDiff = diff(check)
    if (newDiff == 0) {
      console.log(check)
      break
    }

    let n1 = norm(newDiff)
    let sn = norm(diff(start))
    let en = norm(diff(end))
    if (sn == n1) {
      start = check
    } else if (en == n1) {
      end = check
    } else {
      throw Error("Failed to binary search")
    }
  }
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null