import { readLines } from './util'
import * as _ from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day13.txt")

  await part1(input)
  await part2(input)
}

function compare(p1, p2) {
  let a1 = Array.isArray(p1)
  let a2 = Array.isArray(p2)

  if (!a1 && !a2) {
    return p1 - p2
  } else if (a1 && a2) {
    let i = 0
    while (true) {
      if (i >= p1.length && i >= p2.length) {
        return 0
      } else if (i >= p1.length) {
        return -1
      } else if (i >= p2.length) {
        return 1
      }

      let c = compare(p1[i], p2[i])
      if (c != 0) {
        return c;
      }

      i += 1
    } 
  } else if (a1) {
    return compare(p1, [p2])
  } else if (a2) {
    return compare([p1], p2)
  }
}

async function part1(input: string[]): Promise<void> {
  let s = 0

  let i = 0
  let ii = 0
  while (i < input.length) {
    let p1 = JSON.parse(input[i])
    i += 1
    let p2 = JSON.parse(input[i])
    i += 2

    let c = compare(p1, p2)
    if (c == 0) {
      console.log("WARNING! Equal", p1, p2)
    }
    if (c < 0) {
      s += (ii+1)
    }
    ii += 1
  }
  console.log(s)
}

async function part2(input: string[]): Promise<void> {
  let s = 0

  // TODO what is the proper type for this one ??
  let packets: Array<any> = []
  let i = 0
  while (i < input.length) {
    let p1 = JSON.parse(input[i])
    i += 1
    packets.push(p1)
    let p2 = JSON.parse(input[i])
    i += 2
    packets.push(p2)
  }
  packets.push([[2]])
  packets.push([[6]])
  packets.sort((a,b) => compare(a,b))

  let i1 = -1, i2 = -1
  for (let i=0; i<packets.length; i++) {
    if (compare(packets[i], [[2]]) == 0) {
      i1 = i + 1
    }
    if (compare(packets[i], [[6]]) == 0) {
      i2 = i + 1
    }
  }
  console.log(i1 * i2)
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null