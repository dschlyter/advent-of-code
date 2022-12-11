import { readLines } from './util'
import * as _ from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day1.txt")

  await part1(input)
  await part2(input)
}

async function part1(input: string[]): Promise<void> {
  let s = 0, m = 0
  for (const line of input) {
    if (line == "") {
      s = 0
    } else {
      s += parseInt(line)
      m = Math.max(m, s);
    }
  }

  console.log(m)
}

async function part2(input: string[]): Promise<void> {
  let s = [0]
  for (const line of input) {
    if (line == "") {
      s.push(0)
    } else {
      s[s.length-1] += parseInt(line)
    }
  }

  s = s.sort((a, b) => a - b).reverse()

  console.log(s[0] + s[1] + s[2])
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null