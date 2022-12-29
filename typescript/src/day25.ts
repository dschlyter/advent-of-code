import { readLines, ints } from './util'
import * as _ from 'lodash';
import { isInteger, isNumber, times } from 'lodash';

// Note: Run this with --stack-size=1000000 for the DFS

const inputFile = "input/day25.txt"

async function main(): Promise<void> {
  const input = await readLines(inputFile)

  await part1(input)
  await part2(input)
}

let mapping = {
  "2": 2,
  "1": 1,
  "0": 0,
  "-": -1,
  "=": -2,
}
let rMapping = _.invert(mapping)

function from_snafu(s: string) {
  let n = 0
  let base = 1
  for (let i=0; i<s.length; i++) {
    let c = s[s.length-1-i]
    n += mapping[c] * base
    base *= 5
  }
  return n
}

function to_snafu(n: number) {
  let s = ""
  while (n > 0) {
    let m = n % 5
    if (m > 2) {
      m = m - 5
    }
    s = rMapping[m] + s
    n = (n - m)/ 5
  }
  return s || "0"
}

async function part1(input: string[]): Promise<void> {
  let sum = 0
  for (let line of input) {
    sum += from_snafu(line)
  }
  console.log(to_snafu(sum))
}

async function part2(input: string[]): Promise<void> {

}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null