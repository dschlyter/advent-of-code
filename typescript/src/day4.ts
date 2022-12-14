import { readLines } from './util'
import * as _ from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day4.txt")

  await part1(input)
  await part2(input)
}

async function part1(input: string[]): Promise<void> {
  let c = 0
  for (let line of input) {
    let [p1, p2] = line.split(",")
    let [s1, e1] = p1.split("-").map(x => parseInt(x))
    let [s2, e2] = p2.split("-").map(x => parseInt(x))
    if (s1 <= s2 && e2 <= e1 || s2 <= s1 && e1 <= e2) {
      c += 1
    }
  }
  console.log(c)
}

async function part2(input: string[]): Promise<void> {
  let c = 0
  for (let line of input) {
    let [p1, p2] = line.split(",")
    let [s1, e1] = p1.split("-").map(x => parseInt(x))
    let [s2, e2] = p2.split("-").map(x => parseInt(x))
    if (s1 <= e2 && s2 <= e1) {
      c += 1
    }
  }
  console.log(c)

}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null