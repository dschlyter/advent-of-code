import { readLines } from './util'
import * as _ from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day6.txt")

  await part1(input, 4)
  await part1(input, 14)
}

async function part1(input: string[], len: number): Promise<void> {
  for (let row of input) {
    for (let i=0; i<row.length-len+1; i++) {
      let s = row.substring(i, i+len)
      let ss = new Set()
      for (let c of s) {
        ss.add(c)
      }
      if (ss.size == len) {
        console.log(i+len)
        break
      }
    }
  }
}

async function part2(input: string[]): Promise<void> {

}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null