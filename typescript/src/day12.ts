import { readLines } from './util'
import * as _ from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day12.txt")

  await part1(input, 'S')
  await part1(input, 'a')
}

function elevation(c: string) {
  if (c == "S") {
    return elevation('a')
  }
  if (c == "E") {
    return elevation('z')
  }
  return c.charCodeAt(0) - "a".charCodeAt(0)
}

async function part1(input: string[], startPos: string): Promise<void> {
  let visited: {[key: string]: boolean} = {}
  let q: Array<[number, number, number]> = []

  for (let row=0; row<input.length; row++) {
    for (let col=0; col<input[row].length; col++) {
      if (input[row][col] == startPos) {
        q.push([row, col, 0])
      }
    }
  }

  while (q.length > 0) {
    // Inefficient!
    let p = q.shift() || [0,0,0]

    let key = p.slice(0, 2)
    if (visited[key.toString()]) {
      continue
    }
    visited[key.toString()] = true

    let [row, col, steps] = p
    if (input[row][col] == 'E') {
      console.log(steps)
      break
    }

    let near = [[row, col-1], [row, col+1], [row-1, col], [row+1, col]]

    for (let [row2, col2] of near) {
      if(row2 < 0 || row2 >= input.length || col2 < 0 || col2 >= input[row2].length) {
        continue
      }
      if (elevation(input[row2][col2]) - elevation(input[row][col]) > 1) {
        continue
      }
      q.push([row2, col2, steps+1])
    }
  }
}

async function part2(input: string[]): Promise<void> {
  // Reuse code from part1 with a param
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null