import { readLines, ints } from './util'
import * as _ from 'lodash';
import { times } from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day22.txt")

  await part1(input)
  await part2(input)
}

const facing = [[0, 1], [1, 0], [0, -1], [-1, 0]]

async function part1(input: string[]): Promise<void> {
  let map = input.slice(0, input.length-2)
  let pathInput = input[input.length-1]

  // Patch up undefined segments - booo bad puzzle design!
  let width = map.map(row => row.length).reduce((a,b) => Math.max(a, b))
  for (let i=0; i<map.length; i++) {
    if (map[i].length < width) {
      map[i] = map[i].padEnd(width, " ")
    }
  }

  let path = pathInput.replace(/R/g, " R ").replace(/L/g, " L ").split(" ").filter(s => s != " ")
  let dir = 0
  let y = 0, x = 0
  while (map[y][x] == " ") {
    x += 1
  }

  for (let p of path) {
    if (p == "R") {
      dir = (dir + 1) % 4
      console.log("R")
    } else if (p == "L") {
      dir = (dir + 3) % 4
      console.log("L")
    } else {
      let move = parseInt(p)
      while (move > 0) {
        let ny = y, nx = x

        do {
          ny = (ny + facing[dir][0] + map.length) % map.length
          nx = (nx + facing[dir][1] + width) % map[y].length
        } while (map[ny][nx] == " ")

        if (map[ny][nx] == "#") {
          break
        }
        y = ny
        x = nx
        move -= 1
        if (map[ny][nx] != ".") {
          throw Error("Should always land on a . but was " + map[ny][nx])
        }
      }
      console.log(x, y, dir)
    }
  }
  console.log(x, y, dir)
  let password = 1000 * (y+1) + 4 * (x+1) + dir
  console.log(password)
}

async function part2(input: string[]): Promise<void> {

}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null