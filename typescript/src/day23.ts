import { readLines, ints } from './util'
import * as _ from 'lodash';
import { times } from 'lodash';

// Note: Run this with --stack-size=1000000 for the DFS

const inputFile = "input/day23.txt"

type Point = [number, number]
type Move = [Point, Point]

async function main(): Promise<void> {
  const input = await readLines(inputFile)

  await part1(input)
  await part2(input)
}

const moveCones: Point[][] = [
  [[-1, -1], [-1, 0], [-1, 1]],
  [[1, -1], [1, 0], [1, 1]],
  [[-1, -1], [0, -1], [1, -1]],
  [[-1, 1], [0, 1], [1, 1]],
]

function key(y: number, x: number) {
  return `${y},${x}`
}

function show(elves) {
  let y1 = 1e9, y2 = -1e9
  let x1 = 1e9, x2 = -1e9
  showWithBounds(elves, y1, y2, x1, x2)
}

function showWithBounds(elves, y1, y2, x1, x2) {
  for (let k of Object.keys(elves)) {
    let [y, x] = k.split(",").map(s => parseInt(s))
    y1 = Math.min(y1, y)
    y2 = Math.max(y2, y)
    x1 = Math.min(x1, x)
    x2 = Math.max(x2, x)
  }
  for (let y=y1; y<=y2; y++) {
    let s = ""
    for (let x=x1; x<=x2; x++) {
      if (elves[key(y,x)]) {
        s += "#"
      } else {
        s += "."
      }
    }
    console.log(s)
  }
  console.log()
}

function land(elves) {
  let y1 = 1e9, y2 = -1e9
  let x1 = 1e9, x2 = -1e9
  for (let k of Object.keys(elves)) {
    let [y, x] = k.split(",").map(s => parseInt(s))
    y1 = Math.min(y1, y)
    y2 = Math.max(y2, y)
    x1 = Math.min(x1, x)
    x2 = Math.max(x2, x)
  }
  console.log(y1, y2, x1, x2)
  return (1+y2-y1) * (1+x2-x1) - Object.keys(elves).length
}

async function part1(input: string[]): Promise<void> {
  let elves: {[a: string]: boolean} = {}

  for (let y=0; y<input.length; y++) {
    for (let x=0; x<input[y].length; x++) {
      if (input[y][x] == "#") {
        elves[key(y,x)] = true
      }
    }
  }

  show(elves)

  for (let round=0; round<10_000; round++) {
    let plans: Array<Move> = []

    // For all elves
    for (let k of Object.keys(elves)) {
      let [y, x] = k.split(",").map(s => parseInt(s))

      // Stay put if I'm alone here
      let count = 0
      for (let yn=y-1; yn<=y+1; yn++) {
        for (let xn=x-1; xn<=x+1; xn++) {
          if (elves[key(yn, xn)]) {
            count += 1
          }
        }
      }
      if (count <= 1) {
        continue
      }

      // Find the first valid direction for movement (if any)
      for (let dir=0; dir<4; dir++) {
        let cone = moveCones[(round + dir) % 4]
        let empty = true
        for (let [yd, xd] of cone) {
          if (elves[key(y+yd, x+xd)]) {
            empty = false
          }
        }
        if (!empty) {
          continue
        }
        let [yd, xd] = cone[1]
        plans.push([[y, x], [y+yd, x+xd]])
        break
      }
    }

    // Filter out conflicting moves
    let validMoves = Object.values(_.groupBy(plans, move => move[1]))
      .filter(l => l.length == 1)
      .map(m => m[0])

    // Execute moves
    for (let move of validMoves) {
      let [[y1, x1], [y2, x2]] = move
      // TODO sanity check here ??
      delete elves[key(y1, x1)]
      elves[key(y2, x2)] = true
    }

    if (validMoves.length == 0) {
      console.log("No elf moves on round", round+1)
      break
    }

    // console.log("Round", round+1)
    // showWithBounds(elves, -3, 10, -2, 10)

    if (round == 9) {
      console.log("Used land", land(elves))
    }
  }
}

async function part2(input: string[]): Promise<void> {

}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null