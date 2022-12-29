import { readLines, ints } from './util'
import * as _ from 'lodash';
import { isInteger, isNumber, times } from 'lodash';

// Note: Run this with --stack-size=1000000 for the DFS

const inputFile = "input/day24.txt"

type Point = [number, number]
type Positions = {[a: string]: boolean}

async function main(): Promise<void> {
  const input = await readLines(inputFile)

  await part1(input)
  await part2(input)
}

const dirs: Array<Point> = [[0,1], [1,0], [0,-1],[-1,0],[0,0]]

type Blizzard = {
  pos: Point
  dir: Point
}

function key([y, x]: Point) {
  return `${y},${x}`
}

function add(a: Point, b: Point): Point {
  return [a[0] + b[0], a[1] + b[1]]
}

async function part1(input: string[]): Promise<void> {
  let blizzards: Array<Blizzard> = []
  for (let y=0; y<input.length; y++) {
    for (let x=0; x<input[y].length; x++) {
      let c = input[y][x]
      let dir: number | undefined = {
        '>': 0,
        'v': 1,
        '<': 2,
        '^': 3
      }[c]

      if (c != "." && c != "#") {
        if (dir == undefined) {
          throw Error("Invalid blizzard "+c)
        }

        blizzards.push({
          pos: [y, x],
          dir: dirs[dir]
        })
      }
    }
  }

  let height = input.length
  let width = input[0].length
  let start: Point = [0, 1]
  let end: Point = [height-1, width-2]

  let you: Positions = {[key(start)]: true}
  let round = 1
  let phase = 0
  let target = end

  while (round < 10000) {
    // Update blizzard positions, and flag blocked positions
    let blocked: Positions = {}
    for (let b of blizzards) {
      b.pos[0] += b.dir[0]
      b.pos[1] += b.dir[1]
      if (b.pos[0] == 0) {
        b.pos[0] = height - 2
      }
      if (b.pos[0] == height - 1) {
        b.pos[0] = 1
      }
      if (b.pos[1] == 0) {
        b.pos[1] = width - 2
      }
      if (b.pos[1] == width - 1) {
        b.pos[1] = 1
      }
      blocked[key(b.pos)] = true
    }

    // Update all positions where you might be
    let nextYou: Positions = {}
    for (let k of Object.keys(you)) {
      let [y, x] = k.split(",").map(s => parseInt(s))
      for (let d of dirs) {
        let newPos = add([y, x], d)
        let [ny, nx] = newPos
        if (ny < 0 || ny >= height || input[ny][nx] == "#") {
          continue
        }
        if (!blocked[key(newPos)]) {
          nextYou[key(newPos)] = true
        }
      }
    }
    you = nextYou
    // showWithBounds(blocked, 0, 6, 0, 6)
    // console.log(blizzards)
    // console.log("Round", round, Object.keys(you), end)

    // Check for targets
    let target = [end, start, end][phase]
    if (you[key(target)]) {
      console.log("Phase", phase, "took", round, "minutes")
      you = {[key(target)]: true}
      phase += 1
      if (phase == 3) {
        break
      }
    }

    round += 1
  }

  console.log("Navigation took", round, "rounds")
}

async function part2(input: string[]): Promise<void> {

}

function show(pos: Positions) {
  let y1 = 1e9, y2 = -1e9
  let x1 = 1e9, x2 = -1e9
  showWithBounds(pos, y1, y2, x1, x2)
}

function showWithBounds(pos: Positions, y1, y2, x1, x2) {
  for (let k of Object.keys(pos)) {
    let [y, x] = k.split(",").map(s => parseInt(s))
    y1 = Math.min(y1, y)
    y2 = Math.max(y2, y)
    x1 = Math.min(x1, x)
    x2 = Math.max(x2, x)
  }
  for (let y=y1; y<=y2; y++) {
    let s = ""
    for (let x=x1; x<=x2; x++) {
      if (pos[key([y, x])]) {
        s += "#"
      } else {
        s += "."
      }
    }
    console.log(s)
  }
  console.log()
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null