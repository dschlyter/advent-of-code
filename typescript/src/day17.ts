import { readLines, ints } from './util'
import * as _ from 'lodash';
import { times } from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day17.txt")

  await part1(input)
  await part2(input)
}

type Point = [number, number]
type Shape = Array<Point>

function translate(shape: Shape, dir: Point): Shape {
  let ret: Array<Point> = []
  let [xd, yd] = dir
  for (let [x, y] of shape) [
    ret.push([x+xd, y+yd])
  ]
  return ret
}

function printState(block: Shape, occupied: {[a: string]: boolean}) {
  let points: Array<Point> = []
  for (let p of block) {
    points.push(p)
  }
  for (let s of Object.keys(occupied)) {
    let [y, x] = s.split(",").map(x => parseInt(x))
    points.push([y, x])
  }

  let maxY = points.map((([y, x]) => y)).reduce((a, b) => Math.max(a,b), 0)
  // Javascript...
  let cmp = points.map(x => x.toString())
  for (let y=maxY; y>=0; y--) {
    let row = ""
    for (let x=0; x<7; x++) {
      if (cmp.includes([y,x].toString())) {
        row += "#"
      } else {
        row += "."
      }
    }
    console.log(row)
  }
}

const shapes: Array<Shape> = [
  [[0,0], [0,1], [0,2], [0,3]],
  [[2,1], [1,0], [1,1], [1,2], [0,1]],
  [[2,2], [1,2], [0,0], [0,1], [0,2]],
  [[3,0], [2,0], [1,0], [0,0]],
  [[1,0], [1,1], [0,0], [0,1]],
]

async function part1(input: string[]): Promise<void> {
  let pushes = input[0]

  const minX = 0, maxX = 7

  let highestBlock = -1
  let time = 0
  let occupied: {[a: string]: boolean} = {}

  for (let blockCount=0; blockCount<2022; blockCount++) {
    // Init block position
    let block = shapes[blockCount % shapes.length]
    block = translate(block, [0, 2])
    block = translate(block, [highestBlock+4, 0])
    
    while (true) {
      // Push block to the side
      let push = pushes[time % pushes.length]
      // console.log(push)
      let pushedBlock = translate(block, push == ">" ? [0, 1]: [0, -1])
      time += 1
      if (pushedBlock.every(([y, x]) => x >= minX && x < maxX && !occupied[`${y},${x}`])) {
        block = pushedBlock
      }

      // Pushed down
      let dropped = translate(block, [-1, 0])
      if (dropped.every(([y, x]) => y >= 0 && !occupied[`${y},${x}`])) {
        block = dropped
      } else {
        break
      }

    }
    for (let [y, x] of block) {
      occupied[`${y},${x}`] = true
      highestBlock = Math.max(highestBlock, y)
    }
    // printState(block, occupied)
  }

  console.log(highestBlock + 1)
}

async function part2(input: string[]): Promise<void> {
  let pushes = input[0]

  const minX = 0, maxX = 7
  const target = 1000000000000

  let highestBlock = -1
  let time = 0
  let occupied: {[a: string]: boolean} = {}

  let repeats = {}
  let passedBlocks = 0
  let passedHeight = 0

  for (let blockCount=0; blockCount + passedBlocks < target; blockCount++) {
    // Init block position
    let block = shapes[blockCount % shapes.length]
    block = translate(block, [0, 2])
    block = translate(block, [highestBlock+4, 0])
    
    while (true) {
      // Push block to the side
      let push = pushes[time % pushes.length]
      // console.log(push)
      let pushedBlock = translate(block, push == ">" ? [0, 1]: [0, -1])
      time += 1
      if (pushedBlock.every(([y, x]) => x >= minX && x < maxX && !occupied[`${y},${x}`])) {
        block = pushedBlock
      }

      // Pushed down
      let dropped = translate(block, [-1, 0])
      if (dropped.every(([y, x]) => y >= 0 && !occupied[`${y},${x}`])) {
        block = dropped
      } else {
        break
      }
    }
    for (let [y, x] of block) {
      occupied[`${y},${x}`] = true
      highestBlock = Math.max(highestBlock, y)
    }

    let key = `${time % pushes.length},${blockCount % shapes.length}`
    let r = repeats[key]
    if (!r) {
      repeats[key] = {
        baseBlocks: blockCount,
        baseHeight: highestBlock,
        count: 0,
        blocks: -1,
        height: -1,
      }
    } else if (passedBlocks == 0) {
      r.count += 1
      if (r.count == 1) {
        r.height = highestBlock - r.baseHeight
        r.blocks = blockCount - r.baseBlocks
      } else if (r.count > 10) {
        let shouldBe = r.baseHeight + r.count * r.height
        if (shouldBe == highestBlock) {
          // This seems to be a reliable point of repetition
          let blocksLeft = target - blockCount
          let repeatsLeft = Math.floor(blocksLeft / r.blocks)
          passedBlocks = repeatsLeft * r.blocks
          passedHeight = repeatsLeft * r.height
          console.log("found repeat at", blockCount, "will repeat", repeatsLeft, "times")
        } else {
          console.log("bad point for repeat", shouldBe, "!=", highestBlock)
        }
      }
    }
  }

  console.log(passedHeight + highestBlock + 1)
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null