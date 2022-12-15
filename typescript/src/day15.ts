import { readLines, ints } from './util'
import * as _ from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day15.txt")

  await part1(input)
  await part2(input)
}

type GridFlags = {[a: string]: boolean}
type Sensor = {x: number, y: number, dist: number}

async function part1(input: string[]): Promise<void> {
  let sensors: Array<Sensor> = []
  let beacons: GridFlags = {}
  let minX = 0, maxX = 0
  for (let row of input) {
    let [sx, sy, bx, by] = ints(row)
    let d = dist(sx, sy, bx, by)
    maxX = Math.max(maxX, sx + Math.abs(sx - bx))
    minX = Math.min(minX, sx - Math.abs(sx - bx))

    sensors.push({x: sx, y: sy, dist: d})
    beacons[`${bx},${by}`] = true
  }
  // let y = 10
  let y = 2000000
  let count = 0
  for (let x=minX; x<=maxX; x++) {
    let inRange = false
    let isBeacon = beacons[`${x},${y}`] || false
    for (let s of sensors) {
      if (dist(x, y, s.x, s.y) <= s.dist) {
        inRange = true
      }
    }
    if (!isBeacon && inRange) {
      count += 1
    }
  }
  console.log(count)
}

function dist(x1, y1, x2, y2) {
  return Math.abs(x1 - x2) + Math.abs(y1 - y2)
}

async function part2(input: string[]): Promise<void> {
  let sensors: Array<Sensor> = []
  for (let row of input) {
    let [sx, sy, bx, by] = ints(row)
    let dist = Math.abs(by - sy) + Math.abs(bx - sx)

    sensors.push({x: sx, y: sy, dist})
  }

  // let xMax = 20, yMax = 20
  let xMax = 4000000, yMax = 4000000
  for (let y=0; y<=yMax; y++) {
    for (let x=0; x<=xMax; x++) {
      let outside = true
      for (let s of sensors) {
        let diff = s.dist - dist(x, y, s.x, s.y)
        if (diff >= 0) {
          outside = false
          x += diff
          break
        }
      }
      if (outside) {
        console.log(x, y)
        console.log(x * 4000000 + y)
      }
    }
  }

}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null