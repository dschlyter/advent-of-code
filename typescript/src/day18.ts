import { readLines, ints } from './util'
import * as _ from 'lodash';
import { times } from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day18.txt")

  await part1(input)
  await part2(input)
}

const near = [
  [0, 0, -1],
  [0, 0, 1],
  [0, -1, 0],
  [0, 1, 0],
  [-1, 0, 0],
  [1, 0, 0],
]

async function part1(input: string[]): Promise<void> {
  let points: {[a: string]: boolean} = {}
  for (let row of input) {
    points[row] = true
  }

  let res = 0
  for (let p of Object.keys(points)) {
    let [x,y,z] = p.split(",").map(x => parseInt(x))
    for (let [xd, yd, zd] of near) {
      let pos = `${x+xd},${y+yd},${z+zd}`
      if (!points[pos]) {
        res += 1
      }
    }
  }
  console.log(res)
}

async function part2(input: string[]): Promise<void> {
  let points: {[a: string]: boolean} = {}
  for (let row of input) {
    points[row] = true
  }

  let outside: {[a: string]: boolean} = {}

  let pointsInt = Object.keys(points).map(p => p.split(",").map(x => parseInt(x)))
  let minX = pointsInt.map(([x,y,z]) => x).reduce((a,b) => Math.min(a,b)) - 1 
  let maxX = pointsInt.map(([x,y,z]) => x).reduce((a,b) => Math.max(a,b)) + 1
  let minY = pointsInt.map(([x,y,z]) => y).reduce((a,b) => Math.min(a,b)) - 1
  let maxY = pointsInt.map(([x,y,z]) => y).reduce((a,b) => Math.max(a,b)) + 1
  let minZ = pointsInt.map(([x,y,z]) => z).reduce((a,b) => Math.min(a,b)) - 1
  let maxZ = pointsInt.map(([x,y,z]) => z).reduce((a,b) => Math.max(a,b)) + 1

  let q: Array<[number, number, number]> = []
  q.push([minX, minY, minZ])
  let i = 0
  while (i < q.length) {
    let [x, y, z] = q[i]
    i += 1

    if (x < minX || x > maxX || y < minY || y > maxY || z < minZ || z > maxZ) {
      continue
    }
    let pos = `${x},${y},${z}`
    if (outside[pos] || points[pos]) {
      continue
    }
    outside[pos] = true
    for (let [xd, yd, zd] of near) {
      q.push([x+xd, y+yd, z+zd])
    }
  }

  let res = 0
  for (let p of Object.keys(points)) {
    let [x,y,z] = p.split(",").map(x => parseInt(x))
    for (let [xd, yd, zd] of near) {
      let pos = `${x+xd},${y+yd},${z+zd}`
      if (outside[pos]) {
        res += 1
      }
    }
  }
  console.log(res)
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null