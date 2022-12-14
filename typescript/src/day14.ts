import { readLines } from './util'
import * as _ from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day14.txt")

  await part1(input)
  await part2(input)
}

async function part1(input: string[]): Promise<void> {
  let rock = makeRock(input);

  let res = 0
  while (sandfall(0, 500, rock)) {
    res += 1
  }
  console.log(res)
}

function makeRock(input: string[]) {
  let rock = {};

  for (let line of input) {
    let p = line.split(" -> ");
    for (let i = 0; i < p.length - 1; i++) {
      let [x1, y1] = p[i].split(",").map(x => parseInt(x));
      let [x2, y2] = p[i + 1].split(",").map(x => parseInt(x));

      for (let y = Math.min(y1, y2); y < Math.max(y1, y2) + 1; y++) {
        for (let x = Math.min(x1, x2); x < Math.max(x1, x2) + 1; x++) {
          rock[`${y},${x}`] = true;
        }
      }
    }
  }

  return rock;
}

const abyss = 1000
function sandfall(y: number, x: number, rock: {[a: string]: boolean}): boolean {
  if (y >= abyss) {
    return false
  }
  if (rock[`${y},${x}`]) {
    // We spawned in a blocked area
    return false
  }

  for (let [y2, x2] of [[y+1, x], [y+1, x-1], [y+1, x+1]]) {
    if (!rock[`${y2},${x2}`]) {
      return sandfall(y2, x2, rock)
    }
  }

  rock[`${y},${x}`] = true
  return true
}

async function part2(input: string[]): Promise<void> {
  let rock = makeRock(input);

  let floor = 0
  for (let pos of Object.keys(rock)) {
    let y = parseInt(pos.split(",")[0])
    floor = Math.max(floor, y + 2)
  }
  console.log("floor at", floor)

  let mid = 500
  for (let x = mid-2*floor; x<mid+2*floor; x++) {
    rock[`${floor},${x}`] = true
  }

  let res = 0
  while (sandfall(0, 500, rock)) {
    res += 1
  }
  console.log(res)
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null