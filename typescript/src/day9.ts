import { readLines } from './util'
import * as _ from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day9.txt")

  await part1(input)
  await part2(input)
}

function norm(n: number) {
    if (n > 0) {
        return 1
    } else if (n < 0) {
        return -1
    } else {
        return 0;
    }
}

async function part1(input: string[]): Promise<void> {
    let h = [0, 0]
    let t = [0, 0]
    let pos = {}
    for (let m of input) {
        let [dir, steps] = m.split(" ")

        for (let s=0; s<parseInt(steps); s++) {
            if (dir == "R") {
                h[0] += 1
            } else if (dir == "L") {
                h[0] -= 1
            } else if (dir == "U") {
                h[1] += 1
            } else if (dir == "D") {
                h[1] -= 1
            }
        
            if (Math.abs(h[0] - t[0]) > 1 || Math.abs(h[1] - t[1]) > 1) {
                t[0] = t[0] + norm(h[0] - t[0])
                t[1] = t[1] + norm(h[1] - t[1])
            }

            pos[""+t[0]+","+t[1]] = 1
        }
    }

    console.log(Object.keys(pos).length)
}

type Point = {
    x: number,
    y: number
}

async function part2(input: string[]): Promise<void> {
    let rope: Point[] = []
    for (let i=0; i<10; i++) {
        rope.push({x: 0, y: 0})
    }

    let pos = {}
    for (let m of input) {
        let [dir, steps] = m.split(" ")

        for (let s=0; s<parseInt(steps); s++) {
            if (dir == "R") {
                rope[0].x += 1
            } else if (dir == "L") {
                rope[0].x -= 1
            } else if (dir == "U") {
                rope[0].y += 1
            } else if (dir == "D") {
                rope[0].y -= 1
            }
        
            for (let i=0; i<rope.length-1; i++) {
                let n = i+1
                if (Math.abs(rope[i].x - rope[n].x) > 1 || Math.abs(rope[i].y - rope[n].y) > 1) {
                    rope[n].x = rope[n].x + norm(rope[i].x - rope[n].x)
                    rope[n].y = rope[n].y + norm(rope[i].y - rope[n].y)
                }
            }

            pos[""+rope[9].x+","+rope[9].y] = 1
        }
    }

    console.log(Object.keys(pos).length)
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null