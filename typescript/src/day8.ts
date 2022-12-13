import { readLines } from './util'
import * as _ from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day8.txt")

  await part1(input)
  await part2(input)
}

async function part1(input: string[]): Promise<void> {
    let visible = {}
    for (let row=0; row<input.length; row++) {
        let m = -1
        for (let col=0; col<input[row].length; col++) {
            let h = parseInt(input[row][col])
            if (h > m) {
                m = h
                let key = `${row},${col}`
                visible[key] = true
            }
        }

        m = -1
        for (let col=input[row].length-1; col>=0; col--) {
            let h = parseInt(input[row][col])
            if (h > m) {
                m = h
                let key = `${row},${col}`
                visible[key] = true
            }
        }
    }

    for (let col = 0; col < input[0].length; col++) {
        let m = -1
        for (let row=0; row<input.length; row++) {
            let h = parseInt(input[row][col])
            if (h > m) {
                m = h
                let key = `${row},${col}`
                visible[key] = true
            }
        }

        m = -1
        for (let row=input.length-1; row>=0; row--) {
            let h = parseInt(input[row][col])
            if (h > m) {
                m = h
                let key = `${row},${col}`
                visible[key] = true
            }
        }
    }

    console.log(Object.keys(visible).length)
}

async function part2(input: string[]): Promise<void> {
    let best = 0
    for (let row=0; row<input.length; row++) {
        for (let col=0; col<input[row].length; col++) {
            let h = parseInt(input[row][col])

            let up = 0;
            for (let r2=row-1; r2>=0; r2--) {
                up += 1
                if (parseInt(input[r2][col]) >= h) {
                    break
                }
            }

            let down = 0;
            for (let r2=row+1; r2<input.length; r2++) {
                down += 1
                if (parseInt(input[r2][col]) >= h) {
                    break
                }
            }

            let left = 0;
            for (let c2=col-1; c2>=0; c2--) {
                left += 1
                if (parseInt(input[row][c2]) >= h) {
                    break
                }
            }

            let right = 0;
            for (let c2=col+1; c2<input[0].length; c2++) {
                right += 1
                if (parseInt(input[row][c2]) >= h) {
                    break
                }
            }

            let score = up * down * left * right
            if (score > best) {
                best = score
                console.log(score)
            }
        }
    }
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null