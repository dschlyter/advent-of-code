import { readLines } from './util'
import * as _ from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day10.txt")

  await part1(input)
  await part2(input)
}


async function part1(input: string[]): Promise<void> {
    let cycle = 1, X = 1
    let signalSum = 0

    for (let op of input) {

        let s = op.split(" ")
        let o = s[0]
        if (o == 'noop') {
            cycle += 1
        } else if (o == 'addx') {
            cycle += 1
            if ((cycle - 20) % 40 == 0) {
                let signal = cycle * X
                signalSum += signal
            }
            cycle += 1
            X += parseInt(s[1])
        } else {
            throw Error("Invalid op "+op)
        }

        if ((cycle - 20) % 40 == 0) {
            let signal = cycle * X
            signalSum += signal
        }
    }

    console.log(signalSum)
}

async function part2(input: string[]): Promise<void> {
    let cycle = 1, X = 1
    let crt = ""

    function draw(cycle, X) {
        console.log("cycle", cycle, "X", X)
        if (Math.abs((cycle-1)%40 - X) <= 1) {
            return "#"
        } else {
            return "."
        }
    }

    for (let op of input) {
        let s = op.split(" ")
        let o = s[0]
        if (o == 'noop') {
            crt += draw(cycle, X)
            cycle += 1
        } else if (o == 'addx') {
            crt += draw(cycle, X)
            cycle += 1
            crt += draw(cycle, X)
            cycle += 1
            X += parseInt(s[1])
        } else {
            throw Error("Invalid op "+op)
        }
    }

    let disp = ""
    for (let i=0; i<crt.length; i++) {
        if (i % 40 == 0) {
            disp += "\n";
        }
        disp += crt[i]
    }
    console.log(disp)
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null