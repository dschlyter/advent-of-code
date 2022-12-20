import { readLines, ints } from './util'
import * as _ from 'lodash';
import { times } from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day20.txt")

  await part1(input)
  await part2(input)
}

function move2<T>(a: Array<T>, i: number, offset: number): Array<T> {
  let elem = a[i]
  let without = a.slice(0, i).concat(a.slice(i+1, a.length))
  let newPos = i + offset
  while (i < 0) {
    newPos += without.length
  }
  newPos = newPos % without.length
  return without.slice(0, newPos).concat([elem]).concat(without.slice(newPos, without.length))
}

async function part1(input: string[]): Promise<void> {
  let n = input.map(s => parseInt(s))

  // Track which numbers are moved
  let n2: Array<[number, boolean]> = n.map(x => [x, true])
  let i = 0
  while (i < n2.length) {
    if (!n2[i][1]) {
      i += 1
      continue
    }

    n2[i][1] = false
    n2 = move2(n2, i, n2[i][0])
  }

  let n3 = n2.map(x => x[0])
  let start = n3.indexOf(0)
  let a = n3[(start+1000) % n3.length]
  let b = n3[(start+2000) % n3.length]
  let c = n3[(start+3000) % n3.length]
  console.log(a, b, c, a+b+c)
}

async function part2(input: string[]): Promise<void> {
  let n: Array<[number, number]> = input.map((s, index) => [parseInt(s) * 811589153, index])

  // Track which numbers are moved
  for (let k=0; k<10; k++) {
    for (let i=0; i<n.length; i++) {
      let pos = -1
      for (let j=0; j<n.length; j++) {
        if (n[j][1] == i) {
          pos = j
        }
      }

      n = move2(n, pos, n[pos][0])
    }
  }

  let n3 = n.map(x => x[0])
  let start = n3.indexOf(0)
  let a = n3[(start+1000) % n3.length]
  let b = n3[(start+2000) % n3.length]
  let c = n3[(start+3000) % n3.length]
  console.log(a, b, c, a+b+c)
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null