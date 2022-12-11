import { readLines } from './util'
import * as _ from 'lodash';

type Input = {
  startingItems: number[],
  operation: (a: number) => number,
  test: number,
  target: [number, number]
}

async function main(): Promise<void> {
  // const input = await readLines("input/day11.txt")

  // Manual input data because it was a pain to parse - but more pain to write like this :(
  let inputData: Input[] = [{
    startingItems: [61],
    operation: old => old * 11,
    test: 5,
    target: [7, 4]
  }, {
    startingItems: [76, 92, 53, 93, 79, 86, 81],
    operation: old => old + 4,
    test: 2,
    target: [2, 6]
  }, {
    startingItems: [91, 99],
    operation: old => old * 19,
    test: 13,
    target: [5, 0]
  }, {
    startingItems: [58, 67, 66],
    operation: old => old * old,
    test: 7,
    target: [6, 1]
  }, {
    startingItems: [94, 54, 62, 73],
    operation: old => old + 1,
    test: 19,
    target: [3, 7]
  }, {
    startingItems: [59, 95, 51, 58, 58],
    operation: old => old + 3,
    test: 11,
    target: [0, 4]
  }, {
    startingItems: [87, 69, 92, 56, 91, 93, 88, 73],
    operation: old => old + 8,
    test: 3,
    target: [5, 2]
  }, {
    startingItems: [71, 57, 86, 67, 96, 95],
    operation: old => old + 7,
    test: 17,
    target: [3, 1]
  }]

  let inputDataTest: Input[] = [
    {
      startingItems: [79, 98],
      operation: old => old * 19,
      test: 23,
      target: [2, 3]
    },
    {
      startingItems: [54, 65, 75, 74],
      operation: old => old + 6,
      test: 19,
      target: [2, 0]
    },
    {
      startingItems: [79, 60, 97],
      operation: old => old * old,
      test: 13,
      target: [1, 3]
    },
    {
      startingItems: [74],
      operation: old => old + 3,
      test: 17,
      target: [0, 1]
    }
  ]


  await part1(inputData)
  await part2(inputData)
}

type MonkeyState = {
  items: number[],
  throws: number,
  def: Input
}

async function part1(input: Input[]): Promise<void> {
  let monkeys: MonkeyState[] = []
  for (let m of input) {
    monkeys.push({
      items: [...m.startingItems],
      throws: 0,
      def: m
    })
  }

  for (let i=0; i<20; i++) {
    for (let m of monkeys) {
      for (let item of m.items) {
        item = m.def.operation(item)
        item = Math.floor(item / 3)
        if (item % m.def.test == 0) {
          monkeys[m.def.target[0]].items.push(item)
        } else {
          monkeys[m.def.target[1]].items.push(item)
        }
        m.throws += 1
      }
      m.items = []
    }
  }

  let t = monkeys.map(m => m.throws)
  t = t.sort((a,b) => a - b).reverse()
  let res = t[0] * t[1]
  console.log(res)
}

async function part2(input: Input[]): Promise<void> {
  let div = input.map(m => m.test).reduce((a,b) => a*b)

  let monkeys: MonkeyState[] = []
  for (let m of input) {
    monkeys.push({
      items: [...m.startingItems],
      throws: 0,
      def: m
    })
  }

  for (let i=0; i<10_000; i++) {
    for (let m of monkeys) {
      for (let item of m.items) {
        item = m.def.operation(item)
        // item = Math.floor(item / 3)
        item = item % div
        if (item % m.def.test == 0) {
          monkeys[m.def.target[0]].items.push(item)
        } else {
          monkeys[m.def.target[1]].items.push(item)
        }
        m.throws += 1
      }
      m.items = []
    }
  }

  let t = monkeys.map(m => m.throws)
  t = t.sort((a,b) => a - b).reverse()
  let res = t[0] * t[1]
  console.log(res)
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null