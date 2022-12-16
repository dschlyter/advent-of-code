import { readLines, ints } from './util'
import * as _ from 'lodash';
import { times } from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day16_test.txt")

  await part1(input)
  await part2(input)
}

type Valve = {
  name: string,
  rate: number,
  leadsTo: Array<string>
}

type SearchNode = {
  time: number,
  valve: string,
  open: Array<string>
  rate: number,
  total: number
}

type QueueNode = {
  data: SearchNode,
  next: QueueNode | null
}

type Queue = {
  head: QueueNode | null,
  tail: QueueNode | null
}

function push(q: Queue, n: SearchNode) {
  let node = {data: n, next: null}
  if (q.head == null || q.tail == null) {
    q.head = node
    q.tail = node
  } else {
    q.tail.next = node
    q.tail = q.tail.next
  }
}

function push_first(q: Queue, n: SearchNode) {
  let node: QueueNode = {data: n, next: null}
  if (q.head == null || q.tail == null) {
    q.head = node
    q.tail = node
  } else {
    node.next = q.head
    q.head = node
  }
}

async function part1(input: string[]): Promise<void> {
  let valves: {[a: string]: Valve} = {}
  let canOpen = 0
  let totalFlow = 0

  for (let row of input) {
    let [p1, p2] = row.split(";")
    let [valve, rateStr] = p1.replace("Valve ", "").replace("has flow rate=", "").split(" ")
    let rate = parseInt(rateStr)
    let leadsTo = p2.replace(" tunnels lead to valves ", "").replace(" tunnel leads to valve ", "").split(", ")

    valves[valve] = {name: valve, rate, leadsTo}
    if (rate > 0) {
      canOpen += 1
      totalFlow += rate
    }
  }
  console.log(valves)

  let results = {
    release: 0
  }
  let best = {}

  let q: Queue = {
    head: null,
    tail: null
  }
  push(q, {time: 0, valve: 'AA', open: [], rate: 0, total: 0})

  while (q.head) {
    let h = q.head.data
    q.head = q.head.next

    if (h.time >= 30) {
      continue
    }
    let inferior = false
    for (let t=0; t<=h.time; t++) {
      let key = `${t},${h.valve},${h.open}`
      if (best[key] && best[key] > h.total) {
        inferior = true
      }
    }
    if (inferior) {
      continue
    }
    if (h.total + (30-h.time)*totalFlow < results.release) {
      // Cannot possibly catch up
      continue
    }
    let key = `${h.time},${h.valve},${h.open}`
    best[key] = h.total
    
    let newTotal = h.total + h.rate
    if (newTotal > results.release) {
      console.log(h.time, newTotal, h.open.length, canOpen)
      results.release = newTotal
    }

    if (h.open.length >= canOpen) {
      // All open, stay where you are
      push_first(q, {time: h.time+1, valve: h.valve, open: h.open, rate: h.rate, total: newTotal})
      continue
    }
    if (valves[h.valve].rate > 0 && !h.open.includes(h.valve)) {
      push(q, {time: h.time+1, valve: h.valve, open: h.open.concat(h.valve).sort(), rate: h.rate + valves[h.valve].rate, total: newTotal})
    }
    for (let v of valves[h.valve].leadsTo) {
      push(q, {time: h.time+1, valve: v, open: h.open, rate: h.rate, total: newTotal})
    }
  }
}

async function part2(input: string[]): Promise<void> {

}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null