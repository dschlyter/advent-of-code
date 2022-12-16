import { readLines, ints } from './util'
import * as _ from 'lodash';
import { times } from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day16.txt")

  await part1(input)
  await part2(input)
}

type Valve = {
  name: string,
  rate: number,
  leadsTo: Array<string>
}

type AllPairs = {
  [a: string]: {
    [b: string]: number
  }
}

async function part1(input: string[]): Promise<void> {
  let { valves, flowValves }: { valves: { [a: string]: Valve; }; flowValves: string[]; } = parseInput(input);

  let allPairs: AllPairs = calcAllPairs(valves);
  // console.log(allPairs)

  let results = {
    release: 0,
    tried: 0
  }

  function tryAll(tried: Array<string>, time: number, flow: number, total: number) {
    results.tried += 1
    if (time >= 30) {
      return
    }
    for (let next of flowValves) {
      if (tried.includes(next)) {
        continue
      }
      let travelTime = allPairs[tried[tried.length-1]][next] + 1
      tryAll(tried.concat([next]), time + travelTime, flow + valves[next].rate, total + travelTime * flow)
    }
    // Calculate the total if we stay put here
    let remainingTime = 30 - time
    let finalTotal = total + remainingTime * flow
    if (finalTotal > results.release) {
      results.release = finalTotal
      console.log(finalTotal)
    }
  }
  tryAll(['AA'], 0, 0, 0)
  console.log("search size", results.tried)

  console.log("---------")

  results = {
    release: 0,
    tried: 0
  }

  let maxFlow = Object.values(valves).map(x => x.rate).reduce((a,b) => a+b)
  console.log("max flow", maxFlow)

  function tryAll2(tried: Array<string>, time: number, t1: number, p1: string, t2: number, p2: string, flow: number, total: number) {
    results.tried += 1
    if (time >= 26) {
      if (total > results.release) {
        results.release = total
        console.log(results.release, tried.length, flowValves.length, results.tried)
      }
      return
    }

    // pre opt: 2445 13 15 1030500871
    // post opt: 2582 13 15 45945702
    if (total + (26 - time)*maxFlow < results.release) {
      // Current search branch is inferior, prune!
      return
    }

    if (tried.length < flowValves.length && (t1 <= time || t2 <= time)) {
      // Pick new locations
      for (let next of flowValves) {
        if (tried.includes(next)) {
          continue
        }
        if (t1 <= t2) {
          let travelTime = allPairs[p1][next] + 1
          tryAll2(tried.concat([next]), time, time + travelTime, next, t2, p2, flow, total)
        } else {
          let travelTime = allPairs[p2][next] + 1
          tryAll2(tried.concat([next]), time, t1, p1, time + travelTime, next, flow, total)
        }
      }
    } else {
      // Advance time and add flow
      let tNext = time + 1
      let addedFlow = 0
      if (t1 == tNext) {
        addedFlow += valves[p1].rate
      }
      if (t2 == tNext) {
        addedFlow += valves[p2].rate
      }
      tryAll2(tried, tNext, t1, p1, t2, p2, flow + addedFlow, total + flow)
    }
  }
  tryAll2([], 0, -1, 'AA', -1, 'AA', 0, 0)
  console.log("search size", results.tried)
}

function parseInput(input: string[]) {
  let valves: { [a: string]: Valve; } = {};
  // TODO remove, flowValves.length can replace
  let flowValves: Array<string> = [];

  for (let row of input) {
    let [p1, p2] = row.split(";");
    let [valve, rateStr] = p1.replace("Valve ", "").replace("has flow rate=", "").split(" ");
    let rate = parseInt(rateStr);
    let leadsTo = p2.replace(" tunnels lead to valves ", "").replace(" tunnel leads to valve ", "").split(", ");

    valves[valve] = { name: valve, rate, leadsTo };
    if (rate > 0) {
      flowValves.push(valve);
    }
  }
  console.log("number of flow valves", flowValves.length);
  return { valves, flowValves };
}

function calcAllPairs(valves: { [a: string]: Valve; }) {
  let allPairs: AllPairs = {};
  for (let v of Object.values(valves)) {
    allPairs[v.name] = { [v.name]: 0 };
    for (let n of valves[v.name].leadsTo) {
      allPairs[v.name][n] = 1;
    }
  }
  for (let k = 0; k < Object.values(valves).length; k++) {
    for (let v1 of Object.keys(valves)) {
      for (let v2 of Object.keys(valves)) {
        for (let mid of Object.keys(valves)) {
          if (allPairs[v1][mid] && allPairs[mid][v2] && v1 != v2) {
            allPairs[v1][v2] = Math.min(allPairs[v1][v2] || 9999, allPairs[v1][mid] + allPairs[mid][v2]);
          }
        }
      }
    }
  }
  return allPairs;
}

async function part2(input: string[]): Promise<void> {

}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null

  /*
Old solution, unused

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
  */