import { readLines, ints } from './util'
import * as _ from 'lodash';
import { times } from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day19.txt")

  // await part1(input, 24)
  await part2(input)
}

type Blueprint = {
  id: number,
  ore: number,
  clay: number,
  obsidian: [number, number],
  geode: [number, number],
}

async function part1(input: string[], timeAvailable: number): Promise<void> {
  let blueprints: Array<Blueprint> = parseInput(input);

  let t = {count: 0, best: 0}
  let mem = {}

  function search(bp: Blueprint, time, order, ore, clay, obsidian, oreRobot, clayRobot, obsidianRobot, geodeRobot) {
    t.count += 1
    if (t.count % 1000) {
      // console.log(time, oreRobot, clayRobot, obsidianRobot, geodeRobot, t.best)
    }

    let key = [time, order, ore, clay, obsidian, oreRobot, clayRobot, obsidianRobot, geodeRobot].join(",")
    if (mem[key] !== undefined) {
      return mem[key]
    }

    if (time >= timeAvailable) {
      return 0
    }

    // Prune if we build more robots than we can spend material
    if (oreRobot > Math.max(bp.clay, bp.obsidian[0], bp.geode[0])) {
      return 0
    }
    if (clayRobot > bp.obsidian[1]) {
      return 0
    }
    if (obsidianRobot > bp.geode[1]) {
      return 0
    }

    let newOre = oreRobot
    let newClay = clayRobot
    let newObsidian = obsidianRobot
    let newGeode = geodeRobot

    if (order == "ore" && ore >= bp.ore) {
      ore -= bp.ore
      oreRobot += 1
      order = null
    }
    if (order == "clay" && ore >= bp.clay) {
      ore -= bp.clay
      clayRobot += 1
      order = null
    }
    if (order == "obsidian" && ore >= bp.obsidian[0] && clay >= bp.obsidian[1]) {
      ore -= bp.obsidian[0]
      clay -= bp.obsidian[1]
      obsidianRobot += 1
      order = null
    }
    if (order == "geode" && ore >= bp.geode[0] && obsidian >= bp.geode[1]) {
      ore -= bp.geode[0]
      obsidian -= bp.geode[1]
      geodeRobot += 1
      order = null
    }

    let bestGeodes = 0
    if (order !== null) {
      bestGeodes = search(bp, time + 1, order, ore + newOre, clay + newClay, obsidian + newObsidian, oreRobot, clayRobot, obsidianRobot, geodeRobot)
    } else {
      for (let newOrder of ["ore", "clay", "obsidian", "geode"]) {
        bestGeodes = Math.max(bestGeodes, search(bp, time + 1, newOrder, ore + newOre, clay + newClay, obsidian + newObsidian, oreRobot, clayRobot, obsidianRobot, geodeRobot))
      }
    }

    let res = newGeode + bestGeodes
    mem[key] = res
    t.best = Math.max(t.best, res)
    return res
  }

  let qSum = 0
  for (let bp of blueprints) {
    console.log("Blueprint", bp.id)
    mem = {}
    let best = search(bp, 0, null, 0, 0, 0, 1, 0, 0, 0)
    console.log("best is", best)

    let quality = bp.id * best
    qSum += quality
  }
  console.log("quality sum", qSum)
}

function parseInput(input: string[]) {
  let blueprints: Array<Blueprint> = [];
  for (let i = 0; i < input.length; i++) {
    let row = input[i];
    let s = row.split(":")[1].split(".").map(x => x.trim().split(" "));

    blueprints.push({
      id: i + 1,
      ore: parseInt(s[0][4]),
      clay: parseInt(s[1][4]),
      obsidian: [parseInt(s[2][4]), parseInt(s[2][7])],
      geode: [parseInt(s[3][4]), parseInt(s[3][7])],
    });
  }
  return blueprints;
}

class State {
  ore: number;
  clay: number;
  obsidian: number;
  geode: number;
  oreRobot: number;
  clayRobot: number;
  obsidianRobot: number;
  geodeRobot: number;

  constructor(ore: number, clay: number, obsidian: number, geode: number, oreRobot: number, clayRobot: number, obsidianRobot: number, geodeRobot: number) {
    this.ore = ore
    this.clay = clay
    this.obsidian = obsidian;
    this.geode = geode;
    this.oreRobot = oreRobot;
    this.clayRobot = clayRobot;
    this.obsidianRobot = obsidianRobot;
    this.geodeRobot = geodeRobot;
  }

  better(s: State, bp: Blueprint, timeLeft: number): boolean {
    return (
      this.maxOre(bp, timeLeft) > s.maxOre(bp, timeLeft) || 
      this.maxClay(bp, timeLeft) > s.maxClay(bp, timeLeft) || 
      this.maxObsidian(bp, timeLeft) > s.maxObsidian(bp, timeLeft) || 
      // this.ore > s.ore ||
      // this.clay > s.clay || 
      // this.obsidian > s.obsidian ||
      this.geode > s.geode ||
      this.oreRobot > s.oreRobot ||
      this.clayRobot > s.clayRobot ||
      this.obsidianRobot > s.obsidianRobot ||
      this.geodeRobot > s.geodeRobot
    )
  }

  // These methods cap the amount of resources to the max that might be needed, this means that superfluous resources are not better
  maxOre(bp: Blueprint, timeLeft: number) {
    let maxNeeded = Math.max(0, Math.max(bp.ore, bp.clay, bp.obsidian[0], bp.geode[0]) * timeLeft - this.oreRobot * (timeLeft - 1))
    return Math.min(this.ore, maxNeeded)
  }

  maxClay(bp: Blueprint, timeLeft: number) {
    let maxNeeded = Math.max(0, bp.obsidian[1] * timeLeft - this.clayRobot * (timeLeft - 1))
    return Math.min(this.clay, maxNeeded)
  }

  maxObsidian(bp: Blueprint, timeLeft: number) {
    let maxNeeded = Math.max(0, bp.geode[1] * timeLeft - this.obsidianRobot * (timeLeft - 1))
    return Math.min(this.obsidian, maxNeeded)
  }
}

/**
 * The idea for this one is to track the "pareto frontier" of the entire search space 
 * Any candidate which is worse than some other (less or equal amount of all resources and bots) will be discarded, since it cannot yield the best result
 * Comparing the candidates is O(n^2) but this is worth it to keep the search space small
 * 
 * One issue is that there a large part of this frontier is hoarding more resources than needed, thus the comparison caps the amount of resources to the max amount needed, this prunes a vast majority of the search candidates
 * 
 * This seems to work quite well, solving the problem in 0.4 seconds
 */
async function part2(input: string[]): Promise<void> {
  const timeLimit = 32
  const blueprints: Array<Blueprint> = parseInput(input);

  let result = 1
  for (let bp of blueprints) {
    if (bp.id > 3) {
      continue
    }

    let oreCost = bp.ore
    let clayCost = bp.clay
    let obsidianOreCost = bp.obsidian[0]
    let obsidianClayCost = bp.obsidian[1]
    let geodeOreCost = bp.geode[0]
    let geodeObsidianCost = bp.geode[1]

    // In the first cycle, the ore bot harvests one ore
    let frontier: Array<State> = [new State(1, 0, 0, 0, 1, 0, 0, 0)]

    for (let time=1; time<timeLimit; time++) {
      let nextFrontier: Array<State> = []

      for (let f of frontier) {
        // Avoid creating more robots than we can spend resources, this will always be waste
        if (f.oreRobot > Math.max(oreCost, clayCost, obsidianOreCost, geodeOreCost) || f.clayRobot > obsidianClayCost || f.obsidianRobot > geodeObsidianCost) { 
          continue
        }

        // Create successors for all construction options, including not constructing
        if (f.ore >= oreCost) {
          nextFrontier.push(new State(f.ore + f.oreRobot - oreCost, f.clay + f.clayRobot, f.obsidian + f.obsidianRobot, f.geode + f.geodeRobot, f.oreRobot + 1, f.clayRobot, f.obsidianRobot, f.geodeRobot))
        }
        if (f.ore >= clayCost) {
          nextFrontier.push(new State(f.ore + f.oreRobot - clayCost, f.clay + f.clayRobot, f.obsidian + f.obsidianRobot, f.geode + f.geodeRobot, f.oreRobot, f.clayRobot + 1, f.obsidianRobot, f.geodeRobot))
        }
        if (f.ore >= obsidianOreCost && f.clay >= obsidianClayCost) {
          nextFrontier.push(new State(f.ore + f.oreRobot - obsidianOreCost, f.clay + f.clayRobot - obsidianClayCost, f.obsidian + f.obsidianRobot, f.geode + f.geodeRobot, f.oreRobot, f.clayRobot, f.obsidianRobot + 1, f.geodeRobot))
        }
        if (f.ore >= geodeOreCost && f.obsidian >= geodeObsidianCost) {
          nextFrontier.push(new State(f.ore + f.oreRobot - geodeOreCost, f.clay + f.clayRobot, f.obsidian + f.obsidianRobot - geodeObsidianCost, f.geode + f.geodeRobot, f.oreRobot, f.clayRobot, f.obsidianRobot, f.geodeRobot + 1))
        }
        nextFrontier.push(new State(f.ore + f.oreRobot, f.clay + f.clayRobot, f.obsidian + f.obsidianRobot, f.geode + f.geodeRobot, f.oreRobot, f.clayRobot, f.obsidianRobot, f.geodeRobot))
      }

      let size1 = nextFrontier.length 

      let keep: Array<State> = []
      for (let f of nextFrontier) {
        let keep2: Array<State> = []
        let bad = false

        for (let f2 of keep) {
          let b = f.better(f2, bp, timeLimit - time)
          let w = f2.better(f, bp, timeLimit - time)
          if (w && !b) {
            // This one is inferior, discard it. Keep all the existing ones unchanged.
            bad = true
            keep2 = keep
            break
          }
          if (w) {
            // If the comparison point is not better in any way, it can be discarded
            keep2.push(f2)
          }
        }

        if (!bad) {
          keep2.push(f)
        }
        keep = keep2
      }

      let best = 0
      for (let f of frontier) {
        best = Math.max(best, f.geode)
      }

      frontier = keep
      console.log("time", time, "frontier size", size1, "after prune", frontier.length, "best progress", best)
    }

    let best = 0
    for (let f of frontier) {
      best = Math.max(best, f.geode)
    }
    console.log(best)
    result *= best
  }

  console.log(result)
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null