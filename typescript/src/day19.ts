import { readLines, ints } from './util'
import * as _ from 'lodash';
import { times } from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day19.txt")

  await part1(input)
  await part2(input)
}

type Blueprint = {
  id: number,
  ore: number,
  clay: number,
  obsidian: [number, number],
  geode: [number, number],
}

async function part1(input: string[]): Promise<void> {
  let blueprints: Array<Blueprint> = []
  for (let i=0; i<input.length; i++) {
    let row = input[i]
    let s = row.split(":")[1].split(".").map(x => x.trim().split(" "))

    blueprints.push({
      id: i+1,
      ore: parseInt(s[0][4]),
      clay: parseInt(s[1][4]),
      obsidian: [parseInt(s[2][4]), parseInt(s[2][7])],
      geode: [parseInt(s[3][4]), parseInt(s[3][7])],
    })
  }

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

    if (time >= 24) {
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
  console.log(qSum)
}

async function part2(input: string[]): Promise<void> {
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null