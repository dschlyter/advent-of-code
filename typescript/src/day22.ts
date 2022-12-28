import { readLines, ints } from './util'
import * as _ from 'lodash';
import { times } from 'lodash';

// Note: Run this with --stack-size=1000000 for the DFS

const inputFile = "input/day22.txt"

async function main(): Promise<void> {
  const input = await readLines(inputFile)

  await part1(input)
  await part2(input)
}

const facing = [[0, 1], [1, 0], [0, -1], [-1, 0]]

async function part1(input: string[]): Promise<void> {
  let map = input.slice(0, input.length-2)
  let pathInput = input[input.length-1]

  // Patch up undefined segments - booo bad puzzle design!
  let width = map.map(row => row.length).reduce((a,b) => Math.max(a, b))
  for (let i=0; i<map.length; i++) {
    if (map[i].length < width) {
      map[i] = map[i].padEnd(width, " ")
    }
  }

  let path = pathInput.replace(/R/g, " R ").replace(/L/g, " L ").split(" ").filter(s => s != " ")
  let dir = 0
  let y = 0, x = 0
  while (map[y][x] == " ") {
    x += 1
  }

  for (let p of path) {
    if (p == "R") {
      dir = (dir + 1) % 4
      console.log("R")
    } else if (p == "L") {
      dir = (dir + 3) % 4
      console.log("L")
    } else {
      let move = parseInt(p)
      while (move > 0) {
        let ny = y, nx = x

        do {
          ny = (ny + facing[dir][0] + map.length) % map.length
          nx = (nx + facing[dir][1] + width) % map[y].length
        } while (map[ny][nx] == " ")

        if (map[ny][nx] == "#") {
          break
        }
        y = ny
        x = nx
        move -= 1
        if (map[ny][nx] != ".") {
          throw Error("Should always land on a . but was " + map[ny][nx])
        }
      }
      console.log(y ,x, dir)
    }
  }
  console.log(y, x, dir)
  let password = 1000 * (y+1) + 4 * (x+1) + dir
  console.log(password)
}

async function part2(input: string[]): Promise<void> {
  // Make a cube, rubiks color notation:
  // W
  // B O G R
  // Y

  // Cube transitions. Tracks which sides leads to which and how to rotate into the new coordinate system.
  const cubeTransitions = {
    "B": [["O", 0], ["Y", 0], ["R", 0], ["W", 0]],
    "O": [["G", 0], ["Y", 1], ["B", 0], ["W", 3]],
    "G": [["R", 0], ["Y", 2], ["O", 0], ["W", 2]],
    "R": [["B", 0], ["Y", 3], ["G", 0], ["W", 1]],
    "W": [["O", 1], ["B", 0], ["R", 3], ["G", 2]],
    "Y": [["O", 3], ["G", 2], ["R", 1], ["B", 0]],
  }

  const tileSize = inputFile.includes("test") ? 4 : 50

  function cubeStep(cubeSide: string, y: number, x: number, direction: number): [string, number, number, number] {
    y += facing[direction][0]
    x += facing[direction][1]

    if (!(y >= 0 && y < tileSize && x >= 0 && x < tileSize)) {
      y = (y + tileSize) % tileSize
      x = (x + tileSize) % tileSize
      let [newCubeSide, rotations] = cubeTransitions[cubeSide][direction]
      direction = (direction + rotations) % 4
      for (let i=0; i<rotations; i++) {
        let oldX = x
        x = tileSize - 1 - y
        y = oldX
      }
      cubeSide = newCubeSide
    }

    return [cubeSide, y, x, direction]
  }

  // Used this to find a bug, verify we can walk around the cube back to the original pos in all dirs
  for (let side of Object.keys(cubeTransitions)) {
    for (let dir of _.range(4)) {
      for (let y of _.range(tileSize)) {
        for (let x of _.range(tileSize)) {
          let [s, yp, xp, d] = [side, y, x, dir]
          let steps: Array<[string, number, number, number]> = []
          for (let step = 0; step < tileSize * 4; step++) {
            [s, yp, xp, d] = cubeStep(s, yp, xp, d)
            steps.push([s, yp, xp, d])
          }
          if (s != side || yp != y || xp != x || d != dir) {
            console.error("Fail", side, y, x, dir, steps)
            throw Error("fail")
          }
        }
      }
    }
  }

  let map = input.slice(0, input.length-2)
  let pathInput = input[input.length-1]

  // Find the first position, this is the blue side
  let gridY = 0, gridX = 0;
  for (let i=0; i<map[gridY].length; i++) {
    if (map[gridY][i] != " ") {
      gridX = i
      break
    }
  }

  let visited = {}
  let cube = {}
  let cubeToMap = {}

  // Traverse the cube and the grid at the same time, and copy the values
  function dfs(gridY: number, gridX: number, direction: number, cubeSide: string, cubeY: number, cubeX: number, cubeDirection: number) {
    // Abort if out of bounds or already visited
    if (gridY < 0 || gridY >= map.length || gridX < 0 || gridX >= map[gridY].length) {
      return
    }
    if (map[gridY][gridX] == " ") {
      return
    }
    let key = "" + gridY + "," + gridX
    if (visited[key]) {
      return
    }
    visited[key] = true

    // Update the cube
    cube[cubeSide] = cube[cubeSide] || []
    cube[cubeSide][cubeY] = cube[cubeSide][cubeY] || []
    cube[cubeSide][cubeY][cubeX] = map[gridY][gridX]

    // Track the cube pos -> map pos in order to be able to calc the password which uses the map coordinate
    cubeToMap[cubeSide] = cubeToMap[cubeSide] || []
    cubeToMap[cubeSide][cubeY] = cubeToMap[cubeSide][cubeY] || []
    cubeToMap[cubeSide][cubeY][cubeX] = [gridY, gridX, (4 + direction - cubeDirection) % 4]

    // Walk in all directions
    for (let rot=0; rot<4; rot++) {
      let newDir = (direction + rot) % 4
      let newGridY = gridY + facing[newDir][0]
      let newGridX = gridX + facing[newDir][1]

      let [newCubeSide, newCubeY, newCubeX, newCubeDirection] = cubeStep(cubeSide, cubeY, cubeX, (cubeDirection + rot) % 4)
      dfs(newGridY, newGridX, newDir, newCubeSide, newCubeY, newCubeX, newCubeDirection)
    }
  }
  dfs(gridY, gridX, 0, "B", 0, 0, 0)

  console.log(cube)

  let path = pathInput.replace(/R/g, " R ").replace(/L/g, " L ").split(" ").filter(s => s != " ")
  let dir = 0
  let y = 0, x = 0
  let side = "B"

  for (let p of path) {
    if (p == "R") {
      dir = (dir + 1) % 4
      console.log("R")
    } else if (p == "L") {
      dir = (dir + 3) % 4
      console.log("L")
    } else {
      let move = parseInt(p)
      while (move > 0) {
        let [newSide, ny, nx, nDir] = cubeStep(side, y, x, dir)

        if (cube[newSide][ny][nx] == "#") {
          break
        }

        side = newSide
        y = ny
        x = nx
        dir = nDir
        move -= 1
        if (cube[side][y][x] != ".") {
          throw Error("Should always land on a . but was " + cube[side][y][x])
        }
      }
      console.log(side, y, x, dir)
    }
  }
  console.log("after move", side, y, x, dir)
  let [finalGridY, finalGridX, finalRot] = cubeToMap[side][y][x]
  // TODO rotation somehow ??
  let password = 1000 * (finalGridY+1) + 4 * (finalGridX+1) + (dir + finalRot) % 4
  console.log(password)

  console.log(finalGridY, finalGridX, dir, finalRot)
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null