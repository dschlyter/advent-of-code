import { readLines } from './util'
import * as _ from 'lodash';

async function main(): Promise<void> {
  const input = await readLines("input/day7.txt")

  let root = await part1(input)
  await part2(root)
}

type Dir = {
    dirs: {[name: string]: Dir},
    files: Array<number>,
    populated: boolean
}

function size(dir: Dir): number {
    let files: number = dir.files.reduce((a,b) => a+b, 0)
    let subDirs = Object.values(dir.dirs).map(d => size(d)).reduce((a,b) => a+b, 0)
    return files + subDirs
}

async function part1(input: string[]): Promise<Dir> {
    let stack: Array<Dir> = []
    let root: Dir = {dirs: {}, files: [], populated: false}
    let dir: Dir = root
    let i = 0

    while (i < input.length) {
        let cmd = input[i]
        i++;

        if (cmd == '$ cd /') {
            console.log("ROOT")
        } else if (cmd == '$ cd ..') {
            dir = stack.pop()!
        } else if (cmd.startsWith('$ cd ')) {
            let name = cmd.split(" ")[2]
            stack.push(dir)
            dir = dir.dirs[name]!
        } else if (cmd == '$ ls') {
            while (i < input.length && !input[i].startsWith('$')) {
                let res = input[i]
                i++;

                if (dir.populated) {
                    console.log("DUP LS")
                    continue
                }

                if (res.startsWith("dir")) {
                    let name: string = res.split(" ")[1]
                    dir.dirs[name] = {dirs: {}, files: [], populated: false}
                } else {
                    let size: number = parseInt(res.split(" ")[0])
                    let name: string = res.split(" ")[1]
                    dir.files.push(size)
                }
            }
            dir.populated = true
        } else {
            throw Error("Fail on cmd "+cmd)
        }
    }

    function sizeCrawl(dir: Dir) {
        let res = 0
        let s = size(dir)
        if (s <= 100000) {
            res = s
        }

        for (let d of Object.values(dir.dirs)) {
            res += sizeCrawl(d)
        }

        return res
    }
    console.log(sizeCrawl(root))

    return root
}

async function part2(root: Dir): Promise<void> {
    let total = 70000000
    let needed = 30000000
    let free = total - size(root)
    let minDelete = needed - free

    function sizeCrawl(dir: Dir, minDelete: number) {
        let res = 99999999999
        let s = size(dir)
        if (s >= minDelete) {
            res = s
        }

        for (let d of Object.values(dir.dirs)) {
            let s2 = sizeCrawl(d, minDelete)
            res = Math.min(res, s2)
        }

        return res
    }
    let r = sizeCrawl(root, minDelete)
    console.log(r)
}

main()

// Make sure this is detected as a module and not compiled in the global scope
export default null