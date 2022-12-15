import * as fs from 'fs';

function read(file: string): Promise<string> {
    return new Promise((resolve, reject) => {
        fs.readFile(file, 'utf8', function (err, data) {
            if (err) {
                return reject(err);
            }
            resolve(data);
        });
    });
}

async function readLines(file: string): Promise<string[]> {
    return (await read(file)).trim().split("\n")
}

function ints(row: string): Array<number> {
    let f = row.replace(/[^-0-9]/g, ' ')
    let r = f.split(" ").filter(s => s != "").filter(s => s != "-").map(x => parseInt(x))
    return r
}

export {
    read,
    readLines,
    ints
}