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

export {
    read,
    readLines
}