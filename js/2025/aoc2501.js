const { run } = require('aoc-copilot');
const { mapKey, parseMapKey, DIRS4, DIRS8 } = require('../utils');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    return inputs;
}

function part1(parsed) {
    let i = 50;
    let numZeros = 0;
    parsed.map(x => (x[0] === "L" ? -1 : 1 ) * parseInt(x.substring(1), 10)).forEach(x => {
        i = (i + 100 + x) % 100;
        if (i === 0) {
            numZeros++;
        }
    });
    return numZeros;
}

function part2(parsed) {
    let i = 50;
    let numZeros = 0;
    parsed
        .map(x => [x[0] === 'L' ? -1 : 1, parseInt(x.substring(1), 10)])
        .forEach(([dir, x]) => {
            while (x !== 0) {
                i = (i + 100 + dir) % 100;
                if (i === 0) {
                    numZeros++;
                }
                x--;
            }
    });
    return numZeros;
}

run(__filename, solve);