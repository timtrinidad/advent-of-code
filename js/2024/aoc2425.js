const { run } = require('aoc-copilot');
const { mapKey, parseMapKey, DIRS4, DIRS8 } = require('../utils');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

/**
 * Convert each scheme into a list of heights, categorized by locks or keys
 */
function parse(inputs) {
    const locks = [];
    const keys = [];

    for (let scheme of inputs.join('\n').split('\n\n')) {
        scheme = scheme.split('\n');
        const heights = [];
        for (let x = 0; x < scheme[0].length; x++) {
            let numInCol = 0;
            for (let y = 0; y < scheme.length; y++) {
                if(scheme[y][x] === '#') {
                    numInCol++;
                }
            }
            heights.push(numInCol);
        }
        if(scheme[0][0] === '#') {
            locks.push(heights);
        } else {
            keys.push(heights);
        }
    }

    return {locks, keys}
}

function part1({locks, keys}) {
    let count = 0;
    // Go through each lock and find the number of keys that fit
    locks.forEach(lock => {
        const matches = keys.filter(key => {
            // Match if total number of heights (lock + key) is less than 7
            return key.every((ky, i) => ky + lock[i] <= 7)
        });
        count += matches.length;
    })
    return count;
}

function part2(parsed) {
    return 0;
}

run(__filename, solve);