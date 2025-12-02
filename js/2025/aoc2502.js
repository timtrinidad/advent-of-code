const { run } = require('aoc-copilot');
const { mapKey, parseMapKey, DIRS4, DIRS8 } = require('../utils');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    return inputs
        .join('')
        .replace('\n', '')
        .split(',')
        .map(x => x
            .split('-')
        )
}

function part1(parsed) {
    const invalidNums = [];
    parsed.forEach(([left, right]) => {
        const leftLength = Math.floor(left.length/2);
        const rightLength = Math.ceil(right.length/2);
        const min = leftLength === 0 ? 0 : left.substring(0, leftLength);
        const max = right.substring(0, rightLength);
        const leftInt = parseInt(left, 10);
        const rightInt = parseInt(right, 10);
        const minInt = parseInt(min, 10);
        const maxInt = parseInt(max, 10);

        for (let i = minInt; i <= maxInt; i++) {
            const toTest = parseInt(`${i}${i}`, 10);
            if(toTest >= leftInt && toTest <= rightInt) {
                invalidNums.push(toTest);
            }
        }

    })
    return invalidNums.reduce((prev, curr) => prev + curr);
}

function part2(parsed) {
    // parsed = [['95', '115']]
    const invalidNums = new Set();
    parsed.forEach(([left, right]) => {
        const leftLength = 1
        const rightLength = Math.ceil(right.length/2);
        const min = leftLength === 0 ? 0 : left.substring(0, leftLength);
        const max = right.substring(0, rightLength);
        const leftInt = parseInt(left, 10);
        const rightInt = parseInt(right, 10);
        const minInt = parseInt(min, 10);
        const maxInt = parseInt(max, 10);

        for (let i = 0; i <= maxInt; i++) {
            for (let times = 1; times <= 10; times++) {
                const toTest = parseInt(i.toString().repeat(times), 10);
                if(toTest > rightInt) {
                    break;
                }
                if(toTest >= leftInt && toTest <= rightInt) {
                    invalidNums.add(toTest);
                }
            }
        }

    })
    return Array.from(invalidNums).reduce((prev, curr) => prev + curr);
}

run(__filename, solve);