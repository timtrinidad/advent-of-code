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
    console.log(parsed);
    return 0;
}

function part2(parsed) {
    return 0;
}

run(__filename, solve);