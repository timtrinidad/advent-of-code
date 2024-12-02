const { NotImplemented, run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    return [];
}

function part1(parsed) {
    return 0;
}

function part2(parsed) {
    return 0;
}

run(__filename, solve);