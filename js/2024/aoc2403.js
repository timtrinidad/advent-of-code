const { NotImplemented, run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    const str = inputs.join('');
    const instructions = [...str.matchAll(/mul\((\d+),(\d+)\)/g)]
    return instructions.map(x => [parseInt(x[1], 10), parseInt(x[2], 10)]);
}

function part1(parsed) {
    return parsed.map(([a, b]) => a * b).reduce((prev, curr) => prev + curr);
}

function part2(parsed) {
    return 0;
}

run(__filename, solve);