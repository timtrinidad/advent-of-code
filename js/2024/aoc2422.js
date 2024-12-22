const { run } = require('aoc-copilot');
const { mapKey, parseMapKey, DIRS4, DIRS8 } = require('../utils');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    return inputs.map(x => parseInt(x, 10));
}

function part1(parsed) {
    const evolved = parsed.map(x => {
        let num = 2000;
        while (num--) {
            x = evolve(x);
        }
        return x;
    });
    return evolved.reduce((prev, curr) => prev + curr);
}

function part2(parsed) {
    return 0;
}

const mix = (secret, input) =>  Number(BigInt(secret) ^ BigInt(input));
const prune = secret => secret % 16777216;
const evolve = secret => {
    secret = prune(mix(secret, secret * 64));
    secret = prune(mix(secret, Math.floor(secret/32)))
    return prune(mix(secret, secret * 2048))
}

run(__filename, solve);