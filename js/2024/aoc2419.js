const { run } = require('aoc-copilot');
const { mapKey, parseMapKey } = require('../utils');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    const [patterns, designs] = inputs.join('\n').split('\n\n');
    return {
        patterns: patterns.split(', '),
        designs: designs.split('\n'),
    };
}

function part1({patterns, designs}) {
    return designs.filter(x => isPossible(patterns, x)).length;
}

function part2(parsed) {
    return 0;
}

function isPossible(patterns, design) {
    if(!design.length) {
        return true;
    }
    const usablePattens = patterns.filter(pattern => {
        return pattern === design.slice(0, pattern.length);
    });
    if(!usablePattens.length) {
        return false;
    }
    return usablePattens.some(pattern => isPossible(patterns, design.slice(pattern.length)));
}

run(__filename, solve);