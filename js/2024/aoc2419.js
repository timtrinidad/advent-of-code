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

function part2({patterns, designs}) {
    let i = 0;
    // Count the number of possible arrangements per design and sum the values
    return designs.reduce((prev, x) => {
        const count = countPossible(patterns, x, new Map());
        return prev + count;
    }, 0)
}

function isPossible(patterns, design) {
    if(!design.length) {
        return true;
    }
    // Check to see which patterns could work for the beginning of this string
    const usablePattens = patterns.filter(pattern => {
        return pattern === design.slice(0, pattern.length);
    });
    if(!usablePattens.length) {
        return false;
    }
    // For each of the current patterns, recursively check to see if the remaining string is possible
    // Return immediately as soon as one is found.
    return usablePattens.some(pattern => isPossible(patterns, design.slice(pattern.length)));
}

function countPossible(patterns, design, cache) {
    // Memoize in case we've seen this specific substring before
    // and know whether or not it's already possible or not possible
    const cacheEntry = cache.get(design);
    if(cacheEntry !== undefined) {
        return cacheEntry;
    }

    // If the design string is empty, we've matched the whole thing.
    // Count this as a matched pattern
    if(!design.length) {
        return 1;
    }

    const usablePattens = patterns.filter(pattern => {
        return pattern === design.slice(0, pattern.length);
    });
    if(!usablePattens.length) {
        return 0;
    }
    // For each of the possible patterns, recurse with the remaiing substring
    // and sum the total number of possibilities
    const res = usablePattens.reduce((sum, pattern) => sum + countPossible(patterns, design.slice(pattern.length), cache), 0);
    cache.set(design, res);
    return res;
}

run(__filename, solve, {skipTests: true, partNum: 2});