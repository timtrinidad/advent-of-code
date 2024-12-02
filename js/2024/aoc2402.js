const { NotImplemented, run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    return inputs.map(x => x.split(' ').map(y => parseInt(y, 10)));
}

function part1(parsed) {
    return parsed.map(x => {
        const increasing = x.every((curr, i, arr) => i === 0 || curr > arr[i-1]);
        const decreasing = x.every((curr, i, arr) => i === 0 || curr < arr[i-1]);
        const noGaps = x.every((curr, i, arr) => i === 0 || Math.abs(curr - arr[i-1]) <= 3);
        return (increasing || decreasing) && noGaps;
    }).filter(x => x).length;
}

function part2(parsed) {
    return 0;
}

run(__filename, solve);