const { NotImplemented, run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    return inputs.map(x => x.split(' ').map(y => parseInt(y, 10)));
}

function part1(parsed) {
    return parsed.map(isSafe).filter(x => x).length;
}

function part2(parsed) {
    return parsed.map(x => {
        if(isSafe(x)) {
            return true;
        }
        for (let i = 0; i < x.length; i++) {
            if(isSafe(x.filter((_, idx) => i !== idx))) {
                return true;
            }
        }
        return false;
    }).filter(x => x).length;
}

function isSafe(x) {
    console.log(x);
    const increasing = x.every((curr, i, arr) => i === 0 || curr > arr[i-1]);
    const decreasing = x.every((curr, i, arr) => i === 0 || curr < arr[i-1]);
    const noGaps = x.every((curr, i, arr) => i === 0 || Math.abs(curr - arr[i-1]) <= 3);
    return (increasing || decreasing) && noGaps;
}

run(__filename, solve);