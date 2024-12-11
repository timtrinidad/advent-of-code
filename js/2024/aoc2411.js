const { run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    if(isSample) {
        if(partNum === 2) {
            return 0;
        }

        inputs = ['125 17'];
    }

    const parsed = parse(inputs);

    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    return new Map(inputs[0].split(' ').map(x => [parseInt(x, 10), 1]));
}

function part1(parsed, numIterations = 25) {
    let curr = parsed;
    for (let i = 0; i < numIterations; i++) {
        curr = blink(curr);
    }
    return [...curr.values()].reduce((prev, curr) => prev + curr)
}

function part2(parsed) {
    return part1(parsed, 75);

}

/**
 * Process all stones during a blink, making sure to only process each unique value once
 * (and keep track of the number of repeat values each time)
 */
function blink(stones) {
    const map = new Map();
    for (const [val, numVal] of stones) {
        for (const newVal of processStone(val)) {
            map.set(newVal, (map.get(newVal) || 0) + numVal)
        }
    }
    return map;   
}

function processStone(stone) {
    const stoneStr = stone.toString();
    if(stone === 0) { return [1]; };
    if(stoneStr.length % 2 === 0) { return [
        parseInt(stoneStr.substring(0, stoneStr.length/2), 10), 
        parseInt(stoneStr.substring(stoneStr.length/2), 10)
    ]; }
    return [stone * 2024];
}

run(__filename, solve);