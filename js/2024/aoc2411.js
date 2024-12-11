const { run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    if(isSample) {
        inputs = ['125 17']
    }

    const parsed = parse(inputs);

    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    return inputs[0].split(' ').map(x => parseInt(x, 10));
}

function part1(parsed) {
    let curr = parsed;
    for (let i = 0; i < 25; i++) {
        curr = blink(curr);
    }
    return curr.length;
}

function part2(parsed) {
    return 0;
}

function blink(stones) {
    return stones.flatMap(stone => {
        const stoneStr = stone.toString();
        if(stone === 0) { return [1]; };
        if(stoneStr.length % 2 === 0) { return [
            parseInt(stoneStr.substring(0, stoneStr.length/2), 10), 
            parseInt(stoneStr.substring(stoneStr.length/2), 10)
        ]; }
        return stone * 2024;
    })
}

run(__filename, solve);