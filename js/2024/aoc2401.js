const { NotImplemented, run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    const l1 = [];
    const l2 = [];
    inputs.forEach(x => {
        const [a, b] = x.split(" ").filter(x => x != '');
        l1.push(parseInt(a, 10));
        l2.push(parseInt(b, 10));
    });
    return [l1, l2]
}

function part1([l1, l2]) {
    l1.sort();
    l2.sort();
    let sum = 0;
    for(let i = 0; i < l1.length; i++) {
        sum += Math.abs(l1[i] - l2[i]);
    }
    return sum;
}

function part2([l1, l2]) {
    const map = new Map();
    l2.forEach(x => {
        map.set(x, (map.get(x) || 0) + 1);
    });
    
    let sum = 0;
    l1.forEach(x => sum += x*(map.get(x) || 0));
    return sum;
}

run(__filename, solve);