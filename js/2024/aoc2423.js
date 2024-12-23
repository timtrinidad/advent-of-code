const { run } = require('aoc-copilot');
const { mapKey, parseMapKey, DIRS4, DIRS8 } = require('../utils');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    return inputs.map(x => x.split('-'))
}

function part1(parsed) {
    const map = new Map();
    parsed.forEach(([a, b]) => {
        map.set(a, [...(map.get(a) || []), b]);
        map.set(b, [...(map.get(b) || []), a]);
    });

    const loops = findLoops(map);
    const uniqueLoops = new Set(loops.map(x => x.sort().join(',')));

    let count = 0;
    for (const loop of uniqueLoops) {
        if(loop.match(/t\w/)) {
            count++
        }
    }

    return count;
}

function part2(parsed) {
    return 0;
}

function findLoops(map) {
    const keys = [...map.keys()];
    const queue = keys.map(x => [x]);

    const loops = [];
    let q;
    while(q = queue.shift()) {
        const tail = q[q.length - 1];
        const connections = map.get(tail);

        connections.forEach(conn => {
            if(conn === q[0] && q.length === 3) {
                loops.push(q);
            } else if(q.length < 3 && !q.includes(conn)) {
                queue.unshift([...q, conn]);
            }
        })
    };
    return loops;
}

run(__filename, solve);