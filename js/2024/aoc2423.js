const { run } = require('aoc-copilot');
const { mapKey, parseMapKey, DIRS4, DIRS8 } = require('../utils');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    const groups = inputs.map(x => x.split('-'));

    const map = new Map();
    // Create a mapping of each node and its neighbors
    parsed.forEach(([a, b]) => {
        map.set(a, [...(map.get(a) || []), b]);
        map.set(b, [...(map.get(b) || []), a]);
    });

    return {map, groups}
}

function part1({map}) {
    // Find all loops of length 3
    const loops = findLoops(map);
    // Combine loops that are just shifted in order
    const uniqueLoops = new Set(loops.map(x => x.sort().join(',')));

    // Count the number of unique loops with a node that starts with 't'
    let count = 0;
    for (const loop of uniqueLoops) {
        if(loop.match(/t\w/)) {
            count++
        }
    }

    return count;
}

function part2({map, groups}) {
    const largest = findStronglyConnected(map, groups);
    return largest.sort().join(',')
}

function findLoops(map) {
    const keys = [...map.keys()];
    const queue = keys.map(x => [x]);

    // DFS to look for any loops
    const loops = [];
    let q;
    while(q = queue.shift()) {
        const tail = q[q.length - 1];
        const connections = map.get(tail);

        // Go through each of the neighbors of the last node in this chain
        connections.forEach(conn => {
            if(conn === q[0] && q.length === 3) {
                // If this neighbor matches the beginning of the chain and it's of length 3, keep track of it
                loops.push(q);
            } else if(q.length < 3 && !q.includes(conn)) {
                // If this is a new node in the chain AND it's not already too long,
                // add to chain and look for the next neighbor
                queue.unshift([...q, conn]);
            }
        })
    };
    return loops;
}

function findStronglyConnected(map, groups) {
    const queue = groups;

    let group;
    let lastGroup;
    const seen = new Set();
    // Go through each current set
    while(group = queue.shift()) {
        // Given all nodes in the group, find possible candidates
        // by finding common neighbors
        const candidates = group.reduce((prev, curr) => {
            return prev === undefined ? map.get(curr) : intersect(prev, map.get(curr));
        }, undefined);

        // For each candidate, add it to the group and find new candidates to add
        candidates.forEach(candidate => {
            const newGroup = [...group, candidate].sort();
            const newGroupKey = newGroup.join(',');
            // Ignore groups that have already been evaluated (but in a different order)
            if(!seen.has(newGroupKey)) {
                seen.add(newGroupKey)
                queue.push(newGroup);
            }
        });
        lastGroup = group;
    }

    return lastGroup;
}

const intersect = (a, b) => a.filter(x => b.includes(x));

run(__filename, solve);