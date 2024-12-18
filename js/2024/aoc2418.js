const { run } = require('aoc-copilot');
const { mapKey, parseMapKey } = require('../utils');
const Graph = require('node-dijkstra');

const DIRS = [[0, 1], [0, -1], [1, 0], [-1, 0]];

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);

    return partNum === 1 ? part1(parsed, isSample) : part2(parsed, isSample);
}

function parse(inputs) {
    return inputs.map(x => x.split(',').map(y => parseInt(y, 10)));
}

function part1(parsed, isSample) {
    const limit = isSample ? 12 : 1024;
    const set = new Set(parsed.map(x => mapKey(...x)).splice(0, limit));

    const width = isSample ? 6 + 1 : 70 + 1;
    const graph = new Graph();
    for (let x = 0; x < width; x++) {
        for (let y = 0; y < width; y++) {
            const neighbors = [];
            for (const [dx, dy] of DIRS) {
                const x1 = x + dx;
                const y1 = y + dy;
                if(x1 < 0 || x1 >= width || y1 < 0 || y1 >= width) {
                    continue;
                }
                if(set.has(mapKey(x1, y1))) {
                    continue;
                }
                neighbors[mapKey(x1, y1)] = 1;
            }
            graph.addNode(mapKey(x, y), neighbors);
        }
    }

    return graph.path(mapKey(0, 0), mapKey(width - 1, width - 1)).length - 1;
}

function part2(parsed, isSample) {
    const corrupted = parsed.map(x => mapKey(...x));

    const width = isSample ? 6 + 1 : 70 + 1;
    const graph = new Graph();
    for (let x = 0; x < width; x++) {
        for (let y = 0; y < width; y++) {
            const neighbors = [];
            for (const [dx, dy] of DIRS) {
                const x1 = x + dx;
                const y1 = y + dy;
                if(x1 < 0 || x1 >= width || y1 < 0 || y1 >= width) {
                    continue;
                }
                neighbors[mapKey(x1, y1)] = 1;
            }
            graph.addNode(mapKey(x, y), neighbors);
        }
    }

    const avoid = [];
    let start = 0;
    let end = corrupted.length - 1;
    while (start < end) {
        const mid = Math.floor((start + end) / 2);
        const hasPath = graph.path(mapKey(0, 0), mapKey(width - 1, width - 1), {
            avoid: corrupted.slice(0, mid)
        });
        if(hasPath) {
            start = mid + 1
        } else {
            end = mid;
        }
    }
    return corrupted[start-1];
}

run(__filename, solve);