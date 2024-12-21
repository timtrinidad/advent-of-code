const { run } = require('aoc-copilot');
const { mapKey, parseMapKey, DIRS4, DIRS8 } = require('../utils');
const Graph = require('node-dijkstra');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    let start, end;
    const width = inputs[0].length;
    const height = inputs.length;
    const walls = [];
    const graph = new Graph();

    for (let x = 0; x < width; x++) {
        for (let y = 0; y < height; y++) {
            let pt = inputs[y][x];
            // Keep track of the start/end
            if(pt === "E") { start = [x, y]; pt = '.'; }
            if(pt === "S") { end = [x, y]; pt = '.'; }
            if(pt === '#') { walls.push([x, y]); }

            // Create a graph of points and their possible neighbors
            const neighbors = [];
            for (const [dx, dy] of DIRS4) {
                const x1 = x + dx;
                const y1 = y + dy;
                if(x1 < 0 || x1 >= width || y1 < 0 || y1 >= width) {
                    continue;
                }
                if(inputs[y1][x1] === '#') {
                    continue;
                }
                neighbors[mapKey(x1, y1)] = 1;
            }
            graph.addNode(mapKey(x, y), neighbors);
        }
    }
    return {width, height, start, end, walls, graph}
}

function part1({graph, start, end, walls}) {
    // Get the main ideal path
    const basePath = graph.path(mapKey(...start), mapKey(...end));

    // Go through each wall point
    const totalSaved = {};
    let numSavedGt100 = 0;
    walls.forEach(([x, y]) => {
        const right = basePath.indexOf(mapKey(x + 1, y));
        const left = basePath.indexOf(mapKey(x - 1, y));
        const top = basePath.indexOf(mapKey(x, y - 1));
        const bottom = basePath.indexOf(mapKey(x, y + 1));

        let saved;

        // If a given wall has a point left and right of the wall point,
        // subtract their index positions
        if(right !== -1 && left !== -1) {
            saved = Math.abs(left - right) - 2;
        }
        // Same, but above and below
        if(top !== -1 && bottom !== -1) {
            saved = Math.abs(top - bottom) - 2;
        }
        if(saved) {
            totalSaved[saved] = (totalSaved[saved] || 0) + 1;
        }
        if(saved >= 100) {
            numSavedGt100++;
        }
    })

    return numSavedGt100;
}

function part2({graph, start, end, walls}) {
    // Get the main ideal path
    const basePath = graph.path(mapKey(...start), mapKey(...end));
    const pathMap = new Map();
    basePath.forEach((pt, i) => {
        pathMap.set(pt, i);
    });

    // For each point on the path, look around for all points on the
    // path that are up to 20 picoseconds away
    let num = 0;
    let nums = {};
    basePath.forEach((pt, i) => {
        const [x, y] = parseMapKey(pt);
        const currPos = pathMap.get(pt);
        for (let dx = -20; dx <= 20; dx++) {
            for (let dy = -20; dy <= 20; dy++) {
                const newDist = Math.abs(dx) + Math.abs(dy);
                if(newDist > 20) {
                    continue;
                }
                const x1 = x + dx;
                const y1 = y + dy;
                const newPos = pathMap.get(mapKey(x1, y1));
                // This point is not on the path
                if(newPos === undefined) {
                    continue;
                }
                // This is a step backwards
                const oldDist = newPos - currPos;
                if(oldDist <= 0) {
                    continue;
                }
                
                // Compare the original distance to the new one with the cheat
                const diff = oldDist - newDist;

                if(diff > 0) {
                    nums[diff] = (nums[diff] || 0) + 1
                }
                if(diff >= 100) {
                    num++;
                }
            }
        }
    });

    console.log(nums);
    return num;
}

run(__filename, solve, {skipTests: true, testsOnly: false});