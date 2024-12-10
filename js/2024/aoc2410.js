const { run } = require('aoc-copilot');

const DIRS = [[1, 0], [0, 1], [-1, 0], [0, -1]];

async function solve(inputs, partNum, isSample) {
    
    // overwrite sample
    if(isSample) {
        inputs = 
`89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732`.split('\n');
    }
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    const trailHeads = [];
    const map = new Map();
    for (let x = 0; x < inputs[0].length; x++) {
        for (let y = 0; y < inputs.length; y++) {
            const elev = parseInt(inputs[y][x], 10);
            map.set(mapKey(x, y), elev);
            if(elev === 0) {
                trailHeads.push([x, y]);
            }
        }
    }
    return {map, trailHeads};
}

function part1({map, trailHeads}) {
    let sum = 0;
    for (const [x, y] of trailHeads) {
        const peaks = traverse(map, x, y);
        sum += new Set(peaks.map(mapKey)).size;
    };
    return sum;
}

function part2({map, trailHeads}) {
    let sum = 0;
    for (const [x, y] of trailHeads) {
        const peaks = traverse(map, x, y);
        sum += peaks.length;
    };
    return sum;
}

function traverse(map, currX, currY, path = []) {
    const currElev = map.get(mapKey(currX, currY));
    if (currElev === 9) {
        return [[currX, currY]];
    }

    return DIRS.flatMap(([dx, dy]) => {
        const nextX = currX + dx;
        const nextY = currY + dy;
        
        const nextElev = map.get(mapKey(nextX, nextY));
        if(nextElev - currElev !== 1) {
            return [];
        }
        return traverse(map, nextX, nextY, [...path, [nextX, nextY, nextElev]]);
    });
}

const mapKey = (x, y) => Array.isArray(x) ? `${x[0]},${x[1]}` : `${x},${y}`;

run(__filename, solve);