const { NotImplemented, run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    const width = inputs[0].length;
    const height = inputs.length;
    const map = new Map();
    let startPoint;
    for (let x = 0; x < width; x++) {
        for (let y = 0; y < height; y++) {
            const point = inputs[y][x];
            if (point === '^') {
                startPoint = [x, y];
            }
            map.set(mapKey(x, y), point);
        }
    }
    return {
        map,
        startPoint,
        width,
        height,
    }
}

function part1({map, startPoint, width, height}) {
    const visited = new Set();
    let [currX, currY] = startPoint;
    let dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]];
    while (currX >= 0 && currX < width && currY >= 0 && currY < height) {
        visited.add(mapKey(currX, currY));
        const currDir = dirs[0];
        const nextX = currX + currDir[0];
        const nextY = currY + currDir[1];
        // If next point is an obstacle, don't actually move, just change dirs
        if (map.get(mapKey(nextX, nextY)) === '#') {
            dirs = [...dirs.slice(1), currDir];
        } else {
            currX = nextX;
            currY = nextY;
        }
    }
    return visited.size;
}

function part2(parsed) {
    return 0;
}

const mapKey = (x, y) => `${x},${y}`;

run(__filename, solve);