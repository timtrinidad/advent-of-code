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

function part1(parsed) {
    const visitedPoints = traverseMap(parsed, false);
    
    return visitedPoints.size;
}

function part2(parsed) {
    const { map, width, height } = parsed;
    
    let num = 0;
    // For every point, convert it to an obstacle and see if we ended up in a loop
    for (let x = 0; x < width; x++) {
        for (let y = 0; y < height; y++) {
            const currPoint = map.get(mapKey(x, y));
            if(currPoint === '^' || currPoint === '#') {
                continue;
            }

            map.set(mapKey(x, y), '#');
            try {
                traverseMap(parsed, true);
            } catch(e) {
                num++;
            }
            map.set(mapKey(x, y), '.');
        }
    }

    return num;
}

function traverseMap({map, startPoint, width, height}, detectLoops) {
    const visited = new Set();
    let [currX, currY] = startPoint;
    const dirs = [[0, -1], [1, 0], [0, 1], [-1, 0]];
    let dirIdx = 0;
    while (currX >= 0 && currX < width && currY >= 0 && currY < height) {
        const [currDirX, currDirY] = dirs[dirIdx];

        // We don't need the direction as part of the set unless we're trying to detect loops
        const setKey = `${currX},${currY}` + (detectLoops ? `,${currDirX},${currDirY}` : '');
        if(detectLoops && visited.has(setKey)) {
            // Loop detected - throw error
            throw new Error('Loop');
        }

        visited.add(setKey);
        const nextX = currX + currDirX;
        const nextY = currY + currDirY;
        if (map.get(mapKey(nextX, nextY)) === '#') {
            dirIdx = (dirIdx + 1) % 4;
        } else {
            // Otherwise, move
            currX = nextX;
            currY = nextY;
        }
    }
    return visited;
}

const mapKey = (x, y) => `${x},${y}`;

run(__filename, solve);