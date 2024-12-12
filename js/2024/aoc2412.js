const { run } = require('aoc-copilot');

const DIRS = [[0,1], [0,-1], [1,0], [-1,0]];

async function solve(inputs, partNum, isSample) {
    if(isSample && partNum === 1) {
inputs = `RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE`.split("\n");
    }

    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    const map = new Map();
    const width = inputs[0].length;
    const height = inputs.length;
    for(let x = 0; x < width; x++) {
        for (let y = 0; y < height; y++) {
            map.set(mapKey(x, y), inputs[y][x]);
        }
    }
    return {map, width, height};
}

function part1({map, width, height}) {
    // const types = new Set([...map.values()]);
    // const prices = [...types.values()].map(x => getPrice(map, width, height, x));
    // return prices.reduce((prev, curr) => prev + curr);
    const regions = getRegions(map, width, height);
    return regions
        .map(region => region.size * getPerimeter(map, region))
        .reduce((prev, curr) => prev + curr)
}

function part2(parsed) {
    return 0;
}

const mapKey = (x, y) => `${x},${y}`;
const parseMapKey = key => key.split(',').map(x => parseInt(x, 10));


function getRegions(map, width, height) {
    const regions = [];
    const seen = new Set();

    for (let x = 0; x < width; x++) {
        for (let y = 0; y < height; y++) {
            if(seen.has(mapKey(x, y))) {
                continue;
            }
            const region = floodFill(map, x, y, map.get(mapKey(x, y)));
            for (const xy of region) {
                seen.add(xy);
            }
            regions.push(region);
        }
    }
    return regions;
}

function floodFill(map, x, y, type) {
    const region = new Set();
    const seen = new Set();
    const queue = [[x, y]];
    while(queue.length) {
        const [currX, currY] = queue.shift();
        const currMapKey = mapKey(currX, currY);
        if (seen.has(currMapKey)) {
            continue;
        }
        seen.add(currMapKey);
        if(map.get(currMapKey) === type) {
            region.add(currMapKey);
            DIRS.forEach(([dx,dy]) => queue.push([currX + dx, currY + dy]));
        }
    }
    return region;
}

function getPerimeter(map, region) {
    let perimiter = 0;
    const regionPoints = [...region.values()].map(parseMapKey);
    const type = map.get(mapKey(regionPoints[0][0], regionPoints[0][1]));
    for(const [x, y] of regionPoints) {
        for (const [dx, dy] of DIRS) {
            if(map.get(mapKey(x + dx, y + dy)) !== type) {
                perimiter++;
            }
        }
    }
    return perimiter;
}

run(__filename, solve);