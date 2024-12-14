const { run } = require('aoc-copilot');

const DIRS = [[0,1], [0,-1], [1,0], [-1,0]];

async function solve(inputs, partNum, isSample) {
    if(isSample) {
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
        .map(region => region.regionSet.size * getPerimeter(map, region))
        .reduce((prev, curr) => prev + curr)
}

function part2({map, width, height}) {
    const regions = getRegions(map, width, height);
    return regions
        .map(region => region.regionSet.size * getCorners(map, width, height, region))
        .reduce((prev, curr) => prev + curr)
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
            const regionList = [...region.values()].map(parseMapKey);
            regions.push({
                regionSet: region,
                regionList,
                regionType: map.get(mapKey(regionList[0][0], regionList[0][1])),
            });
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

function getPerimeter(map, {regionList, regionType}) {
    let perimiter = 0;
    for(const [x, y] of regionList) {
        for (const [dx, dy] of DIRS) {
            if(map.get(mapKey(x + dx, y + dy)) !== regionType) {
                perimiter++;
            }
        }
    }
    return perimiter;
}

function getCorners(map, width, height, {regionSet, regionType}) {
    let numCorners = 0;
    for (let x = -1; x < width; x++) {
        for (let y = -1; y < height; y++) {
            const cornerPts = [[0, 0], [1, 0], [1, 1], [0, 1]].map(([dx, dy]) => {
                return regionSet.has(mapKey(x + dx, y + dy)) ? '1' : '0'
            }).join('');
            switch (cornerPts) {
                case '0000':
                case '1100':
                case '0011':
                case '0110':
                case '1001':
                case '1111':
                    break;
                case '1000':
                case '0100':
                case '0010':
                case '0001':
                case '0111':
                case '1011':
                case '1101':
                case '1110':
                    numCorners++;
                    break;
                case '1010':
                case '0101':
                    numCorners += 2;
                    break;
                default:
                    throw new Error('Unknown: ' + cornerPts);
            }
        }
    }

    return numCorners

}

run(__filename, solve);