const { NotImplemented, run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

/**
 * Convert the full string into a map keyed by x,y for quick lookup
 */
function parse(inputs) {
    const chars = inputs.map(x => x.split(''));
    const map = new Map();
    for (let y = 0; y < chars.length; y++) {
        for (let x = 0; x < chars[0].length; x++) {
            map.set(`${x},${y}`, chars[y][x]);
        }
    }
    const width = chars[0].length;
    const height = chars.length;
    return [width, height, map];
}

function part1([width, height, map]) {
    let count = 0;

    // For each point, go in a specific direction traversing recursively
    const dirs = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]
    for(let x = 0; x < width; x++) {
        for (let y = 0; y < height; y++) {
            dirs.forEach(([dx, dy]) => {
                count += wordSearch([x, y], [dx, dy], [width, height], map, getMapValue(map, x, y)) || 0;
            });
        }
    }

    return count;
}

function part2(parsed) {
    return 0;
}

/**
 * Look for 'XMAS' given a point and a direction
 */
function wordSearch([currX, currY], [dx, dy], [width, height], map, str) {
    // Base case
    if(str == 'XMAS') {
        return 1;
    }

    const [nextX, nextY] = [currX + dx, currY + dy];
    // Out of bounds
    if(nextX < 0 || nextY < 0 || nextX >= width || nextY >= height) {
        return 0;
    }

    let nextChar;
    switch(str) {
        case 'X':
            nextChar = 'M';
            break;
        case 'XM':
            nextChar = 'A';
            break;
        case 'XMA':
            nextChar = 'S';
            break;
        default:
            break;
    }
    // Next character is not expected
    if(!nextChar || getMapValue(map, nextX, nextY) !== nextChar) {
        return 0;
    }

    // Recurse
    return wordSearch([nextX, nextY], [dx, dy], [width, height], map, str + nextChar);
}

function getMapValue(map, x, y) {
    return map.get(`${x},${y}`);
}

run(__filename, solve);