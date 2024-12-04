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

    // For each point, go in a specific direction and get the four characters
    const dirs = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]
    for(let x = 0; x < width; x++) {
        for (let y = 0; y < height; y++) {
            dirs.forEach(([dx, dy]) => {
                const search = [0, 1, 2, 3].map(m => getMapValue(map, x + dx * m, y + dy * m)).join('');
                if(search === 'XMAS') {
                    count++;
                }
            });
        }
    }

    return count;
}

function part2([width, height, map]) {
    let count = 0;
    for(let x = 0; x < width; x++) {
        for(let y = 0; y < height; y++) {
            const currChar = getMapValue(map, x, y);
            if(currChar === 'A') {
                const corners = [[-1, -1], [1, -1], [-1, 1], [1, 1]]
                    .map(([dx, dy]) => getMapValue(map, x + dx, y + dy))
                    .join('')
                // These cases are valid - MSSM or SMMS have both 
                // S or M opposite each other which is invalid
                if(['MMSS', 'SSMM', 'SMSM', 'MSMS'].includes(corners)) {
                    count++;
                }
            }
        }
    }
    return count;
}

function getMapValue(map, x, y) {
    return map.get(`${x},${y}`);
}

run(__filename, solve);