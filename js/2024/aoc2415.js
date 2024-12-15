const { run } = require('aoc-copilot');

const dirs = {
    '^': [0, -1],
    'v': [0, 1],
    '<': [-1, 0],
    '>': [1, 0],
}

async function solve(inputs, partNum, isSample) {
    if(isSample) {
        inputs = `########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<`.split('\n');
    }
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    let [room, directions] = inputs.join('\n').split('\n\n');
    directions = directions.replaceAll('\n', '').split('').map(x => dirs[x]);
    room = room.split('\n');
    const map = new Map();
    const width = room[0].length;
    const height = room.length;
    let currPos;
    for (let x = 0; x < width; x++) {
        for (let y = 0; y < height; y++) {
            const currPt = room[y][x]
            switch(currPt) {
                case '.':
                    break;
                case '@':
                    currPos = [x, y];
                    // continue to next case
                default:
                    map.set(mapKey(x, y), currPt);

            }
        }
    }
    return {directions, map, width, height, currPos};
}

function part1({directions, map, width, height, currPos}) {
    const [finalMap, finalPos] = directions.reduce(([currMap, currPos], currDir) => {
        return move(currMap, currPos, currDir)
    }, [map, currPos])
    return [...finalMap.entries()].reduce((prev, [key, pt]) => {
        if(pt !== 'O') {
            return prev;
        }
        const [x, y] = parseMapKey(key);
        return prev + x + y*100;
    }, 0)
}

function part2(parsed) {
    return 0;
}

const mapKey = (x, y) => `${x},${y}`;
const parseMapKey = key => key.split(',').map(k => parseInt(k, 10));

function move(map, [currX, currY], [dx, dy]) {
    const newX = currX + dx;
    const newY = currY + dy;
    const currPt = map.get(mapKey(currX, currY));
    const newPt = map.get(mapKey(newX, newY));

    // If new pos is a wall, do nothing
    if(newPt === '#') {
        return [map, [currX, currY]];
    }

    // If new pos is a box, try to move it
    if(newPt) {
        const [newMap, [boxX, boxY]] = move(map, [newX, newY], [dx, dy]);
        // Nothing moved - do nothing
        if (boxX === newX && boxY === newY) {
            return [map, [currX, currY]];
        }
    }

    // Otherwise it's empty, move to it
    map.delete(mapKey(currX, currY));
    map.set(mapKey(newX, newY), currPt);
    return [map, [newX, newY]];
}

function renderMap(map, width, height) {
    for (let y = 0; y < height; y++) {
        const row = [];
        for (let x = 0; x < width; x++) {
            row.push(map.get(mapKey(x, y)) || '.');
        }
        console.log(row.join(''));
    }
}

run(__filename, solve);