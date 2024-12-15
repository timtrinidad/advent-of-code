const { run } = require('aoc-copilot');

const dirs = {
    '^': [0, -1],
    'v': [0, 1],
    '<': [-1, 0],
    '>': [1, 0],
}

async function solve(inputs, partNum, isSample) {
    if(isSample && partNum === 1) {
        inputs = `########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<`.split('\n');
    } else if(isSample && partNum === 2) {
        inputs = `#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^`.split('\n');
        inputs = `##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^`.split('\n');
    }
    const parsed = parse(inputs, partNum);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs, partNum) {
    let [room, directions] = inputs.join('\n').split('\n\n');
    directions = directions.replaceAll('\n', '').split('').map(x => dirs[x]);
    room = room.split('\n');
    const map = new Map();
    const width = room[0].length * partNum;
    const height = room.length;
    let currPos;
    const xIncr = partNum === 1 ? 1 : 2;
    for (let x = 0; x < width; x += xIncr) {
        for (let y = 0; y < height; y++) {
            const currPt = room[y][x / xIncr]
            switch(currPt) {
                case '.':
                    break;
                case '@':
                    currPos = [x, y];
                    map.set(mapKey(x, y), currPt);
                    break;
                case 'O':
                    if(partNum === 1) {
                        map.set(mapKey(x, y), currPt);
                    } else {
                        map.set(mapKey(x, y), '[');
                        map.set(mapKey(x+1, y), ']');
                    }
                    break;
                case '#':
                    if(partNum === 1) {
                        map.set(mapKey(x, y), currPt);
                    } else {
                        map.set(mapKey(x, y), '#');
                        map.set(mapKey(x+1, y), '#');
                    }

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
        if(pt !== 'O' && pt !== '[') {
            return prev;
        }
        const [x, y] = parseMapKey(key);
        return prev + x + y*100;
    }, 0)
}

function part2(parsed) {
    // Do the same thing as Part 1 - but input will be different
    return part1(parsed);
}

const mapKey = (x, y) => `${x},${y}`;
const parseMapKey = key => key.split(',').map(k => parseInt(k, 10));

/**
 * Recursively move all items in the way. If unable to, return the existing point rather
 * than the new point.
 */
function move(map, [currX, currY], [dx, dy]) {
    const newX = currX + dx;
    const newY = currY + dy;
    const currPt = map.get(mapKey(currX, currY));
    const newPt = map.get(mapKey(newX, newY));

    // If new pos is a wall, do nothing
    if(newPt === '#') {
        return [map, [currX, currY]];
    }

    // If double box, check to see if other side of the box is hitting a wall as well
    if (currPt === '[' && map.get(mapKey(newX+1, newY)) === '#') {
        return [map, [currX, currY]];
    }
    if (currPt === ']' && map.get(mapKey(newX-1, newY)) === '#') {
        return [map, [currX, currY]];
    }

    // If new pos is a simple box or is a double box moving sideways, try to move it
    if(newPt === 'O' || (dy == 0 && (newPt === '[' || newPt === ']'))) {
        const [newMap, [boxX, boxY]] = move(map, [newX, newY], [dx, dy]);
        // Nothing moved - do nothing
        if (boxX === newX && boxY === newY) {
            return [map, [currX, currY]];
        }
        map = newMap;
    }
    // If it's a double box, try to move both sides
    else if(newPt === '[' || newPt === ']') {
        let [newMap, [boxX, boxY]] = move(map, [newX, newY], [dx, dy]);
        // Nothing moved, do nothing
        if (boxX === newX && boxY === newY) {
            return [map, [currX, currY]];
        }

        // Otherwise, try to move the other part of the box
        const newX2 = newPt === '[' ? newX + 1 : newX - 1;
        [newMap, [boxX, boxY]] = move(newMap, [newX2, newY], [dx, dy]);
        // nothing moved, do nothing and ignore the previous move of the other side
        if(boxX === newX2 && boxY === newY) {
            return [map, [currX, currY]];
        }
        map = newMap;
    }

    // The new point is empty, move to it
    const updatedMap = new Map(map);
    updatedMap.delete(mapKey(currX, currY));
    updatedMap.set(mapKey(newX, newY), currPt);
    return [updatedMap, [newX, newY]];
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