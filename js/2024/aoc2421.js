const { run } = require('aoc-copilot');
const { mapKey, parseMapKey, DIRS4, DIRS8 } = require('../utils');

const NUM_KEYPAD = {
    7: [0,0],
    8: [1,0],
    9: [2,0],
    4: [0,1],
    5: [1,1],
    6: [2,1],
    1: [0,2],
    2: [1,2],
    3: [2,2],
    0: [1,3],
    A: [2,3],
}

const DIR_KEYPAD = {
    '^': [1,0],
    'A': [2,0],
    '<': [0,1],
    'v': [1,1],
    '>': [2,1],
}

/**
 * List of optimal moves when expanding from a dir robot to another dir robot.
 * Created by experimentation (trying each one until lowest number is found after a depth of 4)
 * but eliminating invalid options (ones that allow the robot to go over an invalid position)
 */
const DIR_MOVES_MAP = new Map(Object.entries({
    '^,^': [ 'A' ],
    '^,A' : [ '>', 'A' ],
    '^,<' : [ 'v', '<', 'A' ],
    '^,v' : [ 'v', 'A' ],
    '^,>' : [ 'v', '>', 'A' ],
    'A,^' : [ '<', 'A' ],
    'A,A' : [ 'A' ],
    'A,<' : [ 'v', '<', '<', 'A' ],
    'A,v' : [ '<', 'v', 'A' ],
    'A,>' : [ 'v', 'A' ],
    '<,^' : [ '>', '^', 'A' ],
    '<,A' : [ '>', '>', '^', 'A' ],
    '<,<' : [ 'A' ],
    '<,v' : [ '>', 'A' ],
    '<,>' : [ '>', '>', 'A' ],
    'v,^' : [ '^', 'A' ],
    'v,A' : [ '^', '>', 'A' ],
    'v,<' : [ '<', 'A' ],
    'v,v' : [ 'A' ],
    'v,>' : [ '>', 'A' ],
    '>,^' : [ '<', '^', 'A' ],
    '>,A' : [ '^', 'A' ],
    '>,<' : [ '<', '<', 'A' ],
    '>,v' : [ '<', 'A' ],
    '>,>' : [ 'A' ]
}));

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs, isSample);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs, isSample) {
    if (isSample) {
        inputs = ['029A', '980A', '179A', '456A', '379A'];
    }

    return inputs.map(x => x.split(''));
}

function part1(parsed, numDirRobots = 2) {
    let total = 0;
    for (const code of parsed) {
        // Returns all dir robot button presses (not necessarily the shortest)
        const numRes = numRobot(code);
        
        let minLen;
        // Iterate through all possible num robot button presses to find the lowest total number of button presses
        for (let res of numRes) {
            res.unshift('A');
            
            let len = 0;
            // Expand each pair of button moves all the way to a depth of 25 to get a count
            for (let i = 1; i < res.length; i++) {
                len += dirMove(res[i-1], res[i], 25);
            }

            // Keep track of the lowest total length for the given code
            if (minLen === undefined || len < minLen) {
                minLen = len;
            }
        }
        
        total += parseInt(code.join(''), 10) * minLen;
    }
    return total;
}

function part2(parsed) {
    return part1(parsed, 25);
}

/**
 * BFS to identify all possible paths for a number sequence
 */
function numRobot(buttons) {
    // Start from position 2,3 ('A')
    const queue = [[2, 3, buttons, []]];

    const options = [];
    let q;
    while(q = queue.shift()) {
        const [x, y, remainingBtns, pressed] = q;

        // No more buttons to press - return the list of directional buttons pressed up until this point
        if(!remainingBtns.length) {
            options.push(pressed);
            continue;
        }

        // Determine the x and y moves needed to move from current position to the next position
        const [targetX, targetY] = NUM_KEYPAD[remainingBtns[0]];
        const dx = targetX - x;
        const dy = targetY - y;

        // Add to the queue all possible directional moves
        // Ensure all directional moves with both x and y are consecutive, i.e. vvv<<< or <<<vvv, but never v<v<v<
        // since that would require many more directional buttons for the next level
        if (dx) {
            // Don't queue a path that would be invalid = i.e. it would put the robot outside the keypad
            const wouldBeInvalid = x + dx === 0 && y === 3;
            if(!wouldBeInvalid){
                queue.push([x + dx, y, remainingBtns, [...pressed, ...Array(Math.abs(dx)).fill(dx > 0 ? '>' : '<')]]);
            }
        }
        if (dy) {
            // Don't queue a path that would be invalid = i.e. it would put the robot outside the keypad
            const wouldBeInvalid = x === 0 && y + dy === 3;
            if (!wouldBeInvalid) {
                queue.push([x, y + dy, remainingBtns, [...pressed, ...Array(Math.abs(dy)).fill(dy > 0 ? 'v' : '^')]]);
            }
        }
        // We're over the button we want to press, just queue 'A' and remove the button from the list of remaining buttons
        if (!dx && !dy) {
            queue.push([x, y, remainingBtns.slice(1), [...pressed, 'A']]);
        }
    }

    return options;
}

/**
 * Given a given source and destination button, return a count
 * of number of buttons at the given depth
 */
function dirMove(from, to, targetDepth, depth = 1, cache = new Map()) {
    // Memoize
    const cacheKey = `${from},${to},${depth}`;
    const cacheRes = cache.get(cacheKey);
    if(cacheRes !== undefined) {
        return cacheRes;
    }

    // Get optimal path for the given button pair
    const expanded = DIR_MOVES_MAP.get(`${from},${to}`);
    
    // If we're at the target depth, nothing left to expand. Return the number of moves
    // needed at this level from "from" to "to"
    if(depth === targetDepth) {
        cache.set(cacheKey, expanded.legnth);
        return expanded.length;
    }

    // Set the next robot to start from 'A'
    const dirs = ['A', ...expanded];

    // Recurse down to the requested depth for each button pair
    let sum = 0;
    for (let i = 1; i < dirs.length; i++) {
        sum += dirMove(dirs[i-1], dirs[i], targetDepth, depth + 1, cache);
    }

    cache.set(cacheKey, sum);
    return sum;
}

run(__filename, solve, {skipTests: true}); 
