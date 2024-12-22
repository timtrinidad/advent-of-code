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
        // Iterate through all possible L1 robot button presses to find the lowest total number of button presses
        for (let res of numRes) {

            // Create a list of button pairs and put them into a map, counting each pair
            let pairsMap = new Map();
            res.unshift('A');
            for (let i = 1; i < res.length; i++) {
                const pair = `${res[i-1]},${res[i]}`;
                pairsMap.set(pair, (pairsMap.get(pair) || 0) + 1);
            }

            let num = numDirRobots;
            while (num--) {
                const newPairsMap = new Map();
                
                // Similar to lanternfish - create a new pair map by expanding each pair based on the
                // optimal DIR_MOVES_MAP, incrementing by the number of times that pair was found
                // in the original map
                for (const [pair, pairCount] of pairsMap) {
                    const pattern = ['A', ...DIR_MOVES_MAP.get(pair)];
                    for (let i = 1; i < pattern.length; i++) {
                        const patternPair = `${pattern[i-1]},${pattern[i]}`;
                        newPairsMap.set(patternPair, (newPairsMap.get(patternPair) || 0) + pairCount );
                    }
                }

                pairsMap = newPairsMap;
            }

            // Count the number of total pairs
            const len = [...pairsMap.values()].reduce((prev, curr) => prev + curr);

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

run(__filename, solve, {skipTests: true}); 
