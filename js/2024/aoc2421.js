const { run } = require('aoc-copilot');
const { mapKey, parseMapKey, DIRS4, DIRS8 } = require('../utils');

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

function part1(parsed) {
    return parsed.map(btns => {
        const res1 = numRobot(btns);
        const res2 = res1.flatMap(i => dirRobot(i));
        const res3 = res2.flatMap(i => dirRobot(i));
        const len = Math.min(...res3.map(x => x.length));


        const num = parseInt(btns.join(''));
        console.log(len, num);
        return len * num
    }).reduce((prev, curr) => prev + curr)
    
}

function part2(parsed) {
    return 0;
}

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

function numRobot(buttons, first = false) {
    const queue = [[2, 3, buttons, []]];

    const shortests = [];
    let shortestLength;
    let q;
    while(q = queue.shift()) {
        const [x, y, remainingBtns, pressed] = q;

        if(x === 0 && y === 3) {
            continue;
        }

        if(!remainingBtns.length) {
            if(!shortestLength) {
                shortestLength = pressed.length;
            }
            
            if(pressed.length == shortestLength) {
                shortests.push(pressed);
                if(first) {
                    return shortests;
                }
            }
            continue;
        }

        const [targetX, targetY] = NUM_KEYPAD[remainingBtns[0]];
        const dx = targetX - x;
        const dy = targetY - y;

        if (dx) {
            queue.push([x + dx, y, remainingBtns, [...pressed, ...Array(Math.abs(dx)).fill(dx > 0 ? '>' : '<')]]);
        }
        if (dy) {
            queue.push([x, y + dy, remainingBtns, [...pressed, ...Array(Math.abs(dy)).fill(dy > 0 ? 'v' : '^')]]);
        }
        if (!dx && !dy) {
            queue.push([x, y, remainingBtns.slice(1), [...pressed, 'A']]);
        }

    }

    return shortests;
}

function dirRobot(buttons, first = false) {
    const queue = [[2, 0, buttons, []]];

    const shortests = [];
    let shortestLength;
    let q;
    while(q = queue.shift()) {
        const [x, y, remainingBtns, pressed] = q;

        if(x === 0 && y === 0) {
            continue;
        }

        if(!remainingBtns.length) {
            if(!shortestLength) {
                shortestLength = pressed.length;
            }
            
            if(pressed.length == shortestLength) {
                shortests.push(pressed);
                if(first) {
                    return shortests;
                }
            }
            continue;
        }

        const [targetX, targetY] = DIR_KEYPAD[remainingBtns[0]];
        const dx = targetX - x;
        const dy = targetY - y;

        if (dx) {
            queue.push([x + dx, y, remainingBtns, [...pressed, ...Array(Math.abs(dx)).fill(dx > 0 ? '>' : '<')]]);
        }
        if (dy) {
            queue.push([x, y + dy, remainingBtns, [...pressed, ...Array(Math.abs(dy)).fill(dy > 0 ? 'v' : '^')]]);
        }
        if (!dx && !dy) {
            queue.push([x, y, remainingBtns.slice(1), [...pressed, 'A']]);
        }

    }

    return shortests;
}

run(__filename, solve); 