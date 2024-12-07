const { NotImplemented, run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    return inputs.map(x => {
        // Parse out all numbers, store first one as the "total" and the rest as values
        const [res, ...vals] =
            [...x.matchAll(/(\d+)/g)]
            .map(y => parseInt(y[0], 10));
        return {res, vals}
    });
}

function part1(parsed, possibleOps = [multiply, add]) {
    let sum = 0;

    for (const {res, vals} of parsed) {
        // If any given line can be made into an equation,
        // add its total to the sum
        if(isValid(vals.slice(1), vals[0], res, possibleOps)) {
            sum += res;
        }
    }
    return sum;
}

function part2(parsed) {
    return part1(parsed, [multiply, add, combine])
}


/**
 * Determine, using DFS,if the result can be accomplished from the given values.
 */
function isValid(remainingValues, actual, expected, possibleOps) {
    if(!remainingValues.length) {
        return expected === actual;
    }
    return possibleOps.some(op => isValid(
        remainingValues.slice(1), // Recurse without the number we just used
        op(actual, remainingValues[0]), // Run the operation and update our accumulator
        expected, // Just pass along the exit condition
        possibleOps // Pass along our possible operations
    ));
}

const multiply = (a, b) => a*b;
const add = (a, b) => a+b;
const combine = (a, b) => parseInt(`${a}${b}`, 10);

run(__filename, solve);