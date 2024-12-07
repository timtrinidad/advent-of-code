const { NotImplemented, run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    return inputs.map(x => {
        // Parse out all numbers, store first one as the "total" and the rest as values
        const [res, ...vals] = [...x.matchAll(/(\d+)/g)].map(y => parseInt(y[0], 10));
        return {res, vals}
    });
}

function part1(parsed, possibleOps = ['+', '*']) {
    let sum = 0;

    for (const line of parsed) {
        // If any given line can be made into an equation,
        // add its total to the sum
        if(isValid(line, possibleOps)) {
            sum += line.res;
        }
    }
    return sum;
}

function part2(parsed) {
    return part1(parsed, ['+', '*', '|'])
}


/**
 * Determine, if based on the fully expanded set of possible equations,
 * if the result can be accomplished from the given values.
 */
function isValid({res, vals}, possibleOps) {
    // No need to expand the first value into all possible options - it will
    // always just be returned (specifically be added to the initial value of 0)
    const equation = [{op: '+', val: vals[0]}];

    // Get the full set of possible equations - permutations of equations combined with values
    const equations = expandOps(vals.slice(1), equation, possibleOps);

    // Use those operations to get an output number from the values
    // and return true if any of them match our result for the line
    for (const equation of equations) {
        const total = equation.reduce((prev, {val, op}) => {
            switch(op) {
                case '+': return prev + val;
                case '*': return prev * val;
                case '|': return parseInt(`${prev}${val}`, 10);
            }
            return op === '+' ? prev + val : prev * val;
        }, 0);
        if(total === res) {
            return true;
        }
    }
    return false;
}

/**
 * Recursively combine each value with one of the various operators
 * and return the full set of permutations
 */
function expandOps(vals, equation = [], possibleOps) {
    if(!vals.length) {
        return [equation];
    }
    const currVal = vals[0];
    return possibleOps.flatMap(op => {
        return expandOps(vals.slice(1), [...equation, {val: currVal, op}], possibleOps)
    });
}

run(__filename, solve);