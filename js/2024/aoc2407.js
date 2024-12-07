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

function part1(parsed) {
    let sum = 0;

    for (const line of parsed) {
        // If any given line can be made into an equation,
        // add its total to the sum
        if(isValid(line)) {
            sum += line.res;
        }
    }
    return sum;
}

function part2(parsed) {
    return 0;
}

/**
 * Recursively combine each value with one of the various operators
 * and return the full set of permutations
 */
function isValid({res, vals}) {
    // No neex to expand the first value into all possible options - it will
    // always just be returned (specifically be added to the initial value of 0)
    const valsWithOps = [{op: '+', val: vals[0]}];

    // Get the full set of possible equations - permutations of equations combined with values
    const equations = expandOps(vals.slice(1), valsWithOps);
    for (const equation of equations) {
        const total = equation.reduce((prev, {val, op}) => {
            return op === '+' ? prev + val : prev * val;
        }, 0);
        if(total === res) {
            return true;
        }
    }
    return false;
}

function expandOps(vals, ops = []) {
    if(!vals.length) {
        return [ops];
    }
    const currVal = vals[0];
    return ['+', '*'].flatMap(op => {
        return expandOps(vals.slice(1), [...ops, {val: currVal, op}])
    });
}

run(__filename, solve);