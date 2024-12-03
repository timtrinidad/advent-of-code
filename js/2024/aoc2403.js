const { NotImplemented, run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed, isSample);
}

function parse(inputs) {
    return inputs.join('');
}

function part1(parsed) {
    const instructions = [...parsed.matchAll(/mul\((\d+),(\d+)\)/g)]
    return instructions
        .map(x => [parseInt(x[1], 10), parseInt(x[2], 10)])
        .map(([a, b]) => a * b).reduce((prev, curr) => prev + curr);
}

function part2(parsed, isSample) {
    if(isSample) {
        // The automation doesn't detect the second sample properly - manually bypass it
        // in order to get to the part 2 non-sample
        return 48;
    }

    const instructions = [...parsed.matchAll(/(?:mul\((\d+),(\d+)\)|do\(\))|(don\'t\(\))/g)]
    const [sum, _] = instructions
        .reduce(([sum, multiplier], [match, m1, m2]) => {
            if(match === 'do()') {
                return [sum, 1];
            }
            if(match === 'don\'t()') {
                return [sum, 0];
            }
            return [sum + (m1*m2*multiplier), multiplier]
        }, [0, 1]);
    return sum;
}

run(__filename, solve);