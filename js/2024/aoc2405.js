const { NotImplemented, run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    const inputStr = inputs.join("\n");
    let [rules, lines] = inputStr.split("\n\n").map(x => x.split("\n"));
    lines = lines.map(x => x.split(",").map(y => parseInt(y, 10)));
    
    return [rules, lines]
}

function part1([rules, lines]) {
    const validLines = filterLines(lines, new Set(rules), true)

    // Get the mids and sum them
    const mids = validLines.map(x => x[Math.floor(x.length/2)]);
    return mids.reduce((prev, next) => prev + next);
}

function part2([rules, lines]) {
    const ruleSet = new Set(rules);
    const invalidLines = filterLines(lines, ruleSet, false);
    const correctedLines = invalidLines.map(line => fixLine(line, [], ruleSet));
    
    const mids = correctedLines.map(x => x[Math.floor(x.length/2)]);
    return mids.reduce((prev, next) => prev + next);
}

/**
 * Get only lines that are valid based on the rules
 */
function filterLines(lines, rules, returnValid) {
    return lines.filter(line => returnValid ? isLineValid(line, rules) : !isLineValid(line, rules));
}

function isLineValid(line, rules) {
    for(let i = 0; i < line.length - 1; i++) {
        for(let j = i + 1; j < line.length; j++) {
            // Filter out any line that contains a pair whose opposite
            // order appears in the rules
            if(rules.has(`${line[j]}|${line[i]}`)) {
                return false;
            }
        }
    }
    return true;
}

/**
 * Reorder the numbers on a line based on the given rules
 */
function fixLine(remainingNums, existingLine, rules) {
    if(!remainingNums.length) {
        return existingLine;
    }

    // Look for which number doesn't appear in the rules on the right for any of the remaining numbers on the left
    // That's our next number
    const firstNum = remainingNums.find(x => {
        return remainingNums.every(y => {
            return x === y || !rules.has(`${y}|${x}`);
        });
    });

    existingLine.push(firstNum);
    // Recurse to find the remaining numbers
    return fixLine(remainingNums.filter(x => x !== firstNum), existingLine, rules);
}

run(__filename, solve, {skipTests: false});