const { NotImplemented, run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    const inputStr = inputs.join("\n");
    let [rules, pages] = inputStr.split("\n\n").map(x => x.split("\n"));
    rules = new Set(rules);
    pages = pages.map(x => x.split(",").map(y => parseInt(y, 10)));
    
    return [rules, pages]
}

function part1([rules, pages]) {
    // Go through each page line
    const validPages = pages.filter(pageLine => {
        for(let i = 0; i < pageLine.length - 1; i++) {
            for(let j = i + 1; j < pageLine.length; j++) {
                // Filter out any line that contains a pair whose opposite
                // order appears in the rules
                if(rules.has(`${pageLine[j]}|${pageLine[i]}`)) {
                    return false;
                }
            }
        }
        return true;
    });

    // Get the mids and sum them
    const mids = validPages.map(x => x[Math.floor(x.length/2)]);
    return mids.reduce((prev, next) => prev + next);
}

function part2(parsed) {
    return 0;
}

run(__filename, solve);