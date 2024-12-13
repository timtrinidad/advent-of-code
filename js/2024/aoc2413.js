const { run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    const str = inputs.join(" ");
    const ints = [...str.matchAll(/(\d+)/g)].map(x => parseInt(x[0], 10));
    const games = [];
    while(ints.length) {
        const [ ax, ay, bx, by, px, py ] = ints.splice(0, 6);
        games.push({ax, ay, bx, by, px, py});
    }
    

    return games;
}

function part1(games) {
    const res = games.map(x => getGamePrice(x));
    return res.reduce((prev, curr) => prev + curr);
}

function part2(games) {
    const res = games.map(x => getGamePrice({...x, px: x.px + 10000000000000, py: x.py + 10000000000000}));
    return res.reduce((prev, curr) => prev + curr);
}

function getGamePrice({ax, ay, bx, by, px, py}) {
    const b = (ax * py - px*ay) / (ax * by - bx*ay);
    const a = (px - b*bx)/ax;
   
    if(a % 1 < .00000001 && b % 1 < .00000001) {
        return a * 3 + b;
    }
    return 0;
}

run(__filename, solve);