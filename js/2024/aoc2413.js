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
    const res = games.map(x => findPrizeCost(x, 0, 0, new Map()));
    return res.reduce((prev, curr) => prev + curr);
}

function part2(parsed) {
    return 0;
}

function findPrizeCost(game, numA, numB, cache) {
    const cacheKey = `${numA},${numB}`;
    const c = cache.get(cacheKey);
    if(c !== undefined) {
        return c;
    }

    const currX = game.ax * numA + game.bx * numB;
    const currY = game.ay * numA + game.by * numB;
    let res;
    if(currX == game.px && currY == game.py) {
        res = numA * 3 + numB;
    }
    else if(currX > game.px || currY > game.py) {
        res = null;
    } else {
        const g1 = numA < 100 ? findPrizeCost(game, numA + 1, numB, cache) : null;
        const g2 = numB < 100 ? findPrizeCost(game, numA, numB + 1, cache) : null;
        
        if (!g2) {
            res = g1;
        }
        else if(!g1) {
            res = g2;
        } else {
            res = g1 < g2 ? g1 : g2;
        }
    }
    cache.set(cacheKey, res);
    return res;
}

run(__filename, solve);