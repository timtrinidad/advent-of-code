const { run } = require('aoc-copilot');
const { mapKey, parseMapKey, DIRS4, DIRS8 } = require('../utils');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs, partNum, isSample);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs, partNum, isSample) {
    if(partNum === 2 && isSample) {
        return [1, 2, 3, 2024]
    }

    return inputs.map(x => parseInt(x, 10));
}

function part1(parsed) {
    // Evolve each of the input numbers 2000 times and sum the results
    const evolved = parsed.map(x => {
        let num = 2000;
        while (num--) {
            x = evolve(x);
        }
        return x;
    });
    return evolved.reduce((prev, curr) => prev + curr);
}

function part2(parsed) {
    // Evolve each of the input number 2000 times,
    // keeping track of the price changes
    const evolved = parsed.map(x => {
        let num = 2000;
        let lastPrice = x % 10;
        let last4Changes = [];
        const sequences = new Map();
        while (num--) {
            x = evolve(x);
            const price = x % 10;
            last4Changes.push(price - lastPrice);
            if(last4Changes.length === 5) {
                last4Changes.shift();
                // Track the first time each sequence is seen and the price
                // at that sequence
                const seqKey = last4Changes.join(',');
                if(!sequences.has(seqKey)) {
                    sequences.set(seqKey, price);
                }
            }
            lastPrice = price;
        }
        return sequences;
    });
    
    // Combine all of the maps to sum the prices for 
    // any shared sequences across all of the inpnuts
    const sequences = evolved.reduce((prev, curr) => {
        for (const [k, v] of curr) {
            prev.set(k, (prev.get(k) || 0) + v);
        }
        return prev;
    })
    let currMax;
    // Find whatever has the maximum sum of prices
    for (const [k, v] of sequences) {
        if(currMax === undefined || currMax[1] < v) {
            currMax = [k, v];
        }
    }
    console.log(currMax);
    return currMax[1]
}

const mix = (secret, input) =>  Number(BigInt(secret) ^ BigInt(input));
const prune = secret => secret % 16777216;
const evolve = secret => {
    secret = prune(mix(secret, secret * 64));
    secret = prune(mix(secret, Math.floor(secret/32)))
    return prune(mix(secret, secret * 2048))
}

run(__filename, solve, {skipTests: true});