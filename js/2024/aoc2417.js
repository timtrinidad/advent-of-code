const { run } = require('aoc-copilot');
const { mapKey, parseMapKey } = require('../utils');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    const registers = {
        A: parseInt(inputs[0].substring(12), 10),
        B: parseInt(inputs[1].substring(12), 10),
        C: parseInt(inputs[2].substring(12), 10),
    };
    const instructions = inputs[4].substring(9).split(',').map(x => parseInt(x, 10));
    return {registers, instructions};
}

function part1({registers, instructions}) {
    const res = evaluate(registers, instructions);
    return res.join(',');
}

function part2({registers, instructions}) {
    // These two options by outputting all the outputs of very number up to 8^8.
    // These two numbers had outputs that matched the last 7 numbers of the input program.
    for (const n of [277327, 289726]) {
        for (let regA = n * Math.pow(8, 9); regA < (n+1) * Math.pow(8, 9); regA++) {
            if (regA % 1000000 === 0) {
                console.log(regA);
            }
            registers = {A: regA, B: 0, C: 0};
            const res = evaluate(registers, instructions, true);
            const isMatch = res.length === instructions.length && (res.every((x, i) => instructions[i] === x))
            if (isMatch) {
                return regA;
            }
        }
    }
}

const seen = new Set();
function evaluate(registers, instructions, matchInstructions = false, instPtr = 0, out = []) {
    if(instPtr >= instructions.length) {
        // Halt
        return out;
    }

    const instruction = instructions[instPtr];
    const literalOperand = instructions[instPtr + 1];
    instPtr += 2;

    switch (instruction) {
        case 0:
            registers.A = Math.floor(registers.A / Math.pow(2, getComboOperand(registers, literalOperand)));
            break;
        case 1:
            registers.B = registers.B ^ literalOperand;
            break;
        case 2:
            registers.B = getComboOperand(registers, literalOperand) % 8;
            break;
        case 3:
            if (registers.A !== 0) {
                instPtr = literalOperand;
            }
            break;
        case 4:
            registers.B = Number(BigInt(registers.B) ^ BigInt(registers.C));
            break;
        case 5:
            const toOut = getComboOperand(registers, literalOperand) % 8;
            if(matchInstructions && toOut !== instructions[out.length]) {
                return [];
            }
            out.push(toOut);
            break;
        case 6:
            registers.B = Math.floor(registers.A / Math.pow(2, getComboOperand(registers, literalOperand)));
            break;
        case 7:
            registers.C = Math.floor(registers.A / Math.pow(2, getComboOperand(registers, literalOperand)));
            break;
    }
    return evaluate(registers, instructions, matchInstructions, instPtr, out);

}

const getComboOperand = (registers, comboType) => ({
    0: 0,
    1: 1,
    2: 2,
    3: 3,
    4: registers.A,
    5: registers.B,
    6: registers.C
}[comboType])

run(__filename, solve, {skipTests: true});