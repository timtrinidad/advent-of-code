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
    // registers = {A: 2024}
    // instructions = [0,1,5,4,3,0];
    const res = evaluate(registers, instructions);
    console.log(registers);
    return res.join(',');
}

function part2({registers, instructions}) {
    let isMatch = false;
    let regA = 38895504088832;
    let lastLen = 0;
    do {
        regA--;
        if (regA % 1000000 === 0) {
            console.log(regA);
        }
        registers = {A: regA, B: 0, C: 0};
        const res = evaluate(registers, instructions);
        // if(res.length ) {
        //     console.log(regA, res.length, res.join(','));
        //     lastLen = res.length;
        // }
        isMatch = res.length === instructions.length && (res.every((x, i) => instructions[i] === x))
    } while (!isMatch)
    // console.log(evaluate({A: 38842779000000, B: 0, C:0}, instructions));

    return regA;
}

const seen = new Set();
function evaluate(registers, instructions, instPtr = 0, out = []) {
    // console.log(registers);
    if(instPtr >= instructions.length) {
        // Halt
        return out;
    }

    // const seenKey = `${registers.B},${registers.C},${instPtr}`;
    // console.log(seenKey);
    // if (seen.has(seenKey)) {
    //     // console.log('loop');
    //     return [];
    // }
    // seen.add(seenKey);

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
            registers.B = registers.B ^ registers.C;
            break;
        case 5:
            const toOut = getComboOperand(registers, literalOperand) % 8;
            if(toOut !== instructions[out.length]) {
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
    return evaluate(registers, instructions, instPtr, out);

}

function getComboOperand(registers, comboType) {
    switch (comboType) {
        case 0:
        case 1:
        case 2:
        case 3:
            return comboType;
        case 4:
            return registers.A;
        case 5:
            return registers.B;
        case 6:
            return registers.C;
        case 7:
            throw new Error('Invalid operand');
    }
}

run(__filename, solve, {skipTests: true, onlyPart: 2});