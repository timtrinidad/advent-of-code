const { run } = require('aoc-copilot');
const { mapKey, parseMapKey, DIRS4, DIRS8 } = require('../utils');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs, isSample);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs, isSample) {
    let [inits, conns] = inputs.join('\n').split('\n\n');

    if (isSample) {
        [inits, conns] = `x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj`.split('\n\n');
    }

    // Initial part of puzzle, has initial values for some variables
    inits = new Map(inits.split('\n').map(x => {
        const [name, val] = x.split(': ');
        return [name, val === '1' ? true : false];
    }));

    // Parse out the connections, including the number of z variables
    let zLen = 0;
    conns = new Map(conns.split('\n').map(x => {
        const [in1, op, in2, _, out] = x.split(' ');
        if (out[0] === 'z') {
            const zNum = parseInt(out.slice(1), 10);
            if(zNum > zLen) {
                zLen = zNum;
            }
        }
        return [out, {in1, op, in2}]
    }));

    return {inits, conns, zLen}
}

function part1({inits, conns, zLen}) {
    let res = 0;
    // For each z variable, get its value and set the appropriate bit
    for (let z = 0; z <= zLen; z++) {
        const val = getVal(inits, conns, `z${String(z).padStart(2, '0')}`);
        res += Math.pow(2, z) * (val ? 1 : 0)
    }
    return res;
}

function part2(parsed) {
    return 0;
}

function evaluate(map, conns) {
    con

    return map;
}

/**
 * Given a key, evaluate all dependent variables until we get the necessary values
 */
function getVal(map, conns, key) {
    // Base case - value already found
    if(map.has(key)) {
        return map.get(key);
    }

    // Find both dependent variables and get those values
    const {in1, in2, op} = conns.get(key);
    const val1 = getVal(map, conns, in1);
    const val2 = getVal(map, conns, in2);

    // Perform the op
    switch (op) {
        case 'AND': return val1 && val2;
        case 'OR': return val1 || val2;
        case 'XOR': return val1 !==val2;
    }

}

run(__filename, solve);