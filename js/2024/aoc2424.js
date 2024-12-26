const { run } = require('aoc-copilot');
const { mapKey, parseMapKey, DIRS4, DIRS8 } = require('../utils');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs, partNum, isSample);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs, partNum, isSample) {
    let [inits, conns] = inputs.join('\n').split('\n\n');

    if (isSample && partNum == 1) {
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

    if (isSample && partNum == 2) {
        [inits, conns] = `x00: 0
x01: 1
x02: 0
x03: 1
x04: 0
x05: 1
y00: 0
y01: 0
y02: 1
y03: 1
y04: 0
y05: 1

x00 AND y00 -> z05
x01 AND y01 -> z02
x02 AND y02 -> z01
x03 AND y03 -> z03
x04 AND y04 -> z04
x05 AND y05 -> z00`.split('\n\n');
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
    const values = new Map(inits.entries());
    calculateZValues(values, conns, zLen);
    return binToInt(values, 'z');
}

function part2({inits, conns, zLen}) {
    // const values = new Map(inits.entries());
    const values = new Map([
        ["x00",false],
        ["x01",false],
        ["x02",false],
        ["x03",false],
        ["x04",false],
        ["x05",false],
        ["x06",false],
        ["x07",false],
        ["x08",false],
        ["x09",false],
        ["x10",false],
        ["x11",false],
        ["x12",false],
        ["x13",false],
        ["x14",false],
        ["x15",false],
        ["x16",false],
        ["x17",false],
        ["x18",false],
        ["x19",false],
        ["x20",false],
        ["x21",false],
        ["x22",false],
        ["x23",true],
        ["x24",false],
        ["x25",false],
        ["x26",false],
        ["x27",false],
        ["x28",false],
        ["x29",false],
        ["x30",false],
        ["x31",false],
        ["x32",false],
        ["x33",false],
        ["x34",false],
        ["x35",false],
        ["x36",false],
        ["x37",false],
        ["x38",false],
        ["x39",false],
        ["x40",false],
        ["x41",false],
        ["x42",false],
        ["x43",false],
        ["x44",false],

        ["y00",false],
        ["y01",false],
        ["y02",false],
        ["y03",false],
        ["y04",false],
        ["y05",false],
        ["y06",false],
        ["y07",false],
        ["y08",false],
        ["y09",false],
        ["y10",false],
        ["y11",false],
        ["y12",false],
        ["y13",false],
        ["y14",false],
        ["y15",false],
        ["y16",false],
        ["y17",false],
        ["y18",false],
        ["y19",false],
        ["y20",false],
        ["y21",false],
        ["y22",false],
        ["y23",false],
        ["y24",false],
        ["y25",false],
        ["y26",false],
        ["y27",false],
        ["y28",false],
        ["y29",false],
        ["y30",false],
        ["y31",false],
        ["y32",false],
        ["y33",false],
        ["y34",false],
        ["y35",false],
        ["y36",false],
        ["y37",false],
        ["y38",false],
        ["y39",false],
        ["y40",false],
        ["y41",false],
        ["y42",false],
        ["y43",false],
        ["y44",false],
    ]);
    calculateZValues(values, conns, zLen);
    console.log(binToInt(values, 'z'));
    console.log(JSON.stringify(Array.from(getDependencies(conns, 'z01').values()).reverse()))
    console.log(JSON.stringify(Array.from(getDependencies(conns, 'z02').values()).reverse()))
    console.log(JSON.stringify(Array.from(getDependencies(conns, 'z03').values()).reverse()));

    // console.log(Array.from(getDependencies(conns, 'z20')).length);
    // console.log(Array.from(getDependencies(conns, 'z21')).length);
    // console.log(Array.from(getDependencies(conns, 'z22')).length);
    // console.log(Array.from(getDependencies(conns, 'z23')).length);
    // console.log(Array.from(getDependencies(conns, 'z24')).length);
    // console.log(Array.from(getDependencies(conns, 'z25')).length);
    // console.log(getDependents(conns, 'x'));
    console.log(conns.get('z23'));
    return 1;
    
    // const values = new Map(inits.entries());
    calculateZValues(values, conns, zLen);
    const [xVal, yVal, zVal] = ['x', 'y', 'z'].map(prefix => binToInt(values, prefix));
    console.log(xVal, yVal, zVal);
    const expectedZVal = xVal + yVal;

    // switchConnections(inits, conns, zLen, expectedZVal, 8);


    // const values = new Map(inits.entries());
    // calculateZValues(values, conns, zLen, new Map([
    //     ['z05', 'z00'],
    //     ['z00', 'z05'],
    //     ['z01', 'z02'],
    //     ['z02', 'z01'],
    // ]));
    // console.log(bitToInt(values, 'x'));
    // console.log(bitToInt(values, 'y'));
    // console.log(bitToInt(values, 'z'));
    // return bitToInt(values, 'z');

    // let missing = getDependents(conns, 'z');
    // console.log(missing);

    // for (const connKey of conns.keys()) {
    //     const connsCopy = new Map(conns.entries());

    //     const switchKey1 = 'z23';
    //     const switchKey2 = connKey;
    //     if(switchKey1 === switchKey2) {
    //         continue;
    //     }

    //     const switchVal1 = connsCopy.get(switchKey1);
    //     const switchVal2 = connsCopy.get(switchKey2);
    //     connsCopy.set(switchKey1, switchVal2);
    //     connsCopy.set(switchKey2, switchVal1);
    //     missing = getDependents(connsCopy, 'z');
    //     if(missing.size === 1) {
    //         console.log(switchKey1, switchKey2, missing);
    //     }
    // }

}

function switchConnections(inits, conns, zLen, expectedVal, numSwitches, switches = new Map()) {
    const values = new Map(inits.entries());
    console.log(switches.entries())
    calculateZValues(values, conns, zLen, switches);
    if(binToInt(values, 'z') === expectedVal) {
        console.log(expectedVal, switches);
    }

    const outputs = [...conns.keys()];
    if(switches.size < numSwitches) {
        for (let i = 0; i < outputs.length-1; i++) {
            for (let j = i + 1; j < outputs.length; j++) {
                if(switches.has(outputs[i]) || switches.has(outputs[j])) {
                    continue;
                }

                const newSwitches = new Map(switches.entries());
                newSwitches.set(outputs[i], outputs[j]);
                newSwitches.set(outputs[j], outputs[i]);
                switchConnections(inits, conns, zLen, expectedVal, numSwitches, newSwitches);
            }
        }
    }
    
    
}

/**
 * Given a key, evaluate all dependent variables until we get the necessary values
 */
function getVal(map, conns, switches, key, seen = new Set()) {
    // Base case - value already found
    if(map.has(key)) {
        return map.get(key);
    }

    // prevent loops
    if(seen.has(key)) {
        return;
    }
    seen.add(key);

    if(switches.has(key)) {
        key = switches.get(key);
    }

    // Find both dependent variables and get those values
    if(!conns.has(key)) {
        throw new Error(`No value found for ${key}`)
    }
    let {in1, in2, op} = conns.get(key);

    const val1 = getVal(map, conns, switches, in1, seen);
    map.set(in1, val1);
    const val2 = getVal(map, conns, switches, in2, seen);
    map.set(in2, val2);

    // Perform the op
    switch (op) {
        case 'AND': return val1 && val2;
        case 'OR': return val1 || val2;
        case 'XOR': return val1 !==val2;
    }

}

function binToInt(map, prefix) {
    const keys = [...map.keys()].filter(x => x[0] === prefix).sort();
    return keys.reduce((prev, curr, z) => {
        const val = map.get(curr);
        return prev + Math.pow(2, z) * (val ? 1 : 0)
    }, 0);
}

function calculateZValues(values, conns, zLen, switches = new Map()) {
    // For each z variable, calculate its value
    for (let z = 0; z <= zLen; z++) {
        const key = `z${String(z).padStart(2, '0')}`;
        const val = getVal(values, conns, switches, key);
        values.set(key, val);
    }
}

function getDependencies(conns, key, deps = new Set()) {
    const dependencies = conns.get(key);
    if(!dependencies) {
        deps.add(key);
        return;
    }
    const { in1, in2 } = dependencies

    deps.add(in1);
    deps.add(in2);
    getDependencies(conns, in1, deps);
    getDependencies(conns, in2, deps);

    return deps;
    // return [getDependencyTree(conns, in1), ...getDependencyTree(conns, in2)]
    // return {[key]: [getDependencyTree(conns, in1), getDependencyTree(conns, in2)]}
}

function getDependents(conns, prefix) {
    const keys = [...conns.keys()].filter(x => x[0] === prefix);
    
    const deps = new Map();
    keys.forEach(key => {
        const dependencies = getDependencies(conns, key);
        for (const d of dependencies) {
            const list = deps.get(d) || [];
            list.push(key)
            deps.set(d, list);
        }
    });
    const allMissing = new Set();
    [...deps.keys()].sort().forEach(key => {
        const d = deps.get(key).sort();
        const start = parseInt(d[0].slice(1), 10);
        const end = parseInt(d[d.length-1].slice(1), 10);
        const missing = [];
        for (let i = start; i <= end; i++) {
            const toSearch = `z${String(i).padStart(2,'0')}`;
            if(!d.includes(toSearch)) {
                missing.push(toSearch);
                allMissing.add(toSearch);
            }
        }

        // console.log(key, d.join(','));
        // if(missing.length) {
            // console.log('!!! MISSING !!!', missing.join(','));
        // }
    });

    return allMissing;
}

function testAddition(inits, conns, zLen) {
    const values = new Map(inits.entries());
    calculateZValues(values, conns, zLen);
    const [xVal, yVal, zVal] = ['x', 'y', 'z'].map(prefix => binToInt(values, prefix));
    console.log(xVal, yVal, zVal);
    const expectedZVal = xVal + yVal;
    console.log(zVal.toString(2));
    console.log(expectedZVal.toString(2));
}

run(__filename, solve, {skipTests: true, onlyPart: 2});