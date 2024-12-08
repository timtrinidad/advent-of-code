const { NotImplemented, run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    const width = inputs[0].length;
    const height = inputs.length;
    const frequencies = new Map();
    const antennaPoints = [];
    for (let x = 0; x < width; x++) {
        for (let y = 0; y < height; y++) {
            const point = inputs[y][x];
            if(point !== '.') {
                frequencies.set(point, (frequencies.get(point) || new Set()).add(encodeMapKey(x, y)));
                antennaPoints.push([x, y]);
            }
        }
    }
    return {
        map,
        width,
        height,
        frequencies,
        antennaPoints
    }
}

function part1({width, height, map, frequencies}) {
    const antinodes = findAntinodes(frequencies, width, height, 1);
    const set = new Set(
        antinodes.map(([x, y]) => encodeMapKey(x, y))
    );
    return set.size;
}

function part2({width, height, map, frequencies, antennaPoints}) {
    const antinodes = findAntinodes(frequencies, width, height, 2);
    antinodes.push(...antennaPoints);
    const set = new Set(
        antinodes.map(([x, y]) => encodeMapKey(x, y))
    );
    return set.size;
}

const inBounds = (x, y, width, height) => x >= 0 && y >= 0 && x < width && y < height;

const encodeMapKey = (x, y) => `${x},${y}`;
const decodeMapKey = key => key.split(',').map(x => parseInt(x, 10));

function findAntinodes(frequencies, width, height, partNum) {
    const antinodes = [];

    for (const pointKeys of frequencies.values()) {
        const points = [...pointKeys.values()].map(decodeMapKey);
        for (let i = 0; i < points.length - 1; i++) {
            for (let j = i + 1; j < points.length; j++) {
                const [x1, y1] = points[i];
                const [x2, y2] = points[j];
                const dx = x2 - x1;
                const dy = y2 - y1;
                let candidates = [];
                if(partNum == 1) {
                    candidates = [[x1 - dx, y1 - dy], [x2 + dx, y2 + dy]];
                } else {
                    let mult = 1;
                    while(true) {
                        const newPt = [x1 - (dx * mult), y1 - (dy * mult)];
                        if(!inBounds(newPt[0], newPt[1], width, height)) {
                            break;
                        }
                        candidates.push(newPt);
                        mult++;
                    }
                    mult = 1;
                    while(true) {
                        const newPt = [x2 + (dx * mult), y2 + (dy * mult)];
                        if(!inBounds(newPt[0], newPt[1], width, height)) {
                            break;
                        }
                        candidates.push(newPt);
                        mult++;
                    }
                }

                antinodes.push(...candidates.filter(([x, y]) => inBounds(x, y, width, height)))
            }
        }
    };

    return antinodes
}

run(__filename, solve);