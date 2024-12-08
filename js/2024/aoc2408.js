const { NotImplemented, run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
    const width = inputs[0].length;
    const height = inputs.length;
    const map = new Map();
    const frequencies = new Map();
    for (let x = 0; x < width; x++) {
        for (let y = 0; y < height; y++) {
            const point = inputs[y][x];
            map.set(encodeMapKey(x, y), point);
            if(point !== '.') {
                frequencies.set(point, (frequencies.get(point) || new Set()).add(encodeMapKey(x, y)));
            }
        }
    }
    return {
        map,
        width,
        height,
        frequencies,
    }
}

function part1({width, height, map, frequencies}) {
    const antinodes = [];

    for (const pointKeys of frequencies.values()) {
        const points = [...pointKeys.values()].map(decodeMapKey);
        for (let i = 0; i < points.length - 1; i++) {
            for (let j = i + 1; j < points.length; j++) {
                const [x1, y1] = points[i];
                const [x2, y2] = points[j];
                console.log(points[i], points[j])
                const dx = x2 - x1;
                const dy = y2 - y1;
                antinodes.push([x1 - dx, y1 - dy]);
                antinodes.push([x2 + dx, y2 + dy]);
            }
        }
    };
    const set = new Set(
        antinodes
            .filter(([x, y]) => x >= 0 && y >= 0 && x < width && y < height)
            .map(([x, y]) => encodeMapKey(x, y))
    );
    return set.size;
}

function part2(parsed) {
    return 0;
}

const encodeMapKey = (x, y) => `${x},${y}`;
const decodeMapKey = key => key.split(',').map(x => parseInt(x, 10));

run(__filename, solve);