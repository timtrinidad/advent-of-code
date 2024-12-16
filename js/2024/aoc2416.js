const { run } = require('aoc-copilot');
const { mapKey, parseMapKey } = require('../utils');
const Graph = require('node-dijkstra');

async function solve(inputs, partNum, isSample) {
    if(isSample) {
        inputs = `#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################`.split('\n');
    }

    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

const DIRS = [['S', 0, 1], ['N', 0, -1], ['W', -1, 0], ['E', 1, 0]];
function parse(inputs) {
    const width = inputs[0].length;
    const height = inputs.length;
    let startPt;
    let endPt;
    const graph = new Graph();
    for (let x = 0; x < width; x++) {
        for (let y = 0; y < height; y++) {
            let pt = inputs[y][x];
            if(pt === 'S') {
               startPt = [x, y];
            }
            if(pt === 'E') {
                endPt = [x, y];
            }

            if(pt === '#') {
                continue;
            }

            for (const [dn1, dx1, dy1] of DIRS) {
                const neighbors = {};
                for (const [dn2, dx2, dy2] of DIRS) {
                    const newX = x + dx2;
                    const newY = y + dy2;
                    if (newX < 0 || newX >= width || newY < 0 || newY >= width) {
                        continue;
                    }
                    const neighborPt = inputs[newY][newX];
                    if (neighborPt !== '.' && neighborPt !== 'E') {
                        continue;
                    }
                    neighbors[mapKey(x + dx2, y + dy2)+dn2] = dn1 === dn2 ? 1 : 1001;
                }
                graph.addNode(mapKey(x, y)+dn1, neighbors);
            }
        }
    }
    return {graph, startPt, endPt};
}

function part1({graph, startPt, endPt}) {

    return DIRS.map(([dn, dx, dy]) => {
        const path = graph.path(mapKey(startPt[0], startPt[1]) + 'E', mapKey(endPt[0], endPt[1]) + dn, {cost: true});
        return path.cost;
    }).filter(x => x !== 0).reduce((prev, curr) => prev < curr ? prev : curr);
}

function part2(parsed) {
    return 0;
}

run(__filename, solve);