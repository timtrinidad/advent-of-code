const { run } = require("aoc-copilot");
const { mapKey, parseMapKey, DIRS4, DIRS8 } = require("../utils");

async function solve(inputs, partNum, isSample) {
  const parsed = parse(inputs);

  return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
  const map = new Map();
  const width = inputs[0].length;
  const height = inputs.length;
  inputs.map((line, y) => {
    line.split("").map((char, x) => {
      map.set(mapKey(x, y), char);
    });
  });
  return {
    map,
    width,
    height,
  };
}

function part1({ map, width, height }) {
  let num = 0;
  for (let x = 0; x < width; x++) {
    for (let y = 0; y < height; y++) {
      if (map.get(mapKey(x, y)) !== "@") {
        continue;
      }

      const numSurrounding = DIRS8.reduce(
        (acc, [dx, dy]) =>
          map.get(mapKey(x + dx, y + dy)) === "@" ? acc + 1 : acc,
        0
      );
      if (numSurrounding < 4) {
        num++;
      }
    }
  }
  return num;
}

function part2(parsed) {
  return 0;
}

run(__filename, solve);
