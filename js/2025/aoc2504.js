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
  const accessibleRolls = findAccessibleRolls(map, width, height);

  return accessibleRolls.length;
}

function part2({ map, width, height }) {
  let accessibleRolls;
  let removedRolls = 0;
  do {
    accessibleRolls = findAccessibleRolls(map, width, height);
    accessibleRolls.forEach(([x, y]) => map.set(mapKey(x, y), "."));
    removedRolls += accessibleRolls.length;
  } while (accessibleRolls.length > 0);
  return removedRolls;
}

function findAccessibleRolls(map, width, height) {
  const accessibleRolls = [];
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
        accessibleRolls.push([x, y]);
      }
    }
  }
  return accessibleRolls;
}

run(__filename, solve);
