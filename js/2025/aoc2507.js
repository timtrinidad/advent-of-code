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
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const char = inputs[y][x];
      map.set(mapKey(x, y), char === "S" ? "|" : char);
    }
  }
  return { map, width, height };
}

function part1({ height, width, map }) {
  for (let y = 1; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const curr = map.get(mapKey(x, y));
      const above = map.get(mapKey(x, y - 1));

      // If point above is a beam and current point is empty, convert to beam
      if (curr === "." && above === "|") {
        map.set(mapKey(x, y), "|");
      }

      // If splitter, add beam to left and right
      if (curr === "^" && above === "|") {
        if (x > 0) map.set(mapKey(x - 1, y), "|");
        if (x < width - 1) map.set(mapKey(x + 1, y), "|");
      }
    }
  }

  //   printMap(map, width, height);

  // Count the number of splitters with beams above them
  return Array.from(map).reduce((acc, [key, char]) => {
    const [x, y] = parseMapKey(key);
    if (char === "^" && map.get(mapKey(x, y - 1)) === "|") {
      return acc + 1;
    }
    return acc;
  }, 0);
}

function part2(parsed) {
  return 0;
}

function printMap(map, width, height) {
  process.stderr.write("\n");
  process.stderr.write("\n");
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      process.stdout.write(map.get(mapKey(x, y)));
    }
    process.stderr.write("\n");
  }
  process.stderr.write("\n");
  process.stderr.write("\n");
}

run(__filename, solve);
