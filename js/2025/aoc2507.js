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
  let start;
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      let char = inputs[y][x];
      if (char === "S") {
        start = [x, y];
        char = "|";
      }
      map.set(mapKey(x, y), char);
    }
  }
  return { map, width, height, start };
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

function part2({ map, width, height, start: [startX, startY] }) {
  return traverse(map, width, height, startX, startY);
}

// memoization
const seen = new Map();

function traverse(map, width, height, x, y) {
  // Memoized value - we already know how many paths are possible from this position
  const seenVal = seen.get(mapKey(x, y));
  if (seenVal !== undefined) {
    return seenVal;
  }

  // Reached the bottom - counts as one path
  if (y === height - 1) {
    return 1;
  }

  const charBelow = map.get(mapKey(x, y + 1));
  let numPaths;

  if (charBelow === ".") {
    // No splitter below - just move down one spot
    numPaths = traverse(map, width, height, x, y + 1);
  } else {
    // Splitter is below - split to two possible paths
    numPaths =
      traverse(map, width, height, x + 1, y + 1) +
      traverse(map, width, height, x - 1, y + 1);
  }

  seen.set(mapKey(x, y), numPaths);
  return numPaths;
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
