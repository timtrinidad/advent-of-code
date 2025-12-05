const { run } = require("aoc-copilot");
const { mapKey, parseMapKey, DIRS4, DIRS8 } = require("../utils");

async function solve(inputs, partNum, isSample) {
  const parsed = parse(inputs);

  return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
  const [ranges, ids] = inputs
    .join("\n")
    .split("\n\n")
    .map((x) => x.split("\n"));

  return {
    ranges: ranges.map((x) => x.split("-").map((y) => parseInt(y, 10))),
    ids: ids.map((x) => parseInt(x, 10)),
  };
}

function part1({ ranges, ids }) {
  const freshIds = ids.filter((id) =>
    ranges.some(([min, max]) => id >= min && id <= max)
  );
  return freshIds.length;
}

function part2(parsed) {
  return 0;
}

run(__filename, solve);
