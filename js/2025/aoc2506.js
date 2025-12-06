const { run } = require("aoc-copilot");
const { mapKey, parseMapKey, DIRS4, DIRS8 } = require("../utils");

async function solve(inputs, partNum, isSample) {
  const parsed = parse(inputs);

  return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
  const parsed = inputs.map((x) => x.trim().split(/\s+/));
  const ops = parsed.pop();
  return { nums: parsed.map((x) => x.map((y) => parseInt(y, 10))), ops };
}

function part1({ nums, ops }) {
  const totals = ops.map((op, x) =>
    nums
      .map((row) => row[x])
      .reduce((acc, curr) => (op === "*" ? acc * curr : acc + curr))
  );
  return totals.reduce((acc, curr) => acc + curr);
}

function part2(parsed) {
  return 0;
}

run(__filename, solve);
