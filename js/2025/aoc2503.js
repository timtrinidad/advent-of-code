const { run } = require("aoc-copilot");
const { mapKey, parseMapKey, DIRS4, DIRS8 } = require("../utils");

async function solve(inputs, partNum, isSample) {
  const parsed = parse(inputs);

  return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
  return inputs.map((x) => x.split("").map((y) => parseInt(y, 10)));
}

function part1(parsed) {
  const vals = parsed.map((line) =>
    line.reduce(
      (acc, curr, idx) =>
        Math.max(
          acc,
          line
            .slice(idx + 1)
            .reduce((acc2, curr2) => Math.max(acc2, curr * 10 + curr2), acc)
        ),
      0
    )
  );
  return vals.reduce((acc, curr) => acc + curr);
}

function part2(parsed) {
  return 0;
}

run(__filename, solve);
