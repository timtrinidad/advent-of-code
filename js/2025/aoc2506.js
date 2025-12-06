const { run } = require("aoc-copilot");
const { mapKey, parseMapKey, DIRS4, DIRS8 } = require("../utils");

async function solve(inputs, partNum, isSample) {
  const parsed = parse(inputs);

  return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs) {
  return inputs;
}

function part1(parsed) {
  const split = parsed.map((x) => x.trim().split(/\s+/));
  const ops = split.pop();
  const nums = split.map((x) => x.map((y) => parseInt(y, 10)));

  const totals = ops.map((op, x) =>
    nums
      .map((row) => row[x])
      .reduce((acc, curr) => (op === "*" ? acc * curr : acc + curr))
  );
  return totals.reduce((acc, curr) => acc + curr);
}

function part2(parsed) {
  const nums = parsed.map((x) => x.split(""));
  const ops = nums.pop();

  let groupOps = [];
  let groupNums = [];
  let groupNum = 0;
  // Go char column by column until we find a col with all spaces
  for (let x = 0; x < ops.length; x++) {
    // Capture the op for this group
    if (ops[x] !== " ") {
      groupOps[groupNum] = ops[x];
    }
    // Gather all characters in this column
    const num = nums.map((row) => row[x]);

    // If all blanks, we've finished the column. Move onto next group.
    if (num.every((char) => char === " ")) {
      groupNum++;
      continue;
    }

    // Parse the numbers in the column and add to current group
    numParsed = parseInt(num.filter((char) => char !== " ").join(""), 10);
    if (!groupNums[groupNum]) {
      groupNums[groupNum] = [];
    }
    groupNums[groupNum].push(numParsed);
  }

  return groupOps
    .map((op, groupNum) =>
      groupNums[groupNum].reduce((acc, curr) =>
        op === "*" ? acc * curr : acc + curr
      )
    )
    .reduce((acc, curr) => acc + curr);
}

run(__filename, solve);
