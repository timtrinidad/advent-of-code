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
  const vals = parsed.map((line, lineNum) => {
    console.log(lineNum);
    return findMax(line);
  });
  return vals.reduce((acc, curr) => acc + curr);
}

function findMax(nums) {
  const queue = [[nums, 0]];
  let currMax = 0;

  // BFS through each list of numbers
  const seen = new Map();
  while (queue.length) {
    const [[curr, ...rest], max] = queue.pop();

    // Exit early if a greater number with the same length was already found
    const lenMax = max.toString().length;
    if (seen.has(lenMax) && seen.get(lenMax) > max) {
      continue;
    }
    seen.set(lenMax, max);

    // Terminal condition
    if (max >= 100_000_000_000) {
      // Keep track of current max 12 digit number
      if (max > currMax) {
        currMax = max;
      }
      continue;
    }

    if (curr) {
      queue.push([rest, max * 10 + curr]);
    }

    // Don't skip if we'll have less than the required number of digits left
    if (rest.length + max.toString().length >= 12) {
      queue.push([rest, max]);
    }
  }
  return currMax;
}

run(__filename, solve);
