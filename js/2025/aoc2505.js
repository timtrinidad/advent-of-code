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

function part2({ ranges }) {
  let numRanges = ranges.length;
  while (true) {
    ranges = combineRanges(ranges);
    if (ranges.length === numRanges) {
      break;
    }
    numRanges = ranges.length;
  }

  return ranges.reduce((acc, [min, max]) => acc + max - min + 1, 0);
}

function combineRanges(ranges) {
  return ranges
    .reduce((actualRanges, [min, max]) => {
      const newSet = [];
      let needsToBeAdded = true;
      for (const [actualMin, actualMax] of actualRanges) {
        // Do nothing if falls within an existing range
        if (min >= actualMin && max <= actualMax) {
          return actualRanges;
        }
        // Min is outside range but max is within range - extend min
        if (min <= actualMin && max >= actualMin && max <= actualMax) {
          needsToBeAdded = false;
          newSet.push([min, actualMax]);
        }

        // min is within range but max is out - extend max
        if (min >= actualMin && min <= actualMax && max >= actualMax) {
          needsToBeAdded = false;
          newSet.push([actualMin, max]);
        }

        // range is a superset of another
        if (min <= actualMin && max >= actualMax) {
          needsToBeAdded = false;
          newSet.push([min, max]);
        }

        // range is before or after
        if (max < actualMin || min > actualMax) {
          newSet.push([actualMin, actualMax]);
        }
      }

      if (needsToBeAdded) {
        newSet.push([min, max]);
      }

      return newSet;
    }, [])
    .sort((a, b) => (a[0] == b[0] ? a[1] - b[1] : a[0] - b[0]));
}

run(__filename, solve);
