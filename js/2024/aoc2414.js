const { run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs, isSample);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse(inputs, isSample) {
    const robots = inputs.map(x => {
        const matches = x.match(/p=(.+),(.+) v=(.+),(.+)/)
        return {
            p: [parseInt(matches[1], 10), parseInt(matches[2], 10)],
            v: [parseInt(matches[3], 10), parseInt(matches[4], 10)],
        }
    });
    return {
        robots,
        width: isSample ? 11 : 101,
        height: isSample ? 7 : 103,
    }
}

function part1({robots, width, height}) {
    for (let i = 0; i < 100; i++) {
        robots = moveRobots(robots, width, height);
    }
    const quadrants = {1: 0, 2: 0, 3: 0, 4: 0}
    robots.forEach(({p: [x, y]}) => {
        const midX = (width-1)/2;
        const midY = (height-1)/2;

        if(x < midX && y < midY) quadrants[1]++;
        if(x > midX && y < midY) quadrants[2]++;
        if(x < midX && y > midY) quadrants[3]++;
        if(x > midX && y > midY) quadrants[4]++;
    })
    return Object.values(quadrants).reduce((prev, curr) => prev * curr);

}

function part2(parsed) {
    return 0;
}

function moveRobots(robots, width, height) {
    return robots.map(({p: [x, y], v: [dx, dy]}) => {
       let newX = x + dx;
       if (newX >= width) newX = newX % width;
       if (newX < 0) newX = width + newX;
       let newY = y + dy;
       if (newY >= height) newY = newY % height;
       if (newY < 0) newY = height + newY;

       return {
           p: [newX, newY],
           v: [dx, dy],
       }
    });
}

run(__filename, solve);