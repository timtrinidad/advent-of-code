const { run } = require('aoc-copilot');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs, isSample);

    if(partNum === 2 && isSample) {
        return 0;
    }

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

function part2({robots, width, height}) {
    let i = 0;
    let isMirror;
    do {
        i++
        robots = moveRobots(robots, width, height);

        const rowMap = new Map();
        robots.forEach(({p: [x, y]}) => {
            const side = x < 50 ? 'L' : 'R';
            const k = `${y}${side}`;
            rowMap.set(k, (rowMap.get(k) || 0) + 1);
        });
        isMirror = true;
        for (let y = 0; y < height; y++) {
            if(rowMap.get(`${y}L`) !== rowMap.get(`${y}R`)) {
                isMirror = false;
                break;
            }
        }

        // Recognized a cluster formed (not always a christmas tree) every 69 + 101x iterations,
        // so just render those
        if(i % 101 === 69) {
            renderRobots(robots, height, width)
            console.log("---"+i)
        }

        // All the posisions seem to loop after 101*103 times, so don't go past that
    } while (!isMirror && i < 10403);

    // Not the actual answer - the actual iteration would be determined by actually looking through
    // the rendered output
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

function renderRobots(robots, width, height, scale = 1) {
    for (let y = 0; y < height; y += scale) {
        let row = [];
        for (let x = 0; x < width; x += scale) {
            const hasRobot = robots.some(({p: [px, py]}) => px === x && py === y);
            row.push(hasRobot ? 'x' : '.');
        }
        console.log(row.join(''))
    }
    console.log();
}

const mapKey = (x, y) => `${x},${y}`;

run(__filename, solve);