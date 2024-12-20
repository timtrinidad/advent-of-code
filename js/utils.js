const mapKey = (x, y) => `${x},${y}`;
const parseMapKey = key => key.split(',').map(k => parseInt(k, 10));
const DIRS4 = [[0, 1], [0, -1], [1, 0], [-1, 0]];
const DIRS8 = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]];

module.exports = {
    mapKey,
    parseMapKey,
    DIRS4,
    DIRS8,
}
