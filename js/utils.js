const mapKey = (x, y) => `${x},${y}`;
const parseMapKey = key => key.split(',').map(k => parseInt(k, 10));

module.exports = {
    mapKey,
    parseMapKey
}
