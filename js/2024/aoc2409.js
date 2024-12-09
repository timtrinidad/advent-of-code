const { run } = require('aoc-copilot');
const { Yallist: LinkedList, Node } = require('yallist');

async function solve(inputs, partNum, isSample) {
    const parsed = parse(inputs);
    
    return partNum === 1 ? part1(parsed) : part2(parsed);
}

function parse([input]) {
    let isFile = true;
    let fileId = 0;

    // Create a doubly linked list of lengths and file IDs
    // (where fileId is `undefined` if it's a free space
    const ll = new LinkedList();
    for(const len of input) {
        const lenInt = parseInt(len, 10);
        if(isFile) {
            ll.push({id: fileId, len: lenInt});
            fileId++;
        } else {
            ll.push({id: undefined, len: lenInt});
        }
        isFile = !isFile;
    }
    return ll;
}

function part1(list) {
    // Start with the first space
    let currSpace = list.head.next;
    // Start with the last file
    let currFile = list.tail.value.id !== undefined ? list.tail : list.tail.prev;

    // While there aren't two free spaces in a row (which the space and file
    // pointers have met in the middle and we're done)
    while (currSpace.value.id === undefined && currSpace.next.value.id !== undefined) {
        const spaceLen = currSpace.value.len;
        const fileLen = currFile.value.len;

        if(spaceLen <= fileLen) {
            // Current space is smaller than the file in question:
            // Fill the space with the current file and reduce the file length
            currSpace.value.id = currFile.value.id;
            currFile.value.len = fileLen - spaceLen;
        } else {
            // Current space is larger than the file in question:
            // Fill as much space as possible and add a new space node
            // after the currently relocated file
            currSpace.value.id = currFile.value.id;
            currSpace.value.len = fileLen;
            const remainingSpace = new Node({id: undefined, len: spaceLen - fileLen}, currSpace, currSpace.next, list);
            currSpace.next.prev = remainingSpace;
            currSpace.next = remainingSpace;
            // Set the remaining file length to 0
            currFile.value.len = 0;
        }

        // Move on to the next available space (which may be the one we just added)
        currSpace = currSpace.next.value.id === undefined ? currSpace.next : currSpace.next.next;
        // Only move onto the next file if the current one has been full moved
        if(currFile.value.len === 0) {
            // If so, remove its entry
            const toDelete = currFile;
            currFile = currFile.prev.prev;
            list.removeNode(toDelete);
        }
    }

    // Calculate the checksum by multiplying block IDs with fileIds
    let blockId = 0;
    let sum = 0;
    list.forEach(({id: fileId, len}) => {
        for(let i = 0; i < len; i++) {
            if(fileId) {
                sum += fileId * (blockId + i);
            }
        }
        blockId += len;
    });

    return sum;
}

function part2(parsed) {
    return 0;
}

run(__filename, solve);