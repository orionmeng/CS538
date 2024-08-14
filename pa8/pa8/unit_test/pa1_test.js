/* This file is for testing quick.js (we won't test extra.js) 
 * it may help to revisit your initial pa1 submission         */

const { List } = require('immutable')

const test_fold_left = (fold_left) => {
    try {
        // test 1: sum
        assert.strictEqual(fold_left((acc, x) => acc + x, 0, List([1, 2, 3, 4])), 10);

        // test 2: product
        assert.strictEqual(fold_left((acc, x) => acc * x, 1, List([1, 2, 3, 4])), 24);

        // test 3: cconcat
        assert.strictEqual(fold_left((acc, x) => acc + x, '', List(['hello', ' ', 'world'])), 'hello world');

        // test 4: empty list
        assert.strictEqual(fold_left((acc, x) => acc + x, 0, List([])), 0);

        // test 5: non-primitive types
        assert.deepStrictEqual(fold_left((acc, x) => acc.push(x), List([]), List([1, 2, 3])), List([1, 2, 3]));
    } catch(e) {
        return false;
    }
    return true;
}

const test_map = (map) => {
    try {
        // test 1: double
        assert.deepStrictEqual(map((x) => x * 2, List([1, 2, 3, 4])), List([2, 4, 6, 8]));

        // test 2: uppercase
        assert.deepStrictEqual(map((str) => str.toUpperCase(), List(['hello', 'world'])), List(['HELLO', 'WORLD']));

        // test 3: empty list
        assert.deepStrictEqual(map((x) => x * 2, List([])), List([]));
    } catch(e) {
        return false;
    }
    return true;
}


const test_filter = (filter) => {
    try {
        // test 1: even numbers
        assert.deepStrictEqual(filter((x) => x % 2 === 0, List([1, 2, 3, 4, 5])), List([2, 4]));

        // test 2: strings longer than 3 chars
        assert.deepStrictEqual(filter((str) => str.length > 3, List(['cat', 'dog', 'elephant', 'bird'])), List(['elephant', 'bird']));

        // test 3: empty list
        assert.deepStrictEqual(filter((x) => x > 5, List([])), List([]));
    } catch(e) {
        return false;
    }
    return true;
}


const test_partition = (partition) => {
    try {
        // test 1: numbers less than or equal to 2
        assert.deepStrictEqual(partition((x) => x <= 2, List([1, 2, 3, 4])), List([List([1, 2]), List([3, 4])]));

        // test 2: even and odd numbers
        assert.deepStrictEqual(partition((x) => x % 2 === 0, List([1, 2, 3, 4])), List([List([2, 4]), List([1, 3])]));

        // test 3: empty list
        assert.deepStrictEqual(partition((x) => x < 5, List([])), List([List([]), List([])]));
    } catch(e) {
        return false;
    }
    return true;
}


const test_quicksort = (quicksort) => {
    try {
        // test 1: numbers
        assert.deepStrictEqual(quicksort(List([4, 7, 3, 6, 8, 7, 1, 2, 2, 1, 5])), List([1, 1, 2, 2, 3, 4, 5, 6, 7, 7, 8]));

        // test 2: strings
        assert.deepStrictEqual(quicksort(List(['banana', 'apple', 'grape', 'cherry'])), List(['apple', 'banana', 'cherry', 'grape']));

        // test 3: empty list
        assert.deepStrictEqual(quicksort(List([])), List([]));
    } catch(e) {
        return false;
    }
    return true;
}


/* DO NOT MODIFY BELOW THIS LINE */
exports.test_fold_left = test_fold_left;
exports.test_map = test_map;
exports.test_filter = test_filter;
exports.test_partition = test_partition;
exports.test_quicksort = test_quicksort;
