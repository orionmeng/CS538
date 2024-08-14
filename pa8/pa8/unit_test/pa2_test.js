/* This file is for testing helpers.js, require-js.js, lazy.js
 * it may help to revisit your initial pa2 submission           */

const test_for_ = (for_) => {
  try {
    let sum = 0;
    for_(0, (cur) => cur < 5, (cur) => cur + 1, (cur) => sum += cur);
    assert.strictEqual(sum, 10);

    let count = 0;
    for_(10, (cur) => cur > 0, (cur) => cur - 1, () => count++);
    assert.strictEqual(count, 10);
  } catch(e) {
    return false;
  }
  return true;
};

const test_each = (each) => {
  try {
    let sum = 0;
    each(List([1, 2, 3, 4, 5]), (el) => sum += el);
    assert.strictEqual(sum, 15);

    let count = 0;
    each(List([1, 2, 3, 4, 5]), () => count++);
    assert.strictEqual(count, 5);
  } catch(e) {
    return false;
  }
  return true;
};

// test.json exists in the auto-grader folder. You can load this file in for testing
// with loadJSONFile("test.json")
// it has the following structure:
// {
//   "key": 10,
//   "key2": 20
// }
const test_loadJSONFile = (loadJSONFile) => {
  try {
    const jsonData = loadJSONFile('test.json');
    assert.strictEqual(jsonData.key, 10);
    assert.strictEqual(jsonData.key2, 20);
  } catch(e) {
    return false;
  }
  return true;
};

exports.test_for_ = test_for_;
exports.test_each = test_each;
exports.test_loadJSONFile = test_loadJSONFile;
