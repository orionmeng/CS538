
/* This file is for testing lazy.js
 * it may help to revisit your initial pa3 submission */

const test_delay = (delay) => {
  try {
    let t = delay(() => 2 + 3);
    assert.strictEqual(t(), 5);
    assert.strictEqual(t(), 5);
  } catch(e) {
    return false;
  }
  return true;
};

const test_enumFrom = (enumFrom) => {
  try {
    let t = enumFrom(1);
    assert.strictEqual(t().head, 1);
    t = t().tail;
    assert.strictEqual(t().head, 2);
    t = t().tail;
    assert.strictEqual(t().head, 3);
  } catch(e) {
    return false;
  }
  return true;
};

const test_map = (map) => {
  try {
    let t = enumFrom(1);
    t = map(x => x * 2, t);
    assert.strictEqual(t().head, 2);
    t = t().tail;
    assert.strictEqual(t().head, 4);
    t = t().tail;
    assert.strictEqual(t().head, 6);
  } catch(e) {
    return false;
  }
  return true;
};

const test_zipWith = (zipWith) => {
  try {
    let t1 = enumFrom(1);
    let t2 = enumFrom(2);
    let t = zipWith((x, y) => x + y, t1, t2);
    assert.strictEqual(t().head, 3);
    t = t().tail;
    assert.strictEqual(t().head, 5);
    t = t().tail;
    assert.strictEqual(t().head, 7);
  } catch(e) {
    return false;
  }
  return true;
};

const test_tail = (tail) => {
  try {
    let t = enumFrom(1);
    t = tail(t);
    assert.strictEqual(t().head, 2);
  } catch(e) {
    return false;
  }
  return true;
};

const test_cons = (cons) => {
  try {
    let t = enumFrom(1);
    t = cons(0, t);
    assert.strictEqual(t().head, 0);
    assert.strictEqual(t().tail().head, 1);
  } catch(e) {
    return false;
  }
  return true;
};

exports.test_delay = test_delay;
exports.test_enumFrom = test_enumFrom;
exports.test_map = test_map;
exports.test_zipWith = test_zipWith;
exports.test_tail = test_tail;
exports.test_cons = test_cons;
