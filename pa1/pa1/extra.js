const { List } = require('immutable')
const assert = require('assert');
const { fold_left } = require('./quick');
const { filter } = require('./quick');
const { map } = require('./quick');

/*
  The following problems will give you a chance to practice higher order
  functions. To solve these problems, you can only use map, filter, fold_left
  (see quick.js), partition, or a combination of these. You should not use for-
  or while-loops.  For each problem, a simple assertion is provided to test
  your implementation.  You may wish to add additional tests, though.
*/

let ls = List([1,2,3,4,5]);

/* QUESTION 1: Average of squares */

/*
  First, implement a function that returns the size of a list using fold_left.
*/

const ls_size = ls => fold_left((acc, x) => acc + 1, 0, ls);

assert(ls_size(ls) == 5);

/*
  Second, implement a function that computes the sum-of-squares of a list of
  integers using fold_left.
  Example: sum_sqrs([2,3]) => 2*2 + 3*3 = 13.
*/

const sum_sqrs = ls => fold_left((acc, x) => x * x + acc, 0, ls);

assert(sum_sqrs(ls) == 55);

/*
  Finally, use both functions to implement the average of squares function.
*/

const avg_sqrs = ls => sum_sqrs(ls) / ls_size(ls);

assert(avg_sqrs(ls) == 11);


/* QUESTION 2: Min/Max and Evens/Odds */

/*
  Implement min and max functions using fold_left

  An empty list has no minimum or maximum value, therefore, return undefined.
*/

const ls_min = ls => fold_left((acc, x) => x < acc ? x : acc, Number.MAX_VALUE, ls);
const ls_max = ls => fold_left((acc, x) => x > acc ? x : acc, Number.MIN_VALUE, ls);

assert(ls_min(ls) == 1);
assert(ls_max(ls) == 5);

/*
  Implement two functions that return evens and odds from a list of integers
  using filter
*/

const ls_evens = ls => filter((x) => x % 2 == 0, ls);
const ls_odds  = ls => filter((x) => x % 2 == 1, ls);

assert(ls_evens(ls).equals(List([2,4])));
assert(ls_odds(ls).equals(List([1,3,5])));

/*
  Using function composition, implement a function that returns the maximum
  number in the even subset of a given list and another function that returns
  the minimum number in the evens. Recall the definition of compose below:
*/
function compose(f, g) {
  return (x) => {
    return f(g(x));
  };
}

const max_even = ls => ls_max(ls_evens(ls));
const min_even = ls => ls_min(ls_evens(ls));

assert(max_even(ls) == 4);
assert(min_even(ls) == 2);


/* QUESTION 3: The reverse function and Palindromes */

/*
  Implement reverse using recursion
*/

const reverse = (ls) => {
  if (ls.size <= 1) {
    return ls;
  }
  const val = ls.first();
  ls = ls.shift();
  return reverse(ls).concat(List([val]));
};

assert(reverse(ls).equals(List([5,4,3,2,1])));

/*
  Implement reverse using fold_left
*/

const reverse2 = ls => fold_left((acc, x) => List([x]).concat(acc), [], ls);

assert(reverse2(ls).equals(List([5,4,3,2,1])));

/*
  Using map and your reverse function, write a function that takes
  a list of strings and returns a list of pairs where each pair contains the
  original string and true if it is a palindrome or false if it is not.
  You may want to use the function reverse_str below in your implementation.
*/
const reverse_str = str => reverse(List(str.split(''))).join('');

assert(reverse_str("reverse") == "esrever");

const palindromes = ls => map((x) => List([x, reverse_str(x) == x]), ls);

const str_ls = List(["test", "testset"]);
const expected = List([List(["test", false]), List(["testset", true])]);
assert(palindromes(str_ls).equals(expected));

exports.ls_size     = ls_size;
exports.sum_sqrs    = sum_sqrs;
exports.avg_sqrs    = avg_sqrs;
exports.ls_min      = ls_min;
exports.ls_max      = ls_max;
exports.ls_evens    = ls_evens;
exports.ls_odds     = ls_odds;
exports.max_even    = max_even;
exports.min_even    = min_even;
exports.reverse     = reverse;
exports.reverse2    = reverse2;
exports.palindromes = palindromes;
