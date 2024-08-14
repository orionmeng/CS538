import Test.HUnit  -- HUnit for unit tests

-- Import necessary modules from the source code
import Syntax
import Type
import Lexer
import Parser
import Eval
import Infer
import Pretty

import Control.Exception (try, SomeException, evaluate)  -- For handling exceptions
import Data.Either (isLeft)  -- For checking if a value is a Left

-- Test Cases

-- Test for basic addition
testMl :: Test
testMl = TestCase $ do
  let x = 1 + 1
  assertEqual "1 + 1 should be 2" 2 x

-- Test concatenation of lists
testConcat :: Test
testConcat = TestCase $ do
  let list1 = [1, 2]
  let list2 = [3, 4]
  let expected = [1, 2, 3, 4]
  assertEqual "Concatenation of [1, 2] and [3, 4]" expected (list1 ++ list2)

-- Test list of integers
testListOfInts :: Test
testListOfInts = TestCase $ do
  let intList = [1, 2, 3]
  assertEqual "List of ints [1, 2, 3]" [1, 2, 3] intList

-- Test list of booleans
testListOfBools :: Test
testListOfBools = TestCase $ do
  let boolList = [True, False, True]
  assertEqual "List of bools [True, False, True]" [True, False, True] boolList

-- Test list of lists
testListOfLists :: Test
testListOfLists = TestCase $ do
  let listOfLists = [[1, 2], [3, 4]]
  assertEqual "List of lists [[1, 2], [3, 4]]" [[1, 2], [3, 4]] listOfLists

-- Test cons operation
testCons :: Test
testCons = TestCase $ do
  let originalList = [1, 2, 3]
  let newList = 0 : originalList
  assertEqual "Cons operation with 0 and [1, 2, 3]" [0, 1, 2, 3] newList

-- Test expression lists
testExprLists :: Test
testExprLists = TestCase $ do
  let exprList = [2 + 3, 4 * 2]
  assertEqual "Expression list [2 + 3, 4 * 2]" [5, 8] exprList

-- Test cons errors
testConsError :: Test
testConsError = TestCase $ do
  let originalList = [1, 2, 3]
  result <- try (evaluate (undefined : originalList)) :: IO (Either SomeException [Int])
  assertBool "Cons error should raise an exception" (isLeft result)

-- Test patterns with int, bool, and list
testPatterns :: Test
testPatterns = TestCase $ do
  let (a, b) = (1, True)
  assertEqual "Pattern matching with int and bool" (1, True) (a, b)

-- Test patterns with int lists
testExecIntPatterns :: Test
testExecIntPatterns = TestCase $ do
  let originalList = [1, 2, 3]
  let (x:xs) = originalList
  assertEqual "Pattern matching with int list" (1, [2, 3]) (x, xs)

-- Test patterns with bool lists
testExecBoolPatterns :: Test
testExecBoolPatterns = TestCase $ do
  let boolList = [True, False, True]
  let (y:ys) = boolList
  assertEqual "Pattern matching with bool list" (True, [False, True]) (y, ys)

-- Test patterns with lists
testExecListPatterns :: Test
testExecListPatterns = TestCase $ do
  let listOfLists = [[1, 2], [3, 4]]
  let (firstList:rest) = listOfLists
  assertEqual "Pattern matching with list of lists" ([1, 2], [[3, 4]]) (firstList, rest)

-- Test suite definition
testSuite :: Test
testSuite = TestList
  [ testMl
  , testConcat
  , testListOfInts
  , testListOfBools
  , testListOfLists
  , testCons
  , testExprLists
  , testConsError
  , testPatterns
  , testExecIntPatterns
  , testExecBoolPatterns
  , testExecListPatterns
  ]

-- Main function to run the test suite
main :: IO ()
main = do
  _ <- runTestTT testSuite
  return ()
