-- DO NOT SUBMIT THIS FILE

module Lab1Tests where

import Lab1

-- if the following line causes an error, type (at the terminal prompt):
--   cabal update
--   cabal install HUnit

import Test.HUnit -- see http://hackage.haskell.org/package/HUnit for
                  -- documentation if you want to write your own test
                  -- cases

-- Test Cases

-- minus
test2a = TestCase $ assertEqual "minus 1 1" 0 (minus 1 1)
test2b = TestCase $ assertEqual "minus 7 4" 3 (minus 7 4)

-- funN
_ = fun1 :: Integer -> Integer
_ = fun2 :: Integer -> Integer -> Integer
_ = fun3 :: Integer -> (Integer,Integer)
_ = fun4 :: (Integer,Integer) -> Integer
_ = fun5 :: Integer -> Double -> String -> String
_ = fun6 :: Integer -> String -> String -> Integer -> (Integer,String)

-- sumSquareDiff
test4a = TestCase $ assertEqual "sumSquareDiff 1" 0 (sumSquareDiff 1)
test4b = TestCase $ assertEqual "sumSquareDiff 10" 2640 (sumSquareDiff 10)
test4c = TestCase $ assertEqual "sumSquareDiff 100" 25164150 (sumSquareDiff 100)
test4d = TestCase $ assertEqual "sumSquareDiff 1000" 250166416500 (sumSquareDiff 1000)

-- for running all the tests (type "runTests" within GHCi -- without
-- the quotes)
runTests = runTestTT $ TestList [test2a, test2b, test4a, test4a, test4b, test4c, test4d]
