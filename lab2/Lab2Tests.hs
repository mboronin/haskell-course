-- DO NOT SUBMIT THIS FILE

module Lab2Tests where

import Lab2

-- if the following line causes an error, type (at the terminal prompt):
--   cabal update
--   cabal install HUnit

import Test.HUnit -- see http://hackage.haskell.org/package/HUnit for
                  -- documentation if you want to add your own test
                  -- cases

-- Test Cases

-- iota
test2a = TestCase $ assertEqual "iota 3" [0,1,2] (iota 3)
test2b = TestCase $ assertEqual "iota 0" [] (iota 0)
test2c = TestCase $ assertEqual "iota 1" [0] (iota 1)

-- inter
test3a = TestCase $ assertEqual "inter [2,1] [1]" [1] (inter [2,1] [1])
test3b = TestCase $ assertEqual "inter [1] [1,2]" [1] (inter [1] [1,2])
test3c = TestCase $ assertEqual "inter [2,1] []" [] (inter [2,1] [])
test3d = TestCase $ assertEqual "inter [] [1,2]" [] (inter [] [1,2])
test3e = TestCase $ assertEqual "inter [2,3,4,1] [7,1,0]" [1] (inter [2,3,4,1] [7,1,0])
test3f = TestCase $ assertBool "inter [1,2] [1,2]" (let r = inter [1,2] [1,2] in r == [1,2] || r == [2,1])
test3g = TestCase $ assertBool "inter [3,4] [4,3]" (let r = inter [3,4] [4,3] in r == [3,4] || r == [4,3])
test3h = TestCase $ assertBool "inter [4,3] [3,4]" (let r = inter [4,3] [3,4] in r == [3,4] || r == [4,3])

-- interOrdered
test3i = TestCase $ assertEqual "interOrdered [1,2] [1]" [1] (interOrdered [1,2] [1])
test3j = TestCase $ assertEqual "interOrdered [1] [1,2]" [1] (interOrdered [1] [1,2])
test3k = TestCase $ assertEqual "interOrdered [1,2] []" [] (interOrdered [1,2] [])
test3l = TestCase $ assertEqual "interOrdered [] [1,2]" [] (interOrdered [] [1,2])
test3m = TestCase $ assertEqual "interOrdered [1,2,3,4] [0,1,7]" [1] (interOrdered [1,2,3,4] [0,1,7])
test3n = TestCase $ assertEqual "interOrdered [1,2,3,4] [2,4,5]" [2,4] (interOrdered [1,2,3,4] [2,4,5])
test3o = TestCase $ assertEqual "interOrdered [3,4] [3,4]" [3,4] (interOrdered [3,4] [3,4])

-- isMatch
test4a = TestCase $ assertEqual "isMatch \"aa\" \"a\"" False (isMatch "aa" "a")
test4b = TestCase $ assertEqual "isMatch \"aa\" \"*\"" True (isMatch "aa" "*")
test4c = TestCase $ assertEqual "isMatch \"cb\" \"?a\"" False (isMatch "cb" "?a")
test4d = TestCase $ assertEqual "isMatch \"adceb\" \"*a*b\"" True (isMatch "adceb" "*a*b")
test4e = TestCase $ assertEqual "isMatch \"acdcb\" \"a*c?b\"" False (isMatch "acdcb" "a*c?b")

-- for running all the tests (type "runTests" within GHCi -- without
-- the quotes)
runTests = runTestTT $ TestList
  [
    test2a, test2b, test2c,
    test3a, test3b, test3c, test3d, test3e, test3f, test3g, test3h, test3i, test3j, test3k, test3l, test3m, test3n, test3o,
    test4a, test4b, test4c, test4d, test4e
  ]
