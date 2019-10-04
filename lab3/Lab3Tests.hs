-- DO NOT SUBMIT THIS FILE

module Lab3Tests where

import Prelude hiding ((++),elem,last,reverse,filter)

import Lab3

-- if the following line causes an error, type (at the terminal prompt):
--   cabal update
--   cabal install HUnit

import Test.HUnit -- see http://hackage.haskell.org/package/HUnit for
                  -- documentation if you want to add your own test
                  -- cases

-- Test Cases

-- sumPrice
test1a = TestCase $ assertEqual "sumPrice [] 3.0 2.0 5.0" 0.0 (sumPrice [] 3.0 2.0 5.0)
test1b = TestCase $ assertEqual "sumPrice [...] 3.0 2.0 5.0" 64.0
  (sumPrice [Banana 4.0, Apple 3.0, Lemon 7, Banana 2.0, Apple 1.0, Lemon 1] 3.0 2.0 5.0)

-- subTree

t = BSNode (BSNode (BSNode Void 0 (BSNode Void 2 Void))
                   3
                   (BSNode Void 5 Void))
           6
           (BSNode Void
                   7
                   (BSNode Void 8 (BSNode Void 9 Void)))

{- inOrderList t
   Converts a binary search tree into a list.
   RETURNS: a list containing all labels in t (in-order)
   EXAMPLES: inOrderList Void == [], inOrderList (BSNode Void 1 Void) = [1]
 -}
inOrderList :: BSTree -> [Integer]
inOrderList Void = []
inOrderList (BSNode l x r) = inOrderList l ++ (x : inOrderList r)

test2a = TestCase $ assertEqual "subTree 5 8 t" [5,6,7] (inOrderList $ subTree 5 8 t)
test2b = TestCase $ assertEqual "subTree 10 20 t" [] (inOrderList $ subTree 10 20 t)
test2c = TestCase $ assertEqual "subTree 0 1 t" [0] (inOrderList $ subTree 0 1 t)
test2d = TestCase $ assertEqual "subTree 1 2 t" [] (inOrderList $ subTree 1 2 t)

-- Tree

tree1 = Node "hej" []
tree2 = Node 1 [Node 2 []]
tree3 = Node "hej" [Node "hello" [Node "ni hao" [Node "ahoj" []]],
                    Node "bonjour" [Node "privet" [Node "guten tag" []]],
                    Node "namaste" [Node "ciao" [Node "as-salam alaykom" [Node "saluton" [Node "hei" [Node "halo" []]],
                                                                          Node "kon-nichiwa" [Node "an-nyong ha-se-yo " [Node "ola" []]],
                                                                          Node "sa-wat-dee" [Node "selam" [Node "jambo" []]]]]]]

-- count
test3a = TestCase $ assertEqual "count tree1" 1 (count tree1)
test3b = TestCase $ assertEqual "count tree2" 2 (count tree2)
test3c = TestCase $ assertEqual "count tree3" 19 (count tree3)

-- labels
test3d = TestCase $ assertEqual "labels tree1" ["hej"] (labels tree1)
test3e = TestCase $ assertBool "labels tree2" (let r = labels tree2 in r==[1,2] || r==[2,1])
test3f = TestCase $ assertEqual "labels tree3" 19 (length $ labels tree3)

-- height
test3g = TestCase $ assertEqual "height tree1" 1 (height tree1)
test3h = TestCase $ assertEqual "height tree2" 2 (height tree2)
test3i = TestCase $ assertEqual "height tree3" 7 (height tree3)

-- (++)
test4a = TestCase $ assertEqual "[] ++ [1,2]" [1,2] ([] ++ [1,2])
test4b = TestCase $ assertEqual "[3,2] ++ []" [3,2] ([3,2] ++ [])
test4c = TestCase $ assertEqual "[1,7,3,4,5] ++ [3,4,10,1]" [1,7,3,4,5,3,4,10,1] ([1,7,3,4,5] ++ [3,4,10,1])

-- elem
test4d = TestCase $ assertEqual "elem 'a' ['b']" False (elem 'a' ['b'])
test4e = TestCase $ assertEqual "elem 5 [1,2,3,4,200,100,5]" True (elem 5 [1,2,3,4,200,100,5])
test4f = TestCase $ assertEqual "elem 1 [1,2,3,4,200,100,5]" True (elem 1 [1,2,3,4,200,100,5])
test4g = TestCase $ assertEqual "elem 4 [1,2,3,4,200,100,5]" True (elem 4 [1,2,3,4,200,100,5])
test4h = TestCase $ assertEqual "elem 6 [1,2,3,4,200,100,5]" False (elem 6 [1,2,3,4,200,100,5])

-- last
test4i = TestCase $ assertEqual "last [1,2,3,4,200,100,5]" 5 (last [1,2,3,4,200,100,5])
test4j = TestCase $ assertEqual "last [\"X\"]" "X" (last ["X"])

-- reverse
test4k = TestCase $ assertEqual "reverse [\"X\"]" ["X"] (reverse ["X"])
test4l = TestCase $ assertEqual "reverse [1,2,3,4,200,100,5]" [5,100,200,4,3,2,1] (reverse [1,2,3,4,200,100,5])

-- filter
test4m = TestCase $ assertEqual "filter (>50) [1,2,3,4,200,100,5]" [200,100] (filter (>50) [1,2,3,4,200,100,5])
test4n = TestCase $ assertEqual "filter (<0) [1,2,3,4,200,100,5]" [] (filter (<0) [1,2,3,4,200,100,5])

-- for running all the tests (type "runTests" within GHCi -- without
-- the quotes)
runTests = runTestTT $ TestList
  [
    test1a, test1b,
    test2a, test2b, test2c, test2d,
    test3a, test3b, test3c, test3d, test3e, test3f, test3g, test3h, test3i,
    test4a, test4b, test4c, test4d, test4e, test4f, test4g, test4h, test4i, test4j, test4k, test4l, test4m, test4n
  ]
