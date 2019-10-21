-- DO NOT SUBMIT THIS FILE

module Lab4Tests where

  import qualified Graph
  import qualified Lab4
  
  -- if the following line causes an error, type (at the terminal prompt):
  --   cabal update
  --   cabal install HUnit
  
  import Test.HUnit -- see http://hackage.haskell.org/package/HUnit for
                    -- documentation if you want to add your own test
                    -- cases
  
  -- Test Cases
  
  g1 = Graph.addVertex (Graph.addVertex (Graph.addVertex Graph.empty "a") "b") "c"
  g2 = Graph.addEdge (Graph.addEdge (Graph.addEdge g1 ("a","b")) ("a","c")) ("b","c")
  g3 = Graph.addEdge (Graph.addEdge (Graph.addEdge g1 ("b","a")) ("c","a")) ("c","b")
  
  -- vertices
  test1a = TestCase $ assertBool "vertices empty" (null $ Graph.vertices (Graph.empty :: Graph.Graph ()))
  test1b = TestCase $ assertEqual "vertices [\"a\"]" ["a"] (Graph.vertices (Graph.addVertex Graph.empty "a"))
  test1c = TestCase $ assertEqual "vertices g1" 3 (length $ Graph.vertices g1)
  test1d = TestCase $ assertEqual "vertices g2" 3 (length $ Graph.vertices g2)
  test1e = TestCase $ assertEqual "vertices g3" 3 (length $ Graph.vertices g3)
  
  -- neighbors
  test1f = TestCase $ assertBool "neighbors g1 \"a\"" (null $ Graph.neighbors g1 "a")
  test1g = TestCase $ assertEqual "neighbors g2 \"a\"" 2 (length $ Graph.neighbors g2 "a")
  test1h = TestCase $ assertEqual "neighbors g3 \"a\"" 2 (length $ Graph.neighbors g3 "a")
  
  -- dot (these tests always succeed---you will need to inspect the
  -- output manually)
  test3a = TestCase $ putStrLn "\ndot g1:" >> putStrLn (Lab4.dot g1)
  test3b = TestCase $ putStrLn "\ndot g2:" >> putStrLn (Lab4.dot g2)
  test3c = TestCase $ putStrLn "\ndot g3:" >> putStrLn (Lab4.dot g3)
  {-
  -- connectedComponent
  test4a = TestCase $ assertEqual "vertices (connectedComponent g1 \"a\")" ["a"] (Graph.vertices (Lab4.connectedComponent g1 "a"))
  test4b = TestCase $ assertBool "neighbors (connectedComponent g1 \"a\") \"a\"" (null $ Graph.neighbors (Lab4.connectedComponent g1 "a") "a")
  test4c = TestCase $ assertEqual "vertices (connectedComponent g2 \"a\")" 3 (length $ Graph.vertices (Lab4.connectedComponent g2 "a"))
  test4d = TestCase $ assertEqual "neighbors (connectedComponent g2 \"a\") \"a\"" 2 (length $ Graph.neighbors (Lab4.connectedComponent g2 "a") "a")
  test4e = TestCase $ assertEqual "vertices (connectedComponent g3 \"a\")" 3 (length $ Graph.vertices (Lab4.connectedComponent g3 "a"))
  test4f = TestCase $ assertEqual "neighbors (connectedComponent g3 \"a\") \"a\"" 2 (length $ Graph.neighbors (Lab4.connectedComponent g3 "a") "a")
  -}
  -- for running all the tests (type "runTests" within GHCi -- without
  -- the quotes)
  runTests = runTestTT $ TestList
    [
      test1a, test1b, test1c, test1d, test1e, test1f, test1g, test1h,
      test3a, test3b, test3c
      --test4a, test4b, test4c, test4d, test4e, test4f
    ]
  