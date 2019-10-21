module Graph(Graph,empty,addVertex,addEdge,vertices,neighbors) where

import Data.List

data Graph a = Graph 
  { vrtcs :: [a]
  , edges :: [(a,a)]
  } deriving Eq

empty :: Graph a
empty = Graph [] []

addVertex :: Eq a => Graph a -> a -> Graph a
addVertex (Graph vrtcs edges) node = 
    (Graph vrtcs' edges)
        where vrtcs' = nub $ node : vrtcs

addEdge :: Eq a => Graph a -> (a,a) -> Graph a
addEdge graph (v1,v2) = 
    let graph' = (addVertex graph v2)
        graph'' = (addVertex graph v1)
        vrtcs' = vrtcs graph''
        edges' = (v1, v2) : edges graph''
    in (Graph vrtcs' edges')

vertices :: Eq a => Graph a -> [a]
vertices (Graph vrtcs edges) = vrtcs

neighbors :: Eq a => Graph a -> a -> [a]
neighbors graph@(Graph vrtcs edges) node = 
    [ v2 | (v1,v2) <- edges, v1 == node] 
    ++ [ v1 | (v1,v2) <- edges, v2 == node]

{-
neighbours :: Graph a -> a -> [a]
neighbours g@(vs,es) v = es ! v
-}
{- 2 -}
-- For all operations, state their (worst-case) time complexity,
-- expressed as a function of the number of vertices or edges (or
-- both) that are contained in the graph.
{-
1. addVertex: T(n) = 1, since we insert in the beggining of the list
2. addEdge: T(n) = 1, since we insert three times to the beginning of the list
3. vertices: T(n) = 1, since we return a list of objects
4. neighbors: T(n) = 2n, since we go throw the list of edges twice to look for first element and then for the second
-}