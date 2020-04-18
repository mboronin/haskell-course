module Graph(Graph,empty,addVertex,addEdge,vertices,neighbors) where

import Data.List


{- represents a graph with vertices and edges
   vertices are stored as a simple list, edges are stored as a list of tuples which mean an undirected edge (a,b) (b,a) are the same thing
   all vertices and edges are of the same type
-}
data Graph a = Graph 
  { vrtcs :: [a]
  , edges :: [(a,a)]
  } deriving (Eq, Show)


{- empty Graph a
Return an instance of an emtpy graph
-}  
empty :: Graph a
empty = Graph [] []


{- addVertex Graph a, vertex a
Adds a vertex to a graph
PRE: type of first and second arg match
RETURNS: Graph a
-}
addVertex :: Eq a => Graph a -> a -> Graph a
addVertex (Graph vrtcs edges) node = 
    (Graph vrtcs' edges)
        where vrtcs' = nub $ node : vrtcs

{- addVertex Graph a, edge a,a
Adds an edge to a graph
PRE: type of first and second arg match
RETURNS: Graph a
-}        
addEdge :: Eq a => Graph a -> (a,a) -> Graph a
addEdge graph (v1,v2) = 
    let graph' = (addVertex graph v2)
        graph'' = (addVertex graph v1)
        vrtcs' = vrtcs graph''
        edges' = (v1, v2) : edges graph''
    in (Graph vrtcs' edges')

{- vertices Graph a
Returns vertices of a graph
PRE: Graph datatype 
RETURNS: list of a
-}    
vertices :: Eq a => Graph a -> [a]
vertices (Graph vrtcs edges) = vrtcs


{- neighbors Graph a, a
PRE: Graph datatype and second arg match 
RETURNS: list of a
-}
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
1. addVertex: T(n) = n^2, because of nub usage, https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#v:nub
2. addEdge: T(n) = 1, since we insert three times to the beginning of the list
3. vertices: T(n) = 1, since it just geets vertices from a graph
4. neighbors: T(n) = n, since we go throw the list of edges twice to look for first element and then for the second one
-}