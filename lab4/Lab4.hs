-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/

module Lab4(dot, connectedComponent) where

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

-- import your Graph module
import Graph

{- 3 -}
{- dotPrint Graph, String
simple usage of concatMap for going through all vertices and their neighbours
PRE: Valid string types
RETURNS: String
-}
dotPrint ::  Graph String -> String -> String
dotPrint graph vs = "  " ++ vs ++ ";" ++ "\n" ++ (concatMap (dotPrint' vs) (neighbors graph vs))


{- dotPrint' Graph, String
simple usage of concatMap for going through all vertices and their neighbours
PRE: Valid string types
RETURNS: String
-}
dotPrint' :: String -> String -> String
dotPrint' vs ns = "  " ++ vs ++ " -- " ++ ns ++ ";\n"

{- dot Graph
PRE: String type
RETURNS: String
-}
dot :: Graph String -> String
dot graph = "graph {" ++ "\n" ++ concatMap (dotPrint graph) (vertices graph)  ++ "}"

{- 4 -}
{- connectedComponent Graph a, vertex a
uses dfs to get all the nodes which are reachable from the node
internal foldl adds all vertices to the graph which is initialized with start point
external foldl adds all edges based on the original graph and removes the dublicates
PRE: match types of args
RETURNS: Graph a
-}
connectedComponent :: Eq a => Graph a -> a -> Graph a
connectedComponent g vrtx | vrtx `notElem`  vertices g = empty
                            --adds all the nodes and edges to the created graph   
                          | otherwise = foldl addEdge (foldl (\graph x -> addVertex graph x) initialGraph nodes) edges
                                --gets all neighbors of the nodes retrieved in dfs
                                where 
                                    nodes = concatMap (neighbors g) (dfs g vrtx) 
                                    -- init the graph with the start node
                                    initialGraph = addVertex empty vrtx
                                    edges = removeDub [(x,y) | x <- (vertices g), y <- (neighbors g x), x `elem` dfs g vrtx]


{- removeDub [(a,a)]
Removes dublicates from the edges if (a,b) and (b,a) exist
RETURNS: list of tuples
VARIANT: list [(a,a)]
-}                                    

removeDub ::Eq a => [(a,a)] -> [(a,a)]
removeDub [] = []
removeDub ((x,y):xs) | (y,x) `elem` xs = removeDub xs
                     | otherwise = (x,y) : removeDub xs


{- dfs Graph a, a
Getting the Graph and a vertex to start from
PRE: args match types
RETURNS: list of a
-}

dfs :: Eq a => Graph a -> a -> [a]
dfs graph node | graph == empty = []
                 -- stop if this is the node
               | [x | x<-(vertices graph), x==node] == [] = []
               | otherwise = dfs' (vertices graph) graph [node]

{- help function for dfs
PRE: list of vertices, graph, list
RETURNS: list of a
VARIANT: list of nodes, last arg
-}
dfs' [] _ _ = []
dfs' _ _ [] = []
dfs' vs graph (top:stack) | [x | x <- vs, x==top] == [] = dfs' nextv graph stack
                        -- append the current one and go to the next node
                     | otherwise = top : dfs' nextv graph (adjacent ++ stack)
                     where
                        -- getting all neibourhs for currently viewed node 
                        adjacent = neighbors graph top
                        -- getting the next node
                        nextv = [x|x<-vs,x/=top]