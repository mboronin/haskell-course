-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/

module Lab4(dot,connectedComponent) where

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

-- import your Graph module
import Graph
import Data.List

{- 3 -}
-- Make connected components print it
-- remember to provide a function specification
dotPrint ::  Graph String -> String -> String
dotPrint graph vs = vs ++ ";" ++ "\n" ++ (concatMap (dotPrint' vs) (neighbors graph vs))

dotPrint' :: String -> String -> String
dotPrint' vs ns = vs ++ " -- " ++ ns ++ ";\n"
dot :: Graph String -> String
dot graph = "{\n" ++ concatMap (dotPrint graph) (vertices graph)  ++ "}"

{- 4 -}
-- remember to provide a function specification
connectedComponent :: Eq a => Graph a -> a -> Graph a
connectedComponent = undefined

bfs :: (a -> [a]) -> a -> [a]
bfs neighbors start = concat $ takeWhile (not . null) $ iterate neighbors