-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/

module Lab3(Fruit(Apple,Banana,Lemon),sumPrice,BSTree(Void,BSNode),subTree,Tree(Node),count,labels,height,(++),elem,last,reverse,filter) where

import Prelude hiding ((++),elem,last,reverse,filter)

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- 1.1 -}
-- remember to provide the datatype representation
data Fruit = Apple Double
 | Banana Double
 | Lemon Int

{- 1.2 -}
-- remember to provide a function specification
sumPrice :: [Fruit] -> Double -> Double -> Double -> Double
sumPrice ((Apple a):xs) ap bp lp = a*ap + sumPrice xs ap bp lp
sumPrice ((Banana a):xs)ap bp lp = a*bp + sumPrice xs ap bp lp
sumPrice ((Lemon a):xs) ap bp lp = (fromIntegral a) * lp + sumPrice xs ap bp lp
sumPrice [] _ _ _ = 0
{-
sumPrice f a b l
take a lists of fruits and sum the fruits price given in a(apple) b(banana) l(lemon)
PRE: you must have a list with fruit
RETURNS : sum of fruit prices
EXAMPLES: sumPrice [Banana 4.0, Apple 3.0, Lemon 7, Banana 2.0, Apple 1.0, Lemon 1] 3.0 2.0 5.0 == 64.0
	  sumPrice [Apple 2.0] 2.0 4.0 8.0 == 4.0
sumPrice :: [Fruit] -> Double -> Double -> Double -> Double
-}

{- 2 -}

{- Binary search trees

   Void represents an empty tree. BSNode l x r represents a tree with
   left subtree l, root label x, and right subtree r.

   INVARIANT: in every tree of the form BSNode l x r, all labels in l
     are < x, and all labels in r are > x.
 -}
data BSTree = Void | BSNode BSTree Integer BSTree deriving Show -- do not modify this line

-- remember to provide a function specification
subTree :: Integer -> Integer -> BSTree -> BSTree
subTree _ _ Void = Void
subTree a b (BSNode lt self rt) 
 | self < a = subTree a b rt
 | self >= a && self < b = (BSNode (subTree a b lt)  self  (subTree a b rt))
 | self >= b = subTree a b lt
{-
subTree a b (lt self rt)
take two numbers and traverse the given BSTree(BSNode lt self rt), and return a BSTree which value x | a <= x < b
PRE: you should pass the BSTree as argument
RETURNS : a BSTree with all of the value greater than and equal a and smaller than b
EXAMPLES: subTree 5 8 (	BSNode (BSNode (BSNode Void 0 (BSNode Void 2 Void))
                   3
                   (BSNode Void 5 Void))
           6
           (BSNode Void
                   7
                   (BSNode Void 8 (BSNode Void 9 Void))))
			== BSNode (BSNode Void 5 Void) 6 (BSNode Void 7 Void)
subTree :: Integer -> Integer -> BSTree -> BSTree
-}

{- 3.1 -}
-- remember to provide the datatype representation
data Tree a = Node a [Tree a] deriving Show

{- 3.2 a) -}
-- remember to provide a function specification
count :: Tree a -> Integer
count (Node a []) = 1
count (Node a b) = 1 + sum (map count b)
{-
count (Node a [Tree a])
take a Tree and count the nodes on the Tree
PRE: you should pass the Tree as argument
RETURNS : number of nodes on the Tree
EXAMPLES: count (Node 1 [Node 2 []]) == 2
count:: Tree a -> Interger
-}

{- 3.2 b) -}
-- remember to provide a function specification
labels:: Tree a -> [a]
labels (Node a []) = [a]
labels (Node a b) = a : concatMap labels b
{-
labels (Node a [Tree a])
take a Tree and list the nodes on the Tree
PRE: you should pass the Tree as argument
RETURNS : a list of nodes on the Tree
EXAMPLES: labels (Node 1 [Node 2 []]) == [1,2] or ==[2,1]
labels:: Tree a -> [a]
-}

{- 3.2 c) -}
-- remember to provide a function specification
height:: Tree a -> Integer
height (Node a []) = 1
height (Node a b) = 1 + maximum (map height b)
{-
height (Node a [Tree a])
take a Tree and traverse the height
PRE: you should pass the Tree as argument
RETURNS : the height of the Tree
EXAMPLES: height (Node 1 [Node 2 []]) == 2
height:: Tree a -> Integer
-}

{- 4.1 -}
-- remember to provide a function specification
(++)::[a] -> [a] -> [a]
(++) x y = foldr (:) y x 
{-
(++) x y
take two lists and concat them into one
PRE: you should pass two lists with same type
RETURNS : a list of x concat y
EXAMPLES: [1,2,3] ++ [4,5,6] == [1,2,3,4,5,6]
(++):: [a] -> [a] -> [a]
-}

{- 4.2 -}
-- remember to provide a function specification
elem :: Eq a => a -> [a] -> Bool
elem x y = foldl (\a b -> if x==b then True else a) False y
{-
elem x y
take a element and a list, return true if the element is in the list
PRE: you should pass two arguments in the list with same type
RETURNS : true if the element in the list
EXAMPLES: elem 1 [1,2,3] == True
elem:: a -> [a] -> Bool
-}

{- 4.3 -}
-- remember to provide a function specification
last :: [a] -> a
last x = foldl1 (\_ x -> x) x 
{-
last x 
take a list, return the last value
PRE: you should pass two lists with same type
RETURNS : the last element in the list
EXAMPLES: last ['a','b','c'] == 'c'
last:: [a] -> a
-}

{- 4.4 -}
-- remember to provide a function specification
reverse:: [a] -> [a]
reverse x = foldl (\a b -> (b:a) ) [] x 
{-
reverse x
take a list and return its reverse
PRE: you should pass a list
RETURNS : the reversed list
EXAMPLES: reverse [1,2,3,4,5] == [5,4,3,2,1]
reverse:: [a] -> [a]
-}

{- 4.5 -}
-- remember to provide a function specification
filter ::(a -> Bool) -> [a] -> [a]
filter p x = foldr (\a b -> if p a then (a:b) else b) [] x
{-
filter x y
take a condition statement and a list, return a list of elements that satisfy the condition 
PRE: you should pass the correct type of condition statement
RETURNS : a list of elements which satisfy the condition
EXAMPLES: filter (>6) [1,3,5,6,7,8] == [7,8]
filter ::(a -> Bool) -> [a] -> [a]
-}



