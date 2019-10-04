-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/

module Lab2(iota,inter,interOrdered,isMatch) where

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- 1

    1. head :: [a] -> a
    2. tail :: [a] -> [a]
    3. \x -> x :: a -> a
    4. (,) :: a -> b -> (a,b)
    5. (:) :: a -> [a] -> [a]
    6. [[]] :: [[a]]
    7. tail [[]] :: [a]
    8. id : []  :: [a -> a]
    9. id id :: a -> a
   10. head [id] "foo" :: [Char]

   The polymorphic expressions are 1-9, 10 is specific for Char
 -}

{- 2 -}
-- remember to provide a function specification
iota:: Int -> [Int]
iota x
 | x<0 = error "negative number"
 | x<1 = []
 | otherwise = iota2 x 0

iota2:: Int -> Int -> [Int]
iota2 x y
 | x<y = error "something goes wrong"
 | x==y = [] 
 | otherwise = (y:iota2 x (y+1))
 
{-
iota n
take an interger and make a list from 0 to n-1
PRE: 0 < n
RETURNS : a list of 0 to n-1
EXAMPLES: iota 1 == [0]
	  iota 2 == [0,1]
iota :: Int -> [Int]

iota2 x y
take two intergers and make a list from y to x-1
PRE: y<x
RETURNS : a list of y to x-1
EXAMPLES: iota 1 0 == [0]
	  iota 2 0 == [0,1]
iota2 :: Int -> Int -> [Int]
-}

{- 3.1 -}
-- remember to provide a function specification
inter (x:s1) s2 | s1 == [] = if elem x s2 then [x] else []
                | elem x s2 = x:inter s1 s2
                | otherwise = inter s1 s2
inter x s2 = []
{-
inter s1 s2
take unordered two lists and make a list of elements that are in s1 and s2
PRE: you must have two lists with same type
RETURNS : a list of elements in both s1 s2 (ordered like the one in s1)
EXAMPLES: inter [1..10] [1,3..10] == [1,3,5,7,9]
	  inter ['a','b','c'] ['z','h','b','c'] == "bc"
inter :: (Eq a, Foldable t) => [a] -> t a -> [a]
-}

{- 3.2 -}
-- remember to provide a function specification
interOrdered (x:s1) (y:s2) | x==y  = if s1==[] || s2==[] then [x] else x : interOrdered s1 s2
                           | x < y = if s1==[] then [] else interOrdered s1 (y:s2)
                           | x > y = if s2==[] then [] else interOrdered (x:s1) s2
interOrdered s1 s2 = []
{-
interOrdered s1 s2
take ordered two lists and make a list of elements that are in s1 and s2
PRE: you must have two lists with same type
RETURNS : a list of elements in both s1 s2 (ordered)
EXAMPLES: interOrdered [1..10] [1,3..10] == [1,3,5,7,9]
	  interOrdered [1..100] [1,4..91] == [1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,58,61,64,67,70,73,76,79,82,85,88,91]
interOrdered :: Ord a => [a] -> [a] -> [a]
-}

{- 3.3 -}

s1 = iota 100000
s2 = iota 1000000

t1 = inter s1 s2
t2 = interOrdered s1 s2

{-
*A> length s1
100000
(0.08 secs, 33,661,312 bytes)
*A> length s2
1000000
(0.69 secs, 336,062,584 bytes)
*A> length t1
100000
(33.88 secs, 25,660,760 bytes)
*A> length t2
100000
(0.09 secs, 44,860,736 bytes)
-}

{- 4 -}
-- remember to provide a function specification
isMatch (x:s1) (y:s2) | x=='?' || y=='?' = isMatch s1 s2
                      | x=='*' = isMatch s1 s2 || isMatch (x:s1) s2 || isMatch s1 (y:s2)
					  | y=='*' = isMatch s1 s2 || isMatch s1 (y:s2) || isMatch (x:s1) s2
					  | x==y = isMatch s1 s2
                      | x/=y = False

isMatch x y | x==[] && y==[] = True
            | x==[] = allStar y
			| y==[] = allStar x

allStar (x:s1) | x=='*' && s1==[] = True
               | x=='*' = allStar s1
			   | otherwise = False
{-
isMatch s1 s2
take ordered two lists and tell if they are the same
PRE: you must have two lists with same type
RETURNS : return true if they are the same otherwise false
EXAMPLES: isMatch "adceb" "*a***b" == True
	isMatch "adceb" "adc?b" == True
	isMatch "adceb" "adc?c" == False
isMatch::[Char] -> [Char] -> Bool
-}

