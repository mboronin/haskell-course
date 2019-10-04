-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/

module Lab1(minus,fun1,fun2,fun3,fun4,fun5,fun6,sumSquareDiff) where

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- 1.1

       product 2
   --> let n = 2
          if 2 == 1 then 1 else n * product (n - 1)
          if False then 1 else n * product (n - 1)
          2 * product (2 - 1)
          2 * (if 1 == 1 then 1 else n * product (n - 1))
          2 * (if True then 1 else n * product (n - 1))
          2 * 1

   1.2

   product n
   take n integer
      Calculate factoria of n
      PRE: 0 <= n
      RETURNS: factorial of n
      EXAMPLES: product 1 == 1
                product 2 == 2
                product 3 == 6
    product :: Int -> Int

   1.3

   VARIANT: n or n+1 or n^2
 -}

{- 2.1 -}
-- remember to provide a function specification
minus = \x y -> x - y

{- 2.2
      minus = \x y -> x - y

      :t foo
      foo :: Integer

   2.3
      :t bar
      bar :: Integer -> Integer

   2.4

       minus 5 4
   --> x - 4
       5 - 4
       = 1

   .
   .
   .
 -}

{- 3.1 -}
-- remember to provide a function specification
fun1 :: Integer -> Integer; fun1 x = x + 1

{- 3.2 -}
-- remember to provide a function specification
fun2 :: Integer -> Integer -> Integer; fun2 x y = x - y

{- 3.3 -}
-- remember to provide a function specification
fun3 :: Integer -> (Integer, Integer); fun3 x = (x,x^2)

{- 3.4 -}
-- remember to provide a function specification
fun4 :: (Integer,Integer) -> Integer; fun4 (x,y) = x^y

{- 3.5 -}
-- remember to provide a function specification
fun5 :: Integer -> Double -> String -> String; fun5 x y z = show(fromIntegral x * y) ++ z

{- 3.6 -}
-- remember to provide a function specification
fun6 :: Integer -> String -> String -> Integer -> (Integer, String); fun6 x y z i = (x+i, y++z)

{- 4 -}

-- use helper functions as needed

-- remember to provide a function specification
sumList [] = 0; sumList (x:xs) = x + sumList xs; 
sum_square n = sumList [x^2 | x <- [1..n]]
square x = x*x; square_sum n = square (sumList [x | x <- [1..n]])

-- sum_square 0 = 0; sum_square n = n * n + sum_square(n-1)
-- counter 1 = 1; counter n = n + counter(n - 1); square x = x*x; square_sum n = square(counter n)
sumSquareDiff n = if n < 1 then 0 else square_sum n - sum_square n