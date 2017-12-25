module Lib where

quicksort [] = []
quicksort (x:xs) = quicksort left ++ [x] ++ quicksort right
    where left  = [ y | y <- xs, y < x ]
          right = [ y | y <- xs, y >= x ]

signum :: Int -> Int
signum n = if n < 0 then -1 else
             if n == 0 then 0 else 1

abs n | n > 0 = n
      | otherwise = -n

negate :: Bool -> Bool
negate True = False
negate False = True


sayNumUpTo5 :: (Integral a) => a -> String
sayNumUpTo5 1 = "One"
sayNumUpTo5 2 = "Two"
sayNumUpTo5 3 = "Three"
sayNumUpTo5 4 = "Four"
sayNumUpTo5 5 = "Five"
sayNumUpTo5 _ = "Not between 1 and 5!"