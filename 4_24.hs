

-- Collatz Conjecture
module CollatzConjecture (collatz) where


collatz :: Integer -> Maybe Integer
collatz n | n <= 0 = Nothing
          | n == 1 = Just 0
          | even n     = succ <$> collatz (n `div` 2)
          | otherwise  = succ <$> collatz (3 * n + 1)
		  

{-
so in the output there, the output of each step is seen in the right column, 
you start with 12, then apply the steps as described and reach 1 in 9 steps
Like in the end will be 1+1+1+1+1+1+1+1+1 + Just 0
return Just 9
succ is defined to increment its argument
-}