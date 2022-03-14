module Diy where
{-

-}


bmiTell :: Double -> Double -> String
bmiTell w h
   | bmi <= 18.5 = "underwight"
   | bmi <= 25.0 = "normal"
   | bmi <= 30.0 = "fat"
   | otherwise   = "whale"
   where bmi = w / h ^ 2


initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
   let smallerOrEqual = [a | a <- xs, a <= x]
       larger = [a | a <- xs, a > x]
   in quicksort smallerOrEqual ++ [x] ++ quicksort larger



isUpperAlpha :: Char -> Bool
isUpperAlpha = (`elem` ['A'..'Z'])