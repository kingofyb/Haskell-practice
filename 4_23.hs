

-- 是否为闰年
isLeapYear :: Integer -> Bool
isLeapYear year = isD 400 || (isD 4 && not (isD 100))
   where isD d = year `mod` d == 0
   
   
-- case
data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

divd :: Float -> Float
divd seconds = seconds / 31557600

ageOn :: Planet -> Float -> Float
ageOn planet seconds = earthYears / case planet of
  Earth   -> 1
  Mercury -> 0.2408467
  Venus   -> 0.61519726
  Mars    -> 1.8808158 
  Jupiter -> 11.862615
  Saturn  -> 29.447498
  Uranus  -> 84.016846
  Neptune -> 164.79132
  where
    earthYears = divd seconds
	
	
-- isPangram

import           Data.Char (toUpper)
isPangram :: String -> Bool
isPangram text = all (`elem` map toUpper text) ['A'..'Z']