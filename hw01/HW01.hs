{-
Name: Gary Fixler
Collaborators: Ben Deane
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW01 where         -- We'll learn more about this later

isThisWorking :: String
isThisWorking = "Yes"
-- Load this file into GHCi (say, with `ghci HW01.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

-- Put your work below.

lastDigit :: Integer -> Integer
lastDigit n = n - (n `div` 10 * 10)

dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

toDigits :: Integer -> [Integer]
toDigits n
    | n < 1     = []
    | otherwise = toDigits (dropLastDigit n) ++ [lastDigit n]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doDoubles . reverse
    where doDoubles [] = []
          doDoubles [x] = [x]
          doDoubles (x:y:ys) = x : y * 2 : doDoubles ys

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

validate :: Integer -> Bool
validate n = checksum `mod` 10 == 0
    where checksum = sumDigits . doubleEveryOther . toDigits $ n

