-- 
-- Actual Work from CIS 194
-- 
{-# OPTIONS_GHC -Wall -Werror #-} -- this would work for the below

toDigitsRev :: Integer -> [Integer]
-- Determine the smallest digit of a by modulo 10 the number and add the result
-- to the list, continue by recursing the function and dividing a by 10 to
-- remove the smallest digit. Continue till a == 0.
toDigitsRev a | a > 0 =  a `mod` 10 : toDigitsRev (a `div` 10)
              | otherwise = []

toDigits :: Integer -> [Integer]
-- Apply reverse to the returned toDigitsRev list.
toDigits a = reverse (toDigitsRev a)

doubleEveryOther :: [Integer] -> [Integer]
-- If the parameter list is empty, return an empty list
doubleEveryOther [] = []
-- If the parameter list only contains one value, return an empty list.
doubleEveryOther [_] = []
-- For every two elements in the list, double the first, and append both to the
-- returned list.
doubleEveryOther (x:y:zs) = (x + x) : y : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
-- If given an empty list, return 0.
sumDigits [] = 0
-- Ensure the digits are correctly singular, and flatten the mapped list, sum
-- the concat'd list.
sumDigits xs = sum (concatMap toDigits xs)

validate :: Integer -> Bool
-- With a positive integer, determine if it's a clean multiple of 10, disregard
-- any integer 0 or less as False.
validate a | a > 0 = a `mod` 10 == 0
           | otherwise = False

cardNum :: [Integer]
cardNum = [4012888888881881,
           4012888888881812,
           5594589764218858,
           1234567898765432]

main :: IO ()
main = do
    print ([validate (sumDigits (doubleEveryOther (toDigits x))) | x <- cardNum])
