-- Towers of Hanoi
{-# OPTIONS_GHC -Wall -Werror #-}

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c | n > 1 = hanoi (n-1) a c b ++ [(a, c)] ++ hanoi (n-1) b a c
              | otherwise = [(a, c)]

main :: IO ()
main =
    print (length (hanoi 15 "a" "b" "c"))
