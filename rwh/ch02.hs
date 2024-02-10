-- FROM: Real World Haskell
double a = a + a
neg a = -a

myDrop n xs =
    if n <= 0 || null xs
    then xs
    else myDrop (n - 1) (tail xs)

isOdd n =
    mod n 2 == 1

isEven n = not (isOdd n)

lastButOne xs =
    last (init xs)
