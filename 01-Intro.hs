import Data.Char

main = putStrLn "Hello, world!"

lenVec3 x y z = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

sign x = if x == 0 then 0 else (if x > 0 then 1 else -1)

infix |-|
x |-| y = if x - y >= 0 then x - y else y - x 

-- logBase 4 (min 20 (9 + 7)) == logBase 4 $ min 20 $ 9 + 7

discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5 

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then 10 * digitToInt x + digitToInt y else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact (n - 2)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 && even n = - fibonacci (-n)
            | otherwise = fibonacci (-n)

fibonacci' :: Integer -> Integer
fibonacci' 0 = 0
fibonacci' n | n > 0 = helper (n - 1) 0 1
             | n < 0 && even n = - fibonacci' (-n)
             | otherwise = fibonacci' (-n)
helper 0 a b = b
helper n a b | n > 0 = helper (n - 1) b (a + b)
             | n < 0 = error "Fuck!"

seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = 
    let 
        helper 0 a b c = a
        helper n a b c = helper (n - 1) (a + b - 2 * c) a b
    in helper (n - 2) 3 2 1


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = helper (abs x) 0 0
    where
        helper :: Integer -> Integer -> Integer -> (Integer, Integer) 
        helper y n m | y >= 10 = helper (fst divmod) (n + snd divmod) (m + 1)
                     | otherwise = (n + y, m + 1) 
                     where divmod = divMod y 10


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = 
    let 
        h = (b - a) / 1000
        f_map = map f $ take 999 [(a + i * h) | i <- [1..999]]
    in h * ((f a + f b) / 2 + sum f_map)

