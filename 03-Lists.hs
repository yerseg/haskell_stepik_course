import Data.Char
import Data.List


addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y list = x : (y : list)


nTimes:: a -> Int -> [a]
nTimes elem n = f elem n []
    where f x n l | n == 0 = l 
                  | otherwise = f x (n - 1) (x : l)


sndHead ((_, x) : _) = x


oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x : xs) | odd x = x : oddsOnly xs 
                  | otherwise = oddsOnly xs


isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome (x : []) = True
isPalindrome (x : xs) = x == last xs && (isPalindrome . init $ xs)


sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 [] y z = sum3 [0] y z 
sum3 x [] z = sum3 x [0] z 
sum3 x y [] = sum3 x y [0] 
sum3 (x : xs) (y : ys) (z : zs) = (x + y + z) : sum3 xs ys zs


groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x : xs) = let (ys, zs) = span (== x) xs in (x : ys) : groupElems(zs)


readDigits :: String -> (String, String)
readDigits x = span isDigit x


filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj f g [] = []
filterDisj f g (x : xs) | f x || g x = x : filterDisj f g xs
                        | otherwise = filterDisj f g xs


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort xs@(x : xs') = qsort (filter (< x) xs') ++ [x] ++ qsort (filter (>= x) xs')


squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x ^ 2, x ^ 3])


perms :: [a] -> [[a]]
perms []     = [[]]
perms [x]    = [[x]]
perms (x : xs) = concatMap (\list -> map (\idx -> take idx list ++ [x] ++ drop idx list) [0..length(list)]) $ perms xs


delAllUpper :: String -> String
delAllUpper = unwords . filter (\x -> x /= "")  . map (\x -> if all isUpper x then "" else x) . words


max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 xs ys zs = zipWith3 (\x y z -> max x $ max y z) xs ys zs 


fibStream :: [Integer]
fibStream = [0, 1] ++ zipWith (+) fibStream (tail fibStream)

repeat :: a -> [a]
repeat = iterate repeatHelper
    where repeatHelper x = x


data Odd = Odd Integer 
  deriving (Eq, Show)

-- I'm so stupid... It can be solved using map and Odd ctor with couple of lines...
--
-- Perfect solution:
-- instance Enum Odd where
--   toEnum i = Odd(toInteger i)
--   fromEnum (Odd n) = fromEnum n

--   succ (Odd n) = Odd (n+2)
--   pred (Odd n) = Odd (n-2)

--   enumFrom (Odd n) = map Odd [n,n+2..]
--   enumFromTo (Odd n) (Odd m) = map Odd [n,n+2..m]
--   enumFromThen (Odd n) (Odd n') = map Odd [n,n'..]
--   enumFromThenTo (Odd n) (Odd n') (Odd m) = map Odd [n,n'..m]
instance Enum Odd where
    toEnum x = Odd (toInteger x)
    fromEnum (Odd x) = fromEnum x

    succ (Odd x) = Odd (x + 2)
    pred (Odd x) = Odd (x - 2)

    enumFrom x = x : enumFrom (succ x)

    enumFromThen (Odd x) (Odd y) | x < y = filter (\(Odd m) -> (m - x) `mod` (y - x) == 0 || m == x) $ enumFrom (Odd x)
                                 | x == y = Prelude.repeat (Odd x)
                                 | otherwise = filter (\(Odd m) -> (m - x) `mod` (x - y) == 0 || m == x) $ enumFromReveresed (Odd x)
                            where enumFromReveresed x = x : enumFromReveresed (pred x)

    enumFromTo (Odd x) (Odd y) | x <= y = takeWhile (\(Odd x) -> x <= y) $ enumFrom (Odd x)
                               | otherwise = []

    enumFromThenTo (Odd x) (Odd y) (Odd z) | z < x && x == y = []
                                           | x <= z && x <= y = takeWhile (\(Odd x) -> x <= z) $ enumFromThen (Odd x) (Odd y)
                                           | z <= x && y <= x = takeWhile (\(Odd x) -> x >= z) $ enumFromThen (Odd x) (Odd y)
                                           | otherwise = []
   

-- Get brain fuck out 
coins :: Num a => [a]; coins = [2, 3, 7]
change :: (Ord a, Num a) => a -> [[a]]
change n | n < 0     = []
         | n == 0    = [[]]
         | otherwise = [ x : xs | x <- coins, xs <- change (n - x) ]


concatList :: [[a]] -> [a]
concatList = foldr (++) []


lengthList :: [a] -> Int
lengthList = foldr (\_ x -> x + 1) 0


sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0


-- Can be used uncurry
meanList :: [Double] -> Double
meanList = mn . foldr (\x (count, sum) -> (count + 1, sum + x)) (0, 0)
    where mn (c, s) = s / c 


evenOnly :: [a] -> [a]
evenOnly = snd . foldl (\ (idx, list) x -> (idx + 1, if even idx then list ++ [x] else list)) (1, [])


evenOnly' :: [a] -> [a]
evenOnly' = snd . foldr (\x (idx, list) -> (idx + 1, if even idx then x : list else list)) (0, [])


evenOnly'' :: [a] -> [a]
evenOnly'' = snd . foldr (\a ~(xs, ys) -> (a : ys, xs)) ([], [])


lastElem :: [a] -> a
lastElem = foldl1 (\x xs -> xs)


revRange :: (Char,Char) -> [Char]
revRange = unfoldr g
    where g (x, y) | x > y = Nothing
                   | otherwise = Just (y, (x, pred y))








