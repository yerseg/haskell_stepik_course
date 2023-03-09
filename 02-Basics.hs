import Data.Function


getSecondFrom :: a -> b -> c -> b
getSecondFrom x y z = y


multSecond = g `on` h
g = (*)
h = snd


on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)


doItYourself = x . y . z
x = logBase 2
y = (^ 3)
z = max 42


class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString True = "true"
    toString _ = "false" 

instance Printable () where
    toString _ = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
    toString x = "(" ++ toString(fst x) ++ "," ++ toString(snd x) ++ ")"


class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab x | doesEnrageMork x && doesEnrageGork x = stomp $ stab x
                  | not (doesEnrageMork x) && doesEnrageGork x = stab x
                  | doesEnrageMork x && not (doesEnrageGork x) = stomp x
                  | not (doesEnrageMork x) && not (doesEnrageGork x) = x


class (Eq a, Bounded a, Enum a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x | x == maxBound = minBound
          | otherwise = succ x

  spred :: a -> a
  spred x | x == minBound = maxBound
          | otherwise = pred x


avg :: Int -> Int -> Int -> Double
avg x y z = (fromInteger((toInteger x) + (toInteger y) + (toInteger z)) / 3.0) :: Double
