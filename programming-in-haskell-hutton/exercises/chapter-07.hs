import Data.Char (chr, ord)

-- ====== Exercise 1 ======
exprOld :: (a -> b) -> (a -> Bool) -> [a] -> [b]
exprOld f p xs = [f x | x <- xs, p x]

exprNew :: (a -> b) -> (a -> Bool) -> [a] -> [b]
exprNew f p = map f . filter p

-- ====== Exercise 2 ======
-- 1. myAll
myAll :: (a -> Bool) -> [a] -> Bool
myAll p xs = length (filter p xs) == length xs

-- 2. myAny
myAny :: (a -> Bool) -> [a] -> Bool
myAny p xs = length (filter p xs) > 0

-- 3. myTakeWhile
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p [] = []
myTakeWhile p (x : xs)
  | p x = x : myTakeWhile p xs
  | otherwise = []

-- 4. myDropWhile
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile p [] = []
myDropWhile p (x : xs)
  | p x = myDropWhile p xs
  | otherwise = x : xs

-- ====== Exercise 3 ======
-- 1. myMap
myMap :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
myMap f = foldr (\a xs -> f a : xs) []

-- 2. myFilter
myFilter :: Foldable t => (a -> Bool) -> t a -> [a]
myFilter p = foldr (\a xs -> if p a then a : xs else xs) []

-- ====== Exercise 4 ======
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> y + 10 * x) 0

-- dec2int [2, 3, 4, 5] evaluates to 2345

-- ====== Exercise 5 ======
-- The reason is that the functions inside the list have different types.

-- ====== Exercise 6 ======
-- 1. myCurry
myCurry :: ((a, b) -> c) -> (a -> b -> c)
myCurry f = \x -> \y -> f (x, y)

-- 2. myUncurry
myUncurry :: (a -> b -> c) -> ((a, b) -> c)
myUncurry f = \(x, y) -> f x y

-- ====== Exercise 7 ======
-- Definitions from the book
type Bit = Int

myUnfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
myUnfold p h t x
  | p x = []
  | otherwise = h x : myUnfold p h t (t x)

-- Own definitions
-- 1. myChop8
myChop8 :: [Bit] -> [[Bit]]
myChop8 = myUnfold null (take 8) (drop 8)

-- 2. myMap2
myMap2 :: (a1 -> a2) -> [a1] -> [a2]
myMap2 f = myUnfold null (f . head) tail

-- 3. myIterate
myIterate :: (a -> a) -> a -> [a]
myIterate f = myUnfold (\y -> False) id f

-- ====== Exercise 8 ======
-- Definitions from the book
bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip weights bits]
  where
    weights = iterate (* 2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- Own definitions
make8Parity :: [Bit] -> [Bit]
make8Parity bits = bits' ++ [parityBit]
  where
    bits' = take 8 (bits ++ repeat 0)
    parityBit = if even (sum bits') then 0 else 1

chop8Parity :: [Bit] -> [[Bit]]
chop8Parity [] = []
chop8Parity bits
  | sum bits'' `mod` 2 == bits' !! 8 = bits'' : chop8Parity (drop 9 bits)
  | otherwise = error "parity bit error"
  where
    bits' = take 9 bits
    bits'' = init bits'

encodeWithParity :: String -> [Bit]
encodeWithParity = concat . map (make8Parity . int2bin . ord)

decodeWithParity :: [Bit] -> String
decodeWithParity = map (chr . bin2int) . chop8Parity

-- ====== Exercise 9 ======
transmit :: ([Bit] -> [Bit]) -> String -> String
transmit channel = decodeWithParity . channel . encodeWithParity

workingChannel :: [Bit] -> [Bit]
workingChannel = id

-- transmit workingChannel "Haskell is cool" evaluates to "Haskell is cool"

faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail

-- transmit faultyChannel "Haskell is cool" evaluates to: Exception: parity bit error