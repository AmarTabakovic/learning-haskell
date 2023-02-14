-- ====== Exercise 1 ======
myExp :: Integral a => a -> a -> a
myExp n 0 = 1
myExp n k = n * myExp n (k - 1)

-- myExp 2 3
-- 2 * (myExp 2 2)
-- 2 * (2 * (myExp 2 1))
-- 2 * (2 * (2 * (myExp 2 0)))
-- 2 * (2 * (2 * 1))
-- 2 * (2 * 2)
-- 2 * 4
-- 8

-- ====== Exercise 2 ======
-- 1. length:
-- length [1, 2, 3]
-- 1 + length [2, 3]
-- 1 + 1 + length [3]
-- 1 + 1 + 1 + length []
-- 1 + 1 + 1 + 0
-- 3

-- 2. drop:
-- drop 3 [1, 2, 3, 4, 5]
-- drop 2 [2, 3, 4, 5]
-- drop 1 [3, 4, 5]
-- drop 0 [4, 5]
-- [4, 5]

-- 3. init:
-- init [1, 2, 3]
-- 1 : init [2, 3]
-- 1 : 2 : init [3]
-- 1 : 2 : []
-- [1, 2]

-- ====== Exercise 3 ======
-- 1. recAnd
recAnd :: [Bool] -> Bool
recAnd [] = True
recAnd (x : xs) = x && recAnd xs

-- 2. recConcat
recConcat :: [[a]] -> [a]
recConcat [] = []
recConcat (xs : xss) = xs ++ recConcat xss

-- 3. recReplicate
recReplicate :: Int -> a -> [a]
recReplicate 0 _ = []
recReplicate n a = a : recReplicate (n - 1) a

-- 4. recSelect
recSelect :: [a] -> Int -> a
recSelect [] _ = error "index out of bounds"
recSelect (x : xs) 0 = x
recSelect (x : xs) i = recSelect xs (i - 1)

-- 5. recElem
recElem :: Eq a => a -> [a] -> Bool
recElem a [] = False
recElem a (x : xs)
  | a == x = True
  | otherwise = recElem a xs

-- ====== Exercise 4 ======
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- ====== Exercise 5 ======
msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge (msort xs') (msort xs'')
  where
    xs' = take halfLen xs
    xs'' = drop halfLen xs
    halfLen = length xs `div` 2

-- ====== Exercise 6 ======
-- 1. mySum
-- Step 1: Define the type
-- mySum :: [Int] -> Int

-- Step 2: Enumerate the cases
-- mySum [] =
-- mySum (n : ns) =

-- Step 3: Define the simple cases
-- mySum [] = 0
-- mySum (n : ns) =

-- Step 4: Define the other cases
-- mySum [] = 0
-- mySum (n : ns) = n + mySum ns

-- Step 5: Generalise and simplify
-- mySum :: Num a => [a] -> [a]
-- mySum ns = foldr (+) 0 ns

-- Final function:
mySum :: Num a => [a] -> a
mySum ns = foldr (+) 0 ns

-- 2. myTake
-- Step 1: Define the type
-- myTake :: Int -> [a] -> [a]

-- Step 2: Enumerate the cases
-- myTake 0 _ =
-- myTake _ [] =
-- myTake n (x : xs) =

-- Step 3: Define the simple cases
-- myTake 0 _ = []
-- myTake _ [] = []
-- myTake n (x : xs) =

-- Step 4: Define the other cases
-- myTake 0 _ = []
-- myTake _ [] = []
-- myTake n (x : xs) = x : myTake (n - 1) xs

-- Step 5: Generalise and simplify
-- myTake :: Integral i => i -> [a] -> [a]
-- myTake 0 _ = []
-- myTake _ [] = []
-- myTake n (x : xs) = x : myTake (n - 1) xs

-- Final function:
myTake :: Integral i => i -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake n (x : xs) = x : myTake (n - 1) xs

-- 3. myLast
-- Step 1: Define the type
-- myLast :: [a] -> a

-- Step 2: Enumerate the cases
-- myLast [] =
-- myLast [x] =
-- myLast (x : xs) =

-- Step 3: Define the simple cases
-- myLast [] = error "empty list"
-- myLast [x] = x
-- myLast (x : xs) =

-- Step 4: Define the other cases
-- myLast [] = error "empty list"
-- myLast [x] = x
-- myLast (x : xs) = myLast xs

-- Step 5: Generalise and simplify
-- myLast :: [a] -> a
-- myLast [] = error "empty list"
-- myLast [x] = x
-- myLast (x : xs) = myLast xs

-- Final function:
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (x : xs) = myLast xs