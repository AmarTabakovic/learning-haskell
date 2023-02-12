-- ====== Exercise 1 ======
-- double (double 2)
-- (double 2) + (double 2)
-- (2 + 2) + (double 2)
-- (2 + 2) + (2 + 2)
-- 4 + (2 + 2)
-- 4 + 4
-- 8

-- ====== Exercise 2 ======
-- sum [x]
-- x + (sum [])
-- x + 0
-- x

-- ====== Exercise 3 ======
myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

-- my_product [2, 3, 4] evaluates to 24

-- ====== Exercise 4 ======
qsortReverse :: Ord a => [a] -> [a]
qsortReverse [] = []
qsortReverse (x : xs) = qsortReverse larger ++ [x] ++ qsortReverse smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b >= x]

-- qsortReverse [3, 5, 1, 4, 2] evaluates to [5, 4, 3, 2, 1]

-- ====== Exercise 5 ======
qsortStrict :: Ord a => [a] -> [a]
qsortStrict [] = []
qsortStrict (x : xs) = qsortStrict smaller ++ [x] ++ qsortStrict larger
  where
    smaller = [a | a <- xs, a < x]
    larger = [b | b <- xs, b >= x]

-- qsortStrict [2, 2, 3, 1, 1]
-- (qsortStrict [1, 1]) ++ [2] ++ (qsortStrict [2, 3])
-- ...
-- [1, 1, 2, 2, 3]