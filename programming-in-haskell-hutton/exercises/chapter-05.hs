import Data.Char (chr, isLower, isUpper, ord, toLower, toUpper)

-- ====== Exercise 1 ======
hundredSquareIntegers = [x ^ 2 | x <- [1 .. 100]]

-- ====== Exercise 2 ======
myReplicate :: Int -> a -> [a]
myReplicate n a = [a | _ <- [1 .. n]]

-- myReplicate 3 True evaluates to [True, True, True]

-- ====== Exercise 3 ======
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- set, y <- set, z <- set, x ^ 2 + y ^ 2 == z ^ 2]
  where
    set = [1 .. n]

-- pyths 10 evaluates to [(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]

-- ====== Exercise 4 ======
perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], x == sum [y | y <- [1 .. x - 1], x `mod` y == 0]]

-- perfects 500 evaluates to [6, 28, 496]

-- ====== Exercise 5 ======
expr = concat [[(x, y) | y <- [4 .. 6]] | x <- [1 .. 3]]

-- expr evaluates to [(1, 4), (1, 5), (1, 6), (2, 4), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6)]

-- ====== Exercise 6 ======
myFind :: Eq a => a -> [(a, b)] -> [b]
myFind k t = [v | (k', v) <- t, k == k']

positionsWithFind :: Eq a => a -> [a] -> [Int]
positionsWithFind x xs = [i | i <- myFind x y]
  where
    y = zip xs [0 .. (length xs)]

-- positionsWithFind 'a' "abcdabcaabc" evaluates to [0, 4, 7, 8]

-- ====== Exercise 7 ======
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- scalarproduct [1,2,3] [4,5,6] evaluates to 32

-- ====== Exercise 8 ======
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = toUpper (shift n (toLower c))
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- encode (-1) "Bnbs Ubcblpwjd" evaluates to "Amar Tabakovic"