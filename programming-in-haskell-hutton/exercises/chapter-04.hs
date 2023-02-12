-- ====== Exercise 1 ======
halve :: [a] -> ([a], [a])
halve xs
  | even (length xs) = splitAt (length xs `div` 2) xs
  | otherwise = error "uneven list length"

-- halve [1, 2, 3, 4, 5, 6] evaluates to ([1, 2, 3], [4, 5, 6])

-- ====== Exercise 2 ======
-- a. Conditional expression
safetailCondExpr :: [a] -> [a]
safetailCondExpr xs = if null xs then xs else tail xs

-- b. Guarded equation
safetailGuardedEq :: [a] -> [a]
safetailGuardedEq xs
  | null xs = xs
  | otherwise = tail xs

-- c. Pattern matching
safetailPatternMatching :: [a] -> [a]
safetailPatternMatching [] = []
safetailPatternMatching xs = tail xs

-- ====== Exercise 3 ======
or1 :: Bool -> Bool -> Bool
or1 True True = True
or1 True False = True
or1 False True = True
or1 False False = False

or2 :: Bool -> Bool -> Bool
or2 True _ = True
or2 _ True = True
or2 _ _ = False

or3 :: Bool -> Bool -> Bool
or3 False b = b
or3 b False = b
or3 _ _ = True

or4 :: Bool -> Bool -> Bool
or4 False False = False
or4 _ _ = True

-- ====== Exercise 4 ======
condAnd1 :: Bool -> Bool -> Bool
condAnd1 a b =
  if a == True
    then if b == True then True else False
    else False

-- ====== Exercise 5 ======
condAnd2 :: Bool -> Bool -> Bool
condAnd2 a b =
  if a == True
    then b
    else False

-- ====== Exercise 6 ======
mult = \x -> (\y -> (\z -> x * y * z))