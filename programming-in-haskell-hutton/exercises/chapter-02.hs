-- ====== Exercise 1 ======
-- 1. (2 ^ 3) * 4
-- 2. (2 * 3) + (4 * 5)
-- 3. 2 + (3 * (4 ^ 5))

-- ====== Exercise 2 ======
-- Nothing to see here...

-- ====== Exercise 3 ======
n = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

-- n evaluates to 2

-- ====== Exercise 4 ======
last1 xs = head (drop (length xs - 1) xs)

last2 xs = head (reverse xs)

last3 xs = xs !! (length xs - 1)

-- ====== Exercise 5 ======
init1 xs = reverse (tail (reverse xs))

init2 xs = take (length xs - 1) xs