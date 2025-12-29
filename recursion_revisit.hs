-- recursion

maximum' :: (Ord a) => [a] -> a
maximum' [x] = x
maximum' (x : xs)
  | x > maxTail = x
  | otherwise = maxTail
  where
    maxTail = maximum' xs

myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "empty list!"
myMaximum [x] = x
myMaximum (x : xs) = max x (myMaximum xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = last xs : reverse' (init xs)

-- reverse' (x : xs) = reverse' xs ++ [x]

repeat' :: (Num i, Ord i) => i -> a -> [a]
repeat' n x
  | n <= 0 = []
  | otherwise = x : repeat' (n - 1) x

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' i (x : xs)
  | i == x = True
  | otherwise = elem' i xs

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys