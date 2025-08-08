-- recursion

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "it is an empty list"
maximum' [x] = x
maximum' (x : xs)
  | x > maxTail = x
  | otherwise = maxTail
  where
    maxTail = maximum' xs

myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "it is an empty list"
myMaximum [x] = x
myMaximum (x : xs) = max x (myMaximum xs)

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' 1 x = [x]
replicate' n x
  | n < 0 = error "invalid input value"
  | otherwise = x : replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs
