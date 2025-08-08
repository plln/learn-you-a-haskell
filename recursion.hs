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
myMaximum (x:xs) = max x (myMaximum xs)

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' 1 x = [x]
replicate' n x = x : replicate' (n - 1) x
