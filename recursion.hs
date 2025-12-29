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

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = last xs : myReverse (init xs)

repeat' :: Int -> a -> [a]
repeat' n x
  | n <= 0 = []
  | otherwise = x : repeat' (n - 1) x

-- replicate' and repeat' are the same

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x : xs)
  | a == x = True
  | otherwise = elem' a xs