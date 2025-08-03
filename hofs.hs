-- curried functions
-- every function in Haskell officially takes only one parameter
-- if we call a function with too few parameters, we get back a partially applied function

max4 :: (Ord a, Num a) => a -> a
max4 = max 4

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multNineWith = multThree 9

multNineWithTwo = multNineWith 2

multNineWithTwoWithThree = multNineWithTwo 3

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

-- :t compare is (Ord a) => a -> a -> Ordering
-- :t compare 100 is (Ord a) => (a -> Ordering)

-- infix functions can be partially applied by using sections.
-- to section an infix function, simply surround it with parentheses
-- and only supply a parameter on one side

divideBy10 :: (Floating a) => a -> a
divideBy10 = (/ 10)

subtract4 :: (Num a) => a -> a
subtract4 = subtract 4

isUpperCase :: Char -> Bool
isUpperCase = (`elem` ['A' .. 'Z'])

-- all the above are partially applied functions that return function with left out parameters

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

-- when we call flip' f without parameters x and y, it will return an f that takes two parameters
-- but calls them flipped

myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f = g
  where
    g x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs
