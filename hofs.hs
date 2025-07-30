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

divideBy10 :: (Floating a) => a -> a
divideBy10 = (/ 10)

subtract4 :: (Num a) => a -> a
subtract4 = subtract 4

isUpperCase :: Char -> Bool
isUpperCase = (`elem` ['A' .. 'Z'])

-- all the above are partially applied functions that return function with left out parameters
