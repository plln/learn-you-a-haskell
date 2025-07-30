-- pattern matching

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you are out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Apple"
charName 'b' = "Banana"
charName 'c' = "Cherry"
charName _ = "Unknown character"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

sumTuples :: (Num a) => [(a, a)] -> [a]
sumTuples xs = [a + b | (a, b) <- xs]

sumTriples :: (Num a) => [(a, a, a)] -> [a]
sumTriples xs = [a + b + c | (a, b, c) <- xs]

head' :: [a] -> a
head' [] = error "Cant call head on empty list!"
head' (x : _) = x

myHead :: [a] -> a
myHead [] = error "Cant call head on empty list!"
myHead xs = xs !! 0

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x
tell (x : [y]) = "The list has 2 elements: " ++ show x ++ " and " ++ show y
tell (x : y : _) = "The list is long, the first 2 elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_ : xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

product' :: (Num a) => [a] -> a
product' [] = 1
product' (x : xs) = x * product' xs

-- guards

densityTell :: (RealFloat a) => a -> a -> String
densityTell mass volume
  | density < air = "Wow! You are going for a ride in the sky!"
  | density <= water = "Have fun swimming!"
  | otherwise = "You are going to sink!"
  where
    density = mass / volume
    air = 1.2
    water = 1000.0

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

-- you can also use where bindings to pattern match!
initials :: String -> String -> String
initials firstName lastName = [f] ++ "." ++ [l] ++ "."
  where
    (f : _) = firstName
    (l : _) = lastName

initials' :: String -> String -> String
initials' (f : _) (l : _) = [f] ++ "." ++ [l] ++ "."

-- just like we have defined constants in where blocks, we can also define functions.
calcDensities :: (RealFloat a) => [(a, a)] -> [a]
calcDensities xs = [density m v | (m, v) <- xs]
  where
    density mass volume = mass / volume

calcDensitiesAndTell :: (RealFloat a) => [(a, a)] -> [String]
calcDensitiesAndTell xs = [densityTell m v | (m, v) <- xs]

-- where bindings can also be nested

-- let bindings

-- case expressions