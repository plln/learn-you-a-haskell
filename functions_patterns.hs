allowUpperCaseOnly :: String -> String
allowUpperCaseOnly st = [ch | ch <- st, ch `elem` ['A' .. 'Z']]

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck pal!"

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

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

sumPairs :: (Num a) => [(a, a)] -> [a]
sumPairs xs = [a + b | (a, b) <- xs]

genPairs :: (Num a, Num b, Enum a, Enum b) => a -> b -> [(a, b)]
genPairs a b = [(x, y) | x <- [1 .. a], y <- [1 .. b]]

head' :: [a] -> a
head' [] = error "cannot invoke head on empty list!"
head' (x : _) = x

tail' :: [a] -> [a]
tail' [] = error "cannot invoke tail on empty list"
tail' (_ : xs) = xs

last' :: [a] -> a
last' [] = error "cannot invoke last on empty list"
last' [x] = x
last' (_ : xs) = last xs

init' :: [a] -> [a]
init' [] = error "cannot invoke init on empty list"
init' [_] = []
init' (x : xs) = x : init' xs

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x
tell [x, y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x : y : _) =
  "This list is long, first two elements are: "
    ++ show x
    ++ " and "
    ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_ : xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

product' :: (Num a) => [a] -> a
product' [] = 1
product' (x : xs) = x * product' xs

capital :: String -> String
capital "" = "Empty string!"
capital all@(x : xs) =
  "The first letter of "
    ++ all
    ++ " is "
    ++ [x]
