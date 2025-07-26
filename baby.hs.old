doubleMe x = x + x

tripleMe x = x + x + x

doubleUs x y = tripleMe x + tripleMe y

doubleSmallNumber x = if x > 100 then x else doubleMe x

doubleSmallNumber' x = doubleSmallNumber x + 1

-- Set comprehension
doubleNumbers xs = [ 2*x | x <- xs ]

boomBang xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]

multiply xs ys = [ x * y | x <- xs, y <- ys, x * y > 50 ]

length' xs = sum [ 1 | _ <- xs ]

removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase str = [ ch | ch <- str, ch `elem` ['A'..'Z']]

even' xxs = [ [ x | x <- xs, even x ] | xs <- xxs]

rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

-- Eq Typeclass == /=

-- Ord Typeclass > < >= <= compare

-- Show show

-- Read read

-- Patter matching

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you are out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "No between 1 and 5"

factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

charName :: Char -> String
charName 'a' = "Albert Einstein"
charName 'b' = "Broccoli"
charName 'c' = "Caesar"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

head' :: [a] -> a
head' [] = error "Cant call head on an empty list!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element " ++ show x
tell (x:y:[]) = "The list has 2 elements " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long, the first 2 elements are " ++ show x ++ " and " ++ show y

len' :: (Num b) => [a] -> b
len' [] = 0
len' (_:xs) = 1 + len' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- guards 
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You are underweight"
    | bmi <= 25.0 = "You are normal"
    | bmi <= 30.0 = "You are fat"
    | otherwise = "You are a whale"


max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ "." ++ [l] ++ "."

initials' :: String -> String -> String
initials' firstName lastName = [f] ++ [l]
    where (f:_) = firstName
          (l:_) = lastName