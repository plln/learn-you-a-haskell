
doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x >= 100 then x else 2 * x

doubleSmallNumber' x = doubleSmallNumber x + 1

conanO'Brien :: String
conanO'Brien = "It's me, Conan O' Brien!"

oddList xs = [x | x <- xs, odd x]

twoTimes xs = [2 * x | x <- xs]

threeTimes xs = [3 * x | x <- xs]

times n xs = [n * x | x <- xs]

boomBang xs limit = [if x <= limit then "BOOM!" else "BANG!" | x <- xs, odd x]

combine :: [String] -> [String] -> [String]
combine adjs nouns = [adj ++ " " ++ noun | adj <- adjs, noun <- nouns]

length' xs = sum [1 | _ <- xs]

-- machine trying to be smart (1)
removeLowercase' :: String -> String
removeLowercase' str = [ch | ch <- str, ch `notElem` ['a' .. 'z']]

removeLowercase :: String -> String
removeLowercase str = [ch | ch <- str, ch `elem` ['A' .. 'Z']]

chrsWlkn :: (String, String, Int)
chrsWlkn = ("Christopher", "Walken", 55)

evenLists xxs = [[x | x <- xs, even x] | xs <- xxs]

-- machine trying to be smart (0)
zipNumbersSumGt10 xs ys = [(x, y) | (x, y) <- zip xs ys, x + y > 10]

zipTill xs limit = [(y, x) | (x, y) <- zip xs [1 ..], y <= limit]

rightTriangles = [(a, b, c) | a <- [1 .. 10], b <- [1 .. 10], c <- [1 .. 10], a ^ 2 + b ^ 2 == c ^ 2]

-- even more tight
rightTriangles' = [(a, b, c) | c <- [1 .. 10], b <- [1 .. c], a <- [1 .. b], a ^ 2 + b ^ 2 == c ^ 2]

rightTrianglesWithPerimeter = [(a, b, c) | (a, b, c) <- rightTriangles', a + b + c == 24]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial x = product [1 .. x]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

--Types and typeclasses
-- Standard Types: Int, Integer, Float, Double, Char, Bool, String
-- Typeclasses: Eq, Ord, Show, Read, Enum, Bounded, Num, Integral, Floating
-- Typeclass Instances: Int is an instance of Num, Eq, Ord, Show, Read
