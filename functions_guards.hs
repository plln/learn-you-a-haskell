-- function guards

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
myCompare x y
  | x > y = GT
  | x == y = EQ
  | otherwise = LT

densityTell :: (RealFloat a) => a -> a -> String
densityTell mass volume
  | density < air = "You are going for a ride in the sky!"
  | density <= water = "Have fun swimming, but watch out for sharks!"
  | otherwise = "If it's sink or swim, you're going to sink."
  where
    density = mass / volume
    air = 1.2
    water = 1000.0

canDrive :: (Ord a, Num a) => a -> Bool -> String
canDrive age haveDL
  | age < 15 = "Too young to drive"
  | age < 18 && haveDL = "Still cannot drive alone"
  | age < 18 = "Apply for Learners License"
  | age < 58 && haveDL = "Good to drive!"
  | age < 58 = "Apply for Driving License"
  | otherwise = "Too old to drive"

initials :: String -> String -> String
initials (f : _) (l : _) =
  "Your initials are " ++ [f] ++ "." ++ [l] ++ "."

initials' :: String -> String -> String
initials' first last = "Your initials are " ++ [f] ++ "." ++ [l] ++ "."
  where
    (f : _) = first
    (l : _) = last

calcDensities :: (RealFloat a) => [(a, a)] -> [a]
calcDensities xs = [density m v | (m, v) <- xs]
  where
    density mass volume = mass / volume

densityTellMany :: (RealFloat a, Show a) => [(a, a)] -> [String]
densityTellMany xs = [show (m / v) ++ " = " ++ densityTell m v | (m, v) <- xs]