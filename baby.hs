doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x >= 100 then x else 2 * x

doubleSmallNumber' x = doubleSmallNumber x + 1

conanO'Brien = "It's me, Conan O' Brien!"

oddList xs = [x | x <- xs, odd x]

twoTimes xs = [2 * n | n <- xs]

threeTimes xs = [3 * x | x <- xs]

times n xs = [n * x | x <- xs]

boomBang xs limit = [if x <= limit then "BOOM!" else "BANG!" | x <- xs, odd x]

combine adjs nouns = [adj ++ " " ++ noun | adj <- adjs, noun <- nouns]

length' xs = sum [1 | _ <- xs]

-- machine trying to be smart (1)
removeLowercase' str = [ch | ch <- str, ch `notElem` ['a' .. 'z']]

removeLowercase str = [ch | ch <- str, ch `elem` ['A' .. 'Z']]

chrsWlkn = ("Christopher", "Walken", 55)

evenLists xxs = [[x | x <- xs, even x] | xs <- xxs]

-- machine trying to be smart (0)
zipNumbersTill10 xs ys = [(x, y) | (x, y) <- zip xs ys, x + y > 10]

zipTill xs limit = [(y, x) | (x, y) <- zip xs [1..], y <= limit]
