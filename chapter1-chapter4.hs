doubleMe x = x + x

doubleUs x y = x*2 + y*2　

doubleSmallNumber x = if x　> 100
						then x
						else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y ,_) = y

third :: (a, b, c) -> c
third (_, _ ,z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and "++ show y
tell (x:y:_) = "The list is long. The first two elements: " ++ show x ++ " and "++ show y

length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
	| bmi <= 18.5 = "You're underweight, you emo, u!"
	| bmi <= 25.0 = "You're supposedly normal. Pffft, i bet you're ugly"
	| bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
	| otherwise = "You're a whale, congratulations!"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
	| weight / height ^ 2 <= 18.5 = "You're underweight, you emo, u!"
	| weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, i bet you're ugly"
	| weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
	| otherwise = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
	| a > b = a
	| otherwise = b

myCompare :: (Ord a) => a -> a ->Ordering
a `myCompare` b
	| a > b = GT
	| a == b = EQ
	| otherwise = LT
	
bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' weight height
	| bmi <= skinny = "You're underweight, you emo, you!"
	| bmi <= normal = "You're supposedly normal. Pffft, i bet you're ugly"
	| bmi <= fat = "You're fat! lose some weright! fatty"
	| otherwise = "You're a whale, congratulations!"
	where
		bmi = weight / height ^ 2
		(skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
	where 
		(f:_) = firstname
		(l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
	where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
	let 
		sideArea = 2 * pi * r * h
		topArea = pi * r ^ 2
	in
		sideArea + 2 * topArea

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcBmis'' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis'' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

describleList :: [a] -> String
describleList xs = "The list is" ++ case xs of 
	[] -> "empty."
	[x] -> "a singleton list. "
	xs -> "a longer list."


describleList' ::[a] -> String
describleList' xs = "This list is " ++ what xs
	where 
		what [] = "empty."
		what [x] = "a singleton list."
		what xs = "a longer list."
