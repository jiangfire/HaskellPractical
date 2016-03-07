import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

serach :: (Eq a) => [a] -> [a] -> Bool
serach needle haystack = 
	let nlen = length needle
	in foldl (\acc x -> if take nlen x == needle then True else False) False (tails haystack)

on' :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on'` g = \x y -> f (g x) (g y)