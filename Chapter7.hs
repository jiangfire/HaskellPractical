import Data.List
import Data.Char
import qualified Data.Map as Map
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

serach :: (Eq a) => [a] -> [a] -> Bool
serach needle haystack = 
	let nlen = length needle
	in foldl (\acc x -> if take nlen x == needle then True else False) False (tails haystack)

on' :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on'` g = \x y -> f (g x) (g y)

encode' :: Int -> String -> String
encode' shift msg =
	let
		ords = map ord msg
		shifted = map (+ shift) ords
	in
		map chr shifted

decode' :: Int -> String -> String
decode' shift msg = encode' (negate shift) msg

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key==k) $ xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key [] = Nothing 
findKey' key ((k, v):xs) = 
	if key == k then
		Just v
	else
		findKey' key xs

findKey'' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey'' key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing

fromList' :: (Ord k) => [(k, v)] -> Map.Map k v
fromList' = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty

phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs