import Data.List
import Data.Ord

flip_at xs n = reverse ( take n xs ) ++ (drop n xs)
--flip_at xs 1 = reverse xs
--flip_at (x:xs) n = x:flip_at xs (n - 1)

fix xs i = reverse ( flip_at xs i )

max_index :: (Ord a ) =>  [a] -> Int
max_index = fst . maximumBy (comparing snd) . zip [1..]

naive [x] = []

naive xs = 
    if i == 1 then (length xs):(naive $ reverse $ tail xs)
    else if i == (length xs) then naive $ take ( i - 1) xs
    else i : (naive $ flip_at xs i)
    where i = max_index xs

visualize xs [] = [xs]
visualize xs (f:fs) = xs : (visualize ( flip_at xs f) fs)


test = [2,7,3,1]
