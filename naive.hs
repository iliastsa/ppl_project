import Data.List
import Data.Ord

flip_at xs n = reverse ( take n xs ) ++ (drop n xs)
--flip_at xs 1 = reverse xs
--flip_at (x:xs) n = x:flip_at xs (n - 1)

fix xs i = reverse ( flip_at xs i )

max_index :: (Ord a ) =>  [a] -> Int
max_index = maximumBy (comparing snd) . zip [1..]

naive [x] = []

naive xs = 
    -- move top pancake at the botoom 
    if i == 1 then (length xs):(naive $ reverse $ tail xs)
    -- bottom pancake is correct
    else if i == (length xs) then naive $ take ( i - 1) xs
    -- move max pancake at the top
    else i : (naive $ flip_at xs i)
    where (i, _) = max_index xs

visualize flip xs [] = [xs]
visualize flip xs (f:fs) = xs : (visualize ( flip xs f) fs)
    

test = [2,7,3,1]
