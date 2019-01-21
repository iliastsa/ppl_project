-- import qualified Data.List as List
import Data.List
import Data.Ord


burnt_reverse = (map fix) . reverse
    where fix (x, c) = (x, (abs (c - 1)))

burnt_flip_at xs n = (burnt_reverse ( take n xs )) ++ (drop n xs)
    
max_index selector =  maximumBy (comparing (selector . snd)) . zip [1..]

fix_burnt (_, 1) = []
fix_burnt (_, 0) = [1]

burnt_naive [] = []
burnt_naive [(_, 0)] = []
burnt_naive [(_, 1)] = [1]

burnt_naive xs = 
    -- max pancake is at top, use one flip to fix orientation
    if i == 1 then  (fix_burnt x) ++ ((length xs):(burnt_naive $ burnt_reverse $ tail xs))
    -- bottom pancake is correct
    else if i == (length xs) && (snd x) == 0 then burnt_naive $ take ( i - 1) xs
    -- move max pancake at the top
    else i : (burnt_naive $ burnt_flip_at xs i)
    where (i, x) = max_index fst xs

visualize flip xs [] = [xs]
visualize flip xs (f:fs) = xs : (visualize ( flip xs f) fs)

burnt_visualize = visualize burnt_flip_at


test = [(4,1),(9,0),(1,0),(10,1),(17,0)]
