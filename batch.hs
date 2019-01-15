{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

import Data.Ord
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

pattern Empty   <- (Seq.viewl -> Seq.EmptyL)  where Empty = Seq.empty
pattern x :< xs <- (Seq.viewl -> x Seq.:< xs) where (:<)  = (Seq.<|) 
pattern xs :> x <- (Seq.viewr -> xs Seq.:> x) where (:>)  = (Seq.|>) 

data Node a = Branch {element :: a, parent :: (Node a), action :: Int} | Root {element :: a} 

flip_at xs n = reverse ( take n xs ) ++ (drop n xs)

visualize xs [] = [xs]
visualize xs (f:fs) = xs : (visualize ( flip_at xs f) fs)

visit (queue, closed) node
    | Map.member (element node) closed = (queue, closed)
    | otherwise = (queue', closed')
    where
        queue' = (queue :> node)
        closed' = Map.insert (element node) node closed

solution2 (Root _) = [] 
solution2 (Branch _ parent action) = action:(solution2 parent)

solution node = reverse (solution2 node)

m_unwrap (Just v) = v 
map_get k m = m_unwrap (Map.lookup k m)

indexed_sort x = map snd (List.sortOn fst (zip x [1..]))
rel_order_list x =  indexed_sort (indexed_sort x)

--batch_bfs _ Empty _ _ = []
batch_bfs goal (node :< queue) closed get_children
    | goal == (element node) = ((solution2 node), queue, closed)
    | otherwise = (batch_bfs goal queue' closed' get_children)
    where
        (queue', closed') = List.foldl' visit (queue, closed) (get_children node) 

simple_get_child (node, action) = Branch (flip_at (element node) action) node action

simple_get_children node = map simple_get_child (zip (repeat node) (take (length (element node)) [2..]) )

batch2 [] _ _ = []
batch2 (x:xs) queue closed 
    | Map.member x closed = (solution2 (map_get x closed)): (batch2 xs queue closed)
    | otherwise =  sol:(batch2 xs queue' closed')
    where 
        (sol, queue', closed') = batch_bfs x queue closed simple_get_children

batch :: [[Int]] -> [[Int]]
batch xs = batch2 rel_xs (Root goal :< Empty) Map.empty
    where 
        goal = take (length (head xs)) [1..]
        rel_xs = map rel_order_list xs




test = [2,7,3,1]

