{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

import Data.Ord
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

pattern Empty   <- (Seq.viewl -> Seq.EmptyL)  where Empty = Seq.empty
pattern x :< xs <- (Seq.viewl -> x Seq.:< xs) where (:<)  = (Seq.<|) 
pattern xs :> x <- (Seq.viewr -> xs Seq.:> x) where (:>)  = (Seq.|>) 

data Node a = Branch {state :: a, parent :: (Node a), action :: Int} 
    | Root {state :: a} 

flip_at xs n = reverse ( take n xs ) ++ (drop n xs)

visualize xs [] = [xs]
visualize xs (f:fs) = xs : (visualize ( flip_at xs f) fs)

visit (queue, closed) node
    | Set.member (state node) closed = (queue, closed)
    | otherwise = (queue', closed')
    where
        queue' = (queue :> node)
        closed' = Set.insert (state node) closed

solution2 (Root _) = [] 
solution2 (Branch _ parent action) = action:(solution2 parent)

solution node = reverse (solution2 node)


gen_bfs _ Empty _ _ = []
gen_bfs is_goal (node :< queue) closed get_children
    | is_goal (state node) = solution node
    | otherwise = (gen_bfs is_goal queue' closed' get_children)
    where
        (queue', closed') = List.foldl' visit (queue, closed) (get_children node) 

simple_is_goal goal node = goal == node

simple_get_child (node, action) = Branch (flip_at (state node) action) node action

simple_get_children node = map simple_get_child (zip (repeat node) (take (length (state node)) [2..]) )

simple_bfs list goal = gen_bfs (simple_is_goal goal) (Root list :< Empty) Set.empty simple_get_children

bfs list = simple_bfs list (List.sort list)

test = [2,7,3,1]

