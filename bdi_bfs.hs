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

bidi_visit (queue, closed) node
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

--TODO test
bidi_solution front back = sol_front ++ sol_back
    where 
        sol_back = solution2 back
        sol_front = solution front

gen_bidi (front_node :< front_queue) front_closed (back_node :< back_queue) back_closed get_children 
    | (element front_node) == (element back_node) = bidi_solution front_node back_node
    | Map.member (element front_node) back_closed = bidi_solution front_node (map_get (element front_node) back_closed)
    | Map.member (element back_node) front_closed = bidi_solution (map_get (element back_node) front_closed) back_node
    | otherwise = gen_bidi front_queue' front_closed' back_queue' back_closed' get_children 
    where
        (back_queue', back_closed') = List.foldl' bidi_visit (back_queue, back_closed) (get_children back_node) 
        (front_queue', front_closed') = List.foldl' bidi_visit (front_queue, front_closed) (get_children front_node) 


simple_get_child (node, action) = Branch (flip_at (element node) action) node action

simple_get_children node = map simple_get_child (zip (repeat node) (take (length (element node)) [2..]) )

--simple_bfs list goal = gen_bfs (simple_is_goal goal) (Root list :< Empty) Set.empty simple_get_children

bidirectional list = gen_bidi (Root list :< Empty) Map.empty (Root goal :< Empty) Map.empty simple_get_children
    where goal = (List.sort list)

--bidi = bidirectional

test = [2,7,3,1]

