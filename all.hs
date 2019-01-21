{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
-- Ilias 
-- Kostas Alexopoulos (sdi1600002)

import Data.Ord
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

pattern Empty   <- (Seq.viewl -> Seq.EmptyL)  where Empty = Seq.empty
pattern x :< xs <- (Seq.viewl -> x Seq.:< xs) where (:<)  = (Seq.<|) 
pattern xs :> x <- (Seq.viewr -> xs Seq.:> x) where (:>)  = (Seq.|>) 

-- Utilities

maybe_unwrap (Just value) = value

-- be careful, this may explodeee
map_get key map = maybe_unwrap (Map.lookup key map)

indexed_sort selector xs = 
    map fst 
    $ List.sortOn snd 
    $ zip [1..]
    $ map snd 
    $ List.sortOn fst 
    $ zip (map selector xs) [1..]

max_index selector =  List.maximumBy (comparing (selector . snd)) . zip [1..]



-- BFS related structures

data Node state_type action_type =
    Branch {
        parent :: (Node state_type action_type), 
        state :: state_type, 
        action :: action_type
    } 
    | Root {
        state :: state_type
    } 
    
visit (queue, closed) node
    | Map.member (state node) closed = (queue, closed)
    | otherwise = (queue', closed')
    where
        queue' = (queue :> node)
        closed' = Map.insert (state node) node closed

solution (Root _) = [] 
solution (Branch parent _ action) = action:(solution parent)

breadth_first_search :: Ord state_type =>
    (state_type -> Bool) 
    -> Seq.Seq (Node state_type action_type)
    -> Map.Map state_type (Node state_type action_type)
    -> (state_type -> [(state_type, action_type)])
    -> [action_type]

-- breadth_first_search _ Empty _ _ = []
breadth_first_search is_goal (node :< queue) closed successors
    | is_goal (state node) = (solution node)
    | otherwise = (breadth_first_search is_goal queue' closed' successors)
    where
        (queue', closed') = List.foldl' visit (queue, closed) node_successors
        node_successors = List.map (uncurry (Branch node)) (successors (state node))

bidi_solution front rear = sol_front ++ sol_rear
    where 
        sol_rear = solution rear
        sol_front = reverse (solution front)

bidi_breadth_first (front_node :< front_queue) front_closed (rear_node :< rear_queue) rear_closed successors
    -- matching states, a solution is searched
    | (state front_node) == (state rear_node) = bidi_solution front_node rear_node
    
    -- Front node has already been explored by rear search, return solution
    | in_closed front_node = bidi_solution front_node (map_get (state front_node) rear_closed)
    
    -- Rear node has already been explored by front search, return solution
    | in_front rear_node = bidi_solution (map_get (state rear_node) front_closed) rear_node
    
    -- Paths have not crossed yet, keep looking
    | otherwise = bidi_breadth_first front_queue' front_closed' rear_queue' rear_closed' successors 
    where
        in_front node = Map.member (state node) front_closed 
        in_closed node = Map.member (state node) rear_closed
        (rear_queue', rear_closed') = List.foldl' visit (rear_queue, rear_closed) (node_successors rear_node)
        (front_queue', front_closed') = List.foldl' visit (front_queue, front_closed) (node_successors front_node)
        node_successors node = List.map (uncurry $ Branch node) (successors $ state node)


batch_search_step is_goal (node :< queue) closed successors
    | is_goal (state node) = (solution node, queue, closed)
    | otherwise = batch_search_step is_goal queue' closed' successors
    where
        (queue', closed') = List.foldl' visit (queue, closed) (node_successors node) 
        node_successors node = List.map (uncurry $ Branch node) (successors $ state node)
        
batch_search [] _ _ _ = []
batch_search (x:xs) queue closed successors
    | Map.member x closed = (solution (map_get x closed)):(batch_search xs queue closed successors)
    | otherwise =  sol:(batch_search xs queue' closed' successors)
    where 
        (sol, queue', closed') = batch_search_step (== x) queue closed successors
                

-- -- Problem related function 

flip_at xs n = reverse ( take n xs ) ++ (drop n xs)


burnt_reverse = (map fix) . reverse
    where fix (x, c) = (x, (abs (c - 1)))

burnt_flip_at xs n = (burnt_reverse ( take n xs )) ++ (drop n xs)
    

-- naive
naive [x] = []

naive xs = 
    -- move top pancake at the botoom 
    if i == 1 then (length xs):(naive $ reverse $ tail xs)
    -- bottom pancake is correct
    else if i == (length xs) then naive $ take ( i - 1) xs
    -- move max pancake at the top
    else i : (naive $ flip_at xs i)
    where (i, _) = max_index id xs

-- bfs

simple_successors state = map child [2..len] 
        where 
            child action = ((flip_at state action), action)
            len = length state

bfs xs = breadth_first_search (== xs) ( Seq.singleton (Root goal) ) Map.empty simple_successors
    where goal = List.sort xs

bidirectional xs = bidi_breadth_first ( Seq.singleton (Root xs) ) Map.empty ( Seq.singleton (Root goal) ) Map.empty simple_successors
    where goal = List.sort xs

batch xs = batch_search rel_xs (Seq.singleton (Root goal)) Map.empty simple_successors
    where 
        goal = [1..length (head xs)]
        rel_xs = map (indexed_sort id) xs


fix_burnt (_, 1) = []
fix_burnt (_, 0) = [1]

-- burnt_naive
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

-- burnt bfs

burnt_successors state = map child [2..len] 
        where 
            child action = (burnt_flip_at state action, action)
            len = length state

burnt_goal xs = List.map (\x -> (x, 0)) (List.sort $ List.map fst xs)

burnt_bfs xs = breadth_first_search (== xs) ( Seq.singleton (Root goal) ) Map.empty burnt_successors
    where goal = burnt_goal xs

burnt_bidirectional xs = bidi_breadth_first ( Seq.singleton (Root xs) ) Map.empty ( Seq.singleton (Root goal) ) Map.empty burnt_successors
    where goal = burnt_goal xs

burnt_batch xs = batch_search rel_xs (Seq.singleton (Root goal)) Map.empty burnt_successors
    where 
        goal = zip [1..length (head xs)] (repeat 0)
        rel_xs = map tranform xs
        -- black magic
        tranform x = map (\ ((_, b), rel) -> (rel, b) ) 
            $ zip x 
            $ indexed_sort fst x

        
_visualize flip xs [] = [xs]
_visualize flip xs (f:fs) = xs : (_visualize flip ( flip xs f) fs)
    
visualize = _visualize flip_at

burnt_visualize = _visualize burnt_flip_at
    
-- -- bfs stack = 


test_bfs = [12,23,9,36,20,34,4]
test_burnt = [(4,1),(9,0),(1,0),(10,1),(17,0)]
test_batch_burnt = repeat [(4,1),(9,0),(1,0),(10,1),(17,0)]

test_batch = [[6,12,1,7,3],[1,4,2,7,8],[10,2,11,8,9]]