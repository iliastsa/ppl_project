{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
-- Ilias Tsatiris (sdi1500162)
-- Kostas Alexopoulos (sdi1600002)

import Data.Ord
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Maybe as Maybe

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

-- used for debugging 
-- breadth_first_search :: Ord state_type =>
--     (state_type -> Bool) 
--     -> Seq.Seq (Node state_type action_type)
--     -> Map.Map state_type (Node state_type action_type)
--     -> (state_type -> [(state_type, action_type)])
--     -> [action_type]

-- in the problem a solution always exists thus there is no need for a base case.
breadth_first_search is_goal (node :< queue) closed successors
    | is_goal (state node) = (solution node)
    | otherwise = (breadth_first_search is_goal queue' closed' successors)
    where
        (queue', closed') = List.foldl' visit (queue, closed) node_successors
        -- node_successors expects
        node_successors = List.map (uncurry $ Branch node) (successors $ state node)

-- returns the solution of the bidirectional search
bidi_solution front rear 
    | (mod(length sol) 2) == 0 = sol
    | otherwise = reverse sol
    where 
        sol  = sol_front ++ sol_rear
        sol_rear = solution rear
        sol_front = reverse (solution front)

-- used for folding when expanding a layer in bidirectional bfs.
bidi_visit (queue, closed) node
    | Map.member (state node) closed = (queue, closed)
    | otherwise = (node:queue, closed')
    where
        closed' = Map.insert (state node) node closed

find_first (x:xs) map 
    | Map.member (state x) map = (x, map_get (state x) map)
    | otherwise = find_first xs map

bidi_breadth_first front_queue front_closed rear_queue rear_closed successors
    -- If a solution is found (a node in the front layer is found the rear closed set)
    | Maybe.isJust in_rear = bidi_solution (Maybe.fromJust in_rear) (map_get (state $ Maybe.fromJust in_rear) rear_closed)

    -- Else recurse, by expanding one layer for front and swapping roles (front becomes rear and vice versa). This is a neat trick to 
    -- avoid duplicate code.
    | otherwise = bidi_breadth_first rear_queue rear_closed front_queue' front_closed' successors
    where
        in_rear = List.foldl' (\may node -> if Maybe.isJust may then may else Map.lookup (state node) front_closed) Nothing rear_queue

        (front_queue', front_closed') = List.foldl' bidi_visit ([], front_closed) (all_successors front_queue)
        
        all_successors nodes = List.concatMap node_successors nodes
        node_successors node = List.map (uncurry $ Branch node) (successors $ state node)

-- Simple solution; we just search from the goal to the starting nodes, and memoize previous results.
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
                

-- Problem related functions 

flip_at xs n = reverse ( take n xs ) ++ (drop n xs)

-- Reverse a burnt stack, fixing the burnt bits appropriately
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

bfs xs = breadth_first_search (== xs) ( Seq.singleton (Root goal) ) (Map.singleton goal (Root goal)) simple_successors
    where goal = List.sort xs

bidirectional xs = bidi_breadth_first [Root xs] (Map.singleton xs (Root xs)) [Root goal] (Map.singleton goal (Root goal)) simple_successors
    where goal = List.sort xs


batch xs = batch_search rel_xs (Seq.singleton (Root goal)) (Map.singleton goal (Root goal)) simple_successors
    where 
        goal = [1..length (head xs)]
        rel_xs = map (indexed_sort id) xs

-- Used to fix top pancake (max 1 extra flip when before moving the largest pancake to the bottom of the stack)
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

burnt_bfs xs = breadth_first_search (== xs) ( Seq.singleton (Root goal) ) (Map.singleton goal (Root goal)) burnt_successors
    where goal = burnt_goal xs

burnt_bidirectional xs = bidi_breadth_first [Root xs] (Map.singleton xs (Root xs)) [Root goal] (Map.singleton goal (Root goal)) burnt_successors
    where goal = burnt_goal xs

burnt_batch xs = batch_search rel_xs (Seq.singleton (Root goal)) (Map.singleton goal (Root goal)) burnt_successors
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
    
-- gates
abs_diff a b = abs (a - b)

adjacent a b n = (abs_diff a b) == 1 || (a == n && b == 1) || ( a == 1 && b == n)


split_at (x:xs) item 
    | x == item =  ([], xs)
    | otherwise = ((x:head),tail)
    where
        (head, tail) = split_at xs item

last_adjacent [] prev _ = prev
last_adjacent (x:xs) prev n
    | adjacent x prev n = last_adjacent xs x n
    | otherwise = prev

index (x:xs) item 
    | x == item = 1
    | otherwise = (index xs item) + 1

block_at xs item n = 
    (last_adjacent (reverse head) item n, last_adjacent tail item n)
    where 
        (head, tail) = split_at xs item

flip_1 xs item = 
        [(index xs item) - 1] 

flip_3 xs min max
    | min_i > max_i = flip_3 xs max min
    | otherwise = [min_i, (min_i - 1), max_i, (max_i - min_i)]
    where
        min_i = index xs min
        max_i = index xs max

flip_6 xs x free tail 
    | free_i < tail_i = [free_i, free_i - x_i, tail_i, tail_i - free_i]
    | otherwise = [free_i, free_i - x_i, free_i, free_i - tail_i - x_i]
    where 
        x_i    = index xs x
        free_i = index xs free
        tail_i = index xs tail

flip_7 xs x k k_head
    | k_i == k_head_i = [x_i, k_i]
    | otherwise = [k_i, k_i - x_i]
    where 
        x_i = index xs x
        k_i = index xs k
        k_head_i = index xs k_head 

flip_8 (x:xs) n 
    | x == 1 = []
    | x == n = [n]
    | one_i < n_i = [n, n - n_i + 1, n, one_i]
    | otherwise = [n_i, n, n - one_i + 1]
    where 
        one_i = index (x:xs) 1
        n_i = index (x:xs) n
    
pre_gates list n 
    -- case 1
    | x_free && plus_head == plus_tail 
        = next_step (flip_1 list plus)
    | x_free && minus_head == minus_tail 
        = next_step (flip_1 list minus)

    -- case 2 
    | x_free && plus_head == plus 
        = next_step (flip_1 list plus)
    | x_free && minus_head == minus 
        = next_step (flip_1 list minus)

    -- case 3
    | x_free && plus_tail == plus && minus_tail == minus 
        = next_step (flip_3 list minus plus)

    -- case 4
    -- TODO remove this case is supported by case 5
    | not x_free && plus_head == plus_tail 
        = next_step (flip_1 list plus)
    | not x_free && minus_head == minus_tail 
        = next_step (flip_1 list minus)

    -- case 5
    | not x_free 
        && plus_head == plus 
        = next_step (flip_1 list plus)
    | not x_free 
        && minus_head == minus 
        = next_step (flip_1 list minus)


    -- case 6
    | not x_free
        && minus == minus_tail 
        && not (minus_tail == x_tail)
        && k_plus_head == k_plus_tail 
        = next_step (flip_6 list x_tail k_plus minus) 

    | not x_free
        && plus == plus_tail 
        && not (plus_tail == x_tail)
        && k_minus_head == k_minus_tail 
        = next_step (flip_6 list x_tail k_minus plus) 

    
    -- case 7
    | not x_free
        && not (k_plus_head == k_plus_tail)
        && not (k_plus_tail == x_tail)
        = next_step (flip_7 list x_tail k_plus k_plus_head)


    | not x_free
        && not (k_minus_head == k_minus_tail)
        && not (k_minus_tail == x_tail)
        = next_step (flip_7 list x_tail k_minus k_minus_head)


    -- case 8
    | otherwise 
        = flip_8 list n

    where
        minus = magic (x - 1)
        plus = magic (x + 1)

        k_minus = magic (x_tail - 1)
        k_plus = magic (x_tail + 1) 
        
        x_free = x_head == x_tail
        x = head list
        (x_head, x_tail) = block_at list x n
        (plus_head, plus_tail) = block_at list plus n
        (minus_head, minus_tail) = block_at list minus n

        (k_plus_head, k_plus_tail) = block_at list k_plus n
        (k_minus_head, k_minus_tail) = block_at list k_minus n

        magic x 
            | x <= 0 = n + x
            | x > n = x - n
            | otherwise = x
        next_step flips = flips ++ pre_gates (foldl flip_at list flips) n

        -- next_step flips = foldl flip_at list flips
        -- next_step flips = flips
gates xs = pre_gates xs_indexed (length xs)
    where 
        xs_indexed = indexed_sort id xs