
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

flip_at xs n = reverse ( take n xs ) ++ (drop n xs)

flip_1 xs item = 
        [(index xs item) - 1] 

flip_3 xs min max
    | min_i < max_i = flip_3 xs max min
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
        -- = 1
    | x_free && minus_head == minus_tail 
        = next_step (flip_1 list minus)
        -- = 1

    -- case 2 
    -- TODO combine cases 1 and 2
    | x_free && plus_head == plus 
        = next_step (flip_1 list plus)
        -- = 2
    | x_free && minus_head == minus 
        = next_step (flip_1 list minus)
        -- = 2

    -- case 3
    | x_free && plus_tail == plus && minus_tail == minus 
        = next_step (flip_3 list minus plus)
        -- = 3

    -- case 4
    -- TODO remove this case is supported by case 5
    | not x_free && plus_head == plus_tail 
        = next_step (flip_1 list plus)
        -- = 4
    | not x_free && minus_head == minus_tail 
        = next_step (flip_1 list minus)
        -- = 4

    -- case 5
    | not x_free 
        && plus_head == plus 
        = next_step (flip_1 list plus)
        -- = 5
    | not x_free 
        && minus_head == minus 
        = next_step (flip_1 list minus)
        -- = 5


    -- case 6
    | not x_free
        && minus == minus_tail 
        && not (minus_tail == x_tail)
        && k_plus_head == k_plus_tail 
        = next_step (flip_6 list x_tail k_plus minus) 
        -- = 6

    | not x_free
        && plus == plus_tail 
        && not (plus_tail == x_tail)
        && k_minus_head == k_minus_tail 
        = next_step (flip_6 list x_tail k_minus plus) 
        -- = 6

    
    -- case 7
    | not x_free
        && not (k_plus_head == k_plus_tail)
        && not (k_plus_tail == x_tail)
        = next_step (flip_7 list x_tail k_plus k_plus_head)
        -- = 7


    | not x_free
        && not (k_minus_head == k_minus_tail)
        && not (k_minus_tail == x_tail)
        = next_step (flip_7 list x_tail k_minus k_minus_head)
        -- = 7


    -- case 8
    | otherwise 
        = flip_8 list n
        -- = 8

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
gates xs = foldl flip_at xs (pre_gates xs (length xs))
    -- where
    --     comp = pre_gates xs (length xs)
    --     (top_head, top_tail) = block_at 



case_1_a = [1,3,4,2]
case_1_b = [2,4,3,1]

case_ran :: [Int]
case_ran = [3, 5, 4, 1, 2]

case_hard :: [Int]
case_hard =  [6,7,8,1,2,3,4,5]

case_1_hard :: [Int]
case_1_hard = [4,6,8,5,3,7,2,1]

s4 :: [Int]
s4 = [4,5,6,7,8,3,2,1]

