
abs_diff a b = abs (a - b)

adjacent a b n = (abs_diff a b) == 1 || (a == n && b == 1) || ( a == 1 && b == n)



split_at (x:xs) item =
    if x == item then ([], xs)
    else ((x:head),tail)
    where
        (head, tail) = split_at xs item

last_adjacent [] prev _ = prev
last_adjacent (x:xs) prev n
    | adjacent x prev n = last_adjacent xs x n
    | otherwise = prev

index (x:xs) item 
    | x == item = 0
    | otherwise = (index xs item) + 1

block_at xs item n = 
    (last_adjacent (reverse head) item n, last_adjacent tail item n)
    where 
        (head, tail) = split_at xs item

gates list n 
    -- case 1
    | x_free && plus_head == plus_tail = 1
    | x_free && minus_head == minus_tail = 1

    -- case 2 
    | x_free && plus_head == plus = 1
    | x_free && minus_head == minus = 1

    -- case 3
    | x_free && plus_tail == plus && minus_tail == minus = 1
    
    -- case 4
    | ! x_free && plus_head == plus_tail = 1
    | ! x_free && minus_head == minus_tail = 1

    -- case 5
    | ! x_free && plus_head == plus = 1
    | ! x_free && minus_head == minus = 1

    -- case 6
    | ! x_free 
        && minus == minus_tail 
        && minus_tail != x_tail 
        && k_plus_head == k_plus_tail = 1
    | ! x_free
        plus == plus_tail 
        && plus_tail != x_tail
        && k_minus_head == k_minus_tail = 1
    
    -- case 7
    | ! x_free
        && minus == minus_tail 
        && minus_tail != x_tail = 1

    | ! x_free
        &&  plus == plus_tail 
        && plus_tail != x_tail = 1

    -- case 8
    otherwise = 1


    where
        minus = magic (x - 1)
        plus = magic ( x + 1)

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
            | x < 0 = n - x
            | x > n = x - n
            | otherwise = x
