
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

block_at xs item n = 
    (last_adjacent (reverse head) item n, last_adjacent tail item n)
    where 
        (head, tail) = split_at xs item