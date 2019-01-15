import Data.Ord
import Data.List

indexed_sort x = map snd (sortOn fst (zip x [1..]))
rel_order_list x =  indexed_sort (indexed_sort x)
    

