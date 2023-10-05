import Control.Parallel

parallel_map :: (a -> b) -> [a] -> [b]
parallel_map _ [] = []
parallel_map f (x:xs) = x' `par` (xs' `pseq` (x' : xs'))
  where
    x' = f x
    xs' = parallel_map f xs

--For an empty list, it returns back an empty list.

--For a non-empty list (x:xs), it first applies the function f to x and stores the result in x'. This part is evaluated in parallel using `par`.

--It then recursively applies parallel_map to the rest of the list xs and stores the result in xs'. This part is evaluated sequentially using `pseq`.

--Finally, it combines x' and xs' into a single list (x' : xs'). 