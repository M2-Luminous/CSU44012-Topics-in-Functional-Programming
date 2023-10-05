



import Control.Parallel

--f1 :: Char -> Int -> Pair Char Int
--f1 x y = BothOfThem x y


--
-- Part 1
--

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            

-- This counts leaves
count :: Tree a -> Integer
count (Leaf _)   = 1
count (Node l r) = count l + count r

-- The question actually asked for the count of "Nodes", 
-- so being pedantic about it I suppose the following is right.
-- (I was thinking about a count of the data, though, so 
--  the first one is what I intended)
count1 :: Tree a -> Integer
count1 (Leaf _)   = 0
count1 (Node l r) = 1 + count l + count r

depth :: Tree a -> Integer
depth (Leaf _)   = 1
depth (Node l r) = 1 + max (depth l) (depth r)

flatten :: Tree a -> [a]
flatten (Leaf x)   = [x]
flatten (Node l r) = flatten l ++ flatten r

--
-- Part 2 was trying out ThreadScope 
--

--
-- Part 3 

{-
 This is a possible solution - whether this actually generates meaningful parallelism
 will depend on the type of operation being mapped over the list. The "force" function
 is an example of something that could be used to demand the value of x' is calculated.
 If the result of F is a simple value then this could be enough, but if it's a data value
 then it might only compute the outer constructor. In that case something like 
 deepSeq in the Control.Parallel module, which forces the entire structure, might be needed.

-}


parallel_map :: (a -> b) -> [a] -> [b]
parallel_map _ [] = []
parallel_map f (x:xs) = x' `par` (xs' `pseq` x':xs')
    where x'  = f x
          xs' = parallel_map f xs
          
