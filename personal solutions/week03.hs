instance Functor List where
    fmap _ Nil = Nil												--pattern match on 'Nil' empty list and return 'Nil' empty list since nothing applied
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)						--for Cons, we apply the function f to the head of the list x and recursively apply fmap to the tail

instance Applicative List where
    pure x = Cons x Nil												--pure takes a value x and creates a singleton list with that value
    Nil <*> _ = Nil													--empty list returns empty list since nothing applied
    (Cons f fs) <*> xs = (fmap f xs) `append` (fs <*> xs)			-- <*> combines two list: each function in the first list -> every elements in the second list 
																	--used fmap function to apply the function f to every element in the list xs
																	--apply rest of the functions fs to list of values xs by using <*>
        where append Nil ys = ys
              append (Cons y ys) zs = Cons y (append ys zs)			--concatenate results

instance Monad List where											--sequence computation by using >>+
  return x = Cons x Nil												--return is used for creating a computation that produces a value within the context of a Monad
  Nil >>= _ = Nil													--Nil returns Nil
  (Cons x xs) >>= f = (f x) `append` (xs >>= f)						--'>>=' takes a list of 'xs' and a function f, and it applies 'f' to each element of 'xs', flattening the 
																	--resulting lists into a single list using the 'append' function
    where append Nil xs = xs
          append (Cons x xs) ys = Cons x (append xs ys)
		  
		  
--Functor Instance for Pair:
--To define a `Functor` instance for the `Pair` type, 
--we can map a function over the second component b while keeping the first component a unchanged. 
--The instance and the brief proof that the Functor laws hold:
instance Functor (Pair a) where
	fmap f (P a b) = P a (f b)

--two functor equational laws
--the identity law:
--Identity fmap id = id
--to prove identity law
	fmap id (P a b) = P a (id b) = P a b
--in this case, simply switching f to id simply provides the same result as the original case
--this satisfy the identity law

--the composition law: 
--Composition fmap(f . g) = fmap f . fmap g
--to prove composition law	
    fmap g (fmap f (P a b)) = fmap g (P a (f b)) = P a (g (f b))
--since the two sides of the composition law are equal and provides the same results, the composition law holds



--it's not possible to define a valid `Applicative` instance for the `Pair` type. 
--This is because the `Applicative` type class requires providing both `pure` and `<*>` functions, 
--and in this case, there's no suitable way to define `pure` for `Pair`.

--In Haskell's `Applicative` type class, `pure` should be able to create a value of any type `f a`, 
--but in the case of `Pair`, we will have a fixed structure with two components (`a` and `b`). 
--We can't simply create a `Pair` with arbitrary values, 
--so it doesn't meet the requirements of the `Applicative` type class.