-- I want this function name that's already defined in the standard Prelude,
-- so I don't import it.
import Prelude hiding ((<>)) 

data List a = Nil | Cons a (List a) deriving (Show, Read)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  
--
-- this function is called (++) for regular lists
-- I named it (<>) for ... reasons. You'll agree with me later.
-- (it's because this list thing can also be an instance of a class
--  called monoid, which is basically things that can be combined.
--  the (<>) or "mappend" is how they are combined)
--

(<>) :: List a -> List a -> List a
Nil         <> y   = y
x           <> Nil = x
(Cons x xs) <> ys  = Cons x $ xs <> ys

-- consider this to be a prefix-form of the function.
-- which you could use (as I do in an example below) 
-- to remind us of the (++) operator.
plusplus = (<>)


instance Applicative List where
  pure a = Cons a Nil
  
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  Cons f x <*> y = (f <$> y) <> (x <*> y)

-- Or, if you don't like all the infix malarkey we could cut it down
--  Cons f x <*> y =  (fmap f y) `plusplus` (x <*> y)

concatL :: List (List a) -> List a
concatL Nil = Nil
concatL (Cons xs xss) = xs <> (concatL xss)
    
instance Monad List where
  return = pure
  l >>= k = concatL (fmap k l)
  
--
-- Sketch proofs of the monad laws
--

-- (1) return x >>= f   ===  f x
-- This proof proceeds quite simply by substituting the definition of "return" on the l.h.s.
-- of bind, and then expanding the definition of bind to show that the l.h.s.
-- corresponds to "f a <> []" which is trivially reducable to "f a"
-- 
-- (2) m >>= return    ===     m
-- This proof is a little longer but proceeds similarly.
-- Expand the definition of ">>=" and then consider the two cases for "m"
-- Either m is "[]", in which case the result follows trivially, or
--        m is [x1,x2,x3...] in which case we can expand the definitions of fmap and return to show that the
--        result will be  [x1] ++ concat [ [x2], [x3] ...], and by the definition of (++) we get
--        a result of [x1, x2, x3...]

-- (3) (m >>= f) >>= g   ===   m >>= (\x -> f x >>= g)
-- Proving associativity is a bit trickier. The standard technique
-- is to use induction over the list, but the proof is quite long.
-- Luckily we were only asked for a sketch proof that would be
-- intuitively convincing.
-- First we do a case-split on m, and show that the law holds for m==[] (which is simple).
-- Then we consider the case where m is some list "as". We assume that the law holds, and then
-- attempt to prove it for the case where m is some extended list "a:as".
-- This step is possible but a bit wordy; the main trick is to use (++) to
-- split the list into two parts, separating the application of f to the head of m from
-- the further applications of f to the tail of m, which allows us to apply the hypothesis.
--




--
-- Part 2
-- 

data Pair a b = Pair a b

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)
  
{-
 - But now we have a problem; to make an instance of applicative 
 - I need at least a definition of pure that can create a Pair a b
 - out of just a value of type a. 
 - I'm stuck, because I can't supply a value for the other
 - argument to the constructor
 - 
 - Actually, because I am a big cheater, I will give an instance
 - for Applicative anyway. But it's a stupid instance and it's not
 - what I wanted you to produce!
 - 
 - It might illustrate the point, though. There is nothing useful
 - that you can write in place of "undefined" here that will let
 - you complete the instance!
 -}

instance Applicative (Pair a) where
  pure x = Pair undefined x
  (<*>) = undefined
  
