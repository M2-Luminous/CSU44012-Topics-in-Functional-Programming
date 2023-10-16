--
-- Part 1: The Writer Monad
--

type Log = [String]

data Writer a = Writer Log a 
  deriving Show

runWriter :: Writer a -> (Log, a)
runWriter (Writer l x) = (l,x)

instance Functor Writer where
  fmap f (Writer log val) = Writer log (f val)
  
instance Applicative Writer where
  pure x = Writer [] x
  (Writer l1 f) <*> (Writer l2 a) = Writer (l1++l2) (f a)
  
{-- 
 -- The default definition of <*> is 
 -- <*> = liftA2 id
 --  and liftA2 is
 -- liftA2 f x = (<*>) (fmap f x)
 -- so we'll get an infinite recursion trying to apply it 
 -- if we don't define something sensible
 --}

instance Monad Writer where
  return  = pure
  m >>= k = let Writer l a   = m
                Writer l' a' = k a
                in Writer (l++l') a'

tell :: String -> Writer ()
tell logMessage = Writer [logMessage] ()

example :: Writer Int
example = do
  tell "entry 1"
  tell "entry 2"
  return (1+1)
  

test = (runWriter example) == ( ["entry 1","entry 2"], 2)

---
--- Part 2
---


--- a



data Writer2 l a = Writer2 [l] a 


runWriter2 :: Monoid l => Writer2 l a -> ([l], a)
runWriter2 (Writer2 l x) = (l,x)


instance Functor (Writer2 l) where
  fmap f (Writer2 log val) = Writer2 log (f val)
  
instance Applicative (Writer2 l) where
  pure x = Writer2 mempty x
  (Writer2 l1 f) <*> (Writer2 l2 a) = Writer2 (l1 ++ l2) (f a)
  
instance Monoid l => Monad (Writer2 l) where
  return  = pure
  m >>= k = let Writer2 l a   = m
                Writer2 l' a' = k a
                in Writer2 (l++l') a'


tell2 :: a -> Writer2 a ()
tell2 logMessage = Writer2 [logMessage] ()

example2 :: Writer2 String Int
example2 = do
  tell2 "entry 1"
  tell2 "entry 2"
  return (1+1)
  

test2 = (runWriter2 example2) == ( ["entry 1","entry 2"], 2)




--- b


-- having the log be a value of any type will run into problems when we try to define <*>
-- and (>>=). Both of those take the decision to concatenate logs, and there's no guarentee
-- that the type we have chosen as the log value is catenable!
--
-- Basically, when you try to write, say, <*> you have to say something like
--
-- (Writer l1 f) <*> (Writer l2 a) = Writer (l1 `***SOMETHING***` l2) (f a)
-- and you don't know what the something is.
--
-- You also need an "empty log" when defining "pure", BTW
--
-- One solution could be to define a class that had exactly the things that you want.
-- Basically, we need:
--   A "combining" operator that takes two elements of the type and returns a new element. Addition, of a sort.
--   An "empty" operator. We want it to be the case that "pure" doesn't add new log elements so it should be
--   something that acts as a sort of neutral element. Like "0" in addition, or "1" in multiplication.
--
-- class addablethingy a where
--   addthing   :: a -> a -> a
--   emptything :: a
--
-- Statements like " x `addthing` emptything === x " should be true. If they are then we know that
-- our addablethingy is offering the right behaviour.
--
--
-- There is such a class, as it happens:
-- 
-- class Monoid a where
--   (<>) :: a -> a -> a
--   mempty :: a
--
-- The sorts of statements that we want to be true (the "monoid laws") are
--   (x <> y) <> z = x <> (y <> z)   --- associativity
--   mempty <> x  = x  -- left identity
--   x <> mempty  = x  -- right identity
--
-- in fact, Monoid gets the (<>) operator from the class "semigroup" which is a class of things that have addition but
-- not necessarily any natural "empty" element.
-- 
-- Lists are monoids:
-- instance Monoid List where
--   (<>) = (++)
--   mempty = []
--
-- So we can make our generalised writer:


data Writer3 l a = Writer3 l a 


runWriter3 :: Monoid l => Writer3 l a -> (l, a)
runWriter3 (Writer3 l x) = (l,x)


instance Functor (Writer3 l) where
  fmap f (Writer3 log val) = Writer3 log (f val)
  
instance Monoid l => Applicative (Writer3 l) where
  pure x = Writer3 mempty x
  (Writer3 l1 f) <*> (Writer3 l2 a) = Writer3 (l1 <> l2) (f a)
  
instance Monoid l => Monad (Writer3 l) where
  return  = pure
  m >>= k = let Writer3 l a   = m
                Writer3 l' a' = k a
                in Writer3 (l<>l') a'


tell3 :: a -> Writer3 [a] ()
tell3 logMessage = Writer3 [logMessage] ()

example3 :: Writer3 [String] Int
example3 = do
  tell3 "entry 1"
  tell3 "entry 2"
  return (1+1)
  

test3 = (runWriter3 example3) == ( ["entry 1","entry 2"], 2)

