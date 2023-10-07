--
-- Q 1
--

{- DO each operation in turn, and return the value of the last one -}
{-
   Because we have to return a value it's not possible to operate on
   an empty list
-}

f1 :: [IO a] -> IO a
f1 []     = error "Cannot operate on an empty list of actions"
f1 [x]    = x
f1 (x:xs) = x >> f1 xs


--
-- Q2
--

-- It prints "hello", of course. Even though the results of the actions are
-- thrown away by the (>>) operator the side-effects still take place.

--
-- Q3
--

-- This will print "Hi there".
-- the other IO actions don't contribute any values to the result of 'main'
-- and so they are never evaluted, via laziness
--
-- This illustrates the point that IO operations are not "statements" in the
-- sense that imperative languages have statements. They are values and only
-- have effects if they are are fully evaluated.

--
-- Q4
--

{- There are many ways to write this -}
while :: IO Bool -> IO ()
while loopBody = do
  cond <- loopBody
  case cond of
    True -> while loopBody
    _    -> return ()


{- using if/then -}
--while :: IO Bool -> IO ()
--while loopBody = do
--  cond <- loopBody
--  if cond then while loopBody else return ()

{-  this already exists, under the name doWhile_ in the Control.Monad.Extra library -}

--
--  Q5
--

{-
 - This is significantly more elegant that f1, because when we have an
 - empty list we can just return an empty list!
 -}

-- Again, several ways to write this. A simple and direct form:

f2 :: [IO a] -> IO [a]
f2 [] = return []
f2 (x:xs) = do 
              x' <- x
              xs' <- f2 xs
              return (x' : xs')

-- a fancy HoF way (this version is in Control.Monad as "sequence")
-- It also has a cousing sequence_ which just returns IO (), i.e. it
-- performs the side-effecting actions and throws away the actual results.

f2 = foldr mcons (return [])
     where mcons p q = p >>= \x -> q >>= \y -> return (x:y)

{--
 - btw, sequence_ is defined quite simply, since it doesn't need ot be
 - concerned with the values being produced. If f1 from part 1 had been
 - ignoring the results this would have been sufficient. 
 -}

sequence_ = foldr (>>) (return ())

read10 :: IO String
read10 = f2 $ take 10 actions
          where actions = getChar : actions
