--Q1
import Control.Monad (sequence)

f1 :: [IO a] -> IO a
f1 actions = do
  results <- sequence actions
  case results of
    []     -> error "Empty list"
    (x:xs) -> return x

--Q2
--Each putChar action prints a single character to the standard output, 
--and they are executed in the order they are provided in the actions list, 
--we will see "hello" printed on the screen.

--Q3
--a is the result of calling f1 with the actions list.
	--actions list are not executed because a is not used 
--b is defined using an if statement. 
	--However, Haskell is a lazy language, so b is not evaluated in this code block.
	--if statement is only evaluated when its value is actually needed
	--so the putChar instruction is never been used
--program calls putStr "Hi there" and prints "Hi there" to the standard output.

--Q4
while :: IO Bool -> IO () -> IO ()
while condition action = do
  cond <- condition
  if cond
    then action >> while condition action
    else return ()


--Q5
import Control.Monad

f2 :: [IO a] -> IO [a]
f2 [] = return []
f2 (x:xs) = do
  result <- x
  rest <- f2 xs
  return (result : rest)
  
--function f2 takes a list of IO actions and returns a list of their results
--This is a recursive process that executing each action and collecting result into list

read10 :: IO String
read10 = f2 $ take 10 actions
          where actions = getChar : actions
--defined read10 and use f2 function to read 10 characters
--action is a list of 10 getChar actions
