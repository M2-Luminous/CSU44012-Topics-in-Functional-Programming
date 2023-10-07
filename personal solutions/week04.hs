--Q1
module WriterMonad where

newtype Writer a = Writer { runWriter :: ([String], a) }			--Writer is a newtype wrapper for a tuple containing the list of log entries and the result value.

instance Monad Writer where											--Monad instances are defined for the Writer type to enable the use of standard monadic operations 
  return x = Writer ([], x)
  Writer (log1, x) >>= f = let Writer (log2, y) = f x in Writer (log1 ++ log2, y)

tell :: String -> Writer ()											--The tell function is used to append log entries to the current journal.
tell entry = Writer ([entry], ())

-- Usage:
example :: Writer Int
example = do
  tell "entry 1"
  tell "entry 2"
  return (1 + 1)

--import Control.Monad.Writer
--
--example :: Writer [String] Int
--example = do
--  tell ["entry 1", "entry 2"]
--  return (1 + 1)


--Q2
--When attempting to make the log type a parameter in the `Writer` monad, 
--we will encounter an issue related to ensuring the log type supports the required operations like appending and combining logs. 
--The issue arises from the fact that the Writer monad needs to append log entries to the log 
--during the tell operation and combine logs during monadic operations like >>= and >>.
--A potential solution is to use advanced type system features
--to define a generic log type that can be parameterized by the entry type,
--allowing flexibility while ensuring the necessary operations are supported. 
--This involves more complex type-level programming, which is beyond the scope of a basic `Writer` monad implementation.