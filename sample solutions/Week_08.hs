-- Homework 8 solution
-- I have made minimal changes to this file to add a logger. To help you find the changes
-- there are comments before each declaration that's changed. Search for
-- the string "logger" to find them.


-- I want these language extensions for my syntactic sugaring tricks at the end

{-# Language MultiParamTypeClasses, FlexibleInstances #-}
-- compile this module with -package mtl

module Interpreter7a where

-- I want my own definition of lookup and I want to write my own function named "print".

import Prelude hiding (lookup, print)

import qualified Data.Map as Map
import Data.Maybe

-- I want to get at the standard "print" function using the name System.print

import qualified System.IO as System

-- I plan to use these monads to construct the parts of my interpreter

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

{-------------------------------------------------------------------}
{- The pure expression language                                    -}
{-------------------------------------------------------------------}

data Val = I Int | B Bool
           deriving (Eq, Show)

data Expr = Const Val
     | Add Expr Expr | Sub Expr Expr  | Mul Expr Expr | Div Expr Expr
     | And Expr Expr | Or Expr Expr | Not Expr
     | Eq Expr Expr | Gt Expr Expr | Lt Expr Expr
     | Var Name
   deriving (Eq, Show)

type Name = String
type Env = Map.Map Name Val

lookup :: String -> Env -> Eval Val
lookup k t = case Map.lookup k t of
               Just x -> return x
               Nothing -> throwError ("Unknown variable "++k)


{-- Monadic style expression evaluator,
 -- with error handling and Reader monad instance to carry dictionary
 --}

type Eval a = ReaderT Env (ExceptT String Identity) a
runEval env ex = runIdentity ( runExceptT ( runReaderT ex env) )

{--
 -- Exercise: This evaluator could be a little neater
 -- Integer typed expressions
--}

evali op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (I i0, I i1) -> return $ I (i0 `op` i1)
                         _            -> throwError "type error in arithmetic expression"

-- Boolean expressions

evalb op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                         (B i0, B i1) -> return $ B (i0 `op` i1)
                         _            -> throwError "type error in boolean expression"

-- Operations over integers which produce Booleans

evalib op e0 e1 = do e0' <- eval e0
                     e1' <- eval e1
                     case (e0', e1') of
                          (I i0, I i1) -> return $ B (i0 `op` i1)
                          _            -> throwError "type error in arithmetic expression"

-- Evaluate an expression

eval :: Expr -> Eval Val
eval (Const v) = return v
eval (Add e0 e1) = evali (+) e0 e1
eval (Sub e0 e1) = evali (-) e0 e1
eval (Mul e0 e1) = evali (*) e0 e1
eval (Div e0 e1) = evali div e0 e1

eval (And e0 e1) = evalb (&&) e0 e1
eval (Or e0 e1) =  evalb (||) e0 e1

eval (Not e0  ) = evalb (const not) e0 (Const (B True))

eval (Eq e0 e1) = evalib (==) e0 e1
eval (Gt e0 e1) = evalib (>) e0 e1
eval (Lt e0 e1) = evalib (<) e0 e1

eval (Var s) = do env <- ask
                  lookup s env


{-------------------------------------------------------------------}
{- The statement language                                          -}
{-------------------------------------------------------------------}


data Statement = Assign String Expr
               | If Expr Statement Statement
               | While Expr Statement
               | Print Expr
               | Seq Statement Statement
               | Try Statement Statement
               | Pass
      deriving (Eq, Show)

-- The 'Pass' statement is useful when making Statement an instance of
-- Monoid later on, we never actually expect to see it in a real program.

-- LOGGER
--   I've changed this 'Run' monad to include a new transformer, WriterT.
--   the choice about where to put it in the chain of transformers is a choice
--   about how the writer is managed.
--   I've put it around the exception monad, and inside the state monad.
--   The implication of that choice is that if the program runs properly to
--   completion then there will be a log of the progress, but if it throws an
--   exception we lose both the state and the log.
--   I could have transformed IO (the innermst monad) instead. Then I'd need to
--   use 'lift' an extra time to access writer, and I'd have to handle the log in
--   the error case of 'run'. Making that chance might be an interesting challenge for you.
-- 
type Run a = StateT Env (WriterT [String] (ExceptT String IO)) a

set :: (Name, Val) -> Run ()
set (s,i) = state $ (\table -> ((), Map.insert s i table))

exec :: Statement -> Run ()

-- LOGGER
-- I've added one new action here that adds a string to the log...
--   I have to use "lift" here because "tell" is not a state monad
--   action, it's a writer monad action. So we have to promote the function
--   one level in the stack of monads.
--
--   all the equations of 'exec' are modified the same way.
exec (Assign s v) = do st <- get
                       lift $ tell ["Doing assignment of " ++ show s ++ " to " ++ show v]
                       case runEval st (eval v) of
                         Right val -> set (s,val)
                         Left err  -> throwError err

exec (Seq s0 s1) = do exec s0 >> exec s1

exec (Print e) = do st <- get
                    lift $ tell ["printing " ++ show e]
                    case  runEval st (eval e) of
                      Right val -> printout val
                      Left err  -> throwError err

exec (If cond s0 s1) = do st <- get
                          lift $ tell ["if, splitting on " ++ show cond]
                          case runEval st (eval cond) of
                            Right (B val) -> do
                              if val then do exec s0 else do exec s1
                            Left err -> throwError err

exec (While cond s) = do st <- get
                         lift $ tell ["while, looping on " ++ show cond]
                         case runEval st (eval cond) of
                           Right (B val) -> do
                             if val then do exec s >> exec (While cond s) else return ()
                           Left err -> throwError err

exec (Try s0 s1) = do catchError (exec s0) (\e -> exec s1)

-- We never actually expect to encounter one of these, the programs should run fine if we left this equation out:

-- LOGGER
--  I forgot to add a log action here. I guess, add it yourself?
exec Pass = return ()

{--

The transformer libraries define an overloaded "liftIO" operation that passes the required operation along the stack of monads to the next "liftIO" in line until the actual IO monad is reached. In this case it's equivalent to :

 lift . lift . System.IO.print

because we have to pass through StateT and ExceptT to reach the IO monad.

--}
printout :: Val -> Run ()
printout = liftIO . System.print

{-------------------------------------------------------------------}
{- Pour some sugar over this -}
{-------------------------------------------------------------------}

{- This next section deals exclusively with defining convenience functions -}
{- which we will use when writing the actual programs in the DSL. -}

-- A couple of convenience functions so that we don't have to litter the program
-- with ``obvious'' constructors

int = Const . I
bool = Const . B
var = Var

-- The idea is that we can put a simple value on the RHS and have Haskell select the correct
-- instance by inspecting the type.

class SmartAssignment a where
  assign :: String -> a -> Statement

instance SmartAssignment Int where
  assign v i = Assign v (Const (I i))

instance SmartAssignment Bool where
  assign v b = Assign v (Const (B b))

instance SmartAssignment Expr where
  assign v e = Assign v e

-- going further with this (and only providing the classes and instances we actually usein the example program, but there could be others)

class PrettyExpr a b where
  (.*) :: a -> b -> Expr
  (.-) :: a -> b -> Expr

instance PrettyExpr String String where
  x .* y = (Var x) `Mul` (Var y)
  x .- y = (Var x) `Sub` (Var y)

instance PrettyExpr String Int where
  x .* y = (Var x) `Mul` (Const (I y))
  x .- y = (Var x) `Sub` (Const (I y))


{--
Making use of this we can write a program in a slightly nicer style:

I feel we're hitting the point of diminishing returns here, but I
fancy one last example of using a monad. Something to remove the need
to explicitely write "Seq" inbetween each pair of statements. Recall
that I said that >>= could be though of as a progammable semicolon?
--}


type Program = Writer Statement ()


{--
The writer monad has an operation, "tell" which appends a piece of
output to an accumulated value. For this to work the type we are
accumulating (Statement, in this case) must be have both an appending
(plus-like) operation and a base (zero-like) operation. In algebra
something with that structure is called a Monoid:
--}


-- Semigroup: how do we join two values together?
instance Semigroup Statement where
  a <> b = a `Seq` b

-- monoid: a semigroup with an identity
instance Monoid Statement where
  mempty = Pass
--  mappend a b = (<>)


{--

The idea is that the bind of the Writer monad will be used to apply
Seq (our semicolon-like operation) between each "statement". The zero
statement is needed for theoretical completeness, but it will only
appear in a result if we were to write something like this:

junk :: Program
junk = return ()

For this reason we never expect to see it in a real "compiled"
statement, so there's no case for it in the exec function.

Converting a Program to a Statement just means running the writer monad:
--}

compile :: Program -> Statement
compile p = snd . runIdentity $ runWriterT p

{--
Executing a "program" means compiling it and then running the
resulting Statement with an empty variable map.
--}

-- LOGGER
--   I've changed this definition around a bit to try to make it easier to read.
--   The actual changes to support the writer monad are minimal:
--     * a call to 'runWriterT'
--     * a change to the pattern match of 'result' to handle the fact that we now get
--       the final state wrapped in a tuple with the log.
--
run :: Program -> IO ()
run program = do result <- runExceptT exceptionOrLog
                 case result of
                      Right ( ( (), env), log ) -> printlog log
                      Left exn -> System.print ("Uncaught exception: "++exn) 
   where (_,prog) = runIdentity $ (runWriterT program)

         finalState :: WriterT [String] (ExceptT String IO) ((), Env)
         finalState = (runStateT $ exec $ prog) Map.empty

         exceptionOrLog :: ExceptT String IO (((), Env), [String])
         exceptionOrLog = runWriterT finalState

{--
Display log information. Obviously we could do all sorts of processing here,
but we'll just print out the log in a semi-readable form
--}
printlog :: [String] -> IO ()
printlog l = putStrLn "==========\n LOG:" >> mapM_ putStrLn l

{--
And finally some convenience functions for our syntactic sugar:
--}

infixl 1 .=
(.=) :: String -> Expr -> Program
var .= val = tell $ assign var val


{-- if is a keyword in Haskell so I can't hide it. I renamed it so: --}

iif :: Expr -> Program -> Program -> Program
iif cond tthen eelse = tell $ If cond (compile tthen) (compile eelse)

while :: Expr -> Program -> Program
while cond body = tell $ While cond (compile body)

{-- This is why I wanted to hide the system function "print": --}

print :: Expr -> Program
print e = tell $ Print e

try :: Program -> Program -> Program
try block recover = tell $ Try (compile block) (compile recover)

{--
Phew.

After all that our embedded imperative language is ready to go. Here's the factorial function in all it's glory:
--}

prog10 :: Program
prog10 = do
           "arg"     .= int 10
           "scratch" .= var "arg"
           "total"   .= int 1
           while ( (var "scratch") `Gt` (int 1) ) (
            do "total"   .=  "total" .* "scratch"
               "scratch" .= "scratch" .- (1::Int)
               print $ var "scratch"
            )
           print $ var "total"

{--
to run it just evaluate

run prog10
--}


