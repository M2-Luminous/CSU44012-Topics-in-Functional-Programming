{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

import Prelude hiding (lookup, print)
import qualified Data.Map as Map
import Data.Maybe
import qualified System.IO as System
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer


-- Modified Eval to include WriterT for logging
type Eval a = ReaderT Env (ExceptT String (WriterT [String] Identity)) a 
type EvalLog = [String] -- Type for logging

runEval :: Env -> Eval a -> (Either String a, EvalLog)
runEval env ex = runIdentity (runWriterT (runExceptT (runReaderT ex env)))


-- The Statement data type
data Statement = Assign String Expr              -- Assignment statement
               | If Expr Statement Statement     -- Conditional statement
               | While Expr Statement            -- While loop
               | Print Expr                      -- Print statement
               | Seq Statement Statement         -- Sequence of two statements
               | Try Statement Statement         -- Try-Catch block
               | Pass                            -- No operation (Pass)
               -- Add more statement types as needed
               deriving (Eq, Show)


-- Logging function
logStatement :: String -> Eval ()
logStatement msg = tell [msg]

-- Modified evaluation function for statements
evalStmt :: Statement -> Eval ()
evalStmt (Assign s e) = do
    val <- eval e
    env <- ask
    let newEnv = Map.insert s val env
    logStatement $ "Assign " ++ s ++ " " ++ show val
    local (const newEnv) (return ())

evalStmt (Seq s1 s2) = do
    evalStmt s1
    evalStmt s2

evalStmt (If e s1 s2) = do
    B cond <- eval e
    logStatement $ "If " ++ show e
    if cond then evalStmt s1 else evalStmt s2

evalStmt (While e s) = do
    B cond <- eval e
    logStatement $ "While " ++ show e
    when cond $ evalStmt s >> evalStmt (While e s)

evalStmt (Print e) = do
    val <- eval e
    liftIO $ System.print val
    logStatement $ "Print " ++ show e
	
evalStmt :: Statement -> Eval ()
evalStmt (Seq s1 s2) = do
    evalStmt s1
    evalStmt s2
    logStatement $ "Seq (" ++ show s1 ++ ") (" ++ show s2 ++ ")"

evalStmt (Try s1 s2) = do
    catchError (evalStmt s1) (\_ -> evalStmt s2)
    logStatement $ "Try"

evalStmt Pass = do
    logStatement "Pass"

-- main function
main :: IO ()
main = do
    let env = Map.empty -- Initial empty environment
    let (result, log) = runEval env (evalStmt exampleProgram)
    putStrLn "Program Log:"
    mapM_ putStrLn log

-- Example Program
exampleProgram :: Statement
exampleProgram = Seq 
                    (Assign "x" (Const (I 5)))
                    (Seq
                        (Print (Var "x"))
                        (If (Gt (Var "x") (Const (I 3)))
                            (Assign "x" (Const (I 10)))
                            Pass)
                    )


-- Example Output
-- Program Log:
-- Assign x 5
-- Print x
-- If x > 3
-- Assign x 10

