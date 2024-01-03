module Lib where

import System.Random
import Data.Array.IO
import Control.Monad (forM_)

-- Fisher-Yates shuffle implementation
-- It shuffles an input list in a random order.
fisherYatesShuffle :: [a] -> IO [a]
fisherYatesShuffle arr = do
    let arrLength = length arr
    newArray <- newListArray (0, arrLength - 1) arr
    forM_ [1..arrLength - 1] $ \i -> do
        j <- randomRIO (0, i)
        swapElements i j newArray
    getElems newArray

-- It swaps two elements in an IOArray at indices i and j. 
-- IOArray is used because Fisher-Yates shuffle requires in-place array modification, which is not possible with regular Haskell lists.
swapElements :: Int -> Int -> IOArray Int a -> IO ()
swapElements i j arr = do
    xi <- readArray arr i
    xj <- readArray arr j
    writeArray arr j xi
    writeArray arr i xj

-- It first shuffles the entire list using fisherYatesShuffle,
-- and then takes the first n elements of the shuffled list.
sampleN :: Int -> [a] -> IO [a]
sampleN n arr = do
    shuffledArr <- fisherYatesShuffle arr
    return (take n shuffledArr)
