module Lib where

import System.Random
import Data.Array.IO
import Control.Monad (forM_)

-- Fisher-Yates shuffle implementation
fisherYatesShuffle :: [a] -> IO [a]
fisherYatesShuffle arr = do
    let arrLength = length arr
    newArray <- newListArray (0, arrLength - 1) arr
    forM_ [1..arrLength - 1] $ \i -> do
        j <- randomRIO (0, i)
        swapElements i j newArray
    getElems newArray

-- Function to swap elements in an IOArray
swapElements :: Int -> Int -> IOArray Int a -> IO ()
swapElements i j arr = do
    xi <- readArray arr i
    xj <- readArray arr j
    writeArray arr j xi
    writeArray arr i xj

-- SampleN function remains the same
sampleN :: Int -> [a] -> IO [a]
sampleN n arr = do
    shuffledArr <- fisherYatesShuffle arr
    return (take n shuffledArr)
