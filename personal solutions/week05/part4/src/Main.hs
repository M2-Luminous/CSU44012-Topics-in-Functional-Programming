module Main where

import Codec.Picture (writePng)

import Render (renderCircles)

main :: IO ()
main = do
  writePng "output.png" renderCircles
  putStrLn "Image Completed"