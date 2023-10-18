module Main where

import Shapes
import Render (render,defaultWindow)

exampleDrawing =  [ (scale (point 0.5 0.25) <+> translate (point 1.2 0.4), circle) ]

exampleDrawing2 = reverse $ (identity, circle) : take (floor copies) (map mand [ 0, (2*pi)/copies..])
  where mand n = ( rotate n <+> translate (point 0.5 0.5) <+>  scale (point 0.15 0.15), mandelbrotset)
        copies :: Double
        copies = 8


main = render "output.png" defaultWindow exampleDrawing2
