module Render(Window,defaultWindow,samples,render) where
import Codec.Picture
--import Data.Colour
--import Data.Colour.Palette
import Shapes
import System.Random


--  A window specifies what part of the world to render and at which
--  resolution.
--  Values are top left & bottom right corner to be rendered, 
--             and the size of the output device to render into
data Window = Window Point Point (Int,Int)

-- Default window renders a small region around the origin into
-- a 50x50 pixel image
defaultWindow :: Window
defaultWindow = Window (point (-1.5) (-1.5)) (point 1.5 1.5) (500,500)


-- Generate a list of evenly spaced samples between two values.
-- e.g. samples -1.5 +1.5 25 generates 25 samples evenly spaced
--      between the two bounds: [ -1.5, -1.375, -1.25 ... ]
samples :: Double -> Double -> Int -> [Double]
samples c0 c1 n = take n [ c0, c0 + (c1-c0) / fromIntegral (n-1) .. ]

-- Generate the matrix of points corresponding to the pixels of a window.
pixels :: Window -> [[Point]]
pixels (Window p0 p1 (w,h)) =
  [ [ point x y | x <- samples (getX p0) (getX p1) w ]
                | y <- reverse $ samples (getY p0) (getY p1) h
  ]

-- generate list of all screen coordinates in window
coords :: Window -> [[(Int,Int)]]
coords (Window _ _ (w,h)) = [ [(x,y) | x <- [0..w]] | y <- [0..h] ]


-- To make the renderer more efficient I'll write a coordinate-transformer
-- that way the O(n) lookup of locations will become an O(1) calculation of locations

-- Linearly scale a value from the range [a1,a2] to the range [b1,b2]
scaleValue :: Fractional a => (a,a) -> (a,a) -> a -> a
scaleValue (a1,a2) (b1,b2) v = b1 + (v - a1) * (b2-b1) / (a2-a1)

-- Convert a screen-space point into an image-space point
-- in a specific window
mapPoint :: Window -> (Int,Int) -> Point
mapPoint (Window p0 p1 (w,h)) (x,y) = point scaledX scaledY
  where
    scaledX = scaleValue (0,fromIntegral w) (getX p0, getX p1) (fromIntegral x)
    scaledY = scaleValue (0,fromIntegral h) (getY p0, getY p1) (fromIntegral y)


-- Render a drawing into an image, then save into a file
-- This version relates the Shape language coordinates to the pixel coordinates
-- using the scaleValue function which is much faster than the original lookup based code.
render :: String -> Window -> Drawing -> IO ()
render path win sh = writePng path $ generateImage pixRenderer w h
    where
      Window _ _ (w,h) = win
      pixRenderer x y = PixelRGB8 r g b where (r,g,b) = colorForImage $ mapPoint win (x,y)


      -- Our colours from from 0-100. If we just bash them into Pixel8 values then we'll

      colorForImage :: Point -> (Pixel8,Pixel8,Pixel8)
      colorForImage p = makeColour (p `insidecolour` sh)


makeColour :: Int -> (Pixel8, Pixel8, Pixel8)
makeColour c = generateColours 101 !! c

generateColours :: Int -> [(Pixel8, Pixel8, Pixel8)]
generateColours n = take n $ zip3 rs gs bs
  where
    -- Generate random values for r, g, and b in the range [0, 255]
    rs = take n $ randomRs (0, 255) (mkStdGen 42)
    gs = take n $ randomRs (0, 255) (mkStdGen 43)
    bs = take n $ randomRs (0, 255) (mkStdGen 44)
