module Render  where
import Codec.Picture
import Shapes
{--
{-- 
-
PixelRGB8 = PixelRGB8 !PixelRGB8 ! PixelRGB8 ! PixelRGB8 
JuicyPixel Offical document
https://hackage.haskell.org/package/JuicyPixels-3.3.8/docs/Codec-Picture.html
--}
--}


--  A window specifies what part of the world to render and at which
--  resolution.
--  Values are top left & bottom right corner to be rendered, 
--             and the size of the output device to render into
data Window = Window Point Point (Int,Int)

-- Default window renders a small region around the origin into
-- a 2000 *2000 pixel image
defaultWindow :: Window
defaultWindow = Window (point (-10) (-10)) (point 10 10) (2000,2000)


-- Generate a list of evenly spaced samples between two values.
-- e.g. samples -1.5 +1.5 25 generates 25 samples evenly spaced
--      between the two bounds: [ -1.5, -1.375, -1.25 ... ]
samples :: Double -> Double -> Int -> [Double]
samples c0 c1 n = take n [ c0, c0 + (c1-c0) / (fromIntegral $ n-1) .. ]

-- Generate the matrix of points corresponding to the pixels of a window.
pixels :: Window -> [[Point]]
pixels (Window p0 p1 (w,h)) =
  [ [ point x y | x <- samples (getX p0) (getX p1) w ]
                | y <- reverse $ samples (getY p0) (getY p1) h
  ]

samplePoint :: Int -> Double -> Double -> Int -> Double
samplePoint z c0 c1 n = c0 + (((c1-c0) / (fromIntegral $ n-1)) * fromIntegral(z))
-- Precompute  
-- yields the point corresponding to the pixel of a window.  
pixel :: (Int, Int) -> Window -> Point
pixel (x,y) (Window p0 p1 (w,h)) = 
    point pointX pointY
    where
      pointX = samplePoint x (getX p0) (getX p1) w
      pointY = samplePoint (h-y) (getY p0) (getY p1) h
-- generate list of all screen coordinates in window
coords :: Window -> [[(Int,Int)]]
coords (Window _ _ (w,h)) = [ [(x,y) | x <- [0..w]] | y <- [0..h] ]
{--

Instead for looking up the location online ,we could precompute the pixel in advance 
like we need to store into an array as the time compleixty for looking for an array 
is O（1）

--}
-- render a drawing into an image, then save into a file
-- NB: the lookup1 function is a VERY inefficient way to convert screen coordinates to drawing
--     coordinates! It should be possible to do this in O(1) time, not O(N) time!!
render :: String -> Window -> Drawing -> IO ()
render path win sh = writePng path $ generateImage pixRenderer w h
    where
      Window _ _ (w,h) = win

      pixRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) (colorForImage $  generatePoint (x,y))
      
      generatePoint :: (Int, Int) -> Point
      generatePoint (x, y) = 
          if (x >= 0 && x < w && y >= 0 && y < h) then pixel (x,y) win else point 0 0

      colorForImage p | p `inside` sh = 255
                      | otherwise     = 0
      {--      
      lookup1 :: (Int,Int) -> [((Int,Int), Point)] -> Point
      lookup1 a m = case lookup a m of
                      Nothing -> point 0 0
                      Just x  -> x

      locations :: [ ((Int,Int), Point) ]
      locations = concat $ zipWith zip (coords win) (pixels win)
      colorForImage p | p `inside` sh = 255
                      | otherwise     = 0
--}
-- type transformer
rgbRender :: String -> Window -> ColoredDrawing -> IO ()
rgbRender path win sh = writePng path $ generateImage pixRenderer w h
    where
      Window _ _ (w,h) = win

      pixRenderer x y = toPixelRGBA8 $ colorPixel (generatePoint (x,y)) sh
      
      toPixelRGBA8 :: Color -> PixelRGBA8
      toPixelRGBA8 col = PixelRGBA8 (r col) (g col) (b col) (a col)
      
      generatePoint :: (Int, Int) -> Point
      generatePoint (x, y) = 
          if (x >= 0 && x < w && y >= 0 && y < h) then pixel (x,y) win else point 0 0