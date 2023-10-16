module Render(Window,defaultWindow,samples,render) where
import Codec.Picture
import Shapes


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

-- render a drawing into an image, then save into a file
-- NB: the lookup1 function is a VERY inefficient way to convert screen coordinates to drawing
--     coordinates! It should be possible to do this in O(1) time, not O(N) time!!
--     If you enlarge the viewport in defaultWindow from 50x50 to 500x500 then you will see the problem.
render :: String -> Window -> Drawing -> IO ()
render path win sh = writePng path $ generateImage pixRenderer w h
    where
      Window _ _ (w,h) = win

      pixRenderer x y = PixelRGB8 c c c where c = colorForImage $ mapPoint win (x,y)

      mapPoint :: Window -> (Int, Int) -> Point
      mapPoint (Window p0 p1 (w, h)) (x, y) = point
        (getX p0 + (fromIntegral x / fromIntegral w) * (getX p1 - getX p0))
        (getY p0 + (fromIntegral y / fromIntegral h) * (getY p1 - getY p0))

      colorForImage p | p `inside` sh = 255
                      | otherwise     = 0