-- This module is responsible for rendering shapes.
module Render where

-- Importing necessary modules for image processing and shape definitions.
import Codec.Picture
import Shapes
import Data.Array

-- Definition of a window for rendering, specified by two corner points and resolution.
data Window = Window Point Point (Int,Int)

-- Default window configuration with specified dimensions and resolution.
defaultWindow :: Window
defaultWindow = Window (point (-10) (-10)) (point 10 10) (2000,2000)

-- Function to generate a list of samples between a start and end value, given the number of samples.
samples :: Double -> Double -> Int -> [Double]
samples start end numSamples = [start + (end - start) * fromIntegral i / fromIntegral (numSamples - 1) | i <- [0..numSamples-1]]

-- Calculates the coordinate value for a given pixel index in a range.
sampleCoordinate :: Int -> Double -> Double -> Int -> Double
sampleCoordinate index start end total = start + ((end - start) / fromIntegral (total - 1)) * fromIntegral index

-- Maps pixel coordinates to their corresponding points in the window.
mapPixel :: (Int, Int) -> Window -> Point
mapPixel (pixelX, pixelY) (Window topLeft bottomRight (width, height)) = 
    point (sampleCoordinate pixelX (getX topLeft) (getX bottomRight) width) 
          (sampleCoordinate (height - pixelY) (getY topLeft) (getY bottomRight) height)

-- Computes an array mapping each pixel to a point in the window.
computePixelMap :: Window -> Array (Int, Int) Point
computePixelMap window@(Window topLeft bottomRight (width, height)) = 
  array ((0, 0), (width - 1, height - 1)) 
        [((x, y), mapPixel (x, y) window) | x <- [0..width-1], y <- [0..height-1]]

-- Renders a drawing to an image file.
render :: String -> Window -> Drawing -> IO ()
render filePath window shape = writePng filePath $ generateImage pixRenderer width height
  where
    Window _ _ (width, height) = window
    pm = computePixelMap window

    generatePoint (x, y) = pm ! (x, y)

    -- Pixel renderer that converts a point to a color based on whether it is inside the shape.
    pixRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) (colorForImage $ generatePoint (x, y))
    colorForImage p | p `inside` shape = 255
                    | otherwise     = 0

-- Renders a colored drawing to an image file.
rgbRender :: String -> Window -> ColoredDrawing -> IO ()
rgbRender filePath window shape = writePng filePath $ generateImage pixRenderer width height
  where
    Window _ _ (width, height) = window
    pm = computePixelMap window

    generatePoint (x, y) = pm ! (x, y)

    -- Pixel renderer for colored drawings.
    pixRenderer x y = toPixelRGBA8 $ colorPixel (generatePoint (x, y)) shape
    -- Converts a Color value to a PixelRGBA8 value.
    toPixelRGBA8 col = PixelRGBA8 (r col) (g col) (b col) (a col)
