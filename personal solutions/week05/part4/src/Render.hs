module Render where

import Codec.Picture (Image, Pixel8, generateImage)

-- Define the properties of the image
width, height :: Int
width = 800
height = 800

-- Define the center and radius of the two circles
circle1CenterX, circle1CenterY, circle1Radius :: Int
circle1CenterX = 300
circle1CenterY = 400
circle1Radius = 100

circle2CenterX, circle2CenterY, circle2Radius :: Int
circle2CenterX = 600
circle2CenterY = 400
circle2Radius = 100

-- Function to render the two circles
renderCircles :: Image Pixel8
renderCircles = generateImage pixelRenderer width height
  where
    pixelRenderer x y
      | (x - circle1CenterX) ^ 2 + (y - circle1CenterY) ^ 2 <= circle1Radius ^ 2 = 255
      | (x - circle2CenterX) ^ 2 + (y - circle2CenterY) ^ 2 <= circle2Radius ^ 2 = 255
      | otherwise = 0
