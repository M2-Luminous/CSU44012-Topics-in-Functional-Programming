module Shapes  where

import Data.Word

-- Data types 
type Point  = Vector
type Drawing = [(Transform,Shape)]
type ColoredDrawing = [(Transform, Shape, Color)]
type AspectRatio = Double

data Shape = Empty 
           | Circle 
           | Square
           | Rectangle AspectRatio
           | Ellipse AspectRatio
           | ConvexPolygon [Point]
             deriving Show

empty, circle, square :: Shape
rectangle, ellipse :: Double -> Double -> Shape
convexPolygon :: [Point] -> Shape

empty = Empty
circle = Circle
square = Square
rectangle width height = Rectangle (width/height)
ellipse width height = Ellipse (width/height)
convexPolygon (p:ps) = ConvexPolygon (p:ps)

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | ShearX Matrix
           | ShearY Matrix
           | ShearXY Matrix
           | Compose Transform Transform
           | Rotate Matrix
             deriving Show

identity = Identity
translate = Translate
scale = Scale

rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)
shearX shearFactorX = ShearX $ matrix 1 0 shearFactorX 1 
shearY shearFactorY = ShearY $ matrix 1 shearFactorY 0 1
shearXY shearFactorX shearFactorY = ShearXY $ matrix 1 shearFactorY shearFactorX 1

Translate (Vector x1 y1) <+> Translate (Vector x2 y2) = Translate (Vector (x1+x2) (y1+y2))
Scale (Vector x1 y1) <+> Scale (Vector x2 y2) = Scale (Vector (x1+x2) (y1+y2))
Rotate angle1 <+> Rotate angle2 = Rotate (angle1 `add` angle2)
ShearX shearFactorX1 <+> ShearX shearFactorX2 = ShearX(shearFactorX1 `add`  shearFactorX2)
ShearY shearFactorY1 <+> ShearY shearFactorY2 = ShearY(shearFactorY1 `add`  shearFactorY2)

t0 <+> t1= Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity                   x = id x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform( ShearX m)                 p = (invert m) `mult` p
transform( ShearY m)                 p = (invert m) `mult` p
transform( ShearXY m)                p = (invert m) `mult` p

transform (Rotate m)                 p = (invert m) `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p

-- Colors
data Color = Color {
    r :: Word8, 
    g :: Word8, 
    b :: Word8, 
    a :: Word8
} deriving Show

createColor :: Word8 -> Word8 -> Word8 -> Word8 -> Color
createColor = Color

red, green, blue, pink, aquamarine, salmon, gold, khaki, turquoise, powderblue, transparent :: Color
red = createColor 255 0 0 255
green = createColor 0 255 0 255
blue = createColor 0 0 255 255
pink = createColor 255 192 203 255
aquamarine = createColor 127 255 212 255
salmon = createColor 250 128 114 255
gold = createColor 255 215 0 255
khaki = createColor 255 246 143 255
turquoise = createColor 64 224 208 255
powderblue = createColor 176 224 230 255
transparent = createColor 255 255 255 0

-- Mask functions
switchingLayer:: Word8 -> ColoredDrawing -> ColoredDrawing -> ColoredDrawing

switchingLayer _ _ [] = []
switchingLayer alphaValue [] ((mt, ms, mc) : rest) = (mt, ms, mc { a = alphaValue }) : switchingLayer alphaValue [] rest
switchingLayer alphaValue ((bt, bs, bc) : rest) secondP = (bt, bs, bc) : switchingLayer alphaValue rest secondP 

colorPixel :: Point -> ColoredDrawing -> Color
colorPixel p drawing = mergeColor p drawing transparent 

mergeColor :: Point -> ColoredDrawing -> Color -> Color
mergeColor p [] col = col
mergeColor p ((t,s,c):ds) col = 
  if insideShape p (t, s) 
    then recursive_call alphabender 
    else recursive_call col 
    where
    recursive_call = mergeColor p ds
    alphabender = (Color (blendColor (r col) (r c) (a c)) (blendColor (g col) (g c) (a c)) (blendColor (b col) (b c) (a c)) 255)
--  Foreground and background seperation boudnary
blendColor :: Word8 -> Word8 -> Word8 -> Word8 
blendColor backgroundCol foregroundCol foregroundAlpha = fromIntegral $ (bc * (255 - fa) + fc * fa) `div` 255 
     where 
          bc = fromIntegral backgroundCol
          fc = fromIntegral foregroundCol
          fa = fromIntegral foregroundAlpha


---  Inside functionality Added Eclise Recentangle and ConvexPolygon 
inside :: Point -> Drawing -> Bool
inside point drawing = or $ map (insideShape point) drawing

insideShape :: Point -> (Transform, Shape) -> Bool
insideShape point (t,s) = isInsideShape (transform t point) s

isInsideShape :: Point -> Shape -> Bool
point `isInsideShape` Empty = False
point `isInsideShape` Circle = distance point <= 1
point `isInsideShape` Square = maxnorm  point <= 1
Vector x y `isInsideShape` Rectangle ratio = (sqrt (y**2) <= 1) && (sqrt (x**2) <= ratio)
Vector x y `isInsideShape` Ellipse ratio = (x**2/ratio**2) + (y**2/1**2) <= 1

Vector x y `isInsideShape` ConvexPolygon (p1:(p2:[])) = halfPlaneIntersection (Vector x y) p1 p2
Vector x y `isInsideShape` ConvexPolygon (p1:(p2:ps)) = if halfPlaneIntersection (Vector x y) p1 p2 then (Vector x y) `isInsideShape` ConvexPolygon (p2:ps) else False

halfPlaneIntersection :: Point -> Point -> Point -> Bool
halfPlaneIntersection (Vector x y) (Vector x1 y1) (Vector x2 y2) =
    crossProduct (Vector (x1 - x) (y1 - y)) (Vector (x2 - x1) (y2 - y1)) < 0
    where crossProduct (Vector a b) (Vector c d) = a * d - b * c

-- Utility functions
distance, maxnorm :: Point -> Double
distance (Vector x y) = sqrt (x^2 + y^2)
maxnorm (Vector x y) = max (abs x) (abs y)

-- Geomertry Utility Function

data Vector = Vector Double Double
              deriving Show
vector = Vector
point :: Double -> Double -> Point
point = vector
cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

add :: Matrix -> Matrix -> Matrix
add (Matrix (Vector a1 b1) (Vector c1 d1)) (Matrix (Vector a2 b2) (Vector c2 d2)) = Matrix (Vector (a1+a2) (b1+b2)) (Vector (c1+c2) (d1+d2))

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c
        
-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX (Vector x y) = x
getY (Vector x y) = y
