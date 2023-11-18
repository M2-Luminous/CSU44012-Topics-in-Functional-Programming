module Shapes  where

import Data.Word
-- Data type 
type Point  = Vector
type Drawing = [(Transform,Shape)]
type ColoredDrawing = [(Transform, Shape, Color)]
type WidthHeightRatio = Double

-- Shapes
{--

-
Haskell build in geomertry class :Deep embeded

--}




data Shape = Empty 
           | Circle 
           | Square
           | Rectangle WidthHeightRatio
           | Ellipse WidthHeightRatio
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

-- Transformations
{--

Transformation :Affine transformatio:
Translate,scale:vector mainpulation 
Rotatation  ,Shear :Matrix mainputation

Deep embeded
--}
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
{--
[cos -sin  ]
[sin  cos]
--}
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)
{--

1 0
sx 1
--}
shearX sx  = ShearX $ matrix (1)  (0) (sx) (1) 
{--

1 sy
0 1
--}
shearY sy  = ShearX $ matrix (1)  (sy) (0) (1)
{--
1 sy
sx 1
--}
shearXY sx sy = ShearXY $ matrix (1)  (sy) (sx) (1)


{--

Optimisation same type including 
Tranlsate,Scale ,Rotate,ShearX,Shear Y 

--}
Translate (Vector x1 y1) <+> Translate (Vector x2 y2) = Translate (Vector (x1+x2) (y1+y2))
Scale (Vector x1 y1) <+> Scale (Vector x2 y2) = Scale (Vector (x1+x2) (y1+y2))
Rotate angle1 <+> Rotate angle2 = Rotate (angle1 `add` angle2)
ShearX sx1 <+> ShearX sx2 = ShearX(sx1 `add`  sx2)
ShearY sy1 <+> ShearY sy2 = ShearY(sy1 `add`  sy2)
--ShearXY sx1 sy1 <*> ShearXY sx2 sy2 = Compose(ShearX(sx1 `add`  sx2) ShearY(sy1 `add`  sy2))
--t0 <*> t1 = Compose ((t0) (t1))
t0<+> t1= Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity                   x = id x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform( ShearX m)                 p = (invert m) `mult` p
transform( ShearY m)                 p = (invert m) `mult` p
transform( ShearXY m)                 p = (invert m) `mult` p

transform (Rotate m)                 p = (invert m) `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p


{--
ColorType:
Deep embeded since we need to merge into a tuple 
--}
data Color = Color {
  r :: Word8, 
  g :: Word8, 
  b:: Word8, 
  a ::Word8
} deriving Show

color r g b a = Color r g b a
red = Color 255 0 0 255
blue = Color 0 0 255 255
lime = Color 0 255 0 255
yellow = Color 255 255 0 255
maroon = Color 128 0 0 255
gray  = Color 128 128 128 255
transparent = Color 255 255 255 0
white = Color 255 255 255 255
-- average rgb of lime and yellow 
lime_and_yellow_average = Color 127 255  127 255
{--
Mask : Shallow embded only provide functioanlty
If the the shape of the first one is larger than the second one,the second one would 
be hinded
--}
maskFirstOverSecond :: Word8 -> ColoredDrawing -> ColoredDrawing -> ColoredDrawing
maskFirstOverSecond _ _ [] = []
maskFirstOverSecond alphaValue [] ((mt, ms, mc) : rest) = (mt, ms, (Color (r mc) (g mc) (b mc) alphaValue)) : maskFirstOverSecond alphaValue [] rest

maskFirstOverSecond alphaValue ((bt, bs, bc) : rest) (secondP) = (bt, bs, bc) : maskFirstOverSecond alphaValue rest secondP 


maskSecondOverFirst :: Word8 -> ColoredDrawing -> ColoredDrawing -> ColoredDrawing
maskSecondOverFirst _ _ [] = []
maskSecondOverFirst alphaValue ((mt, ms, mc) : rest) [] =  (mt, ms, (Color (r mc) (g mc) (b mc) alphaValue)) : maskSecondOverFirst alphaValue rest []
maskSecondOverFirst alphaValue firstP ((bt, bs, bc) : rest) = (bt, bs, bc) : maskSecondOverFirst alphaValue firstP rest 


colorPixel :: Point -> ColoredDrawing -> Color
colorPixel p drawing = mergeColor p drawing transparent 

mergeColor :: Point -> ColoredDrawing -> Color -> Color
mergeColor p [] col = col
mergeColor p ((t,s,c):ds) col = 
  if inside1 p (t, s) 
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
inside p d = or $ map (inside1 p) d

inside1 :: Point -> (Transform, Shape) -> Bool
inside1 p (t,s) = insides (transform t p) s

insides :: Point -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm  p <= 1
Vector x y `insides` Rectangle ratio = (sqrt (y**2) <= 1) && (sqrt (x**2) <= ratio)
Vector x y `insides` Ellipse ratio = (x**2/ratio**2) + (y**2/1**2) <= 1

Vector x y `insides` ConvexPolygon (p1:(p2:[])) = halfPlaneIntersection (Vector x y) p1 p2
Vector x y `insides` ConvexPolygon (p1:(p2:ps)) = if halfPlaneIntersection (Vector x y) p1 p2 then (Vector x y) `insides` ConvexPolygon (p2:ps) else False
--  Closewise  crossproduct < 0 
{--

Comptitive programming 
https://cp-algorithms.com/geometry/point-in-convex-polygon.html
--}
halfPlaneIntersection :: (Vector) -> (Vector) -> (Vector) -> Bool
halfPlaneIntersection (Vector x y) (Vector x1 y1) (Vector x2 y2) = innerLine_crossProduct (Vector (x1-x) (y1-y)) (Vector (x2-x1) (y2-y1)) < 0
                                                        where innerLine_crossProduct (Vector a b) (Vector c d) = a*d - b*c



---  Utility Functionality
distance :: Point -> Double
distance (Vector x y ) = sqrt ( x**2 + y**2 )

maxnorm :: Point -> Double
maxnorm (Vector x y ) = max (abs x) (abs y)

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
