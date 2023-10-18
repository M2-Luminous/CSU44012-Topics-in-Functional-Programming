module Shapes(
  Shape, Point, Vector, Transform, Drawing,
  point, getX, getY,
  empty, circle, square, mandelbrotset,
  identity, translate, rotate, scale, (<+>),
  inside, insidecolour)  where

-- Utilities

data Vector = Vector Double Double
              deriving Show
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

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

-- Shapes

type Point  = Vector

point :: Double -> Double -> Point
point = vector


data Shape = Empty
           | Circle
           | Square
           | MandelbrotSet
             deriving Show

empty, circle, square :: Shape

empty = Empty
circle = Circle
square = Square
mandelbrotset = MandelbrotSet

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Matrix
             deriving Show

identity = Identity
translate = Translate
scale = Scale
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity                   x = x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform (Rotate m)                 p = invert m `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p

-- Drawings

type Drawing = [(Transform,Shape)]

-- interpretation function for drawings

{--}
-- This is a translation of the Mandelbrot set program from
-- Mark Jones' paper.
-- The only meaningful change is that we use the Point type
-- from Shapes, and thus use getX and getY to project out the
-- x and y portions of the point.

-- "next" generates a series of points iteratively
-- If the points in the series end up being "close" to the original point
-- then we declare it to be part of the set, otherwise it is not.
next :: Point -> Point -> Point
next p0 p1 = point (x * x - y * y + u) (2 * x * y + v)
  where (u, v) = (getX p0, getY p0)
        (x, y) = (getX p1, getY p1)

mandelbrot :: Point -> [Point]
mandelbrot p = iterate (next p) (point 0 0)

fairlyClose :: Point -> Bool
fairlyClose p = (u * u + v * v) < 100
   where (u,v) = (getX p, getY p)

approxTest :: Int -> Point -> Bool
approxTest n p = all fairlyClose (take n (mandelbrot p))

{--}


inside :: Point -> Drawing -> Bool
inside p d = any (inside1 p) d

inside1 :: Point -> (Transform, Shape) -> Bool
inside1 p (t,s) = insides (transform t p) s

insides :: Point -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm  p <= 1
p `insides` MandelbrotSet = approxTest 100 p

distance :: Point -> Double
distance (Vector x y ) = sqrt ( x**2 + y**2 )

maxnorm :: Point -> Double
maxnorm (Vector x y ) = max (abs x) (abs y)


{--}


approxinside :: Point -> Shape -> Int

-- Most of our shapes have a very simple inside/outside relationship
-- and a Boolean is a nice thing to return. But when we look at fractals
-- it's possible to have 'degrees' of membership based on varying
-- parameters of the argument to "approxtest". That's what this next function
-- models. We return a number between 0 and 100 for the "insideness" of a point

p `approxinside` MandelbrotSet = approxinside' p
              where approxinside' = length . take 100 . takeWhile fairlyClose . mandelbrot

-- For anything else it's always 0 or 100, depending on what 'inside' says:

p `approxinside` s | p `insides` s = 100
                   | otherwise     = 0


-- Now we apply this approxinside test to every shape, with
-- transforms. In practical use this means that the fractal extends
-- very far across the plane, with big regions of colours 0..6 which
-- don't look very "fractal-y".  So what I did here was just make the
-- pretty arbitrary decision that any colour number from 1..5 would be
-- replaced with 0 (background), which makes things look a bit nicer.
-- A better solution might be to have a special kind of transform that
-- replace a specific colour number with 0, or a "mask" operation that
-- allowed us to convert a region of a transformed shape into background
-- colours, or something like that. The number "5" here was determined
-- experimentally by making something that looks "about right"

approxinside1 :: Point -> (Transform, Shape) -> Int
approxinside1 p (t, MandelbrotSet) | c < 5 = 0
                                   | otherwise = c
                    where c = approxinside (transform t p) MandelbrotSet

approxinside1 p (t,s) = approxinside (transform t p) s




-- we now write an "insidecolour" function that can be applied
-- to a while drawing. This will give us
-- a number from 0-100 for each point in the shape.
-- First, we get the colour numbers for each shape in the drawing.
-- If a colour is 0 then that means that the point was "outside" that
-- shape, so we'll take the colour of the _next_ point, and so on.
-- At the last shape, there's nothing underneath, so use it's colour.

-- We could do this in a fancy way with Maybe's and folding, but it's not
-- a saving in complexity here.

insidecolour :: Point -> Drawing -> Int
insidecolour p d = firstColour $ map (approxinside1 p) d -- head $ map (approxinside1 p) d 
                   where firstColour :: [Int] -> Int
                         firstColour [x]      = x -- Down to the last shape? Use it's colour
                         firstColour (0:xs) = firstColour xs -- skip any 100's unless we're at the end
                         firstColour (x:_)    = x -- if you find an "inside" colour return it.