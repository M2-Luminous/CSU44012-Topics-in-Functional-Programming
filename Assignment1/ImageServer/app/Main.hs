{-# LANGUAGE OverloadedStrings #-}
import Data.Text.Lazy
import qualified Web.Scotty as Scotty
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes hiding (id)
import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.String (fromString)

import Shapes
import Render 

main = do 
    rgbRender "circleDrawing.png" defaultWindow circleDrawing
    rgbRender "recetangleDrawing.png" defaultWindow recetangleDrawing
    rgbRender "polygonDrawing.png" defaultWindow polygonDrawing
    rgbRender "ecplliseDrawing.png" defaultWindow ecplliseDrawing
    rgbRender "eclipseOverPolygon.png" defaultWindow eclipseOverPolygon
    rgbRender "polygonOverEclipse.png" defaultWindow polygonOverEclipse
    rgbRender "transparentOverlapping.png" defaultWindow transparentOverlapping
    
    Scotty.scotty 3000 $ do
        Scotty.get "/" $ do
            Scotty.html $ response
        Scotty.get "/images/:image" $ do 
            image <- Scotty.param "image"
            Scotty.file ("./" ++ image ++ ".PNG")
        
response :: Text
response = renderHtml $ do
    H.head $ do
        H.style (".content { \n \
        \    max-width: 2000px; \n\
        \    margin: auto; \n\
        \    text-align: center; \n\
        \} \n\
        \.image { \n\
        \    display: block; \n\
        \    margin: auto; \n\
        \    max-width: 100%; \n\
        \    height: auto; \n\
        \}")
    H.head $ do
        H.style (".content { \n \
        \    max-width: 2000px; \n\
        \    margin: auto; \n\
        \    text-align: center; \n\
        \} \n\
        \img { display: none; }")
        H.script "function showImage(imgId) { \n\
        \    var images = document.getElementsByClassName('image'); \n\
        \    for (var i = 0; i < images.length; i++) { \n\
        \        images[i].style.display = 'none'; \n\
        \    } \n\
        \    document.getElementById(imgId).style.display = 'block'; \n\
        \}"
    H.body $ do
        H.div ! A.class_ "content" $ do
            H.h1 "Image eDSL Server"
            H.button ! A.onclick "showImage('circleImage')" $ "Show Circle"
            H.button ! A.onclick "showImage('rectangleImage')" $ "Show Rectangle"
            H.button ! A.onclick "showImage('polygonImage')" $ "Show Convex Polygon"
            H.button ! A.onclick "showImage('ellipseImage')" $ "Show Ellipse"
            H.button ! A.onclick "showImage('EoverP')" $ "Show Mask First"
            H.button ! A.onclick "showImage('PoverE')" $ "Show Mask Second"
            H.button ! A.onclick "showImage('Translucent')" $ "Show Translucent"

            H.img ! A.id (fromString "circleImage") ! A.class_ "image" ! A.src "images/circleDrawing" ! A.width "50%"
            H.img ! A.id (fromString "rectangleImage") ! A.class_ "image" ! A.src "images/recetangleDrawing" ! A.width "50%"
            H.img ! A.id (fromString "polygonImage") ! A.class_ "image" ! A.src "images/polygonDrawing" ! A.width "50%"
            H.img ! A.id (fromString "ellipseImage") ! A.class_ "image" ! A.src "images/ecplliseDrawing" ! A.width "50%"
            H.img ! A.id (fromString "EoverP") ! A.class_ "image" ! A.src "images/eclipseOverPolygon" ! A.width "50%"
            H.img ! A.id (fromString "PoverE") ! A.class_ "image" ! A.src "images/polygonOverEclipse" ! A.width "50%"
            H.img ! A.id (fromString "Translucent") ! A.class_ "image" ! A.src "images/transparentOverlapping" ! A.width "50%"

            -- New section for shape details
            H.h2 "Shape Details"
            H.ul $ do
                H.li "Circle 1: Scale with 3 * 3 factor on x and y axis and translate to (-1,1), color pink"
                H.li "Circle 2: Scale normally and translate to (4,3), color red"
                H.li "Rectangle 1: ShearX with parameter 2, translate to (-3,0), rectangle width 4 height 3, color poderblue"
                H.li "Rectangle 2: ShearY with parameter 2, translate to (0,0), rectangle width 4 height 3, color aquamarine"
                H.li "Rectangle 3: ShearXY with parameters 2 and 2, translate to (3,0), rectangle width 4 height 3, color turquoise"
                H.li "Polygon: Scale to (3,3), Translate to (0,0), using 4 parameter(-1,0|1,0|0,1.732|0,-1.732)"
                H.li "Ellipse 1: Rotate by 90 degrees, shear by 8 2, color gold."
                H.li "Ellipse 2: Rotate by 270 degrees, shear by 8 2, color khaki."
                H.li "Applied eclipse over polygon by Transparency 255."
                H.li "Applied polygon over eclipse by Transparency 255."

            -- New section for eDSL code details
            H.h2 "eDSL Code Details"
            H.ul $ do
                H.li "empty, circle, square :: Shape"
                H.li  "rectangle, ellipse :: Double -> Double -> Shape"
                H.li  "polygon :: [Point] -> Shape"                      
                H.li  "empty = Empty"
                H.li  "circle = Circle"
                H.li  "square = Square"
                H.li  "rectangle width height = Rectangle (width/height)"
                H.li  "ellipse width height = Ellipse (width/height)"
                H.li  "polygon (p:ps) = Polygon (p:ps)"

            H.h2 "Render eDSL Code Details"
            H.ul $ do
                H.li "samples :: Double -> Double -> Int -> [Double]"
                H.li "samples start end numSamples = [start + (end - start) * fromIntegral i / fromIntegral (numSamples - 1) | i <- [0..numSamples-1]]"
                H.li "sampleCoordinate :: Int -> Double -> Double -> Int -> Double"
                H.li "sampleCoordinate index start end total = start + ((end - start) / fromIntegral (total - 1)) * fromIntegral index"
                H.li "mapPixel :: (Int, Int) -> Window -> Point"
                H.li "mapPixel (pixelX, pixelY) (Window topLeft bottomRight (width, height)) = "
                H.li "point (sampleCoordinate pixelX (getX topLeft) (getX bottomRight) width)"
                H.li "(sampleCoordinate (height - pixelY) (getY topLeft) (getY bottomRight) height)"
                H.li "array ((0, 0), (width - 1, height - 1))"
                H.li "[((x, y), mapPixel (x, y) window) | x <- [0..width-1], y <- [0..height-1]]"
                H.li "render :: String -> Window -> Drawing -> IO ()"
                H.li "render filePath window shape = writePng filePath $ generateImage pixRenderer width height"
                H.li "Window _ _ (width, height) = window"
                H.li "pm = computePixelMap window"
                H.li "colorForImage p | p `inside` shape = 255"
                H.li "| otherwise     = 0"
                H.li "rgbRender :: String -> Window -> ColoredDrawing -> IO ()"
                H.li "rgbRender filePath window shape = writePng filePath $ generateImage pixRenderer width height"
                H.li "Window _ _ (width, height) = window"
                H.li "pm = computePixelMap window"
                H.li "generatePoint (x, y) = pm ! (x, y)"
                H.li "pixRenderer x y = toPixelRGBA8 $ colorPixel (generatePoint (x, y)) shape"
                H.li "toPixelRGBA8 col = PixelRGBA8 (r col) (g col) (b col) (a col)"

            

-- Shape definitions
circleDrawing = [
    (scale (point 3 3) <+> (translate (point (-1) (1))), circle, pink),  -- Circle 1
    (identity <+> (translate (point (4) (3))), circle, red)]             -- Circle 2
    
recetangleDrawing = [(shearX 2 <+> (translate (point (-3) (0))), rectangle 4 3, powderblue), -- Rectangle 1
                     (shearY 2 <+> (translate (point (0) (0))), rectangle 4 3, aquamarine),  -- Rectangle 2
                     (shearXY 2 2 <+> (translate (point (3) (0))), rectangle 4 3, turquoise)]-- Rectangle 3


polygonDrawing = [(scale (point 3 3) <+> (translate (point (0) (0))), polygon [(point (-1) 0), (point (0) 1.732), (point (1) (0)), (point 0 (-1.732)), (point (-1) 0)], salmon)]

ecplliseDrawing = [(rotate 90, ellipse 8 2, gold),      -- Eclipse 1
                   (rotate 270, ellipse 8 2, khaki)]    -- Eclipse 2

eclipseOverPolygon = switchingLayer 255 polygonDrawing ecplliseDrawing
polygonOverEclipse = switchingLayer 255 ecplliseDrawing polygonDrawing

transparentOverlapping = switchingLayer 130 polygonDrawing ecplliseDrawing