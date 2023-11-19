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
    rgbRender "convexPolygonDrawing.png" defaultWindow convexPolygonDrawing
    rgbRender "ecplliseDrawing.png" defaultWindow ecplliseDrawing
    rgbRender "maskExampleFirstOverSecondDrawing.png" defaultWindow maskExampleFirstOverSecondDrawing
    rgbRender "maskExampleSecondOverFirstDrawing.png" defaultWindow maskExampleSecondOverFirstDrawing
    
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
            H.h1 "Image Shape DSL Server"
            H.button ! A.onclick "showImage('circleImage')" $ "Show Circle"
            H.button ! A.onclick "showImage('rectangleImage')" $ "Show Rectangle"
            H.button ! A.onclick "showImage('convexPolygonImage')" $ "Show Convex Polygon"
            H.button ! A.onclick "showImage('ellipseImage')" $ "Show Ellipse"
            H.button ! A.onclick "showImage('maskFirstImage')" $ "Show Mask First"
            H.button ! A.onclick "showImage('maskSecondImage')" $ "Show Mask Second"

            H.img ! A.id (fromString "circleImage") ! A.class_ "image" ! A.src "images/circleDrawing" ! A.width "50%"
            H.img ! A.id (fromString "rectangleImage") ! A.class_ "image" ! A.src "images/recetangleDrawing" ! A.width "50%"
            H.img ! A.id (fromString "convexPolygonImage") ! A.class_ "image" ! A.src "images/convexPolygonDrawing" ! A.width "50%"
            H.img ! A.id (fromString "ellipseImage") ! A.class_ "image" ! A.src "images/ecplliseDrawing" ! A.width "50%"
            H.img ! A.id (fromString "maskFirstImage") ! A.class_ "image" ! A.src "images/maskExampleFirstOverSecondDrawing" ! A.width "50%"
            H.img ! A.id (fromString "maskSecondImage") ! A.class_ "image" ! A.src "images/maskExampleSecondOverFirstDrawing" ! A.width "50%"

            -- New section for shape details
            H.h2 "Shape Details"
            H.ul $ do
                H.li "Circle: No transformations applied."
                H.li "Rectangle: ShearXY with parameters 2 and 6."
                H.li $ do
                    "Convex Polygon: Composed transformations - "
                    H.ul $ do
                        H.li "ShearXY with parameters 3 and 9."
                        H.li "Rotate by 56 degrees."
                        H.li "Translate by point (-6, -6) followed by point (6, 6)."
                H.li "Ellipse: Rotate by 30 degrees."
                H.li "Mask Examples: Applied over circle and rectangle drawings with mask parameter 120."
            

-- Shape definitions
circleDrawing = [
    (scale (point 3 3) <+> (translate (point (-1) (1))), circle, pink),  -- Circle 1
    (identity <+> (translate (point (4) (3))), circle, red)]             -- Circle 2
    
recetangleDrawing = [(shearX 2 <+> (translate (point (-3) (0))), rectangle 4 3, powderblue), -- Rectangle 1
                     (shearY 2 <+> (translate (point (0) (0))), rectangle 4 3, aquamarine),  -- Rectangle 2
                     (shearXY 2 2 <+> (translate (point (3) (0))), rectangle 4 3, turquoise)]-- Rectangle 3


convexPolygonDrawing = [(scale (point 2 2) <+> (translate (point (0) (-2))), convexPolygon [(point (0) 3.464), (point (2) 0), (point (-2) 0), (point (0) 3.464)], salmon)]

ecplliseDrawing = [(rotate 90, ellipse 8 2, gold),      -- Eclipse 1
                   (rotate 270, ellipse 8 2, khaki)]    -- Eclipse 2

maskExampleFirstOverSecondDrawing = switchingLayer 255 convexPolygonDrawing ecplliseDrawing
maskExampleSecondOverFirstDrawing = switchingLayer 255 ecplliseDrawing convexPolygonDrawing