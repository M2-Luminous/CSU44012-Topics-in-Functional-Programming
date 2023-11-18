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

-- Shape definitions
circleDrawing = [(identity, circle, maroon)]
recetangleDrawing = [(shearXY 2 6, rectangle 4 3, lime_and_yellow_average)]
convexPolygonDrawing = [(((shearXY 3 9) <+> (rotate 56) <+> (translate (point (-6) (-6))) <+> (translate (point 6 6))), convexPolygon [(point (-1.8) 5.2), (point (3) 0), (point (-1.2) 0), (point (-1.8) 5.2)], yellow)]
ecplliseDrawing = [(rotate 30, ellipse 3 2, blue)]
maskExampleFirstOverSecondDrawing = maskFirstOverSecond 120 circleDrawing recetangleDrawing
maskExampleSecondOverFirstDrawing = maskSecondOverFirst 120 circleDrawing recetangleDrawing
