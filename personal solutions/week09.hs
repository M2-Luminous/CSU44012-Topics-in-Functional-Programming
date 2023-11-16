{-# LANGUAGE OverloadedStrings #-}
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    -- Create a numeric display
    display <- UI.input # set UI.type_ "text" # set UI.id_ "display" # set style [("text-align", "right")]

    -- Create buttons for digits and operations
    let digits = ["7", "8", "9", "4", "5", "6", "1", "2", "3", "0"]
    digitButtons <- mapM (mkButton display) digits
    let ops = [("+", (+)), ("-", (-)), ("*", (*)), ("/", (/))]
    opButtons <- mapM (mkOpButton display) ops
    clearBtn <- UI.button #+ [string "C"]

    -- Arrange buttons in a grid
    let buttonGrid = grid
          [ take 3 digitButtons
          , take 3 (drop 3 digitButtons)
          , take 3 (drop 6 digitButtons)
          , [digitButtons !! 9, opButtons !! 0, opButtons !! 1]
          , [opButtons !! 2, opButtons !! 3, clearBtn]
          ]

    -- Layout
    getBody window #+ [column [element display, buttonGrid]]

    -- Set up event handling for the Clear button
    on UI.click clearBtn $ const $ do
        element display # set value ""

mkButton :: Element -> String -> UI Element
mkButton display label = do
    button <- UI.button #+ [string label]
    on UI.click button $ const $ do
        currentVal <- get value display
        element display # set value (currentVal ++ label)
    return button

mkOpButton :: Element -> (String, Double -> Double -> Double) -> UI Element
mkOpButton display (opLabel, operation) = do
    button <- UI.button #+ [string opLabel]
    on UI.click button $ const $ do
        currentVal <- get value display
        if null currentVal
            then return ()
            else do
                let lhs = readMaybe currentVal :: Maybe Double
                element display # set value ""
                on UI.click button $ const $ do
                    newVal <- get value display
                    let rhs = readMaybe newVal :: Maybe Double
                    case (lhs, rhs) of
                        (Just x, Just y) -> element display # set value (show (operation x y))
                        _                -> element display # set value "Error"
    return button
