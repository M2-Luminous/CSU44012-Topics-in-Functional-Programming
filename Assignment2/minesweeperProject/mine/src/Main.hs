{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where
import Mine
import Lib
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.SVG  as SVG
import Control.Monad
import Data.IORef
import Text.Printf
import Data.Maybe

makeBoard :: Int -> Int -> Int -> [(Int, Int)] -> IO Board
makeBoard dimx dimy count forbidden = do
    randomCells <- rnd
    return $ foldr placeMine initial randomCells
  where
    initial = emptyBoard (dimx, dimy)
    indices = [(x, y) | x <- [0..dimx - 1], y <- [0..dimy - 1], (x, y) `notElem` forbidden]
    rnd = sampleN count indices
    placeMine xy = put xy (Cell Mine Hidden)

    
sweepOrFlag :: Point -> Board -> (Board, Result)
sweepOrFlag (x, y) b
    | isFlagging = flagBoardAt point b
    | otherwise  = clickBoardAt point b
  where
    isFlagging = x < 0 && y < 0
    point = (abs x, abs y)

    
autoMove :: Board -> (Board, Result)
autoMove b = 
    case obviousMines b of
        (x:_) -> flagBoardAt x b
        _ ->
            case obviousNonMines b of
                (x:_) -> clickBoardAt x b
                _ -> 
                    (b, Continue)    

  
gameGUI :: IO ()
gameGUI = do
    let config = defaultConfig { jsAddr = Just "localhost", jsPort = Just 3000 }
    startGUI config setup
    
data GameState = 
    Start  | 
    Sweep  | 
    Mark   |
    Over 
    deriving (Show, Eq, Ord)
    
-- Helper function for creating input fields
createInputField :: String -> String -> [(String, String)] -> UI Element
createInputField id value styles = UI.input
    # set UI.id_ id
    # set UI.value value
    # set style styles

-- Helper function for creating buttons
createButton :: String -> [(String, String)] -> UI Element
createButton text styles = UI.button
    # set UI.text text
    # set style styles

setup :: Window -> UI ()
setup w = do
    t1 <- UI.div
        # set text "Welcome to Mine Sweeper Game"
        # set style [("color", "black"), ("font-30", "20px"), ("position", "absolute"), ("left", "630px"), ("top", "10px")]

    t <- UI.div # set UI.id_ "text"

    s1 <- createInputField "s1" "16" [("position", "absolute"), ("left", "1300px"), ("top", "50px"), ("transform", "scale(1.5)")]
    s2 <- createInputField "s2" "16" [("position", "absolute"), ("left", "1300px"), ("top", "100px"), ("transform", "scale(1.5)")]
    s3 <- createInputField "s3" "20" [("position", "absolute"), ("left", "1300px"), ("top", "150px"), ("transform", "scale(1.5)")]

    newgame <- createButton "New Games" [("position", "absolute"), ("left", "1300px"), ("top", "250px"), ("transform", "scale(2.0)")]
    mine <- createButton "Sweep Mine" [("position", "absolute"), ("left", "1300px"), ("top", "350px"), ("transform", "scale(2.0)")]
    mark <- createButton "Flag Mines" [("position", "absolute"), ("left", "1300px"), ("top", "450px"), ("transform", "scale(2.0)")]
    auto <- createButton "Play Moves" [("position", "absolute"), ("left", "1300px"), ("top", "550px"), ("transform", "scale(2.0)")]

    getBody w #+ [element s1, element s2, element s3, element newgame, element mine, element mark, element auto, element t1]
    container <- UI.div #. "wrap" # set UI.id_ "container"
    getBody w #+ [element t, element container]

    refb <- liftIO $ newIORef (Board (0, 0) [], Start)

    let updateGameState newState textUpdate = do
            (b, st) <- liftIO $ readIORef refb
            unless (st == Over) $ do
                void $ element t # set UI.text textUpdate
                liftIO $ writeIORef refb (b, newState)

    on UI.click newgame $ \_ -> do
        ss1 <- s1 # UI.get UI.value
        ss2 <- s2 # UI.get UI.value
        ss3 <- s3 # UI.get UI.value
        bw <- liftIO $ makeBoard (read ss1) (read ss2) (read ss3) []
        liftIO $ writeIORef refb (bw, Start)
        board2svg w refb (isClicked w refb)
        return t # set UI.text "New Game Started"
        return()

    on UI.click mine $ \_ -> updateGameState Sweep "Sweep"
    on UI.click mark $ \_ -> updateGameState Mark "Mark"
    on UI.click auto $ \_ -> do
        (b, st) <- liftIO $ readIORef refb
        case st of
            Over -> void $ return t # set UI.text "Game Already Over"
            _    -> do
                void $ return t # set UI.text "Auto Player Running"
                let (newb, verdict) = autoMove b
                case verdict of
                    GameOver -> do
                        liftIO $ writeIORef refb (newb, Over)
                        board2svg w refb (isClicked w refb)
                        void $ return t # set UI.text "Game Over!!!"
                    _ -> if isWin newb then do
                            liftIO $ writeIORef refb (newb, Over)
                            board2svg w refb (isClicked w refb)
                            void $ return t # set UI.text "Congratulations, You Win!!!"
                         else do
                            liftIO $ writeIORef refb (newb, st)
                            board2svg w refb (isClicked w refb)


    -- Set window title
    void $ return w # set title "mine sweeper"

    
isClicked :: Window -> IORef (Board, GameState) -> (Int, Int) -> (Double, Double) -> UI ()
isClicked w refb xxyy _ = do
    (bb, st) <- liftIO $ readIORef refb
    case st of
        Start -> do
            s1 <- fromJust <$> getElementById w "s1"
            s2 <- fromJust <$> getElementById w "s2"
            s3 <- fromJust <$> getElementById w "s3"
            ss1 <- s1 # UI.get UI.value
            ss2 <- s2 # UI.get UI.value
            ss3 <- s3 # UI.get UI.value
            newb <- liftIO $ makeBoard (read ss1) (read ss2) (read ss3) [xxyy]
            let (newb', _) = clickBoardAt xxyy newb
            liftIO $ writeIORef refb (newb', Sweep)
            board2svg w refb (isClicked w refb)

        Mark -> do
            let (newb, _) = flagBoardAt xxyy bb
            liftIO $ writeIORef refb (newb, Mark)
            board2svg w refb (isClicked w refb)

        Sweep -> do
            let (newb, verdict) = sweepOrFlag xxyy bb
            case verdict of 
                GameOver -> getElementById w "text" >>= \case
                    Just t -> do
                        liftIO $ writeIORef refb (newb, Over)
                        board2svg w refb (isClicked w refb)
                        void $ element t # set UI.text "GameOver"
                    _ -> return ()
                
                _ -> do
                    when (isWin newb) $ do
                        getElementById w "text" >>= \case
                            Just t -> do
                                liftIO $ writeIORef refb (newb, Over)
                                board2svg w refb (isClicked w refb)
                                void $ element t # set UI.text "Win"
                            _ -> return ()

                    unless (isWin newb) $ do
                        liftIO $ writeIORef refb (newb, st)
                        board2svg w refb (isClicked w refb)

        _ -> return ()

board2svg :: Window -> IORef (Board, GameState) -> ((Int, Int) -> (Double, Double) -> UI ()) -> UI ()
board2svg w refb event = do
    (b, _st) <- liftIO $ readIORef refb
    let Board (dx, dy) _ = b
    getElementById w "container" >>= \case
        Just container -> do
            void $ element container # set html ""
            let svgHeight = show (dx * 30)
            let svgWidth = show (dy * 30)
            context <- SVG.svg # set SVG.height svgHeight # set SVG.width svgWidth
            void $ element container #+ [element context]
            mapM_ (mineElement context event) (idxboard b)
        Nothing -> return ()
    
mineElement context e ((x, y), b) = do
    r <- SVG.g
    on UI.mouseup r (e (x, y))
    
    -- Base rectangle for each cell
    rect <- SVG.rect
        # set SVG.y (show (x * 30))
        # set SVG.width (show 30)
        # set SVG.x (show (y * 30))
        # set SVG.height (show 30)
        # set SVG.stroke "grey"
        # set SVG.stroke_width "1"
        # set SVG.fill (case b of
                         Cell _ Hidden -> "#D3D3D3"
                         _             -> "#FFFFFF")
    _ <- return r #+ [element rect]

    case b of
        Cell _ Hidden -> return ()

        Cell Mine Revealed -> do
            -- Enhanced mine symbol
            circle <- SVG.circle
                # set SVG.cy (show (x * 30 + 30 `div` 2))
                # set SVG.cx (show (y * 30 + 30 `div` 2))
                # set SVG.r (show (30 `div` 4))
                # set SVG.fill "red"
            _ <- return r #+ [element circle]
            return ()

        Cell (Space 0) Revealed -> return ()

        Cell (Space n) Revealed -> do
            -- Number with bold, clear font
            text <- SVG.text
                # set SVG.y (show (x * 30 + 30 `div` 2 + 5))
                # set SVG.x (show (y * 30 + 30 `div` 2))
                # set text (show n)
                # set (attr "dominant-baseline") "middle" 
                # set (attr "text-anchor") "middle"
                # set SVG.fill (numberColor n)
                # set SVG.font_size "20px"
                # set SVG.font_weight "bold"
            _ <- return r #+ [element text]
            return ()

        Cell _ Flag -> do
            -- Flag pole
            pole <- SVG.rect
                # set SVG.x (show (y * 30 + 30 `div` 4))
                # set SVG.y (show (x * 30 + 30 `div` 4))
                # set SVG.width "2"
                # set SVG.height (show (30 `div` 2))
                # set SVG.fill "black"
            _ <- return r #+ [element pole]

            -- Flag cloth
            flagCloth <- SVG.path
                # set (attr "d") (printf "M %d %d l 12 6 -12 6 z" (y * 30 + 30 `div` 4 + 2) (x * 30 + 30 `div` 4))
                # set SVG.fill "red"
            _ <- return r #+ [element flagCloth]

            return ()

    return context #+ [element r]

-- Function to determine the color of numbers
numberColor :: Int -> String
numberColor n = case n of
    1 -> "blue"
    2 -> "green"
    3 -> "red"
    4 -> "purple"
    _ -> "black"

    

main :: IO ()
main = do
    gameGUI