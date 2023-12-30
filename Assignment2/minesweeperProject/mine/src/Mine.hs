module Mine where
import Data.Maybe
import Debug.Trace

data MineElement   = Mine | Space Int deriving (Show, Eq, Ord)
data Display   = Hidden | Revealed | Flag | Question deriving (Show, Eq, Ord)
data BoardCell = Cell MineElement Display deriving (Show, Eq, Ord)
data Result = Continue | GameOver deriving (Show, Eq, Ord) 
data Board = Board Dimension [[BoardCell]]  deriving (Show, Eq, Ord)

type Dimension = (Int, Int)
type Point = (Int, Int)


getter :: Point -> Board -> Maybe BoardCell
getter (x, y) (Board _ a) = do
    row <- safeRow x a
    safeCell y row
  where
    safeRow :: Int -> [[BoardCell]] -> Maybe [BoardCell]
    safeRow n rows = if n >= 0 && n < length rows then Just (rows !! n) else Nothing

    safeCell :: Int -> [BoardCell] -> Maybe BoardCell
    safeCell n cells = if n >= 0 && n < length cells then Just (cells !! n) else Nothing
    
putter :: Point -> BoardCell -> Board -> Board    
putter (x, y) z (Board (xx, yy) a) =
    Board (xx, yy) c
    where 
         c = [ [ if (tx, ty) == (x, y) then z else a !! tx !! ty  | 
                 ty <- [0..yy - 1] ] | tx <- [0..xx - 1] ]

countNeighbors :: Point -> Board -> Int
countNeighbors (x, y) b = 
    length [ () | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0,
                  let z = getter (x + dx, y + dy) b,
                  isJust z,
                  case fromJust z of
                      Cell Mine _ -> True
                      _ -> False ]

sweepingMines :: Point -> Board -> (Board, Result)
sweepingMines point@(x, y) board@(Board dim cells) =
    case getter point board of
        Just (Cell Mine Hidden) -> (revealAllMines board, GameOver)
        Just (Cell (Space _) Hidden) ->
            let v = countNeighbors point board
                newBoard = putter point (Cell (Space v) Revealed) board
            in if v == 0 then expandEmptyCells newBoard point else (newBoard, Continue)
        _ -> (board, Continue)

expandEmptyCells :: Board -> Point -> (Board, Result)
expandEmptyCells board point = 
    let neighbors = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0, let x = fst point, let y = snd point]
        clickedBoard = foldr clickBoardAux (board, Continue) neighbors
    in clickedBoard
    where
        clickBoardAux p (b, _) = sweepingMines p b

revealAllMines :: Board -> Board
revealAllMines (Board dim cells) =
    Board dim (map (map revealMine) cells)
    where
        revealMine cell@(Cell Mine Hidden) = Cell Mine Revealed
        revealMine cell = cell
        
markingMines :: Point -> Board -> (Board, Result)
markingMines point board =
    case getter point board of
        Just (Cell a Hidden) -> (putter point (Cell a Flag) board, Continue)
        Just (Cell a Flag)   -> (putter point (Cell a Hidden) board, Continue)
        _                    -> (board, Continue)     
   
isWin :: Board -> Bool
isWin (Board _ b) = 
    all (all ok) b
    where
        ok (Cell Mine Revealed) = False
        ok (Cell Mine _) = True
        ok (Cell _ Revealed) = True
        ok _ = False
        
neighborProcessing :: Board -> (Point -> [Maybe BoardCell] -> [Point] -> Maybe [Point]) -> [Point]
neighborProcessing (Board (dx, dy) boardCells) processFunction = 
    concatMap processCell [(x, y) | x <- [0..dx-1], y <- [0..dy-1]]
    where
        processCell p@(x, y) =
            let nbCells = [ getter (nx, ny) (Board (dx, dy) boardCells) 
                            | nx <- [x-1..x+1], ny <- [y-1..y+1], (nx, ny) /= p ]
                nbPoints = [ (nx, ny) 
                             | nx <- [x-1..x+1], ny <- [y-1..y+1], (nx, ny) /= p ]
            in maybe [] id (processFunction p nbCells nbPoints)
        
saveMove :: Board -> [Point]
saveMove board = neighborProcessing board save
    where
        save (x, y) nbCells nbPoints =
            case getter (x, y) board of
                Just (Cell (Space n) Revealed) ->
                    let hiddenOrFlagCount = length $ filter isHiddenOrFlag nbCells
                    in if hiddenOrFlagCount == n 
                        then Just [p | (p, Just (Cell _ Hidden)) <- zip nbPoints nbCells]
                        else Nothing
                _ -> Nothing

        isHiddenOrFlag (Just (Cell _ Hidden)) = True
        isHiddenOrFlag (Just (Cell _ Flag)) = True
        isHiddenOrFlag _ = False

dangerousMove :: Board -> [Point]
dangerousMove board = neighborProcessing board dangerous
    where
        dangerous (x, y) nbCells nbPoints =
            case getter (x, y) board of
                Just (Cell (Space n) Revealed) ->
                    let flagCount = length $ filter isFlag nbCells
                    in if flagCount == n 
                        then Just [p | (p, Just (Cell _ Hidden)) <- zip nbPoints nbCells]
                        else Nothing
                _ -> Nothing

        isFlag (Just (Cell _ Flag)) = True
        isFlag _ = False


