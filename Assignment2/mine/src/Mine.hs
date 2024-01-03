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

-- Safely retrieves a BoardCell from the Board at a given Point.
getter :: Point -> Board -> Maybe BoardCell
getter (x, y) (Board _ a) = do
    row <- safeRow x a
    safeCell y row
  where
    safeRow :: Int -> [[BoardCell]] -> Maybe [BoardCell]
    safeRow n rows = if n >= 0 && n < length rows then Just (rows !! n) else Nothing

    safeCell :: Int -> [BoardCell] -> Maybe BoardCell
    safeCell n cells = if n >= 0 && n < length cells then Just (cells !! n) else Nothing

-- Places a BoardCell at a specified Point on the Board.     
putter :: Point -> BoardCell -> Board -> Board    
putter (x, y) z (Board (xx, yy) a) =
    Board (xx, yy) c
    where 
         c = [ [ if (tx, ty) == (x, y) then z else a !! tx !! ty  | 
                 ty <- [0..yy - 1] ] | tx <- [0..xx - 1] ]

-- Counts the number of mines adjacent to a given point on the board.
countNeighbors :: Point -> Board -> Int
countNeighbors (x, y) b = 
    length [ () | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0,
                  let z = getter (x + dx, y + dy) b,
                  isJust z,
                  case fromJust z of
                      Cell Mine _ -> True
                      _ -> False ]

-- If the cell contains a hidden mine, it triggers game over. 
-- If it's an empty space, it reveals the cell and expands to neighboring cells if no mines are adjacent.
sweepingMines :: Point -> Board -> (Board, Result)
sweepingMines point@(x, y) board@(Board dim cells) =
    case getter point board of
        Just (Cell Mine Hidden) -> (revealAllMines board, GameOver)
        Just (Cell (Space _) Hidden) ->
            let v = countNeighbors point board
                newBoard = putter point (Cell (Space v) Revealed) board
            in if v == 0 then expandEmptyCells newBoard point else (newBoard, Continue)
        _ -> (board, Continue)

-- Recursively reveals adjacent cells around a given point until it reaches cells adjacent to mines.
expandEmptyCells :: Board -> Point -> (Board, Result)
expandEmptyCells board point = 
    let neighbors = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0, let x = fst point, let y = snd point]
        clickedBoard = foldr clickBoardAux (board, Continue) neighbors
    in clickedBoard
    where
        clickBoardAux p (b, _) = sweepingMines p b

-- This function is typically called when the game is over to show the player where all the mines were.
revealAllMines :: Board -> Board
revealAllMines (Board dim cells) =
    Board dim (map (map revealMine) cells)
    where
        revealMine cell@(Cell Mine Hidden) = Cell Mine Revealed
        revealMine cell = cell

-- Toggles a flag on a cell. It marks a cell as flagged if it's hidden and unmarks it if it's already flagged.        
markingMines :: Point -> Board -> (Board, Result)
markingMines point board =
    case getter point board of
        Just (Cell a Hidden) -> (putter point (Cell a Flag) board, Continue)
        Just (Cell a Flag)   -> (putter point (Cell a Hidden) board, Continue)
        _                    -> (board, Continue)     

-- Checks if the game is won. A game is won when all mines are correctly flagged, and all non-mine cells are revealed.   
isWin :: Board -> Bool
isWin (Board _ b) = 
    all (all ok) b
    where
        ok (Cell Mine Revealed) = False
        ok (Cell Mine _) = True
        ok (Cell _ Revealed) = True
        ok _ = False

-- Recursively calculate the number of mines around each cell X
-- ? ? ?
-- ? X ?
-- ? ? ? 
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

-- Find cells that can be safely revealed based on the information available from their neighbors.        
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

-- Used for automating moves when there are no obvious safe moves left.
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


