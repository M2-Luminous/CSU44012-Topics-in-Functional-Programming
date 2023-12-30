module Mine where
import Data.Maybe
import Debug.Trace

data MineElement   = Mine | Space Int deriving (Show, Eq, Ord)
data Display   = Hidden | Revealed | Flag | Question deriving (Show, Eq, Ord)
data BoardCell = Cell MineElement Display deriving (Show, Eq, Ord)
data Result = Continue | GameOver deriving (Show, Eq, Ord) 

type Dimension = (Int, Int)
type Point = (Int, Int)
data Board = Board Dimension [[BoardCell]]  deriving (Show, Eq, Ord)

get :: Point -> Board -> Maybe BoardCell
get (x, y) (Board _ a) = do
    row <- safeRow x a
    safeCell y row
  where
    safeRow :: Int -> [[BoardCell]] -> Maybe [BoardCell]
    safeRow n rows = if n >= 0 && n < length rows then Just (rows !! n) else Nothing

    safeCell :: Int -> [BoardCell] -> Maybe BoardCell
    safeCell n cells = if n >= 0 && n < length cells then Just (cells !! n) else Nothing
    
put :: Point -> BoardCell -> Board -> Board    
put (x, y) z (Board (xx, yy) a) =
    Board (xx, yy) c
    where 
         c = [ [ if (tx, ty) == (x, y) then z else a !! tx !! ty  | 
                 ty <- [0..yy - 1] ] | tx <- [0..xx - 1] ]

countNeighbour :: Point -> Board -> Int
countNeighbour (x, y) b = 
    length [ () | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0,
                  let z = get (x + dx, y + dy) b,
                  isJust z,
                  case fromJust z of
                      Cell Mine _ -> True
                      _ -> False ]

clickBoardAt :: Point -> Board -> (Board, Result)
clickBoardAt (x, y) b = 
    case get (x, y) b of
        Just (Cell Mine Hidden) -> (revealAllMines b, GameOver)
        Just (Cell (Space _) Hidden) ->
            let v = countNeighbour (x, y) b
                r = put (x, y) (Cell (Space v) Revealed) b
            in                
            if v == 0 then
                let clicks =  [ (x + dx, y + dy) | 
                                        dx <- [-1..1], dy <- [-1..1], 
                                        dx /= 0 || dy /= 0 ]
                    click xy b = fst (clickBoardAt xy b)                                    
                in
                    (foldr click r clicks, Continue)    
            else
                (r, Continue)
        Just _ -> (b, Continue)
        _ -> (b, GameOver)

revealAllMines :: Board -> Board
revealAllMines (Board dim cells) =
    Board dim (map (map revealMine) cells)
    where
        revealMine cell@(Cell Mine Hidden) = Cell Mine Revealed
        revealMine cell = cell
        
flagBoardAt (x, y) b = 
    case get (x, y) b of
        Just (Cell a Hidden) -> (put (x, y) (Cell a Flag) b, Continue)
        Just (Cell a Flag) -> (put (x, y) (Cell a Hidden) b, Continue)
        Just _ -> (b, Continue)
        _ -> (b, GameOver)       
   
        
isWin :: Board -> Bool
isWin (Board _ b) = 
    all (all ok) b
    where
        ok (Cell Mine Revealed) = False
        ok (Cell Mine _) = True
        ok (Cell _ Revealed) = True
        ok _ = False
        
processCells :: Board -> (Point -> [Maybe BoardCell] -> [Point] -> Maybe [Point]) -> [Point]
processCells (Board (dx, dy) boardCells) processFunction = 
    concatMap processCell [(x, y) | x <- [0..dx-1], y <- [0..dy-1]]
    where
        processCell p@(x, y) =
            let nbCells = [ get (nx, ny) (Board (dx, dy) boardCells) 
                            | nx <- [x-1..x+1], ny <- [y-1..y+1], (nx, ny) /= p ]
                nbPoints = [ (nx, ny) 
                             | nx <- [x-1..x+1], ny <- [y-1..y+1], (nx, ny) /= p ]
            in maybe [] id (processFunction p nbCells nbPoints)
        
obviousMines :: Board -> [Point]
obviousMines board = processCells board obviousMine
    where
        obviousMine (x, y) nbCells nbPoints =
            case get (x, y) board of
                Just (Cell (Space n) Revealed) ->
                    let hiddenOrFlagCount = length $ filter isHiddenOrFlag nbCells
                    in if hiddenOrFlagCount == n 
                        then Just [p | (p, Just (Cell _ Hidden)) <- zip nbPoints nbCells]
                        else Nothing
                _ -> Nothing

        isHiddenOrFlag (Just (Cell _ Hidden)) = True
        isHiddenOrFlag (Just (Cell _ Flag)) = True
        isHiddenOrFlag _ = False

obviousNonMines :: Board -> [Point]
obviousNonMines board = processCells board obviousNonMine
    where
        obviousNonMine (x, y) nbCells nbPoints =
            case get (x, y) board of
                Just (Cell (Space n) Revealed) ->
                    let flagCount = length $ filter isFlag nbCells
                    in if flagCount == n 
                        then Just [p | (p, Just (Cell _ Hidden)) <- zip nbPoints nbCells]
                        else Nothing
                _ -> Nothing

        isFlag (Just (Cell _ Flag)) = True
        isFlag _ = False


