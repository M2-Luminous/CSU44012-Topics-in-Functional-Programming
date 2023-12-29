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

idxboard :: Board -> [(Point, BoardCell)]
idxboard (Board (dx, dy) b) = 
    [ ((x, y), b !! x !! y)   | x <- [0..dx-1], y <- [0..dy-1] ]

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
                 
 
isMine :: BoardCell -> Bool                 
isMine (Cell Mine _) = True
isMine _ = False

isHidden :: BoardCell -> Bool                 
isHidden (Cell _ Hidden) = True
isHidden _ = False

isFlag :: BoardCell -> Bool                 
isFlag (Cell _ Flag) = True
isFlag _ = False
   
countNeighbour :: Point -> Board -> Int
countNeighbour (x, y) b = 
    length [ () | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0,
                  let z = get (x + dx, y + dy) b,
                  isJust z,
                  isMine (fromJust z) ]
                  
emptyBoard :: Dimension -> Board
emptyBoard (dx, dy) = 
   Board (dx, dy) [  [ Cell (Space 0) Hidden | _ <- [0..dy - 1]] | _ <- [0..dx - 1] ]
   
clickBoardAt :: Point -> Board -> (Board, Result)
clickBoardAt (x, y) b = 
    case get (x, y) b of
        Just (Cell Mine Hidden) -> (put (x, y) (Cell Mine Revealed) b, GameOver)
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
        
getNeighbors :: Point -> Board -> [(Point, BoardCell)]
getNeighbors (x, y) b = 
    [ ((x + dx, y + dy), fromJust z) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0,
                  let z = get (x + dx, y + dy) b,
                  isJust z ]

        
obviousMines :: Board -> [Point]
obviousMines b = 
    [ r | (idx, _) <- idxboard b, r <- obviousMine idx b ]
    where
        obviousMine (x, y) b = 
            let nb = getNeighbors (x, y) b 
                content = get (x, y) b 
            in
            case content of
                Just (Cell (Space n) Revealed) ->
                    if length [ () | (_, x) <- nb, isHidden x || isFlag x ] == n then
                        [ (x, y) | ((x, y), z) <- nb, isHidden z ] 
                    else
                        []
                _ -> []
    

obviousNonMines ::  Board -> [Point]
obviousNonMines b = 
    [ r | (idx, _) <- idxboard b, r <- obviousNonMine idx b ]
    where
        obviousNonMine (x, y) b = 
            let nb = getNeighbors (x, y) b 
                content = get (x, y) b 
            in
            case content of
                Just (Cell (Space n) Revealed) ->
                    if length [ () | (_, x) <- nb, isFlag x ] == n then
                        [ (x, y) | ((x, y), z) <- nb, isHidden z ] 
                    else
                        []
                _ -> []