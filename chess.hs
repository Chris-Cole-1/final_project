import Data.Char
type Tile = Maybe Piece             -- represents a square on the baord
type Board = [[Tile]]               -- represents a 2D list of Tiles
type Piece = (PieceType, Color)     -- represents a chess piece that has a type and color
--type Position = (Char, Int)         -- represents algebraic notation of board positions
    
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Show, Eq)
data Color = White | Black


empty::Tile
empty = Nothing

-- Chess board initialized with peices in starting positions
board :: Board
board = 
    [
    [Just(Rook,Black), Just(Knight,Black), Just(Bishop,Black), Just(King,Black),  Just(Queen,Black), Just(Bishop,Black), Just(Knight,Black), Just(Rook,Black)],
    [Just(Pawn,Black), Just(Pawn,Black),   Just(Pawn,Black),   Just(Pawn,Black),  Just(Pawn,Black),  Just(Pawn,Black),   Just(Pawn,Black),   Just(Pawn,Black)],
    [Nothing,          Nothing,            Nothing,            Nothing,           Nothing,           Nothing,            Nothing,            Nothing         ],
    [Nothing,          Nothing,            Nothing,            Nothing,           Nothing,           Nothing,            Nothing,            Nothing         ],
    [Nothing,          Nothing,            Nothing,            Nothing,           Nothing,           Nothing,            Nothing,            Nothing         ],
    [Nothing,          Nothing,            Nothing,            Nothing,           Nothing,           Nothing,            Nothing,            Nothing         ],
    [Just(Pawn,White), Just(Pawn,White),   Just(Pawn,White),   Just(Pawn,White),  Just(Pawn,White),  Just(Pawn,White),   Just(Pawn,White),   Just(Pawn,White)],
    [Just(Rook,White), Just(Knight,White), Just(Bishop,White), Just(Queen,White), Just(King,White),  Just(Bishop,White), Just(Knight,White), Just(Rook,White)]
    ]

-- Returns the tile at the specified row and col
getTile :: Board -> (Int,Int) -> Tile
getTile board (row,col) = (board !! row) !! col

-- Returns true if specifed row and col is within range of the board
isValidTile :: (Int, Int) -> Bool
isValidTile (row,col) = row >= 0 && row < 8 && col >= 0 && col < 8

-- Returns true if the specifed tile (row,col) on the board is occupied by another piece, false otherwise
isOccupied :: Board -> (Int,Int) -> Bool
isOccupied board (row,col) = case getTile board (row,col) of
                            Just _ -> True
                            Nothing -> False

-- Takes the peice at the specified row col if it is occupied
capturePiece :: Board -> (Int,Int) -> Board
capturePiece board rc = updateElement board rc Nothing 

addPiece :: Board -> Piece -> (Int, Int) -> Board
addPiece board piece (row,col) =
    if isValidTile (row,col)
        then updateElement board (row,col) (Just piece)
        else board

splitOn :: Char -> String -> [String]
splitOn c [] = []
splitOn c (x:xs) =  if x == c 
                    then splitOn c xs 
                    else let (mv,rest) = span isAlphaNum (x:xs)
                         in mv : splitOn c rest

posToTile :: String -> ((Int,Int),(Int,Int))
posToTile s = let lst = splitOn ' ' s
                in ((read [(last (head lst))], letterToNum (head (head lst))),
                    (read [(last (last lst))], letterToNum (head (last lst))))

letterToNum :: Char -> Int
letterToNum c | c == 'A' = 0
letterToNum c | c == 'B' = 1
letterToNum c | c == 'C' = 2
letterToNum c | c == 'D' = 3
letterToNum c | c == 'E' = 4
letterToNum c | c == 'F' = 5
letterToNum c | c == 'G' = 6
letterToNum c | c == 'H' = 7
letterToNum c = -1

-- isValidMove :: (Int, Int) -> (Int, Int) -> Bool
-- isValidMove (curRow,curCol) (nxtRow,nxtCol) = case getTile (curRow,curCol) of
--     Piece p -> False
--     Nothing -> True



updateElement :: Board -> (Int,Int) -> Tile -> Board
updateElement board (row,col) x =
    take row board ++
    [take col (board !! row) ++ [x] ++ drop (col + 1) (board !! row)] ++
    drop (row + 1) board


main :: IO ()
main = do
    putStrLn "Welcome!\nPlease choose from one of the following options.\n ~ Start Game\n ~ Quit\n"
    repl board

repl :: Board -> IO ()
repl board = do 
    putStrLn "Please enter your move\n"
    s <- getLine
    case s of
        "quit" -> return ()