import Data.Char

-------------------------------------------------------------------------------------------------------------------
-- Type and Data Definitions
-------------------------------------------------------------------------------------------------------------------

type Square = Maybe Piece             -- represents a square on the baord
type Board = [[Square]]               -- represents a 2D list of Squares
type Piece = (PieceType, Color)     -- represents a chess piece that has a type and color
--type Position = (Char, Int)         -- represents algebraic notation of board positions
    
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Show, Eq)
data Color = White | Black
    deriving (Show, Eq)

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

-- -- Takes the peice at the specified row col if it is occupied
-- capturePiece :: Board -> (Int,Int) -> Board
-- capturePiece board rc = updateElement board rc Nothing 

-- addPiece :: Board -> Piece -> (Int, Int) -> Board
-- addPiece board piece (row,col) =
--     if isValidSquare (row,col)
--         then updateElement board (row,col) (Just piece)
--         else board

-------------------------------------------------------------------------------------------------------------------
-- Making moves
-------------------------------------------------------------------------------------------------------------------
makeMove :: Board -> (Int,Int) -> (Int,Int) -> Board
makeMove board s c = board


-------------------------------------------------------------------------------------------------------------------
-- Updating the board
-------------------------------------------------------------------------------------------------------------------

-- Given a list, and index, and an element, update that list at that index with that element
updateRow :: [a] -> Int -> a -> [a]
updateRow [] _ _ = []
updateRow (x:xs) i elem = if i == 0 then elem : xs else x : updateRow xs (i - 1) elem

-- Given a 2D list, an index, and an element, update the index in that list with that element
updateBoard :: [[a]] -> (Int,Int) -> a -> [[a]]
updateBoard [[]] _ _ = [[]]
updateBoard (a:arr) (r,c) elem = if r == 0 then updateRow a c elem : arr else a : updateBoard arr (r - 1,c) elem

-- updateRow :: [[a]] -> Int -> [a] -> [[a]]
-- updateRow (a:arr) i row = if i == 0 then row : arr else a : updateRow arr (i - 1) row

-- updateSquare :: Board -> (Int,Int) -> Square -> Board
-- updateSquare b (row,col) s = b

-------------------------------------------------------------------------------------------------------------------
-- Computing valid moves
-------------------------------------------------------------------------------------------------------------------

-- Returns true if specifed row and col is within range of the board
isValidSquare :: (Int, Int) -> Bool
isValidSquare (row,col) = row >= 0 && row < 8 && col >= 0 && col < 8

-- Returns true if the specifed Square (row,col) on the board is occupied by another piece, false otherwise
isOccupied :: Board -> (Int,Int) -> Bool
isOccupied board (row,col) = case getSquare board (row,col) of
                            Just _ -> True
                            Nothing -> False

-- Returns the Square at the specified row and col
getSquare :: Board -> (Int,Int) -> Square
getSquare board (row,col) = if isValidSquare (row,col) then (board !! row) !! col else error "Out of Board range!!"

-- Returns True if the two given squares contain pieces of the same color
-- If either or both squares are empty (Nothing), it returns False
friendly :: Board -> Square -> Square -> Bool
friendly b (Nothing) _ = False
friendly b _ (Nothing) = False
friendly b (Just (pType1,color1)) (Just (pType2,color2)) = color1 == color2

-- Returns True if the two given squares contain pieces of t different colors
-- If either or both squares are empty (Nothing), it returns False
hostile :: Board -> Square -> Square -> Bool
hostile b (Nothing) _ = False
hostile b _ (Nothing) = False
hostile b (Just (pType1,color1)) (Just (pType2,color2)) = color1 /= color2

isValidMove :: Board -> (Int, Int) -> (Int, Int) -> Bool
isValidMove board (curRow,curCol) (nxtRow,nxtCol) = case getSquare board (curRow,curCol) of
    Just (King,c) -> False
    Just (Queen,c) -> False
    Just (Knight,c) -> False
    Just (Bishop,c) -> False
    Just (Rook,c) -> False
    Just (Pawn,c) -> False
    Nothing -> error "ERROR: No piece at first input square!"

-- scan :: Board -> (Int, Int) -> [Sqaure]
-- scan board (row,col) = 

kingMoves :: Board -> Square -> [Square]
kingMoves b s = []

queenMoves :: Board -> (Int,Int) -> [Square]
queenMoves b (row,col) = []

knightMoves :: Board -> Square -> [Square]
knightMoves b s = []

bishopMoves :: Board -> Square -> [Square]
bishopMoves b s = []

rookMoves :: Board -> Square -> [Square]
rookMoves b s = []

pawnMoves :: Board -> Square -> [Square]
pawnMoves b s = []

-- Determines if the king of the specified color is in check
isCheck :: Color -> Board -> Bool
isCheck c b = False

-- Determines if the king of the specified color is in checkmate
isCheckmate :: Color -> Board -> Bool
isCheckmate c b = False


-------------------------------------------------------------------------------------------------------------------
-- Getting squares
-------------------------------------------------------------------------------------------------------------------

-- Get all sqaures north of the current (row,col) 
getN :: Board -> (Int,Int) -> [Square]
getN board (row,col) | isValidSquare (row-1,col) = getSquare board (row-1,col) : getN board (row-1,col)
getN board (row,col) = []

-- Get all sqaures south of the current (row,col) 
getS :: Board -> (Int,Int) -> [Square]
getS board (row,col) | isValidSquare (row+1,col) = getSquare board (row+1,col) : getS board (row+1,col)
getS board (row,col) = []

-- Get all sqaures east of the current (row,col) 
getE :: Board -> (Int,Int) -> [Square]
getE board (row,col) | isValidSquare (row,col+1) = getSquare board (row,col+1) : getE board (row,col+1)
getE board (row,col) = []

-- Get all sqaures west of the current (row,col) 
getW :: Board -> (Int,Int) -> [Square]
getW board (row,col) | isValidSquare (row,col-1) = getSquare board (row,col-1) : getW board (row,col-1)
getW board (row,col) = []

-- Get all sqaures northeast of the current (row,col) 
getNE :: Board -> (Int,Int) -> [Square]
getNE board (row,col) | isValidSquare (row-1,col+1) = getSquare board (row-1,col+1) : getNE board (row-1,col+1)
getNE board (row,col) = []

-- Get all sqaures northwestof the current (row,col) 
getNW :: Board -> (Int,Int) -> [Square]
getNW board (row,col) | isValidSquare (row-1,col-1) = getSquare board (row-1,col-1) : getNW board (row-1,col-1)
getNW board (row,col) = []

-- Get all sqaures northeast of the current (row,col) 
getSE :: Board -> (Int,Int) -> [Square]
getSE board (row,col) | isValidSquare (row+1,col+1) = getSquare board (row+1,col+1) : getSE board (row+1,col+1)
getSE board (row,col) = []

-- Get all sqaures northeast of the current (row,col) 
getSW :: Board -> (Int,Int) -> [Square]
getSW board (row,col) | isValidSquare (row-1,col-1) = getSquare board (row-1,col-1) : getSW board (row-1,col-1)
getSW board (row,col) = []


-------------------------------------------------------------------------------------------------------------------
-- I/O
-------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Welcome!\nPlease choose from one of the following options.\n ~ Start Game\n ~ Quit\n"
    repl board

repl :: Board -> IO ()
repl board = do 
    putStrLn "Please enter your move\n"
    s <- getLine
    case s of
        "Quit" -> return ()
        --"Start Game" -> 

-- Splits a string on a specified character
splitOn :: Char -> String -> [String]
splitOn c [] = []
splitOn c (x:xs) =  if x == c 
                    then splitOn c xs 
                    else let (mv,rest) = span isAlphaNum (x:xs)
                         in mv : splitOn c rest

-- Converts string input from user into rows and cols on the board
posToSquare :: String -> ((Int,Int),(Int,Int))
posToSquare s = let lst = splitOn ' ' s
                in ((read [(last (head lst))], letterToNum (head (head lst))),
                    (read [(last (last lst))], letterToNum (head (last lst))))

-- Converts letters to numbers to access the board
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

-- Converts chess board number labes to indexes to access the board
numToNum :: Int -> Int
numToNum n | n == 8 = 0
numToNum n | n == 7 = 1
numToNum n | n == 6 = 2
numToNum n | n == 5 = 3
numToNum n | n == 4 = 4
numToNum n | n == 3 = 5
numToNum n | n == 2 = 6
numToNum n | n == 1 = 7
numToNum n = -1

showPiece :: Maybe Piece -> Char
showPiece (Just (King, White))   = 'K'
showPiece (Just (Queen, White))  = 'Q'
showPiece (Just (Rook, White))   = 'R'
showPiece (Just (Bishop, White)) = 'B'
showPiece (Just (Knight, White)) = 'N'
showPiece (Just (Pawn, White))   = 'P'
showPiece (Just (King, Black))   = 'k'
showPiece (Just (Queen, Black))  = 'q'
showPiece (Just (Rook, Black))   = 'r'
showPiece (Just (Bishop, Black)) = 'b'
showPiece (Just (Knight, Black)) = 'n'
showPiece (Just (pawn, Black))   = 'p'
showPiece _ = ' '