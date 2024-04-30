import Data.Char

type Square = Maybe Piece             -- represents a square on the baord
type Board = [[Square]]               -- represents a 2D list of Squares
type Piece = (PieceType, Color)     -- represents a chess piece that has a type and color
--type Position = (Char, Int)         -- represents algebraic notation of board positions
    
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Show, Eq)
data Color = White | Black
    deriving (Show, Eq)


-- empty::Square
-- empty = Nothing

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

----------------------------------------------------------
-- Computing valid moves
----------------------------------------------------------

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
getSquare board (row,col) = (board !! row) !! col

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

-- isValidMove :: (Int, Int) -> (Int, Int) -> Bool
-- isValidMove (curRow,curCol) (nxtRow,nxtCol) = case getSquare (curRow,curCol) of
--     Piece p -> False
--     Nothing -> True

-- Given a list, and index, and an element, update that list at that index with that element
updateElem :: [a] -> Int -> a -> [a]
updateElem [] _ _ = []
updateElem (x:xs) i elem = if i == 0 then elem : xs else x : updateElem xs (i - 1) elem

-- Given a 2D list, an index, and an element, update the index in that list with that element
updateBoard :: [[a]] -> (Int,Int) -> a -> [[a]]
updateBoard [[]] _ _ = [[]]
updateBoard (a:arr) (r,c) elem = if r == 0 then updateElem a c elem : arr else a : updateBoard arr (r - 1,c) elem

-- updateRow :: [[a]] -> Int -> [a] -> [[a]]
-- updateRow (a:arr) i row = if i == 0 then row : arr else a : updateRow arr (i - 1) row

-- updateSquare :: Board -> (Int,Int) -> Square -> Board
-- updateSquare b (row,col) s = b

kingMoves :: Board -> Square -> [Square]
kingMoves b s = []

queenMoves :: Board -> Square -> [Square]
queenMoves b s = []

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

-- updateElement :: Board -> (Int,Int) -> Square -> Board
-- updateElement board (row,col) x =
--     take row board ++
--     [take col (board !! row) ++ [x] ++ drop (col + 1) (board !! row)] ++
--     drop (row + 1) board


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