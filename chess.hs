import Data.Char
import Data.List

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
-- Updating the board
-------------------------------------------------------------------------------------------------------------------

-- Assumes the move to be made is valid and updates the board with that move
makeMove :: Board -> (Int,Int) -> (Int,Int) -> Board
makeMove board cur dst =    let newBoard = updateBoard board dst (getPiece (getSquare board cur))
                            in updateBoard newBoard cur Nothing

-- Given a list, and index, and an element, update that list at that index with that element
updateRow :: [a] -> Int -> a -> [a]
updateRow [] _ _ = []
updateRow list index elem = take (index) list ++ [elem] ++ (drop (index+1) list)
--updateRow (x:xs) i elem = if i == 0 then elem : xs else x : updateRow xs (i - 1) elem

-- Given a 2D list, an index, and an element, update the index in that list with that element
updateBoard :: [[a]] -> (Int,Int) -> a -> [[a]]
updateBoard [[]] _ _ = [[]]
updateBoard list (r,c) elem = take r list ++ [updateRow (list !! r) c elem] ++ drop (r+1) list
--updateBoard (a:arr) (r,c) elem = if r == 0 then updateRow a c elem : arr else a : updateBoard arr (r - 1,c) elem


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

-- Checks if a given square is empty
isEmpty :: Square -> Bool
isEmpty Nothing = True
isEmpty _ = False

-- Returns the color of a the peice at the given square
getColor :: Square -> Color
getColor Nothing = error "No piece at this square!!!"
getColor (Just (p,c)) = c

-- Returns the Square at the specified row and col
getSquare :: Board -> (Int,Int) -> Square
getSquare board (row,col) = if isValidSquare (row,col) then (board !! row) !! col else error "Out of Board range!!"

-- Scans in a straight line in a certain direction to to see if we can make
-- a move from the current position to the destination
-- scanStraight :: Board -> Piece -> (Int,Int) -> (Int,Int) -> Bool
-- scanStraight board piece (curRow,curCol) dst =  let dir = getDir (curRow,curCol) dst
--                                                     cur = getSquare board (curRow,curCol)
--                                                 in case dir of 
--                                                     (0,0) -> not (friendly piece cur)
--                                                     (r,0) -> isEmpty cur && scanStraight board piece (curRow+r,curCol) dst 
--                                                     (0,c) -> isEmpty cur && scanStraight board piece (curRow,curCol+c) dst
--                                                     _     -> False 
scanStraight :: Board -> Piece -> (Int,Int) -> (Int,Int) -> Bool
scanStraight board piece (curRow,curCol) dst =  let dir = getDir (curRow,curCol) dst
                                                in  let nxt = getSquare board (curRow + fst dir,curCol + snd dir)
                                                    in case dir of 
                                                        (0,0) -> not (friendly piece nxt)
                                                        (r,0) -> scanStraight' board piece (curRow+r,curCol) dst dir
                                                        (0,c) -> scanStraight' board piece (curRow,curCol+c) dst dir
                                                        _     -> False 

-- Helper function for scanStraight. Performs recursion of scanning diagonally 
--      from a starting position (curRow, curCol) towards a destination (dstRow, dstCol).
-- Checks if the current position is equal to the destination position. If so, returns True.
-- Checks if the current position is within the bounds of the chessboard. If not, returns False.
-- Checks if the square at the current position contains a friendly piece. If so, returns False.
-- Recursively calls itself with the updated position (curRow + dirRow, curCol + dirCol) 
--      to continue scanning in the same direction until reaching the destination or encountering a blocking condition. 
scanStraight' :: Board -> Piece -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
scanStraight' board piece (curRow,curCol) (dstRow,dstCol) (dirRow,dirCol)
    | curRow == dstRow && curCol == dstCol = True  -- Reached destination
    | not (isValidSquare (curRow, curCol)) = False    -- Out of bounds
    | friendly piece (getSquare board (curRow, curCol)) = False  -- Friendly piece blocking
    | otherwise = scanStraight' board piece (curRow + dirRow, curCol + dirCol) (dstRow, dstCol) (dirRow, dirCol)

-- Scans in a diagonal line in a certain direction to see if we can make
-- a move from the current position to the destination 
scanDiag :: Board -> Piece -> (Int,Int) -> (Int,Int) -> Bool
scanDiag board piece (curRow,curCol) dst =  let dir = getDir (curRow,curCol) dst
                                            in case dir of 
                                                (0, 0) -> not (friendly piece (getSquare board (curRow, curCol)))
                                                (r, 0) -> False
                                                (0, c) -> False
                                                (r, c) -> scanDiag' board piece (curRow + r, curCol + c) dst dir

-- Helper function for scanDiag. Performs recursion of scanning diagonally 
--      from a starting position (curRow, curCol) towards a destination (dstRow, dstCol).
-- Checks if the current position is equal to the destination position. If so, returns True.
-- Checks if the current position is within the bounds of the chessboard. If not, returns False.
-- Checks if the square at the current position contains a friendly piece. If so, returns False.
-- Recursively calls itself with the updated position (curRow + dirRow, curCol + dirCol) 
--      to continue scanning in the same direction until reaching the destination or encountering a blocking condition. 
scanDiag' :: Board -> Piece -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
scanDiag' board piece (curRow,curCol) (dstRow,dstCol) (dirRow,dirCol)
    | curRow == dstRow && curCol == dstCol = True  -- Reached destination
    | not (isValidSquare (curRow, curCol)) = False    -- Out of bounds
    | friendly piece (getSquare board (curRow, curCol)) = False  -- Friendly piece blocking
    | otherwise = scanDiag' board piece (curRow + dirRow, curCol + dirCol) (dstRow, dstCol) (dirRow, dirCol)
-- Given two positions gets the direction of the move needed in a tuple
-- Row: 1 is down, -1 is up
-- Col: 1 is right, -1 is left
-- If (0,0) you are at the destination
getDir :: (Int,Int) -> (Int,Int) -> (Int,Int)
getDir (curRow,curCol) (nxtRow,nxtCol) = (
    if curRow > nxtRow then -1 else if curRow == nxtRow then 0 else 1,
    if curCol > nxtCol then -1 else if curCol == nxtCol then 0 else 1)

-- Returns True if the two given squares contain pieces of the same color
-- If either or both squares are empty (Nothing), it returns False
friendly :: Piece -> Square -> Bool
friendly _ (Nothing) = False
friendly p (Just (t,c)) = snd p == c
-- friendly b (Nothing) _ = False
-- friendly b _ (Nothing) = False
-- friendly b (Just (pType1,color1)) (Just (pType2,color2)) = color1 == color2

-- Given a square returns what is at that square (nothing or a piece)
getPiece :: Square -> Maybe Piece
getPiece Nothing = Nothing
getPiece (Just p) = Just p

-- Returns True if the two given squares contain pieces of t different colors
-- If either or both squares are empty (Nothing), it returns False
hostile :: Board -> Square -> Square -> Bool
hostile b (Nothing) _ = False
hostile b _ (Nothing) = False
hostile b (Just (pType1,color1)) (Just (pType2,color2)) = color1 /= color2

-- Checks if a specified move is valid on the board
isValidMove :: Board -> (Int, Int) -> (Int, Int) -> Bool
isValidMove board cur dst = case getSquare board cur of
    -- For the king, we make the desired move on a hypothetical board and check to see if that would cause a check
    Just (King,c) ->    let dsq = getSquare board dst
                            lst = moves cur kingTuples
                        in not $ friendly (King,c) dsq && not (isCheck (makeMove board cur dst) dst) && dst `elem` lst
    Just (Queen,c) ->   scanDiag board (Queen,c) cur dst || scanStraight board (Queen,c) cur dst
    Just (Knight,c) ->  let dsq = getSquare board dst
                            lst = moves cur knightTuples
                        in not $ friendly (Knight,c) dsq && dst `elem` lst
    Just (Bishop,c) -> scanDiag board (Bishop,c) cur dst
    Just (Rook,c) -> scanStraight board (Rook,c) cur dst
    Just (Pawn,White) ->    let dsq = getSquare board dst
                            in  if isEmpty dsq
                                then    if pawnAtStart board cur && (fst cur - 2,snd cur) == dst 
                                        then True
                                        else (fst cur - 1,snd cur) == dst
                                else pawnCanTake board cur dst
    Just (Pawn,Black) ->    let dsq = getSquare board dst
                            in  if isEmpty dsq
                                then    if pawnAtStart board cur && (fst cur + 2,snd cur) == dst 
                                        then True
                                        else (fst cur + 1,snd cur) == dst
                                else pawnCanTake board cur dst
    Nothing -> error "ERROR: No piece at first input square!"

-- Returns a list of possible moves given a position on the board and a list of valid movements
--      Basically filtering out moves that would go off the board
moves :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
moves (r,c) validMoves = foldr (\x acc -> let mv = (r + fst x,c + snd x) 
                                          in if isValidSquare mv then mv : acc else acc) [] validMoves

-- Returns true if a pawn can take an opposing piece
pawnCanTake :: Board -> (Int,Int) -> (Int,Int) -> Bool
pawnCanTake board cur dst = let pawn = getSquare board cur
                                opp = getSquare board dst
                            in case pawn of
                                Just (Pawn,White) -> (getColor opp == Black) && ((fst cur - 1, snd cur - 1) == dst || (fst cur - 1, snd cur + 1) == dst)
                                Just (Pawn,Black) -> (getColor opp == White) && ((fst cur + 1, snd cur - 1) == dst || (fst cur + 1, snd cur + 1) == dst)

-- Given a row and col, returns true if that pawn has not moved yet
pawnAtStart :: Board -> (Int,Int) -> Bool
pawnAtStart board (row,col) = case getSquare board (row,col) of 
                                Just (Pawn,c) -> if c == White then row == 6 else row == 1
                                _ -> False

-- Given a current index and a destination index,
--      returns true if ?????
canCaptureKing :: Board -> (Int,Int) -> (Int,Int) -> Bool
canCaptureKing board king opp = let k = getPiece (getSquare board king)
                                    o = getPiece (getSquare board opp)
                                in isValidMove board opp king

-- Determines if the king of the specified color is in check
isCheck :: Board -> (Int,Int) -> Bool
isCheck board king =    let opps = getPiecesBoard board (0,0) (getColor (getSquare board king))
                        in foldr (\x acc -> isValidMove board x king || acc) False opps

-- Determines if the king of the specified color is in checkmate
isCheckmate :: Color -> Board -> Bool
isCheckmate c b = False

-- Gets all peices from a row with the specified color and stores those indexes in a list
getPiecesRow :: [Square] -> Int -> Int -> Color -> [(Int,Int)]
getPiecesRow [] _ _ _ = []
getPiecesRow (l:ls) row col color = if getColor l == color then [(row,col)] else getPiecesRow ls row (col+1) color

-- Get all pieces from the board with the specified color and stores those indexes in a list
--      NOTE* -> Need to pass it like: getPiecesBoard (0,0) color so that it starts at the beginning of the board
getPiecesBoard :: [[Square]] -> (Int,Int) -> Color -> [(Int,Int)]
getPiecesBoard [] _ _ = []
getPiecesBoard (row:rest) (r,c) color = getPiecesRow row r c color ++ getPiecesBoard rest (r-1,0) color

-- All possible king moves
kingTuples :: [(Int,Int)]
kingTuples = [(i,j) | i <- [-1..1], j <- [-1..1], not (i == 0 && j == 0)]

-- All possible knight moves
knightTuples :: [(Int,Int)]
knightTuples = [(1,2), (2,1), (-1,-2), (-2,-1), (1,-2), (-1,2), (-2,1), (2,-1)]

-- All possible white pawn moves
whitePawnTuples :: [(Int,Int)]
whitePawnTuples = [(-1,0)]

whitePawnTake :: [(Int,Int)]
whitePawnTake = [(-1,-1),(-1,1)]

whitePawnStep2 :: [(Int,Int)]
whitePawnStep2 = [(-2,0)]

-- All possible black pawn moves
blackPawnTuples :: [(Int,Int)]
blackPawnTuples = [(1,0)]

blackPawnTake :: [(Int,Int)]
blackPawnTake = [(1,1),(1,-1)]

blackPawnStep2 :: [(Int,Int)]
blackPawnStep2 = [(2,0)]


-------------------------------------------------------------------------------------------------------------------
-- I/O
-------------------------------------------------------------------------------------------------------------------

-- Function to print a single row of the chessboard
printSquare :: Square -> String
printSquare square = [showPiece square]

-- Function to print a single row of the chessboard
printRow :: [Square] -> Int -> IO ()
printRow row num = putStrLn $ "|  " ++ intercalate "  |  " (map printSquare row) ++ "  |  " ++ show num
--printRow row num = putStrLn $ "|  " ++ intercalate "  |  " (map unicodePiece row) ++ "  |  " ++ show num

-- Function to print the entire chessboard
printBoard :: Board -> IO ()
printBoard board = do
    mapM_ (\(row, num) -> do putStrLn "------------------------------------------------"; printRow row num) (zip board [8,7..1])
    putStrLn "------------------------------------------------"
    putStrLn "  A      B     C     D     E     F     G     H "

nextPlayer::Color -> Color
nextPlayer White = Black
nextPlayer Black = White

main :: IO ()
main = do
    putStrLn "Welcome!\nPlease choose from one of the following options.\n ~ Start Game\n ~ Quit\n"
    s <- getLine
    case s of
        "Quit" -> return ()
        "Start Game" -> repl board White
    --repl board

repl :: Board -> Color -> IO ()
repl board col = do 
    printBoard board
    putStrLn $ "Player " ++ show col ++ " Please enter your move\n(Enter Quit to exit game)"
    s <- getLine
    case s of
        "Quit" -> return ()
        _ -> let move = posToSquare s in
                        case getSquare board (fst (fst move), snd (fst move)) of
                            Just (pieceType, pieceColor) -> if col == pieceColor
                                then
                                     if isValidMove board (fst move) (snd move)
                                        then do
                                            let update = makeMove board (fst move) (snd move)
                                            repl update (nextPlayer col)
                                        else do
                                            putStrLn "Invalid Move"
                                            repl board col
                                    --if the color matches the turn then allow the move which should be the code down below
                                else do
                                    putStrLn "Invalid Move, Try Again"
                                    repl board col
        --"Start Game" -> 
            --parse input
            --check move
            --make move
            --print board

            --implement turns



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
                in ((numToNum (last (head lst)), letterToNum (head (head lst))),
                    (numToNum (last (last lst)), letterToNum (head (last lst))))

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

-- Converts chess board number labels to indexes to access the board
numToNum :: Char -> Int
numToNum n | n == '8' = 0
numToNum n | n == '7' = 1
numToNum n | n == '6' = 2
numToNum n | n == '5' = 3
numToNum n | n == '4' = 4
numToNum n | n == '3' = 5
numToNum n | n == '2' = 6
numToNum n | n == '1' = 7
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

unicodePiece :: Maybe Piece -> String
unicodePiece (Just (King, Black))   = "♔"
unicodePiece (Just (Queen, Black))  = "♕"
unicodePiece (Just (Rook, Black))   = "♖"
unicodePiece (Just (Bishop, Black)) = "♗"
unicodePiece (Just (Knight, Black)) = "♘"
unicodePiece (Just (Pawn, Black))   = "♙"
unicodePiece (Just (King, White))   = "♚"
unicodePiece (Just (Queen, White))  = "♛"
unicodePiece (Just (Rook, White) )  = "♜"
unicodePiece (Just (Bishop, White)) = "♝"
unicodePiece (Just (Knight, White)) = "♞"
unicodePiece (Just (Pawn, White))   = "♟"
unicodePiece _ = " "
