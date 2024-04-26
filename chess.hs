type Tile = Maybe Piece
type Board = [[Tile]]
type Piece = (PieceType, Color)
    
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



-- isValidMove :: (Int, Int) -> (Int, Int) -> Bool
-- isValidMove (curRow,curCol) (nxtRow,nxtCol) = case getTile (curRow,curCol) of
--     Piece p -> False
--     Nothing -> True



updateElement :: Board -> (Int,Int) -> Tile -> Board
updateElement board (row,col) x =
    take row board ++
    [take col (board !! row) ++ [x] ++ drop (col + 1) (board !! row)] ++
    drop (row + 1) board