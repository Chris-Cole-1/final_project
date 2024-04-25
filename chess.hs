type Tile = Maybe Piece
type Board = [[Tile]]
type Piece = (PieceType, Color)
    
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Show, Eq)

data Color = White | Black

empty::Tile
empty = Nothing

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


-- setBoard :: Board -> Board
-- setBoard board = 

-- Returns the tile at the specified row and col
getTile :: Board -> (Int,Int) -> Tile
getTile board (row,col) = (board !! row) !! col

addPiece :: Board -> Piece -> (Int, Int) -> Board
addPiece board piece (row,col) =
    if isValidSquare (row,col)
        then updateElement board (row,col) (Just piece)
        else board

isValidSquare :: (Int, Int) -> Bool
isValidSquare (row,col) = row >= 0 && row < 8 && col >= 0 && col < 8

isValidMove :: Piece -> (Int, Int) -> (Int, Int) -> Bool
isValidMove (Pawn,c) (curRow,curCol) (nxtRow,nxtCol) = curRow == nxtRow + 1 -- plus or minus one??? board orientation??
isValidMove (Knight,c) (curRow,curCol) (nxtRow,nxtCol) = curRow == nxtRow + 1
isValidMove (Bishop,c) (curRow,curCol) (nxtRow,nxtCol) = curRow == nxtRow + 1
isValidMove (Rook,c) (curRow,curCol) (nxtRow,nxtCol) = curRow == nxtRow + 1
isValidMove (King,c) (curRow,curCol) (nxtRow,nxtCol) | curRow + 1 == nxtRow = True
isValidMove (Queen,c) (curRow,curCol) (nxtRow,nxtCol) = curRow == nxtRow + 1

updateElement :: Board -> (Int,Int) -> Tile -> Board
updateElement board (row,col) x =
    take row board ++
    [take col (board !! row) ++ [x] ++ drop (col + 1) (board !! row)] ++
    drop (row + 1) board