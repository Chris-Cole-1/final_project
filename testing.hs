import Data.Char
import Data.List
import Data.Array


--type Post = String
type Name = String
type MoveSet = [String]

--data Piece = (String, [String]) -- (Name, list of moves)

-- Create a 2D array (matrix) of a specific size using nested lists
createMatrix :: Int -> Int -> a -> [[a]]
createMatrix rows cols defaultValue = replicate rows (replicate cols defaultValue)

-- Rank of Pieces that are identified by a certain int
k :: Int -- King
k = 0
q :: Int -- Queen
q = 1
r :: Int -- Rook
r = 2
b :: Int -- Bishop
b = 3
n :: Int -- Knight
n = 4
p :: Int -- Pawn
p = 5

-- Create a 3x4 matrix filled with zeros (default value)
board :: [[Int]]
board = createMatrix 8 8 0

-- Initialize board pieces
-- How to access: element = (matrix !! i) !! j
startPieces :: [[Int]] Int -> Int -> Int -> [[Int]]
startPieces board row col piece = (board !! row) !! col = piece
