import Data.Char
import Data.List

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
    mapM ((row, num) -> do putStrLn "------------------------------------------------"; printRow row num) (zip board [8,7..1])
    putStrLn "------------------------------------------------"
    putStrLn "  A      B     C     D     E     F     G     H "


main :: IO ()
main = do
    putStrLn "Welcome!\nPlease choose from one of the following options.\n ~ Start Game\n ~ Quit\n"
    s <- getLine
    case s of
        "Quit" -> return ()
        "Start Game" -> repl board
    --repl board

repl :: Board -> IO ()
repl board = do 
    printBoard board
    putStrLn "Please enter your move\n(Enter Quit to exit game)"
    s <- getLine
    case s of
        "Quit" -> return ()
         -> let move = posToSquare s
           in if isValidMove board (fst move) (snd move)
            then do
                let update = makeMove board (fst move) (snd move)
                repl update
                else do
                    putStrLn "Invalid Move"
                    repl board
        --"Start Game" -> 
            --parse input
            --check move
            --make move
            --print board
