module TicTacToe.Core where

import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Char (digitToInt, isDigit)

import TicTacToe.Types
import TicTacToe.AI (AI, trivialAi, aiMove)

emptyBoard :: Board
emptyBoard = Board (replicate 9 E)

setCell :: Cell -> Pos -> Board -> Board
setCell c i (Board cs) = 
    let (l,(_:rs)) = splitAt (i-1) cs
     in Board $ l ++ c:rs

getCell :: Pos -> Board -> Cell
getCell i (Board cs) = cs !! (i-1)

getInput :: IO Input
getInput = do
    char <- getChar
    return $ case extractPos char of
        Just validPos -> IPos validPos
        Nothing       -> 
            if char == 'q' then
                ICmd Exit
            else
                IInvalid

extractPos :: Char -> Maybe Int
extractPos c | isDigit c = 
  case digitToInt c of
    0 -> Nothing
    x -> Just x
extractPos _               = Nothing

isValidMove :: Move -> Board -> Bool
isValidMove (Move _ pos) board = getCell pos board == E

runGame :: IO ()
runGame = gameStep (GameState emptyBoard X)

gameResult :: Board -> GameResult
gameResult (Board cs) = 
    let chunks = chunksOf 3 cs
        hasEmptyCell = any (==E) cs
        diagonals = [map (cs!!) [0,4,8], map (cs!!) [2,4,6]]
        winner = extractWinner $ map getSame $ chunks ++ transpose chunks ++ diagonals
     in case winner of
          Just p  -> PlayerWon p
          Nothing -> if hasEmptyCell then InProgress else EndedWithoutWinner
  where
    getSame :: [Cell] -> Maybe Player
    getSame [P x, P y, P z] | x == y && x == z = Just x
    getSame _                                  = Nothing

    extractWinner :: [Maybe a] -> Maybe a
    extractWinner [] = Nothing
    extractWinner (Just x:_) = Just x
    extractWinner (_:rest) = extractWinner rest


performMove :: GameState -> Move -> Maybe GameState
performMove (GameState board currentPlayer) move@(Move movePlayer pos)
  | currentPlayer /= movePlayer  = Nothing
  | pos < 1 || pos > 9           = Nothing
  | not (isValidMove move board) = Nothing
  | otherwise = 
      Just $ GameState (doMove move board) newPlayer
        where
            doMove :: Move -> Board -> Board
            doMove (Move piece pos) = setCell (P piece) pos

            newPlayer = if currentPlayer == X then O else X

gameStep :: GameState -> IO ()
gameStep gs@(GameState board currentPlayer) = do
    case gameResult board of
        PlayerWon winner -> print board >> (putStrLn $ show winner ++ " won the game! :)")
        EndedWithoutWinner -> print board >> (putStrLn $ "Game has finished without winner :/")
        InProgress -> 
            if currentPlayer == X then do
                mMove <- playerMove gs
                case mMove of
                    Nothing   -> putStrLn "Goodbye :)"   -- Exit game
                    Just move -> 
                        case performMove gs move of
                            Just newGameState -> gameStep newGameState
                            Nothing           -> putStrLn "Illegal move :(" >> gameStep gs
            else do
                let move = aiMove trivialAi gs
                case performMove gs move of
                    Just newGameState -> gameStep newGameState
                    Nothing           -> putStrLn "AI performed an invalid move!" -- End game

  where
    playerMove :: GameState -> IO (Maybe Move)
    playerMove gs@(GameState board currentPlayer) = do
        print board

        putStrLn "Enter your move"
        input <- getInput

        case input of
            IInvalid -> do
                putStrLn "Illegal move :("
                playerMove gs
            ICmd Exit -> 
                pure Nothing
            IPos pos ->
                pure . Just $ Move currentPlayer pos
