module TicTacToe.Game where

import TicTacToe.Types
import TicTacToe.Core
import TicTacToe.AI

runGame :: IO ()
runGame = gameStep (GameState emptyBoard X)

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
