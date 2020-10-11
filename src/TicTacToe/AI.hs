module TicTacToe.AI where

import TicTacToe.Types

data AI = AI { makeMove :: GameState -> Move }

possibleMoves :: GameState -> [Move]
possibleMoves (GameState (Board cs) currentPlayer) = 
    [Move currentPlayer pos | (c, pos) <- zip cs [1..], c == E]

trivialAi :: AI
trivialAi = AI (head . possibleMoves)
