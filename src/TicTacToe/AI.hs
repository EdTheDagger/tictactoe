module TicTacToe.AI where

import TicTacToe.Core

possibleMoves :: GameState -> [Move]
possibleMoves (GameState (Board cs) currentPlayer) = 
    [Move currentPlayer pos | (c, pos) <- zip cs [1..], c == E]
