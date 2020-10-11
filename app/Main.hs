module Main where

import TicTacToe (runGame)

import System.IO (stdin, hSetBuffering, hSetEcho, BufferMode(NoBuffering))

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    runGame
