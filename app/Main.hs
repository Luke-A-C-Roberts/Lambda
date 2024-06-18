module Main where

import Lambda

import Control.Monad (forever)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

-- Main -------------------------------------------------------------------------------------------
main :: IO()
main = do
  hSetBuffering stdout NoBuffering
  forever $ do
    putStr ">> "
    input <- getLine 
    putStrLn $ eval TraceSteps input
