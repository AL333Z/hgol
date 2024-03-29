module Main where

import Lib
import ADT
import System.Console.ANSI
import Control.Concurrent
import Control.Monad (when)

-- Main evolution loop
mainEvolution :: String -> World -> Int -> IO ()
mainEvolution name w step =
        when (step > 0) $
             do
                clearScreen
                putStrLn name
                newWorld <- evolveLogging w
                threadDelay 200000
                mainEvolution name newWorld (step - 1)

main :: IO ()
main = do
        let patternFilenames = map (\i -> "patterns/pic" ++ show i ++ ".life") [1..157]
        mapM_ eval patternFilenames

        where
            eval :: String -> IO ()
            eval patternFilename = do
                res <- readLife patternFilename
                case res of
                    Left err -> print err
                    Right world -> mainEvolution patternFilename world 15
