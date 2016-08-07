module Main where

import Lib
import ADT
import System.Console.ANSI
import Control.Concurrent

-- Main evolution loop
mainEvolution :: String -> World -> Int -> IO ()
mainEvolution name w step = do
        if step > 0
            then do
                clearScreen
                putStrLn name
                newWorld <- evolveLogging w
                threadDelay 200000
                mainEvolution name newWorld (step - 1)
            else return ()

main :: IO ()
main = do
        let patternFilenames = map (\i -> "patterns/pic" ++ (show i) ++ ".life") [1..157]
        mapM_ eval patternFilenames

        where
            eval :: String -> IO ()
            eval patternFilename = do
                res <- readLife patternFilename
                case res of
                    Left err -> print err
                    Right world -> mainEvolution patternFilename world 15
