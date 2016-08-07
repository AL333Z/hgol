module Main where

import Lib
import Data.List
import Data.Matrix
import Control.Monad
import System.Process
import Control.Concurrent
import System.Console.ANSI

worldMatrix :: World -> Matrix Char
worldMatrix w =
    let
        r =  maximum (map (\(Cell x y) -> x) w)
        c =  maximum (map (\(Cell x y) -> y) w)
    in matrix r c $ \(i, j) -> if Cell i j `elem` w then '*' else '.'

printWorld :: World -> IO ()
printWorld w = print $ worldMatrix w

isAlive :: World -> Cell -> Bool
isAlive w c =  c `elem` w

-- FIXEME rewrite this..
neighbours :: Cell -> [Cell]
neighbours (Cell x y) = [Cell (x-1) (y-1)] ++ [Cell (x-1) y ] ++ [Cell (x-1) (y+1)] ++ [Cell x (y-1)] ++
        [Cell x (y+1)] ++ [Cell (x+1) (y-1)] ++ [Cell (x+1) y] ++ [Cell (x+1) (y+1)]

aliveCells :: World -> [Cell] -> [Cell]
aliveCells world cells = cells `intersect` world

deadCells :: World -> [Cell] -> [Cell]
deadCells world cells = cells \\ world

aliveNeighboursCount :: World -> [Cell] -> Int
aliveNeighboursCount world cells = length $ aliveCells world cells

cellsWithNeighboursCount :: World -> [Cell] -> [(Cell, Int)]
cellsWithNeighboursCount world cellsToEvaluate =
    map (\cell -> (cell, aliveNeighboursCount world (neighbours cell))) cellsToEvaluate

cellsAfterEvolution :: [(Cell, Int)] -> (Int -> Bool) -> [Cell]
cellsAfterEvolution cells pred = map fst $ filter (\(cell, count) -> pred count) cells

keepAlivePred :: Int -> Bool
keepAlivePred x | x < 2 = False
                | x == 2 || x == 3 = True
                | x > 3  = False

resurrectDeadPred :: Int -> Bool
resurrectDeadPred x = x == 3

evolve :: World -> World
evolve currentWorld = stillAliveCells ++ resurrectedCells
    where
        deadCellsToEvaluate = nub $ currentWorld >>= (\cell -> deadCells currentWorld $ neighbours cell)
        deadCellsWithNeighbours = cellsWithNeighboursCount currentWorld deadCellsToEvaluate
        resurrectedCells = cellsAfterEvolution deadCellsWithNeighbours resurrectDeadPred

        aliveCellsWithNeighbours = cellsWithNeighboursCount currentWorld currentWorld
        stillAliveCells = cellsAfterEvolution aliveCellsWithNeighbours keepAlivePred

evolveLogging :: World -> IO World
evolveLogging w = do
                let newWorld = evolve w
                printWorld newWorld
                return newWorld

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

-- Main loop
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
