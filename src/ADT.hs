module ADT where

import Data.List
import Data.Matrix

-- ADT for a cell, which is a pair of x and y position
data Cell = Cell Int Int
    deriving (Show, Eq, Ord)

-- A representation for the world as a list that only contains alive cells
type World = [Cell]

-- Given a World and Cell, returns if a cell is alive
isAlive :: World -> Cell -> Bool
isAlive w c =  c `elem` w

-- FIXEME rewrite this..
-- Returns all the neighbours (both dead and alive) of a given Cell
neighbours :: Cell -> [Cell]
neighbours (Cell x y) = [Cell (x-1) (y-1)] ++ [Cell (x-1) y ] ++ [Cell (x-1) (y+1)] ++ [Cell x (y-1)] ++
        [Cell x (y+1)] ++ [Cell (x+1) (y-1)] ++ [Cell (x+1) y] ++ [Cell (x+1) (y+1)]

-- Given a World and a list of Cells, returns all the alive cells
aliveCells :: World -> [Cell] -> [Cell]
aliveCells world cells = cells `intersect` world

-- Given a World and a list of Cells, returns all the dead cells
deadCells :: World -> [Cell] -> [Cell]
deadCells world cells = cells \\ world

-- Given a World and a list of Cell, returns a
aliveCellsCount :: World -> [Cell] -> Int
aliveCellsCount world cells = length $ aliveCells world cells

-- Given a World and a list of Cell, returns a list of pair of Cell, neighbours count
cellsWithNeighboursCount :: World -> [Cell] -> [(Cell, Int)]
cellsWithNeighboursCount world cells =
    map (\cell -> (cell, aliveCellsCount world (neighbours cell))) cells

--Given a list of cell and neighbours count and a predicate, return a list of alive cells
cellsAfterEvolution :: [(Cell, Int)] -> (Int -> Bool) -> [Cell]
cellsAfterEvolution cells pred = map fst $ filter (\(cell, count) -> pred count) cells

-- Predicate that given a number of alive neighbours, returns if a Cell should be alive
keepAlivePred :: Int -> Bool
keepAlivePred x | x < 2 = False
                | x == 2 || x == 3 = True
                | x > 3  = False

-- Predicate that given a number of dead neighbours, returns if a Cell should be dead
resurrectDeadPred :: Int -> Bool
resurrectDeadPred x = x == 3

-- Function that given a World, return a new World
evolve :: World -> World
evolve currentWorld = stillAliveCells ++ resurrectedCells
    where
        deadCellsToEvaluate = nub $ currentWorld >>= (\cell -> deadCells currentWorld $ neighbours cell)
        deadCellsWithNeighbours = cellsWithNeighboursCount currentWorld deadCellsToEvaluate
        resurrectedCells = cellsAfterEvolution deadCellsWithNeighbours resurrectDeadPred

        aliveCellsWithNeighbours = cellsWithNeighboursCount currentWorld currentWorld
        stillAliveCells = cellsAfterEvolution aliveCellsWithNeighbours keepAlivePred

-- Decorate world evolution, printing out and returning the new World
evolveLogging :: World -> IO World
evolveLogging w = do
                let newWorld = evolve w
                printWorld newWorld
                return newWorld

-- Utility method that returns a matrix representation for a World
worldMatrix :: World -> Matrix Char
worldMatrix w =
    let
        r =  maximum (map (\(Cell x y) -> x) w)
        c =  maximum (map (\(Cell x y) -> y) w)
    in matrix r c $ \(i, j) -> if Cell i j `elem` w then '*' else '.'

-- Utility method that prints a world
printWorld :: World -> IO ()
printWorld w = print $ worldMatrix w
