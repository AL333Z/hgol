module Lib where

import Text.Parsec ((<|>), many, many1)
import Text.Parsec (ParseError)
import Text.Parsec.String
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Data.Matrix (fromLists, toList, (!))
import ADT

-- Parses a dead cell
deadCell :: Parser Bool
deadCell = char '.' >> return False

-- Parses an alive cell
aliveCell :: Parser Bool
aliveCell = char '*' >> return True

-- Parses a row made up of dead and living cells
row :: Parser [Bool]
row = many1 (deadCell <|> aliveCell)

-- Parses a natural number
nat :: Parser Int
nat = do ds <- many1 digit
         return $ read ds

-- Parses an offset with respect to the origin (0,0)
offset :: Parser Cell
offset = do
           string "#P"
           many1 space
           x <- nat
           many1 space
           y <- nat
           newline
           return $ Cell x y

-- Parses a matrix of boolean, which represents the inital world
inputBlock :: Parser [[Bool]]
inputBlock = do
    m <- many1 $ row <* newline
    let cols = maximum (map length m)
        sqr  = map (\r -> r ++ replicate (cols - length r) False) m
    return sqr

-- Parses a world
inputWorld :: Parser World
inputWorld = do
    input <- inputBlock
    let matrix = fromLists input
        rowNum = length input
        colNum = length $ head input
        cells = [((Cell i j), (matrix ! (i, j))) | i <- [1..rowNum], j <- [1..colNum]]
        aliveCells = filter (\x -> snd x == True) cells
        world = map fst aliveCells
    return world

-- Reads a world map from a specified file and creates a World of alive cells
readLife ::  String -> IO (Either ParseError World)
readLife file = parseFromFile (offset >> inputWorld <* many space) file
