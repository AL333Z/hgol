module Lib where

import Text.Parsec ((<|>), many, many1)
import Text.Parsec (ParseError)
import Text.Parsec.String
import Text.Parsec.Char
import Data.Char (isLetter, isDigit)
import Text.ParserCombinators.Parsec.Combinator
import Data.Matrix (fromLists, toList, (!))


-- Move this to ADT module
data Cell = Cell Int Int
    deriving (Show, Eq, Ord)

type World = [Cell]

-- Actual parsing stuff

deadCell :: Parser Bool
deadCell = char '.' >> return False

aliveCell :: Parser Bool
aliveCell = char '*' >> return True

nat :: Parser Int
nat = do ds <- many1 digit
         return (read ds)

-- Parses a block of contiguous same-state cells
paramRow :: Parser [Bool]
paramRow = do
             n <- nat
             b <- aliveCell <|> deadCell
             return (replicate n b)

-- Parses a row of a map made up solely of dead and living cells
plainRow :: Parser [Bool]
plainRow = many1 (deadCell <|> aliveCell)

-- Parses a full row, which may have parameterised blocks or not
row :: Parser [Bool]
row = plainRow
--     do
--         f <- plainRow <|> paramRow
--         g <- row
--         return (f ++ g) <|> return []

-- Parses an offset (in the world map) pair with respect to the origin (0,0)
offset :: Parser Cell
offset = do
           string "#P"
           many1 space
           x <- nat
           many1 space
           y <- nat
           newline
           return $ Cell x y

-- Parses a world map block
inputBlock :: Parser [[Bool]]
inputBlock = do
    m <- many1 $ row <* newline
    let cols = maximum (map length m)
        sqr  = map (\r -> r ++ replicate (cols - length r) False) m
    return sqr

inputWorld :: Parser World
inputWorld = do
    input <- inputBlock
    let matrix = fromLists input
        cells = [((Cell i, j), matrix ! (i, j)) | i <- [1..(length input)],
                                                  j <- [1..(length $ head input)] ]
        listOfCells = toList $ fromLists cells
        aliveCells = filter (\x -> snd x == True) listOfCells
        world = map fst aliveCells
    return world

-- -- Parses a line of comments or other non-considered information
infoLine :: Parser ()
infoLine = char '#' >> many (alphaNum <|> space) >> newline *> return ()

-----------------------------------------------------------------------------
{-= World map file reading =-}

-- Reads a world map from a specified file and creates a World of alive cells
readLife :: String -> IO ()
readLife file = do
--     result <- parseFromFile ((infoLine `manyTill` offset) >> many1 inputBlock <* many space) file
--     result <- parseFromFile (offset >> many1 inputBlock <* many space) file
--     result <- parseFromFile (infoLine `manyTill` many1 inputBlock <* many space) file
--     result <- parseFromFile (infoLine `manyTill` inputBlock) file
    result <- parseFromFile (offset >> inputBlock <* many space) file
    case result of
        Left err  -> print err
        Right res  -> print res
