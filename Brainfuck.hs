-- Brainfuck interpreter written in Haskell
module Brainfuck (
   DataCell(..)
  ,DataPointer
  ,decreasePointer
                 ) where

import Data.Char

-- Virtual machine
-- data structures
data DataCell = Cell  { value :: Int } deriving Show
data BFMachine = Machine { cells :: [DataCell], pointer :: DataPointer, output :: String } deriving Show

-- synonyms
type DataPointer = Int

-- maximum number of cells
max_cell_size = 300 :: Int

-- functions
decreasePointer :: BFMachine -> BFMachine
decreasePointer Machine { cells=oldcells , pointer=0, output=out } = Machine { cells=oldcells, pointer=0, output=out }
decreasePointer machine = Machine { cells=oldcells, pointer=(oldpointer - 1), output=out }
  where oldpointer = pointer machine
        oldcells = cells machine
        out = output machine

increasePointer :: BFMachine -> BFMachine
increasePointer machine
  | (pointer machine == max_cell_size) = machine
  | otherwise = Machine { cells=oldcells, pointer=(oldpointer + 1), output=out }
    where oldpointer = pointer machine
          oldcells = cells machine
          out = output machine
        
replaceCellAtPointer :: BFMachine -> DataCell -> BFMachine
replaceCellAtPointer machine newcell = Machine { cells=(less ++ [newcell] ++ more),pointer=datapointer, output=output machine }
  where oldcells = cells machine
        datapointer = pointer machine
        (less,_:more) = splitAt datapointer (oldcells)

increaseCell :: BFMachine -> BFMachine
increaseCell machine = replaceCellAtPointer machine newcell
  where oldcell = cells machine !! pointer machine
        newcell = Cell((value oldcell) + 1)

decreaseCell :: BFMachine -> BFMachine
decreaseCell machine = replaceCellAtPointer machine newcell
  where oldcell = cells machine !! pointer machine
        newcell = Cell ((value oldcell) - 1)

showCurrentCell :: BFMachine -> BFMachine
showCurrentCell machine = Machine { cells=cells machine, pointer=pointer machine, output = output machine ++ [chr currentValue]}
  where currentCell = cells machine !! pointer machine
        currentValue = value currentCell

-- Auxiliary functions
generateCells :: [DataCell]
generateCells = replicate max_cell_size (Cell 0)

generateMachine :: BFMachine
generateMachine = Machine { cells=generateCells,pointer=0,output=[] }

-- END virtual machine

-- Code interpreter
type CodePointer = Int
type Code = String

-- auxiliary functions
findMatchingBracketL :: Code -> CodePointer -> Maybe CodePointer
findMatchingBracketL code codepointer
  | (code !! codepointer) /= ']' = Nothing
  | otherwise = matching
  where (left,right) = splitAt codepointer code
        leftbrackets =  filter (\x -> fst x == ']') $ zip left [0..] -- zip for ordering the brackets
        rightbrackets = filter (\x -> fst x == '[') $ zip left [0..]
        matching
          | (length rightbrackets >= length leftbrackets) = Just (snd $ (reverse rightbrackets) !! (length leftbrackets)) -- reversed cuz we need to check them 
          | otherwise = Nothing
-- END code interpreter

main = do
  line <- getLine
  let code = words line
      machine = generateMachine
    in return ()