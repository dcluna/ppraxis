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
data BFMachine = Machine { cells :: [DataCell], pointer :: DataPointer } deriving Show

-- synonyms
type DataPointer = Int

-- maximum number of cells
max_cell_size = 10 :: Int

-- functions
decreasePointer :: BFMachine -> BFMachine
decreasePointer Machine { cells=oldcells , pointer=0 } = Machine { cells=oldcells, pointer=0 }
decreasePointer machine = Machine { cells=oldcells, pointer=(oldpointer - 1) }
  where oldpointer = pointer machine
        oldcells = cells machine

increasePointer :: BFMachine -> BFMachine
increasePointer machine
  | (pointer machine == max_cell_size) = machine
  | otherwise = Machine { cells=oldcells, pointer=(oldpointer + 1) }
    where oldpointer = pointer machine
          oldcells = cells machine
        
replaceCellAtPointer :: BFMachine -> DataCell -> BFMachine
replaceCellAtPointer machine newcell = Machine { cells=(less ++ [newcell] ++ more),pointer=datapointer }
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

showCurrentCell :: BFMachine -> IO ()
showCurrentCell machine = do
  putStrLn $ show(chr $ value (cells machine !! pointer machine))

-- Auxiliary functions
generateCells :: [DataCell]
generateCells = replicate max_cell_size (Cell 0)

generateMachine :: BFMachine
generateMachine = Machine { cells=generateCells,pointer=0 }

-- Enforces Haskell's type system for vm instructions
instruction :: (BFMachine -> a) -> BFMachine -> a
instruction f machine = f machine

-- END virtual machine

--decodeInstruction :: Char -> (BFMachine -> a)
decodeInstruction '<' = decreasePointer
decodeInstruction '>' = increasePointer
decodeInstruction '+' = increaseCell
decodeInstruction '-' = decreaseCell
--decodeInstruction '.' = showCurrentCell

main = do
  line <- getLine
  let code = words line
      machine = generateMachine
    in return ()