-- Brainfuck interpreter written in Haskell
module Brainfuck (
   DataCell(..)
  ,Cells
  ,DataPointer
  ,decrease
                 ) where
-- data structures
data DataCell = Cell  { value :: Int }

-- synonyms
type DataPointer = Int
type Cells = [DataCell]

-- functions
-- DataPointer
decrease :: DataPointer -> DataPointer
decrease 0           = 0
decrease datapointer = datapointer - 1

increase :: DataPointer -> DataPointer
increase datapointer = datapointer + 1