-- Brainfuck interpreter written in Haskell
-- yea, gross, right?
data DataCell = Cell Int
type Cells = [DataCell] -- synonym