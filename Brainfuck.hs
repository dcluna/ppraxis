-- Brainfuck interpreter written in Haskell
module Brainfuck (
   DataCell(..)
  ,DataPointer
  ,decreasePointer
                 ) where

import Data.Char
import Data.Maybe

-- Virtual machine
-- data structures
data DataCell = Cell  { value :: Int } deriving Show
data BFMachine = Machine { cells :: [DataCell], pointer :: DataPointer, output :: String, code :: Code, codepointer :: CodePointer } deriving Show

-- synonyms
type DataPointer = Int

-- maximum number of cells
max_cell_size = 30000 :: Int

-- functions
decreasePointer :: BFMachine -> BFMachine
decreasePointer Machine { cells=oldcells , pointer=0, output=out, code=input, codepointer=cpointer } = Machine { cells=oldcells,pointer=0,output=out,code=input,codepointer=cpointer }
decreasePointer machine = Machine { cells=oldcells, pointer=(oldpointer - 1), output=out, code=code machine, codepointer=codepointer machine }
  where oldpointer = pointer machine
        oldcells = cells machine
        out = output machine

increasePointer :: BFMachine -> BFMachine
increasePointer machine
  | (pointer machine == max_cell_size) = machine
  | otherwise = Machine { cells=oldcells, pointer=(oldpointer + 1), output=out,code=(code machine),codepointer=(codepointer machine) }
    where oldpointer = pointer machine
          oldcells = cells machine
          out = output machine
        
increaseCodePointer :: BFMachine -> BFMachine
increaseCodePointer machine = Machine {cells=cells machine,pointer=pointer machine,output=output machine,code=code machine,codepointer=(codepointer machine)+1}

replaceCellAtPointer :: BFMachine -> DataCell -> BFMachine
replaceCellAtPointer machine newcell = Machine { cells=(less ++ [newcell] ++ more),pointer=datapointer, output=output machine,code=(code machine),codepointer=(codepointer machine)}
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
showCurrentCell machine = Machine { cells=cells machine, pointer=pointer machine, output = output machine ++ [chr currentValue],code=(code machine),codepointer=(codepointer machine)}
  where currentCell = cells machine !! pointer machine
        currentValue = value currentCell

-- Auxiliary functions
generateCells :: [DataCell]
generateCells = replicate max_cell_size (Cell 0)

generateMachine :: Code -> BFMachine
generateMachine input = Machine { cells=generateCells,pointer=0,output=[],codepointer=0,code=input }

run :: Char -> (BFMachine -> BFMachine)
run '<' = increaseCodePointer . decreasePointer
run '>' = increaseCodePointer . increasePointer
run '-' = increaseCodePointer . decreaseCell
run '+' = increaseCodePointer . increaseCell
run '.' = increaseCodePointer . showCurrentCell
run '[' = increaseCodePointer . checkCondition
run ']' = increaseCodePointer . restartLoop

-- recursive function to loop until the end of the code (or error occurrences)
vmloop :: BFMachine -> Maybe BFMachine
vmloop machine
  | (codepointer machine == -1) = Nothing
  | ((codepointer machine) >= (length $ code machine)) = Just machine
  | otherwise = let codei = code machine
                    cpointer = codepointer machine
                    currentInst = codei !! cpointer
                    result = run currentInst machine
                in vmloop result
-- END virtual machine

-- Code interpreter
type CodePointer = Int
type Code = String

-- loops
checkCondition :: BFMachine -> BFMachine
checkCondition machine = if (datav == 0) then endLoop machine
                         else machine -- we don't increment here because we do so in 'run'
                           where datacell = cells machine !! pointer machine
                                 datav = value datacell      

-- program flow functions
endLoop :: BFMachine -> BFMachine
endLoop machine
  | error = Machine {cells=cells machine,pointer=pointer machine,output = output machine,code=(code machine),codepointer = -1}
  | otherwise = Machine {cells=cells machine,pointer=pointer machine,output = output machine,code=(code machine),codepointer = bracketPoint}
    where error = (matchingBracket == Nothing)
          matchingBracket = findMatchingBracketR (code machine) (codepointer machine)
          Just bracketPoint = matchingBracket

restartLoop :: BFMachine -> BFMachine
restartLoop machine
  | error = Machine {cells=cells machine,pointer=pointer machine,output = output machine,code=(code machine),codepointer = -1}
  | otherwise = Machine {cells=cells machine,pointer=pointer machine,output = output machine,code=(code machine),codepointer = bracketPoint}
    where error = (matchingBracket == Nothing)
          matchingBracket = findMatchingBracketL (code machine) (codepointer machine)
          Just bracketPoint = matchingBracket

-- auxiliary functions
-- the idea here is to grab both brackets' codepointers and use them as 'gotos'
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
                        
findMatchingBracketR :: Code -> CodePointer -> Maybe CodePointer
findMatchingBracketR code codepointer
  | (code !! codepointer) /= '[' = Nothing
  | otherwise = matching
  where (left,right) = splitAt (codepointer+1) code
        rightbrackets =  filter (\x -> fst x == ']') $ zip right $ map (+(codepointer+1)) [0..] -- zip for ordering the brackets
        leftbrackets = filter (\x -> fst x == '[') $ zip right $ map (+(codepointer+1)) [0..]
        matching
          | (length rightbrackets >= length leftbrackets) = Just (snd $ rightbrackets !! (length leftbrackets)) -- reversed cuz we need to check them 
          | otherwise = Nothing
-- END code interpreter

main = do
  line <- getLine
  let code = line
      machine = generateMachine code
      result = vmloop machine
    in if (isNothing result) then putStrLn "Error in code"
   else putStrLn $ output $ fromJust result