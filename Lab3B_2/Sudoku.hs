module Sudoku where

import Test.QuickCheck
import Data.Char (digitToInt)
import Data.Maybe (fromJust,listToMaybe, isNothing, isJust)


------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row]
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [[Nothing | x <- [1..9]] | y <- [1..9]]

-- * A2

-- | isSudoku sudoku checks if sudoku is really a 
-- valid representation of a sudoku puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rows) =
    length rows == 9 && all validRow rows
  where
    validRow row = length row == 9 && all validCell row
    validCell Nothing  = True
    validCell (Just n) = n `elem` [1..9]



-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku s) = and [False | row <- s, x <- row, isNothing x]

---------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
-- 'Just 0' here is for generating \n at the end
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku s)= putStr $
  concat [ x | row <- s, x <- map func (row ++ [Just 0])]
   where func (Just a) | a /= 0    = show a  -- to the String
                       | otherwise = "\n"  -- the end of row
         func Nothing = "."


-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku

-- helper function : f is for convert list of String to list of Cell
f :: [Char] -> [Maybe Int]
f [] = []
f (x:xs) | x == '.'  = Nothing : f xs
         | otherwise = Just (digitToInt x) : f xs


-- readSudoku: text is [String]
-- row is lines text , it is [[String]], they should be 9 rows and
-- total 81 char
-- if conditions satisfied, they need to be converted and show on IO
-- otherwise error
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do
  text <- readFile file
  let row = lines text
  let rowNumber = length row == 9
  let totalChar = sum [length y | y<-row] == 81
  if rowNumber && totalChar then
    return $ Sudoku $ map f row
  else return $ error "Not a Sudoku!"


------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
cell = frequency 
   [
    (9, return Nothing), -- Higher frequency for Nothing
    (1, elements (map Just [1..9])) -- Only generate Just values from 1 to 9
   ]


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do
    -- 9 rows [Gen [Cell]], each row has 9 Gen [Cell]
    let listGen = [vectorOf 9 cell | x<-[1..9]]
    table <- sequence listGen
    return $ Sudoku table


 -- hint: get to know the QuickCheck function vectorOf

-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku  = isSudoku 
  -- hint: this definition is simple!

------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell

-- * D1

-- as long as : Nothing can be many, 
--- but 'Just X' can be only one time in one block
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (x:xs)
  | isJust x  = notElem x xs && isOkayBlock xs
  | otherwise = isOkayBlock xs



-- * D2
-- we have three kinds of block:
-- 1. all rows , which are the elements is list 's' in (Sudoku s)
-- 2. all column, they are s(0:8)(index), index ∈ [0..8] 
-- for example: s(0)(index==6),s(1)(6)..... s(8)(6) are in the same block
-- 3. the square blcok in sudoku (you must know what it means)
-- to implement 3. we first to group three rows togethor as 'row3' in 'a'
-- we read row3 one time and read "the bias" which is 'y'
-- if bias =0, we traverse the left most three blocks
-- bias==1 is middle, bias==2 is right
blocks :: Sudoku -> [Block]
blocks (Sudoku s) =
    s
    ++ [ [row !! index | row <- s]              | index <- [0..8] ]
    ++ [ [row !! (x+y) | row<-row3, x<-[0..2] ] | y<-[0,3,6] , row3<-a]
    
    -- a is a set of three group rows: [[row0-2],[row3-5],[row6-8]]
    where a = [ take 3 s, take 3 $ drop 3 s,drop 3 $ drop 3 s]



-- * D3
isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)





--test function for A: 

--A1:
prop_allBlankSudoku :: Bool
prop_allBlankSudoku = all (all (== Nothing)) (rows allBlankSudoku)

--A2:
prop_isSudoku :: Sudoku -> Bool
prop_isSudoku sudoku = 
  isSudoku sudoku == (length (rows sudoku) == 9 
  && all ((== 9) . length) (rows sudoku))

--A3:
prop_isFilled :: Sudoku -> Bool
prop_isFilled sudoku = 
  isFilled sudoku == all (notElem Nothing) (rows sudoku)



--test function for D:
--D1:
--Check that a block does not contain duplicate numbers
hasDuplicates :: [Cell] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = isJust x && elem x xs || hasDuplicates xs

prop_isOkayBlock :: Block -> Bool
prop_isOkayBlock block = isOkayBlock block == not (hasDuplicates block)
    

--D2:
--This ensures that blocks function is splitting the 
--Sudoku into 27 blocks of 9 cells each.
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths s
  = length (blocks s) == 27
  && and [ length b == 9 | b <- blocks s ]

--D3:
--Test that a Sudoku has no duplicate numbers in any block
prop_isOkay :: Sudoku -> Bool
prop_isOkay sudoku = isOkay sudoku == all isOkayBlock (blocks sudoku)




---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks (Sudoku s) = 
  [(x,y) | x <- [0..8], y<-[0..8], isNothing ((s !! x) !! y)]

-- * E2
(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = take i xs ++ [y] ++ drop (i+1) xs


update :: Sudoku -> Pos -> Cell -> Sudoku
update (Sudoku s) (x,y) value
  = Sudoku (s !!= (x,row)) where
    row = (s !! x) !!= (y,value)






------------------------------------------------------------------------------

-- * F1
-- backtracking' :: Sudoku -> [Sudoku]
-- backtracking' s | not(isOkay s) = []
-- backtracking' s | isOkay s && isFilled s= [s]
-- backtracking' s | isOkay s && not(isFilled s)= 
--   concat [
--     backtracking' (update s xy (Just n))  
--     | n<-[1..9] 
--     ] where xy = head $ blanks s

-- solve' :: Sudoku -> Maybe Sudoku
-- solve' s | not(isOkay s)  = Nothing
--         | otherwise = listToMaybe $ backtracking' s


backtracking :: Sudoku -> [Pos] -> [Sudoku]
backtracking s [] | isOkay s = [s]
                  | otherwise = []
backtracking s (xy:xys) 
  | isOkay s = 
    concat [ backtracking (update s xy (Just n)) xys | n<-[1..9] ]
  | otherwise = []

solve :: Sudoku -> Maybe Sudoku
solve s | not(isOkay s)  = Nothing
        | otherwise = listToMaybe $ backtracking s (blanks s)



-- * F2
readAndSolve :: FilePath -> IO ()
readAndSolve path = do
  s <- readSudoku path 
  case solve s of
    Nothing -> putStr "(no solution)\n"
    _ -> printSudoku (fromJust (solve s))


-- * F3

check :: Sudoku -> Sudoku -> Bool
check s1 s2 
  | not $ isFilled s2 = check s1 (update s2 (x,y) ((rows s1 !! x) !! y))
  | otherwise = s1==s2
  where (x,y) = head $ blanks s2

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s1 s2 = isOkay s1 && isFilled s1 && check s1 s2











--test function for E
--E1:

prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = 
  all (isBlankCell allBlankSudoku) (blanks allBlankSudoku) &&
  sum [1 | x<- blanks allBlankSudoku] == 81
  where
    isBlankCell (Sudoku s) (x, y) = isNothing (s !! x !! y) 

--E2:
--prop_bangBangEquals_correct :: ...
--prop_bangB

prop_bangBangEquals_correct :: [Int] -> Int -> Int -> Property
prop_bangBangEquals_correct list index element =
  not (null list) ==>
    let updatedList = list !!= (modIndex, element)
        modIndex = index `mod` length list
    in  updatedList !! modIndex == element &&
        all (\(i, x) -> i == modIndex || x == (list !! i)) 
        (zip [0..] updatedList)


-- E3

prop_update_updated_outofRange :: Sudoku -> (Int, Int) -> Maybe Int -> Bool
prop_update_updated_outofRange (Sudoku s) (x,y) value = 
  (rows s' !! x) !! y == value
  where s' = update (Sudoku s) (x,y) value

prop_update_updated :: Sudoku -> (Int, Int) -> Maybe Int -> Bool
prop_update_updated (Sudoku s) (x,y)  
  = prop_update_updated_outofRange (Sudoku s) (x',y')  
  where 
    x' = abs x `mod` 9
    y' = abs y `mod` 9




--Test function for F:
--F1:

prop_solve :: Sudoku -> Property
prop_solve sudoku 
  = isSudoku sudoku && isOkay sudoku && not (isFilled sudoku) ==>
  property $ case solve sudoku of
    -- Solving an incomplete Sudoku may not always yield a result
    Nothing -> True  
    Just s  -> isSudoku s && isFilled s


--F3:

prop_isSolutionOf :: Sudoku -> Property
prop_isSolutionOf sudoku 
  = isSudoku sudoku && isOkay sudoku && not (isFilled sudoku) ==>
  property $ case solve sudoku of
    -- No solution available
    Nothing -> True  
    Just s  -> s `isSolutionOf` sudoku


--F4:
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sudoku 
  = isSudoku sudoku && isOkay sudoku && not (isFilled sudoku) ==>
  property $ case solve sudoku of
     -- No solution available
    Nothing -> True 
    Just s  -> s `isSolutionOf` sudoku



fewerChecks :: Testable prop => prop -> IO ()
fewerChecks  = quickCheckWith stdArgs { maxSuccess = 30 } 

main :: IO ()
main = do
  putStrLn "Testing prop_Solve with fewer checks"
  fewerChecks prop_solve
  putStrLn "Testing prop_isSolutionOf with fewer checks"
  fewerChecks prop_isSolutionOf
  putStrLn "Testing prop_SolveSound with fewer checks"
  fewerChecks prop_SolveSound
