{- butrfeld Andrew Butterfield -}
module Ex04 where

name, idno, username :: String
name      =  "Cowan, James"  -- replace with your name
idno      =  "19309917"    -- replace with your student id
username  =  "cowanja"   -- replace with your TCD username

declaration -- do not modify this
 = unlines
     [ ""
     , "@@@ This exercise is all my own work."
     , "@@@ Signed: " ++ name
     , "@@@ "++idno++" "++username
     ]

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !


-- a binary tree datatype, honestly!
data BinTree k d
  = Branch (BinTree k d) (BinTree k d) k d
  | Leaf k d
  | Empty
  deriving (Eq, Show)


-- Part 1 : Tree Insert -------------------------------

-- Implement:
ins :: Ord k => k -> d -> BinTree k d -> BinTree k d
ins k d Empty = Leaf k d
ins k d (Leaf key val)
  | key == k = Leaf k d
  | key < k = Branch Empty (Leaf k d) key val
  | key > k = Branch (Leaf k d) Empty key val
ins k d (Branch left right key val)
  | key == k = Branch left right key val
  | key < k = Branch left (ins k d right) key val
  | key > k = Branch (ins k d left) right key val

-- Part 2 : Tree Lookup -------------------------------

-- Implement:
lkp :: (Monad m, Ord k) => BinTree k d -> k -> m d
lkp Empty _ = fail "Empty"
lkp (Branch left right key val) k
  | k == key = return val
  | k > key = lkp right k
  | k < key = lkp left k
lkp (Leaf key val) k
  | k == key = return val
  | k /= key = fail "Not Found"
  
-- Part 3 : Tail-Recursive Statistics

{-
   It is possible to compute BOTH average and standard deviation
   in one pass along a list of data items by summing both the data
   and the square of the data.
-}
twobirdsonestone :: Double -> Double -> Int -> (Double, Double)
twobirdsonestone listsum sumofsquares len
 = (average,sqrt variance)
 where
   nd = fromInteger $ toInteger len
   average = listsum / nd
   variance = sumofsquares / nd - average * average

{-
  The following function takes a list of numbers  (Double)
  and returns a triple containing
   the length of the list (Int)
   the sum of the numbers (Double)
   the sum of the squares of the numbers (Double)

   You will need to update the definitions of init1, init2 and init3 here.
-}
getLengthAndSums :: [Double] -> (Int,Double,Double)
getLengthAndSums ds = getLASs init1 init2 init3 ds
init1 :: Int
init2 :: Double
init3 :: Double

init1 = 0
init2 = 0.0
init3 = 0.0

{-
  Implement the following tail-recursive  helper function
-}
getLASs :: Int -> Double -> Double -> [Double] -> (Int,Double,Double)
getLASs initLen initNum initSqr xs = (len, num, sqr)
  where 
    len = initLen + length xs
    num = initNum + sum xs
    sqr = initSqr + sum [x*x | x<-xs]

-- Final Hint: how would you use a while loop to do this?
--   (assuming that the [Double] was an array of double)
