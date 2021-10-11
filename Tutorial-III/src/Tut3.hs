module Tut3 where
import Data.Char (toUpper)

name, idno, username :: String
name      =  "Myself, Me"  -- replace with your name
idno      =  "01234567"    -- replace with your student id
username  =  "memyselfi"   -- replace with your TCD username


declaration -- do not modify this
 = unlines
     [ ""
     , "@@@ This exercise is all my own work."
     , "@@@ Signed: " ++ name
     , "@@@ "++idno++" "++username
     ]

data BinTree a
  = Empty
  | Node (BinTree a) a (BinTree a)


{- Part 1
Write `binsert` to insert items into a BinTree
-}
binsert = error "binsert NYI!"

{- Part 2
Write `blookup` to search a BinTree
-}
blookup = error "blookup NYI!"

{- Part 3
Write `tpose` to transpose a list of lists
-}
tpose = error "tpose NYI!"
