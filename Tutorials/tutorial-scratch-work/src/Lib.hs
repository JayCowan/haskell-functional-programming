module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data BinTree a
    =Empty
    | Node (BinTree a) a (BinTree a)
    deriving Show 

binsert = error "binsert NYI!"

blookup = error "blookup NYI!"

tpose = error "tpose NYI!"