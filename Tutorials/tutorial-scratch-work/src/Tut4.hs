module Tut4
import Data.Char (toUpper)

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

data Predicate
  = TrueP
  | FalseP
  | AtomicP Name
  | PropOp Name [Predicate]
  | QuantOp Name Predicate
  deriving (Eq, Ord, Show, Read)

andp = PropOp "And"

andIdem = (PropOp "And" [a,b])
   | a == b = a
   | otherwise = PropOp "And" [a,b]
andIdem p = (False, p')

notp = PropOp "Not"

orTrue = (PropOp "Or" [a, (PropOp "Not" [b])])
   | a == b = (True, TrueP)
orTrue p = (False, p)

lawList = [andIdem, orTrue]

prove :: Predicate -> Bool
prove p
  = let
   (modified, p') = outerLoop lawList p
   in p' == TrueP

outerLoop llist p
  = let
      inner@(modified,p') = innerLoop llist p
    in if modified
      then innerLoop llist p'
      else (False,p')

innerLoop [] p = (False, p')
innerLoop (law: laws)
  = let
      inner2(modified, p') = law p
    in if modified then inner
      else innerLoop laws p