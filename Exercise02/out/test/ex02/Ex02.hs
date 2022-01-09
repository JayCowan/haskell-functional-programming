{- butrfeld Andrew Butterfield -}
module Ex02 where

import Data.List.NonEmpty (isPrefixOf)


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

-- Datatypes and key functions -----------------------------------------------

-- do not change anything in this section !

type Id = String

data Expr
  = Val Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Dvd Expr Expr
  | Var Id
  | Def Id Expr Expr
  deriving (Eq, Show)

type Dict k d  =  [(k,d)]

define :: Dict k d -> k -> d -> Dict k d
define d s v = (s,v):d

find :: Dict String d -> String -> Either String d
find []             name              =  Left ("undefined var "++name)
find ( (s,v) : ds ) name | name == s  =  Right v
                         | otherwise  =  find ds name

type EDict = Dict String Double

v42 = Val 42 ; j42 = Just v42

-- do not change anything above !

-- Part 1 : Evaluating Expressions -- (50 test marks, worth 25 Exercise Marks) -

-- Implement the following function so all 'eval' tests pass.

-- eval should return `Left msg` if:
  -- (1) a divide by zero operation was going to be performed;
  -- (2) the expression contains a variable not in the dictionary.
  -- see test outcomes for the precise format of those messages
eval :: EDict -> Expr -> Either String Double
eval d e = case reduce d e of 
            (Val x) -> Right x
            (Var y) -> Left y
            _ -> error "undefined behavior"
            
reduce :: EDict -> Expr -> Expr
reduce d (Var n) = case var d (Var n) of
                    Left x -> Var x
                    Right y -> Val y
reduce _ (Val n) = Val n
reduce d (Def n m i)| m == Var "div by zero" = m
                    | i == Var "div by zero" = i
                    | otherwise = def d n (reduce d m) (reduce d i)
reduce [] (Add (Var n) _) = Var (findHandler ("undefined var "++n))
reduce [] (Add _ (Var m)) = Var (findHandler ("undefined var "++m))
reduce d (Add (Var n) (Var m)) = case find d n of
                                  Left s -> Var s
                                  Right r -> case find d m of
                                             Left s2 -> Var s2
                                             Right r2 -> Val (r+r2)
reduce d (Add n m) = add (reduce d n) (reduce d m)
reduce [] (Sub (Var n) _) = Var (findHandler ("undefined var "++n))
reduce [] (Sub _ (Var m)) = Var (findHandler ("undefined var "++m))
reduce d (Sub (Var n) (Var m)) = case find d n of
                                 Left s -> Var s
                                 Right r -> case find d m of
                                            Left s2 -> Var s2
                                            Right r2 -> Val (r-r2)
reduce d (Sub n m) = sub (reduce d n) (reduce d m)
reduce [] (Mul (Var n) _) = Var (findHandler ("undefined var "++n))
reduce [] (Mul _ (Var m)) = Var (findHandler ("undefined var "++m))
reduce d (Mul (Var n) (Var m)) = case find d n of
                                 Left s -> Var s
                                 Right r -> case find d m of
                                            Left s2 -> Var s2
                                            Right r2 -> Val (r*r2)
reduce d (Mul n m) = mul (reduce d n) (reduce d m)
reduce [] (Dvd (Var n) _) = Var (findHandler ("undefined var "++n))
reduce [] (Dvd _ (Var m)) = Var (findHandler ("undefined var "++m))
reduce d (Dvd (Var n) (Var m)) = case find d n of
                                 Left s -> Var s
                                 Right r -> case find d m of
                                            Left s2 -> Var s2
                                            Right r2 -> if r2 /= 0 then Val (r/r2) else Var "div by zero"

reduce d (Dvd n m) = case dvd (reduce d n) (reduce d m) of
                      Left _ -> Var "div by zero"
                      Right x -> x

def :: EDict -> Id -> Expr -> Expr -> Expr
def d n m i = case eval d m of
              Left x -> Var x
              Right y -> reduce (define d n y) i

add :: Expr -> Expr -> Expr
add (Var a) (Var b) | a == "div by zero" = Var "div by zero" 
                    | b == "div by zero" = Var "div by zero"
                    | otherwise = Add (Var a) (Var b)
add (Var a) b | a == "div by zero" = Var "div by zero"
              | otherwise = Add (Var a) b
add a (Var b) | b == "div by zero" = Var "div by zero"
              | otherwise = Add a (Var b)
add (Val a) (Val b) = Val (a + b)
add (Val 0) b = b
add a (Val 0) = a
add a b = Add a b

sub :: Expr -> Expr -> Expr
sub (Var a) (Var b) | a == "div by zero" = Var "div by zero" 
                    | b == "div by zero" = Var "div by zero"
                    | otherwise = Sub (Var a) (Var b)
sub (Var a) b | a == "div by zero" = Var "div by zero"
              | otherwise = Sub (Var a) b
sub a (Var b) | b == "div by zero" = Var "div by zero"
              | otherwise = Sub a (Var b)
sub (Val a) (Val b) = Val (a - b)
sub a (Val 0) = a
sub a b = Sub a b

mul :: Expr -> Expr -> Expr
mul (Var a) (Var b) | a == "div by zero" = Var "div by zero" 
                    | b == "div by zero" = Var "div by zero"
                    | otherwise = Mul (Var a) (Var b)
mul (Var a) b | a == "div by zero" = Var "div by zero"
              | otherwise = Mul (Var a) b
mul a (Var b) | b == "div by zero" = Var "div by zero"
              | otherwise = Mul a (Var b)
mul (Val a) (Val b) = Val (a * b)
mul (Val 0) _ = Val 0
mul _ (Val 0) = Val 0
mul (Val 1) b = b
mul a (Val 1) = a
mul a b = Mul a b

dvd :: Expr -> Expr -> Either String Expr
dvd (Var a) (Var b) | a == "div by zero" = Left "div by zero" 
                    | b == "div by zero" = Left "div by zero"
                    | otherwise = Right (Dvd (Var a) (Var b))
dvd (Var a) b | a == "div by zero" = Left "div by zero"
              | otherwise = Right (Dvd (Var a) b)
dvd a (Var b) | b == "div by zero" = Left "div by zero"
              | otherwise = Right (Dvd a (Var b))
dvd (Val 0) _ = Right (Val 0)
dvd _ (Val 0) = Left "div by zero"
dvd a (Val 1) = Right a
dvd (Val a) (Val b) = Right (Val (a / b))
dvd a b = Right (Dvd a b)

var :: EDict -> Expr -> Either String Double
var d (Var x) | x == "div by zero" = Left "div by zero"
              | otherwise = case find d x of
                            Left i -> Left (findHandler i)
                            Right y -> Right y
findHandler :: String -> String
findHandler s = case length "undefined var undefined var " > length s of
                True -> s
                False -> drop (length "undefined var ") s





-- Part 1 : Expression Laws -- (15 test marks, worth 15 Exercise Marks) --------

{-

There are many, many laws of algebra that apply to our expressions, e.g.,

  x + y         =   y + z            Law 1
  (x + y) + z   =   x + (y + z)      Law 2
  (x - y) - z   =   x - (y + z)      Law 3
  x*x - y*y     =   (x + y)*(x - y)  Law 4
  ...

  We can implement these directly in Haskell using Expr

  Function LawN takes an expression:
    If it matches the "shape" of the law lefthand-side,
    it replaces it with the corresponding righthand "shape".
    If it does not match, it returns Nothing

    Implement Laws 1 through 4 above
-}


law1 :: Expr -> Maybe Expr
-- law1 (Add (Val x) (Val y)) = Just (Add (Val y) (Val x))
law1 (Add x y) = Just (Add y x)
law1 _ = Nothing

law2 :: Expr -> Maybe Expr
law2 (Add (Add x y) z) = Just (Add x (Add y z))
law2 _ = Nothing

law3 :: Expr -> Maybe Expr
law3 (Sub (Sub x y) z) = Just (Sub x (Add y z))
law3 _ = Nothing

law4 :: Expr -> Maybe Expr
law4 (Sub (Mul a b) (Mul c d)) | a == b && c == d = Just (Mul (Add a c) (Sub a c))
law4 (Mul (Add a c) (Add b d)) | a == b && c == d = Just (Sub (Mul a b) (Mul c d))
law4 _ = Nothing
