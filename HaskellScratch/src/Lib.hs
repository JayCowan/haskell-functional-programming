module Lib
    ( someFunc
    ) where
import Prelude
import GHC.IO.Handle.Types (Handle)



someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Higher order functions
hof :: b -> (a->b->b) -> [a] -> b
hof m _ [] = m
hof m common (x:xs) = (common) x (hof m common xs)
f1 (n:ns) = hof 0 + (n:ns)

f2 (n:ns) = hof 1 * (n:ns)

f3 (x:xs) = hof 0 (2*) (x:xs)

f4 (ys:yss) = hof [] ++ (ys:yss)

f5 (c:cs) = hof [] toUpper (c:cs)

-- exists in prelude as foldr

-- Monads and expr
-- if there are 2 identical strings in the dictionary, then on lookup, the value would fail, resulting in eval failing by extension
-- if there is an eval _ (K _/=0) then the function will fail as there is no pattern match for any K other than 0.
--  if there is any attempt at at division by zero a runtime exception will occur and the function will fail

data Expr = V String
  | K Int
  | Let String Expr Expr
  | Sub Expr Expr
  | Dvd Expr Expr
type Dict = [(String,Int)]
ins :: (String k, Int n, Monad m) => Dict d k -> k -> m d
ins [] key val = [(key, val)]


lkp :: (String k, Monad m) => Dict d k -> k -> m d
lkp [] key = fail ( "mfind - Couldn’t find: " ++ show key )
lkp ((k,v):kds) key
  | key == k = return v
  | otherwise = lkp kds key

{-class Monad lkp where
  (>>=) :: m Dict d -> (d -> d[(String s, Int n)] -> n ) -> n
  return :: Int -> m n
  fail :: String -> m s-}
eval :: Dict -> Expr -> Either Int MonadFail
eval _ (K n) = n
eval d (V s) = getInt $ lkp s d
eval d (Let v e1 e2) = eval (ins d v i) e2 where i = eval d e1


-- IO



-- Exam