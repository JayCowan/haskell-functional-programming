-- 1 
-- a
hof :: (a->b->b) -> b -> [a] -> b
hof _ m [] = m
hof pattern m (x:xs) = (pattern) x (hof (pattern) m xs)
-- b
f1 s ns = hof (*) s ns

f2 t bs = hof (||) t bs

f3 a xs = hof (+) (2*a) xs

f4 xs (ys:yss) = hof (concat) [ys, xs] yss

f5 m ns = hof (-) m ns
-- c
-- The foldr function in prelude mirrors hof, applying a function, in this case a prefix operation to a value and a list

--2 
-- a 
-- if the monad function lkp fails when making a call beval _ (B True) operation, resulting in a divide by zero error 
-- due to the (3 `div` 0) expression, which throws a runtime error

-- b 
-- One pattern matching failure that could occur in beval is the attempted evaluation of a Not proposition, for example, 
-- beval _ (Not someProp) as there is no pattern match for this case.
-- Another pattern matching failure that could occur in the beval function is the attempted evaluation of a B False prop
-- for example, beval _ (B False) would result in an error as it lacks defined behavior

-- c 
data Prop = B Bool
  | P String
  | Not Prop
  | And Prop Prop
  | Let String Prop Prop
  deriving Show
type Dict = [(String,Bool)]
ins :: String -> Bool -> Dict -> Dict
lkp :: (Monad m, MonadFail m) => String -> Dict -> m Bool

beval :: (Monad m, MonadFail m) => Dict -> Prop -> m Bool
beval d (Not p1) = return not beval d p1
beval d (And p1 p2) = return beval d p1 && beval d p2
beval d (P s) = do b <- lkp s d
                  if b == Nothing
                  then fail "key of lookup not in dictionary"
                  else return (getBool b)
beval d (Let v p1 p2) = beval (ins v b d) p2 where b = beval d p1
beval _ (B b) = b

getBool Nothing = False
getBool (Just x) = x
  
-- 3
-- a
-- case xs = []
-- len xs = 0
-- len xs ++ x = 1
-- len [x] = 1
-- b 
-- given len (xs++ys) = len xs + len ys
-- len rev [] = len [] = 0
-- len [a,b] = 2
-- rev [a,b] = rev [b] ++ a, 
-- len a = 1, 
-- rev [b] = [] ++ b -> len [] + 1, 
-- len rev [a, b] = len [] + 1 + 1 = 2
-- len [a,b] == len rev [a,b]
-- c 
changeMyFiles :: Handle -> Handle -> IO String
changeMyFiles input1 input2 =
  do
    eof <- isEOF input1 || isEOF input2
    if eof
      then
        return ""
      else
        do
          first <- getLine input1
          second <- getLine input2
          writeFile "output12.txt" putStrLn first
          writeFile "output12.txt" putStrLn second
          readMyFile input1 input2
main = do
  readMyFile (openFile "input1.txt" ReadMode) (openFile "input2.txt" ReadMode)
