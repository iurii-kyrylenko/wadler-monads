import Control.Monad

data Term = Con Int | Div Term Term
            deriving Show

eval :: Term -> Int
eval (Con a)   = a
eval (Div t u) = eval t `div` eval u

answer :: Term
answer = (Div (Div (Con 1972 ) (Con 2 )) (Con 23 ))

t1 = eval answer

----------------

newtype M a = Out (Output, a)
type Output = String

instance Monad M where
  return a = Out ("", a)
  Out (x, a) >>= k = let Out (y, b) = k a
                     -- in  Out (x ++ y, b)
                     in  Out (y ++ x, b)

instance Functor M where
  fmap = liftM

instance Applicative M where
  pure = return
  (<*>) = ap

line :: (Show a) => Term -> a -> Output
line x a = "eval(" ++ show x ++ ") <= " ++ show a ++ "\n"

out :: Output -> M ()
out s = Out (s, ())

outLine :: (Show a) => Term -> a -> M ()
outLine x a = out $ line x a

outLine' :: (Show a) => Term -> a -> M a
outLine' x a = Out (line x a, a)

runOutput :: M a -> Output
runOutput (Out (x, _)) = x

evalM :: Term -> M Int
evalM t@(Con a)   = do outLine t a
                       return a
evalM v@(Div t u) = do a <- evalM t
                       b <- evalM u
                       let res = a `div` b
                       outLine v res
                       return res

evalM' :: Term -> M Int
evalM' t@(Con a)   = outLine' t a
evalM' v@(Div t u) = do a <- evalM' t
                        b <- evalM' u
                        outLine' v $ a `div` b

t2 = putStr . runOutput . evalM $ answer
t3 = putStr . runOutput . evalM' $ answer
