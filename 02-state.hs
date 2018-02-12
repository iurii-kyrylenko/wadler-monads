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

data M a = S (State -> (a, State))
type State = Int

instance Monad M where
  return a = S $ \s -> (a, s)
  (S m) >>= k = S $ \s -> let (a, x) = m s
                              S t    = k a
                          in  t x

instance Functor M where
  fmap = liftM

instance Applicative M where
  pure = return
  (<*>) = ap

runState :: M a -> State -> (a, State)
runState (S t) s = t s 

evalM :: Term -> M Int
evalM (Con a) = return a
evalM (Div t u) = do a <- evalM t
                     b <- evalM u
                     S $ \s -> ((), s + 1)
                     return (a `div` b)

t2 = runState (evalM answer) 0
