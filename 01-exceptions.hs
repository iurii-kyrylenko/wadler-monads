import Control.Monad

data Term = Con Int | Div Term Term
            deriving Show

eval :: Term -> Int
eval (Con a)   = a
eval (Div t u) = eval t `div` eval u

answer,err :: Term
answer = (Div (Div (Con 1972 ) (Con 2 )) (Con 23 ))
err = (Div (Con 1)(Con 0))

t1 = eval answer
t2 = eval err

----------------

data M a = Raise Exception | Return a deriving Show
type Exception = String

instance Monad M where
  return = Return
  m >>= k = case m of
              Raise e -> Raise e
              Return a -> k a

instance Functor M where
  fmap = liftM

instance Applicative M where
  pure = return
  (<*>) = ap

evalM :: Term -> M Int
evalM (Con a) = return a
-- evalM (Div t u) = evalM t >>= \a -> evalM u >>= \b -> unit a b
--                   where unit a b = if b == 0
--                                     then Raise "divide by zero"
--                                     else return (a `div` b)
evalM (Div t u) = do a <- evalM t
                     b <- evalM u
                     if b == 0 then Raise "divide by zero"
                               else return (a `div` b)

t3 = evalM answer
t4 = evalM err
