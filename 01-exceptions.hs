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

eval' :: Term -> M Int
eval' (Con a) = return a
eval' (Div t u) = eval' t >>= \a -> eval' u >>= \b -> unit a b
                  where unit a b = if b == 0
                                    then Raise "divide by zero"
                                    else return (a `div` b)

t1' = eval' answer
t2' = eval' err
