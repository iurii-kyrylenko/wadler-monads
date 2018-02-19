import Control.Monad

type Name = String

data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term

data Value = Wrong
           | Num Int
           | Fun (Value -> M Value)

type Environment = [(Name, Value)]

interp :: Term -> Environment -> M Value
interp (Var x) e   = lookup' x e
interp (Con c) _   = return (Num c)
interp (Add t u) e = do x <- interp t e
                        y <- interp u e
                        add x y
interp (Lam x t) e = return $ Fun $ \a -> interp t ((x, a):e)

lookup' :: Name -> Environment -> M Value
lookup' _ []         = return Wrong
lookup' x ((n, v):e) = if x == n then return v
                                 else lookup' x e

add :: Value -> Value -> M Value
add (Num x) (Num y) = return $ Num (x + y)
add _ _ = return Wrong

showVal :: Value -> String
showVal Wrong   = "<wrong>"
showVal (Num i) = show i
showVal (Fun f) = "<function>"

-------------

newtype M a = I a

instance Monad M where
  return a = I a
  (I a) >>= k  = k a

instance Functor M where
  fmap = liftM

instance Applicative M where
  pure = return
  (<*>) = ap

showM :: M Value -> String
showM (I a) = showVal a
