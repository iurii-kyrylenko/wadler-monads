import Control.Monad

type Name = String

data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term

data Value = Wrong
           | Num Int
           | Fun (Value -> E Value)

type Environment = [(Name, Value)]

interp :: Term -> Environment -> E Value
interp (Var x) e   = lookup' x e
interp (Con c) _   = return (Num c)
interp (Add t u) e = do x <- interp t e
                        y <- interp u e
                        add x y
interp (Lam x t) e = return $ Fun $ \a -> interp t ((x, a):e)
interp (App t u) e = do f <- interp t e
                        x <- interp u e
                        apply f x

lookup' :: Name -> Environment -> E Value
lookup' x []         = error' $ "unboun variable:" ++ x
lookup' x ((n, v):e) = if x == n then return v
                                 else lookup' x e

add :: Value -> Value -> E Value
add (Num x) (Num y) = return $ Num (x + y)
add x y = error' $ "not a number:" ++ showVal x ++ "," ++ showVal y

apply :: Value -> Value -> E Value
apply (Fun f) a = f a
apply x _ = error' $ "not a function: " ++ showVal x

test :: Term -> E Value
test t = interp t []

showVal :: Value -> String
showVal Wrong   = "<wrong>"
showVal (Num i) = show i
showVal (Fun f) = "<function>"

-------------

data E a = Success a | Error String

instance Monad E where
  return a = Success a
  (Success a) >>= k = k a
  (Error s)   >>= _ = Error s

instance Functor E where
  fmap = liftM

instance Applicative E where
  pure = return
  (<*>) = ap

showE :: E Value -> String
showE (Success a) = "Success: " ++ showVal a
showE (Error s)   = "Error: " ++ show s

error' :: String -> E a
error' s = Error s

showTest :: Term -> IO ()
showTest = putStrLn . showE . test

-------------

term1 = App (Lam "x" (Add (Var "x") (Var "x")))(Add (Con 10) (Con 11))
term2 = App (Con 1) (Con 2)
t1 = showTest term1
t2 = showTest term2
