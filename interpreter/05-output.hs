import Control.Monad

type Name = String

data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Out Term

data Value = Wrong
           | Num Int
           | Fun (Value -> O Value)

type Environment = [(Name, Value)]

interp :: Term -> Environment -> O Value
interp (Var x) e   = lookup' x e
interp (Con c) _   = return (Num c)
interp (Add t u) e = do x <- interp t e
                        y <- interp u e
                        add x y
interp (Lam x t) e = return $ Fun $ \a -> interp t ((x, a):e)
interp (App t u) e = do f <- interp t e
                        x <- interp u e
                        apply f x
interp (Out t) e   = do x <- interp t e
                        outO x
                        return x

lookup' :: Name -> Environment -> O Value
lookup' _ []         = return Wrong
lookup' x ((n, v):e) = if x == n then return v
                                 else lookup' x e

add :: Value -> Value -> O Value
add (Num x) (Num y) = return $ Num (x + y)
add _ _ = return Wrong

apply :: Value -> Value -> O Value
apply (Fun f) a = f a
apply _ _ = return Wrong

test :: Term -> O Value
test t = interp t []

showVal :: Value -> String
showVal Wrong   = "<wrong>"
showVal (Num i) = show i
showVal (Fun f) = "<function>"

-------------

newtype O a = O (String, a)

instance Monad O where
  return a = O ("", a)
  O (s, a) >>= k = let O (t, b) = k a in O (s ++ t, b)

instance Functor O where
  fmap = liftM

instance Applicative O where
  pure = return
  (<*>) = ap

showO :: O Value -> String
showO (O (s, a)) = "Output: " ++ s ++ " Value: " ++ showVal a

outO :: Value -> O ()
outO x = O (showVal x ++ "; ", ())

-------------

term0 = Out(App (Out(Lam "x" (Out((Add (Var "x") (Var "x")))))) (Out((Add (Out(Con 10)) (Out (Con 11))))))
t1 = showO $ test term0
