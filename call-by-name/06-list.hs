import Control.Monad

type Name = String

data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Fail
          | Amb Term Term

data Value = Wrong
           | Num Int
           | Fun (L Value -> L Value)

type Environment = [(Name, L Value)]

interp :: Term -> Environment -> L Value
interp (Var x) e   = lookup' x e
interp (Con c) _   = return (Num c)
interp (Add t u) e = do x <- interp t e
                        y <- interp u e
                        add x y
interp (Lam x t) e = return $ Fun $ \a -> interp t ((x, a):e)
interp (App t u) e = do f <- interp t e
                        let x = interp u e
                        apply f x
interp Fail _ = zeroL
interp (Amb t u) e = plusL (interp t e) (interp u e)

lookup' :: Name -> Environment -> L Value
lookup' _ []         = return Wrong
lookup' x ((n, v):e) = if x == n then v
                                 else lookup' x e

add :: Value -> Value -> L Value
add (Num x) (Num y) = return $ Num (x + y)
add _ _ = return Wrong

apply :: Value -> L Value -> L Value
apply (Fun f) a = f a
apply _ _ = return Wrong

test :: Term -> L Value
test t = interp t []

showVal :: Value -> String
showVal Wrong   = "<wrong>"
showVal (Num i) = show i
showVal (Fun f) = "<function>"

-------------

type L a = [a]

showL :: L Value -> String
showL m = show $ do a <- m
                    return $ showVal a

zeroL :: L a
zeroL = []

plusL :: L a -> L a -> L a
l `plusL` r = l ++ r

term1 = (App (Lam "x" (Add (Var "x") (Var "x"))) (Amb (Con 1) (Con 2)))
term2 = App (Lam "x" (Add (Var "x") (Var "x")))(Add (Con 10) (Amb (Con 11) (Con 12)))
t1 = putStrLn . showL . test $ term1
t2 = putStrLn . showL . test $ term2
