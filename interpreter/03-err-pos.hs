import Control.Monad

type Name = String

data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | At Position Term

data Value = Wrong
           | Num Int
           | Fun (Value -> P Value)

type Environment = [(Name, Value)]

interp :: Term -> Environment -> P Value
interp (Var x) e   = lookup' x e
interp (Con c) _   = return (Num c)
interp (Add t u) e = do x <- interp t e
                        y <- interp u e
                        add x y
interp (Lam x t) e = return $ Fun $ \a -> interp t ((x, a):e)
interp (App t u) e = do f <- interp t e
                        x <- interp u e
                        apply f x
interp (At p t) e  = resetP p $ interp t e

lookup' :: Name -> Environment -> P Value
lookup' x []         = errorP $ "unboun variable: " ++ x
lookup' x ((n, v):e) = if x == n then return v
                                 else lookup' x e

add :: Value -> Value -> P Value
add (Num x) (Num y) = return $ Num (x + y)
add x y = errorP $ "not a number:" ++ showVal x ++ "," ++ showVal y

apply :: Value -> Value -> P Value
apply (Fun f) a = f a
apply x _ = errorP $ "not a function: " ++ showVal x

test :: Term -> P Value
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

errorE :: String -> E a
errorE s = Error s

showTest :: Term -> IO ()
showTest = putStrLn . showP . test

-------------

type Position = Int
newtype P a = P (Position -> E a)

instance Monad P where
  return a = P $ \p -> return a
  (P t) >>= k = P $ \p -> t p >>= \a -> let P u = k a in u p

instance Functor P where
  fmap = liftM

instance Applicative P where
  pure = return
  (<*>) = ap

errorP :: String -> P a
errorP s = P $ \p -> errorE $ s ++ " [at: " ++ show p ++ "]"

pos0 :: Position
pos0 = 0

showP :: P Value -> String
showP (P t) = showE $ t pos0

resetP :: Position -> P a -> P a
resetP p (P t) = P $ \q -> t p

-------------

term1 = App (Lam "x" (Add (Var "x") (Var "x")))(Add (Con 10) (Con 11))
term2 = At 1 (App (Con 1) (Con 2))
term3 = App (Lam "x" (Add (Var "x") (At 2 (Var "y"))))(Add (Con 10) (Con 11))

t1 = showTest term1
t2 = showTest term2
t3 = showTest term3
