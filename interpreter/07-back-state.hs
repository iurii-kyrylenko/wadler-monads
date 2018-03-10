import Control.Monad

type Name = String

data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Count

data Value = Wrong
           | Num Int
           | Fun (Value -> S Value)

type Environment = [(Name, Value)]

interp :: Term -> Environment -> S Value
interp (Var x) e   = lookup' x e
interp (Con c) _   = return (Num c)
interp (Add t u) e = do x <- interp t e
                        y <- interp u e
                        add x y
interp (Lam x t) e = return $ Fun $ \a -> interp t ((x, a):e)
interp (App t u) e = do f <- interp t e
                        x <- interp u e
                        apply f x
interp Count _ = do count <- fetchS
                    return $ Num count

lookup' :: Name -> Environment -> S Value
lookup' _ []         = return Wrong
lookup' x ((n, v):e) = if x == n then return v
                                 else lookup' x e

add :: Value -> Value -> S Value
add (Num x) (Num y) = do tickS
                         return $ Num (x + y)
-- add (Num x) (Num y) = tickS' . return . Num $ (x + y)
add _ _ = return Wrong

apply :: Value -> Value -> S Value
apply (Fun f) a = do tickS
                     f a
-- apply (Fun f) a = tickS' . f $ a
apply _ _ = return Wrong

test :: Term -> S Value
test t = interp t []

showVal :: Value -> String
showVal Wrong   = "<wrong>"
showVal (Num i) = show i
showVal (Fun f) = "<function>"

-------------

type State = Int

newtype S a = S (State -> (a, State))

instance Monad S where
  return a = S $ \s -> (a, s)
  -- (S t) >>= k  = S $ \s0 -> let (a, s1) = t s0
  --                               S u     = k a
  --                               (b, s2) = u s1
  --                           in  (b, s2)
  (S t) >>= k  = S $ \s2 -> let (a, s0) = t s1
                                S u     = k a
                                (b, s1) = u s2
                            in  (b, s0)

instance Functor S where
  fmap = liftM

instance Applicative S where
  pure = return
  (<*>) = ap

tickS' :: S a -> S a
tickS' (S t) = S $ \s -> let (a, s2) = t s in (a, s2 + 1)

tickS :: S ()
tickS = S $ \s -> ((), s + 1)

showS :: S Value -> State -> String
showS (S t) s = let (a, s2) = t s in "Result: " ++ showVal a ++ " Reduces: " ++ show s2

fetchS :: S State
fetchS = S $ \s -> (s, s)

-------------

term1 = App (Lam "x" (Add (Var "x") (Var "x")))(Add (Con 10) (Con 11))
term2 = Add (Add (Con 1) (Con 2)) Count
term3 = Add (Add (Add (Con 1) (Con 2)) Count) Count
t1 = showS (test term1) 0
t2 = showS (test term2) 0
t3 = showS (test term3) 0
