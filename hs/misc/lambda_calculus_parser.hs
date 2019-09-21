-- Inspired by "The essence of functional programming" by Philip Wadler

-- TODO write one without looking... and without monads...
-- start with simple expressions

-- type M a = a
-- unitM a = a
-- a >>= k = k a
-- --showM a = showval a
--
-- type Name = String
--
-- data Term = Var Name
--   | Con Int
--   | Add Term Term
--   | Lam Name Term
--   | App Term Term
--
-- data Value = Wrong
--   | Num Int
--   | Fun (Value -> M Value)
--
-- type Environment = [(Name, Value)]
--
-- interp :: Term -> Environment -> Value
-- interp (Var e) = lookup x e
-- interp ...
--
-- term0 = (App
--   (Lam "x" (Add (Var "x") (Var "x")))
--   (Add (Con 10) (Con 11)))

type Name = String

-- rename Term as Expr?
data Term = Con Int
  | Add Term Term
  | Var Name
  | Lam Name Term
  | App Term Term
  deriving Show

data Value = Wrong
  | Num Int
  | Fun (Value -> Value)
  deriving Show

interp :: Term -> Value
interp (Con x)   = Num x
interp (Add x y) = add (interp x) (interp y)
interp (App f x) = (interp f) (interp x)
interp (Lam x t) = Fun (\x -> interp t x)  -- TODO wut

add :: Value -> Value -> Value
add (Num x) (Num y) = Num (x + y)

-- term0 = Add (Con 3) (Con 4)

term0 = (App
  (Lam "x" (Add (Var "x") (Var "x")))
  (Add (Con 10) (Con 11)))

main :: IO ()
main = do
  print $ show term0
  print $ show $ interp term0

