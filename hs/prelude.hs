import Prelude ((.), ($), (++), id)
import System.IO

-- -- Summary:
-- fmap   :: (a -> b) -> m a   -> m b     -- Map function over container
-- (>>=)  :: m a -> (a -> m b) -> m b     -- Map function over container
-- join   :: m (m a)           -> m a     -- Reduces   container "dimension"
-- return :: a                 -> m a     -- Increases container "dimension"
--
-- -- These may be defined in terms of each other:
-- fmap   f xs = xs >>= (return . f)
-- (>>=)  xs f = (join . fmap) f xs
-- join   xss  = xss >>= id
-- return x    = (undefinable)
--
-- -- Monad Laws:
-- return x   >>= f       ≡  f x
-- xs         >>= return  ≡  xs
-- (xs >>= f) >>= g       ≡  xs >>= (\x -> f x >>= g)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class (Functor m) => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Maybe a = Just a | Nothing

instance Functor Maybe where
  fmap f x = x >>= (return . f)

instance Monad Maybe where
  return = Just
  (>>=) (Just x) f = f x
  (>>=) Nothing  f = Nothing

maybe :: b -> (a -> b) -> Maybe a -> b
maybe d f (Just x) = f x
maybe d f Nothing  = d

-- Implement List, Applicatives, ...
-- Monads as containers, computation

main :: IO ()
main = do
  let f x = x ++ " there!"
      putMaybeStrLn x = putStrLn $ maybe "" id x
  putMaybeStrLn $ fmap f (Just "Hello")
  putMaybeStrLn $ fmap f Nothing
  putMaybeStrLn $ ((Just "Over") >>= (Just . f))
