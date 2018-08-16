import Prelude ((.), ($), (++), id)
import System.IO

class Functor f where
  fmap :: (a -> b) -> f a -> f b

class (Functor m) => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  -- join :: m (m a) -> m a

data Maybe a = Just a | Nothing

instance Functor Maybe where
  fmap fn (Just x) = Just $ fn x
  fmap fn Nothing  = Nothing

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
