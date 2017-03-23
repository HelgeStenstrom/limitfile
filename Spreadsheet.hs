module Spreadsheet
where



type Origin = String
data CellWithOrigin  t = CellWithOrigin (Origin, t) deriving Show


instance Functor CellWithOrigin
  where
  fmap f (CellWithOrigin (origin, a))  = CellWithOrigin (origin, (f a))
--   (<$) a (CellWithOrigin (origin, b) ) = CellWithOrigin (origin, a)


instance Applicative CellWithOrigin
  where
  pure a =  CellWithOrigin ("unknown", a)
  (<*>) (CellWithOrigin (o1, f))  (CellWithOrigin (o2, x)) =  CellWithOrigin (o2, (f x) )


instance Monad CellWithOrigin where
  (>>=) (CellWithOrigin (o, a)) f = f a


