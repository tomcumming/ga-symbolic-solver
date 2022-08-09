module Math.MV.Elem
  ( Vec(..),
    Elem(..),
  )
where

import Relude hiding (show)
import Prelude (show)

-- | Basis vector (aka e1)
newtype Vec = Vec Word8 deriving (Eq, Ord)

-- | Element (aka e12)
newtype Elem = Elem (Set Vec) deriving (Eq, Ord)

instance Show Elem where
  show (Elem vs)
    | null vs = ""
    | otherwise = "e" <> fold (map (\(Vec v) -> show v) $ toList vs)
