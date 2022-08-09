module Math.Basis
  ( BasisSig (..),
    WithBasis (..),
    withBasis,
  )
where

import Math.MV.Elem (Vec (..))
import Math.MV.Scalar (Con)
import Relude

data BasisSig = BasisSig
  { sigPos :: Word8,
    sigNeg :: Word8,
    sigZero :: Word8
  }
  deriving (Show)

class Monad m => WithBasis m where
  squareVec :: Vec -> m Con

type MVB = ReaderT BasisSig Maybe

instance WithBasis MVB where
  squareVec (Vec v) = do
    BasisSig p n z <- ask
    if
        | v < p -> pure 1
        | v < p + n -> pure (-1)
        | v < p + n + z -> pure 0
        | otherwise -> lift $ Nothing

withBasis :: BasisSig -> (forall m. WithBasis m => m a) -> Maybe a
withBasis bs mx = runReaderT mx bs
