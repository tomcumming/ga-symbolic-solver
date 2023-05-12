module Symbolic.Scalar
  ( Power,
    Scalar,
    fromRat,
    var,
  )
where

import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Map qualified as M
import Data.Ratio (denominator, numerator)
import GHC.Show (appPrec)

type Power = Int

newtype Products v = Products (M.Map v Power) deriving (Eq, Ord)

newtype Scalar v = Scalar (M.Map (Products v) Rational) deriving (Eq, Ord)

instance Show v => Show (Products v) where
  showsPrec d (Products ps)
    | M.null ps = showsPrec d (1 :: Int)
    | otherwise =
        showParen (d > appPrec) $
          showString (fold $ intersperse " * " $ map (uncurry showPair) $ M.toAscList ps)
    where
      showPair v power
        | power == 1 = show v
        | otherwise = fold [show v, "^", show power]

instance (Show v, Ord v) => Show (Scalar v) where
  showsPrec d (Scalar ss)
    | M.null ss = showsPrec d (0 :: Int)
    | otherwise =
        showParen (d > appPrec) $
          showString (fold $ intersperse " + " $ map (uncurry showPair) $ M.toAscList ss)
    where
      showPair ps r =
        let rs = if denominator r == 1 then show (numerator r) else show r
            pss = showsPrec d ps ""
         in if
                | ps == Products mempty -> rs
                | r == 1 -> pss
                | denominator r == 1 -> rs <> " * " <> pss
                | otherwise -> fold ["(" <> show r <> ") * ", pss]

fromPowers :: M.Map v Power -> Products v
fromPowers = Products . M.filter (/= 0)

fromProducts :: M.Map (Products v) Rational -> Scalar v
fromProducts = Scalar . M.filter (/= 0)

fromRat :: Ord v => Rational -> Scalar v
fromRat = fromProducts . M.singleton (Products mempty)

var :: v -> Scalar v
var v = fromProducts $ M.singleton (fromPowers $ M.singleton v 1) 1

mulProducts :: Ord v => Products v -> Products v -> Products v
mulProducts (Products l) (Products r) = Products $ M.unionWith (+) l r

instance Ord v => Num (Scalar v) where
  Scalar l + Scalar r = Scalar $ M.unionWith (+) l r
  Scalar l * Scalar r = fromProducts . M.fromListWith (+) $ do
    (lp, lr) <- M.toList l
    (rp, rr) <- M.toList r
    pure (mulProducts lp rp, lr * rr)
  abs = error "Scalar abs"
  signum = error "Scalar signum"
  fromInteger = fromProducts . M.singleton (fromPowers mempty) . fromInteger
  negate = (* fromInteger (-1))
