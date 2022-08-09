module Math.MV
  ( Vec,
    Elem,
    MV,
    sym,
    scalar,
    el,
    add,
    mul,
  )
where

import Data.Map.Strict (assocs, filter, unionWith)
import Data.Set (size, splitMember)
import Math.Basis (WithBasis (..))
import Math.MV.Elem (Elem (..), Vec (..))
import Math.MV.Scalar (Con, Expr, Sym)
import qualified Math.MV.Scalar as Scalar
import Relude hiding (filter, show)
import Prelude (show)

newtype MV = MV (Map Elem Expr) deriving (Eq, Ord)

simplify :: MV -> MV
simplify (MV ms) = MV $ filter (/= Scalar.scalar 0) ms

sym :: Sym -> MV
sym x = MV $ one (Elem mempty, Scalar.sym x)

scalar :: Con -> MV
scalar n = simplify $ MV $ one (Elem mempty, Scalar.scalar n)

el :: (WithBasis m, Foldable f) => f Word8 -> m MV
el =
  foldlM
    (\mv v -> mul mv (MV $ one (Elem $ one $ Vec v, Scalar.scalar 1)))
    (scalar 1)

add :: MV -> MV -> MV
add (MV ms1) (MV ms2) = simplify $ MV $ unionWith Scalar.add ms1 ms2

mul :: WithBasis m => MV -> MV -> m MV
mul (MV lhs) (MV rhs) = do
  wtfs <- sequence $ do
    (lhsEl, lhsEx) <- assocs lhs
    (rhsEl, rhsEx) <- assocs rhs
    pure $ do
      (sign, elm) <- mulElem lhsEl rhsEl
      let s = Scalar.mul (Scalar.scalar sign) $ Scalar.mul lhsEx rhsEx
      pure $ MV $ one (elm, s)
  pure $ foldl' add (scalar 0) wtfs

mulElem :: forall m. WithBasis m => Elem -> Elem -> m (Con, Elem)
mulElem lhsElm (Elem es2) = foldlM go (1, lhsElm) es2
  where
    go :: (Con, Elem) -> Vec -> m (Con, Elem)
    go (c, Elem lhs) v = case splitMember v lhs of
      (lower, memb, higher) -> do
        let flp = if even (size higher) then 1 else -1
        if memb
          then do
            sq <- squareVec v
            pure (flp * sq * c, Elem $ lower <> higher)
          else pure (flp * c, Elem $ lower <> one v <> higher)

showMV :: MV -> [String]
showMV (MV ms)
  | null ms = ["0"]
  | otherwise = showPart <$> assocs ms
  where
    showPart :: (Elem, Expr) -> String
    showPart (Elem elm, ex)
      | null elm = show ex
      | ex == Scalar.scalar 1 = show (Elem elm)
      | Scalar.showBraces ex = "(" <> show ex <> ") * " <> show (Elem elm)
      | otherwise = show ex <> " * " <> show (Elem elm)

instance Show MV where
  show mv =
    showMV mv
      & intersperse " + "
      & fold
