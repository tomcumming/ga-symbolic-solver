module Math.MV.Scalar
  ( Sym,
    Con,
    Expr,
    scalar,
    sym,
    add,
    mul,
    showBraces,
  )
where

import Data.Map.Strict (assocs, filter, unionWith)
import Relude hiding (filter, show)
import Prelude (show)

-- | Symbol representing a scalar value
type Sym = Text

-- | Constant numerical value
type Con = Rational

-- | Product of symbols (aka x^2 * y)
newtype SymPows = SymPows (Map Sym Word8) deriving (Eq, Ord)

-- | A symbolic scalar expression
newtype Expr = Expr (Map SymPows Con) deriving (Eq, Ord)

simplify :: Expr -> Expr
simplify (Expr ss) = Expr $ filter (/= 0) ss

scalar :: Rational -> Expr
scalar n = simplify $ Expr $ one (SymPows mempty, n)

sym :: Sym -> Expr
sym x = Expr $ one (SymPows $ one (x, 1), 1)

add :: Expr -> Expr -> Expr
add (Expr ss1) (Expr ss2) = simplify $ Expr $ unionWith (+) ss1 ss2

mul :: Expr -> Expr -> Expr
mul (Expr lhsSs) (Expr rhsSs) = foldl' add (scalar 0) subExprs
  where
    subExprs :: [Expr]
    subExprs = do
      (lhsS, lhsC) <- assocs lhsSs
      (rhsS, rhsC) <- assocs rhsSs
      pure $ Expr $ one (mulSymPows lhsS rhsS, lhsC * rhsC)

mulSymPows :: SymPows -> SymPows -> SymPows
mulSymPows (SymPows ss1) (SymPows ss2) = SymPows $ unionWith (+) ss1 ss2

showBraces :: Expr -> Bool
showBraces (Expr ex) = length ex > 1

instance Show SymPows where
  show (SymPows ss) =
    assocs ss
      & map (\(x, p) -> toString x <> if p == 1 then "" else "^" <> show p)
      & intersperse " * "
      & fold

instance Show Expr where
  show (Expr e) =
    assocs e
      & map showSymPow
      & intersperse " + "
      & fold
    where
      showSymPow :: (SymPows, Con) -> String
      showSymPow (SymPows ss, c)
        | null ss = cStr
        | length ss == 1 = (cMul <> show (SymPows ss))
        | otherwise = (cMul <> "(" <> show (SymPows ss) <> ")")
        where
          cStr = show (fromRational c :: Float)
          cMul
            | c == 1 = ""
            | otherwise = cStr <> " * "
