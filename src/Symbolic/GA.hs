module Symbolic.GA
  ( Basis (..),
    Elem,
    MV,
    GA,
    fromRat,
    var,
    vec,
    revers,
    dual,
    undual,
    calculate,
  )
where

import Control.Monad (join)
import Data.Foldable (fold, foldlM)
import Data.List (intersperse)
import Data.Map qualified as M
import Data.Set qualified as S
import GHC.Show (appPrec)
import Symbolic.Scalar (Scalar)
import Symbolic.Scalar qualified as Sc

newtype Elem b = Elem (S.Set b) deriving (Eq, Ord)

newtype MV b v = MV (M.Map (Elem b) (Scalar v)) deriving (Eq, Ord)

data Basis b = Basis
  { basSquare :: b -> Rational,
    -- | Pseudoscalar, aka all basis vectors
    basPss :: S.Set b
  }

newtype GAM b a = GAM (Basis b -> a) deriving (Functor, Applicative, Monad)

type GA b v = GAM b (MV b v)

instance Show (Elem b) where
  showsPrec = undefined

instance (Show b, Show v, Ord v) => Show (MV b v) where
  show (MV mv)
    | M.null mv = show (0 :: Int)
    | otherwise = fold . intersperse " + " . map (uncurry showElem) $ M.toList mv
    where
      showElem :: Elem b -> Scalar v -> String
      showElem (Elem es) s
        | S.null es = show s
        | s == 1 = esStr
        | otherwise = showsPrec appPrec s "" <> " * " <> esStr
        where
          esStr =
            fold
              . intersperse " * "
              . map show
              $ S.toList es

squareBasis :: b -> GAM b Rational
squareBasis b = GAM $ \Basis {basSquare} -> basSquare b

basisVectors :: GAM b (S.Set b)
basisVectors = GAM $ \Basis {basPss} -> basPss

mulElem :: forall b. Ord b => Elem b -> Elem b -> GAM b (Rational, Elem b)
mulElem origL (Elem origR) = foldlM go (1, origL) origR
  where
    go :: (Rational, Elem b) -> b -> GAM b (Rational, Elem b)
    go (m, Elem l) r = do
      let (_, mem, lr) = S.splitMember r l
      sq <- if mem then squareBasis r else pure 1
      let flipSign = if odd (S.size lr) then (-1) else 1
      let l' = if mem then S.delete r l else S.insert r l
      pure (m * flipSign * sq, Elem l')

fromElems :: (Ord v, Ord b) => M.Map (Elem b) (Scalar v) -> MV b v
fromElems = MV . M.filter (/= 0)

fromScalar :: (Ord b, Ord v) => Scalar v -> MV b v
fromScalar = fromElems . M.singleton (Elem mempty)

mul :: (Ord b, Ord v) => MV b v -> MV b v -> GA b v
mul (MV l) (MV r) = fmap (fromElems . M.fromList) . sequence $ do
  (lb, lv) <- M.toList l
  (rb, rv) <- M.toList r
  [ do
      (r', b') <- mulElem lb rb
      pure (b', lv * rv * Sc.fromRat r')
    ]

fromRat :: (Ord b, Ord v) => Rational -> GA b v
fromRat = pure . fromElems . M.singleton (Elem mempty) . Sc.fromRat

var :: (Ord b, Ord v) => v -> GA b v
var = pure . fromScalar . Sc.var

vec :: (Ord v, Ord b) => b -> GA b v
vec = pure . fromElems . (`M.singleton` 1) . Elem . S.singleton

revers :: Ord v => GA b v -> GA b v
revers = fmap $ \(MV mv) -> MV $ M.mapWithKey go mv
  where
    go (Elem bs) s = s * if S.size bs `mod` 4 > 1 then -1 else 1

-- TODO there must be a smarter way
dual' ::
  forall b v.
  (Ord v, Ord b) =>
  (S.Set b -> Maybe (b, S.Set b)) ->
  ((S.Set b, S.Set b) -> S.Set b) ->
  GA b v ->
  GA b v
dual' view side ga = do
  vs <- basisVectors
  fmap
    ( \(MV mv) ->
        fromElems
          . M.fromListWith (+)
          . map (uncurry (elemDual vs))
          $ M.toList mv
    )
    ga
  where
    elemDual :: S.Set b -> Elem b -> Scalar v -> (Elem b, Scalar v)
    elemDual vs (Elem bs) s = case view bs of
      Nothing -> (Elem vs, s)
      Just (b, bs') ->
        let flips = if odd . S.size $ side (S.split b vs) then -1 else 1
         in elemDual (S.delete b vs) (Elem bs') (s * flips)

dual :: forall b v. (Ord v, Ord b) => GA b v -> GA b v
dual = dual' S.minView fst

undual :: forall b v. (Ord v, Ord b) => GA b v -> GA b v
undual = dual' S.maxView snd

calculate :: Basis b -> GA b v -> MV b v
calculate bas (GAM ga) = ga bas

instance (Ord v, Ord b) => Num (GA b v) where
  lga + rga = do
    MV lmv <- lga
    MV rmv <- rga
    pure . fromElems $ M.unionWith (+) lmv rmv
  lga * rga = join $ mul <$> lga <*> rga
  abs = error "GA abs"
  signum = error "GA signum"
  fromInteger = pure . fromElems . M.singleton (Elem mempty) . fromInteger
  negate = (* fromInteger (-1))
