{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Polynomial.NonEmpty (PolynomialNE) where

import Control.Exception
-- import Data.Char
import Data.List          qualified as L
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as LNE
import Polynomial.Class
import Polynomial.Error
import Polynomial.Internal
import Polynomial.Pretty

newtype PolynomialNE a = PolynomialNE {
    getPolynomialNE :: NonEmpty a
} deriving stock (Eq, Show)

instance Num a => Num (PolynomialNE a) where
    PolynomialNE (a :| _as) + PolynomialNE (b :| _bs) = PolynomialNE $ (a + b) :| undefined -- (zipWith (+) as bs)
    PolynomialNE xs * PolynomialNE ys = PolynomialNE $ fmap sum $ nediagonals $ fmap (\x' -> fmap (\y' -> y' * x') ys) xs
    abs (PolynomialNE xs) = PolynomialNE (fmap abs xs)
    signum (PolynomialNE xs) = PolynomialNE (fmap signum xs)
    fromInteger integer = PolynomialNE (fromInteger integer :| [])
    negate (PolynomialNE xs) = PolynomialNE (fmap negate xs)

instance (Num a) => IsPolynomial (PolynomialNE a) where
    x = PolynomialNE (LNE.fromList [0, 1])

instance (Num a, Eq a) => FromNumList PolynomialNE a where
    fromNumList ∷ [a] → PolynomialNE a
    fromNumList xs
        | null xs = throw EmptyListException
        | all (== 0) xs = throw AllZeroException
        | otherwise = PolynomialNE . LNE.fromList $ xs

instance (Num a, Eq a) => FromNumNonEmpty PolynomialNE a where
    fromNumNonEmpty ∷ NonEmpty a → PolynomialNE a
    fromNumNonEmpty xs
        | all (== 0) xs = throw AllZeroException
        | otherwise = PolynomialNE $ xs

instance (Num a, Eq a) => FromNumListEither PolynomialNE a where
    fromNumListEither ∷ [a] → Either PolynomialError (PolynomialNE a)
    fromNumListEither xs
        | null xs = Left EmptyListException
        | all (== 0) xs = Left AllZeroException
        | otherwise = Right . PolynomialNE . LNE.fromList $ xs

instance (Num a, Eq a) => FromNumNonEmptyEither PolynomialNE a where
    fromNumNonEmptyEither ∷ NonEmpty a → Either PolynomialError (PolynomialNE a)
    fromNumNonEmptyEither xs
        | all (== 0) xs = Left AllZeroException
        | otherwise = Right . PolynomialNE $ xs

instance (PrettyNum a, Num a, Eq a) => PrettyPoly (PolynomialNE a) where
    prettyPoly ∷ PrettyPolyOptions -> PolynomialNE a → String
    prettyPoly PrettyPolyOptions { termOrder, displayXFn, displayPowerFn } = fixLeadingSign
        . L.unwords
        . termOrder
        . L.reverse
        . fmap (renderTerm displayXFn displayPowerFn)
        . LNE.filter removeNullCoefficient
        . LNE.zip (LNE.fromList [0..])
        . getPolynomialNE

instance Num a => EvalPoly PolynomialNE a where
    evalPolyAt at poly = sum $ fmap (\(i, val) -> val * at ^ (i :: Integer)) $ LNE.zip (LNE.fromList [0..]) (getPolynomialNE poly)