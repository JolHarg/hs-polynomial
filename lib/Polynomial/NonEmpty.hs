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
    -- >>> pretty <$> fromIntList [1,2,3]
    -- Right "1 + 2x + 3x\178"
    --
    fromNumList ∷ [a] → PolynomialNE a
    fromNumList xs
        | null xs = throw EmptyListException
        | all (== 0) xs = throw AllZeroException
        | otherwise = PolynomialNE . LNE.fromList $ xs

    -- >>> pretty <$> (fromNonEmpty . fromList $ [1,2,3,4])
    -- Right "1 + 2x + 3x\178 + 4x\179"
    --
    fromNumNonEmpty ∷ NonEmpty a → PolynomialNE a
    fromNumNonEmpty xs
        | all (== 0) xs = throw AllZeroException
        | otherwise = PolynomialNE $ xs

instance (Num a, Eq a) => FromNumListEither PolynomialNE a where
    -- >>> pretty <$> fromIntList [1,2,3]
    -- Right "1 + 2x + 3x\178"
    --
    fromNumListEither ∷ [a] → Either PolynomialError (PolynomialNE a)
    fromNumListEither xs
        | null xs = Left EmptyListException
        | all (== 0) xs = Left AllZeroException
        | otherwise = Right . PolynomialNE . LNE.fromList $ xs

    -- >>> pretty <$> (fromNonEmpty . fromList $ [1,2,3,4])
    -- Right "1 + 2x + 3x\178 + 4x\179"
    --
    fromNumNonEmptyEither ∷ NonEmpty a → Either PolynomialError (PolynomialNE a)
    fromNumNonEmptyEither xs
        | all (== 0) xs = Left AllZeroException
        | otherwise = Right . PolynomialNE $ xs

instance (PrettyNum a, Num a, Eq a) => PrettyPoly (PolynomialNE a) where
    -- >>> putStrLn . prettyPolyReverse . Polynomial . fromNumList $ [1,2,3,4]
    -- 1 + 2x + 3x² + 4x³

    -- >>> putStrLn . prettyPolyReverse . Polynomial . fromNumList $ [0, 2, 3, 0]
    --
    prettyPolyReverse ∷ PolynomialNE a → String
    prettyPolyReverse = fixLeadingSign
        . L.unwords
        . fmap (renderPiece showPoly)
        . LNE.filter removeNullCoefficient
        . LNE.zip (LNE.fromList [0..])
        . getPolynomialNE

    -- >>> putStrLn . prettyPolyConventional . Polynomial . fromNumList $ [1,2,3,4]
    -- 1 + 2x + 3x² + 4x³
    --
    prettyPolyConventional ∷ PolynomialNE a → String
    prettyPolyConventional = fixLeadingSign
        . L.unwords
        . L.reverse
        . fmap (renderPiece showPoly)
        . LNE.filter removeNullCoefficient
        . LNE.zip (LNE.fromList [0..])
        . getPolynomialNE

instance (PrettyNum a, Num a, Eq a) => PrettyPolyHTML (PolynomialNE a) where
    -- >>> putStrLn . prettyPolyReverse . Polynomial . fromNumList $ [1,2,3,4]
    -- 1 + 2x + 3x² + 4x³

    -- >>> putStrLn . prettyPolyReverse . Polynomial . fromNumList $ [0, 2, 3, 0]
    --
    prettyPolyReverseHTML ∷ PolynomialNE a → String
    prettyPolyReverseHTML = fixLeadingSign
        . L.unwords
        . fmap (renderPiece showPolyHTML)
        . LNE.filter removeNullCoefficient
        . LNE.zip (LNE.fromList [0..])
        . getPolynomialNE

    -- >>> putStrLn . prettyPolyConventional . Polynomial . fromNumList $ [1,2,3,4]
    -- 1 + 2x + 3x² + 4x³
    --
    prettyPolyConventionalHTML ∷ PolynomialNE a → String
    prettyPolyConventionalHTML = fixLeadingSign
        . L.unwords
        . L.reverse
        . fmap (renderPiece showPolyHTML)
        . LNE.filter removeNullCoefficient
        . LNE.zip (LNE.fromList [0..])
        . getPolynomialNE

    -- mul :: PolynomialNE -> PolynomialNE -> PolynomialNE
    -- mul (Polynomial (x :| xs)) (Polynomial (y :| ys)) = undefined --

instance Num a => EvalPoly PolynomialNE a where
    evalPolyAt at poly = sum $ fmap (\(i, val) -> val * at ^ (i :: Integer)) $ LNE.zip (LNE.fromList [0..]) (getPolynomialNE poly)