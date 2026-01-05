{-# LANGUAGE OverloadedLists #-}

module Polynomial (Polynomial) where

-- import Data.Char
import Data.List          qualified as L
import Data.List.NonEmpty qualified as LNE
import Polynomial.Class
import Polynomial.Error
import Polynomial.Internal
import Polynomial.Pretty

newtype Polynomial a = Polynomial {
    getPolynomial :: [a]
} deriving stock (Eq, Show)

instance (Num a) => IsPolynomial (Polynomial a) where
    x = Polynomial [0, 1]

instance FromNumList Polynomial a where
    fromNumList :: [a] -> Polynomial a
    fromNumList = Polynomial
    
instance FromNumNonEmpty Polynomial a where
    fromNumNonEmpty :: LNE.NonEmpty a -> Polynomial a
    fromNumNonEmpty = Polynomial . LNE.toList

instance FromNumListEither Polynomial a where
    fromNumListEither :: [a] -> Either PolynomialError (Polynomial a)
    fromNumListEither = Right . Polynomial
    
instance FromNumNonEmptyEither Polynomial a where
    fromNumNonEmptyEither :: LNE.NonEmpty a -> Either PolynomialError (Polynomial a)
    fromNumNonEmptyEither = Right . Polynomial . LNE.toList

instance (Num a, Enum a) => Differentiable Polynomial a where
    differentiate (Polynomial xs) = Polynomial . drop 1 . fmap (\(pow, coeff) -> coeff * pow) $ zip [0..] xs

instance (Num a, Enum a, Fractional a) => Integrable Polynomial a where
    integrate cval (Polynomial xs) = Polynomial $ (cval :) . fmap (\(pow, coeff) -> coeff / (pow + 1)) $ zip [0..] xs

instance Num a => Num (Polynomial a) where
    Polynomial [] + Polynomial []         = Polynomial []
    Polynomial (a : as) + Polynomial (b : bs) = Polynomial $ (a + b) : getPolynomial (Polynomial as + Polynomial bs)
    Polynomial as + Polynomial [] = Polynomial as
    Polynomial [] + Polynomial as = Polynomial as
    Polynomial [] * Polynomial []         = Polynomial []
    Polynomial xs * Polynomial ys = Polynomial $ fmap sum $ diagonals $ fmap (\x' -> fmap (\y' -> y' * x') ys) xs
    abs (Polynomial xs) = Polynomial (fmap abs xs)
    signum (Polynomial xs) = Polynomial (fmap signum xs)
    fromInteger integer = Polynomial [fromInteger integer]
    negate (Polynomial xs) = Polynomial (fmap negate xs)

instance (PrettyNum a, Num a, Eq a) => PrettyPoly (Polynomial a) where
    prettyPoly ∷ PrettyPolyOptions -> Polynomial a → String
    prettyPoly PrettyPolyOptions { termOrder, displayXFn, displayPowerFn } = fixLeadingSign
        . L.unwords
        . termOrder
        . L.reverse
        . fmap (renderTerm displayXFn displayPowerFn)
        . L.filter removeNullCoefficient
        . L.zip [0..]
        . getPolynomial

instance Num a => EvalPoly Polynomial a where
    evalPolyAt at poly = sum $ fmap (\(i, val) -> val * at ^ (i :: Integer)) $ zip [0..] (getPolynomial poly)