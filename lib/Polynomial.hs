{-# LANGUAGE OverloadedLists #-}

module Polynomial (Polynomial) where

-- import Data.Char
import Data.List          qualified as L
import Data.List.NonEmpty qualified as LNE
import Polynomial.Class
import Polynomial.Error
import Polynomial.Internal

newtype Polynomial a = Polynomial {
    getPolynomial :: [a]
} deriving stock (Eq, Show)

instance (Num a) => IsPolynomial (Polynomial a) where
    x = Polynomial [0, 1]

instance FromNumList Polynomial a where
    fromNumList :: [a] -> Polynomial a
    fromNumList = Polynomial
    
    fromNumNonEmpty :: LNE.NonEmpty a -> Polynomial a
    fromNumNonEmpty = Polynomial . LNE.toList

instance FromNumListEither Polynomial a where
    fromNumListEither :: [a] -> Either PolynomialError (Polynomial a)
    fromNumListEither = Right . Polynomial
    
    fromNumNonEmptyEither :: LNE.NonEmpty a -> Either PolynomialError (Polynomial a)
    fromNumNonEmptyEither = Right . Polynomial . LNE.toList

-- >>> Polynomial (fromNumList [1, 2]) + Polynomial (fromNumList [2, 3])
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
    -- >>> putStrLn . prettyPoly . Polynomial . fromNumList $ [1,2,3,4]
    -- 1 + 2x + 3x² + 4x³
    --

    -- >>> putStrLn . prettyPoly . Polynomial . fromNumList $ [0, 2,3,(-1),0]

    prettyPolyReverse ∷ Polynomial a → String
    prettyPolyReverse = fixLeadingSign
        . L.unwords
        . fmap (renderPiece showPoly)
        . L.filter removeNullCoefficient
        . L.zip [0..]
        . getPolynomial

    prettyPolyConventional ∷ Polynomial a → String
    prettyPolyConventional = fixLeadingSign
        . L.unwords
        . L.reverse
        . fmap (renderPiece showPoly)
        . L.filter removeNullCoefficient
        . L.zip [0..]
        . getPolynomial


instance (PrettyNum a, Num a, Eq a) => PrettyPolyHTML (Polynomial a) where
    -- >>> putStrLn . prettyPoly . Polynomial . fromNumList $ [1,2,3,4]
    -- 1 + 2x + 3x² + 4x³
    --

    -- >>> putStrLn . prettyPoly . Polynomial . fromNumList $ [0, 2,3,(-1),0]

    prettyPolyReverseHTML ∷ Polynomial a → String
    prettyPolyReverseHTML = fixLeadingSign
        . L.unwords
        . fmap (renderPiece showPolyHTML)
        . L.filter removeNullCoefficient
        . L.zip [0..]
        . getPolynomial

    prettyPolyConventionalHTML ∷ Polynomial a → String
    prettyPolyConventionalHTML = fixLeadingSign
        . L.unwords
        . L.reverse
        . fmap (renderPiece showPolyHTML)
        . L.filter removeNullCoefficient
        . L.zip [0..]
        . getPolynomial

-- mul :: Polynomial -> Polynomial -> Polynomial
-- mul (Polynomial (x :| xs)) (Polynomial (y :| ys)) = undefined --

instance Num a => EvalPoly Polynomial a where
    evalPolyAt at poly = sum $ fmap (\(i, val) -> val * at ^ (i :: Integer)) $ zip [0..] (getPolynomial poly)