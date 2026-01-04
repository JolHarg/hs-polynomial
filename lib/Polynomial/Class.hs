{-# LANGUAGE UndecidableInstances #-}

module Polynomial.Class where

import Data.List.NonEmpty qualified as LNE
import Data.Ratio
import Polynomial.Error

class IsPolynomial p where
    x :: p

class FromNumList p a where
    fromNumList :: [a] -> p a
    fromNumNonEmpty :: LNE.NonEmpty a -> p a

class FromNumListEither p a where
    fromNumListEither :: [a] -> Either PolynomialError (p a)
    fromNumNonEmptyEither :: LNE.NonEmpty a -> Either PolynomialError (p a)

class PrettyNum n where
    displayNum :: n -> String
    displayCoefficient :: n -> String

instance (Show a, Num a, Eq a) => PrettyNum (Ratio a) where
    displayNum r
        | denominator r == 1 = show (numerator r)
        | otherwise = show (numerator r) <> "/" <> show (denominator r)
    displayCoefficient r
        | numerator r == 0 || numerator r == 1 = ""
        | denominator r == 1 = show (numerator r)
        | otherwise = "(" <> show (numerator r) <> "/" <> show (denominator r) <> ")"

instance {-# OVERLAPS #-} (Integral n) => PrettyNum n where
    displayNum = show @Integer . fromIntegral
    displayCoefficient a
        | a == 0 || a == 1 = ""
        | otherwise = show @Integer . fromIntegral $ a

class PrettyPoly p where
    prettyPolyReverse :: p -> String
    prettyPolyConventional :: p -> String

class PrettyPolyHTML p where
    prettyPolyReverseHTML :: p -> String
    prettyPolyConventionalHTML :: p -> String

class EvalPoly p a where
    evalPolyAt :: a -> p a -> a