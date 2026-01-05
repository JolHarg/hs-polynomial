{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wwarn #-}

module Polynomial.Sparse (PolynomialSparse) where

-- import Data.Char
import Data.IntMap        qualified as IM
import Data.List          qualified as L
import Data.List.NonEmpty qualified as LNE
import Polynomial.Class
import Polynomial.Error
import Polynomial.Internal
import Polynomial.Pretty

newtype PolynomialSparse a = PolynomialSparse {
    getPolynomialSparse :: IM.IntMap a
} deriving stock (Eq, Show)

instance (Num a) => IsPolynomial (PolynomialSparse a) where
    x = PolynomialSparse undefined

instance FromNumList PolynomialSparse a where
    fromNumList :: [a] -> PolynomialSparse a
    fromNumList = PolynomialSparse . undefined

instance FromNumNonEmpty PolynomialSparse a where
    fromNumNonEmpty :: LNE.NonEmpty a -> PolynomialSparse a
    fromNumNonEmpty = PolynomialSparse . undefined

instance FromNumListEither PolynomialSparse a where
    fromNumListEither :: [a] -> Either PolynomialError (PolynomialSparse a)
    fromNumListEither = undefined

instance FromNumNonEmptyEither PolynomialSparse a where
    fromNumNonEmptyEither :: LNE.NonEmpty a -> Either PolynomialError (PolynomialSparse a)
    fromNumNonEmptyEither = Right . undefined

instance Num a => Num (PolynomialSparse a) where
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined
    negate = undefined

instance (PrettyNum a, Num a, Eq a) => PrettyPoly (PolynomialSparse a) where
    prettyPoly ∷ PrettyPolyOptions -> PolynomialSparse a → String
    prettyPoly = undefined

instance Num a => EvalPoly PolynomialSparse a where
    evalPolyAt at poly = undefined