module Polynomial.Class where

import Data.List.NonEmpty qualified as LNE
import Polynomial.Error

class IsPolynomial p where
    x :: p

class FromNumList p a where
    fromNumList :: [a] -> p a

class FromNumNonEmpty p a where
    fromNumNonEmpty :: LNE.NonEmpty a -> p a

class FromNumListEither p a where
    fromNumListEither :: [a] -> Either PolynomialError (p a)

class FromNumNonEmptyEither p a where
    fromNumNonEmptyEither :: LNE.NonEmpty a -> Either PolynomialError (p a)

class Differentiable p a where
    differentiate :: p a -> p a

class Integrable p a where
    integrate :: a -> p a -> p a

class EvalPoly p a where
    evalPolyAt :: a -> p a -> a
