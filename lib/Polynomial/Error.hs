module Polynomial.Error where

import Control.Exception

data PolynomialError = EmptyListException | AllZeroException
    deriving stock (Show)

instance Exception PolynomialError

