module Polynomial.Pretty where

import Data.Ratio
import Polynomial.Internal

class PrettyNum n where
    displayNum :: n -> String
    displayCoefficient :: n -> String

-- We need a bit of magic here probably
instance PrettyNum Integer where
    displayNum = show
    displayCoefficient a
        | a == 0 || a == 1 = ""
        | otherwise = show a

instance PrettyNum Int where
    displayNum = show
    displayCoefficient a
        | a == 0 || a == 1 = ""
        | otherwise = show a

instance PrettyNum Double where
    displayNum = show
    displayCoefficient a
        | a == 0 || a == 1 = ""
        | otherwise = show a

instance PrettyNum Float where
    displayNum = show
    displayCoefficient a
        | a == 0 || a == 1 = ""
        | otherwise = show a

{-}      
instance {-# OVERLAPS #-} (Fractional n) => PrettyNum n where
    displayNum = show
    displayCoefficient a
        | a == 0 || a == 1 = ""
        | otherwise = show a
-}

instance {-# OVERLAPS #-} (Show a, Num a, Eq a) => PrettyNum (Ratio a) where
    displayNum r
        | denominator r == 1 = show (numerator r)
        | otherwise = show (numerator r) <> "/" <> show (denominator r)
    displayCoefficient r
        | numerator r == 0 || numerator r == 1 = ""
        | denominator r == 1 = show (numerator r)
        | otherwise = "(" <> show (numerator r) <> "/" <> show (denominator r) <> ")"

type DisplayXFn = Int -> String

type DisplayPowerFn = Int -> String

type TermOrder = [String] -> [String]

termOrderForward :: TermOrder
termOrderForward = id

termOrderReverse :: TermOrder
termOrderReverse = reverse

data PrettyPolyOptions = PrettyPolyOptions {
    termOrder :: TermOrder, -- You could use reverse or id here
    displayXFn :: DisplayXFn,
    displayPowerFn :: DisplayPowerFn
}

-- | Just a nice default. You don't have to use it.
defaultPrettyPolyOptions :: PrettyPolyOptions
defaultPrettyPolyOptions = PrettyPolyOptions {
    termOrder = termOrderForward,
    displayXFn = displayXUnicodeSupp, -- noteworthy for not being suitable for cairo rendering
    displayPowerFn = displayPowerUnicode
}

class PrettyPoly p where
    prettyPoly :: PrettyPolyOptions -> p -> String

-- Auxiliary function to render an individual term for individual instances to use
renderTerm :: (PrettyNum a, Eq a, Num a) => (Int -> String) -> (Int -> String) -> (Int, a) -> String
renderTerm displayXFn displayPowerFn (i, a) = (if signum a == 1 then "+ " else "- ")
    <> (if i == 0
        then displayNum (abs a)
        else displayCoefficient (abs a)
            <> displayXFn i
            <> if i == 0 || i == 1 then "" else displayPowerFn i)