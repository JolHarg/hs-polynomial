module Polynomial.Internal where

import Data.List          qualified as L
import Data.List.NonEmpty qualified as LNE

-- Stolen from Daniel Wagner: https://hackage.haskell.org/package/universe-base-1.1.4/docs/src/Data.Universe.Helpers.html#diagonals
diagonals :: [[a]] -> [[a]]
diagonals = L.drop 1 . go [] where
  -- it is critical for some applications that we start producing answers
  -- before inspecting es_
  go b es_ = [h | h:_ <- b] : case es_ of
    []   -> L.transpose ts
    e:es -> go (e:ts) es
    where ts = [t | _:t <- b]

nediagonals :: LNE.NonEmpty (LNE.NonEmpty a) -> LNE.NonEmpty (LNE.NonEmpty a)
nediagonals = LNE.fromList . fmap LNE.fromList . diagonals . LNE.toList . fmap LNE.toList

fixLeadingSign :: String -> String
fixLeadingSign s = (if s !! 0 == '+' then "" else "-") <> drop 2 s

removeNullCoefficient :: (Num a, Eq a) => (i, a) -> Bool
removeNullCoefficient = (/= 0) . snd

-- Display power functions
displayPowerAscii ∷ Int → String
displayPowerAscii = ("^" <>) . show

displayPowerUnicode ∷ Int → String
displayPowerUnicode = fmap (powerDigit . read @Int . L.singleton) . show where
    powerDigit ∷ Int → Char
    powerDigit = (LNE.fromList "⁰¹²³⁴⁵⁶⁷⁸⁹" LNE.!!)

displayPowerHTML ∷ Int → String
displayPowerHTML = ("<sup>" <>) . (<> "</sup>") . show

-- Display X functions
displayXAscii :: Int -> String
displayXAscii i
    | i == 0 = ""
    | otherwise = "x"

displayXUnicodeSupp :: Int -> String
displayXUnicodeSupp i
    | i == 0 = ""
    | otherwise = "𝑥"
