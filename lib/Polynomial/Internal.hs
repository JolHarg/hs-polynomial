module Polynomial.Internal where

import Data.List          qualified as L
import Data.List.NonEmpty qualified as LNE
import Polynomial.Class

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

-- >>> putStrLn . L.singleton . powerDigit $ 4
-- ⁴
--
powerDigit ∷ Int → Char
powerDigit = (LNE.fromList "⁰¹²³⁴⁵⁶⁷⁸⁹" LNE.!!)

-- >>> putStrLn . powerOf $ 54
-- ⁵⁴
--
powerOf ∷ Int → String
powerOf = fmap (powerDigit . read @Int . L.singleton) . show

-- >>> putStrLn . powerOfHTML $ 54
-- <sup>54</sup>
--
powerOfHTML ∷ Int → String
powerOfHTML = ("<sup>" <>) . (<> "</sup>") . show

-- >>> import Data.Foldable
-- >>> traverse_ putStrLn $ (\i -> show i <> ": (" <> showPoly i <> ")") <$> [0..4]
-- 0: ()
-- 1: (x)
-- 2: (x²)
-- 3: (x³)
-- 4: (x⁴)
--
showPoly ∷ Int → String
showPoly i
    | i == 0 = ""
    | i == 1 = "𝑥"
    | otherwise = "𝑥" <> powerOf i

-- >>> import Data.Foldable
-- >>> traverse_ putStrLn $ (\i -> show i <> ": (" <> showPoly i <> ")") <$> [0..4]
-- 0: ()
-- 1: (x)
-- 2: (x²)
-- 3: (x³)
-- 4: (x⁴)
--
showPolyHTML ∷ Int → String
showPolyHTML i
    | i == 0 = ""
    | i == 1 = "𝑥"
    | otherwise = "𝑥" <> powerOfHTML i


-- >>> intercalate " + " $ toList $  fmap (\(i, a) -> show a <> showPoly i) $ LNE.zip (LNE.fromList [0..]) $ getPolynomialNE $ PolynomialNE $ fromList [1,2,3,4]
-- "1 + 2x + 3x\178 + 4x\179"
--

renderPiece :: (PrettyNum a, Eq a, Num a) => (Int -> String) -> (Int, a) -> String
renderPiece showPolyFn (i, a) = (if signum a == 1 then "+ " else "- ") <> (if i == 0 then displayNum (abs a) else displayCoefficient (abs a)) <> showPolyFn i