{-# LANGUAGE OverloadedLists #-}

module Polynomial.Sparse (PolynomialSparse) where

-- import Data.Char
import Data.IntMap        qualified as IM
import Data.List          qualified as L
import Polynomial.Class
import Polynomial.Error
import Polynomial.Internal

newtype PolynomialSparse a = PolynomialSparse {
    getPolynomialSparse :: IntMap a
} deriving stock (Eq, Show)

instance (Num a) => IsPolynomial (PolynomialNE a) where
    x = PolynomialSparse undefined

instance FromNumList PolynomialSparse a where
    fromNumList :: [a] -> PolynomialSparse a
    fromNumList = Polynomial . undefined
    
    fromNumNonEmpty :: LNE.NonEmpty a -> PolynomialSparse a
    fromNumNonEmpty = PolynomialSparse . undefined

instance FromNumListEither Polynomial a where
    fromNumListEither :: [a] -> Either PolynomialError (PolynomialSparse a)
    fromNumListEither = undefined
    
    fromNumNonEmptyEither :: LNE.NonEmpty a -> Either PolynomialError (PolynomialSparse a)
    fromNumNonEmptyEither = Right . undefined

-- >>> PolynomialSparse (fromNumList [1, 2]) + PolynomialSparse (fromNumList ([2, 3])
-- instance Num a => Num (PolynomialSparse a) where
--     PolynomialSparse [] + PolynomialSparse []         = PolynomialSparse []
--     PolynomialSparse (a : as) + PolynomialSparse (b : bs) = PolynomialSparse $ (a + b) : getPolynomialSparse (PolynomialSparse as + PolynomialSparse bs)
--     PolynomialSparse as + PolynomialSparse [] = PolynomialSparse as
--     PolynomialSparse [] + PolynomialSparse as = PolynomialSparse as
--     PolynomialSparse [] * PolynomialSparse []         = PolynomialSparse []
--     PolynomialSparse xs * PolynomialSparse ys = PolynomialSparse $ fmap sum $ diagonals $ fmap (\x -> fmap (\y -> y * x) ys) xs
--     abs (PolynomialSparse xs) = PolynomialSparse (fmap abs xs)
--     signum (PolynomialSparse xs) = PolynomialSparse (fmap signum xs)
--     fromInteger x = PolynomialSparse [fromInteger x]
--     negate (PolynomialSparse xs) = PolynomialSparse (fmap negate xs)

instance (PrettyNum a, Num a, Eq a) => PrettyPoly (PolynomialSparse a) where
    -- >>> putStrLn . prettyPoly . PolynomialSparse . fromNumList $ [1,2,3,4]
    -- 1 + 2x + 3x² + 4x³
    --

    -- >>> putStrLn . prettyPoly . PolynomialSparse . fromNumList $ [0, 2,3,(-1),0]

    prettyPolyReverse ∷ PolynomialSparse a → String
    prettyPolyReverse = undefined {- fixLeadingSign
        . unwords
        . fmap (renderPiece showPoly)
        . filter (\(_, a) -> a /= 0)
        . getPolynomialSparse -}

    prettyPolyConventional ∷ PolynomialSparse a → String
    prettyPolyConventional = undefined {- fixLeadingSign
        . unwords
        . reverse
        . fmap (renderPiece showPoly)
        . filter (\(_, a) -> a /= 0)
        -- . sort
        . getPolynomialSparse -}


instance (PrettyNum a, Num a, Eq a) => PrettyPolyHTML (PolynomialSparse a) where
    -- >>> putStrLn . prettyPoly . PolynomialSparse . fromNumList $ [1,2,3,4]
    -- 1 + 2x + 3x² + 4x³
    --

    -- >>> putStrLn . prettyPoly . PolynomialSparse . fromNumList $ [0, 2,3,(-1),0]

    prettyPolyReverseHTML ∷ PolynomialSparse a → String
    prettyPolyReverseHTML = undefined {- fixLeadingSign
        . unwords
        . fmap (renderPiece showPolyHTML)
        . filter (\(_, a) -> a /= 0)
        . zip [0..]
        . getPolynomial -}

    prettyPolyConventionalHTML ∷ PolynomialSparse a → String
    prettyPolyConventionalHTML = undefined {- fixLeadingSign
        . unwords
        . reverse
        . fmap (renderPiece showPolyHTML)
        . 
        . zip [0..]
        . getPolynomialSparse -}

-- mul :: PolynomialSparse -> PolynomialSparse -> PolynomialSparse
-- mul (PolynomialSparse (x :| xs)) (PolynomialSparse (y :| ys)) = undefined --

instance Num a => EvalPoly PolynomialSparse a where
    evalPolyAt at poly = undefined -- sum $ fmap (\(i, val) -> val * at ^ (i :: Integer)) $ zip [0..] (getPolynomialSparse poly)