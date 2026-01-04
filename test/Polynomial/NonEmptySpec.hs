module Polynomial.NonEmptySpec (spec) where

import Data.List.NonEmpty qualified as LNE
import Polynomial.Class
import Polynomial.NonEmpty
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

prop_evaluatesToXPower0 :: [Int] -> Property
prop_evaluatesToXPower0 xs = any (/= 0) xs ==> evalPolyAt 0 (fromNumNonEmpty (LNE.fromList (0:xs)) :: PolynomialNE Int) === 0

prop_evaluatesOnlyX0ToInput :: Int -> Property
prop_evaluatesOnlyX0ToInput x' = x' /= 0 ==> evalPolyAt x' (fromNumNonEmpty (LNE.fromList [x']) :: PolynomialNE Int) === x'

spec :: Spec
spec = do
    describe "Polynomial" $ do
        describe "when polynomial has no x^0 power" $
            prop "evaluates to 0" $ prop_evaluatesToXPower0
        describe "when polynomial has only x^0 power" $
            prop "evaluates to input" $ prop_evaluatesOnlyX0ToInput
        describe "with an arbitrary input" $ do
            describe "𝑥⁴ + 𝑥² + 1" $ do
                let eqn :: PolynomialNE Int
                    eqn = fromNumNonEmpty $ LNE.fromList [1,0,1,0,1]
                it "should render the correct equation conventionally" $
                    prettyPolyConventional eqn === "𝑥⁴ + 𝑥² + 1"
                it "should render the correct equation in reverse" $
                    prettyPolyReverse eqn === "1 + 𝑥² + 𝑥⁴"
            describe "𝑥⁴" $ do
                let eqn :: PolynomialNE Int
                    eqn = fromNumNonEmpty $ LNE.fromList [0,0,0,0,1]
                it "should render the correct equation conventionally" $
                    prettyPolyConventional eqn === "𝑥⁴"
                it "should render the correct equation in reverse" $
                    prettyPolyReverse eqn === "𝑥⁴"
        {-
        xdescribe "with a specific binomial multiplication" $ do
            let eqn :: PolynomialNE Int
                eqn = fromNumList [1,1]
                    * fromNumList [1,1]
                    * fromNumList [1,1]
            it "should render the correct equation" $
                prettyPolyConventional eqn === "𝑥³ + 3𝑥² + 3𝑥 + 1"
            it "should render the correct equation in the " $
                prettyPolyConventional eqn === "1 + 3𝑥 + 3𝑥² + 𝑥³"
        -}