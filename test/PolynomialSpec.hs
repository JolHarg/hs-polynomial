module PolynomialSpec (spec) where

import Polynomial
import Polynomial.Class
import Polynomial.Pretty
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

prop_evaluatesToXPower0 :: [Int] -> Property
prop_evaluatesToXPower0 xs = evalPolyAt 0 (fromNumList (0:xs) :: Polynomial Int) === 0

prop_evaluatesOnlyX0ToInput :: Int -> Property
prop_evaluatesOnlyX0ToInput x' = evalPolyAt x' (fromNumList [x'] :: Polynomial Int) === x'

spec :: Spec
spec = do
    describe "Polynomial" $ do
        describe "when polynomial has no x^0 power" $
            prop "evaluates to 0" $ prop_evaluatesToXPower0
        describe "when polynomial has only x^0 power" $
            prop "evaluates to input" $ prop_evaluatesOnlyX0ToInput
        describe "with an arbitrary input" $ do
            describe "𝑥⁴ + 𝑥² + 1" $ do
                let eqn :: Polynomial Int
                    eqn = fromNumList [1,0,1,0,1]
                it "should render the correct equation conventionally" $
                    prettyPoly defaultPrettyPolyOptions eqn === "𝑥⁴ + 𝑥² + 1"
                it "should render the correct equation in reverse" $
                    prettyPoly defaultPrettyPolyOptions { termOrder = termOrderReverse } eqn === "1 + 𝑥² + 𝑥⁴"
            describe "𝑥⁴" $ do
                let eqn :: Polynomial Int
                    eqn = fromNumList [0,0,0,0,1]
                it "should render the correct equation conventionally" $
                    prettyPoly defaultPrettyPolyOptions eqn === "𝑥⁴"
                it "should render the correct equation in reverse" $
                    prettyPoly defaultPrettyPolyOptions { termOrder = termOrderReverse } eqn === "𝑥⁴"
        describe "with a specific binomial multiplication" $ do
            let eqn :: Polynomial Int
                eqn = fromNumList [1,1]
                    * fromNumList [1,1]
                    * fromNumList [1,1]
            it "should render the correct equation" $
                prettyPoly defaultPrettyPolyOptions eqn === "𝑥³ + 3𝑥² + 3𝑥 + 1"
            it "should render the correct equation in reverse" $
                prettyPoly defaultPrettyPolyOptions { termOrder = termOrderReverse } eqn === "1 + 3𝑥 + 3𝑥² + 𝑥³"