{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleInstances #-}
module TestUtils
    ( TestTreeable
    , QC(..), qc
    , (~:)
    , (~:#)

    {- RE-EXPORTS: tasty -}
    , TestTree

    {- RE-EXPORTS: tasty-hunit -}
    , Assertion
    , testCase
    , testCaseSteps
    , assert
    , assertFailure
    , assertBool
    , assertString
    , assertEqual
    , (@?)
    , (@=?)
    , (@?=)

    {- RE-EXPORTS: QuickCheck -}
    , Testable
    , Property(..)
    , (===)
    , (==>)
    ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck.Property
import Test.Tasty.QuickCheck hiding (QC)

--------------------------

infixr 0 ~:

-- | Polymorphic "block-labeling" operator thing
--
-- HUnit assertions:  "label" ~: x @?= y  -- or any 'IO ()'
-- QuickCheck props:  "label" ~: qc someTestable
-- A list of tests:   "label" ~: [testTree1, testTree2]
class TestTreeable a where
    -- | HUnit's (~:), but for Tasty.
    (~:) :: String -> a -> TestTree

instance TestTreeable [TestTree] where (~:) = testGroup
instance TestTreeable (IO ()) where (~:) = testCase -- for HUnit

-- | Newtype wrapper enabling the @"label" ~: qc someProperty@ form
newtype QC a = QC a
-- | Wrap a QuickCheck property for the (~:) operator
qc :: a -> QC a
qc = QC

instance (Testable a)=> TestTreeable (QC a) where
    label ~: (QC x) = label `testProperty` x

-- | Variant of ~: for a list of very similar tests not worth naming.
--   Gives them names like "Test case 1", "Test case 2", ...
(~:#) :: (TestTreeable a)=> String -> [a] -> TestTree
label ~:# tests = label ~: zipWith f ([1..] :: [Int]) tests
  where f n test = ("Test case " ++ show n) ~: test
