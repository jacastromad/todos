module Main (main) where

import Test.HUnit
import qualified TestTodos
import qualified TestSource
import Control.Monad (when)


tests :: Test
tests = TestList [TestTodos.tests, TestSource.tests]


main :: IO ()
main = do
    counts <- runTestTT tests
    when (failures counts + errors counts > 0) $ fail "Tests failed"

