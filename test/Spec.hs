import Traversal
import TraversalPar
import TraversalParV2
import Test.HUnit
import qualified System.Exit as Exit

-- Run the following from the parallel-fs-stat directory to run these test
-- $ cabal test
--
-- Outline based on this guide
-- https://functional.works-hub.com/learn/basic-unit-testing-in-haskell-using-hunit-and-cabal-29e47

-- Sequential traversal
seqTest :: Test
seqTest = TestCase $ do
  seqTravRes <- traverseFS 0 "sampleroot/one_root"
  assertEqual "Sequential traversal of sampleroot/one_root" 2 (length seqTravRes)

-- "Parallel" V1 traversal
parTest :: Test
parTest = TestCase $ do
  parTravRes <- traverseFSPar 0 "sampleroot/one_root"
  assertEqual "Parallel V1 traversal of sampleroot/one_root" 2 (length parTravRes)

-- Parallel V2 traversal
parV2Test :: Test
parV2Test = TestCase $ do
  parV2TravRes <- traverseFSParV2 0 "sampleroot/one_root"
  assertEqual "Parallel V2 traversal of sampleroot/one_root" 2 (length parV2TravRes)

tests :: Test
tests = TestList [TestLabel "seqTest" seqTest
                , TestLabel "parTest" parTest
                , TestLabel "parV2Test" parV2Test]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess



