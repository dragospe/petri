import Prelude

import Data.Petri
import Data.Petri.CE
import Data.Row
import Samples
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = tests >>= defaultMain

type ErrHelper = "transitionNotFound" .== TransitionNotFound String .+ "eventNotEnabled" .== EventNotEnabled String String

tests :: IO TestTree
tests = do
  sample1 <- sample1Tests
  fig20 <- fig20Tests

  pure $
    testGroup
      "CENet Sanity Check"
      [ sample1
      , fig20
      ]
