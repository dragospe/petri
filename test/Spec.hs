import Prelude

import Data.Petri
import Data.Petri.CE
import Data.Row
import Samples
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "CENet Sanity Check"
    [ testCase "sample1 has valid flow relation" $
        assertBool "sample1 flow relation invalid" (isValidFlowRelation . getCENet $ sample1)
    , testCase "sample1 fires correctly" $
        fire sample1 (TransitionIndex "t0") @?= sample1Fired
    , testCase "bogus firing not found" $
        fire sample1 (TransitionIndex "bogus")
          @?= Left
            ( varyTransitionNotFound
                @String
                @( "transitionNotFound" .== TransitionNotFound String
                    .+ "eventNotEnabled" .== EventNotEnabled String String
                 )
                (TransitionIndex "bogus")
            )
    ]
