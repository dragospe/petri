import Prelude

import Data.Petri
import Data.Petri.CE
import Data.Row
import Samples
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

type ErrHelper = "transitionNotFound" .== TransitionNotFound String .+ "eventNotEnabled" .== EventNotEnabled String String

tests :: TestTree
tests =
  testGroup
    "CENet Sanity Check"
    [ testGroup "sample1" $
        [ testCase "sample1 has valid flow relation" $
            assertBool
              "sample1 flow relation invalid"
              (isValidFlowRelation . getCENet $ sample1)
        , testCase "sample1 fires correctly" $
            fire sample1 (TransitionIndex "t0")
              @?= Right @(Var ErrHelper)
                sample1Fired
        , testCase "bogus firing not found" $
            fire sample1 (TransitionIndex "bogus")
              @?= Left
                ( varyTransitionNotFound
                    @String
                    @ErrHelper
                    (TransitionIndex "bogus")
                )
        ]
    , testGroup "fig20" $
        [ testCase "fig20 has a valid flow relation"
            $ assertBool
              "fig20 flow relation invalid"
              ( isValidFlowRelation . getCENet . markFig20 $
                  [Empty, Empty, Empty, Empty, Empty]
              )
        , testCase "fig20 fires correctly" $
            fig20_assertFiring fig20_initial fig20_firings
        ]
    ]
