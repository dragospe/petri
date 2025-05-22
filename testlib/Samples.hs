module Samples (sample1, sample1Fired) where

import Prelude

import Data.Map.Strict qualified as Map
import Data.Petri
import Data.Petri.CE
import Data.Row

{-
p0  f0 t0 f1 p1
(*) -> [] -> ()
-}
sample1 :: CENet String String String
sample1 = CENet (PetriNet ps ts f)
  where
    ps :: Places String FilledState
    ps = Places $ Map.fromList [(PlaceIndex "p0", Full), (PlaceIndex "p1", Empty)]

    ts :: Transitions String ()
    ts = Transitions $ Map.fromList [(TransitionIndex "t0", ())]

    f :: FlowRelation String String String
    f =
      FlowRelation $
        Map.fromList
          [ (ArcIndex "f0", Left (PlaceIndex "p0", TransitionIndex "t0"))
          , (ArcIndex "f1", Right (TransitionIndex "t0", PlaceIndex "p1"))
          ]

{-
p0  f0 t0 f1 p1
(*) -> [] -> ()
-}
sample1Fired ::
  Either
    (Var ("transitionNotFound" .== TransitionNotFound ti .+ "eventNotEnabled" .== EventNotEnabled si ti))
    (CENet String String String)
sample1Fired = Right $ CENet (PetriNet ps ts f)
  where
    ps :: Places String FilledState
    ps = Places $ Map.fromList [(PlaceIndex "p0", Empty), (PlaceIndex "p1", Full)]

    ts :: Transitions String ()
    ts = Transitions $ Map.fromList [(TransitionIndex "t0", ())]

    f :: FlowRelation String String String
    f =
      FlowRelation $
        Map.fromList
          [ (ArcIndex "f0", Left (PlaceIndex "p0", TransitionIndex "t0"))
          , (ArcIndex "f1", Right (TransitionIndex "t0", PlaceIndex "p1"))
          ]
