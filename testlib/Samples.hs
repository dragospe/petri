{-# LANGUAGE OverloadedLabels #-}

module Samples (
  -- * Sample 1
  sample1,
  sample1Fired,

  -- * Fig 20
  markFig20,
  fig20_assertFiring,
  fig20_initial,
  fig20_firings,
) where

import Prelude

import Control.Monad.Error.Class
import Data.Foldable
import Data.Map.Strict qualified as Map
import Data.Petri
import Data.Petri.CE
import Data.Row
import Test.Tasty.HUnit

-- Local type alias to decrease verbosity
type CENetStringy = CENet String String String

--------------------------------------------------------------------------------
-- First sanity check sample: the easiest CENet that can meaningfully fire.
--
-- This is a trivial example, but we'll break out the big guns to demonstrate
-- how large nets could be specified
--
-- The steps go like this
-- 1.) Basically, if we don't care about the names, we just generate a certain
--     number of "indexes" (labels for places, transitions, and arcs) via list
--     comprehensions
-- 2.) The, we zip those up with their requisite data. If the data is trivial,
--     like the data associated with transitions in a CENet, we can do it ahead
--     of time. If we need more (like for place markings), we do it at the
--     appropriate spot
--

sample1_pi :: [PlaceIndex String]
sample1_pi = [PlaceIndex $ 'p' : show i | i <- [0 .. 1 :: Int]]

sample1_ti :: [TransitionIndex String]
sample1_ti = [TransitionIndex $ 't' : show i | i <- [0 :: Int]]

sample1_t :: Transitions String ()
sample1_t = Transitions . Map.fromList $ zip sample1_ti [() | _ <- [0 :: Int ..]]

-- Here we make heavy use of Overloaded strings
sample1_arcs ::
  [ Either
      (PlaceIndex String, TransitionIndex String)
      (TransitionIndex String, PlaceIndex String)
  ]
sample1_arcs =
  [ Left ("p0", "t0")
  , Right ("t0", "p1")
  ]

sample1_flow :: FlowRelation String String String
sample1_flow =
  FlowRelation . Map.fromList $
    zip
      [ArcIndex $ 'f' : show i | i <- [0 :: Int ..]]
      sample1_arcs

-- Use this function to add markings to sample 1
markSample1 :: [FilledState] -> CENetStringy
markSample1 states =
  if length states /= 2
    then error "markSample1: wrong number of states passed"
    else CENet (PetriNet ps sample1_t sample1_flow)
  where
    ps :: Places String FilledState
    ps = Places . Map.fromList $ zip sample1_pi states

{-
p0  f0 t0 f1 p1
(*) -> [] -> ()
-}
sample1 :: CENetStringy
sample1 = markSample1 [Full, Empty]

{-
p0  f0 t0 f1 p1
(*) -> [] -> ()
-}
sample1Fired :: CENetStringy
sample1Fired = markSample1 [Empty, Full]

--------------------------------------------------------------------------------
-- Fig 20 from Wolfgang Resig - Petri Nets: An Introduction (1985)
--
-- Gives one CE net with 9 different cases

fig20_arcs ::
  [ Either
      (PlaceIndex String, TransitionIndex String)
      (TransitionIndex String, PlaceIndex String)
  ]
fig20_arcs =
  [ Right ("e1", "b3")
  , Left ("b3", "e4")
  , Right ("e4", "b5")
  , Left ("b5", "e5")
  , Left ("b3", "e2")
  , Left ("b1", "e1")
  , Right ("e2", "b1")
  , Right ("e5", "b1")
  , Right ("e1", "b2")
  , Left ("b2", "e2")
  , Left ("b2", "e3")
  , Right ("e3", "b4")
  , Left ("b4", "e5")
  ]

fig20_flow :: FlowRelation String String String
fig20_flow =
  FlowRelation . Map.fromList $
    zip
      [ArcIndex $ 'f' : show i | i <- [1 :: Integer ..]]
      fig20_arcs

fig20_ti :: [TransitionIndex String]
fig20_ti = [TransitionIndex $ 'e' : show i | i <- [1 :: Integer .. 5]]

fig20_t :: Transitions String ()
fig20_t = Transitions . Map.fromList $ zip fig20_ti [() | _ <- [0 :: Integer ..]]

fig20_pi :: [PlaceIndex String]
fig20_pi = [PlaceIndex $ 'b' : show i | i <- [1 .. 5 :: Integer]]

markFig20 :: [FilledState] -> CENetStringy
markFig20 states =
  if length states /= 5
    then error "markFig20: incorrect number of states"
    else CENet (PetriNet ps fig20_t fig20_flow)
  where
    ps = Places . Map.fromList $ zip fig20_pi states

fig20_firings :: [(TransitionIndex String, [FilledState])]
fig20_firings =
  --  t      b1    b2     b3     b4     b5
  [ ("e1", [Empty, Full, Full, Empty, Empty])
  , ("e4", [Empty, Full, Empty, Empty, Full])
  , ("e3", [Empty, Empty, Empty, Full, Full])
  , ("e5", [Full, Empty, Empty, Empty, Empty])
  , ("e1", [Empty, Full, Full, Empty, Empty])
  , ("e2", [Full, Empty, Empty, Empty, Empty])
  , ("e1", [Empty, Full, Full, Empty, Empty])
  , ("e3", [Empty, Empty, Full, Full, Empty])
  ]

fig20_initial :: [FilledState]
fig20_initial = [Full, Empty, Empty, Empty, Empty]

{- | Given an initial state and a list of transitions paired with their resulting states,
fire the net and make sure everything lines up
-}
fig20_assertFiring :: [FilledState] -> [(TransitionIndex String, [FilledState])] -> IO ()
fig20_assertFiring initialState statesAndFirings =
  case foldlM foldFunc initialNet statesAndFirings of
    Left e -> assertFailure $ show e
    Right _ -> pure ()
  where
    foldFunc ::
      CENetStringy ->
      (TransitionIndex String, [FilledState]) ->
      Either
        ( Var
            ( "transitionNotFound" .== TransitionNotFound String
                .+ "eventNotEnabled" .== EventNotEnabled String String
                .+ "other" .== String
            )
        )
        CENetStringy

    foldFunc net (t, states) = do
      newNet <- (fire net t)
      if markFig20 states == newNet
        then pure newNet
        else throwError . IsJust #"other" $ "fig20_assertFiring: failed\n" ++
          "expected: " <> show (markFig20 states)
          <> "\nactual: " <> show newNet

    initialNet :: CENetStringy
    initialNet = markFig20 initialState
