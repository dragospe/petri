{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Samples (
  -- * Sample 1
  sample1Tests,

  -- * Fig 20
  fig20Tests,
) where

import Prelude

import Control.Exception
import Control.Monad
import Data.Map.Strict qualified as Map
import Data.Petri
import Data.Petri.CE
import Data.Row
import Test.Tasty
import Test.Tasty.HUnit

-- Local type alias to decrease verbosity
type CENetStringy = CENet String String String

-- | Initialize n places as (b1, TVar Empty), ...
buildNumericPlaces :: Int -> STM (Places String FilledState)
buildNumericPlaces n = do
  let
    pi = [PlaceIndex $ 'b' : show i | i <- [1 .. n]]
  l <-
    mapM
      ( \placeIndex -> do
          tvar <- newTVar Empty
          pure (placeIndex, tvar)
      )
      pi
  pure . Places . Map.fromList $ l

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

sample1_p :: STM (Places String FilledState)
sample1_p = buildNumericPlaces 2

sample1_ti :: [TransitionIndex String]
sample1_ti = [TransitionIndex $ 'e' : show i | i <- [1 :: Int]]

sample1_t :: Transitions String ()
sample1_t = Transitions . Map.fromList $ zip sample1_ti [() | _ <- [1 :: Int ..]]

-- Here we make heavy use of Overloaded strings
sample1_arcs ::
  [ ( Either
        (PlaceIndex String, TransitionIndex String)
        (TransitionIndex String, PlaceIndex String)
    , ()
    )
  ]
sample1_arcs =
  zip
    [ Left ("b1", "e1")
    , Right ("e1", "b2")
    ]
    [() | _ <- [(0 :: Int) ..]]

sample1_flow :: FlowRelation String String String ()
sample1_flow =
  FlowRelation . Map.fromList $
    zip
      [ArcIndex $ 'f' : show i | i <- [0 :: Int ..]]
      sample1_arcs

-- Use this function to add markings to a CENet
-- Note: If the index doesn't appear, then the place will not be updated
markCENet :: [(PlaceIndex String, FilledState)] -> CENetStringy -> STM ()
markCENet states net =
  mapM_
    ( \(index, state) -> writeTVar (places Map.! index) state
    )
    states
  where
    places = getPlaces . s . getCENet $ net

sample1Init :: STM CENetStringy
sample1Init = do
  p <- sample1_p
  pure . CENet $ PetriNet p sample1_t sample1_flow

data FiringException e = FiringException e
  deriving stock (Show, Typeable)

instance (Show e, Typeable e) => Exception (FiringException e)

sample1Tests :: IO TestTree
sample1Tests = do
  {-
  b1  f1 e1 f2 b2
  (*) -> [] -> ( )
  -}
  actual <- atomically $ do
    s1 <- sample1Init
    markCENet [("b1", Full), ("b2", Empty)] s1
    pure s1

  fire1 :: STM () <- runExceptT (mkFiring @FiringHelper actual (TransitionIndex "e1")) >>= either (throwIO . FiringException) return
  atomically fire1
  frozenActual <- atomically $ freezePlaces (s . getCENet $ actual)

  putStrLn "expected"
  {-
  p0  f0 t0 f1 p1
  ( ) -> [] -> (*)
  -}
  expected <- atomically $ do
    s1 <- sample1Init
    markCENet [("b1", Empty), ("b2", Full)] s1
    pure s1

  frozenExpected <- atomically $ freezePlaces (s . getCENet $ expected)

  fire2 <- runExceptT (mkFiring actual (TransitionIndex "bogus"))
  pure $
    testGroup
      "sample1"
      [ testCase "sample1 has valid flow relation" $
          assertBool
            "sample1 flow relation invalid"
            (isValidFlowRelation . getCENet $ actual)
      , testCase "sample1 fires correctly" $
          frozenActual
            @?= frozenExpected
      , testCase "bogus firing not found" $
          case fire2 of
            Left e ->
              e
                @?= varyTransitionNotFound
                  @String
                  @FiringHelper
                  (TransitionIndex "bogus")
            Right _ -> assertFailure "bogus firing not detected"
      ]

--------------------------------------------------------------------------------
-- Fig 20 from Wolfgang Resig - Petri Nets: An Introduction (1985)
--
-- Gives one CE net with 9 different cases

fig20_arcs ::
  [ ( Either
        (PlaceIndex String, TransitionIndex String)
        (TransitionIndex String, PlaceIndex String)
    , ()
    )
  ]
fig20_arcs =
  zip
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
    [() | _ :: Int <- [0 ..]]

fig20_flow :: FlowRelation String String String ()
fig20_flow =
  FlowRelation . Map.fromList $
    zip
      [ArcIndex $ 'f' : show i | i <- [1 :: Integer ..]]
      fig20_arcs

fig20_ti :: [TransitionIndex String]
fig20_ti = [TransitionIndex $ 'e' : show i | i <- [1 :: Integer .. 5]]

fig20_t :: Transitions String ()
fig20_t = Transitions . Map.fromList $ zip fig20_ti [() | _ <- [0 :: Integer ..]]

fig20_pi :: STM (Places String FilledState)
fig20_pi = buildNumericPlaces 5

fig20_firings :: [(TransitionIndex String, [(PlaceIndex String, FilledState)])]
fig20_firings =
  --  t      b1    b2     b3     b4     b5
  [ ("e1", fig20_mkStates [Empty, Full, Full, Empty, Empty])
  , ("e4", fig20_mkStates [Empty, Full, Empty, Empty, Full])
  , ("e3", fig20_mkStates [Empty, Empty, Empty, Full, Full])
  , ("e5", fig20_mkStates [Full, Empty, Empty, Empty, Empty])
  , ("e1", fig20_mkStates [Empty, Full, Full, Empty, Empty])
  , ("e2", fig20_mkStates [Full, Empty, Empty, Empty, Empty])
  , ("e1", fig20_mkStates [Empty, Full, Full, Empty, Empty])
  , ("e3", fig20_mkStates [Empty, Empty, Full, Full, Empty])
  ]

fig20_mkStates :: [FilledState] -> [(PlaceIndex String, FilledState)]
fig20_mkStates = zip [PlaceIndex $ 'b' : show i | i <- [1 .. 5 :: Int]]

fig20_initialState :: [(PlaceIndex String, FilledState)]
fig20_initialState = fig20_mkStates [Full, Empty, Empty, Empty, Empty]

fig20Init :: STM CENetStringy
fig20Init = do
  ps <- fig20_pi
  pure . CENet $ PetriNet ps fig20_t fig20_flow

-- Local type synonym, do not export
type FiringHelper =
  ( "transitionNotFound" .== TransitionNotFound String
      .+ "eventNotEnabled" .== EventNotEnabled String String
      .+ "other" .== String
  )

{- | Given an initial state and a list of transitions paired with their resulting states,
fire the net and make sure everything lines up
-}
fig20_assertFiring :: [(PlaceIndex String, FilledState)] -> [(TransitionIndex String, [(PlaceIndex String, FilledState)])] -> IO ()
fig20_assertFiring initialState statesAndFirings = do
  actual <- atomically $ do
    act <- fig20Init
    markCENet initialState act
    pure act
  frozenActual <- atomically $ freezePlaces (s . getCENet $ actual)
  print frozenActual
  print (t . getCENet $ actual)
  print (f . getCENet $ actual)

  expected <- atomically fig20Init

  let
    -- This folds over the list of firings and expected results,
    -- applies the firing to the "actual" net, and directly applies the expect
    -- marking to the "expected" net.
    -- It accumulates the truth value of `actual == expected` in an All
    foldFunc ::
      All ->
      (TransitionIndex String, [(PlaceIndex String, FilledState)]) ->
      IO All

    foldFunc b (t, states) = do
      eFire <- runExceptT $ mkFiring @FiringHelper actual t
      case eFire of
        Left e -> assertFailure $ show e
        Right fire -> atomically $ do
          fire
          markCENet states expected
          frozenActual <- freezePlaces (s . getCENet $ actual)
          frozenExpected <- freezePlaces (s . getCENet $ expected)
          pure (b <> All (frozenActual == frozenExpected))

  res <- foldM foldFunc (All True) statesAndFirings
  case getAll res of
    False -> assertFailure "fig20_assertFiring: failed\n"
    -- FIXME: Peter, 2025-05-23: Disabled during switch to STM (need a print)
    -- <> ("expected: "
    -- <> show expected
    -- <> "\nactual: "
    -- <> show actual)
    -- )
    True -> pure ()

fig20Tests :: IO TestTree
fig20Tests = do
  fig20Empty <- atomically fig20Init

  pure $
    testGroup
      "fig20"
      [ testCase "fig20 has a valid flow relation" $
          assertBool
            "fig20 flow relation invalid"
            ( isValidFlowRelation . getCENet $ fig20Empty
            )
      , testCase "fig20 fires correctly" $
          fig20_assertFiring fig20_initialState fig20_firings
      ]
