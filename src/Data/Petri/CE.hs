{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Module: Data.Petri.CE
Description: Condition-Event Systems

A condition-event system is a petri net where each place may hold at most one token,
and a transition is fireable only if all input places have tokens and all output places lack
tokens
-}
module Data.Petri.CE (
  FilledState (..),
  CENet (CENet, getCENet),
  mkIsEnabled,
  EventNotEnabledLabel,
  EventNotEnabled,
  mkFiring,
) where

import Prelude

import Control.Monad
import Control.Monad.Error.Class (MonadError)
import Data.Map.Strict ((!))
import Data.Petri (
  PetriNet (s),
  PlaceIndex,
  Places (getPlaces),
  TransitionIndex,
  TransitionNotFoundLabel,
  tPostset,
  tPreset,
 )
import Data.Row (
  AllUniqueLabels,
  Row,
  Var,
  pattern IsJust,
  type (.!),
  type (≈),
 )

data FilledState = Empty | Full
  deriving stock (Eq, Show, Ord)

-- | A Condition-Event net is one where the places carry either one or none tokens.
newtype CENet si ti fi = CENet {getCENet :: PetriNet si ti fi FilledState () ()}
  deriving newtype (Eq)

mkIsEnabled ::
  forall
    (m :: Type -> Type)
    (e :: Row Type)
    (si :: Type)
    (ti :: Type)
    (fi :: Type).
  (Ord ti) =>
  (Ord si) =>
  (MonadError (Var e) m) =>
  (TransitionNotFoundLabel e ti) =>
  TransitionIndex ti ->
  CENet si ti fi ->
  m (STM Bool)
mkIsEnabled event (CENet net) = do
  let
    ps = getPlaces (s net)
  pre :: Set (PlaceIndex si) <- net `tPreset` event
  post <- tPostset @m net event

  let
    -- Checks that the TVars in the given places all match some particular state
    -- TODO: We can factor this out
    helper :: FilledState -> Set (PlaceIndex si) -> STM All
    helper state =
      foldM
        ( \b a -> do
            p <- readTVar (ps ! a)
            pure $ All (p == state) <> b
        )
        (All True)

  pure $ do
    preFull <- helper Full pre
    postEmpty <- helper Empty post
    pure $ getAll $ preFull <> postEmpty

-- TODO: We do this in two passes, but it could be done in one.
-- TODO: Make this report the violated conditions
mkFiring ::
  forall (e :: Row Type) (m :: Type -> Type) (si :: Type) (ti :: Type) (fi :: Type).
  (MonadError (Var e) m) =>
  (Ord ti) =>
  (Ord si) =>
  (TransitionNotFoundLabel e ti) =>
  -- FIXME: (EventNotEnabledLabel e si ti) =>
  CENet si ti fi ->
  TransitionIndex ti ->
  m (STM ())
mkFiring cen@(CENet net) event = do
  let
    ps = getPlaces (s net)
  pre <- net `tPreset` event
  post <- net `tPostset` event

  check <- mkIsEnabled event cen

  -- First pass is in "enabled": we first check if the
  -- transition is fireable at all
  pure $ do
    c <- check
    if not c
      then -- If its not, then we do some extra work to report why
      -- FIXME: Peter, 2025-05-23: Going lax on the error reporting while I swtich to STM

      -- let
      --   emptyPres =
      --     Set.fromList . Map.keys $
      --       Map.filterWithKey
      --         (\k a -> k `Set.member` pre && a == Empty)
      --         ps

      --   fullPosts =
      --     Set.fromList . Map.keys $
      --       Map.filterWithKey
      --         (\k a -> k `Set.member` post && a == Full)
      --         ps
      --  in
        error "unimplemented"
      else do
        -- If it is, then we pass over the map again to make
        -- all of the inputs to the transition empty and all outputs full

        mapM_ (\placeIndex -> writeTVar (ps ! placeIndex) Empty) pre
        mapM_ (\placeIndex -> writeTVar (ps ! placeIndex) Full) post

--------------------------------------------------------------------------------
-- Errors

data EventNotEnabled si ti = EventNotEnabled
  { transition :: !(TransitionIndex ti)
  , emptyInputs :: !(Set (PlaceIndex si))
  , fullOutputs :: !(Set (PlaceIndex si))
  }
  deriving stock (Ord, Show, Eq)

data TransitionState s t = Enabled | NotEnabled (EventNotEnabled s t)
  deriving stock (Ord, Show, Eq)

type EventNotEnabledLabel (r :: Row Type) (si :: Type) (ti :: Type) =
  (AllUniqueLabels r, r .! "eventNotEnabled" ≈ EventNotEnabled si ti)

-- FIXME
_varyEventNotEnabled ::
  forall (si :: Type) (ti :: Type) (r :: Row Type).
  (EventNotEnabledLabel r si ti) =>
  TransitionIndex ti ->
  Set (PlaceIndex si) ->
  Set (PlaceIndex si) ->
  Var r
_varyEventNotEnabled event inputs outputs =
  IsJust #"eventNotEnabled" $ EventNotEnabled event inputs outputs
