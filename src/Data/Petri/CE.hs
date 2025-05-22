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
  enabled,
  EventNotEnabledLabel,
  EventNotEnabled,
  fire,
) where

import Prelude

import Control.Monad.Error.Class (MonadError, throwError)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map
import Data.Monoid (All (All, getAll))
import Data.Petri (
  PetriNet (s),
  PlaceIndex,
  Places (Places, getPlaces),
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
import Data.Set (Set)
import Data.Set qualified as Set

data FilledState = Empty | Full
  deriving stock (Eq, Show, Ord)

-- | A Condition-Event net is one where the places carry either one or none tokens.
newtype CENet si ti fi = CENet {getCENet :: PetriNet si ti fi FilledState ()}
  deriving newtype (Show, Eq, Ord)

enabled ::
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
  m Bool
enabled event (CENet net) = do
  pre <- net `tPreset` event
  post <- net `tPostset` event
  let
    ps = getPlaces (s net)
    preFull :: All
    preFull = Set.foldr (\a b -> All ((ps ! a) == Full) <> b) (All True) pre
    postEmpty :: All
    postEmpty = Set.foldr (\a b -> All ((ps ! a) == Empty) <> b) (All True) post
  pure . getAll $ preFull <> postEmpty

-- TODO: We do this in two passes, but it could be done in one.
fire ::
  forall (m :: Type -> Type) (e :: Row Type) (si :: Type) (ti :: Type) (fi :: Type).
  (MonadError (Var e) m) =>
  (Ord ti) =>
  (Ord si) =>
  (TransitionNotFoundLabel e ti) =>
  (EventNotEnabledLabel e si ti) =>
  CENet si ti fi ->
  TransitionIndex ti ->
  m (CENet si ti fi)
fire cen@(CENet net) event = do
  let
    ps = getPlaces . s $ net
  pre <- net `tPreset` event
  post <- net `tPostset` event

  -- First pass is in "enabled": we first check if the
  -- transition is fireable at all
  b <- enabled event cen
  if not b
    then -- If its not, then we do some extra work to report why

      let
        emptyPres =
          Set.fromList . Map.keys $
            Map.filterWithKey
              (\k a -> k `elem` pre && a == Empty)
              ps

        fullPosts =
          Set.fromList . Map.keys $
            Map.filterWithKey
              (\k a -> k `elem` post && a == Full)
              ps
       in
        throwError $ varyEventNotEnabled event emptyPres fullPosts
    else -- If it is, then we pass over the map again to make
    -- all of the inputs to the transition empty and all outputs full

      let
        mkEmpty ::
          Map (PlaceIndex si) FilledState ->
          PlaceIndex si ->
          Map (PlaceIndex si) FilledState
        mkEmpty m k = Map.update (const (Just Empty)) k m

        emptiedPres :: Map (PlaceIndex si) FilledState
        emptiedPres = foldl' mkEmpty ps pre

        mkFull ::
          Map (PlaceIndex si) FilledState ->
          PlaceIndex si ->
          Map (PlaceIndex si) FilledState
        mkFull m k = Map.update (const (Just Full)) k m

        filledPosts :: Map (PlaceIndex si) FilledState
        filledPosts = foldl' mkFull emptiedPres post
       in
        pure $ CENet (net {s = Places filledPosts})

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

varyEventNotEnabled ::
  forall (si :: Type) (ti :: Type) (r :: Row Type).
  (EventNotEnabledLabel r si ti) =>
  TransitionIndex ti ->
  Set (PlaceIndex si) ->
  Set (PlaceIndex si) ->
  Var r
varyEventNotEnabled event inputs outputs =
  IsJust #"eventNotEnabled" $ EventNotEnabled event inputs outputs
