{-# LANGUAGE OverloadedLabels #-}

-- | Module: Data.Petri.CE
-- Description: Condition-Event Systems
--
-- A condition-event system is a petri net where each place may hold at most one token,
-- and a transition is fireable only if all input places have tokens and all output places lack
-- tokens

{- Note (Peter, 2025-05-22): There's a couple of trade-offs we can make for performance vs
diagnostics.
-}


module Data.Petri.CE where

import Prelude

import Data.Petri
    ( Place(..),
      Transition,
      PetriNet,
      TransitionNotFoundLabel,
      tPreset,
      tPostset)
import Data.Petri.Types.Net (PetriNet (s))
import Control.Monad.Error.Class
import Data.Row
import Data.Set qualified as Set
import Data.Set (Set, (\\))
import Data.Monoid (All (All, getAll))
import Data.Kind ( Type )

data FilledState = Empty | Full
  deriving stock (Eq, Show, Ord)

-- Probably want to newtype these eventually
type Event index = Transition index
type Condition index  = Place (index, FilledState)

isEmpty :: Condition index -> Bool
isEmpty (Place (_index, filledState)) = filledState == Empty

isFull :: Condition index -> Bool
isFull (Place (_index, filledState)) = filledState == Full

-- | A Condition-Event net is one where the places carry either one or none tokens.
newtype CENet s t = CENet (PetriNet (s, FilledState) t)
  deriving newtype (Show)

enabled :: forall (m :: Type -> Type) (e :: Row Type) (t :: Type) (s :: Type).
  Ord (s, FilledState)
  => Ord t
  => MonadError (Var e) m
  => (TransitionNotFoundLabel e t)
  =>
  Event t -> CENet s t -> m Bool
enabled event (CENet net) = do
  pre <- net `tPreset` event
  post <- net `tPostset` event
  let
    preFull :: All
    preFull = Set.foldr (\a b -> All (isFull a) <> b) (All True) pre
    postEmpty :: All
    postEmpty = Set.foldr (\a b -> All (isEmpty a) <> b) (All True) post
  pure . getAll $ preFull <> postEmpty


-- TODO: We do this in two passes, but it could be done in one.
fire :: forall (m :: Type -> Type) (e :: Row Type) (s :: Type) (t :: Type).
  MonadError (Var e) m
  => Ord t
  => Ord s
  => TransitionNotFoundLabel e t
  => EventNotEnabledLabel e s t
  => CENet s t
  -> Event t
  -> m (CENet s t)
fire (CENet net) event = do
  --
  pre <- net `tPreset` event
  post <- net `tPostset` event
  let
    preEmpty :: Set (Condition s)
    preEmpty = Set.filter isEmpty pre

    postFull :: Set (Condition s)
    postFull = Set.filter isFull post

  if not (Set.null preEmpty && Set.null postFull)
  then throwError (varyEventNotEnabled event preEmpty postFull)
  else
    let
       unaffectedPlaces = s net \\ pre \\ post
       affectedPlaces =
          Set.union
          (Set.map (\(Place (index, _)) -> Place (index, Empty)) pre)
          (Set.map (\(Place (index, _)) -> Place (index, Full)) post)

       newPlaces = Set.union unaffectedPlaces affectedPlaces
    in
      pure $ CENet (net {s = newPlaces})


--------------------------------------------------------------------------------
-- Errors

data EventNotEnabled s t = EventNotEnabled
  { event :: !(Event t)
  , emptyInputs :: !(Set (Condition s))
  , fullOutputs :: !(Set (Condition s))
  }
  deriving stock (Ord, Show, Eq)

data TransitionState s t = Enabled | NotEnabled (EventNotEnabled s t)
  deriving stock (Ord, Show, Eq)

type EventNotEnabledLabel (r :: Row Type) (s :: Type) (t :: Type) =
  (AllUniqueLabels r, r .! "eventNotEnabled" ≈ EventNotEnabled s t)

varyEventNotEnabled ::
  forall (s :: Type) (t :: Type) (r :: Row Type).
  (EventNotEnabledLabel r s t) =>
  Event t ->
  Set (Condition s) ->
  Set (Condition s) ->
  Var r
varyEventNotEnabled event inputs outputs =
  IsJust #"eventNotEnabled" $ EventNotEnabled event inputs outputs
