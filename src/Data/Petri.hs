{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}

{- | We newtype places, transitions, and flow relations for two reasons:
|  - Ensure that places are transitions are indeed disjoint sets. For
|    a net N = (S, T, F), it may occur that (In haskell) we have $S :: Set s$
|    and $T :: Set t$, where $s = t$, and this leads to a risk of something like
|    set union accidentally clobbering elements
|  - Clarity in intent. Even if we were dilligent in doing something like using
|    @Either@ to ensure that they weren't clobbered, we could end up in a sitution
|    with @Either Int Int@ and not be sure of the ordering.
-}
module Data.Petri (
  -- | * Nodes
  -- | ** Places
  Place (Place),
  -- | ** Transitions
  Transition (Transition),
  -- | * Arcs
  FlowRelation (FlowRelation),
  -- | * Nets
  PetriNet,
  -- | * Queries
  member,
  -- | * Operations
  preset,
  postset,
  -- | * Errors
  PlaceNotFound,
  TransitionNotFound,
)
where

import Prelude

import Control.Monad ((>=>), unless)
import Control.Monad.Error.Class (MonadError, throwError)
import Data.Kind (Type)
import Data.Petri.Set (setLefts, setRights)
import Data.Petri.Types.FlowRelation (FlowRelation (FlowRelation))
import Data.Petri.Types.Net (PetriNet (PetriNet, f, s, t))
import Data.Petri.Types.Place (Place (Place))
import Data.Petri.Types.Transition (Transition (Transition))
import Data.Row (
  AllUniqueLabels,
  Row,
  switch,
  (.+),
  (.==),
  type (.!),
  type (.+),
  type (.==),
  type (≈),
 )
import Data.Row.Variants (Var, pattern IsJust)
import Data.Set (Set)
import Data.Set qualified as Set

-- -- | Errors
-- -- We use variant based errors. See:
-- --   - (Describing the related issue in haskell) https://www.parsonsmatt.org/2018/11/03/trouble_with_typed_errors.html
-- --   https://target.github.io/row-types/examples/TypedErrors.html

newtype PlaceNotFound (s :: Type) = PlaceNotFound (Place s)

type PlaceNotFoundLabel (r :: Row Type) (s :: Type) =
  (AllUniqueLabels r, r .! "placeNotFound" ≈ PlaceNotFound s)

varyPlaceNotFound ::
  forall (s :: Type) (r :: Row Type).
  (PlaceNotFoundLabel r s) =>
  Place s ->
  Var r
varyPlaceNotFound s = IsJust #"placeNotFound" (PlaceNotFound s)

newtype TransitionNotFound (t :: Type) = TransitionNotFound (Transition t)

type TransitionNotFoundLabel (r :: Row Type) (t :: Type) =
  (AllUniqueLabels r, r .! "transitionNotFound" ≈ TransitionNotFound t)

varyTransitionNotFound ::
  forall (t :: Type) (r :: Row Type).
  (TransitionNotFoundLabel r t) =>
  Transition t ->
  Var r
varyTransitionNotFound t =
  IsJust #"transitionNotFound" $ TransitionNotFound t

-- Row type synonym
type Node (s :: Type) (t :: Type) =
  ( "place" .== Place s .+ "transition" .== Transition t
  )

-- -- --------------------------------------------------------------------------------
-- -- -- Petri Net

-- {- | A valid petri net with three places, three transitions, and two arcs
-- (Partial because we can't prove that the functor instance doesn't change
-- the number of elements). There's probably a better way to do this?
-- -}
-- testSpec :: PetriNet String Int
-- testSpec =
--   let
--     [p1, p2, p3] = Place <$> ["one", "two", "three"]

--     [t1, t2, t3] = Transition <$> [2, 3, 4]

--     [f1, f2] = [Left $ (p1, t1), Right $ (t1, p3)]
--    in
--     PetriNet
--       { s = Set.fromList [p1, p2, p3]
--       , t = Set.fromList [t1, t2, t3]
--       , f = FlowRelation $ Set.fromList [f1, f2]
--       }

sMember ::
  forall (s :: Type) (t :: Type).
  (Ord s) =>
  Place s ->
  PetriNet s t ->
  Bool
sMember s' (PetriNet {s}) = Set.member s' s

tMember ::
  forall (s :: Type) (t :: Type).
  (Ord t) =>
  Transition t ->
  PetriNet s t ->
  Bool
tMember t' (PetriNet {t}) = Set.member t' t

member ::
  forall (s :: Type) (t :: Type).
  (Ord s) =>
  (Ord t) =>
  Var (Node s t) ->
  PetriNet s t ->
  Bool
member node net =
  switch node $
    #place .== (\p -> p `sMember` net)
      .+ #transition .== (\t -> t `tMember` net)

throwGuard :: forall e (m :: Type -> Type). (MonadError e m) => Bool -> e -> m ()
throwGuard b error =
  unless b (throwError error)

guardSMember ::
  forall (s :: Type) (t :: Type) (m :: Type -> Type) (r :: Row Type).
  (Ord s) =>
  (PlaceNotFoundLabel r s) =>
  (MonadError (Var r) m) =>
  Place s ->
  PetriNet s t ->
  m ()
guardSMember s net =
  throwGuard (sMember s net) $ varyPlaceNotFound s

guardTMember ::
  forall (s :: Type) (t :: Type) (m :: Type -> Type) (r :: Row Type).
  (Ord t) =>
  (TransitionNotFoundLabel r t) =>
  (MonadError (Var (r)) m) =>
  Transition t ->
  PetriNet s t ->
  m ()
guardTMember t net =
  throwGuard (tMember t net) $ varyTransitionNotFound t

-- -- -- -- * Presets and Postsets

sPreset ::
  forall (s :: Type) (t :: Type) (m :: Type -> Type) (e :: Row Type).
  (Ord s) =>
  (Ord t) =>
  (PlaceNotFoundLabel e s) =>
  (MonadError (Var e) m) =>
  PetriNet s t ->
  Place s ->
  m (Set (Transition t))
sPreset net@(PetriNet {f = FlowRelation f'}) s = do
  guardSMember s net
  pure $ Set.map fst . Set.filter (\tToS -> snd tToS == s) . setRights $ f'

tPreset ::
  forall (s :: Type) (t :: Type) (m :: Type -> Type) (e :: Row Type).
  (Ord s) =>
  (Ord t) =>
  (TransitionNotFoundLabel e t) =>
  (MonadError (Var e) m) =>
  PetriNet s t ->
  Transition t ->
  m (Set (Place s))
tPreset net@(PetriNet {f = FlowRelation f'}) t = do
  guardTMember t net
  pure $ Set.map fst . Set.filter (\sToT -> snd sToT == t) . setLefts $ f'

{- | Give a node (either place or transition), return all nodes in the preset.

Will throw an error if the given node is not found in the flow relation.
-}
preset ::
  forall (s :: Type) (t :: Type) (m :: Type -> Type) (e :: Row Type).
  (Ord s) =>
  (Ord t) =>
  (TransitionNotFoundLabel e t) =>
  (PlaceNotFoundLabel e s) =>
  (MonadError (Var e) m) =>
  PetriNet s t ->
  Var (Node s t) ->
  m (Either (Set (Place s)) (Set (Transition t)))
preset net node =
  switch node $
    #place
      .== (sPreset net >=> (pure . Right))
      .+ #transition .== (tPreset net >=> (pure . Left))

sPostset ::
  forall (s :: Type) (t :: Type) (m :: Type -> Type) (e :: Row Type).
  (Ord s) =>
  (Ord t) =>
  (PlaceNotFoundLabel e s) =>
  (MonadError (Var e) m) =>
  PetriNet s t ->
  Place s ->
  m (Set (Transition t))
sPostset net@(PetriNet {f = FlowRelation f'}) s = do
  guardSMember s net
  pure $ Set.map snd . Set.filter (\sToT -> fst sToT == s) . setLefts $ f'

tPostset ::
  forall (s :: Type) (t :: Type) (m :: Type -> Type) (e :: Row Type).
  (Ord s) =>
  (Ord t) =>
  (TransitionNotFoundLabel e t) =>
  (MonadError (Var e) m) =>
  PetriNet s t ->
  Transition t ->
  m (Set (Place s))
tPostset net@(PetriNet {f = FlowRelation f'}) t = do
  guardTMember t net
  pure $ Set.map snd . Set.filter (\tToS -> fst tToS == t) . setRights $ f'

{- | Give a  nodes (either place or transition), return all nodes in the postset.
Will throw an error if the node is not in the flow relation
-}
postset ::
  forall (s :: Type) (t :: Type) (m :: Type -> Type) (e :: Row Type).
  (Ord s) =>
  (Ord t) =>
  (TransitionNotFoundLabel e t) =>
  (PlaceNotFoundLabel e s) =>
  (MonadError (Var e) m) =>
  PetriNet s t ->
  Var (Node s t) ->
  m (Either (Set (Place s)) (Set (Transition t)))
postset net node =
  switch node $
    #place .== (sPostset net >=> (pure . Right))
      .+ #transition .== (tPostset net >=> (pure . Left))
