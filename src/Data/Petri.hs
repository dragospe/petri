{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}

-- TODO: Consider IntMap or HashMap for collecting Places/Transitions, but see https://oleg.fi/gists/posts/2021-05-19-dont-default-to-hashmap.html here.
-- I'm wondering whether hashmap would screw us up with serialization/deserilization anywhere.
-- For now, we'll just stick with Data.Map.Strict

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
  Places (Places, getPlaces),
  PlaceIndex (PlaceIndex),
  -- | ** Transitions
  Transition (Transition),
  Transitions (Transitions),
  TransitionIndex (TransitionIndex),
  -- | * Arcs
  FlowRelation (FlowRelation),
  ArcIndex (ArcIndex),
  -- | * Nets
  PetriNet (PetriNet, s, t, f), -- TODO: Make this opaque

  -- | * Queries
  member,
  places,
  transitions,
  flowRelation,
  isValidFlowRelation,
  -- | * Operations
  sPreset,
  tPreset,
  preset,
  sPostset,
  tPostset,
  postset,
  -- | * Errors
  PlaceNotFound,
  PlaceNotFoundLabel,
  TransitionNotFound (TransitionNotFound),
  varyTransitionNotFound,
  TransitionNotFoundLabel,
)
where

import Prelude

import Control.Monad (unless, (>=>))
import Control.Monad.Error.Class (MonadError, throwError)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Petri.Set (setLefts, setRights, u, x)
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
import Data.String (IsString (fromString))

-- -- | Errors
-- -- We use variant based errors. See:
-- --   - (Describing the related issue in haskell) https://www.parsonsmatt.org/2018/11/03/trouble_with_typed_errors.html
-- --   https://target.github.io/row-types/examples/TypedErrors.html

newtype PlaceNotFound (si :: Type) = PlaceNotFound (PlaceIndex si)

type PlaceNotFoundLabel (r :: Row Type) (si :: Type) =
  (AllUniqueLabels r, r .! "placeNotFound" ≈ PlaceNotFound si)

varyPlaceNotFound ::
  forall (si :: Type) (r :: Row Type).
  (PlaceNotFoundLabel r si) =>
  PlaceIndex si ->
  Var r
varyPlaceNotFound si = IsJust #"placeNotFound" (PlaceNotFound si)

newtype TransitionNotFound (ti :: Type) = TransitionNotFound (TransitionIndex ti)
  deriving newtype (Eq, Show)

type TransitionNotFoundLabel (r :: Row Type) (ti :: Type) =
  (AllUniqueLabels r, r .! "transitionNotFound" ≈ TransitionNotFound ti)

varyTransitionNotFound ::
  forall (ti :: Type) (r :: Row Type).
  (TransitionNotFoundLabel r ti) =>
  TransitionIndex ti ->
  Var r
varyTransitionNotFound ti =
  IsJust #"transitionNotFound" $ TransitionNotFound ti

-- Row type synonym
type NodeIndex (si :: Type) (ti :: Type) =
  ( "place" .== PlaceIndex si .+ "transition" .== TransitionIndex ti
  )

--------------------------------------------------------------------------------
-- Place

newtype PlaceIndex si = PlaceIndex si
  deriving newtype (Ord, Eq, Show)

-- | Allows us to use {-# LANGUAGE OverloadedStrings #-} to make specifying
-- these less annoying
instance IsString (PlaceIndex String) where
  fromString = PlaceIndex

newtype Place si s = Place (PlaceIndex si, s)
  deriving newtype (Ord, Eq, Show)

newtype Places si s = Places {getPlaces :: Map (PlaceIndex si) s}
  deriving newtype (Ord, Eq, Show)

--------------------------------------------------------------------------------
-- Transition

newtype TransitionIndex ti = TransitionIndex ti
  deriving newtype (Ord, Eq, Show)

instance IsString (TransitionIndex String) where
  fromString = TransitionIndex

newtype Transition ti t = Transition (TransitionIndex ti, t)
  deriving newtype (Ord, Eq, Show)

newtype Transitions ti t = Transitions {getTransitions :: Map (TransitionIndex ti) t}
  deriving newtype (Ord, Eq, Show)

--------------------------------------------------------------------------------
-- Flow Relation

newtype ArcIndex fi = ArcIndex fi
  deriving newtype (Ord, Eq, Show)

instance IsString (ArcIndex String) where
  fromString = ArcIndex

{- | The "Flow relation" is a binary relation $(S x T) U (T x S)$
TODO: Switch this to use variants?
-}
newtype FlowRelation (si :: Type) (ti :: Type) (fi :: Type)
  = FlowRelation
      ( Map
          (ArcIndex fi)
          ( Either
              (PlaceIndex si, TransitionIndex ti)
              (TransitionIndex ti, PlaceIndex si)
          )
      )
  deriving newtype (Eq, Ord, Show)

-- FIXME: This uses the by-the-book definition. There's probably a more efficient way.
isValidFlowRelation ::
  forall si ti fi s t.
  (Ord si) =>
  (Ord ti) =>
  PetriNet si ti fi s t ->
  Bool
isValidFlowRelation
  ( PetriNet
      { s = (Places s')
      , t = (Transitions t')
      , f = (FlowRelation f')
      }
    ) = f'' `Set.isSubsetOf` ((s'' `x` t'') `u` (t'' `x` s''))
    where
      s'' :: Set (PlaceIndex si)
      s'' = Set.fromList . Map.keys $ s'

      t'' :: Set (TransitionIndex ti)
      t'' = Set.fromList . Map.keys $ t'

      f'' :: Set (Either (PlaceIndex si, TransitionIndex ti) (TransitionIndex ti, PlaceIndex si))
      f'' = Set.fromList . Map.elems $ f'

--------------------------------------------------------------------------------
-- Net

{- | A PetriNet as a new type. Note that, since this is being used on the front end,
| its not necessarily true that the flow relation is valid.
-}
data PetriNet (si :: Type) (ti :: Type) (fi :: Type) (s :: Type) (t :: Type) = PetriNet
  { s :: Places si s
  -- ^ "S" elements -- typically "places"
  , t :: Transitions ti t
  -- ^ "T" elements -- typically "transitions"
  , f :: FlowRelation si ti fi
  -- ^ "Flow relation" -- input/output mappings
  }
  deriving stock (Eq, Ord, Show)

places :: PetriNet si ti fi s t -> Places si s
places = s

transitions :: PetriNet si ti fi s t -> Transitions ti t
transitions = t

flowRelation :: PetriNet si ti fi s t -> FlowRelation si ti fi
flowRelation = f

--------------------------------------------------------------------------------
-- Petri Net

sMember ::
  forall (si :: Type) (ti :: Type) (fi :: Type) (s :: Type) (t :: Type).
  (Ord si) =>
  PlaceIndex si ->
  PetriNet si ti fi s t ->
  Bool
sMember s' (PetriNet {s}) = Map.member s' (getPlaces s)

tMember ::
  forall (si :: Type) (ti :: Type) (fi :: Type) (s :: Type) (t :: Type).
  (Ord ti) =>
  TransitionIndex ti ->
  PetriNet si ti fi s t ->
  Bool
tMember t' (PetriNet {t}) = Map.member t' (getTransitions t)

member ::
  forall (si :: Type) (ti :: Type) (fi :: Type) (s :: Type) (t :: Type).
  (Ord si) =>
  (Ord ti) =>
  Var (NodeIndex si ti) ->
  PetriNet si ti fi s t ->
  Bool
member node net =
  switch node $
    #place .== (\p -> p `sMember` net)
      .+ #transition .== (\t -> t `tMember` net)

throwGuard :: forall e (m :: Type -> Type). (MonadError e m) => Bool -> e -> m ()
throwGuard b error =
  unless b (throwError error)

guardSMember ::
  forall
    (si :: Type)
    (ti :: Type)
    (fi :: Type)
    (s :: Type)
    (t :: Type)
    (m :: Type -> Type)
    (r :: Row Type).
  (Ord si) =>
  (PlaceNotFoundLabel r si) =>
  (MonadError (Var r) m) =>
  PlaceIndex si ->
  PetriNet si ti fi s t ->
  m ()
guardSMember placeIndex net =
  throwGuard (sMember placeIndex net) $ varyPlaceNotFound placeIndex

guardTMember ::
  forall
    (si :: Type)
    (ti :: Type)
    (fi :: Type)
    (s :: Type)
    (t :: Type)
    (m :: Type -> Type)
    (r :: Row Type).
  (Ord ti) =>
  (TransitionNotFoundLabel r ti) =>
  (MonadError (Var (r)) m) =>
  TransitionIndex ti ->
  PetriNet si ti fi s t ->
  m ()
guardTMember t net =
  throwGuard (tMember t net) $ varyTransitionNotFound t

-- -- -- -- * Presets and Postsets

-- FIXME: the performance of this can almost certainly be improved.
sPreset ::
  forall
    (si :: Type)
    (ti :: Type)
    (fi :: Type)
    (s :: Type)
    (t :: Type)
    (m :: Type -> Type)
    (e :: Row Type).
  (Ord si) =>
  (Ord ti) =>
  (PlaceNotFoundLabel e si) =>
  (MonadError (Var e) m) =>
  PetriNet si ti fi s t ->
  PlaceIndex si ->
  m (Set (TransitionIndex ti))
sPreset net@(PetriNet {f = FlowRelation f'}) s = do
  guardSMember s net
  pure
    $ Set.map fst
      . Set.filter (\tToS -> snd tToS == s)
      . setRights
      . Set.fromList
      . Map.elems
    $ f'

tPreset ::
  forall
    (si :: Type)
    (ti :: Type)
    (fi :: Type)
    (s :: Type)
    (t :: Type)
    (m :: Type -> Type)
    (e :: Row Type).
  (Ord si) =>
  (Ord ti) =>
  (TransitionNotFoundLabel e ti) =>
  (MonadError (Var e) m) =>
  PetriNet si ti fi s t ->
  TransitionIndex ti ->
  m (Set (PlaceIndex si))
tPreset net@(PetriNet {f = FlowRelation f'}) t = do
  guardTMember t net
  pure
    $ Set.map fst
      . Set.filter (\sToT -> snd sToT == t)
      . setLefts
      . Set.fromList
      . Map.elems
    $ f'

{- | Give a node (either place or transition), return all nodes in the preset.

Will throw an error if the given node is not found in the flow relation.
-}
preset ::
  forall
    (si :: Type)
    (ti :: Type)
    (fi :: Type)
    (s :: Type)
    (t :: Type)
    (m :: Type -> Type)
    (e :: Row Type).
  (Ord si) =>
  (Ord ti) =>
  (TransitionNotFoundLabel e ti) =>
  (PlaceNotFoundLabel e si) =>
  (MonadError (Var e) m) =>
  PetriNet si ti fi s t ->
  Var (NodeIndex si ti) ->
  m (Either (Set (PlaceIndex si)) (Set (TransitionIndex ti)))
preset net node =
  switch node $
    #place
      .== (sPreset net >=> (pure . Right))
      .+ #transition .== (tPreset net >=> (pure . Left))

sPostset ::
  forall
    (si :: Type)
    (ti :: Type)
    (fi :: Type)
    (s :: Type)
    (t :: Type)
    (m :: Type -> Type)
    (e :: Row Type).
  (Ord si) =>
  (Ord ti) =>
  (PlaceNotFoundLabel e si) =>
  (MonadError (Var e) m) =>
  PetriNet si ti fi s t ->
  PlaceIndex si ->
  m (Set (TransitionIndex ti))
sPostset net@(PetriNet {f = FlowRelation f'}) s = do
  guardSMember s net
  pure
    $ Set.map snd
      . Set.filter (\sToT -> fst sToT == s)
      . setLefts
      . Set.fromList
      . Map.elems
    $ f'

tPostset ::
  forall
    (si :: Type)
    (ti :: Type)
    (fi :: Type)
    (s :: Type)
    (t :: Type)
    (m :: Type -> Type)
    (e :: Row Type).
  (Ord si) =>
  (Ord ti) =>
  (TransitionNotFoundLabel e ti) =>
  (MonadError (Var e) m) =>
  PetriNet si ti fi s t ->
  TransitionIndex ti ->
  m (Set (PlaceIndex si))
tPostset net@(PetriNet {f = FlowRelation f'}) t = do
  guardTMember t net
  pure
    $ Set.map snd
      . Set.filter (\tToS -> fst tToS == t)
      . setRights
      . Set.fromList
      . Map.elems
    $ f'

{- | Give a  nodes (either place or transition), return all nodes in the postset.
Will throw an error if the node is not in the flow relation
-}
postset ::
  forall
    (si :: Type)
    (ti :: Type)
    (fi :: Type)
    (s :: Type)
    (t :: Type)
    (m :: Type -> Type)
    (e :: Row Type).
  (Ord si) =>
  (Ord ti) =>
  (TransitionNotFoundLabel e ti) =>
  (PlaceNotFoundLabel e si) =>
  (MonadError (Var e) m) =>
  PetriNet si ti fi s t ->
  Var (NodeIndex si ti) ->
  m (Either (Set (PlaceIndex si)) (Set (TransitionIndex ti)))
postset net node =
  switch node $
    #place .== (sPostset net >=> (pure . Right))
      .+ #transition .== (tPostset net >=> (pure . Left))
