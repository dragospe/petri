module Data.Petri.Types.Net (PetriNet (PetriNet, s, t, f)) where

import Prelude

import Data.Kind (Type)
import Data.Petri.Types.FlowRelation (FlowRelation)
import Data.Petri.Types.Place (Place)
import Data.Petri.Types.Transition (Transition)
import Data.Set (Set)

-- | A PetriNet as a new type. Note that, since this is being used on the front end,
-- | its not necessarily true that the flow relation is valid.
data PetriNet (s :: Type) (t :: Type) = PetriNet
  { s :: Set (Place s)
  -- ^ "S" elements -- typically "places"
  , t :: Set (Transition t)
  -- ^ "T" elements -- typically "transitions"
  , f :: FlowRelation s t
  -- ^ "Flow relation" -- input/output mappings
  }

instance (Show s, Show t) => Show (PetriNet s t) where
  show (PetriNet { s, t, f }) =
    "S: " <> show s <> "\nT: " <> show t <> "\nF: " <> show f

-- places :: forall (s :: Type) (t :: Type). PetriNet s t -> Places s
-- places (PetriNet { s }) = s

-- transitions :: forall (s :: Type) (t :: Type). PetriNet s t -> Transitions t
-- transitions (PetriNet { t }) = t
