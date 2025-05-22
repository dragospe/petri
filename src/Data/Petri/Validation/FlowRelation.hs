module Data.Petri.Validation.FlowRelation (isValidFlowRelation) where

import Prelude

import Data.Petri.Set (u, x)
import Data.Petri.Types.FlowRelation (FlowRelation(..))
import Data.Petri.Types.Net (PetriNet(PetriNet, s, t, f))
import Data.Set qualified as Set

-- newtype InvalidFlowRelation s t = InvalidFlowRelation (FlowRelation s t)

-- -- (inj _InvalidFlowRelation (InvalidFlowRelation f))
-- _InvalidFlowRelation :: Proxy "InvalidFlowRelation"
-- _InvalidFlowRelation = Proxy

isValidFlowRelation
  :: forall s t
   . Ord s
  => Ord t
  => PetriNet s t
  -> Bool
isValidFlowRelation ( PetriNet
       { s
       , t
       , f = (FlowRelation f')
       }
   ) = f' `Set.isSubsetOf` ((s `x` t) `u` (t `x` s))
