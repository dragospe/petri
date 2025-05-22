module Data.Petri.Types.FlowRelation (FlowRelation (FlowRelation)) where

import Prelude

import Data.Petri.Types.Place (Place)
import Data.Petri.Types.Transition (Transition)
import Data.List (intercalate)
import Data.Kind (Type)
import Data.Set as Set

--------------------------------------------------------------------------------
-- Flow Relation
-- | The "Flow relation" is a binary relation $(S x T) U (T x S)$
newtype FlowRelation (s :: Type) (t :: Type) = FlowRelation
  ( Set
      ( Either
          ( Place s, Transition t)
          ( Transition t, Place s)
      )
  )



instance (Show s, Show t) => Show (FlowRelation s t) where
  show (FlowRelation f) =
    let
      show' (Left (s, t)) = show s <> " -> " <> show t
      show' (Right (t, s)) = show t <> " -> " <> show s

    in
      "{" <> intercalate ", " (show' <$> Set.toList f) <> "}"
