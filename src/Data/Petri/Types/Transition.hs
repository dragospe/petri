module Data.Petri.Types.Transition (Transition (Transition)) where

import Prelude

newtype Transition t = Transition t
  deriving newtype (Ord, Eq)

instance Show t => Show (Transition t) where
  -- | Wraps a transition in []
  show (Transition t) = "[" <> show t <> "]"

