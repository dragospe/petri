module Data.Petri.Types.Place (Place (Place)) where

import Prelude

newtype Place s = Place s
  deriving newtype (Ord, Eq)

instance Show s => Show (Place s) where
  -- | Wraps a place in ()
  show (Place s) = "(" <> show s <> ")"
