-- | Miscellaneous set theory helper function for working with nets
module Data.Petri.Set (u, x, setLefts, setRights) where

import Prelude

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Kind (Type)

-- | The heterogenous, disjoint set union function we need specifically for the petri spec.
-- | call as
-- | @
-- |  a `u` b
-- | @
u
  :: forall (a :: Type) (b :: Type)
   . Ord a
  => Ord b
  => Set a
  -> Set b
  -> Set (Either a b)
u a b =
  let
    a' = Left `Set.map` a
    b' = Right `Set.map` b
  in
    a' `Set.union` b'

-- | Heterogenous, disjoint cartesian product
x
  :: forall (a :: Type) (b :: Type)
   . Ord a
  => Ord b
  => Set a
  -> Set b
  -> Set ( a, b)
x a b =
  let
    l :: [ ( a, b)]
    l = do
      as <- Set.toList a
      bs <- Set.toList b
      pure (as, bs)
  in
    Set.fromList l


-- QUESTION: Should I use foldr or foldl here?
setLefts
  :: forall (a :: Type) (b :: Type)
   . Ord a
  -- => Ord b
  => Set (Either a b)
  -> Set a
setLefts =
  foldr
    ( \ee acc -> case ee of
        Left e -> Set.insert e acc
        Right _ -> acc
    )
    Set.empty

setRights
  :: forall (a :: Type) (b :: Type)
   .
   Ord b
  => Set (Either a b)
  -> Set b
setRights =
  foldr
    ( \ee acc -> case ee of
        Right e -> Set.insert e acc
        Left _ -> acc
    )
    Set.empty
