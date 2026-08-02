module Pawl.Types.DamagePattern where

import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.SourceRelation as SourceRelation

-- | CR 615.1: which damage events a replacement or prevention intercepts. Fog is
-- (Just Combat, AnySource). Nothing means any kind. CR 615's shields that name an
-- AMOUNT or a RECIPIENT are P9's to add; this carries the minimum Fog needs, plus
-- the source scoping CR 614.15 needs.
--
-- `whichSource` is what keys a self-replacement to its own resolution's damage
-- (CR 614.15's "this way"): Galvanic Blast's metalcraft clause is TheSource, and
-- every other damage pattern in the pool -- Fog's prevention, Furnace of Rath's
-- "if A SOURCE would deal damage" -- is AnySource.
data DamagePattern = MkDamagePattern
  { whichKind :: Maybe DamageKind.DamageKind,
    whichSource :: SourceRelation.SourceRelation
  }
  deriving (Eq, Ord, Show)
