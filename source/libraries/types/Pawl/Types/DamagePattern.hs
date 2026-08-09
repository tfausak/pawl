module Pawl.Types.DamagePattern where

import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SourceRelation as SourceRelation

-- | CR 614.1a / 615.1: which damage events a replacement or prevention
-- intercepts -- both, since this type is shared. Fog's prevention is
-- (Just Combat, AnySource, Nothing); Furnace of Rath's replacement is
-- (Nothing, AnySource, Nothing); Mending Hands' shield is
-- (Nothing, AnySource, Just the chosen recipient). Nothing means any kind.
--
-- `whichSource` is what keys a self-replacement to its own resolution's damage
-- (CR 614.15's "this way"): Galvanic Blast's metalcraft clause is TheSource, and
-- every other damage pattern in the pool -- Fog's prevention, Furnace of Rath's
-- "if A SOURCE would deal damage" -- is AnySource.
--
-- `whichRecipient` is the permanent or player a prevention shield covers -- CR
-- 615.7's, and CR 615.3's unbounded one -- and Nothing means EVERY recipient
-- rather than a missing answer. Mending Hands and Selfless Squire each name the
-- one CR 115.4 recipient their resolution chose; Fog and Furnace of Rath name
-- none.
--
-- That Recipient is BAKED by the engine, never authored, exactly as
-- Pawl.Types.PhasePattern.whosePhase is: card data cannot name an ObjectId or a
-- PlayerId, so the only producers are Resolve's three arms that bake one
-- (PreventNextDamage, PreventAllDamage and RedirectDamage), which share one
-- `installDamageRow`. RedirectDamage is also the one that names a KIND -- Turn
-- the Tables' "all combat damage" -- where the two prevention arms name none.
--
-- Not implemented: a CARD-PRINTED recipient condition, which is what a static
-- redirection ability needs -- "all damage that would be dealt to you is dealt
-- to this creature instead" (Palisade Giant, Pariah) (#1054).
--
-- Not implemented: CR 615.1's shields that name a SOURCE by characteristic
-- ("a red source of your choice", Circle of Protection: Red) rather than by
-- identity (#588).
data DamagePattern = MkDamagePattern
  { whichKind :: Maybe DamageKind.DamageKind,
    whichSource :: SourceRelation.SourceRelation,
    whichRecipient :: Maybe Recipient.Recipient
  }
  deriving (Eq, Ord, Show)
