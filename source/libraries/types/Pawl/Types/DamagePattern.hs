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
-- `whichRecipient` is CR 615.7's "the 'shielded' permanent or player", and
-- Nothing is not a missing answer -- it is EVERY recipient. Fog's "prevent all
-- combat damage that would be dealt this turn" names none, and neither does
-- Furnace of Rath; Mending Hands' "would be dealt to any target" names the one
-- CR 115.4 recipient its resolution chose.
--
-- That Recipient is BAKED by the engine, never authored, exactly as
-- Pawl.Types.PhasePattern.whosePhase is: card data cannot name an ObjectId or a
-- PlayerId, so the only producer is Resolve's PreventNextDamage arm, and the
-- codec accepts one only because it is structural over the record.
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
