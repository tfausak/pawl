module Pawl.Types.DamagePattern where

import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Recipient as Recipient

-- | CR 614.1a / 615.1: which damage events a replacement or prevention
-- intercepts -- both, since this type is shared. Fog's prevention is
-- (Just Combat, And [], Nothing); Furnace of Rath's replacement is
-- (Nothing, And [], Nothing); Mending Hands' shield is
-- (Nothing, And [], Just the chosen recipient). Nothing means any kind.
--
-- `whatSource` says WHAT the damage's source is (CR 120.1), as a Filter over its
-- characteristics: Luminesce's "black sources and red sources" is
-- `Or [HasColor Black, HasColor Red]`, and CR 609.7b's recheck is what evaluating
-- it at the event rather than at the shield's creation means. `And []` is the
-- trivial predicate, so a pattern that says nothing about the source needs no
-- "any source" arm -- ZoneChangePattern.whatObject's shape, for its reason.
--
-- CR 614.15's "this way" rides the same Filter as `Filter.IsSource`, the atom
-- that asks whether the candidate IS the evaluation's source object: Galvanic
-- Blast's metalcraft clause replaces the damage its own resolution deals and
-- nothing else. A separate identity enum beside the Filter would be a second
-- spelling of one relation (#163).
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
data DamagePattern = MkDamagePattern
  { whichKind :: Maybe DamageKind.DamageKind,
    whatSource :: Filter.Filter Keyword.Keyword,
    whichRecipient :: Maybe Recipient.Recipient
  }
  deriving (Eq, Ord, Show)
