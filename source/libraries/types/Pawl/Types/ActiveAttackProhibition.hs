module Pawl.Types.ActiveAttackProhibition where

import qualified Pawl.Types.AimedAt as AimedAt
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.RestrictedCreatures as RestrictedCreatures
import qualified Pawl.Types.Timestamp as Timestamp

-- | CR 508.1c / 611.1: a stored, resolution-generated ATTACKING RESTRICTION,
-- held in GameState.attackProhibitions. Netter en-Dal's "target creature
-- can't attack this turn" and Chronomantic Escape's "until your next turn,
-- creatures can't attack you" are the producers.
--
-- Read at Pawl.Engine.CombatRestriction.attackProhibited, which unions a row
-- naming no 'aimedAt' into that module's `cantAttack` beside CR 701.35a's
-- detentions, and at Pawl.Engine.CombatRestriction.cantAttackPlayer, which
-- unions an aimed row into the printed carrier's announcement rows -- so CR
-- 508.1c sees one answer on each axis and Pawl.Engine.Combat never learns which
-- road a restriction took. CR 508.1d's maximization ranges over the candidate
-- list `cantAttack` has already narrowed, which is that rule's "without
-- disobeying any restrictions".
--
-- OUTSIDE the layer system, which is CR 613.11: a restriction on a declaration
-- modifies the rules rather than any object's characteristics, so no
-- Pawl.Types.Modification arm could carry it and Pawl.Engine.Projection never
-- sees one.
--
-- 'affected' holds ids where the printed carrier (Pawl.Types.ForbidAttack) holds
-- an ObjectRef, for Pawl.Types.ActiveBlockProhibition's reason: the ref is read
-- ONCE, as the ability resolves. Its Matching arm is CR 611.2c's carve-out and
-- stays a Filter, re-read at each declaration against the live board.
--
-- `controller` is STORED, Pawl.Types.ActivePlayerEffect's reason: a sorcery is in
-- a graveyard by the time its restriction is read, with no controller to project,
-- and 'aimedAt' asks CR 109.5's "you" of it at every declaration.
--
-- `expiry` decides when a Pawl.Engine.Expiry sweep drops it (CR 514.2, 611.2a,
-- 611.2b); "this turn" arms Expiry.AtCleanup, "until your next turn" arms
-- Expiry.AtTurnOf.
--
-- `timestamp` is stored for ActiveBlockProhibition's reason: CR 613.11 orders by
-- CR 613.7 timestamp, and nothing observes this one because two prohibitions
-- cannot conflict -- CR 508.1c has no degrees.
--
-- No CR 508.1c "unless" gate, because Pawl.Types.ForbidAttack states none for it
-- to carry.
--
-- Runtime-only: card data writes the printed carrier, never one of these. It
-- does have a codec (Pawl.Codec.ActiveAttackProhibition), because a game in
-- progress has to be writable to JSON (#126).
data ActiveAttackProhibition = MkActiveAttackProhibition
  { source :: ObjectId.ObjectId,
    controller :: PlayerId.PlayerId,
    timestamp :: Timestamp.Timestamp,
    expiry :: Expiry.Expiry,
    affected :: RestrictedCreatures.RestrictedCreatures ObjectId.ObjectId,
    -- | Nothing is a restriction on attacking at all.
    aimedAt :: Maybe AimedAt.AimedAt
  }
  deriving (Eq, Ord, Show)
