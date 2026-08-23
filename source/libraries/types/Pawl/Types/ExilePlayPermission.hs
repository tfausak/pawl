module Pawl.Types.ExilePlayPermission where

import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayPermissionOrigin as PlayPermissionOrigin
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 601.3: "a player can begin to cast a spell only if a rule or effect
-- allows that player to cast it" -- one such effect's permission, held on the
-- object it is about (Object.playableFromExile) rather than in a list on the
-- game.
--
-- OBJECT-BORNE, and the rules put it there. CR 715.3d says "for as long as THAT
-- CARD remains exiled, THAT PLAYER may play it" -- one card, one player -- and
-- CR 400.7 then ends it for free, since the incarnation that carries it stops
-- existing the moment the card leaves exile. Object's own haddock argues the
-- rest of that case; this type is what the field holds, whether rule 715.3d
-- wrote it or an Effect did -- and `origin` below is which, because one clause
-- of the rules turns on the difference.
--
-- `player` is CR 109.5's "you": the controller of the spell or ability whose
-- resolution granted the permission, baked in at that moment. Victor Mancha,
-- Runaway prints "YOU may play it", and every other producer surveyed grants it
-- to its controller or to the exiled card's owner; no opcode field names a third
-- party, because no card in this pool asks for one.
--
-- `source` is the object the granting effect came from, and it is load-bearing
-- rather than bookkeeping: Pawl.Engine.Expiry.sweepConditional evaluates an
-- Expiry.While through Filter.contextFor (Just player) (Just source), so a
-- duration whose Condition counts Filter.IsSource -- which is exactly how CR
-- 611.2b's "for as long as you control this creature" is spelled -- has nothing
-- to resolve against without it and would silently count zero, ending the
-- permission on the first sweep. ContinuousEffect.source and
-- ActivePlayerEffect.source are stored for the same reason.
--
-- `expiry` is CR 611.2a's stated duration, armed by Pawl.Engine.Expiry.arm.
-- Expiry.Never is that rule's default for a permission that states none -- CR
-- 715.3d's Adventure permission takes it, and CR 400.7 rather than a sweep is
-- what ends that one.
--
-- `spending` is CR 118.14's "mana of any type can be spent to cast that spell",
-- which Dire Fleet Daredevil prints in the same sentence as the permission --
-- and that rule puts it here rather than on the player: "if that effect also
-- gives a player permission to cast spells, this applies only to mana that
-- player spends to cast spells that way". One permission, one card, one player,
-- so the rider expires exactly when the permission does and needs no sweep of
-- its own. CR 609.4b is the limit on what it may do -- it changes neither the
-- cost nor what mana was actually spent. Pawl.Engine.Cast.spendingFor is the one
-- place that reads this field, and Pawl.Engine.Mana.relax is what acts on the
-- value it hands back.
--
-- `origin` is which rule granted this permission, and CR 715.3d's closing clause
-- is the one place it is read: "it can't be cast as an Adventure this way,
-- ALTHOUGH OTHER EFFECTS that allow a player to cast it may allow a player to
-- cast it as an Adventure". Without it the Adventure exclusion narrows every
-- permission this field can hold, where the rule scopes it to 715.3d's own --
-- Pawl.Engine.Cast.grantedByAdventureRule -- the helper permitsCastFromExile
-- calls in its Adventure conjunct -- is the only reader, and Pawl.CastSpec's
-- "CR 715.3d another effect's permission allows the Adventure half" against
-- Pawl.AdventureSpec's "CR 715.3d from exile the creature is castable and the
-- Adventure is not" is the pair that proves it.
--
-- Runtime-only: a permission is written by a resolution and never printed on a
-- card. It does have a codec (Pawl.Codec.ExilePlayPermission), because a game in
-- progress has to be writable to JSON (#126), and so does Object, which is the
-- only thing that holds one.
data ExilePlayPermission = MkExilePlayPermission
  { player :: PlayerId.PlayerId,
    source :: ObjectId.ObjectId,
    expiry :: Expiry.Expiry,
    spending :: ManaSpending.ManaSpending,
    origin :: PlayPermissionOrigin.PlayPermissionOrigin
  }
  deriving (Eq, Ord, Show)
