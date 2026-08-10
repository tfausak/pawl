module Pawl.Types.ExilePlayPermission where

import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
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
-- rest of that case; this type is what the field holds now that an Effect can
-- write one too.
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
-- Runtime-only: no codec. A permission is written by a resolution and never
-- printed on a card, and Object -- the only thing that holds one -- has no codec
-- either.
data ExilePlayPermission = MkExilePlayPermission
  { player :: PlayerId.PlayerId,
    source :: ObjectId.ObjectId,
    expiry :: Expiry.Expiry
  }
  deriving (Eq, Ord, Show)
