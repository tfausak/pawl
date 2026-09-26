-- CR 702.95: soulbond, the keyword ability that pairs two creatures and the
-- record that pairing leaves on both of them.
--
-- ONE RULE, split the way rule 702.95 splits itself. Rule 702.95a's two triggered
-- abilities are minted by Pawl.Engine.Keyword, with everything else here: `pair`
-- is rule 702.95c's recheck and rule 702.95d's "only one other creature", run as
-- the opcode applies, and `endWhenBroken` is rule 702.95e's three endings, swept
-- where Pawl.Engine.Engine settles.
--
-- A SWEEP AND NOT A LIVE READ, Pawl.Engine.Ring's posture and for its reason: CR
-- 702.95b makes the pairing a record rather than a characteristic, so a reader
-- asks Object.paired, and the sweep is what keeps that record from outliving what
-- rule 702.95e ends. The sweep is SYMMETRIC -- its predicate names both creatures
-- and neither in particular -- which is what keeps the two rows from drifting
-- apart, one creature paired with another that is not paired back.
--
-- Casing on rule 702.95 is the closed half reading its own rulebook,
-- Pawl.Engine.Goad's standing over rule 701.15: what a reader learns here is
-- WHICH creature a creature is paired with, never which card paired them.
module Pawl.Engine.Soulbond where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Types.CardType as CardType
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Pairing as Pairing
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.ProjectedCharacteristics as PC

-- CR 702.95c and CR 702.95d together: pair `one` with `other` under `under`,
-- unless either is no longer a creature on the battlefield under that player's
-- control, or either is already paired. "Neither object becomes paired" is rule
-- 702.95c said exactly -- the gate is on the pair, so a failure writes nothing at
-- all rather than half a pairing.
--
-- Rule 702.95d is the `paired` test: a creature may be paired with only one other
-- creature, and the triggers' intervening "if" (CR 603.4) has already asked the
-- same question a moment earlier. Asked again here because CR 603.4 checks as the
-- ability RESOLVES too, and because Effect.Pair is reachable from any card that
-- states one.
--
-- A no-op for an id naming nothing, and for `one == other`: rule 702.95a says
-- "another creature".
pair :: PlayerId -> ObjectId -> ObjectId -> GameState.GameState -> GameState.GameState
pair under one other gs =
  let pcs = Projection.projectAll gs
      grants = Projection.controlGrants gs
      eligible oid =
        Set.member oid (GameState.battlefield gs)
          && maybe False (Set.member CardType.Creature . PC.cardTypes) (Map.lookup oid pcs)
          && Projection.controllerOfGiven grants oid gs == Just under
          && maybe True (Maybe.isNothing . Object.paired) (Game.lookupObject oid gs)
      mark partner = Map.adjust (\o -> o {Object.paired = Just (Pairing.MkPairing {Pairing.partner = partner, Pairing.under = under})})
   in if one == other || not (eligible one) || not (eligible other)
        then gs
        else gs {GameState.objects = mark other one (mark one other (GameState.objects gs))}

-- CR 702.95b read backwards: drop this permanent's half of a pairing. Only ever
-- CLEARS, Pawl.Engine.Ring.endOnControlChange's posture -- no rule restores a
-- pairing that CR 702.95e has ended.
unpair :: ObjectId -> Map.Map ObjectId Object.Object -> Map.Map ObjectId Object.Object
unpair = Map.adjust (\o -> o {Object.paired = Nothing})

-- CR 702.95e: a paired creature becomes unpaired when another player gains
-- control of it or of the creature it is paired with, when either stops being a
-- creature, or when either leaves the battlefield. Reports whether it acted, so
-- that the settle it runs in loops again -- unpairing ends the continuous effects
-- the pairing turned on, and CR 704.3 owes those a fresh state-based-action pass.
--
-- The rule's own three endings and no fourth: `under` is the seat the pairing was
-- made under, so "another player gains control" is a comparison against a
-- remembered player rather than against the partner's controller -- an effect
-- that takes BOTH creatures at once (Insurrection) leaves the two controllers
-- equal and the pairing ended.
--
-- The LEAVER needs nothing from here: CR 400.7 mints a new object and
-- Object.newIncarnation arrives unpaired. What this supplies is the creature left
-- behind, whose own row no incarnation clears.
--
-- The paired permanents are gathered first, off a stored field, and the
-- projection is forced only if there are any -- Pawl.Engine.Ring.endOnControlChange's
-- posture, almost every board having no pairing on it at all.
endWhenBroken :: Game Bool
endWhenBroken = do
  gs <- State.get
  let marked =
        Maybe.mapMaybe
          (\oid -> fmap ((,) oid) (Game.lookupObject oid gs >>= Object.paired))
          (Set.toList (GameState.battlefield gs))
  if null marked
    then pure False
    else do
      let pcs = Projection.projectAll gs
          grants = Projection.controlGrants gs
          holds under oid =
            Set.member oid (GameState.battlefield gs)
              && maybe False (Set.member CardType.Creature . PC.cardTypes) (Map.lookup oid pcs)
              && Projection.controllerOfGiven grants oid gs == Just under
          lapsed (oid, pairing) =
            not (holds (Pairing.under pairing) oid && holds (Pairing.under pairing) (Pairing.partner pairing))
          broken = fmap fst (filter lapsed marked)
      Monad.unless (null broken) $
        State.put gs {GameState.objects = foldr unpair (GameState.objects gs) broken}
      pure (not (null broken))
