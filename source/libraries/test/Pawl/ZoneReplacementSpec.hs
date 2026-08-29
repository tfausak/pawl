{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Projection.replacementsAffecting over CR 113.6b: which zone a
-- card's PRINTED replacement row functions from, and what the four walks past
-- the battlefield and the command zone gather. The CR 616.1 loop those rows are
-- fed to is Pawl.ReplacementSpec.
module Pawl.ZoneReplacementSpec where

import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Zone as Zone

-- Nexus of Fate (M19 306) is the pool's producer: "If Nexus of Fate would be put
-- into a graveyard from anywhere, reveal Nexus of Fate and shuffle it into its
-- owner's library instead." Its own ruling says which zones that is -- "Nexus of
-- Fate's last ability applies if it would be put into a graveyard in any way,
-- including while it's resolving" -- so the card states every zone (CR 113.6b)
-- and the three cases below drive three of them.
--
-- Not implemented: the reveal and the shuffle. Pawl.Types.ZoneChangeR names a
-- destination zone and nothing else, so the card lands at
-- Pawl.Types.LibraryPosition.defaultValue -- the bottom -- unrevealed (#2591).
-- Both omissions are stricter than the printing rather than looser, which is why
-- the card is committed as it is.
spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Replacement" $ do
  -- CR 113.6b from a HIDDEN zone (CR 400.2), which CR 113.6's own defaults never
  -- reach: the row functions only because the card states the hand.
  --
  -- A discard rather than a bare zone move, because CR 701.9c is the harder road
  -- -- a redirected discard is still a discard -- and Pawl.Engine.Event.discard
  -- is the funnel every discard goes through.
  Spec.it s "CR 113.6b a stated row functions from the hand, so a discard lands in the library" $ do
    nexus <- S.printingOf s registry "Nexus of Fate"
    let (nexusId, gs) = S.addHandCard nexus S.alice (Setup.emptyGame S.bothPlayers)
        discarded = S.runPure S.identityAnswer gs (Event.discard DiscardCause.Ordinary S.alice nexusId)
    Spec.assertEqWith s "CR 614.6 the discarded card is in its owner's library" (length (Game.zoneMembers Zone.Library S.alice discarded)) 1
    Spec.assertEqWith s "and the graveyard the discard aimed at is empty" (length (Game.zoneMembers Zone.Graveyard S.alice discarded)) 0
    Spec.assertEqWith s "the hand it left is empty too" (length (Game.zoneMembers Zone.Hand S.alice discarded)) 0
  -- The other hidden zone, the milling road: a card put into a graveyard from a
  -- library. Driven through the zone-change funnel directly rather than through a
  -- mill effect, since what the case is about is which ZONE the row was gathered
  -- from, and no mill card would put the moving card anywhere else.
  Spec.it s "CR 113.6b a stated row functions from the library, so a milled card stays there" $ do
    nexus <- S.printingOf s registry "Nexus of Fate"
    let (nexusId, gs) = S.addLibraryCard nexus S.alice (Setup.emptyGame S.bothPlayers)
        milled = S.runPure S.identityAnswer gs (Event.changeZone nexusId Zone.Graveyard)
    Spec.assertEqWith s "CR 614.6 the card never left the library" (length (Game.zoneMembers Zone.Library S.alice milled)) 1
    Spec.assertEqWith s "and the graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice milled)) 0
  -- CR 608.2m's trip to the graveyard, replaced from the STACK: the spell is the
  -- object with the ability, and it is still on the stack as the move is
  -- proposed. The ruling above is about exactly this road.
  Spec.it s "CR 113.6b a stated row functions from the stack, so the resolved spell goes to the library" $ do
    island <- S.printingOf s registry "Island"
    nexus <- S.printingOf s registry "Nexus of Fate"
    let base = S.landsInPlay island 7
        (nexusId, gs1) = S.addHandCard nexus S.alice base
        gs =
          gs1
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        cast = S.runPure S.identityAnswer gs (S.cast S.alice nexusId)
        resolved = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertEqWith s "CR 614.6 the resolved spell is in its owner's library" (length (Game.zoneMembers Zone.Library S.alice resolved)) 1
    Spec.assertEqWith s "and CR 608.2m's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 0
    -- After the two above, so that neither can be absorbed by a cast that never
    -- happened: an uncast Nexus would leave the library empty and fail the first.
    Spec.assertEqWith s "the spell really was cast" (length (GameState.stack cast)) 1
    Spec.assertEqWith s "and really did resolve" (length (GameState.stack resolved)) 0
  -- The gate the three cases above cannot show, as a pair of boards differing in
  -- one thing: Rest in Peace's row states NO zone, so CR 113.6's default leaves it
  -- functioning on the battlefield and nowhere else. The enchantment is bob's and
  -- the dying creature alice's, so one graveyard holds the card and the other the
  -- creature.
  Spec.it s "CR 113.6b a row stating no zone does not function from a graveyard" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    piker <- S.printingOf s registry "Goblin Piker"
    let (pikerId, base) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        inGraveyard = snd (S.addGraveyardCard restInPeace S.bob base)
        onBattlefield = snd (S.addCreature restInPeace S.bob base)
        binned st = S.runPure S.identityAnswer st (Event.changeZone pikerId Zone.Graveyard)
    Spec.assertEqWith s "the CARD in a graveyard replaces nothing: the creature reaches its owner's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice (binned inGraveyard))) 1
    Spec.assertEqWith s "and nothing was exiled" (length (Game.zoneMembers Zone.Exile S.alice (binned inGraveyard))) 0
    Spec.assertEqWith s "CR 113.6's default: the same enchantment on the battlefield exiles it instead" (length (Game.zoneMembers Zone.Exile S.alice (binned onBattlefield))) 1
    Spec.assertEqWith s "so that graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice (binned onBattlefield))) 0
