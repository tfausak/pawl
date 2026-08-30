{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Projection.replacementsAffecting over CR 113.6b: which zone a
-- card's PRINTED replacement row functions from, and what the four walks past
-- the battlefield and the command zone gather. The CR 616.1 loop those rows are
-- fed to is Pawl.ReplacementSpec.
module Pawl.ZoneReplacementSpec where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.Zone as Zone

-- Nexus of Fate (M19 306) is the pool's producer: "If Nexus of Fate would be put
-- into a graveyard from anywhere, reveal Nexus of Fate and shuffle it into its
-- owner's library instead." Its own ruling says which zones that is -- "Nexus of
-- Fate's last ability applies if it would be put into a graveyard in any way,
-- including while it's resolving" -- so the card states every zone (CR 113.6b)
-- and the cases below drive the hand, the library and the stack with it.
--
-- The card's two riders -- CR 701.20's reveal and CR 701.24's shuffle -- are
-- Pawl.Types.ZoneChangeR's `revealing` and `shuffling`, proved by the last two
-- cases below.

-- An HONEST interpreter for CR 701.24a's randomness: a genuine permutation of
-- what it was offered, which is what makes the shuffle observable at all --
-- S.identityAnswer hands the order straight back, so a library it shuffled and
-- one it did not are the same list.
reversingShuffle :: Prompt.Prompt r -> r
reversingShuffle p = case p of
  Prompt.Shuffle ids -> reverse ids
  _ -> S.identityAnswer p

-- Which objects a CR 701.20a reveal has shown, in the order the log holds them.
revealed :: GameState.GameState -> [ObjectId.ObjectId]
revealed gs =
  Maybe.mapMaybe
    ( \event -> case event of
        GameEvent.Revealed (Revealed.MkRevealed _ oid _ _) -> Just oid
        _ -> Nothing
    )
    (S.eventsOf gs)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Replacement" $ do
  -- CR 113.6b from a HIDDEN zone (CR 400.2), which CR 113.6's own defaults never
  -- reach: the row functions only because the card states the hand.
  --
  -- A discard rather than a bare zone move, because CR 701.9a's action is the
  -- harder road -- a discard whose move CR 614.6 sends elsewhere is still a
  -- discard -- and Pawl.Engine.Event.discard is the funnel every discard goes
  -- through. CR 701.9c is why the card's reveal is not cosmetic: a card discarded
  -- into a hidden zone unrevealed has all its characteristics considered
  -- undefined, and the row's `revealing` is the branch of that rule Nexus of
  -- Fate takes. Nothing in pawl models undefined characteristics either way, so
  -- the case below reads the reveal off the log rather than off the card.
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
  -- CR 608.2n's trip to the graveyard, replaced from the STACK: the spell is the
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
    Spec.assertEqWith s "and CR 608.2n's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 0
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
  -- CR 701.24a: the redirect does not merely put the card into the library, it
  -- shuffles that library. A pair of boards differing in ONE thing -- which card
  -- alice discards -- because the discard funnel itself never shuffles, so a
  -- library reversed on the control leg would mean the shuffle came from
  -- somewhere other than Nexus of Fate's row.
  --
  -- Three library cards, so the reversal is a different list from the original;
  -- and the redirect puts the Nexus at Pawl.Types.LibraryPosition.defaultValue,
  -- the bottom, so a wrongly-unshuffled library ends with it LAST and the
  -- shuffled one ends with it FIRST. That is what makes the assertion able to
  -- differ.
  --
  -- The arriving card is found by ELIMINATION rather than by id: CR 400.7 mints
  -- a new incarnation on the way into the library, so the id alice discarded
  -- names nothing there.
  Spec.it s "CR 701.24a the redirect shuffles the library it moved the card into" $ do
    nexus <- S.printingOf s registry "Nexus of Fate"
    piker <- S.printingOf s registry "Goblin Piker"
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    let (_, g1) = S.addLibraryCard island S.alice (Setup.emptyGame S.bothPlayers)
        (_, g2) = S.addLibraryCard mountain S.alice g1
        (_, g3) = S.addLibraryCard forest S.alice g2
        (nexusId, g4) = S.addHandCard nexus S.alice g3
        (pikerId, gs) = S.addHandCard piker S.alice g4
        stock = Game.zoneMembers Zone.Library S.alice gs
        discarding oid = S.runPure reversingShuffle gs (Event.discard DiscardCause.Ordinary S.alice oid)
        after = discarding nexusId
        control = discarding pikerId
    Spec.assertEqWith s "CR 701.24a the arriving card is on TOP, which only a shuffle of the library it was put at the bottom of could do" (fmap (`List.elem` stock) (Game.zoneMembers Zone.Library S.alice after)) [False, True, True, True]
    Spec.assertEqWith s "and the three cards already there are in the order the interpreter's permutation named" (drop 1 (Game.zoneMembers Zone.Library S.alice after)) (reverse stock)
    Spec.assertEqWith s "the control: a card with no such row is discarded, and that library keeps the order it had" (Game.zoneMembers Zone.Library S.alice control) stock
    Spec.assertEqWith s "the fixture really stocked three cards for the reversal to move" (length stock) 3
    Spec.assertEqWith s "and the control card really was discarded" (length (Game.zoneMembers Zone.Graveyard S.alice control)) 1
  -- CR 701.20a: the same redirect shows the card. The same pair, since
  -- Pawl.Engine.Event.discard reveals nothing on its own -- CR 701.9c is what
  -- the reveal buys, a card discarded into a hidden zone unrevealed having
  -- undefined characteristics.
  Spec.it s "CR 701.20a the redirect reveals the card it moves" $ do
    nexus <- S.printingOf s registry "Nexus of Fate"
    piker <- S.printingOf s registry "Goblin Piker"
    let (nexusId, g1) = S.addHandCard nexus S.alice (Setup.emptyGame S.bothPlayers)
        (pikerId, gs) = S.addHandCard piker S.alice g1
        discarding oid = S.runPure reversingShuffle gs (Event.discard DiscardCause.Ordinary S.alice oid)
    Spec.assertEqWith s "CR 701.20a the Nexus was shown, under the id it had in the zone it left" (revealed (discarding nexusId)) [nexusId]
    Spec.assertEqWith s "the control: discarding a card with no such row shows nobody anything" (revealed (discarding pikerId)) []
    Spec.assertEqWith s "and the control card really was discarded" (length (Game.zoneMembers Zone.Graveyard S.alice (discarding pikerId))) 1
