{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 116.2e and CR 116.2d end to end: Pawl.Types.SpecialAction and the
-- Pawl.Types.Face.specialActions field that carries both, Pawl.Engine.Action's
-- discardableCards and Pawl.Engine.Ignore's ignorable with the actions they
-- offer, and Pawl.Engine.Engine's arms for them. CR 116.3 -- "if a player takes
-- a special action, that player receives priority afterward" -- is asserted here
-- ONCE for the whole family (#875); the CR 116.2b, CR 116.2d and CR 116.2m arms
-- retain priority the same way and are not separately re-asserted.
--
-- Circling Vultures (WTH 64) is the fixture and the only producer there can be:
-- CR 116.2e names it, so the row is closed at one card. Its upkeep ability is
-- not here -- that clause is CR 406.2's cost component, whose gate-card cases
-- live beside the other components in Pawl.CostSpec.
--
-- THE BOARD SHAPE that makes the offer case discriminating: alice holds three
-- cards -- the Vultures, a Doomed Traveler and a Mountain -- on BOB's turn with
-- a spell on the stack. The Traveler is the negative control (a hand card with
-- no special action of its own, so an implementation that offered the discard
-- for every hand card fails), and the Mountain is the timing control (CR
-- 116.2a's land play is refused in this window, so an implementation that
-- copied CR 116.2a's or CR 116.2m's sorcery-speed gate onto CR 116.2e fails
-- while the Traveler case still passes).
--
-- WHAT MAKES CR 116.3 discriminating: CR 117.3a gives the ACTIVE player
-- priority at the loop's entry, so bob is asked first and passes before alice
-- acts. That standing pass is why the arm's `passes = 0` is
-- observable at all -- without it the reset would be a no-op and the assertion
-- would hold whether or not the arm restarted the count.
module Pawl.SpecialActionSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Zone as Zone

-- bob's turn with a spell on the stack, alice holding the Vultures, a Doomed
-- Traveler and a Mountain. `priority` is set for the cases that ask
-- Pawl.Engine.Action.legalActions directly; the ones that run the priority loop
-- have it overwritten at entry, where CR 117.3a hands priority to the active
-- player.
board ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
board vultures traveler mountain bolt =
  let (vulturesId, gs1) = S.addHandCard vultures S.alice (Setup.emptyGame S.bothPlayers)
      (travelerId, gs2) = S.addHandCard traveler S.alice gs1
      (_, gs3) = S.addHandCard mountain S.alice gs2
      (_, gs4) = S.spellOnStack bolt S.bob gs3
   in ( vulturesId,
        travelerId,
        gs4
          { GameState.activePlayer = S.bob,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      )

isPlay :: Action.Type.Action -> Bool
isPlay action = case action of
  Action.Type.Play {} -> True
  Action.Type.Pass -> False
  Action.Type.Cast {} -> False
  Action.Type.Activate _ _ -> False
  Action.Type.TurnFaceUp _ -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Ignore _ -> False
  Action.Type.ActivateManaAbility _ -> False

isDiscarded :: GameEvent.GameEvent -> Bool
isDiscarded event = case event of
  GameEvent.Discarded {} -> True
  _ -> False

-- Take the named action the first time it is offered and pass ever after,
-- recording which player each ChooseAction prompt went to. That record is what
-- CR 116.3 is asserted on, since who is asked next is the only thing a game
-- observes about who holds priority.
type Log = State.State [PlayerId.PlayerId]

takeThenPass :: Action.Type.Action -> (forall r. Prompt.Prompt r -> Log r)
takeThenPass wanted prompt = case prompt of
  Prompt.ChooseAction _ pid actions -> do
    State.modify' (<> [pid])
    pure (if List.elem wanted actions then wanted else Action.Type.Pass)
  _ -> pure (S.identityAnswer prompt)

-- alice's board for CR 116.2d: nine Forests, a Leonin Arbiter SHE controls --
-- its "players" is possessive-free, so PlayerScope.EachPlayer stops her own
-- searches too -- one Forest left in her library and a Rampant Growth in hand to
-- go and get it with. Nine lands is four ignores or four Growths over, so no
-- case below can fail for want of mana.
arbiterBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
arbiterBoard forest arbiter growth =
  let (arbiterId, gs1) = S.addCreature arbiter S.alice (S.landsInPlay forest 9)
      (_, gs2) = S.addLibraryCard forest S.alice gs1
      (growthId, gs3) = S.addHandCard growth S.alice gs2
   in ( arbiterId,
        growthId,
        gs3
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      )

-- Finds the first card the search offers, and records that it was ASKED at all
-- beside the shuffle that follows. A prohibited search asks nothing, so the log
-- is what separates "searched and declined" -- CR 701.23b's legal outcome, which
-- looks identical in the zones -- from "never searched".
type PromptLog = State.State [String]

searching :: (forall r. Prompt.Prompt r -> PromptLog r)
searching prompt = case prompt of
  Prompt.SearchLibrary _ _ candidates -> do
    State.modify' (<> ["search"])
    pure (case candidates of oid : _ -> Just oid; [] -> Nothing)
  Prompt.Shuffle ids -> do
    State.modify' (<> ["shuffle"])
    pure ids
  _ -> pure (S.identityAnswer prompt)

-- Takes the action the FIRST time it is offered and passes ever after. CR 116.2d
-- puts no limit on how often a player may pay, and Pawl.Engine.Ignore.canIgnore
-- accordingly keeps offering it -- so an answerer that took it whenever offered
-- would drain the board's mana before the spell that observes it is cast.
takeOnce :: Action.Type.Action -> (forall r. Prompt.Prompt r -> State.State Bool r)
takeOnce wanted prompt = case prompt of
  Prompt.ChooseAction _ _ actions -> do
    taken <- State.get
    if not taken && List.elem wanted actions
      then do
        State.put True
        pure wanted
      else pure Action.Type.Pass
  _ -> pure (S.identityAnswer prompt)

-- Cast the Growth and let it resolve, logging the search and the shuffle.
growAndResolve :: ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, [String])
growAndResolve growthId gs =
  let ((_, after), asked) = State.runState (Engine.runGame searching gs (S.cast S.alice growthId >> Stack.resolveTop)) []
   in (after, asked)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = do
  circlingVultures s registry
  leoninArbiter s registry

-- CR 116.2d: "some effects from static abilities allow a player to take an
-- action to ignore the effect from that ability for a duration". Leonin Arbiter
-- (2X2 16) is the producer, and the effect ignored is CR 701.23's "players can't
-- search libraries".
leoninArbiter :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
leoninArbiter s registry = Spec.describe s "CR 116.2d Leonin Arbiter" $ do
  -- The window is CR 116.2b's -- "any time they have priority" -- not CR
  -- 116.2a's, so the second board is the timing control. The third is the COST
  -- control: one Forest cannot pay {2}, and the land play it still offers is
  -- what proves the missing action is about the cost rather than a broken board.
  Spec.it s "the action is offered whenever the player has priority, and only when the cost is payable" $ do
    forest <- S.printingOf s registry "Forest"
    arbiter <- S.printingOf s registry "Leonin Arbiter"
    growth <- S.printingOf s registry "Rampant Growth"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (arbiterId, _, gs) = arbiterBoard forest arbiter growth
        (_, onBobsTurn) = S.spellOnStack bolt S.bob gs
        instantSpeed = onBobsTurn {GameState.activePlayer = S.bob, GameState.priority = Just S.alice}
        (poorId, poor) = S.addCreature arbiter S.alice (S.landsInPlay forest 1)
        broke =
          (snd (S.addHandCard forest S.alice poor))
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
    Spec.assertBool s (List.elem (Action.Type.Ignore arbiterId) (Action.legalActions S.alice gs)) "the Arbiter may be ignored"
    Spec.assertBool s (List.elem (Action.Type.Ignore arbiterId) (Action.legalActions S.alice instantSpeed)) "on another player's turn with a spell on the stack too"
    Spec.assertBool s (List.notElem (Action.Type.Ignore poorId) (Action.legalActions S.alice broke)) "but not with one Forest, which cannot pay {2}"
    Spec.assertBool s (any isPlay (Action.legalActions S.alice broke)) "the control: that same board still offers a land play"
  -- CR 101.2: the prohibition wins, so the search does not happen -- and CR
  -- 701.23 describes only how to look, so the card's own "then shuffle" still
  -- does. Without the log both outcomes are indistinguishable from CR 701.23b's
  -- legal decline.
  Spec.it s "CR 101.2 a prohibited player does not search, but still shuffles" $ do
    forest <- S.printingOf s registry "Forest"
    arbiter <- S.printingOf s registry "Leonin Arbiter"
    growth <- S.printingOf s registry "Rampant Growth"
    let (_, growthId, gs) = arbiterBoard forest arbiter growth
        (after, asked) = growAndResolve growthId gs
    Spec.assertEqWith s "the Forest is still in the library" (S.countByName (S.printingName forest) S.alice after) 1
    Spec.assertEqWith s "and no tenth Forest reached the battlefield" (S.countOnBattlefieldByName (S.printingName forest) S.alice after) 9
    Spec.assertEqWith s "CR 701.23: the search was never offered, and the shuffle still happened" asked ["shuffle"]
  -- The same board and the same spell, with the special action taken first
  -- through the priority loop -- so this is Pawl.Engine.Engine's arm as well as
  -- the suppression, and the case above is its paired control.
  Spec.it s "CR 116.2d paying the cost lets that player, and only that player, search" $ do
    forest <- S.printingOf s registry "Forest"
    arbiter <- S.printingOf s registry "Leonin Arbiter"
    growth <- S.printingOf s registry "Rampant Growth"
    let (arbiterId, growthId, gs) = arbiterBoard forest arbiter growth
        afterIgnore = snd (State.evalState (Engine.runGame (takeOnce (Action.Type.Ignore arbiterId)) gs Engine.priorityLoop) False)
        (after, asked) = growAndResolve growthId afterIgnore
    Spec.assertEqWith s "the Forest left the library" (S.countByName (S.printingName forest) S.alice after) 0
    Spec.assertEqWith s "and is the tenth on the battlefield" (S.countOnBattlefieldByName (S.printingName forest) S.alice after) 10
    Spec.assertEqWith s "the search was offered this time" asked ["search", "shuffle"]
    Spec.assertBool s (not (PlayerEffect.prohibitsSearching S.alice afterIgnore)) "alice paid, so she is not prohibited"
    Spec.assertBool s (PlayerEffect.prohibitsSearching S.bob afterIgnore) "bob did not, so he still is"
  -- CR 514.2: "until end of turn" ends at cleanup, which is the one caller of
  -- Expiry.dropAtCleanup. Asserted by casting the SAME spell on the swept state
  -- and watching it stop searching again.
  Spec.it s "CR 514.2 the ignore ends at cleanup" $ do
    forest <- S.printingOf s registry "Forest"
    arbiter <- S.printingOf s registry "Leonin Arbiter"
    growth <- S.printingOf s registry "Rampant Growth"
    let (arbiterId, growthId, gs) = arbiterBoard forest arbiter growth
        afterIgnore = snd (State.evalState (Engine.runGame (takeOnce (Action.Type.Ignore arbiterId)) gs Engine.priorityLoop) False)
        (after, asked) = growAndResolve growthId (Expiry.dropAtCleanup afterIgnore)
    Spec.assertEqWith s "the Forest is still in the library" (S.countByName (S.printingName forest) S.alice after) 1
    Spec.assertEqWith s "and the search was not offered again" asked ["shuffle"]

circlingVultures :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
circlingVultures s registry = Spec.describe s "CR 116.2e Circling Vultures" $ do
  -- CR 116.2e's last sentence: "a player can take such an action any time they
  -- have priority". The card's own words are "any time you could cast an
  -- instant" and the rule overrides them, so nothing here consults a casting
  -- permission -- and the window below is neither a main phase of alice's turn
  -- nor an empty stack.
  Spec.it s "the action is offered, at instant speed, only for the card that grants it" $ do
    vultures <- S.printingOf s registry "Circling Vultures"
    traveler <- S.printingOf s registry "Doomed Traveler"
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (vulturesId, travelerId, gs) = board vultures traveler mountain bolt
        actions = Action.legalActions S.alice gs
        ownTurn = gs {GameState.activePlayer = S.alice, GameState.stack = []}
    Spec.assertBool s (List.elem (Action.Type.DiscardFromHand vulturesId) actions) "the Vultures may be discarded"
    Spec.assertBool s (List.notElem (Action.Type.DiscardFromHand travelerId) actions) "the Doomed Traveler may not"
    Spec.assertBool s (not (any isPlay actions)) "and no land play is offered in this window"
    Spec.assertBool s (any isPlay (Action.legalActions S.alice ownTurn)) "the control: the same Mountain is playable at sorcery speed"
  -- CR 116.1 / CR 701.9a: the action does not use the stack, and the discard
  -- goes through Pawl.Engine.Event.discard rather than a zone move -- so CR
  -- 702.29d's "cycles or discards" trigger can see it, which a zone poke would
  -- leave it blind to forever. An arm that stacked the card instead would put
  -- it onto the battlefield rather than into the graveyard, which is what the
  -- first two assertions rule out.
  Spec.it s "CR 701.9a taking it discards the card without using the stack" $ do
    vultures <- S.printingOf s registry "Circling Vultures"
    traveler <- S.printingOf s registry "Doomed Traveler"
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (vulturesId, _, base) = board vultures traveler mountain bolt
        gs = base {GameState.stack = []}
        after = snd (State.evalState (Engine.runGame (takeThenPass (Action.Type.DiscardFromHand vulturesId)) gs Engine.priorityLoop) [])
    -- CR 400.7 mints a new object on the move, so the graveyard card is named
    -- rather than compared to the hand id, and the logged event is asserted
    -- against that new id -- which is what ties the two together.
    let graveyard = Game.zoneMembers Zone.Graveyard S.alice after
    Spec.assertEqWith
      s
      "the Vultures are in alice's graveyard"
      (fmap (\oid -> fmap S.nameOf (Game.cardOf oid after)) graveyard)
      [Just (S.printingName vultures)]
    Spec.assertEqWith s "and nothing reached the battlefield" (length (GameState.battlefield after)) 0
    Spec.assertEqWith s "the other two hand cards are untouched" (S.handSize S.alice after) 2
    Spec.assertEqWith
      s
      "CR 701.9a the discard was logged, and CR 702.29c's cycling cause is not what caused it"
      (filter isDiscarded (S.eventsOf after))
      (fmap (\oid -> GameEvent.Discarded S.alice oid DiscardCause.Ordinary) graveyard)
  -- CR 116.3: "if a player takes a special action, that player receives
  -- priority afterward." Both halves of the arm are pinned by the one sequence.
  -- Retaining priority puts alice's second prompt before bob's first; restarting
  -- the pass count is what makes bob asked at all, since the standing pass plus
  -- alice's would otherwise be a full round.
  Spec.it s "CR 116.3 the player receives priority again afterward" $ do
    vultures <- S.printingOf s registry "Circling Vultures"
    traveler <- S.printingOf s registry "Doomed Traveler"
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (vulturesId, _, base) = board vultures traveler mountain bolt
        gs = base {GameState.stack = []}
        asked = State.execState (Engine.runGame (takeThenPass (Action.Type.DiscardFromHand vulturesId)) gs Engine.priorityLoop) []
    Spec.assertEqWith
      s
      "bob passes, alice acts, alice is asked again, and only then is bob asked"
      asked
      [S.bob, S.alice, S.alice, S.bob]
