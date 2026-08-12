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
import qualified Data.Set as Set
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Ignore as Ignore
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Facing as Facing
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
  Prompt.SearchLibrary _ _ candidates cap -> do
    State.modify' (<> ["search"])
    pure (List.genericTake cap candidates)
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

-- Is this the offer to play THAT card as a land? Written out rather than reusing
-- isPlay above, which asks only about the operation.
playing :: ObjectId.ObjectId -> Action.Type.Action -> Bool
playing wanted action = case action of
  Action.Type.Play oid _ -> oid == wanted
  Action.Type.Pass -> False
  Action.Type.Cast {} -> False
  Action.Type.Activate _ _ -> False
  Action.Type.TurnFaceUp _ -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Ignore _ -> False
  Action.Type.ActivateManaAbility _ -> False

-- Is this the offer to cast THAT card face up?
casting :: ObjectId.ObjectId -> Action.Type.Action -> Bool
casting wanted action = case action of
  Action.Type.Cast oid _ facing -> oid == wanted && facing == Facing.FaceUp
  Action.Type.Play _ _ -> False
  Action.Type.Pass -> False
  Action.Type.Activate _ _ -> False
  Action.Type.TurnFaceUp _ -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Ignore _ -> False
  Action.Type.ActivateManaAbility _ -> False

-- Pays CR 116.2d's cost by sacrificing the NAMED permanent, and answers every
-- other prompt as the identity does.
--
-- The victim is PINNED, and that is load-bearing rather than tidy. All four of
-- alice's permanents are legal sacrifices, the Engine among them, so an answerer
-- that took the first candidate could pay by sacrificing the Engine itself --
-- which lifts the prohibition through CR 604.2 instead of through CR 116.2d and
-- leaves every assertion below passing for the wrong reason.
--
-- S.identityAnswer DECLINES a sacrifice, so this arm is also what makes the
-- payment happen at all: without it Cost.pay reports Unpaid and Ignore.ignore
-- restores the board.
sacrificing :: ObjectId.ObjectId -> (forall r. Prompt.Prompt r -> r)
sacrificing victim prompt = case prompt of
  Prompt.ChooseSacrifices {} -> Set.singleton victim
  _ -> S.identityAnswer prompt

-- Damping Engine (ULG 124) on a THREE-seat board, which is what makes CR 116.2d's
-- WHO observable: its "that player" is the one player controlling more permanents
-- than each other player, and on two seats that player cannot be told apart from
-- the Engine's own controller, whom Leonin Arbiter's cases already offer it to.
--
-- alice controls the Engine and three Forests, bob two Forests, carol one. The
-- tallies are DISTINCT so no two readings of "more than each other player" land on
-- the same seat, and every seat controls at least one permanent so every seat can
-- pay the sacrifice -- which leaves the rule's WHO as the only conjunct that can
-- separate them.
--
-- alice's hand holds a Forest, a Woodland Changeling and a Rampant Growth. The
-- Growth is the Filter's negative control and shares the Changeling's exact
-- {1}{G} off the same three Forests: Damping Engine stops artifact, creature and
-- enchantment spells, so a sorcery must stay castable on every board here.
dampingBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
dampingBoard engine forest changeling growth =
  let (engineId, gs1) = S.addCreature engine S.alice S.threePlayerGame
      (victimId, gs2) = S.addCreature forest S.alice gs1
      (_, gs3) = S.addCreature forest S.alice gs2
      (_, gs4) = S.addCreature forest S.alice gs3
      (_, gs5) = S.addCreature forest S.bob gs4
      (_, gs6) = S.addCreature forest S.bob gs5
      (_, gs7) = S.addCreature forest S.carol gs6
      (forestId, gs8) = S.addHandCard forest S.alice gs7
      (changelingId, gs9) = S.addHandCard changeling S.alice gs8
      (growthId, gs10) = S.addHandCard growth S.alice gs9
   in ( engineId,
        victimId,
        forestId,
        changelingId,
        growthId,
        gs10
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      )

-- The paired board, differing from dampingBoard in exactly one thing: bob now
-- controls five permanents to alice's four, so the seat the Engine is affecting
-- moves. Same turn, same phase, same hand, same Engine, same payable cost.
bobLeading :: Printing.Printing -> GameState.GameState -> GameState.GameState
bobLeading forest gs =
  let add g = snd (S.addCreature forest S.bob g)
   in add (add (add gs))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = do
  circlingVultures s registry
  dampingEngine s registry
  leoninArbiter s registry

-- CR 116.2d again, on the two axes Leonin Arbiter cannot reach: WHO the action is
-- offered to (its own scope is EachPlayer, so every seat is offered it) and what
-- one payment covers (it prints one player ability, so a permanent-wide ignore
-- and an ability-wide one agree). Damping Engine (ULG 124) narrows the first and
-- prints two abilities to observe the second.
dampingEngine :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
dampingEngine s registry = Spec.describe s "CR 116.2d Damping Engine" $ do
  -- The pair is the whole case: on one board alice is the player the ability
  -- affects and on the other bob is, and the offer follows the effect rather than
  -- the table. bob being offered it on the second board is also the cost control
  -- -- his one sacrifice was payable on the first board too.
  Spec.it s "the action is offered to the player the ability is affecting, and to no other" $ do
    engine <- S.printingOf s registry "Damping Engine"
    forest <- S.printingOf s registry "Forest"
    changeling <- S.printingOf s registry "Woodland Changeling"
    growth <- S.printingOf s registry "Rampant Growth"
    let (engineId, _, _, _, _, aliceLeads) = dampingBoard engine forest changeling growth
        bobLeads = bobLeading forest aliceLeads
        asked pid gs = Action.legalActions pid (gs {GameState.priority = Just pid})
    Spec.assertBool s (List.elem (Action.Type.Ignore engineId) (asked S.alice aliceLeads)) "alice controls the most permanents, so she may pay to ignore it"
    Spec.assertBool s (List.notElem (Action.Type.Ignore engineId) (asked S.bob aliceLeads)) "bob is not affected, so there is nothing for him to ignore"
    Spec.assertBool s (List.notElem (Action.Type.Ignore engineId) (asked S.carol aliceLeads)) "nor carol"
    Spec.assertBool s (List.elem (Action.Type.Ignore engineId) (asked S.bob bobLeads)) "and bob IS offered it once the lead is his -- so his cost was payable all along"
    Spec.assertBool s (List.notElem (Action.Type.Ignore engineId) (asked S.alice bobLeads)) "while alice, no longer affected, is offered nothing"
  -- "More permanents than each other player" is a STRICT comparison, so a tie for
  -- the lead leaves the ability affecting nobody -- which is the third value this
  -- scope can take and the one a "whoever has the most" reading would miss.
  Spec.it s "the comparison is strict, so a tie for the lead affects nobody" $ do
    engine <- S.printingOf s registry "Damping Engine"
    forest <- S.printingOf s registry "Forest"
    changeling <- S.printingOf s registry "Woodland Changeling"
    growth <- S.printingOf s registry "Rampant Growth"
    let (engineId, _, forestId, _, _, aliceLeads) = dampingBoard engine forest changeling growth
        tied = snd (S.addCreature forest S.bob (snd (S.addCreature forest S.bob aliceLeads)))
        asked pid gs = Action.legalActions pid (gs {GameState.priority = Just pid})
    Spec.assertBool s (List.notElem (Action.Type.Ignore engineId) (asked S.alice tied)) "alice has no lead to be affected by"
    Spec.assertBool s (List.notElem (Action.Type.Ignore engineId) (asked S.bob tied)) "and neither does bob"
    Spec.assertBool s (any (playing forestId) (asked S.alice tied)) "the control: with the ability affecting nobody, alice may play her land"
  -- CR 305.1 and CR 601.3a, from ONE printed sentence declaring two player
  -- abilities. The Growth is what makes the Filter discriminating: same {1}{G},
  -- same three Forests, and a sorcery is not one of the three types named.
  Spec.it s "CR 305.1 / CR 601.3a the affected player can't play a land or cast a creature spell, and a sorcery is untouched" $ do
    engine <- S.printingOf s registry "Damping Engine"
    forest <- S.printingOf s registry "Forest"
    changeling <- S.printingOf s registry "Woodland Changeling"
    growth <- S.printingOf s registry "Rampant Growth"
    let (_, _, forestId, changelingId, growthId, aliceLeads) = dampingBoard engine forest changeling growth
        bobLeads = bobLeading forest aliceLeads
        actions = Action.legalActions S.alice aliceLeads
        unaffected = Action.legalActions S.alice bobLeads
    Spec.assertBool s (not (any (playing forestId) actions)) "no land play is offered"
    Spec.assertBool s (not (any (casting changelingId) actions)) "nor the creature spell"
    Spec.assertBool s (any (casting growthId) actions) "but the sorcery of the same cost is still castable"
    Spec.assertBool s (any (playing forestId) unaffected) "the pair: with bob leading, alice may play the land"
    Spec.assertBool s (any (casting changelingId) unaffected) "and cast the creature"
  -- The narrowing Pawl.Types.SpecialAction carries, asserted rather than assumed:
  -- one payment covers the WHOLE permanent, so both of the Engine's abilities stop
  -- applying to the player who paid. A per-ability ignore would lift one.
  Spec.it s "CR 116.2d one payment lifts every one of that permanent's player abilities" $ do
    engine <- S.printingOf s registry "Damping Engine"
    forest <- S.printingOf s registry "Forest"
    changeling <- S.printingOf s registry "Woodland Changeling"
    growth <- S.printingOf s registry "Rampant Growth"
    let (engineId, victimId, forestId, changelingId, _, aliceLeads) = dampingBoard engine forest changeling growth
        afterIgnore = S.runPure (sacrificing victimId) aliceLeads (Ignore.ignore S.alice engineId)
        actions = Action.legalActions S.alice afterIgnore
    Spec.assertEqWith s "the sacrifice was paid: one of alice's three Forests is gone" (S.countOnBattlefieldByName (S.printingName forest) S.alice afterIgnore) 2
    Spec.assertBool s (any (playing forestId) actions) "CR 305.1's half is lifted"
    Spec.assertBool s (any (casting changelingId) actions) "and CR 601.3a's half with it, off the same one payment"
    -- CR 116.2d forbids no repeat, and Pawl.Engine.PlayerEffect.affectedBy is
    -- asked over the unfiltered gather so that the offer survives being taken.
    Spec.assertBool s (List.elem (Action.Type.Ignore engineId) actions) "and the action is still offered, since paying again is legal"

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
  -- The WHO conjunct read the other way, and the pair Damping Engine's cases are
  -- the other half of: Leonin Arbiter's own prohibition is possessive-free
  -- (EachPlayer), so it affects every seat and every seat is offered the action --
  -- the Arbiter's controller included. A gate that offered it only to the
  -- controller, or only to an opponent, fails here while every case above passes.
  Spec.it s "CR 116.2d an EachPlayer prohibition offers the action to every seat" $ do
    forest <- S.printingOf s registry "Forest"
    arbiter <- S.printingOf s registry "Leonin Arbiter"
    growth <- S.printingOf s registry "Rampant Growth"
    let (arbiterId, _, gs) = arbiterBoard forest arbiter growth
        (_, withBobsLands) = S.addCreature forest S.bob (snd (S.addCreature forest S.bob gs))
        asked pid board_ = Action.legalActions pid (board_ {GameState.priority = Just pid})
    Spec.assertBool s (List.elem (Action.Type.Ignore arbiterId) (asked S.alice withBobsLands)) "the Arbiter's own controller may pay"
    Spec.assertBool s (List.elem (Action.Type.Ignore arbiterId) (asked S.bob withBobsLands)) "and so may bob, whose two Forests pay the {2}"
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
