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
-- CR 116.2k's plot (Djinn of Fool's Fall) and CR 116.2h's foretell (Augury
-- Raven) are covered here too, each with its own board: the three windows rule
-- 116.2 states -- any priority, the owner's own turn, and sorcery speed -- are
-- what the groups' offer cases tell apart.
--
-- CR 702.170c's OTHER route to a plotted card -- an effect rather than the
-- special action (Kellan Joins Up, Pawl.Types.Effect's MakePlotted) -- is here
-- for the same reason: it lands on Pawl.Engine.Plot.becomePlotted beside CR
-- 116.2k's, and the two are asserted against the same rule 702.170d readings.
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

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Ignore as Ignore
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Discarded as Discarded
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.TapState as TapState
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
  Action.Type.TurnFaceUp {} -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Plot _ -> False
  Action.Type.Foretell _ -> False
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
  Action.Type.TurnFaceUp {} -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Plot _ -> False
  Action.Type.Foretell _ -> False
  Action.Type.Ignore _ -> False
  Action.Type.ActivateManaAbility _ -> False

-- Is this the offer to cast THAT card face up?
casting :: ObjectId.ObjectId -> Action.Type.Action -> Bool
casting wanted action = case action of
  Action.Type.Cast oid _ facing -> oid == wanted && facing == Facing.FaceUp
  Action.Type.Play _ _ -> False
  Action.Type.Pass -> False
  Action.Type.Activate _ _ -> False
  Action.Type.TurnFaceUp {} -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Plot _ -> False
  Action.Type.Foretell _ -> False
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

-- Djinn of Fool's Fall (OTJ 43) on alice's own precombat main with the stack
-- empty -- CR 702.170a's window -- holding four Islands, the Djinn and a Doomed
-- Traveler.
--
-- FOUR Islands and not more: the plot cost is {3}{U}, so the board pays it to the
-- last mana. That is what makes the later cast's assertions discriminating, since
-- every land is tapped by the time the plotted card is offered and a cast that
-- charged anything at all could not be paid for.
--
-- The Traveler is the negative control -- a hand card with no plot ability, so an
-- implementation that offered the action for every hand card fails.
plotBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
plotBoard island djinn traveler =
  let (djinnId, gs1) = S.addHandCard djinn S.alice (S.landsInPlay island 4)
      (travelerId, gs2) = S.addHandCard traveler S.alice gs1
   in ( djinnId,
        travelerId,
        gs2
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      )

-- The one exiled card on the board, and the assertion that there is exactly one:
-- CR 400.7 mints a new object as the card leaves the hand, so no test can name
-- the exiled incarnation by the id it plotted.
soleExile :: GameState.GameState -> Maybe ObjectId.ObjectId
soleExile gs = case Set.toList (GameState.exile gs) of
  [oid] -> Just oid
  _ -> Nothing

plotting :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
plotting s registry = Spec.describe s "CR 116.2k Djinn of Fool's Fall" $ do
  -- CR 702.170a's window is CR 116.2a's rather than CR 116.2b's: "any time you
  -- have priority DURING YOUR MAIN PHASE WHILE THE STACK IS EMPTY". Each of the
  -- two paired boards moves exactly one of those conjuncts and nothing else.
  Spec.it s "the action is offered only for a card with plot, and only at sorcery speed" $ do
    island <- S.printingOf s registry "Island"
    djinn <- S.printingOf s registry "Djinn of Fool's Fall"
    traveler <- S.printingOf s registry "Doomed Traveler"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (djinnId, travelerId, gs) = plotBoard island djinn traveler
        actions = Action.legalActions S.alice gs
        opponentsTurn = gs {GameState.activePlayer = S.bob}
        stackBusy = snd (S.spellOnStack bolt S.alice gs)
    Spec.assertBool s (List.elem (Action.Type.Plot djinnId) actions) "the Djinn may be plotted"
    Spec.assertBool s (List.notElem (Action.Type.Plot travelerId) actions) "the Doomed Traveler may not"
    Spec.assertBool s (List.notElem (Action.Type.Plot djinnId) (Action.legalActions S.alice opponentsTurn)) "not on an opponent's turn"
    Spec.assertBool s (List.notElem (Action.Type.Plot djinnId) (Action.legalActions S.alice stackBusy)) "and not with a spell on the stack"
  -- CR 702.170a's "and pay [cost]": an action whose cost cannot be paid is not
  -- offered. The pair differs in the mana available and in nothing else -- three
  -- Islands against the four {3}{U} needs.
  Spec.it s "the action is not offered when the plot cost cannot be paid" $ do
    island <- S.printingOf s registry "Island"
    djinn <- S.printingOf s registry "Djinn of Fool's Fall"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let (djinnId, _, gs) = plotBoard island djinn traveler
        (poorId, poor) = case S.addHandCard djinn S.alice (S.landsInPlay island 3) of
          (oid, g) -> (oid, g {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice})
    Spec.assertBool s (List.elem (Action.Type.Plot djinnId) (Action.legalActions S.alice gs)) "four Islands pay {3}{U}"
    Spec.assertBool s (List.notElem (Action.Type.Plot poorId) (Action.legalActions S.alice poor)) "three do not"
  -- CR 702.170b: the action does not use the stack. The prompt log is what proves
  -- it -- alice acts and is asked AGAIN before bob is asked anything, so no player
  -- got a window to respond and nothing was put on the stack to respond to. CR
  -- 116.3's retained priority is the same sequence read the other way, and is
  -- asserted once for the whole family in the Vultures group above.
  Spec.it s "CR 702.170b taking it exiles the card without using the stack" $ do
    island <- S.printingOf s registry "Island"
    djinn <- S.printingOf s registry "Djinn of Fool's Fall"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let (djinnId, _, gs) = plotBoard island djinn traveler
        (asked, after) = case State.runState (Engine.runGame (takeThenPass (Action.Type.Plot djinnId)) gs Engine.priorityLoop) [] of
          ((_, g), log') -> (log', g)
    Spec.assertEqWith
      s
      "alice acts, alice is asked again, and only then is bob asked"
      asked
      [S.alice, S.alice, S.bob]
    Spec.assertEqWith
      s
      "the Djinn is in exile"
      (fmap (\oid -> fmap S.nameOf (Game.cardOf oid after)) (soleExile after))
      (Just (Just (S.printingName djinn)))
    Spec.assertEqWith s "the Traveler is still in hand" (S.handSize S.alice after) 1
    Spec.assertEqWith s "nothing but the four Islands is on the battlefield" (length (GameState.battlefield after)) 4
    Spec.assertEqWith s "and the stack is empty" (GameState.stack after) []
    -- CR 702.170a's "it becomes a plotted card", which is the whole point of the
    -- action: an arm that exiled the card and stamped nothing would pass every
    -- assertion above.
    Spec.assertEqWith
      s
      "the exiled card is plotted, stamped with this turn"
      (soleExile after >>= \oid -> fmap Object.plotted (Game.lookupObject oid after))
      (Just (Just (GameState.turnNumber after)))
  -- CR 702.170d: "a plotted card's owner may cast it from exile without paying
  -- its mana cost ... during ANY TURN AFTER the turn in which it became plotted."
  -- Every board below is the state the plot left behind, with one thing moved:
  -- the turn number, the caster, or the stamp.
  Spec.it s "CR 702.170d the plotted card is castable only by its owner, and only later" $ do
    island <- S.printingOf s registry "Island"
    djinn <- S.printingOf s registry "Djinn of Fool's Fall"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let (djinnId, _, gs) = plotBoard island djinn traveler
        after = snd (State.evalState (Engine.runGame (takeThenPass (Action.Type.Plot djinnId)) gs Engine.priorityLoop) [])
        later = after {GameState.turnNumber = GameState.turnNumber after + 1}
        unplotted = later {GameState.objects = Map.map (\o -> o {Object.plotted = Nothing}) (GameState.objects later)}
        -- CR 307.5's window belongs to whoever's turn it is, so bob's case has to
        -- be asked on bob's turn or it fails for the timing rather than for the
        -- ownership. `bobOwns` is that same board with the exiled card's owner
        -- moved and nothing else, which is what makes the refusal above CR
        -- 702.170d's rather than a coincidence.
        bobsTurn = later {GameState.activePlayer = S.bob}
        bobOwns oid = bobsTurn {GameState.objects = Map.adjust (\o -> o {Object.owner = S.bob}) oid (GameState.objects bobsTurn)}
    Spec.assertBool s (Maybe.isJust (soleExile after)) "the card was exiled, so the cases below are about a card in exile"
    Monad.forM_ (soleExile after) $ \exiledId -> do
      Spec.assertBool s (not (S.castable S.alice exiledId after)) "not on the turn it became plotted"
      Spec.assertBool s (S.castable S.alice exiledId later) "on the next turn it is castable -- with every Island still tapped, so the cast is free"
      Spec.assertBool s (not (S.castable S.bob exiledId bobsTurn)) "and not by a player who does not own it"
      Spec.assertBool s (S.castable S.bob exiledId (bobOwns exiledId)) "the control: bob's own turn and bob's own plotted card is castable, so the refusal above was the ownership"
      Spec.assertBool s (not (S.castable S.alice exiledId unplotted)) "the control: the same card in the same exile, unplotted, is castable by nobody"
  -- The offer taken rather than merely asked about: the Djinn reaches the
  -- battlefield off a board with no untapped land on it, which is CR 702.170d's
  -- "without paying its mana cost" observed rather than inferred.
  Spec.it s "CR 702.170d casting it costs nothing" $ do
    island <- S.printingOf s registry "Island"
    djinn <- S.printingOf s registry "Djinn of Fool's Fall"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let (djinnId, _, gs) = plotBoard island djinn traveler
        after = snd (State.evalState (Engine.runGame (takeThenPass (Action.Type.Plot djinnId)) gs Engine.priorityLoop) [])
        later = after {GameState.turnNumber = GameState.turnNumber after + 1}
    Monad.forM_ (soleExile after) $ \exiledId -> do
      let resolved = S.runPure S.castAnswer later (S.cast S.alice exiledId >> Stack.resolveTop)
      Spec.assertEqWith
        s
        "the Djinn is on the battlefield"
        (S.countOnBattlefieldByName (S.printingName djinn) S.alice resolved)
        1
      Spec.assertEqWith s "and exile is empty" (length (GameState.exile resolved)) 0

-- Kellan Joins Up (OTJ 216) {G}{W}{U} Legendary Enchantment, "When Kellan Joins
-- Up enters, you may exile a nonland card with mana value 3 or less from your
-- hand. If you do, it becomes plotted" -- CR 702.170c's route into
-- Object.plotted, the one that is NOT CR 116.2k's special action. Its second
-- ability ("whenever a legendary creature you control enters, put a +1/+1
-- counter on each creature you control") is printed and has nothing to fire on
-- this board.
--
-- EXACTLY ONE Forest, one Plains and one Island: the mana cost is {G}{W}{U}, so
-- the board pays it to the last mana and every land is tapped by the time the
-- plotted card is offered. That is what makes the later cast discriminating, the
-- Djinn group's argument one rule over -- a cast priced at anything at all could
-- not be paid.
--
-- The GOBLIN PIKER (2/1) is what the plotted card's own trigger can aim at. Aloe
-- Alchemist prints "when this card becomes plotted, target creature gets +3/+2
-- and gains trample": with no legal target the trigger is removed on resolution
-- (CR 608.2b) and an implementation that stamped the card without recording
-- GameEvent.Plotted would be indistinguishable from one that did. 2/1 to 5/3 is
-- a value nothing else on this board produces.
--
-- The DJINN OF FOOL'S FALL ({3}{U}, mana value 4) is the filter's negative
-- control. It is the only other card in the hand, so "mana value 3 or less"
-- leaves exactly one candidate and CR 608.2d's prompt is elided -- which the
-- prompt log below asserts, since a widened filter would raise it.
kellanBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
kellanBoard forest plains island piker kellan aloe djinn =
  let lands = S.landsFor plains S.alice 1 (S.landsFor island S.alice 1 (S.landsInPlay forest 1))
      (pikerId, g1) = S.addCreature piker S.alice lands
      (kellanId, g2) = S.addHandCard kellan S.alice g1
      (_, g3) = S.addHandCard aloe S.alice g2
      (djinnId, g4) = S.addHandCard djinn S.alice g3
   in ( pikerId,
        kellanId,
        djinnId,
        g4
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      )

-- Says yes to CR 603.5's "may", aims Aloe Alchemist's trigger at the named
-- creature, and records every CR 608.2d hand choice it is asked.
--
-- The RECORD is the point: Pawl.Types.Prompt.ChooseCardInHand is raised only at
-- two or more candidates, so an empty log is the card's own filter having
-- admitted exactly one card. Aiming the trigger is pinned by id rather than left
-- to the default answerer, which takes the smallest recipient and would find the
-- Piker again whatever the payload did.
kellanAnswers :: ObjectId.ObjectId -> (forall r. Prompt.Prompt r -> Log r)
kellanAnswers pikerId prompt = case prompt of
  Prompt.ChooseOptional {} -> pure OptionalDecision.Exercises
  Prompt.ChooseCardInHand _ pid _ _ -> do
    State.modify' (<> [pid])
    pure (S.identityAnswer prompt)
  Prompt.ChooseTargets _ _ _ sets -> pure (S.preferring ((== Just pikerId) . Recipient.objectOf) sets)
  _ -> pure (S.identityAnswer prompt)

makePlotted :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
makePlotted s registry = Spec.describe s "CR 702.170c Kellan Joins Up" $ do
  -- The whole route in one game: alice casts the enchantment, its CR 603.2
  -- trigger resolves, the chosen card leaves her hand for exile and BECOMES
  -- PLOTTED there -- which the board reports twice over, once through the stamp
  -- CR 702.170d reads and once through the trigger the plotted card itself
  -- prints. The two fail independently, which is why both are asserted.
  Spec.it s "CR 702.170c an effect makes an exiled card plotted, stamp and event alike" $ do
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    kellan <- S.printingOf s registry "Kellan Joins Up"
    aloe <- S.printingOf s registry "Aloe Alchemist"
    djinn <- S.printingOf s registry "Djinn of Fool's Fall"
    let (pikerId, kellanId, djinnId, gs) = kellanBoard forest plains island piker kellan aloe djinn
        (asked, after) = case State.runState (Engine.runGame (kellanAnswers pikerId) gs (S.cast S.alice kellanId >> Engine.priorityLoop)) [] of
          ((_, g), log') -> (log', g)
        -- CR 702.170d's "any turn AFTER the turn in which it became plotted",
        -- with the turn number moved and nothing else -- so every land is still
        -- tapped and a cast priced at anything could not be paid.
        later = after {GameState.turnNumber = GameState.turnNumber after + 1}
        -- The control for that: the same card in the same exile on the same
        -- later turn, with the stamp cleared. It is what says the permission
        -- came from Object.plotted rather than from the card being in exile.
        unplotted = later {GameState.objects = Map.map (\o -> o {Object.plotted = Nothing}) (GameState.objects later)}
    Spec.assertEqWith s "the Piker started 2/1" (S.powerToughnessOf pikerId gs) (Just (2, 1))
    -- The EVENT leg. Aloe Alchemist's "when this card becomes plotted" fires from
    -- exile, so an arm that wrote the stamp and recorded nothing leaves the Piker
    -- where it started while every zone assertion below still passes.
    Spec.assertEqWith s "CR 702.170c the plotted card's own trigger fired: the Piker is 5/3" (S.powerToughnessOf pikerId after) (Just (5, 3))
    Spec.assertBool s (Projection.hasKeyword Keyword.Trample pikerId after) "and gained trample, the trigger's other half"
    Spec.assertBool s (Maybe.isJust (soleExile after)) "the card was exiled, so the cases below are about a card in exile"
    Monad.forM_ (soleExile after) $ \exiledId -> do
      -- The STAMP leg, and its two halves. CR 702.170d refuses the turn the card
      -- became plotted, which is what tells a turn number from a bare flag: an
      -- arm stamping the turn before, or stamping True, passes the `later` case
      -- and fails this one.
      Spec.assertBool s (not (S.castable S.alice exiledId after)) "CR 702.170d not on the turn it became plotted"
      Spec.assertBool s (S.castable S.alice exiledId later) "CR 702.170d on the next turn it is castable -- with every land still tapped, so the cast is free"
      Spec.assertBool s (not (S.castable S.alice exiledId unplotted)) "the control: the same card in the same exile, unplotted, is castable by nobody"
      Spec.assertEqWith
        s
        "and the exiled card is Aloe Alchemist, stamped with the turn it became plotted"
        (fmap Object.plotted (Game.lookupObject exiledId after))
        (Just (Just (GameState.turnNumber after)))
      Spec.assertEqWith
        s
        "the exiled card is the Alchemist"
        (fmap S.nameOf (Game.cardOf exiledId after))
        (Just (S.printingName aloe))
    -- The FILTER, read off the hand it left behind: "nonland card with mana value
    -- 3 or less" admitted the Alchemist and refused the mana value 4 Djinn.
    Spec.assertEqWith s "alice's hand is the Djinn alone" (Game.zoneMembers Zone.Hand S.alice after) [djinnId]
    Spec.assertEqWith s "so no CR 608.2d choice was raised: the filter left one candidate" asked []
    Spec.assertEqWith s "all three lands paid for the enchantment" (S.tappedCount S.alice after) 3
    Spec.assertEqWith s "and the stack is empty, so the trigger resolved" (GameState.stack after) []
  -- The permission taken rather than merely asked about, the Djinn group's last
  -- case one route over: the plotted card reaches the battlefield off a board
  -- with no untapped land on it.
  Spec.it s "CR 702.170d the card an effect plotted casts for nothing on a later turn" $ do
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    kellan <- S.printingOf s registry "Kellan Joins Up"
    aloe <- S.printingOf s registry "Aloe Alchemist"
    djinn <- S.printingOf s registry "Djinn of Fool's Fall"
    let (pikerId, kellanId, _, gs) = kellanBoard forest plains island piker kellan aloe djinn
        after = snd (State.evalState (Engine.runGame (kellanAnswers pikerId) gs (S.cast S.alice kellanId >> Engine.priorityLoop)) [])
        later = after {GameState.turnNumber = GameState.turnNumber after + 1}
    Monad.forM_ (soleExile after) $ \exiledId -> do
      let resolved = S.runPure S.castAnswer later (S.cast S.alice exiledId >> Stack.resolveTop)
      Spec.assertEqWith
        s
        "the Alchemist is on the battlefield"
        (S.countOnBattlefieldByName (S.printingName aloe) S.alice resolved)
        1
      Spec.assertEqWith s "exile is empty" (length (GameState.exile resolved)) 0
      Spec.assertEqWith s "and the three lands are still the only tapped permanents: the cast paid nothing" (S.tappedCount S.alice resolved) 3

-- Augury Raven (KHM 44) on alice's own precombat main, holding four Islands, the
-- Raven and a Doomed Traveler.
--
-- FOUR Islands, and each pair is spent by a different rule: CR 116.2h's {2} takes
-- two, and the Raven's foretell cost of {1}{U} takes the other two on the later
-- turn. That is what makes the cast assertions discriminating -- the two Islands
-- left standing are exactly the foretell cost, so a cast priced at the printed
-- {3}{U} cannot be paid and a cast priced at nothing leaves them untapped.
--
-- The Traveler is the negative control -- a hand card with no foretell, so an
-- implementation that offered the action for every hand card fails.
foretellBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
foretellBoard island raven traveler =
  let (ravenId, gs1) = S.addHandCard raven S.alice (S.landsInPlay island 4)
      (travelerId, gs2) = S.addHandCard traveler S.alice gs1
   in ( ravenId,
        travelerId,
        gs2
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      )

-- Tap one of alice's untapped permanents, and nothing else -- the one difference
-- between the two boards the cast's price is read off.
tapOne :: GameState.GameState -> GameState.GameState
tapOne gs =
  let untapped oid = fmap Object.tapped (Game.lookupObject oid gs) == Just TapState.Untapped
   in case filter untapped (Game.zoneMembers Zone.Battlefield S.alice gs) of
        oid : _ -> gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}
        [] -> gs

foretelling :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
foretelling s registry = Spec.describe s "CR 116.2h Augury Raven" $ do
  -- CR 116.2h's window is a THIRD one: "any time a player has priority DURING
  -- THEIR TURN" -- wider than CR 116.2k's plot, which also wants a main phase and
  -- an empty stack, and narrower than CR 116.2b's, which wants only priority. The
  -- positive board carries a spell on the stack, so an implementation that reused
  -- Turn.sorcerySpeedWindow fails it; the opponent's-turn board moves the one
  -- remaining conjunct and nothing else.
  Spec.it s "the action is offered only for a card with foretell, and only on its owner's turn" $ do
    island <- S.printingOf s registry "Island"
    raven <- S.printingOf s registry "Augury Raven"
    traveler <- S.printingOf s registry "Doomed Traveler"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (ravenId, travelerId, gs) = foretellBoard island raven traveler
        stackBusy = snd (S.spellOnStack bolt S.alice gs)
        opponentsTurn = stackBusy {GameState.activePlayer = S.bob}
    Spec.assertBool s (List.elem (Action.Type.Foretell ravenId) (Action.legalActions S.alice stackBusy)) "the Raven may be foretold, with a spell on the stack"
    Spec.assertBool s (List.notElem (Action.Type.Foretell travelerId) (Action.legalActions S.alice stackBusy)) "the Doomed Traveler may not"
    Spec.assertBool s (List.notElem (Action.Type.Foretell ravenId) (Action.legalActions S.alice opponentsTurn)) "and not on an opponent's turn"
  -- CR 116.2h's "may pay {2}": an action whose cost cannot be paid is not
  -- offered. The pair differs in the mana available and in nothing else -- one
  -- Island against the two the rule asks for.
  Spec.it s "the action is not offered when the {2} cannot be paid" $ do
    island <- S.printingOf s registry "Island"
    raven <- S.printingOf s registry "Augury Raven"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let (ravenId, _, gs) = foretellBoard island raven traveler
        (poorId, poor) = case S.addHandCard raven S.alice (S.landsInPlay island 1) of
          (oid, g) -> (oid, g {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice})
    Spec.assertBool s (List.elem (Action.Type.Foretell ravenId) (Action.legalActions S.alice gs)) "two Islands pay {2}"
    Spec.assertBool s (List.notElem (Action.Type.Foretell poorId) (Action.legalActions S.alice poor)) "one does not"
  -- CR 702.143b: the action does not use the stack. The prompt log proves it the
  -- way the plot group's does -- alice acts and is asked again before bob is
  -- asked anything. CR 116.3's retained priority is that same sequence read the
  -- other way, and is asserted once for the whole family in the Vultures group.
  Spec.it s "CR 702.143b taking it exiles the card face down without using the stack" $ do
    island <- S.printingOf s registry "Island"
    raven <- S.printingOf s registry "Augury Raven"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let (ravenId, _, gs) = foretellBoard island raven traveler
        (asked, after) = case State.runState (Engine.runGame (takeThenPass (Action.Type.Foretell ravenId)) gs Engine.priorityLoop) [] of
          ((_, g), log') -> (log', g)
    Spec.assertEqWith
      s
      "alice acts, alice is asked again, and only then is bob asked"
      asked
      [S.alice, S.alice, S.bob]
    Spec.assertEqWith
      s
      "the Raven is in exile"
      (fmap (\oid -> fmap S.nameOf (Game.cardOf oid after)) (soleExile after))
      (Just (Just (S.printingName raven)))
    Spec.assertEqWith s "the Traveler is still in hand" (S.handSize S.alice after) 1
    Spec.assertEqWith s "and the stack is empty" (GameState.stack after) []
    -- CR 116.2h's own words -- "and exile that card FACE DOWN", against CR
    -- 406.3's face-up default.
    Spec.assertEqWith
      s
      "the exiled card is face down"
      (soleExile after >>= \oid -> fmap Object.exiledFaceDown (Game.lookupObject oid after))
      (Just True)
    -- CR 702.143a's foretold card, which is what the later cast is read off: an
    -- arm that exiled the card and stamped nothing passes every assertion above.
    Spec.assertEqWith
      s
      "the exiled card is foretold, stamped with this turn"
      (soleExile after >>= \oid -> fmap Object.foretold (Game.lookupObject oid after))
      (Just (Just (GameState.turnNumber after)))
    -- CR 116.2h's {2}, observed rather than inferred: two of the four Islands
    -- paid for it and two are still standing.
    Spec.assertEqWith s "two Islands paid the {2}" (S.tappedCount S.alice after) 2
  -- CR 702.143a: "they may cast that card AFTER THE CURRENT TURN HAS ENDED by
  -- paying any foretell cost it has". Every board below is the state the special
  -- action left behind, with one thing moved: the turn number, the caster, the
  -- stamp, or one land.
  Spec.it s "CR 702.143a the foretold card is castable only by its owner, only later, and only for the foretell cost" $ do
    island <- S.printingOf s registry "Island"
    raven <- S.printingOf s registry "Augury Raven"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let (ravenId, _, gs) = foretellBoard island raven traveler
        after = snd (State.evalState (Engine.runGame (takeThenPass (Action.Type.Foretell ravenId)) gs Engine.priorityLoop) [])
        later = after {GameState.turnNumber = GameState.turnNumber after + 1}
        unforetold = later {GameState.objects = Map.map (\o -> o {Object.foretold = Nothing}) (GameState.objects later)}
        -- CR 307.5's window belongs to whoever's turn it is, so bob's case is
        -- asked on bob's turn or it fails for the timing rather than for the
        -- ownership -- the plot group's argument unchanged. bob gets two Islands
        -- of his own on the SAME board for the same reason one rule further on:
        -- rule 702.143a's cast is not free, so a bob with no mana would be
        -- refused for the price rather than for the ownership. Both of bob's
        -- boards carry them, so the pair below still differs in the owner alone.
        bobsTurn = S.landsFor island S.bob 2 (later {GameState.activePlayer = S.bob})
        bobOwns oid = bobsTurn {GameState.objects = Map.adjust (\o -> o {Object.owner = S.bob}) oid (GameState.objects bobsTurn)}
    Spec.assertBool s (Maybe.isJust (soleExile after)) "the card was exiled, so the cases below are about a card in exile"
    Monad.forM_ (soleExile after) $ \exiledId -> do
      Spec.assertBool s (not (S.castable S.alice exiledId after)) "not on the turn it was foretold"
      Spec.assertBool s (S.castable S.alice exiledId later) "on the next turn it is castable, off the two Islands the {2} left standing"
      Spec.assertBool s (not (S.castable S.bob exiledId bobsTurn)) "and not by a player who does not own it"
      Spec.assertBool s (S.castable S.bob exiledId (bobOwns exiledId)) "the control: bob's own turn and bob's own foretold card is castable, so the refusal above was the ownership"
      Spec.assertBool s (not (S.castable S.alice exiledId unforetold)) "the control: the same card in the same exile, not foretold, is castable by nobody"
      -- The price, from both sides. Two Islands are enough, which the printed
      -- {3}{U} would not be; ONE is not enough, which a cast charging nothing
      -- would be. The pair differs in a single tapped land.
      Spec.assertBool s (not (S.castable S.alice exiledId (tapOne later))) "one Island does not pay {1}{U}, so the cast is not free"
  -- The offer taken rather than merely asked about: the Raven reaches the
  -- battlefield and the last two Islands go down with it, which is CR 702.143a's
  -- "paying any foretell cost it has" observed.
  Spec.it s "CR 702.143a casting it costs the foretell cost" $ do
    island <- S.printingOf s registry "Island"
    raven <- S.printingOf s registry "Augury Raven"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let (ravenId, _, gs) = foretellBoard island raven traveler
        after = snd (State.evalState (Engine.runGame (takeThenPass (Action.Type.Foretell ravenId)) gs Engine.priorityLoop) [])
        later = after {GameState.turnNumber = GameState.turnNumber after + 1}
    Monad.forM_ (soleExile after) $ \exiledId -> do
      let resolved = S.runPure S.castAnswer later (S.cast S.alice exiledId >> Stack.resolveTop)
      Spec.assertEqWith
        s
        "the Raven is on the battlefield"
        (S.countOnBattlefieldByName (S.printingName raven) S.alice resolved)
        1
      Spec.assertEqWith s "exile is empty" (length (GameState.exile resolved)) 0
      Spec.assertEqWith s "and all four Islands are tapped: {2} for the action, {1}{U} for the cast" (S.tappedCount S.alice resolved) 4

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = do
  circlingVultures s registry
  dampingEngine s registry
  leoninArbiter s registry
  plotting s registry
  makePlotted s registry
  foretelling s registry

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
      (fmap (\oid -> GameEvent.Discarded (Discarded.MkDiscarded S.alice oid DiscardCause.Ordinary)) graveyard)
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
