{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 116.2e and CR 116.2d end to end, on BOTH axes an ability can be
-- aimed on -- a player (Leonin Arbiter, Damping Engine) and an object (Volrath's
-- Curse): Pawl.Types.SpecialAction and the
-- Pawl.Types.Face.specialActions field that carries both, Pawl.Engine.Action's
-- discardableCards and Pawl.Engine.Ignore's ignorable with the actions they
-- offer, and Pawl.Engine.Engine's arms for them. CR 116.3 -- "if a player takes
-- a special action, that player receives priority afterward" -- is asserted here
-- ONCE for the whole family, see #875; the CR 116.2b, CR 116.2d and CR 116.2m arms
-- retain priority the same way and are not separately re-asserted.
--
-- CR 116.2k's plot (Djinn of Fool's Fall, and from a library under CR 702.170f
-- with Fblthp, Lost on the Range) and CR 116.2h's foretell (Augury Raven) are
-- covered here too, each with its own board: the three windows rule
-- 116.2 states -- any priority, the owner's own turn, and sorcery speed -- are
-- what the groups' offer cases tell apart.
--
-- CR 116.2f's suspend (Rift Bolt) is here too, on its own board: its window is a
-- FOURTH shape -- the card's own castability, which is what CR 116.2f states
-- instead of a phase -- and CR 702.62's other two abilities ride the same board,
-- since the countdown and the free play are what the exile was for. Rule
-- 702.62a's last sentence -- the haste -- is a second suspend board (Durkwood
-- Baloth), because what it needs is a creature and a combat rather than a
-- countdown.
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
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Ignore as Ignore
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Plot as Plot
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Suspend as Suspend
import qualified Pawl.Registry as Registry
import qualified Pawl.Scenario as Scenario
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource
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

-- CR 116.2d names an ability rather than a permanent, and these are the names
-- the two fixtures' faces print. Damping Engine's one sentence declares TWO
-- player-ability rows and both carry "the lead", which is what keeps one payment
-- covering both.
theLead :: AbilityName.AbilityName
theLead = AbilityName.MkAbilityName (Text.pack "the lead")

searchBan :: AbilityName.AbilityName
searchBan = AbilityName.MkAbilityName (Text.pack "search ban")

-- Synthetic Warden of Divided Edicts names ONE of its two player abilities. Its
-- "players can't play lands" names none, which is what makes the two readings of
-- CR 116.2d disagree observably.
creatureBan :: AbilityName.AbilityName
creatureBan = AbilityName.MkAbilityName (Text.pack "creature ban")

-- Volrath's Curse names its one sentence, whose three rows -- two combat
-- restrictions and an activation prohibition -- all carry it.
thisEffect :: AbilityName.AbilityName
thisEffect = AbilityName.MkAbilityName (Text.pack "this effect")

isPlay :: Action.Type.Action -> Bool
isPlay action = case action of
  Action.Type.Play {} -> True
  Action.Type.Pass -> False
  Action.Type.Cast {} -> False
  Action.Type.Activate _ _ -> False
  Action.Type.TurnFaceUp {} -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Plot {} -> False
  Action.Type.Foretell _ -> False
  Action.Type.Suspend _ -> False
  Action.Type.PutCompanionIntoHand -> False
  Action.Type.RollPlanarDie -> False
  Action.Type.Ignore _ _ -> False
  Action.Type.EndEffect _ -> False
  Action.Type.ActivateManaAbility _ -> False

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
  let (arbiterId, gs1) = S.addPermanent arbiter S.alice (S.landsInPlay forest 9)
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
  Prompt.Search _ _ candidates cap -> do
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
  Action.Type.Plot {} -> False
  Action.Type.Foretell _ -> False
  Action.Type.Suspend _ -> False
  Action.Type.PutCompanionIntoHand -> False
  Action.Type.RollPlanarDie -> False
  Action.Type.Ignore _ _ -> False
  Action.Type.EndEffect _ -> False
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
  Action.Type.Plot {} -> False
  Action.Type.Foretell _ -> False
  Action.Type.Suspend _ -> False
  Action.Type.PutCompanionIntoHand -> False
  Action.Type.RollPlanarDie -> False
  Action.Type.Ignore _ _ -> False
  Action.Type.EndEffect _ -> False
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
  let (engineId, gs1) = S.addPermanent engine S.alice S.threePlayerGame
      (victimId, gs2) = S.addPermanent forest S.alice gs1
      (_, gs3) = S.addPermanent forest S.alice gs2
      (_, gs4) = S.addPermanent forest S.alice gs3
      (_, gs5) = S.addPermanent forest S.bob gs4
      (_, gs6) = S.addPermanent forest S.bob gs5
      (_, gs7) = S.addPermanent forest S.carol gs6
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
  let add g = snd (S.addPermanent forest S.bob g)
   in add (add (add gs))

-- Synthetic Warden of Divided Edicts on bob's precombat main. alice controls the
-- Warden and three Forests; bob controls four and holds a Forest, a Woodland
-- Changeling and a Rampant Growth.
--
-- WHAT MAKES THE TWO READINGS DISAGREE: the Warden prints two unrelated player
-- abilities and grants the ignore on ONE of them by name -- "your opponents
-- can't cast creature spells" carries the name, "players can't play lands" does
-- not. So alice is reached by the permanent but not by the named ability, and a
-- payment that lifted the permanent would lift a land ban the deal never
-- mentioned.
--
-- THE SEATS ARE ASYMMETRIC on purpose: the named ability's scope is Opponents, so
-- alice (its controller) is outside it while bob is inside, and every seat
-- controls a permanent so the sacrifice is payable at both -- which leaves CR
-- 116.2d's own WHO as the only conjunct that can separate them.
--
-- The Growth is the cost control and shares the Changeling's exact {1}{G} off the
-- same Forests: the Warden stops creature spells only, so a sorcery must stay
-- castable on every board here.
--
-- Taking the permanent to place as an argument is what gives the land-play
-- assertions their pair: passing a vanilla creature builds the same board with
-- nothing prohibiting anything.
wardenBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
wardenBoard warden forest changeling growth =
  let (wardenId, gs1) = S.addPermanent warden S.alice (S.landsInPlay forest 3)
      (victimId, gs2) = S.addPermanent forest S.bob gs1
      (_, gs3) = S.addPermanent forest S.bob gs2
      (_, gs4) = S.addPermanent forest S.bob gs3
      (_, gs5) = S.addPermanent forest S.bob gs4
      (aliceForestId, gs6) = S.addHandCard forest S.alice gs5
      (bobForestId, gs7) = S.addHandCard forest S.bob gs6
      (changelingId, gs8) = S.addHandCard changeling S.bob gs7
      (growthId, gs9) = S.addHandCard growth S.bob gs8
   in ( wardenId,
        victimId,
        aliceForestId,
        bobForestId,
        changelingId,
        growthId,
        gs9
          { GameState.activePlayer = S.bob,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.bob
          }
      )

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

-- Djinn of Fool's Fall's printed plot cost, {3}{U}.
djinnPlotCost :: Cost.Cost Keyword.Keyword
djinnPlotCost = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3, ManaSymbol.OfType (ManaType.Colored Color.Blue)])) []

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
    Spec.assertBool s (List.elem (Action.Type.Plot djinnId djinnPlotCost) actions) "the Djinn may be plotted"
    Spec.assertBool s (List.notElem (Action.Type.Plot travelerId djinnPlotCost) actions) "the Doomed Traveler may not"
    Spec.assertBool s (List.notElem (Action.Type.Plot djinnId djinnPlotCost) (Action.legalActions S.alice opponentsTurn)) "not on an opponent's turn"
    Spec.assertBool s (List.notElem (Action.Type.Plot djinnId djinnPlotCost) (Action.legalActions S.alice stackBusy)) "and not with a spell on the stack"
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
    Spec.assertBool s (List.elem (Action.Type.Plot djinnId djinnPlotCost) (Action.legalActions S.alice gs)) "four Islands pay {3}{U}"
    Spec.assertBool s (List.notElem (Action.Type.Plot poorId djinnPlotCost) (Action.legalActions S.alice poor)) "three do not"
  -- The offer taken rather than merely asked about: the Djinn reaches the
  -- battlefield off a board with no untapped land on it, which is CR 702.170d's
  -- "without paying its mana cost" observed rather than inferred.
  Spec.it s "CR 702.170d casting it costs nothing" $ do
    island <- S.printingOf s registry "Island"
    djinn <- S.printingOf s registry "Djinn of Fool's Fall"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let (djinnId, _, gs) = plotBoard island djinn traveler
        after = snd (State.evalState (Engine.runGame (takeThenPass (Action.Type.Plot djinnId djinnPlotCost)) gs Engine.priorityLoop) [])
        later = after {GameState.turnNumber = GameState.turnNumber after + 1}
    Monad.forM_ (soleExile after) $ \exiledId -> do
      let resolved = S.runPure S.castAnswer later (S.cast S.alice exiledId >> Stack.resolveTop)
      Spec.assertEqWith
        s
        "the Djinn is on the battlefield"
        (S.countOnBattlefieldByName (S.printingName djinn) S.alice resolved)
        1
      Spec.assertEqWith s "and exile is empty" (length (GameState.exile resolved)) 0

-- Fblthp, Lost on the Range (OTJ 48) on alice's battlefield, her precombat main
-- with the stack empty -- CR 702.170a's window -- and FIVE Islands: enough for the
-- Djinn of Fool's Fall's {4}{U} mana cost to the last mana, so every case that
-- pays it leaves nothing untapped.
--
-- The library is stocked bottom first, so `top` is the card on top (CR 401.2)
-- and a second Djinn sits under it. That Djinn is the top-card narrowing's
-- control: it has plot of its own, so an implementation that opened the whole
-- library would offer it.
--
-- `fblthp` is a Maybe so the paired board without it differs in exactly that.
--
-- Not implemented: "You may look at the top card of your library any time",
-- which data/cards/fblthp-lost-on-the-range.json omits under the precedent
-- data/cards/garruks-horde.json sets -- pawl hands every answerer the whole game
-- already, so a looked-at card is indistinguishable from a hidden one (#1412).
fblthpBoard ::
  Printing.Printing ->
  Maybe Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
fblthpBoard island fblthp djinn top =
  let lands = S.landsInPlay island 5
      withFblthp = case fblthp of
        Nothing -> lands
        Just printing -> snd (S.addPermanent printing S.alice lands)
      (underId, g1) = S.addLibraryCard djinn S.alice withFblthp
      (topId, g2) = S.addLibraryCard top S.alice g1
   in ( topId,
        underId,
        g2
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      )

-- Djinn of Fool's Fall's mana cost, {4}{U} -- the plot cost Fblthp grants it.
djinnManaCost :: Cost.Cost Keyword.Keyword
djinnManaCost = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4, ManaSymbol.OfType (ManaType.Colored Color.Blue)])) []

-- The plot actions on offer for one card.
plotsOf :: ObjectId.ObjectId -> [Action.Type.Action] -> [Action.Type.Action]
plotsOf oid = filter (\action -> case action of Action.Type.Plot target _ -> target == oid; _ -> False)

plottingFromLibrary :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
plottingFromLibrary s registry = Spec.describe s "CR 702.170f Fblthp, Lost on the Range" $ do
  -- CR 702.170f's plot from the top of a library, and the ruling that a card
  -- with plot of its own may use either cost. The pair differs only in Fblthp.
  Spec.it s "the top card may be plotted for its own plot cost or its mana cost" $ do
    island <- S.printingOf s registry "Island"
    fblthp <- S.printingOf s registry "Fblthp, Lost on the Range"
    djinn <- S.printingOf s registry "Djinn of Fool's Fall"
    let (topId, underId, gs) = fblthpBoard island (Just fblthp) djinn djinn
        (bareTopId, _, bare) = fblthpBoard island Nothing djinn djinn
        actions = Action.legalActions S.alice gs
    Spec.assertEqWith
      s
      "both plot costs are offered for the top card"
      (plotsOf topId actions)
      [Action.Type.Plot topId djinnPlotCost, Action.Type.Plot topId djinnManaCost]
    Spec.assertEqWith s "the card under it is not offered" (plotsOf underId actions) []
    Spec.assertEqWith s "the control: without Fblthp nothing in the library is" (plotsOf bareTopId (Action.legalActions S.alice bare)) []
  -- "You may plot NONLAND cards": a land on top is not offered. Twiddle ({U}, no
  -- plot of its own) is the control on the same board, offered at its mana cost
  -- -- the granted plot alone.
  --
  -- A REGRESSION FENCE for the nonland filter, not a proof of it: the Island's
  -- granted plot cost is its absent mana cost, unpayable by CR 118.6, so the
  -- refusal holds without the filter. Only a land with a mana cost can tell the
  -- two apart -- Scryfall `t:land mv>0 -is:dfc`, 2026-09-28, answers Glade of
  -- the Pump Spells alone, a playtest card whose "pay {2}{G} to play this land"
  -- pawl cannot state.
  Spec.it s "a land on top cannot be plotted, and a nonland card without plot can" $ do
    island <- S.printingOf s registry "Island"
    fblthp <- S.printingOf s registry "Fblthp, Lost on the Range"
    djinn <- S.printingOf s registry "Djinn of Fool's Fall"
    twiddle <- S.printingOf s registry "Twiddle"
    let (landId, _, landTop) = fblthpBoard island (Just fblthp) djinn island
        (twiddleId, _, twiddleTop) = fblthpBoard island (Just fblthp) djinn twiddle
        twiddleCost = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Blue)])) []
    Spec.assertEqWith s "the Island on top is not offered" (plotsOf landId (Action.legalActions S.alice landTop)) []
    Spec.assertEqWith s "Twiddle on top is offered at {U}" (plotsOf twiddleId (Action.legalActions S.alice twiddleTop)) [Action.Type.Plot twiddleId twiddleCost]
  -- CR 702.170a's window is unchanged by CR 702.170f.
  Spec.it s "only at sorcery speed" $ do
    island <- S.printingOf s registry "Island"
    fblthp <- S.printingOf s registry "Fblthp, Lost on the Range"
    djinn <- S.printingOf s registry "Djinn of Fool's Fall"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (topId, _, gs) = fblthpBoard island (Just fblthp) djinn djinn
        opponentsTurn = gs {GameState.activePlayer = S.bob}
        stackBusy = snd (S.spellOnStack bolt S.alice gs)
    Spec.assertEqWith s "not on an opponent's turn" (plotsOf topId (Action.legalActions S.alice opponentsTurn)) []
    Spec.assertEqWith s "and not with a spell on the stack" (plotsOf topId (Action.legalActions S.alice stackBusy)) []
  -- The action taken at the GRANTED cost: CR 702.170f's "exiled from the zone it
  -- is in", CR 702.170b's no stack, and CR 702.170d's free cast a turn later.
  -- {4}{U} taps all five Islands where the printed {3}{U} would leave one, so the
  -- tapped count is what says which cost was paid.
  Spec.it s "CR 702.170f plotting the top card exiles it from the library, and it is cast free later" $ do
    island <- S.printingOf s registry "Island"
    fblthp <- S.printingOf s registry "Fblthp, Lost on the Range"
    djinn <- S.printingOf s registry "Djinn of Fool's Fall"
    let (topId, underId, gs) = fblthpBoard island (Just fblthp) djinn djinn
        (asked, after) = case State.runState (Engine.runGame (takeThenPass (Action.Type.Plot topId djinnManaCost)) gs Engine.priorityLoop) [] of
          ((_, g), log2) -> (log2, g)
        later = after {GameState.turnNumber = GameState.turnNumber after + 1}
    Spec.assertEqWith
      s
      "the exiled card is plotted, stamped with this turn"
      (soleExile after >>= \oid -> fmap Object.plotted (Game.lookupObject oid after))
      (Just (Just (GameState.turnNumber after)))
    Spec.assertEqWith s "the library's top card is now the Djinn that was under it" (Game.zoneMembers Zone.Library S.alice after) [underId]
    Spec.assertEqWith s "all five Islands paid {4}{U}" (S.tappedCount S.alice after) 5
    Spec.assertEqWith s "alice acts, alice is asked again, and only then is bob asked" asked [S.alice, S.alice, S.bob]
    Spec.assertEqWith s "and the stack is empty" (GameState.stack after) []
    Monad.forM_ (soleExile after) $ \exiledId -> do
      Spec.assertBool s (not (S.castable S.alice exiledId after)) "not castable on the turn it became plotted"
      let resolved = S.runPure S.castAnswer later (S.cast S.alice exiledId >> Stack.resolveTop)
      Spec.assertEqWith s "CR 702.170d cast free on a later turn, the Djinn is on the battlefield" (S.countOnBattlefieldByName (S.printingName djinn) S.alice resolved) 1
  -- Plot from a hand is CR 702.170a's alone: Fblthp grants nothing there, so the
  -- Djinn in hand is offered at {3}{U} and never at its mana cost.
  Spec.it s "a card in hand keeps its own plot cost only" $ do
    island <- S.printingOf s registry "Island"
    fblthp <- S.printingOf s registry "Fblthp, Lost on the Range"
    djinn <- S.printingOf s registry "Djinn of Fool's Fall"
    let (_, _, stocked) = fblthpBoard island (Just fblthp) djinn djinn
        (handId, gs) = S.addHandCard djinn S.alice stocked
    Spec.assertEqWith s "the Djinn in hand is offered at {3}{U} alone" (plotsOf handId (Action.legalActions S.alice gs)) [Action.Type.Plot handId djinnPlotCost]
  -- CR 107.3d: a granted plot cost is a mana cost, and Braingeyser's holds an X
  -- the player names before paying. The pair differs only in the answer.
  Spec.it s "CR 107.3d the X in a granted plot cost is the player's to name" $ do
    island <- S.printingOf s registry "Island"
    fblthp <- S.printingOf s registry "Fblthp, Lost on the Range"
    djinn <- S.printingOf s registry "Djinn of Fool's Fall"
    braingeyser <- S.printingOf s registry "Braingeyser"
    let (topId, _, gs) = fblthpBoard island (Just fblthp) djinn braingeyser
        geyserCost = Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Variable, ManaSymbol.OfType (ManaType.Colored Color.Blue), ManaSymbol.OfType (ManaType.Colored Color.Blue)])) []
        naming :: Natural.Natural -> Prompt.Prompt r -> r
        naming x p = case p of
          Prompt.ChooseX {} -> x
          _ -> S.identityAnswer p
        plottedWith x = S.runPure (naming x) gs (Plot.plot S.manaPerformer S.alice topId geyserCost)
    Spec.assertEqWith s "X = 1 taps three Islands" (S.tappedCount S.alice (plottedWith 1)) 3
    Spec.assertEqWith s "X = 3 taps all five" (S.tappedCount S.alice (plottedWith 3)) 5
    Spec.assertBool s (Maybe.isJust (soleExile (plottedWith 1))) "and the card was plotted"

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

-- Ethereal Valkyrie (KHC 3) {4}{W}{U} Creature -- Spirit Angel, "Flying /
-- Whenever this creature enters or attacks, draw a card, then exile a card from
-- your hand face down. It becomes foretold. Its foretell cost is its mana cost
-- reduced by {2}" -- checked against Scryfall, 2026-09-15. CR 702.143d's route
-- into Object.foretold, the one that is NOT CR 116.2h's special action, and the
-- only printed one that also gives the exiled card a foretell cost. The attack
-- half of its trigger is printed and has no combat to fire in on this board.
--
-- EXACTLY SIX lands -- one Plains and five Islands -- because the mana cost is
-- {4}{W}{U}: the board pays it to the last mana, every land is tapped by the
-- time the foretold card is offered, and the two Islands the later boards add
-- are then the ONLY mana in the game. That is what makes the price cases
-- discriminating -- two Islands are exactly the granted {1}{U}, the printed
-- {3}{U} is a mana more than the board can ever produce, and one Island is a
-- mana less.
--
-- LORE WEAVER ({3}{U}) is the card the trigger exiles, and it prints NO foretell
-- of its own: that is the whole of what makes the granted cost observable, since
-- a card with foretell would be castable off Pawl.Engine.Keyword.foretellCost
-- whether or not the effect gave it anything.
--
-- The DOOMED TRAVELER is in the library rather than the hand, so the trigger's
-- "draw a card" has something to draw and CR 608.2d's choice is a real one --
-- two cards in hand when the exile is chosen, which is what keeps the answerer
-- below from being the only candidate there was.
valkyrieBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
valkyrieBoard plains island valkyrie lore traveler =
  let lands = S.landsFor plains S.alice 1 (S.landsInPlay island 5)
      (valkyrieId, g1) = S.addHandCard valkyrie S.alice lands
      (loreId, g2) = S.addHandCard lore S.alice g1
      (_, g3) = S.addLibraryCard traveler S.alice g2
   in ( valkyrieId,
        loreId,
        g3
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      )

-- Takes the named card out of CR 608.2d's hand choice, and falls through to the
-- default for everything else.
--
-- FILTERED rather than answered by index: the id is checked against the offered
-- set, so an implementation that never offers the Weaver exiles the other card
-- and the identity assertion below reports it, instead of the answerer quietly
-- finding whatever is there.
valkyrieAnswers :: ObjectId.ObjectId -> (forall r. Prompt.Prompt r -> r)
valkyrieAnswers loreId prompt = case prompt of
  Prompt.ChooseCardInHand _ _ _ candidates | elem loreId candidates -> loreId
  _ -> S.identityAnswer prompt

-- Tap one of alice's untapped lands OF THAT PRINTING, and nothing else -- the
-- one difference between the boards each cast's price is read off. Named rather
-- than the first untapped permanent: the Valkyrie herself is untapped on these
-- boards and tapping her moves no mana.
tapOneNamed :: Printing.Printing -> GameState.GameState -> GameState.GameState
tapOneNamed island gs =
  let untapped oid = fmap Object.tapped (Game.lookupObject oid gs) == Just TapState.Untapped
   in case filter untapped (Scenario.namedObjects (S.printingName island) gs) of
        oid : _ -> gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}
        [] -> gs

makeForetold :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
makeForetold s registry = Spec.describe s "CR 702.143d Ethereal Valkyrie" $ do
  -- The whole route in one game: alice casts the Valkyrie, its CR 603.2 trigger
  -- resolves, the chosen card leaves her hand for exile face down and BECOMES
  -- FORETOLD there with a cost the effect gave it -- neither of which the card
  -- itself prints.
  --
  -- Every board below is the state that trigger left behind with ONE thing
  -- moved: the turn number, one Island, the stamp, or the granted cost. The two
  -- Islands are added to the shared board rather than to the later one, so the
  -- same-turn refusal is the TURN and not the price.
  Spec.it s "CR 702.143d an effect makes an exiled card foretold and gives it a foretell cost" $ do
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    valkyrie <- S.printingOf s registry "Ethereal Valkyrie"
    lore <- S.printingOf s registry "Lore Weaver"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let (valkyrieId, loreId, gs) = valkyrieBoard plains island valkyrie lore traveler
        after = S.runPure (valkyrieAnswers loreId) gs (S.cast S.alice valkyrieId >> Engine.priorityLoop)
        -- The two Islands the granted {1}{U} asks for, on the turn the card
        -- became foretold and on the one after it alike.
        withMana = S.landsFor island S.alice 2 after
        later = withMana {GameState.turnNumber = GameState.turnNumber withMana + 1}
        -- The control for the COST: the same foretold card on the same later
        -- turn with only what the effect gave it taken away. The Weaver prints
        -- no foretell, so nothing is left to cast it for.
        ungranted = later {GameState.objects = Map.map (\o -> o {Object.foretellCostReduction = Nothing}) (GameState.objects later)}
        -- The control for the STAMP, the Raven group's one rule over.
        unforetold = later {GameState.objects = Map.map (\o -> o {Object.foretold = Nothing}) (GameState.objects later)}
    Spec.assertBool s (Maybe.isJust (soleExile after)) "the card was exiled, so the cases below are about a card in exile"
    Monad.forM_ (soleExile after) $ \exiledId -> do
      Spec.assertBool s (S.castable S.alice exiledId later) "CR 702.143d on a later turn it is castable, off the two Islands the granted {1}{U} asks for"
      Spec.assertBool s (not (S.castable S.alice exiledId (tapOneNamed island later))) "one Island does not pay it: the granted cost took {2} off the Weaver's {3}{U} and no more"
      Spec.assertBool s (not (S.castable S.alice exiledId ungranted)) "the control: the same foretold card with the granted cost cleared is castable by nobody, the Weaver printing no foretell of its own"
      Spec.assertBool s (not (S.castable S.alice exiledId unforetold)) "the control: the same card in the same exile, not foretold, is castable by nobody"
      Spec.assertBool s (not (S.castable S.alice exiledId withMana)) "CR 702.143d not on the turn it became foretold, off the same two Islands"
      Spec.assertEqWith
        s
        "the exiled card is the Weaver"
        (fmap S.nameOf (Game.cardOf exiledId after))
        (Just (S.printingName lore))
      Spec.assertEqWith
        s
        "it is foretold, stamped with the turn it became one"
        (fmap Object.foretold (Game.lookupObject exiledId after))
        (Just (Just (GameState.turnNumber after)))
      -- CR 702.143d's "exile a card from your hand FACE DOWN", against CR 406.3's
      -- face-up default -- the rider the opcode's producer states rather than
      -- anything rule 702.143d itself says.
      Spec.assertEqWith
        s
        "it is face down in exile"
        (fmap Object.exiledFaceDown (Game.lookupObject exiledId after))
        (Just True)
      Spec.assertEqWith
        s
        "and the effect gave it CR 702.143d's {2} off its own mana cost"
        (fmap Object.foretellCostReduction (Game.lookupObject exiledId after))
        (Just (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])))
    Spec.assertEqWith s "the Valkyrie is on the battlefield" (S.countOnBattlefieldByName (S.printingName valkyrie) S.alice after) 1
    Spec.assertEqWith s "alice's hand is the drawn Traveler alone" (S.handSize S.alice after) 1
    Spec.assertEqWith s "six lands paid for the Valkyrie" (S.tappedCount S.alice after) 6
    Spec.assertEqWith s "and the stack is empty, so the trigger resolved" (GameState.stack after) []
  -- The permission taken rather than merely asked about, the Raven group's last
  -- case one route over: the foretold card reaches the battlefield and the two
  -- Islands go down with it, which is CR 702.143d's "cast for any foretell cost
  -- it has" observed on a cost no card printed.
  Spec.it s "CR 702.143d casting it costs the foretell cost the effect gave it" $ do
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    valkyrie <- S.printingOf s registry "Ethereal Valkyrie"
    lore <- S.printingOf s registry "Lore Weaver"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let (valkyrieId, loreId, gs) = valkyrieBoard plains island valkyrie lore traveler
        after = S.runPure (valkyrieAnswers loreId) gs (S.cast S.alice valkyrieId >> Engine.priorityLoop)
        withMana = S.landsFor island S.alice 2 after
        later = withMana {GameState.turnNumber = GameState.turnNumber withMana + 1}
    Spec.assertBool s (Maybe.isJust (soleExile after)) "the card was exiled, so the case below runs at all"
    Monad.forM_ (soleExile after) $ \exiledId -> do
      let resolved = S.runPure S.castAnswer later (S.cast S.alice exiledId >> Stack.resolveTop)
      Spec.assertEqWith
        s
        "the Weaver is on the battlefield"
        (S.countOnBattlefieldByName (S.printingName lore) S.alice resolved)
        1
      Spec.assertEqWith s "exile is empty" (length (GameState.exile resolved)) 0
      Spec.assertEqWith s "and all eight lands are tapped: six for the Valkyrie, {1}{U} for the cast" (S.tappedCount S.alice resolved) 8

-- The Valkyrie's board again with a MODAL DOUBLE-FACED card in the hand for her
-- to exile: Birgi, God of Storytelling ({2}{R}) // Harnfel, Horn of Bounty
-- ({4}{R}), whose two faces print mana costs four apart -- checked against
-- Scryfall, 2026-09-15.
--
-- That gap is the whole case. CR 712.11b lets either face be cast, and rule
-- 702.143d's granted cost is the mana cost of the face CAST -- the Valkyrie's own
-- ruling says so outright -- so the same exiled card owes {R} as Birgi and
-- {2}{R} as Harnfel. An implementation settling ONE cost when the effect
-- resolved can only have settled the front face's (CR 712.8a), and then Harnfel
-- is offered Birgi's {R}.
--
-- THREE MOUNTAINS and no other untapped land: the Valkyrie's own six are spent to
-- the last mana before these arrive, so three is exactly Harnfel's {2}{R}, one is
-- exactly Birgi's {R}, and the boards between them are the only thing the cases
-- below differ in.
birgiBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, GameState.GameState)
birgiBoard plains island mountain valkyrie birgi traveler =
  let (valkyrieId, birgiId, gs) = valkyrieBoard plains island valkyrie birgi traveler
      after = S.runPure (valkyrieAnswers birgiId) gs (S.cast S.alice valkyrieId >> Engine.priorityLoop)
      later = S.landsFor mountain S.alice 3 (after {GameState.turnNumber = GameState.turnNumber after + 1})
   in (after, later)

makeForetoldPerFace :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
makeForetoldPerFace s registry = Spec.describe s "CR 702.143d Ethereal Valkyrie and a modal double-faced card" $ do
  -- ONE assertion over all five boards rather than five: an implementation that
  -- prices both faces off the front face answers True to Harnfel at one Mountain,
  -- and a failure printing the whole vector says which face went wrong instead of
  -- stopping at the first.
  --
  -- The BIRGI legs are what make the Harnfel refusals about the FACE: the same
  -- card, in the same exile, on the same board, is castable as its front face off
  -- the one Mountain that will not pay for its back face.
  Spec.it s "CR 702.143d the granted cost is the mana cost of the face being cast" $ do
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    valkyrie <- S.printingOf s registry "Ethereal Valkyrie"
    birgi <- S.printingOf s registry "Birgi, God of Storytelling"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let (after, later) = birgiBoard plains island mountain valkyrie birgi traveler
        twoMountains = tapOneNamed mountain later
        oneMountain = tapOneNamed mountain twoMountains
        castableAs name gs = case soleExile after of
          Nothing -> False
          Just exiledId -> Cast.castable S.alice exiledId name Facing.FaceUp gs
    Spec.assertBool s (Maybe.isJust (soleExile after)) "the card was exiled, so the cases below are about a card in exile"
    Spec.assertEqWith
      s
      "CR 702.143d Harnfel's granted cost is its own {4}{R} reduced by {2}, and Birgi's is its {2}{R} reduced by {2}"
      [ ("Harnfel, three Mountains", castableAs harnfelName later),
        ("Harnfel, two Mountains", castableAs harnfelName twoMountains),
        ("Harnfel, one Mountain", castableAs harnfelName oneMountain),
        ("Birgi, one Mountain", castableAs birgiName oneMountain),
        ("Birgi, no Mountain", castableAs birgiName (tapOneNamed mountain oneMountain))
      ]
      [ ("Harnfel, three Mountains", True),
        ("Harnfel, two Mountains", False),
        ("Harnfel, one Mountain", False),
        ("Birgi, one Mountain", True),
        ("Birgi, no Mountain", False)
      ]
    -- The identity leg, AFTER the vector: the card the faces were asked about
    -- really is the double-faced one, so the refusals are the face being priced
    -- and not some other card in exile.
    Monad.forM_ (soleExile after) $ \exiledId ->
      Spec.assertEqWith
        s
        "the exiled card is Birgi // Harnfel"
        (fmap S.nameOf (Game.cardOf exiledId after))
        (Just (S.printingName birgi))
  -- The back-face cast taken rather than merely asked about: Harnfel reaches the
  -- battlefield and all three Mountains go down with it, which is the {2}{R}
  -- observed. An implementation pricing it off the front face taps one.
  Spec.it s "CR 712.11b casting the back face pays the back face's granted cost" $ do
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    valkyrie <- S.printingOf s registry "Ethereal Valkyrie"
    birgi <- S.printingOf s registry "Birgi, God of Storytelling"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let (after, later) = birgiBoard plains island mountain valkyrie birgi traveler
    Spec.assertBool s (Maybe.isJust (soleExile after)) "the card was exiled, so the case below runs at all"
    Monad.forM_ (soleExile after) $ \exiledId -> do
      let resolved = S.runPure S.castAnswer later (Cast.castSpell S.manaPerformer S.alice exiledId harnfelName Facing.FaceUp >> Stack.resolveTop)
      -- Read off Object.face, which is where CR 712.13 carries rule 712.11b's
      -- choice: S.countOnBattlefieldByName asks Card.combined, and CR 712.8a
      -- makes that the FRONT face for every incarnation of this card.
      let facesUp = Maybe.mapMaybe (\oid -> Game.lookupObject oid resolved >>= Object.face) (Game.zoneMembers Zone.Battlefield S.alice resolved)
      Spec.assertEqWith
        s
        "Harnfel is on the battlefield, the back face CR 712.13 put there"
        (filter (== harnfelName) facesUp)
        [harnfelName]
      Spec.assertEqWith s "exile is empty" (length (GameState.exile resolved)) 0
      Spec.assertEqWith s "and all nine lands are tapped: six for the Valkyrie, {2}{R} for the cast" (S.tappedCount S.alice resolved) 9

-- The two halves of Birgi, God of Storytelling // Harnfel, Horn of Bounty, named
-- rather than read off the card: CR 712.11b's choice is the test's to make.
birgiName :: CardName.CardName
birgiName = CardName.MkCardName (Text.pack "Birgi, God of Storytelling")

harnfelName :: CardName.CardName
harnfelName = CardName.MkCardName (Text.pack "Harnfel, Horn of Bounty")

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

-- Rift Bolt (TSP 165) {2}{R} Sorcery, "Rift Bolt deals 3 damage to any target. /
-- Suspend 1--{R}" -- checked against Scryfall, 2026-09-07. CR 702.62's three
-- abilities end to end: CR 116.2f's special action, the upkeep countdown, and
-- the free play when the last time counter leaves.
--
-- ONE MOUNTAIN and nothing else, which is what makes every case below
-- discriminating. {R} pays the suspend cost exactly, and {2}{R} is a mana more
-- than the board can ever produce -- so the free play at the end is observed
-- rather than inferred, and the Mountain being UNTAPPED after it is "no mana was
-- spent" read off the board.
--
-- THE DOOMED TRAVELER is the negative control for the offer: a hand card with no
-- suspend, so an implementation offering the action for every hand card fails.
--
-- THE LIBRARIES are stocked because the fixture advances two whole turns and CR
-- 104.3c would otherwise deck a player before the countdown finishes.
riftBoltBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
riftBoltBoard mountain bolt traveler =
  let (boltId, gs1) = S.addHandCard bolt S.alice (S.landsInPlay mountain 1)
      (travelerId, gs2) = S.addHandCard traveler S.alice gs1
      stocked = List.foldl' (\g pid -> List.foldl' (\h _ -> snd (S.addLibraryCard traveler pid h)) g [1 :: Int .. 8]) gs2 [S.alice, S.bob]
   in ( boltId,
        travelerId,
        stocked
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      )

-- Takes the suspend action the moment it is offered and passes on every other
-- priority; accepts CR 608.2g's offered cast; and aims the Bolt at bob, PICKED
-- OUT OF THE OFFERED SET rather than built, so a recipient the engine did not
-- offer cannot pass for a target (CR 608.2b).
--
-- Stateless, and it need not be: once the card is exiled its hand id is in no
-- action, so the first branch stops matching on its own.
suspendAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> r
suspendAnswer oid p = case p of
  Prompt.ChooseAction _ _ actions ->
    if List.elem (Action.Type.Suspend oid) actions then Action.Type.Suspend oid else Action.Type.Pass
  Prompt.OfferedCast {} -> OptionalDecision.Exercises
  Prompt.ChooseTargets _ _ _ slots ->
    Map.map (\(_, recipients) -> maybe Set.empty Set.singleton (List.find (== Recipient.ToPlayer S.bob) (Set.toList recipients))) slots
  _ -> S.identityAnswer p

-- Run whole steps until the board reaches `stop`, or the game ends. Bounded so a
-- bug cannot loop forever; Pawl.TurnSpec's runTurn is the same shape one turn
-- wide.
runUntil :: (forall r. Prompt.Prompt r -> r) -> (GameState.GameState -> Bool) -> GameState.GameState -> GameState.GameState
runUntil answer stop gs0 =
  let go n g =
        if n <= (0 :: Int) || stop g || Maybe.isJust (GameState.result g)
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 64 gs0

-- The one exiled card on the board, soleExile's reading above.
soleExileOf :: GameState.GameState -> Maybe ObjectId.ObjectId
soleExileOf gs = case Set.toList (GameState.exile gs) of
  [oid] -> Just oid
  _ -> Nothing

-- The intervening "if" of the triggered ability on top of the stack, asked the
-- way Pawl.Engine.Stack's CR 608.2a arm asks it. Nothing when the top is not a
-- triggered ability or states no condition -- which the caller asserts against,
-- so a board that stopped holding one cannot pass by answering neither.
interveningOfTop :: GameState.GameState -> Maybe Bool
interveningOfTop gs = case GameState.stack gs of
  [] -> Nothing
  oid : _ -> do
    obj <- Game.lookupObject oid gs
    case Object.source obj of
      Source.OfTrigger borne -> do
        cond <- TriggeredAbility.intervening (TriggeredAbilitySource.ability borne)
        pure (Stack.interveningStillHolds gs obj (TriggeredAbilitySource.source borne) (TriggeredAbility.condition (TriggeredAbilitySource.ability borne)) cond)
      _ -> Nothing

-- Aims Pull from Eternity at the one exiled card, PICKED OUT OF THE OFFERED SET
-- rather than built, suspendAnswer's reason (CR 608.2b).
pullAt :: Maybe ObjectId.ObjectId -> Prompt.Prompt r -> r
pullAt exiled p = case p of
  Prompt.ChooseTargets _ _ _ slots ->
    Map.map
      (\(_, recipients) -> maybe Set.empty Set.singleton (List.find (\r -> Recipient.objectOf r == exiled) (Set.toList recipients)))
      slots
  _ -> S.identityAnswer p

suspending :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
suspending s registry = Spec.describe s "CR 116.2f Rift Bolt" $ do
  -- CR 116.2f's window is the CARD'S OWN CASTABILITY -- "only if they could
  -- begin to cast that card by putting it onto the stack" -- so a sorcery with
  -- suspend is offered at sorcery speed and nowhere else, and a card without the
  -- keyword is never offered at all.
  Spec.it s "the action is offered only for a card with suspend, and only where the card could be cast" $ do
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Rift Bolt"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let (boltId, travelerId, gs) = riftBoltBoard mountain bolt traveler
        actions = Action.legalActions S.alice gs
        opponentsTurn = gs {GameState.activePlayer = S.bob}
    Spec.assertBool s (List.elem (Action.Type.Suspend boltId) actions) "the Bolt may be suspended"
    Spec.assertBool s (List.notElem (Action.Type.Suspend travelerId) actions) "the Doomed Traveler may not"
    Spec.assertBool s (List.notElem (Action.Type.Suspend boltId) (Action.legalActions S.alice opponentsTurn)) "and not on an opponent's turn, where the sorcery could not be cast"
  -- CR 702.62a's "if it's exiled" is CR 603.4's intervening "if": it immediately
  -- follows the trigger condition, so CR 608.2a re-checks it as the ability
  -- resolves. The pair is one board where the suspended card is still in exile
  -- when the free-play ability resolves and one where bob's Pull from Eternity
  -- has put it in alice's graveyard first -- same suspend, same counter removal,
  -- same ability on the stack.
  --
  -- ASSERTED ON THE ABILITY'S FATE and not on the board, because the two
  -- readings agree on today's board by accident of CR 400.7: the id the trigger
  -- bound is deleted as the card leaves exile, so
  -- Pawl.Engine.Resolve.Effect.offerCast would find no object to offer and cast
  -- nothing even with no condition to fail. Pawl.Engine.Stack.interveningStillHolds
  -- is the resolution's own question, asked here directly.
  Spec.it s "CR 603.4 the free play is removed if the card has left exile by the time it resolves" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    bolt <- S.printingOf s registry "Rift Bolt"
    traveler <- S.printingOf s registry "Doomed Traveler"
    pull <- S.printingOf s registry "Pull from Eternity"
    let (boltId, _, gs) = riftBoltBoard mountain bolt traveler
        (pullId, withPull) = S.addHandCard pull S.bob (S.landsFor plains S.bob 1 gs)
        suspended = snd (Engine.runGamePure (suspendAnswer boltId) withPull Engine.priorityLoop)
        -- The countdown's own removal, driven through the funnel rather than
        -- through two turns of upkeeps: what this case is about is the window
        -- between the ability being placed and its resolution, and CR 702.62a's
        -- second ability has its own case above.
        placed = case soleExileOf suspended of
          Nothing -> suspended
          Just exiledId -> S.runPure S.identityAnswer suspended (Event.removeCounters exiledId CounterKind.Time 1 >> Engine.settleForPriority)
        -- bob's response, the one thing that differs: the exiled card goes to
        -- alice's graveyard while the ability waits on the stack.
        pulled = S.runPure (pullAt (soleExileOf placed)) placed (S.cast S.bob pullId >> Stack.resolveTop)
    Spec.assertBool s (Maybe.isJust (soleExileOf suspended)) "the Bolt was suspended, so the cases below are about a card in exile"
    Spec.assertEqWith s "the free-play ability is on the stack" (length (GameState.stack placed)) 1
    Spec.assertBool s (Maybe.isJust (soleExileOf placed)) "with the Bolt still exiled while it waits"
    Spec.assertEqWith s "and bob's Pull from Eternity takes it out of exile" (length (GameState.exile pulled)) 0
    -- ONE assertion carrying both readings, so an ability that stopped stating a
    -- condition at all cannot pass by answering neither: Nothing is a failure
    -- here where two separate `forM_`s would have skipped silently.
    Spec.assertEqWith
      s
      "CR 608.2a still exiled the ability resolves, and CR 603.4 pulled out of exile it is removed instead"
      (interveningOfTop placed, interveningOfTop pulled)
      (Just True, Just False)

-- Durkwood Baloth (TSP 193) {4}{G}{G} 5/5 Creature -- Beast, "Suspend 5--{G}"
-- (Oracle text checked on Scryfall, 2026-09-12). A vanilla creature but for the
-- suspend, so the only thing either case below can be about is rule 702.62a's
-- last sentence.
--
-- THE PAIR differs in the road the Baloth took and in nothing else: both boards
-- give alice the same six Forests, and both put the Baloth onto the battlefield
-- during her own turn. The positive starts it in exile with its LAST time
-- counter on -- the board four upkeeps of the countdown leave -- so her upkeep
-- removes it, rule 702.62a's third ability resolves, and the free cast resolves
-- before combat. The negative leaves it in hand and casts it for {4}{G}{G} in
-- the same precombat main phase.
--
-- STARTED ON BOB'S TURN so the countdown's upkeep is one the engine ENTERS: a
-- board handed in already standing in the upkeep step has had its
-- TriggerCondition.StepBegins moment go by.
--
-- THE LIBRARIES are stocked because alice draws on the way through (CR 104.3c).
balothBoard :: Printing.Printing -> Printing.Printing -> Bool -> GameState.GameState
balothBoard forest baloth exiled =
  let base = S.landsInPlay forest 6
      placed =
        if exiled
          then
            let (oid, g) = S.addExiledCard baloth S.alice base
             in -- CR 702.62b: in exile, with suspend, with a time counter on it
                -- -- the definition, and the state the countdown's last upkeep
                -- finds.
                g {GameState.objects = Map.adjust (\o -> o {Object.counters = Map.singleton CounterKind.Time 1}) oid (GameState.objects g)}
          else snd (S.addHandCard baloth S.alice base)
      stocked = List.foldl' (\g pid -> List.foldl' (\h _ -> snd (S.addLibraryCard forest pid h)) g [1 :: Int .. 4]) placed [S.alice, S.bob]
   in stocked
        { GameState.activePlayer = S.bob,
          GameState.phase = Phase.PostcombatMain,
          GameState.priority = Just S.bob
        }

-- Accepts rule 702.62a's offered cast, casts from hand when one is affordable,
-- and attacks bob with everything. ONE answerer for both boards, so the cases
-- differ only in the board: on the exiled board alice's hand is empty and the
-- casting arm never fires.
balothAnswer :: Prompt.Prompt r -> r
balothAnswer p = case p of
  Prompt.OfferedCast {} -> OptionalDecision.Exercises
  Prompt.ChooseAction {} -> S.castAnswer p
  _ -> S.attackTo S.bob p

suspendHaste :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
suspendHaste s registry = Spec.describe s "CR 702.62a Durkwood Baloth" $ do
  -- CR 702.62a's "until you lose control of the SPELL": Aethersnatch takes the
  -- free cast on the stack, and the haste goes to nobody.
  Spec.it s "CR 702.62a / 110.2b the Baloth bob Aethersnatched off the stack enters under him without haste" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    baloth <- S.printingOf s registry "Durkwood Baloth"
    snatch <- S.printingOf s registry "Aethersnatch"
    let played = runUntil snatchAnswer alicesPostcombat (snatchBoard island snatch [S.bob] (balothBoard forest baloth True))
        arrived = balothsIn baloth played
    Spec.assertEqWith
      s
      "CR 702.62a the caster lost control of the spell, so bob's Baloth has no haste"
      (fmap (\oid -> (Projection.controllerOf oid played, Projection.hasKeyword Keyword.Haste oid played)) arrived)
      [(Just S.bob, False)]
  -- The duration ENDS once and for good (CR 611.2b): alice taking the spell back
  -- before it resolves does not restart it. The one road to a stolen Baloth
  -- arriving on its caster's own turn, where haste is an attack.
  Spec.it s "CR 702.62a / 611.2b the Baloth alice snatched back from bob does not attack the turn it arrives" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    baloth <- S.printingOf s registry "Durkwood Baloth"
    snatch <- S.printingOf s registry "Aethersnatch"
    let played = runUntil snatchAnswer alicesPostcombat (snatchBoard island snatch [S.alice, S.bob] (balothBoard forest baloth True))
    Spec.assertEqWith s "CR 702.62a alice lost control of the spell to bob, so her Baloth could not attack" (S.lifeOf S.bob played) (Just 20)
    Spec.assertEqWith
      s
      "the control: the Baloth arrived under alice, who took the spell back"
      (fmap (`Projection.controllerOf` played) (balothsIn baloth played))
      [Just S.alice]

-- balothBoard's exiled Baloth, with an Aethersnatch and the six Islands that
-- pay for it in each listed player's hand and battlefield.
snatchBoard :: Printing.Printing -> Printing.Printing -> [PlayerId.PlayerId] -> GameState.GameState -> GameState.GameState
snatchBoard island snatch thieves gs0 =
  List.foldl' (\g pid -> snd (S.addHandCard snatch pid (S.landsFor island pid 6 g))) gs0 thieves

-- balothAnswer, except that a target prompt takes the EARLIEST object offered.
-- Aethersnatch is offered only while a spell is on the stack, so alice's (when
-- she holds one) is cast first with the Baloth its one target, and bob's answers
-- it; the Baloth spell was put on the stack before either Aethersnatch, so the
-- lowest id is it.
snatchAnswer :: Prompt.Prompt r -> r
snatchAnswer p = case p of
  Prompt.ChooseTargets _ _ _ slots ->
    Map.map (\(_, recipients) -> maybe Set.empty Set.singleton (List.find (Maybe.isJust . Recipient.objectOf) (List.sortOn Recipient.objectOf (Set.toList recipients)))) slots
  _ -> balothAnswer p

-- The postcombat main phase of alice's turn: the countdown's upkeep and her
-- combat have both run.
alicesPostcombat :: GameState.GameState -> Bool
alicesPostcombat g = GameState.phase g == Phase.PostcombatMain && GameState.activePlayer g == S.alice

-- Every Durkwood Baloth on the battlefield, whoever controls it: alice owns it,
-- and Game.zoneMembers indexes by owner.
balothsIn :: Printing.Printing -> GameState.GameState -> [ObjectId.ObjectId]
balothsIn baloth gs = filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (S.printingName baloth)) (Game.zoneMembers Zone.Battlefield S.alice gs)

-- Delay (TSP 57) {1}{U} Instant, "Counter target spell. If the spell is
-- countered this way, exile it with three time counters on it instead of
-- putting it into its owner's graveyard. If it doesn't have suspend, it gains
-- suspend." -- checked against Scryfall, 2026-09-30. A suspend an effect GIVES
-- the exiled card, which the exile trigger scan sees through the projection.
--
-- bob's Goblin Piker is on the stack and alice aims Delay at it. The Piker
-- prints no suspend and bob has no mana, so every counter that comes off and the
-- cast at the end are the grant's. Libraries are stocked for the six turns.
delayBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> GameState.GameState
delayBoard island delay victim traveler =
  let (pikerId, onStack) = S.spellOnStack victim S.bob (S.landsInPlay island 2)
      (gs, delayId) = S.handOne delay onStack
      stocked =
        (stockLibraries traveler gs)
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      -- PICKED OUT OF THE OFFERED SET, suspendAnswer's reason (CR 608.2b).
      atPiker :: Prompt.Prompt r -> r
      atPiker p = case p of
        Prompt.ChooseTargets _ _ _ slots ->
          Map.map (\(_, recipients) -> maybe Set.empty Set.singleton (List.find (\r -> Recipient.objectOf r == Just pikerId) (Set.toList recipients))) slots
        _ -> S.identityAnswer p
      cast = snd (Engine.runGamePure atPiker stocked (S.cast S.alice delayId))
   in snd (Engine.runGamePure atPiker cast Stack.resolveTop)

-- Eight cards in each library, so nobody decks while the countdown runs (CR
-- 104.3c).
stockLibraries :: Printing.Printing -> GameState.GameState -> GameState.GameState
stockLibraries card gs0 = List.foldl' (\g pid -> List.foldl' (\h _ -> snd (S.addLibraryCard card pid h)) g [1 :: Int .. 8]) gs0 [S.alice, S.bob]

-- Passes every priority and takes CR 608.2g's offered cast.
delayAnswer :: Prompt.Prompt r -> r
delayAnswer p = case p of
  Prompt.ChooseAction {} -> Action.Type.Pass
  Prompt.OfferedCast {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- The time counters on the one exiled card.
exiledTimeCounters :: GameState.GameState -> Maybe Natural.Natural
exiledTimeCounters gs = do
  oid <- soleExileOf gs
  obj <- Game.lookupObject oid gs
  pure (Map.findWithDefault 0 CounterKind.Time (Object.counters obj))

-- The end of this player's next draw step: their upkeep has run.
pastUpkeepOf :: Natural.Natural -> GameState.GameState -> Bool
pastUpkeepOf turn g = GameState.turnNumber g >= turn && GameState.phase g == Phase.Beginning BeginningStep.DrawStep

delaying :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
delaying s registry = Spec.describe s "CR 702.62a Delay" $ do
  -- CR 702.62a's second and third abilities, on a card that gained suspend in
  -- exile. The countdown runs at bob's upkeeps, turns 2, 4 and 6.
  Spec.it s "the countered spell is exiled with three time counters, and its granted suspend casts it" $ do
    island <- S.printingOf s registry "Island"
    delay <- S.printingOf s registry "Delay"
    piker <- S.printingOf s registry "Goblin Piker"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let delayed = delayBoard island delay piker traveler
        firstUpkeep = runUntil delayAnswer (pastUpkeepOf 2) delayed
        thirdUpkeep = runUntil delayAnswer (pastUpkeepOf 6) (runUntil delayAnswer (pastUpkeepOf 4) firstUpkeep)
    Spec.assertEqWith
      s
      "exiled with three time counters, and bob's first upkeep takes one off"
      (exiledTimeCounters delayed, exiledTimeCounters firstUpkeep)
      (Just 3, Just 2)
    Spec.assertEqWith
      s
      "the last counter off at bob's third upkeep, bob casts the Piker for free"
      (S.countOnBattlefieldByName (S.printingName piker) S.bob thirdUpkeep, GameState.turnNumber thirdUpkeep)
      (1, 6)
  -- CR 702.62a's "PLAY it": Delay counters bob's Sea Gate Restoration, whose
  -- back face is a land (CR 712.12). The last counter comes off at bob's own
  -- upkeep with his land play unused (CR 305.2a), and he plays Sea Gate, Reborn
  -- rather than casting the sorcery -- a cast would leave his battlefield empty.
  Spec.it s "Delay on Sea Gate Restoration: the land back face is played when the last counter comes off" $ do
    island <- S.printingOf s registry "Island"
    delay <- S.printingOf s registry "Delay"
    seaGate <- S.printingOf s registry "Sea Gate Restoration"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let playsLand :: Prompt.Prompt r -> r
        playsLand p = case p of
          Prompt.ChooseOfferedCastSpell _ _ offered -> Maybe.fromMaybe (NonEmpty.head offered) (List.find ((/= S.printingName seaGate) . snd) (NonEmpty.toList offered))
          _ -> delayAnswer p
        thirdUpkeep = runUntil playsLand (pastUpkeepOf 6) (runUntil playsLand (pastUpkeepOf 4) (runUntil playsLand (pastUpkeepOf 2) (delayBoard island delay seaGate traveler)))
    Spec.assertEqWith
      s
      "Sea Gate, Reborn is on bob's battlefield and nothing is left in exile"
      (fmap (\oid -> Game.cardOf oid thirdUpkeep == Just (Printing.card seaGate)) (Game.zoneMembers Zone.Battlefield S.bob thirdUpkeep), length (GameState.exile thirdUpkeep))
      ([True], 0)
  -- Suspend (MH2 68) {U} Instant, "Exile target creature and put two time
  -- counters on it. If it doesn't have suspend, it gains suspend." -- checked
  -- against Scryfall, 2026-09-30. The same grant reached from the BATTLEFIELD
  -- by a MoveToZone rather than a countering: bob's Piker leaves, and comes
  -- back cast at his second upkeep.
  Spec.it s "Suspend exiles a creature with two time counters, and its granted suspend casts it" $ do
    island <- S.printingOf s registry "Island"
    suspendCard <- S.printingOf s registry "Suspend"
    piker <- S.printingOf s registry "Goblin Piker"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let (pikerId, withPiker) = S.addPermanent piker S.bob (S.landsInPlay island 1)
        (gs, suspendId) = S.handOne suspendCard withPiker
        stocked =
          (stockLibraries traveler gs)
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        atPiker :: Prompt.Prompt r -> r
        atPiker p = case p of
          Prompt.ChooseTargets _ _ _ slots ->
            Map.map (\(_, recipients) -> maybe Set.empty Set.singleton (List.find (\r -> Recipient.objectOf r == Just pikerId) (Set.toList recipients))) slots
          _ -> S.identityAnswer p
        exiled = snd (Engine.runGamePure atPiker (snd (Engine.runGamePure atPiker stocked (S.cast S.alice suspendId))) Stack.resolveTop)
        secondUpkeep = runUntil delayAnswer (pastUpkeepOf 4) (runUntil delayAnswer (pastUpkeepOf 2) exiled)
    Spec.assertEqWith
      s
      "exiled off the battlefield with two time counters, and cast back at bob's second upkeep"
      (exiledTimeCounters exiled, S.countOnBattlefieldByName (S.printingName piker) S.bob exiled, S.countOnBattlefieldByName (S.printingName piker) S.bob secondUpkeep, GameState.turnNumber secondUpkeep)
      (Just 2, 0, 1, 4)
  -- The pair: the same Piker exiled with the same three time counters, but no
  -- suspend. CR 702.62b makes it no suspended card, so nothing counts down.
  Spec.it s "a card exiled with time counters and no suspend keeps them" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    traveler <- S.printingOf s registry "Doomed Traveler"
    let (pikerId, gs) = S.addExiledCard piker S.bob (Setup.emptyGame S.bothPlayers)
        undelayed =
          (stockLibraries traveler (S.addCounter CounterKind.Time 3 pikerId gs))
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
    Spec.assertEqWith s "bob's upkeep takes nothing off" (exiledTimeCounters (runUntil delayAnswer (pastUpkeepOf 2) undelayed)) (Just 3)

-- Benalish Commander (PLC 2) {3}{W} Creature -- Human Soldier */*, "Benalish
-- Commander's power and toughness are each equal to the number of Soldiers you
-- control. / Suspend X--{X}{W}{W}. X can't be 0. / Whenever a time counter is
-- removed from this card while it's exiled, create a 1/1 white Soldier creature
-- token." -- checked against Scryfall, 2026-10-01. CR 107.3d's announcement,
-- which rule 107.3i makes one number: the X the player names as the special
-- action is taken is both the mana it charges and the time counters the card is
-- exiled with. `whileExiled` below is its third ability.
--
-- SIX PLAINS, which is what makes the announcement cases below discriminating.
-- {X}{W}{W} at X=3 taps five of them and leaves one, so the count reads the
-- announced value back off the board rather than merely "some mana was spent";
-- X=4 is affordable too, so a board that could only ever pay one value is not
-- what is being read. The offer case takes fewer, being about a board that
-- cannot pay the floor at all.
benalishBoard :: Int -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
benalishBoard lands plains commander =
  let (commanderId, gs) = S.addHandCard commander S.alice (S.landsInPlay plains lands)
   in ( commanderId,
        gs
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      )

-- Takes the suspend action the moment it is offered, announces this X for it,
-- and passes on every other priority. suspendAnswer's shape with rule 107.3d's
-- answer pinned: the value is fixed by the caller rather than searched for, so a
-- mutation cannot be repaired by an answerer that finds a legal value again.
suspendForX :: Natural.Natural -> ObjectId.ObjectId -> Prompt.Prompt r -> r
suspendForX x oid p = case p of
  Prompt.ChooseAction _ _ actions ->
    if List.elem (Action.Type.Suspend oid) actions then Action.Type.Suspend oid else Action.Type.Pass
  Prompt.ChooseX {} -> x
  _ -> S.identityAnswer p

suspendingForX :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
suspendingForX s registry = Spec.describe s "CR 107.3d Benalish Commander" $ do
  -- CR 101.1 / 101.2: "X can't be 0" beats rule 107.3d's otherwise free choice, so the
  -- announcement is illegal and the special action does nothing. The pair is one
  -- board and one answer apart -- same hand, same six Plains, same action taken.
  Spec.it s "CR 101.1 X can't be 0, so an announcement of zero takes nothing" $ do
    plains <- S.printingOf s registry "Plains"
    commander <- S.printingOf s registry "Benalish Commander"
    let (commanderId, gs) = benalishBoard 6 plains commander
        taking x = S.runPure (suspendForX x commanderId) gs (Suspend.suspend S.manaPerformer S.alice commanderId)
        refused = taking 0
        allowed = taking 1
    -- The action IS offered on this board, so the refusal below is rule 101.1's
    -- and not a window the card was never in.
    Spec.assertBool s (List.elem (Action.Type.Suspend commanderId) (Action.legalActions S.alice gs)) "the control: the Commander may be suspended here"
    Spec.assertEqWith s "at X=0 the Commander is still in alice's hand" (List.elem commanderId (Game.zoneMembers Zone.Hand S.alice refused)) True
    Spec.assertEqWith s "nothing was exiled" (length (GameState.exile refused)) 0
    Spec.assertEqWith s "and no Plains paid for it" (S.tappedCount S.alice refused) 0
    -- The one thing that differs: at X=1 the same call suspends, with one time
    -- counter and {1}{W}{W} paid.
    Spec.assertEqWith
      s
      "CR 107.3d at X=1 the same action exiles it with one time counter"
      (soleExileOf allowed >>= \oid -> fmap (Map.lookup CounterKind.Time . Object.counters) (Game.lookupObject oid allowed))
      (Just (Just 1))
    Spec.assertEqWith s "and three Plains paid {1}{W}{W}" (S.tappedCount S.alice allowed) 3
  -- CR 116.2f offers an action only where the player could take it, and rule
  -- 101.1's floor is part of what that costs: {1}{W}{W} is the cheapest this card
  -- can be suspended for, so a board holding two Plains is not offered the action
  -- while one holding three is. The pair differs in a single land, and the
  -- Commander's own {3}{W} is unaffordable on both -- CR 116.2f asks whether the
  -- card could BEGIN to be cast, not whether it could be paid for.
  Spec.it s "CR 116.2f a board that cannot pay the least legal X is not offered the action" $ do
    plains <- S.printingOf s registry "Plains"
    commander <- S.printingOf s registry "Benalish Commander"
    let (tooPoorId, tooPoor) = benalishBoard 2 plains commander
        (enoughId, enough) = benalishBoard 3 plains commander
    Spec.assertBool s (List.notElem (Action.Type.Suspend tooPoorId) (Action.legalActions S.alice tooPoor)) "two Plains cannot pay {1}{W}{W}, so the action is not offered"
    Spec.assertBool s (List.elem (Action.Type.Suspend enoughId) (Action.legalActions S.alice enough)) "the control: three Plains can, and it is"

-- CR 113.6b: "an ability that states which zones it functions in functions only
-- from those zones". "While it's exiled" is such a statement, so a suspended
-- card's own counter-removal triggers fire from exile, where CR 702.62a's
-- special action put it, as suspend's upkeep ability takes each counter off.
--
-- Libraries are stocked so nobody decks while the countdown runs (CR 104.3c).
whileExiled :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
whileExiled s registry = Spec.describe s "CR 113.6b while it's exiled" $ do
  -- Benalish Commander at X=2: a Soldier for each of the two counters, the
  -- second beside suspend's own last-counter trigger.
  Spec.it s "a suspended Benalish Commander makes a Soldier each time a time counter comes off" $ do
    plains <- S.printingOf s registry "Plains"
    commander <- S.printingOf s registry "Benalish Commander"
    let (commanderId, gs) = benalishBoard 6 plains commander
        suspended = snd (Engine.runGamePure (suspendForX 2 commanderId) (stockLibraries plains gs) Engine.priorityLoop)
        soldiers = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Soldier Token")) S.alice
        firstUpkeep = pastUpkeepOfAlice suspended
        secondUpkeep = pastUpkeepOfAlice firstUpkeep
    Spec.assertEqWith
      s
      "one Soldier after the first counter comes off, two after the last"
      (soldiers suspended, soldiers firstUpkeep, soldiers secondUpkeep)
      (0, 1, 2)
    Spec.assertEqWith s "the first upkeep left one time counter on the exiled card" (exiledTimeCounters firstUpkeep) (Just 1)
  -- Riftmarked Knight's "when the last time counter is removed": nothing at
  -- the first two upkeeps, the Knight token at the third.
  Spec.it s "a suspended Riftmarked Knight makes its Knight only when the last time counter comes off" $ do
    plains <- S.printingOf s registry "Plains"
    knight <- S.printingOf s registry "Riftmarked Knight"
    let (knightId, gs) = benalishBoard 3 plains knight
        suspended = snd (Engine.runGamePure (suspendAnswer knightId) (stockLibraries plains gs) Engine.priorityLoop)
        tokens = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Knight Token")) S.alice
        secondUpkeep = pastUpkeepOfAlice (pastUpkeepOfAlice suspended)
        thirdUpkeep = pastUpkeepOfAlice secondUpkeep
    Spec.assertEqWith s "no Knight token until the last counter, one after it" (tokens secondUpkeep, tokens thirdUpkeep) (0, 1)
    Spec.assertEqWith s "one time counter left before the third upkeep" (exiledTimeCounters secondUpkeep) (Just 1)

-- The end of alice's next draw step, so her next upkeep has run. Counted by
-- steps rather than turn numbers: a board built standing in her precombat main
-- phase runs that turn's beginning phase after it.
pastUpkeepOfAlice :: GameState.GameState -> GameState.GameState
pastUpkeepOfAlice g =
  let drawing h = GameState.activePlayer h == S.alice && GameState.phase h == Phase.Beginning BeginningStep.DrawStep
   in runUntil delayAnswer drawing (snd (Engine.runGamePure delayAnswer g Engine.runStep))

-- Poison the Cup (KHM 103) {1}{B}{B} Instant, "Destroy target creature. If this
-- spell was foretold, scry 2. / Foretell {1}{B}" -- checked against Scryfall,
-- 2026-09-25.
--
-- FOUR SWAMPS AND FOUR ISLANDS, so neither the {2} action, the {1}{B} cast, the
-- printed {1}{B}{B} nor Twincast's {U}{U} is ever refused for want of mana,
-- whichever lands the {2} took. bob's Goblin Piker is the lone creature to aim at.
--
-- THREE CARDS in alice's library, so a scry 2 that bottoms both is observable on
-- the board: the card that started third is then on top.
cupBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
cupBoard swamp island cup piker =
  let lands = S.landsFor island S.alice 4 (S.landsInPlay swamp 4)
      (_, g1) = S.addPermanent piker S.bob lands
      (cupId, g2) = S.addHandCard cup S.alice g1
      (deepId, g3) = S.addLibraryCard piker S.alice g2
      (_, g4) = S.addLibraryCard piker S.alice g3
      (topId, g5) = S.addLibraryCard piker S.alice g4
   in ( cupId,
        deepId,
        topId,
        g5
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      )

-- Bottoms every card a scry looks at, so a scry is visible on the board; casts
-- and otherwise declines, S.castAnswer's answers.
scriesToBottom :: Prompt.Prompt r -> r
scriesToBottom prompt = case prompt of
  Prompt.ChooseScry _ _ looked -> (looked, [])
  _ -> S.castAnswer prompt

foretoldSpell :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
foretoldSpell s registry = Spec.describe s "CR 702.143c Poison the Cup" $ do
  -- The pair: the same card, the same board, the same answers, cast once from
  -- the exile its foretelling put it in and once from the hand. Only the first is
  -- "a spell that was a foretold card before it was cast".
  Spec.it s "CR 702.143c a spell cast from a foretold card was foretold, and one cast from hand was not" $ do
    swamp <- S.printingOf s registry "Swamp"
    island <- S.printingOf s registry "Island"
    cup <- S.printingOf s registry "Poison the Cup"
    piker <- S.printingOf s registry "Goblin Piker"
    let (cupId, deepId, topId, gs) = cupBoard swamp island cup piker
        foretold = snd (State.evalState (Engine.runGame (takeThenPass (Action.Type.Foretell cupId)) gs Engine.priorityLoop) [])
        later = foretold {GameState.turnNumber = GameState.turnNumber foretold + 1}
        fromHand = S.runPure scriesToBottom gs (S.cast S.alice cupId >> Stack.resolveTop)
    Spec.assertEqWith s "setup: bob has the Piker to aim at" (S.creaturesInPlay S.bob gs) 1
    Spec.assertBool s (Maybe.isJust (soleExile foretold)) "the card was foretold into exile, so the case below runs at all"
    Monad.forM_ (soleExile foretold) $ \exiledId -> do
      let fromExile = S.runPure scriesToBottom later (S.cast S.alice exiledId >> Stack.resolveTop)
      Spec.assertEqWith s "CR 702.143c cast from the foretold card, it scried: the card that was third is on top" (take 1 (Game.zoneMembers Zone.Library S.alice fromExile)) [deepId]
      Spec.assertEqWith s "and it destroyed the Piker" (S.creaturesInPlay S.bob fromExile) 0
    Spec.assertEqWith s "the control: cast from hand, it did not scry" (take 1 (Game.zoneMembers Zone.Library S.alice fromHand)) [topId]
    Spec.assertEqWith s "though it destroyed the Piker all the same" (S.creaturesInPlay S.bob fromHand) 0
  -- CR 707.10: "a copy of a spell isn't cast", and it has no card. The copy is
  -- therefore not "a spell that was a foretold card before it was cast", however
  -- the spell it copies was.
  --
  -- The copy keeps the original's target and resolves first, so it destroys the
  -- Piker and the original is then left with no legal target and does not resolve
  -- at all (CR 608.2b) -- so the only spell that COULD scry here is the copy.
  Spec.it s "CR 707.10 a copy of a foretold spell was not foretold" $ do
    swamp <- S.printingOf s registry "Swamp"
    island <- S.printingOf s registry "Island"
    cup <- S.printingOf s registry "Poison the Cup"
    piker <- S.printingOf s registry "Goblin Piker"
    twincast <- S.printingOf s registry "Twincast"
    let (cupId, _, topId, gs0) = cupBoard swamp island cup piker
        (twincastId, gs) = S.addHandCard twincast S.alice gs0
        foretold = snd (State.evalState (Engine.runGame (takeThenPass (Action.Type.Foretell cupId)) gs Engine.priorityLoop) [])
        later = foretold {GameState.turnNumber = GameState.turnNumber foretold + 1}
    Spec.assertEqWith s "setup: bob has the Piker to aim at" (S.creaturesInPlay S.bob gs) 1
    Spec.assertBool s (Maybe.isJust (soleExile foretold)) "the card was foretold into exile, so the case below runs at all"
    Monad.forM_ (soleExile foretold) $ \exiledId -> do
      let copied = S.runPure scriesToBottom later (S.cast S.alice exiledId >> S.cast S.alice twincastId >> Stack.resolveTop >> Stack.resolveTop >> Stack.resolveTop)
      Spec.assertEqWith s "CR 707.10 the copy did not scry: the card that was on top still is" (take 1 (Game.zoneMembers Zone.Library S.alice copied)) [topId]
      Spec.assertEqWith s "the copy destroyed the Piker, so it resolved" (S.creaturesInPlay S.bob copied) 0
      Spec.assertEqWith s "and the stack is empty" (GameState.stack copied) []

foretellTrigger :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
foretellTrigger s registry = Spec.describe s "CR 702.143c Dream Devourer" $ do
  -- CR 702.143c: "foretelling a card" is the special action, and CR 702.143d's
  -- effect makes a card foretold without anyone foretelling it -- Ethereal
  -- Valkyrie's own ruling says the trigger does not fire. The Valkyrie's board
  -- with alice's Devourer added and nothing else changed.
  Spec.it s "CR 702.143d a card an effect makes foretold was not foretold by anyone" $ do
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    valkyrie <- S.printingOf s registry "Ethereal Valkyrie"
    lore <- S.printingOf s registry "Lore Weaver"
    traveler <- S.printingOf s registry "Doomed Traveler"
    devourer <- S.printingOf s registry "Dream Devourer"
    let (valkyrieId, loreId, gs0) = valkyrieBoard plains island valkyrie lore traveler
        (mineId, gs) = S.addPermanent devourer S.alice gs0
        after = S.runPure (valkyrieAnswers loreId) gs (S.cast S.alice valkyrieId >> Engine.priorityLoop)
    Spec.assertEqWith s "CR 702.143d the Devourer did not grow" (Projection.powerOf mineId after) (Just 0)
    Spec.assertEqWith
      s
      "though the Weaver did become foretold"
      (soleExile after >>= \oid -> fmap (Maybe.isJust . Object.foretold) (Game.lookupObject oid after))
      (Just True)

-- Dream Devourer's first ability over Hill Giant, {3}{R} Creature -- Giant
-- 3/3, whose granted foretell cost is {1}{R}. FOUR MOUNTAINS: {2} for the
-- action leaves exactly the {1}{R}, a mana short of the printed {3}{R}.
grantBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  PlayerId.PlayerId ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
grantBoard mountain giant devourer controller =
  let (giantId, g1) = S.addHandCard giant S.alice (S.landsInPlay mountain 4)
      (devourerId, g2) = S.addPermanent devourer controller g1
   in ( giantId,
        devourerId,
        g2
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      )

foretellGrant :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
foretellGrant s registry = Spec.describe s "CR 702.143a Dream Devourer's grant" $ do
  -- CR 613.1f: a layer-6 grant to a card in a hand, where CR 702.143a's ability
  -- functions. The pair differs only in who controls the Devourer: "your hand"
  -- is its controller's.
  Spec.it s "CR 613.1f a card in its controller's hand may be foretold, and one in an opponent's may not" $ do
    mountain <- S.printingOf s registry "Mountain"
    giant <- S.printingOf s registry "Hill Giant"
    devourer <- S.printingOf s registry "Dream Devourer"
    let (giantId, _, mine) = grantBoard mountain giant devourer S.alice
        (theirGiantId, _, theirs) = grantBoard mountain giant devourer S.bob
    Spec.assertBool s (List.elem (Action.Type.Foretell giantId) (Action.legalActions S.alice mine)) "CR 613.1f alice's Devourer lets her foretell the Giant"
    Spec.assertBool s (List.notElem (Action.Type.Foretell theirGiantId) (Action.legalActions S.alice theirs)) "bob's does not"

-- Patriar's Humiliation (HBG, Arena) {W} Instant, "Target creature perpetually
-- loses all abilities, then Patriar's Humiliation deals damage to it equal to
-- the number of creatures you control", then Unsummon bouncing alice's `victim`
-- to her hand, where its suspend or plot functions (CR 702.62a, 702.170a). The
-- perpetual effect follows the card there (Duration.Perpetual).
--
-- The pair differs in what the Humiliation targets and in nothing else: the
-- victim, or bob's Goblin Piker. Both boards spend the same Plains and Island,
-- so the lands `base` holds are untapped on each.
--
-- Answers the bounced card, when it is the only card in alice's hand, and the
-- board after the bounce.
humiliatedBoard ::
  GameState.GameState ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Bool ->
  (Maybe ObjectId.ObjectId, GameState.GameState)
humiliatedBoard base victim plains island piker humiliation unsummon onVictim =
  let lands = S.landsFor island S.alice 1 (S.landsFor plains S.alice 1 base)
      (victimId, g1) = S.addPermanent victim S.alice lands
      (pikerId, g2) = S.addPermanent piker S.bob g1
      (humiliationId, g3) = S.addHandCard humiliation S.alice g2
      (unsummonId, g4) = S.addHandCard unsummon S.alice g3
      atMain =
        g4
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      castAt spell target g = S.runPure (aimAtObject target) g (S.cast S.alice spell >> Stack.resolveTop)
      humiliated = castAt humiliationId (if onVictim then victimId else pikerId) atMain
      bounced = castAt unsummonId victimId humiliated
      inHand = case Game.zoneMembers Zone.Hand S.alice bounced of
        [oid] -> Just oid
        _ -> Nothing
   in (inHand, bounced)

-- Aims a spell at `oid`, PICKED OUT OF THE OFFERED SET rather than built,
-- suspendAnswer's reason (CR 608.2b).
aimAtObject :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtObject oid p = case p of
  Prompt.ChooseTargets _ _ _ slots ->
    Map.map (\(_, recipients) -> maybe Set.empty Set.singleton (List.find (\r -> Recipient.objectOf r == Just oid) (Set.toList recipients))) slots
  _ -> S.identityAnswer p

perpetualLoss :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
perpetualLoss s registry = Spec.describe s "CR 613.1f Patriar's Humiliation" $ do
  -- One Forest for the Baloth's {G}.
  Spec.it s "CR 613.1f a Durkwood Baloth that perpetually lost all abilities cannot be suspended" $ do
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    forest <- S.printingOf s registry "Forest"
    baloth <- S.printingOf s registry "Durkwood Baloth"
    piker <- S.printingOf s registry "Goblin Piker"
    humiliation <- S.printingOf s registry "Patriar's Humiliation"
    unsummon <- S.printingOf s registry "Unsummon"
    let build = humiliatedBoard (S.landsInPlay forest 1) baloth plains island piker humiliation unsummon
        (lostId, lost) = build True
        (keptId, kept) = build False
        offered mId gs = maybe False (\oid -> List.elem (Action.Type.Suspend oid) (Action.legalActions S.alice gs)) mId
    Spec.assertBool s (not (offered lostId lost)) "CR 613.1f the humiliated Baloth back in hand offers no suspend"
    Spec.assertBool s (offered keptId kept) "the control: with the Piker humiliated instead, the bounced Baloth may be suspended"
    Spec.assertBool s (Maybe.isJust lostId) "the Baloth is the one card in alice's hand"
  -- Four Islands for the Djinn's {3}{U}.
  Spec.it s "CR 613.1f a Djinn of Fool's Fall that perpetually lost all abilities cannot be plotted" $ do
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    djinn <- S.printingOf s registry "Djinn of Fool's Fall"
    piker <- S.printingOf s registry "Goblin Piker"
    humiliation <- S.printingOf s registry "Patriar's Humiliation"
    unsummon <- S.printingOf s registry "Unsummon"
    let build = humiliatedBoard (S.landsInPlay island 4) djinn plains island piker humiliation unsummon
        (lostId, lost) = build True
        (keptId, kept) = build False
        offered mId gs = maybe False (\oid -> List.elem (Action.Type.Plot oid djinnPlotCost) (Action.legalActions S.alice gs)) mId
    Spec.assertBool s (not (offered lostId lost)) "CR 613.1f the humiliated Djinn back in hand offers no plot"
    Spec.assertBool s (offered keptId kept) "the control: with the Piker humiliated instead, the bounced Djinn may be plotted"
    Spec.assertBool s (Maybe.isJust lostId) "the Djinn is the one card in alice's hand"
  -- CR 116.2e's discard, through Projection.specialActionsOf. No lands beyond
  -- the two spells': the action costs nothing.
  Spec.it s "CR 613.1f a Circling Vultures that perpetually lost all abilities cannot be discarded" $ do
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    vultures <- S.printingOf s registry "Circling Vultures"
    piker <- S.printingOf s registry "Goblin Piker"
    humiliation <- S.printingOf s registry "Patriar's Humiliation"
    unsummon <- S.printingOf s registry "Unsummon"
    let build = humiliatedBoard (Setup.emptyGame S.bothPlayers) vultures plains island piker humiliation unsummon
        (lostId, lost) = build True
        (keptId, kept) = build False
        offered mId gs = maybe False (\oid -> List.elem (Action.Type.DiscardFromHand oid) (Action.legalActions S.alice gs)) mId
    Spec.assertBool s (not (offered lostId lost)) "CR 613.1f the humiliated Vultures back in hand offer no discard"
    Spec.assertBool s (offered keptId kept) "the control: with the Piker humiliated instead, the bounced Vultures may be discarded"
    Spec.assertBool s (Maybe.isJust lostId) "the Vultures are the one card in alice's hand"

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = do
  circlingVultures s registry
  dampingEngine s registry
  dividedEdicts s registry
  leoninArbiter s registry
  volrathsCurse s registry
  plotting s registry
  plottingFromLibrary s registry
  foretelling s registry
  makeForetold s registry
  makeForetoldPerFace s registry
  foretoldSpell s registry
  foretellTrigger s registry
  foretellGrant s registry
  suspending s registry
  suspendHaste s registry
  suspendingForX s registry
  whileExiled s registry
  delaying s registry
  perpetualLoss s registry

-- CR 116.2d again, on the two axes Leonin Arbiter cannot reach: WHO the action is
-- offered to (its own scope is EachPlayer, so every seat is offered it) and how
-- far ONE NAME reaches. Damping Engine (ULG 124) narrows the first, and its one
-- printed sentence declares two player abilities that carry one name -- so one
-- payment covers both, which is what makes "this effect" the sentence rather than
-- the row. The Warden group below is the other side of that: two names on one
-- permanent, where a payment covers one.
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
    Spec.assertBool s (List.elem (Action.Type.Ignore engineId theLead) (asked S.alice aliceLeads)) "alice controls the most permanents, so she may pay to ignore it"
    Spec.assertBool s (List.notElem (Action.Type.Ignore engineId theLead) (asked S.bob aliceLeads)) "bob is not affected, so there is nothing for him to ignore"
    Spec.assertBool s (List.notElem (Action.Type.Ignore engineId theLead) (asked S.carol aliceLeads)) "nor carol"
    Spec.assertBool s (List.elem (Action.Type.Ignore engineId theLead) (asked S.bob bobLeads)) "and bob IS offered it once the lead is his -- so his cost was payable all along"
    Spec.assertBool s (List.notElem (Action.Type.Ignore engineId theLead) (asked S.alice bobLeads)) "while alice, no longer affected, is offered nothing"
  -- "More permanents than each other player" is a STRICT comparison, so a tie for
  -- the lead leaves the ability affecting nobody -- which is the third value this
  -- scope can take and the one a "whoever has the most" reading would miss.
  Spec.it s "the comparison is strict, so a tie for the lead affects nobody" $ do
    engine <- S.printingOf s registry "Damping Engine"
    forest <- S.printingOf s registry "Forest"
    changeling <- S.printingOf s registry "Woodland Changeling"
    growth <- S.printingOf s registry "Rampant Growth"
    let (engineId, _, forestId, _, _, aliceLeads) = dampingBoard engine forest changeling growth
        tied = snd (S.addPermanent forest S.bob (snd (S.addPermanent forest S.bob aliceLeads)))
        asked pid gs = Action.legalActions pid (gs {GameState.priority = Just pid})
    Spec.assertBool s (List.notElem (Action.Type.Ignore engineId theLead) (asked S.alice tied)) "alice has no lead to be affected by"
    Spec.assertBool s (List.notElem (Action.Type.Ignore engineId theLead) (asked S.bob tied)) "and neither does bob"
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
  -- CR 116.2d's "the effect from that ability" is the printed SENTENCE, and
  -- Damping Engine's declares two rows: both carry the one name its ignore refers
  -- to, so one payment lifts both. An implementation that let one name reach only
  -- the first row it matched would lift one.
  Spec.it s "CR 116.2d one payment lifts every row that ability's name declares" $ do
    engine <- S.printingOf s registry "Damping Engine"
    forest <- S.printingOf s registry "Forest"
    changeling <- S.printingOf s registry "Woodland Changeling"
    growth <- S.printingOf s registry "Rampant Growth"
    let (engineId, victimId, forestId, changelingId, _, aliceLeads) = dampingBoard engine forest changeling growth
        afterIgnore = S.runPure (sacrificing victimId) aliceLeads (Ignore.ignore S.alice engineId theLead)
        actions = Action.legalActions S.alice afterIgnore
    Spec.assertEqWith s "the sacrifice was paid: one of alice's three Forests is gone" (S.countOnBattlefieldByName (S.printingName forest) S.alice afterIgnore) 2
    Spec.assertBool s (any (playing forestId) actions) "CR 305.1's half is lifted"
    Spec.assertBool s (any (casting changelingId) actions) "and CR 601.3a's half with it, off the same one payment"
    -- CR 116.2d forbids no repeat, and Pawl.Engine.PlayerEffect.affectedBy is
    -- asked over the unfiltered gather so that the offer survives being taken.
    Spec.assertBool s (List.elem (Action.Type.Ignore engineId theLead) actions) "and the action is still offered, since paying again is legal"

-- CR 116.2d's GRAIN: "the effect from that ability", singular. No printing can
-- observe it -- Scryfall o:"ignore this effect" returns four producers (checked
-- 2026-09-05) and each states its restriction in one sentence, so "this effect"
-- covers the whole of what the permanent does and an ignore keyed to the
-- permanent agrees with one keyed to the ability. Synthetic Warden of Divided
-- Edicts is the permanent that separates them, and both halves of the rule are
-- asserted: which players are OFFERED the deal, and what taking it SUPPRESSES.
dividedEdicts :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
dividedEdicts s registry = Spec.describe s "CR 116.2d Synthetic Warden of Divided Edicts" $ do
  -- The OFFER half. alice is reached by the Warden -- its land ban stops her own
  -- land play on her own turn -- and is still offered nothing, because the
  -- ability the deal NAMES is her opponents' only. An offer derived from the
  -- permanent rather than from the named ability offers it to her.
  Spec.it s "CR 116.2d the deal is offered to the players the NAMED ability reaches, and to no other" $ do
    warden <- S.printingOf s registry "Synthetic Warden of Divided Edicts"
    forest <- S.printingOf s registry "Forest"
    changeling <- S.printingOf s registry "Woodland Changeling"
    growth <- S.printingOf s registry "Rampant Growth"
    let (wardenId, _, aliceForestId, _, _, _, gs) = wardenBoard warden forest changeling growth
        aliceTurn = gs {GameState.activePlayer = S.alice}
        asked pid board_ = Action.legalActions pid (board_ {GameState.priority = Just pid})
    Spec.assertBool s (List.elem (Action.Type.Ignore wardenId creatureBan) (asked S.bob gs)) "bob's creature spells are banned, so the deal is offered to him"
    Spec.assertBool s (List.notElem (Action.Type.Ignore wardenId creatureBan) (asked S.alice aliceTurn)) "and not to alice, whom the ability the deal names does not reach"
    Spec.assertBool s (not (any (playing aliceForestId) (asked S.alice aliceTurn))) "though the Warden's other, unnamed ability IS reaching her: her own land play is stopped"
    Spec.assertBool s (List.elem (Action.Type.Ignore wardenId creatureBan) (asked S.bob aliceTurn)) "and bob keeps the offer on alice's turn, so her absence is not about the window"
  -- The SUPPRESSION half, and the assertion this unit exists to prove is the last
  -- one: a payment lifts the ability it named and leaves the permanent's other
  -- one in force. The pair for that last one is the same board built around a
  -- vanilla creature, which differs in exactly the Warden.
  Spec.it s "CR 116.2d paying lifts the ability the payment named and nothing else" $ do
    warden <- S.printingOf s registry "Synthetic Warden of Divided Edicts"
    forest <- S.printingOf s registry "Forest"
    changeling <- S.printingOf s registry "Woodland Changeling"
    growth <- S.printingOf s registry "Rampant Growth"
    let (wardenId, victimId, _, bobForestId, changelingId, growthId, gs) = wardenBoard warden forest changeling growth
        (_, _, _, controlForestId, _, _, vanilla) = wardenBoard changeling forest changeling growth
        before = Action.legalActions S.bob gs
        afterIgnore = S.runPure (sacrificing victimId) gs (Ignore.ignore S.bob wardenId creatureBan)
        actions = Action.legalActions S.bob afterIgnore
    Spec.assertBool s (not (any (casting changelingId) before)) "before paying, bob's creature spell is stopped"
    Spec.assertBool s (any (casting growthId) before) "while the sorcery of the same cost is not -- the cost control"
    Spec.assertBool s (any (playing controlForestId) (Action.legalActions S.bob vanilla)) "and the pair: with a vanilla creature in the Warden's place, that same land play IS offered"
    Spec.assertEqWith s "the sacrifice was paid: one of bob's four Forests is gone" (S.countOnBattlefieldByName (S.printingName forest) S.bob afterIgnore) 3
    Spec.assertBool s (any (casting changelingId) actions) "the ability the payment named is lifted"
    Spec.assertBool s (not (any (playing bobForestId) actions)) "and the Warden's OTHER ability still bites, so the payment lifted one ability rather than the permanent"

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
        (poorId, poor) = S.addPermanent arbiter S.alice (S.landsInPlay forest 1)
        broke =
          (snd (S.addHandCard forest S.alice poor))
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
    Spec.assertBool s (List.elem (Action.Type.Ignore arbiterId searchBan) (Action.legalActions S.alice gs)) "the Arbiter may be ignored"
    Spec.assertBool s (List.elem (Action.Type.Ignore arbiterId searchBan) (Action.legalActions S.alice instantSpeed)) "on another player's turn with a spell on the stack too"
    Spec.assertBool s (List.notElem (Action.Type.Ignore poorId searchBan) (Action.legalActions S.alice broke)) "but not with one Forest, which cannot pay {2}"
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
        (_, withBobsLands) = S.addPermanent forest S.bob (snd (S.addPermanent forest S.bob gs))
        asked pid board_ = Action.legalActions pid (board_ {GameState.priority = Just pid})
    Spec.assertBool s (List.elem (Action.Type.Ignore arbiterId searchBan) (asked S.alice withBobsLands)) "the Arbiter's own controller may pay"
    Spec.assertBool s (List.elem (Action.Type.Ignore arbiterId searchBan) (asked S.bob withBobsLands)) "and so may bob, whose two Forests pay the {2}"
  -- The same board and the same spell, with the special action taken first
  -- through the priority loop -- so this is Pawl.Engine.Engine's arm as well as
  -- the suppression, and the case above is its paired control.
  Spec.it s "CR 116.2d paying the cost lets that player, and only that player, search" $ do
    forest <- S.printingOf s registry "Forest"
    arbiter <- S.printingOf s registry "Leonin Arbiter"
    growth <- S.printingOf s registry "Rampant Growth"
    let (arbiterId, growthId, gs) = arbiterBoard forest arbiter growth
        afterIgnore = snd (State.evalState (Engine.runGame (takeOnce (Action.Type.Ignore arbiterId searchBan)) gs Engine.priorityLoop) False)
        (after, asked) = growAndResolve growthId afterIgnore
    Spec.assertEqWith s "the Forest left the library" (S.countByName (S.printingName forest) S.alice after) 0
    Spec.assertEqWith s "and is the tenth on the battlefield" (S.countOnBattlefieldByName (S.printingName forest) S.alice after) 10
    Spec.assertEqWith s "the search was offered this time" asked ["search", "shuffle"]
    Spec.assertBool s (not (PlayerEffect.prohibitsSearching S.alice S.alice S.alice afterIgnore)) "alice paid, so she is not prohibited"
    Spec.assertBool s (PlayerEffect.prohibitsSearching S.bob S.bob S.bob afterIgnore) "bob did not, so he still is"
  -- CR 514.2: "until end of turn" ends at cleanup, which is the one caller of
  -- Expiry.dropAtCleanup. Asserted by casting the SAME spell on the swept state
  -- and watching it stop searching again.
  Spec.it s "CR 514.2 the ignore ends at cleanup" $ do
    forest <- S.printingOf s registry "Forest"
    arbiter <- S.printingOf s registry "Leonin Arbiter"
    growth <- S.printingOf s registry "Rampant Growth"
    let (arbiterId, growthId, gs) = arbiterBoard forest arbiter growth
        afterIgnore = snd (State.evalState (Engine.runGame (takeOnce (Action.Type.Ignore arbiterId searchBan)) gs Engine.priorityLoop) False)
        (after, asked) = growAndResolve growthId (Expiry.dropAtCleanup afterIgnore)
    Spec.assertEqWith s "the Forest is still in the library" (S.countByName (S.printingName forest) S.alice after) 1
    Spec.assertEqWith s "and the search was not offered again" asked ["shuffle"]

circlingVultures :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
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

-- Is this the offer to activate an ability of THAT permanent? `playing`'s shape
-- one action over, and written out for its reason: the operation alone would
-- answer Yes for every other permanent on the board.
activating :: ObjectId.ObjectId -> Action.Type.Action -> Bool
activating wanted action = case action of
  Action.Type.Activate oid _ -> oid == wanted
  Action.Type.Play _ _ -> False
  Action.Type.Pass -> False
  Action.Type.Cast {} -> False
  Action.Type.TurnFaceUp {} -> False
  Action.Type.Unlock _ _ -> False
  Action.Type.DiscardFromHand _ -> False
  Action.Type.Plot {} -> False
  Action.Type.Foretell _ -> False
  Action.Type.Suspend _ -> False
  Action.Type.PutCompanionIntoHand -> False
  Action.Type.RollPlanarDie -> False
  Action.Type.Ignore _ _ -> False
  Action.Type.EndEffect _ -> False
  Action.Type.ActivateManaAbility _ -> False

-- Volrath's Curse under BOB's control, on a Teardrop Kami ALICE controls, on a
-- THREE-seat board: at two seats "the enchanted creature's controller" and "the
-- Aura's controller's opponent" are one player and the readings cannot be told
-- apart.
--
-- Every seat controls a permanent it could sacrifice, so a seat left out of the
-- offer is left out by CR 116.2d's WHO rather than by an unpayable cost -- bob
-- controls a Piker beside the Aura, and carol one of her own.
--
-- bob's Piker rides out so a case can move the Curse onto it, which is the pair
-- for the whole group: one board, one thing different, the offer at the other
-- seat.
curseSeats ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
curseSeats curse kami piker =
  let (kamiId, gs1) = S.addPermanent kami S.alice S.threePlayerGame
      (_, gs2) = S.addPermanent piker S.alice gs1
      (curseId, gs3) = S.addPermanent curse S.bob gs2
      (bobPikerId, gs4) = S.addPermanent piker S.bob gs3
      (_, gs5) = S.addPermanent piker S.carol gs4
   in ( curseId,
        kamiId,
        bobPikerId,
        (S.attach curseId kamiId gs5)
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      )

-- The same Aura and the same seats on a COMBAT board, which is what makes CR
-- 508.1c's and CR 509.1b's halves of the sentence askable: alice is active with
-- the enchanted Kami and one other creature, bob defends with one.
--
-- alice's OTHER creature is the sacrifice victim, and `sacrificing` pins it
-- there. Paying by sacrificing the Kami itself would end the restriction through
-- CR 604.2 -- the creature it names having left the battlefield -- rather than
-- through CR 116.2d, and every assertion would pass for the wrong reason.
curseCombat ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
curseCombat curse kami piker =
  let (gs, _, _) = S.combatBoardOf [] [piker]
      (kamiId, gs1) = S.addPermanent kami S.alice gs
      (victimId, gs2) = S.addPermanent piker S.alice gs1
      (curseId, gs3) = S.addPermanent curse S.bob gs2
   in (curseId, kamiId, victimId, S.attach curseId kamiId gs3)

-- CR 116.2d on the OBJECT axis: an ability aimed at a permanent rather than at a
-- player. Volrath's Curse (TMP 101) is the producer -- "enchanted creature can't
-- attack or block, and its activated abilities can't be activated. That
-- creature's controller may sacrifice a permanent of their choice for that
-- player to ignore this effect until end of turn" -- and its one sentence
-- declares three rows across two carriers, all three named "this effect".
--
-- What the axis changes is WHO. The ability names no player, so CR 116.2d's
-- offer follows the permanent the ability restricts, and the Aura's controller
-- is offered nothing -- the reading a player-axis derivation cannot state, since
-- there is no PlayerScope to read a seat off.
volrathsCurse :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
volrathsCurse s registry = Spec.describe s "CR 116.2d Volrath's Curse" $ do
  -- The OFFER half. The last two assertions are the pair: the same board with
  -- the Curse moved onto a creature bob controls, which differs in exactly the
  -- thing the rule reads.
  Spec.it s "CR 116.2d the offer goes to the enchanted creature's controller, not to the Aura's" $ do
    curse <- S.printingOf s registry "Volrath's Curse"
    kami <- S.printingOf s registry "Teardrop Kami"
    piker <- S.printingOf s registry "Goblin Piker"
    let (curseId, kamiId, bobPikerId, gs) = curseSeats curse kami piker
        moved = S.attach curseId bobPikerId gs
        asked pid board_ = Action.legalActions pid (board_ {GameState.priority = Just pid})
    Spec.assertBool s (List.elem (Action.Type.Ignore curseId thisEffect) (asked S.alice gs)) "alice controls the enchanted creature, so the deal is hers to take"
    Spec.assertBool s (List.notElem (Action.Type.Ignore curseId thisEffect) (asked S.bob gs)) "and not the Aura's controller's, though the Piker beside it would pay his sacrifice"
    Spec.assertBool s (List.notElem (Action.Type.Ignore curseId thisEffect) (asked S.carol gs)) "nor carol's, an opponent of his whom the ability restricts nothing of"
    Spec.assertBool s (List.elem (Action.Type.Ignore curseId thisEffect) (asked S.bob moved)) "the pair: with the Curse on bob's own creature the offer is his"
    Spec.assertBool s (List.notElem (Action.Type.Ignore curseId thisEffect) (asked S.alice moved)) "and no longer alice's, so the seat follows the enchanted creature and not the Aura"
    Spec.assertBool s (List.notElem (Action.Type.Ignore kamiId thisEffect) (asked S.alice gs)) "and it is the AURA that grants it: the enchanted creature offers nothing"
  -- The SUPPRESSION half, and the three assertions this unit exists to prove
  -- come first: after the payment the creature can attack, can block, and its
  -- activated ability can be activated. All three are read at gameplay level --
  -- Pawl.Engine.Combat's declaration questions and the action list a player with
  -- priority is offered -- and all three come off ONE payment, which is what
  -- makes "this effect" the sentence rather than one of its rows.
  --
  -- The Kami's ability costs no mana -- its whole cost is sacrificing itself --
  -- and it is not a mana ability, so neither an empty mana pool nor
  -- Pawl.Engine.Activatable.activatable's blanket False for a mana ability can be
  -- why an activation is missing.
  Spec.it s "CR 116.2d after the enchanted creature's controller pays, it can attack and block and its ability can be activated" $ do
    curse <- S.printingOf s registry "Volrath's Curse"
    kami <- S.printingOf s registry "Teardrop Kami"
    piker <- S.printingOf s registry "Goblin Piker"
    let (curseId, kamiId, victimId, cursed) = curseCombat curse kami piker
        afterIgnore = S.runPure (sacrificing victimId) cursed (Ignore.ignore S.alice curseId thisEffect)
        swept = Expiry.dropAtCleanup afterIgnore
        asked pid board_ = Action.legalActions pid (board_ {GameState.priority = Just pid})
    Spec.assertBool s (Combat.canAttack S.alice kamiId afterIgnore) "CR 508.1c: alice paid, so the enchanted creature may attack"
    Spec.assertBool s (Combat.canBlock S.alice kamiId afterIgnore) "CR 509.1b: and block, off that same one payment"
    Spec.assertBool s (any (activating kamiId) (asked S.alice afterIgnore)) "CR 602.2: and its activated ability may be activated, the third row that one name declares"
    Spec.assertBool s (not (Combat.canAttack S.alice kamiId cursed)) "the pair, on the same board before the payment: it cannot attack"
    Spec.assertBool s (not (Combat.canBlock S.alice kamiId cursed)) "nor block"
    Spec.assertBool s (not (any (activating kamiId) (asked S.alice cursed))) "and its ability cannot be activated"
    Spec.assertBool s (not (Combat.canAttack S.alice kamiId swept)) "CR 514.2: the ignore ends at cleanup, so the restriction is back next turn"
    Spec.assertEqWith s "the sacrifice was paid: alice's other creature is gone" (S.countOnBattlefieldByName (S.printingName piker) S.alice afterIgnore) 0
