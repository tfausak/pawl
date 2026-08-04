{-# LANGUAGE GADTs #-}

-- Covers Pawl.Engine.Game, Pawl.Engine.Engine, and Pawl.Engine.Action: zones and changeZone, legal
-- actions, object facts, engine steps, and engine-rule integration (priority
-- rounds, the CR 103.8a draw skip, CR 514.2 discard, CR 704.5b deck-out).
module Pawl.GameSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Concession as Concession
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EntwineDecision as EntwineDecision
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Game as Game.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.MulliganOffer as MulliganOffer
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerEffect as PlayerEffect.Type
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Program as Program
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

objectFactSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
objectFactSpec s registry = Spec.describe s "ObjectFacts" $ do
  Spec.it s "a Piker's power and toughness are 2 and 1" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "power" (Projection.powerOf oid gs) (Just 2)
    Spec.assertEqWith s "toughness" (Projection.toughnessOf oid gs) (Just 1)

  Spec.it s "CR 111.3 a token's characteristics are read through cardOf" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        goblinCard = Printing.card piker
        (tokId, gs) = S.addToken goblinCard S.alice base
    Spec.assertEqWith s "cardOf returns the token's embedded card" (Game.cardOf tokId gs) (Just goblinCard)
    Spec.assertEqWith s "the token is on the battlefield" (Set.member tokId (GameState.battlefield gs)) True

  Spec.it s "CR 112.1 isSpell is True for a spell on the stack, False off it" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        (spellId, gs1) = S.spellOnStack piker S.alice base
        (permId, gs2) = S.addCreature piker S.bob gs1
        tokenCard = Printing.card piker
        (tokId, gs3) = S.addToken tokenCard S.bob gs2
    Spec.assertBool s (Game.isSpell spellId gs3) "a card on the stack is a spell"
    Spec.assertBool s (not (Game.isSpell permId gs3)) "a battlefield permanent is not a spell"
    Spec.assertBool s (not (Game.isSpell tokId gs3)) "a token is not a spell"

  Spec.it s "a Mountain has no power or toughness" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.landsInPlay mountain 1
    case Game.zoneMembers Zone.Battlefield S.alice gs of
      [] -> Spec.assertFailure s "fixture should have one Mountain"
      oid : _ -> do
        Spec.assertEqWith s "power" (Projection.powerOf oid gs) Nothing
        Spec.assertEqWith s "toughness" (Projection.toughnessOf oid gs) Nothing

  Spec.it s "controllerOf is the owner while nothing can change control" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "controller" (Projection.controllerOf oid gs) (Just S.bob)

  Spec.it s "an unknown id has no facts" $ do
    let gs = Setup.emptyGame S.bothPlayers
        missing = ObjectId.MkObjectId 999
    Spec.assertEqWith s "power" (Projection.powerOf missing gs) Nothing
    Spec.assertEqWith s "controller" (Projection.controllerOf missing gs) Nothing

-- The battlefield after changeZone moves the sole Mountain onto it. Loaded
-- fresh inside each case that needs it -- equivalent because loading is
-- deterministic and cached (batch-recipe.md).
afterMountainMoved :: Printing.Printing -> GameState.GameState
afterMountainMoved mountain =
  S.runPure S.identityAnswer (S.oneMountainState mountain Phase.PrecombatMain) (Event.changeZone (ObjectId.MkObjectId 0) Zone.Battlefield)

gameSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gameSpec s registry = Spec.describe s "Game" $ do
  Spec.it s "changeZone preserves object count" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertEqWith s "count" (Game.objectCount (afterMountainMoved mountain)) 1

  Spec.it s "changeZone drops the old id" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertEqWith s "old gone" (Game.lookupObject (ObjectId.MkObjectId 0) (afterMountainMoved mountain)) Nothing

  Spec.it s "the moved object is on the battlefield, owner preserved" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertEqWith
      s
      "moved"
      (Game.lookupObject (ObjectId.MkObjectId 1) (afterMountainMoved mountain))
      ( Just
          Object.MkObject
            { Object.owner = S.alice,
              Object.enteredUnder = Nothing,
              Object.source = Source.OfCard mountain,
              Object.zone = Zone.Battlefield,
              Object.tapped = TapState.Untapped,
              Object.damage = 0,
              Object.sickness = Sickness.Sick,
              Object.bindings = Map.empty,
              Object.counters = Map.empty,
              Object.attachedTo = Nothing,
              Object.chosenColor = Nothing,
              Object.chosenSubtype = Nothing,
              -- changeZone draws a fresh timestamp; oneMountainState's
              -- nextTimestamp starts at 1 (object 0 already holds 0).
              Object.timestamp = Timestamp.MkTimestamp 1,
              -- CR 400.7: changeZone clears any singled-out face along with
              -- every other per-incarnation field.
              Object.face = Nothing
            }
      )

  Spec.it s "CR 400.7 changeZone forgets a spell's bindings" $ do
    mountain <- S.printingOf s registry "Mountain"
    let base = S.oneMountainState mountain Phase.PrecombatMain
        slot = SlotName.MkSlotName (Text.pack "target")
        stamped =
          base
            { GameState.objects =
                Map.adjust
                  (\o -> o {Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToPlayer S.alice)) Nothing Set.empty})
                  (ObjectId.MkObjectId 0)
                  (GameState.objects base)
            }
        moved = S.runPure S.identityAnswer stamped (Event.changeZone (ObjectId.MkObjectId 0) Zone.Battlefield)
        landed = Map.elems (Map.filter (\o -> Object.zone o == Zone.Battlefield) (GameState.objects moved))
    Spec.assertEqWith s "reset" (fmap Object.bindings landed) [Map.empty]

  Spec.it s "CR 613.7d changeZone stamps the new incarnation with a fresh timestamp" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (oid, gs) = S.addCreature piker S.bob (S.landsInPlay mountain 1)
        before = GameState.nextTimestamp gs
        movedState = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Graveyard)
        movedId = case Game.zoneMembers Zone.Graveyard S.bob movedState of
          i : _ -> i
          [] -> ObjectId.MkObjectId 999
        stamp = fmap Object.timestamp (Game.lookupObject movedId movedState)
    Spec.assertEqWith s "the incarnation carries the pre-move next timestamp" stamp (Just before)
    Spec.assertBool s (GameState.nextTimestamp movedState > before) "the counter advanced"

  Spec.it s "emptyGame starts the timestamp counter at zero" $ do
    Spec.assertEqWith s "zero" (GameState.nextTimestamp (Setup.emptyGame S.bothPlayers)) (Timestamp.MkTimestamp 0)

  Spec.it s "a fresh game has no continuous effects" $ do
    Spec.assertEqWith s "empty" (GameState.continuousEffects (Setup.emptyGame S.bothPlayers)) []

  Spec.it s "a vanilla printing declares no static abilities" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    Spec.assertEqWith s "empty" (Face.staticAbilities (S.combinedFace piker)) []

actionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
actionSpec s registry = Spec.describe s "Action" $ do
  Spec.it s "a land in hand is playable in a main phase" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertBool s (A.Play (ObjectId.MkObjectId 0) `elem` Action.legalActions S.alice (S.oneMountainState mountain Phase.PrecombatMain)) "play"

  Spec.it s "passing is always legal" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertBool s (A.Pass `elem` Action.legalActions S.alice (S.oneMountainState mountain Phase.PrecombatMain)) "pass"

  Spec.it s "no land play outside a main phase" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertEqWith s "only pass" (Action.legalActions S.alice (S.oneMountainState mountain (Phase.Beginning BeginningStep.Upkeep))) [A.Pass]

  Spec.it s "no second land after one is played" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs = (S.oneMountainState mountain Phase.PrecombatMain) {GameState.landPlayed = Set.singleton S.alice}
    Spec.assertEqWith s "only pass" (Action.legalActions S.alice gs) [A.Pass]

goldfishResult :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (Result.Result, GameState.GameState)
goldfishResult s registry = do
  matchup <- S.redRed (S.printingOf s registry)
  pure (Engine.runMatchPure S.identityAnswer matchup)

landState :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
landState s registry = do
  matchup <- S.redRed (S.printingOf s registry)
  pure (snd (Engine.runGamePure S.playLandAnswer (Setup.emptyGame S.bothPlayers) (Engine.playFrom matchup)))

-- Alice is active on turns 1, 3, 5, …; bob on 2, 4, 6, …. With one land play per
-- turn (CR 305.2) a player can never have more lands out than turns taken.
turnsTaken :: PlayerId.PlayerId -> GameState.GameState -> Natural
turnsTaken pid gs =
  let total = GameState.turnNumber gs
   in if pid == S.alice then (total + 1) `div` 2 else total `div` 2

engineSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
engineSpec s registry = Spec.describe s "Engine" $ do
  Spec.it s "goldfish game ends with the starting player winning" $ do
    (result, _) <- goldfishResult s registry
    Spec.assertEqWith s "winner" result (Result.Won S.alice)

  Spec.it s "card conservation holds at end" $ do
    (_, gs) <- goldfishResult s registry
    Spec.assertEqWith s "objects" (Game.objectCount gs) 120

  Spec.it s "playing lands fills the battlefield" $ do
    gs <- landState s registry
    Spec.assertBool s (not (null (Game.zoneMembers Zone.Battlefield S.alice gs))) "non-empty"

  Spec.it s "land play conserves cards" $ do
    gs <- landState s registry
    Spec.assertEqWith s "objects" (Game.objectCount gs) 120

  Spec.it s "CR 305.2 at most one land per turn" $ do
    gs <- landState s registry
    Spec.assertBool
      s
      ( Natural.length (Game.zoneMembers Zone.Battlefield S.alice gs) <= turnsTaken S.alice gs
          && Natural.length (Game.zoneMembers Zone.Battlefield S.bob gs) <= turnsTaken S.bob gs
      )
      "no double land plays"

-- Run setup, then a scripted tweak, then whatever steps the scenario needs.
scenario :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Game.Type.Game () -> m GameState.GameState
scenario s registry steps = do
  matchup <- S.redRed (S.printingOf s registry)
  pure . snd . Engine.runGamePure S.identityAnswer (Setup.emptyGame (fmap fst matchup)) $ do
    Setup.newGame S.performer matchup
    steps

-- Alice starts, so her turn-1 draw is skipped.
aliceFirstDraw :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
aliceFirstDraw s registry = scenario s registry S.drawStep

-- Bob is not the starting player, so his draw happens normally.
bobFirstDraw :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
bobFirstDraw s registry = scenario s registry $ do
  State.modify' $ \gs -> gs {GameState.activePlayer = S.bob, GameState.turnNumber = 2}
  S.drawStep

bobAfterCleanup :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
bobAfterCleanup s registry = scenario s registry $ do
  State.modify' $ \gs -> gs {GameState.activePlayer = S.bob, GameState.turnNumber = 2}
  S.drawStep
  Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)

deckedOut :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
deckedOut s registry = scenario s registry $ do
  State.modify' $ \gs ->
    gs
      { GameState.library = Map.insert S.alice Seq.empty (GameState.library gs),
        GameState.turnNumber = 3
      }
  S.drawStep
  Engine.checkSba

librarySize :: PlayerId.PlayerId -> GameState.GameState -> Int
librarySize pid gs = length (Game.zoneMembers Zone.Library pid gs)

-- Records every player asked for an action, in order, and casts when it can.
-- Recording is the point: whether the caster RETAINS priority is only visible in
-- who gets asked next.
recordingAnswer :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
recordingAnswer p = case p of
  Prompt.Concede _ -> pure Concession.Continues
  Prompt.ChooseDefender _ _ candidates -> pure (NonEmpty.head candidates)
  Prompt.ChooseManaSource _ _ candidates -> pure (NonEmpty.head candidates)
  Prompt.ChooseManaYield _ _ _ candidates -> pure (NonEmpty.head candidates)
  Prompt.ChooseProliferate {} -> pure (Set.empty, Set.empty)
  Prompt.ChooseLegend _ _ candidates -> pure (NonEmpty.head candidates)
  Prompt.DeclareAttackers {} -> pure []
  Prompt.ChooseAttackTarget _ _ _ options -> pure (NonEmpty.head options)
  Prompt.DeclareBlockers {} -> pure Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    pure $ case filter S.isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.Shuffle ids -> pure ids
  Prompt.RandomFirstPlayer order -> pure (NonEmpty.head order)
  Prompt.ChooseTargets _ _ _ sets -> pure (Map.mapMaybe Set.lookupMin sets)
  Prompt.ChooseDiscard _ _ ids n -> pure (List.genericTake n ids)
  Prompt.ChooseAction _ pid actions -> do
    State.modify' (\asked -> asked <> [pid])
    let isCast a = case a of
          A.Cast {} -> True
          _ -> False
    pure $ case filter isCast actions of
      h : _ -> h
      [] -> A.Pass
  Prompt.ChooseLandTypeSwap {} -> pure (Subtype.Mountain, Subtype.Mountain)
  Prompt.ChooseCreatureTypeSwap {} -> pure (Subtype.Frog, Subtype.Frog)
  Prompt.SearchLibrary {} -> pure Nothing
  Prompt.CastWhileSearching {} -> pure Nothing
  Prompt.ChooseX {} -> pure 0
  Prompt.ChooseModes _ _ _ legal count -> pure (Set.fromList (List.genericTake count (Set.toAscList legal)))
  Prompt.ChooseCopyTarget {} -> pure Nothing
  Prompt.ChooseEntryOption {} -> pure 0
  Prompt.ChooseColor {} -> pure Color.White
  Prompt.ChooseBasicLandType {} -> pure Subtype.Mountain
  Prompt.OrderTriggers _ _ entries -> pure (zipWith const [0 ..] entries)
  Prompt.OrderDamage _ _ events -> pure (zipWith const [0 ..] events)
  Prompt.ChooseReplacement {} -> pure 0
  Prompt.ChooseBoundToken _ _ _ candidates -> pure (NonEmpty.head candidates)
  Prompt.ChooseAttachment _ _ _ candidates -> pure (NonEmpty.head candidates)
  Prompt.ChooseSacrifices _ _ _ candidates count -> pure (Set.fromList (List.genericTake count candidates))
  Prompt.ChooseCost _ _ _ candidates -> pure (Cost.firstOffered candidates)
  Prompt.DeclareMulligan {} -> pure MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> pure (List.genericTake count hand)
  Prompt.MulliganAction {} -> pure Nothing
  Prompt.OpeningHandAction {} -> pure Nothing
  -- CR 603.5: declining a printed "may" is the least-eventful answer.
  Prompt.ChooseOptional {} -> pure OptionalDecision.Declines
  -- CR 118.13a: the head is a legal answer -- every offered route is payable --
  -- and is the least eventful default, matching Replay.defaultAnswer.
  Prompt.AnnouncePhyrexianPayment _ _ _ _ offers -> pure (NonEmpty.head offers)
  -- CR 702.42a: declining entwine is always legal, costs nothing and changes
  -- no mode, the least-eventful default (mirrors ChooseOptional -> Declines).
  Prompt.ChooseEntwine {} -> pure EntwineDecision.Declines

-- pikerInHand already builds on Setup.emptyGame bothPlayers, so turnOrder is
-- [alice, bob] and both players are in the players map.
askedPlayers :: Printing.Printing -> Printing.Printing -> [PlayerId.PlayerId]
askedPlayers mountain piker =
  let (gs, _) = S.pikerInHand mountain piker 3 Phase.PrecombatMain
   in State.execState
        (Program.foldProgramM recordingAnswer (State.runStateT Engine.priorityLoop gs))
        []

ruleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ruleSpec s registry = Spec.describe s "Rules" $ do
  Spec.it s "CR 117.4 a full round of passes resolves the stack, not the step" $ do
    -- With a spell on the stack, everyone passing must RESOLVE it and keep
    -- the step alive. Under M0's rule the step would simply end with the
    -- spell still sitting on the stack.
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, oid) = S.pikerInHand mountain piker 3 Phase.PrecombatMain
        steps = do
          S.cast S.alice oid
          Engine.priorityLoop
        after = snd (Engine.runGamePure S.identityAnswer gs steps)
    Spec.assertEqWith s "stack emptied" (length (GameState.stack after)) 0
    Spec.assertEqWith s "piker resolved onto the battlefield" (S.creaturesInPlay S.alice after) 1

  Spec.it s "CR 117.3c the caster is asked again, rather than passing priority on" $ do
    -- alice is asked, casts, and must be asked AGAIN before bob gets a turn.
    -- If priority wrongly advanced to the next player, this would be
    -- [alice, bob, ...] instead.
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    Spec.assertEqWith s "alice twice, then bob" (take 3 (askedPlayers mountain piker)) [S.alice, S.alice, S.bob]

  Spec.it s "CR 103.8a starting player skips first draw" $ do
    gs <- aliceFirstDraw s registry
    Spec.assertEqWith s "hand" (S.handSize S.alice gs) 7
    Spec.assertEqWith s "library" (librarySize S.alice gs) 53

  Spec.it s "CR 103.8a only the starting player skips" $ do
    gs <- bobFirstDraw s registry
    Spec.assertEqWith s "hand" (S.handSize S.bob gs) 8
    Spec.assertEqWith s "library" (librarySize S.bob gs) 52

  Spec.it s "CR 103.8c/800.7 nobody skips the first draw in a three-player game" $ do
    -- CR 103.8a's skip is a TWO-player rule: "In a two-player game, the
    -- player who plays first skips the draw step ... of their first turn."
    -- CR 103.8c: "In all other multiplayer games, no player skips the draw
    -- step of their first turn." Alice heads the three-seat order and is
    -- active on turn 1, so today she skips and this fails.
    --
    -- Neither fixture has a library, so the attempted draw is observable as
    -- drewFromEmpty -- Event.drawCard flags a draw from an empty library.
    -- turnNumber is 1 from Setup.emptyGame, which is load-bearing: it is
    -- skipsDraw's first conjunct.
    let atDrawStep gs = gs {GameState.phase = Phase.Beginning BeginningStep.DrawStep}
        afterThree = S.runPure S.identityAnswer (atDrawStep S.threePlayerGame) S.drawStep
        afterTwo = S.runPure S.identityAnswer (atDrawStep (Setup.emptyGame S.bothPlayers)) S.drawStep
    Spec.assertBool s (Set.member S.alice (GameState.drewFromEmpty afterThree)) "CR 103.8c: alice draws on turn one at three seats"
    Spec.assertBool s (not (Set.member S.alice (GameState.drewFromEmpty afterTwo))) "CR 103.8a: alice still skips at two seats"

  Spec.it s "CR 514.2 discard to hand size" $ do
    gs <- bobAfterCleanup s registry
    Spec.assertEqWith s "hand" (S.handSize S.bob gs) 7

  Spec.it s "CR 704.5b deck-out loses" $ do
    gs <- deckedOut s registry
    Spec.assertEqWith
      s
      "alice departed"
      (fmap Player.status (Map.lookup S.alice (GameState.players gs)))
      (Just (Status.Departed Departure.Type.Lost))

  Spec.it s "CR 704.5b the survivor wins" $ do
    gs <- deckedOut s registry
    Spec.assertEqWith s "bob won" (GameState.result gs) (Just (Result.Won S.bob))

  Spec.it s "CR 723.3/723.5: alice decides for bob, but bob's resources move" $ do
    -- bob's main phase, controlled by alice, with a Mountain and a Bolt.
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let g0 = Setup.emptyGame S.bothPlayers
        (_mtnId, g1) = S.addCreature mountain S.bob g0
        (_boltId, g2) = handBobBolt lightningBolt g1
        g3 =
          g2
            { GameState.activePlayer = S.bob,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.bob,
              GameState.activeControl = Just (Decider.MkDecider S.alice)
            }
        after = snd (Engine.runGamePure slaveAnswer g3 Engine.priorityLoop)
        boltInBobGrave =
          length
            ( filter
                (namedIs (CardName.MkCardName $ Text.pack "Lightning Bolt"))
                (fmap (\i -> Game.lookupObject i after) (Game.zoneMembers Zone.Graveyard S.bob after))
            )
    Spec.assertEqWith s "bob took 3 from his own Bolt" (S.lifeOf S.bob after) (Just 17)
    Spec.assertEqWith s "bob's Bolt is in bob's graveyard" boltInBobGrave 1
    Spec.assertEqWith s "the Mountain (bob's) is tapped" (S.tappedCount S.bob after) 1

  Spec.it s "CR 723.1/723.3 gameplay: Mindslaver hands alice bob's whole turn, then control lapses" $ do
    -- Alice activates a REAL Mindslaver through the driver loop at bob, the
    -- engine promotes control on bob's turn, alice casts bob's Bolt at bob
    -- (bob's own resource), and control ends at the following turn boundary.
    mindslaver <- S.printingOf s registry "Mindslaver"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let g0 = Setup.emptyGame S.bothPlayers
        (_msId, g1) = S.addCreature mindslaver S.alice g0
        -- {4} for Mindslaver's activation: four untapped Mountains for alice.
        (_a1, g2) = S.addCreature mountain S.alice g1
        (_a2, g3) = S.addCreature mountain S.alice g2
        (_a3, g4) = S.addCreature mountain S.alice g3
        (_a4, g5) = S.addCreature mountain S.alice g4
        -- bob's own resources for his controlled turn: a Mountain and a Bolt.
        (_bMtn, g6) = S.addCreature mountain S.bob g5
        (_bBolt, g7) = handBobBolt lightningBolt g6
        gStart =
          g7
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        -- Alice's turn: activate Mindslaver at bob; the ability resolves and
        -- installs pending control for bob (CR 723.1).
        afterActivation = snd (Engine.runGamePure gateAnswer gStart Engine.priorityLoop)
        -- Handoff to bob's turn promotes pendingControl -> activeControl.
        bobsTurn = snd (Engine.runGamePure gateAnswer afterActivation Engine.handoffTurn)
        -- Bob's controlled main phase: alice decides, casting bob's Bolt at bob.
        bobMain = bobsTurn {GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.bob}
        bobPlayed = snd (Engine.runGamePure gateAnswer bobMain Engine.priorityLoop)
        -- Next handoff (bob -> alice) clears control (CR 723.1: ends at the
        -- beginning of the next turn).
        afterBob = snd (Engine.runGamePure gateAnswer bobPlayed Engine.handoffTurn)
        boltInBobGrave =
          length
            ( filter
                (namedIs (CardName.MkCardName $ Text.pack "Lightning Bolt"))
                (fmap (\i -> Game.lookupObject i bobPlayed) (Game.zoneMembers Zone.Graveyard S.bob bobPlayed))
            )
    Spec.assertEqWith s "CR 723.1: control pending for bob after activation" (Map.lookup S.bob (GameState.pendingControl afterActivation)) (Just (Decider.MkDecider S.alice))
    Spec.assertEqWith s "CR 723.1: promoted to active control on bob's turn" (GameState.activeControl bobsTurn) (Just (Decider.MkDecider S.alice))
    Spec.assertEqWith s "CR 723.3: bob is still the active player while controlled" (GameState.activePlayer bobsTurn) S.bob
    Spec.assertEqWith s "CR 723.5: bob's decisions route to alice" (Decide.deciderFor S.bob bobsTurn) (Decider.MkDecider S.alice)
    Spec.assertEqWith s "alice's whole-turn choice moved bob's life" (S.lifeOf S.bob bobPlayed) (Just 17)
    Spec.assertEqWith s "bob's Bolt went to bob's graveyard" boltInBobGrave 1
    Spec.assertEqWith s "bob's Mountain (his resource) is tapped" (S.tappedCount S.bob bobPlayed) 1
    Spec.assertEqWith s "CR 723.1: control lapses at the next turn" (Decide.deciderFor S.bob afterBob) (Decider.MkDecider S.bob)
    Spec.assertEqWith s "active control cleared after bob's turn" (GameState.activeControl afterBob) Nothing

  Spec.it s "CR 723.5 combat: alice declares bob's attackers, so alice takes the hit" $ do
    -- bob's turn, controlled by alice, with one 2/1 Piker. combatBoardOf sets
    -- alice active with `mine` and bob with `theirs`; here alice attacks with
    -- nothing and bob has the Piker, and we flip the active player to bob.
    -- Flipping who attacks flips who defends: combatBoardOf states bob as the
    -- defending player because alice is active there (CR 506.2), so a board
    -- that makes bob active must name alice instead -- otherwise bob would be
    -- declared the defending player on his own turn.
    piker <- S.printingOf s registry "Goblin Piker"
    let (board, _mine, _bobsPikers) = S.combatBoardOf [] [piker]
        g0 =
          board
            { GameState.activePlayer = S.bob,
              GameState.activeControl = Just (Decider.MkDecider S.alice),
              GameState.combat = (GameState.combat board) {Combat.Type.defender = Just S.alice}
            }
        after = S.runCombat controlCombatAnswer g0
    Spec.assertEqWith s "alice took 2 from bob's Piker, declared by alice-as-bob" (S.lifeOf S.alice after) (Just 18)

  Spec.it s "CR 723.5a: the controller spends only the controlled player's resources" $ do
    -- bob (controlled by alice) and alice each have an untapped Mountain; bob
    -- has a Bolt. Alice-as-bob casts bob's Bolt, paid from BOB's Mountain.
    -- alice's Mountain and hand must be untouched.
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let g0 = Setup.emptyGame S.bothPlayers
        (_bMtn, g1) = S.addCreature mountain S.bob g0
        (_aMtn, g2) = S.addCreature mountain S.alice g1
        (_bBolt, g3) = handBobBolt lightningBolt g2
        g4 =
          g3
            { GameState.activePlayer = S.bob,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.bob,
              GameState.activeControl = Just (Decider.MkDecider S.alice)
            }
        after = snd (Engine.runGamePure slaveAnswer g4 Engine.priorityLoop)
    Spec.assertEqWith s "bob took 3 from his own Bolt" (S.lifeOf S.bob after) (Just 17)
    Spec.assertEqWith s "bob's Mountain (his resource) is tapped" (S.tappedCount S.bob after) 1
    Spec.assertEqWith s "CR 723.5a: alice's Mountain is untouched" (S.tappedCount S.alice after) 0
    Spec.assertEqWith s "CR 723.5a: alice's hand is untouched" (S.handSize S.alice after) 0

  Spec.it s "CR 727.1/727.2/727.4 gameplay: bob activates a restart and the game rebuilds from its own cards" $ do
    -- bob controls the synthetic restart artifact and owns 8 cards total;
    -- alice owns 8. Both start with reduced life on a populated board. bob
    -- activates the artifact through the priority loop; it resolves, restarts
    -- the game, and the result is a valid new game with bob as starter.
    syntheticRestart <- S.printingOf s registry "Synthetic Restart"
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.bothPlayers
        (_restartId, g1) = S.addCreature syntheticRestart S.bob g0
        (_aPiker, g2) = S.addCreature piker S.alice g1
        -- fill each owner's pool to >= 7 cards so opening hands draw without a
        -- CR 727.3 loss (the restart artifact + 7 mountains = 8 for bob).
        g3 = addManyG mountain 7 S.bob (addManyG mountain 7 S.alice g2)
        gStart =
          g3
            { GameState.activePlayer = S.bob,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.bob,
              -- Knock both players to 8 life so "both players reset to 20
              -- life" below is load-bearing: Setup.emptyGame already starts
              -- players at 20, so without this reduction the assertions
              -- would pass even if resetPlayer did nothing.
              GameState.players = Map.adjust (\p -> p {Player.life = 8}) S.alice (Map.adjust (\p -> p {Player.life = 8}) S.bob (GameState.players g3))
            }
        after = snd (Engine.runGamePure restartAnswer gStart Engine.priorityLoop)
    Spec.assertEqWith s "CR 727.1a: bob is the new active player" (GameState.activePlayer after) S.bob
    Spec.assertEqWith s "CR 727.1a: the turn order begins with bob" (Maybe.listToMaybe (GameState.turnOrder after)) (Just S.bob)
    Spec.assertEqWith s "both players reset to 20 life (alice)" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "both players reset to 20 life (bob)" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "CR 103.5: alice drew a 7-card opening hand" (S.handSize S.alice after) 7
    Spec.assertEqWith s "CR 103.5: bob drew a 7-card opening hand" (S.handSize S.bob after) 7
    Spec.assertEqWith s "CR 727.4: settled at the first untap step" (GameState.phase after) Turn.firstPhase
    Spec.assertEqWith s "CR 727.2: the battlefield is empty (every card returned to a library)" (Set.null (GameState.battlefield after)) True
    Spec.assertEqWith s "the game did not end -- the new game is live" (GameState.result after) Nothing

  Spec.it s "CR 729.2/729.3/729.5: playSubgame runs a nested game, bob decks, cards funnel back" $ do
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.bothPlayers
        -- alice: 8 library cards; bob: 3 (fewer than seven -> loses, CR 729.3)
        g1 = poolToLibraryG S.bob (poolToLibraryG S.alice (addManyG mountain 3 S.bob (addManyG mountain 8 S.alice g0)))
        (result, after) = Engine.runGamePure S.identityAnswer g1 Engine.playSubgame
        libCount pid = length (Game.zoneMembers Zone.Library pid after)
    Spec.assertEqWith s "CR 729.3: bob has fewer than 7 cards, so alice wins the subgame" result (Result.Won S.alice)
    Spec.assertEqWith s "CR 729.5: alice's cards funnel back into her main-game library" (libCount S.alice) 8
    Spec.assertEqWith s "CR 729.5: bob's cards funnel back into his main-game library" (libCount S.bob) 3
    Spec.assertEqWith s "the main game resumes with no result recorded" (GameState.result after) Nothing

  -- #136 / CR 729.2: "Randomly determine which player goes first." The
  -- interpreter supplies the randomness (Prompt.RandomFirstPlayer); the
  -- engine only asks. Both players get libraries of EXACTLY seven, so each
  -- opening hand (CR 103.5) empties its library without drawing from empty:
  -- nobody loses during setup. The starting player then skips their first
  -- draw (CR 103.8a), so the OTHER player is the one who draws from an empty
  -- library on turn 2 and decks (CR 704.5b) -- the subgame's winner is
  -- exactly whoever the roll started. Flipping the answer flips the winner,
  -- which is what makes the determination observable rather than cosmetic.
  Spec.it s "CR 729.2: the subgame's first player comes from the roll; the answer decides who wins" $ do
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.bothPlayers
        g1 = poolToLibraryG S.bob (poolToLibraryG S.alice (addManyG mountain 7 S.bob (addManyG mountain 7 S.alice g0)))
        winnerWhenStarting starter = fst (Engine.runGamePure (firstPlayerAnswer starter) g1 Engine.playSubgame)
    Spec.assertEqWith s "alice starts, skips her first draw, and bob decks on turn 2" (winnerWhenStarting S.alice) (Result.Won S.alice)
    Spec.assertEqWith s "bob starts, skips his first draw, and alice decks on turn 2" (winnerWhenStarting S.bob) (Result.Won S.bob)

  Spec.it s "CR 729.2: a lone player is not asked -- the determination is forced" $ do
    -- Where the rules leave nothing to determine, don't prompt: with one
    -- player in the turn order, every roll yields the same starter. alice's
    -- library is empty, so her opening draw decks her (CR 704.5b) and the
    -- subgame ends during setup -- long enough to record an ask if one were
    -- made, which is what the transcript is inspected for.
    let g0 = Setup.emptyGame (S.alice NonEmpty.:| [])
        (_, log_) = Replay.record S.identityAnswer g0 Engine.playSubgame
        isRoll r = case r of
          Response.DeterminedFirstPlayer _ -> True
          _ -> False
    Spec.assertEqWith s "no first-player roll was recorded" (length (filter isRoll log_)) 0

  Spec.it s "CR 103.1/729.2: the subgame's turn order is rotated to begin with the starting player" $ do
    -- Not just activePlayer: Engine.skipsDraw (CR 103.8a) reads the HEAD of
    -- the turn order, so a subgame that set one without the other would hand
    -- the skip to the wrong player.
    let sub = Setup.subgameStateFrom S.bob (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "bob is the subgame's active player" (GameState.activePlayer sub) S.bob
    Spec.assertEqWith s "the subgame turn order begins with bob" (GameState.turnOrder sub) [S.bob, S.alice]

  Spec.it s "CR 729.2/729.4 #147: a departed player is not offered as the subgame's first player, and has no library for the subgame to churn" $ do
    -- Three seats; bob has left. CR 800.4a's first clause took his cards with
    -- him, so what this case now pins is (a) the CR 729.2 roll offers only the
    -- players still in the game, and (b) the subgame round trip does not
    -- resurrect a departed player's library. The full-roster span in
    -- Setup.subgameStateFrom and Setup.funnelBack still has to match; see the
    -- comments at both sites.
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.threePlayers
        g1 = poolToLibraryG S.carol (poolToLibraryG S.bob (poolToLibraryG S.alice (addManyG mountain 7 S.carol (addManyG mountain 3 S.bob (addManyG mountain 7 S.alice g0)))))
        parent = Departure.depart Departure.Type.Conceded S.bob g1
        ((_, after), rolls) = State.runState (Engine.runGame subgameRosterAnswer parent Engine.playSubgame) []
    Spec.assertEqWith s "the roll offers only the players still in the game" rolls [[S.alice, S.carol]]
    Spec.assertEqWith s "CR 800.4a: bob's cards left the game with him" (Game.zoneMembers Zone.Library S.bob parent) []
    Spec.assertEqWith s "and the subgame round trip does not give them back" (Game.zoneMembers Zone.Library S.bob after) []
    Spec.assertEqWith s "alice's library came back whole" (length (Game.zoneMembers Zone.Library S.alice after)) 7
    Spec.assertEqWith s "and carol's" (length (Game.zoneMembers Zone.Library S.carol after)) 7
    Spec.assertEqWith s "the main game is not decided by the subgame" (GameState.result after) Nothing

  Spec.it s "CR 729.1b/729.3 gameplay: alice casts a subgame spell, bob decks, bob takes 3" $ do
    -- alice has the {0} subgame sorcery in hand and an 8-card library; bob has
    -- a 3-card library (decks in the subgame, CR 729.3). alice casts through
    -- the priority loop; the subgame resolves alice the winner; the follow-on
    -- DealDamage hits bob (the loser) for 3.
    mountain <- S.printingOf s registry "Mountain"
    syntheticSubgame <- S.printingOf s registry "Synthetic Subgame"
    let g0 = Setup.emptyGame S.bothPlayers
        g1 = poolToLibraryG S.bob (poolToLibraryG S.alice (addManyG mountain 3 S.bob (addManyG mountain 8 S.alice g0)))
        (spellId, g2) = S.addHandCard syntheticSubgame S.alice g1
        gStart =
          g2
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        after = snd (Engine.runGamePure subgameAnswer gStart Engine.priorityLoop)
    Spec.assertEqWith s "CR 729.1b: bob (the subgame loser) took 3 from the follow-on DealDamage" (S.lifeOf S.bob after) (Just 17)
    Spec.assertEqWith s "alice, the winner, is untouched" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "the subgame spell resolved and left the stack" (GameState.stack after) []
    Spec.assertEqWith s "the main game did not end" (GameState.result after) Nothing
    -- Casting routes through changeZone (CR 400.7), which mints a fresh
    -- id and drops spellId entirely -- Game.lookupObject spellId after
    -- is unconditionally Nothing, so that alone proves nothing about
    -- alice's hand. The load-bearing check is that spellId's old
    -- incarnation is not lingering in her hand's member list.
    Spec.assertEqWith s "the subgame spell's original id no longer sits in alice's hand (cast)" (notElem spellId (Game.zoneMembers Zone.Hand S.alice after)) True

  Spec.it s "CR 729.5/729.4b gameplay: cards funnel back, main-game board survives, main-game counters untouched" $ do
    -- library pool built first, THEN the survivor is added to the battlefield --
    -- poolToLibraryG sweeps every object a player owns onto their library, so a
    -- survivor added before it would be swept in too and vanish from the board.
    mountain <- S.printingOf s registry "Mountain"
    syntheticSubgame <- S.printingOf s registry "Synthetic Subgame"
    let g0 = Setup.emptyGame S.bothPlayers
        g1 = poolToLibraryG S.bob (poolToLibraryG S.alice (addManyG mountain 3 S.bob (addManyG mountain 8 S.alice g0)))
        -- a survivor on the main battlefield that must remain after the subgame
        (survivorId, g2) = S.addCreature mountain S.alice g1
        (_spellId, g3) = S.addHandCard syntheticSubgame S.alice g2
        -- give bob a main-game poison counter (CR 729.4b: outside the subgame)
        g4 =
          g3
            { GameState.players =
                Map.adjust
                  (\pl -> pl {Player.counters = Map.insert PlayerCounterKind.Poison 1 (Player.counters pl)})
                  S.bob
                  (GameState.players g3)
            }
        gStart =
          g4
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.PrecombatMain,
              GameState.priority = Just S.alice
            }
        after = snd (Engine.runGamePure subgameAnswer gStart Engine.priorityLoop)
        bobPoison =
          maybe
            0
            (Map.findWithDefault 0 PlayerCounterKind.Poison . Player.counters)
            (Map.lookup S.bob (GameState.players after))
    Spec.assertEqWith s "CR 729.4b: bob's main-game poison counter is untouched by the subgame" bobPoison 1
    Spec.assertEqWith s "CR 729.5: alice's library holds her 8 subgame cards again" (length (Game.zoneMembers Zone.Library S.alice after)) 8
    Spec.assertEqWith s "CR 729.5: bob's library holds his 3 subgame cards again" (length (Game.zoneMembers Zone.Library S.bob after)) 3
    Spec.assertEqWith s "the main-game survivor is still on the battlefield" (Set.member survivorId (GameState.battlefield after)) True

  Spec.it s "CR 729.6 gameplay: a subgame nests a subgame; nesting terminates and the main game resumes" $ do
    -- alice's MAIN-GAME library feeds level 1's library: one nested
    -- synthetic-subgame sorcery + 13 Mountains (14 total). The level-1 opening
    -- hand draws 7 (the sorcery + 6 Mountains), leaving exactly 7 Mountains in
    -- her level-1 library -- enough that she does NOT deck when level 2's
    -- opening hand draws from it (CR 729.2 pulls a subgame's library from its
    -- parent's library). bob's library is exactly 7 Mountains: his level-1
    -- opening hand consumes all seven (no immediate CR 704.5b loss -- every
    -- draw still succeeds), leaving his level-1 library EMPTY, so he decks at
    -- his own level-1 draw step (turn 2) -- real level-1 play, not an instant
    -- SBA loss before anyone gets priority. Sized this way (rather than the
    -- brief's flat "3 Mountains decks instantly") because an immediate deck-out
    -- during subgame setup fires before ANY priorityLoop grants alice priority
    -- (Sba.losesNow reads GameState.drewFromEmpty, set during the opening-hand
    -- draw itself), which would leave alice no window to cast the nested
    -- sorcery at all and collapse this gate to a flat (non-nested) subgame.
    --
    -- CR 729.1a's isolation means a subgame's INTERNAL choices leave no trace
    -- in the parent's GameState -- but the interpreter TRANSCRIPT (every
    -- Response, recorded by Pawl.Engine.Replay.record) is a top-level observable, and
    -- it DOES discriminate nesting depth: each subgame level's setup
    -- (subgameStateFrom -> startGameFromCards) shuffles every player's
    -- library once, and playSubgame's CR 729.5 funnel-back reshuffles the
    -- parent's library once per player -- 2 Response.Shuffled entries per
    -- level, per funnel. A flat (single-level) subgame gate contributes 4
    -- (2 setup + 2 funnel-back); this two-level gate contributes 8 (2 levels
    -- x (2 setup + 2 funnel-back)), so asserting the count is a genuine
    -- nesting regression test, not just a termination guard.
    syntheticSubgame <- S.printingOf s registry "Synthetic Subgame"
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.bothPlayers
        (_nestedId, g1) = libraryCard syntheticSubgame S.alice g0
        g2 = poolToLibraryG S.bob (addToLibraryG mountain 13 S.alice (addManyG mountain 7 S.bob g1))
        (_spellId, g3) = S.addHandCard syntheticSubgame S.alice g2
        gStart = g3 {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
        ((_, after), log_) = Replay.record subgameAnswer gStart Engine.priorityLoop
        isShuffled r = case r of
          Response.Shuffled _ -> True
          _ -> False
        shuffles = length (filter isShuffled log_)
    -- If nesting had not terminated, runGamePure/Replay.record would not return.
    Spec.assertEqWith s "CR 729.6: the top-level main game resumed with no result" (GameState.result after) Nothing
    Spec.assertEqWith s "CR 729.1b: bob took 3 from the level-1 subgame's follow-on" (S.lifeOf S.bob after) (Just 17)
    Spec.assertEqWith s "the top-level subgame spell left the stack" (GameState.stack after) []
    Spec.assertEqWith s "CR 729.6: two nested subgame levels each shuffle on setup and funnel-back (measured; a flat gate yields 4)" shuffles 8

  Spec.it s "a subgame replays deterministically (the reason Prompt.PlaySubgame was rejected, CR 729 / M0's determinism criterion)" $ do
    -- A Prompt would run the subgame INSIDE the answer function, below
    -- Replay.record's interposition point, so its inner choices could never
    -- be recovered from the recorded transcript. Round-tripping this nested
    -- gate's fixture through record -> replay and comparing the final
    -- GameState (derives Eq) is the test that would fail if that design had
    -- been taken instead.
    syntheticSubgame <- S.printingOf s registry "Synthetic Subgame"
    mountain <- S.printingOf s registry "Mountain"
    let g0 = Setup.emptyGame S.bothPlayers
        (_nestedId, g1) = libraryCard syntheticSubgame S.alice g0
        g2 = poolToLibraryG S.bob (addToLibraryG mountain 13 S.alice (addManyG mountain 7 S.bob g1))
        (_spellId, g3) = S.addHandCard syntheticSubgame S.alice g2
        gStart = g3 {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
        ((_, after), log_) = Replay.record subgameAnswer gStart Engine.priorityLoop
        ((_, replayed), desync) = Replay.replay log_ gStart Engine.priorityLoop
    Spec.assertEqWith s "a subgame replays deterministically (the reason PlaySubgame is not a Prompt, CR 729 / M0 determinism)" replayed after
    Spec.assertEqWith s "and the transcript answered every prompt" desync Nothing

  Spec.it s "M5.6b gate: three-player setup -- a free first mulligan (CR 103.5c) and no first-turn draw skip (CR 103.8c)" $ do
    -- A real newGame at three seats: build and shuffle three 60-card
    -- libraries, draw opening hands, run the CR 103.5 round loop with every
    -- player taking exactly one mulligan, then run turn one's draw step.
    --
    -- CR 103.5c: the first mulligan does not count toward the number of cards
    -- bottomed, so every player keeps seven and nothing goes back to the
    -- library -- today each keeps six and bottoms one. CR 103.8c: the
    -- starting player does not skip the first draw -- today alice skips.
    matchup <- S.threeWayMirror (S.printingOf s registry)
    let g0 = Setup.emptyGame (fmap fst matchup)
        afterSetup = snd (Engine.runGamePure mulliganOnceAnswer g0 (Setup.newGame S.performer matchup))
        afterDraw = snd (Engine.runGamePure mulliganOnceAnswer afterSetup S.drawStep)
    Spec.assertEqWith s "alice's free mulligan bottomed nothing" (S.handSize S.alice afterSetup) 7
    Spec.assertEqWith s "bob's did not either" (S.handSize S.bob afterSetup) 7
    Spec.assertEqWith s "nor carol's" (S.handSize S.carol afterSetup) 7
    Spec.assertEqWith s "so alice's library is 60 less her seven, with nothing bottomed back" (librarySize S.alice afterSetup) 53
    Spec.assertEqWith s "CR 103.8c: alice draws on turn one" (S.handSize S.alice afterDraw) 8
    Spec.assertEqWith s "and her library is one lower for it" (librarySize S.alice afterDraw) 52
    Spec.assertEqWith s "the draw step is alice's alone" (S.handSize S.bob afterDraw) 7

-- #133 / CR 104.3a. Concede is a special action, not a card, so the gate is
-- gameplay-level. The central case is CR 723.6: a Mindslaver controller may not
-- concede for the player they control, but that player may still concede
-- themselves -- which is why Prompt.Concede carries no Decider.

-- Concedes for exactly one player, continues for everyone else, and otherwise
-- passes. The PlayerId the prompt carries is the TRUE player: if the engine ever
-- routed this through Decide.deciderFor, this answerer would concede for the
-- wrong person and the CR 723.6 test below would fail.
concedeAnswer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
concedeAnswer who p = case p of
  Prompt.Concede asked -> if asked == who then Concession.Concedes else Concession.Continues
  _ -> S.identityAnswer p

-- Records the PlayerId of every Concede ask, in order, and concedes for `who`.
-- Prompt.Concede is asked of the priority holder before anything else, so the
-- recorded order IS the order priority moved in. Delegating through a wildcard
-- keeps this answerer out of the -Werror exhaustiveness net.
concedeOrderAnswer :: PlayerId.PlayerId -> Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
concedeOrderAnswer who p = case p of
  Prompt.Concede asked -> do
    State.modify' (\seen -> seen <> [asked])
    pure (if asked == who then Concession.Concedes else Concession.Continues)
  _ -> pure (S.identityAnswer p)

concedeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
concedeSpec s registry = Spec.describe s "concede (CR 104.3a)" $ do
  Spec.it s "CR 104.3a/104.2a a concede ends the game immediately, opponent wins" $ do
    let gs = (Setup.emptyGame S.bothPlayers) {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice}
        after = S.runPure (concedeAnswer S.alice) gs Engine.runStep
    Spec.assertEqWith s "bob wins" (GameState.result after) (Just (Result.Won S.bob))

  Spec.it s "the conceding player departs as Conceded, not Lost" $ do
    let gs = (Setup.emptyGame S.bothPlayers) {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice}
        after = S.runPure (concedeAnswer S.alice) gs Engine.runStep
    Spec.assertEqWith s "reason recorded" (fmap Player.status (Map.lookup S.alice (GameState.players after))) (Just (Status.Departed Departure.Type.Conceded))

  Spec.it s "CR 723.6 a controlled player concedes themselves; their controller cannot do it for them" $ do
    -- alice controls bob (Mindslaver). Every ChooseAction for bob is answered
    -- by alice. The concede ask is NOT: it reaches bob, and bob takes it.
    -- If Prompt.Concede carried a Decider, this would be alice's call.
    --
    -- The premise -- that alice genuinely IS bob's decider for this run -- is
    -- not merely set up, it is OBSERVED: bob is given a land to play so a real
    -- Prompt.ChooseAction fires for him before he concedes, and the answerer
    -- records the Decider that prompt actually carried. A silent regression in
    -- Decide.deciderFor (activeControl stops being honoured) would make this
    -- record MkDecider bob instead, and the test would catch it even though
    -- the headline outcome (bob departs Conceded, alice wins) would still hold.
    mountain <- S.printingOf s registry "Mountain"
    let (mountainOid, gs) =
          S.addHandCard
            mountain
            S.bob
            ( (Setup.emptyGame S.bothPlayers)
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.bob,
                  GameState.activeControl = Just (Decider.MkDecider S.alice)
                }
            )
        -- (deciders seen for bob's ChooseAction, PlayerIds seen for Concede).
        -- Bob's first Concede ask answers Continues so the ChooseAction below
        -- fires and gets recorded; he plays the land (which keeps priority
        -- with him, CR 305.3 style timing aside -- Engine.priorityLoop simply
        -- re-loops with `priority = Just p` after a Play), and only on the
        -- SECOND Concede ask -- now with a recorded ChooseAction in hand --
        -- does he actually concede.
        answer :: Prompt.Prompt r -> State.State ([Decider.Decider], [PlayerId.PlayerId]) r
        answer p = case p of
          Prompt.ChooseAction decider pid _ ->
            if pid == S.bob
              then do
                State.modify' (\(ds, cs) -> (ds <> [decider], cs))
                pure (A.Play mountainOid)
              else pure (S.identityAnswer p)
          Prompt.Concede asked -> do
            (_, asksSoFar) <- State.get
            State.modify' (\(ds, cs) -> (ds, cs <> [asked]))
            pure (if null asksSoFar then Concession.Continues else Concession.Concedes)
          _ -> pure (S.identityAnswer p)
        ((_, after), (deciders, concedeAsks)) = State.runState (Engine.runGame answer gs Engine.runStep) ([], [])
    Spec.assertEqWith s "bob's ChooseAction carried alice as decider (bob genuinely is controlled)" deciders [Decider.MkDecider S.alice]
    Spec.assertEqWith s "every Concede ask reached bob himself, not his controller" concedeAsks [S.bob, S.bob]
    Spec.assertEqWith s "bob left by his own concession" (fmap Player.status (Map.lookup S.bob (GameState.players after))) (Just (Status.Departed Departure.Type.Conceded))
    Spec.assertEqWith s "alice wins" (GameState.result after) (Just (Result.Won S.alice))

  -- #144, and the proof that the elision is NOT cosmetic latency. CR 104.3a
  -- says a player may concede "at any time"; Engine.priorityLoop offers the
  -- ask only at a priority the conceder themselves holds, and CR 117.5 /
  -- CR 704.3 put a state-based action check in front of every priority
  -- grant. So the two are in a race, and the race decides the game.
  --
  -- alice is at 0 life and it is her turn, with nothing yet settled. bob
  -- wants out.
  --
  --   * conceding BEFORE the settle: bob leaves immediately (CR 104.3a),
  --     alice's opponents have all left, and CR 104.2a hands her the win --
  --     "immediately and overrides all effects that would preclude that
  --     player from winning the game", her own 0 life included.
  --   * conceding at bob's NEXT priority: settleForPriority runs first,
  --     CR 104.3b takes alice at 0 life, and bob wins before he is ever
  --     asked.
  --
  -- Same start state, two different winners, differing only in WHEN the
  -- concession lands. This pins both arms rather than picking one: no set
  -- of poll points a pull channel can offer is "any time", so widening the
  -- poll would move this race rather than end it.
  Spec.it s "#144 conceding before the settle window and after it name different winners" $ do
    let base = (Setup.emptyGame S.bothPlayers) {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice}
        gs = base {GameState.players = Map.adjust (\p -> p {Player.life = 0}) S.alice (GameState.players base)}
        -- The engine as it stands. concedeOrderAnswer records every Concede
        -- ask, so the test does not have to take the narrowness of the
        -- window on trust: bob is asked nothing at all.
        ((_, atOwnPriority), asks) = State.runState (Engine.runGame (concedeOrderAnswer S.bob) gs Engine.runStep) []
        -- The counterfactual, reached through the same door the engine
        -- uses (Departure.leaveGame IS priorityLoop's concede arm): bob's
        -- concession lands before the settle instead of after it.
        conceded = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.bob)
        beforeSettle = S.runPure (concedeAnswer S.bob) conceded Engine.runStep
    Spec.assertEqWith s "the settle got there first: nobody was ever asked" asks []
    Spec.assertEqWith s "CR 104.3b takes alice first, so bob wins" (GameState.result atOwnPriority) (Just (Result.Won S.bob))
    Spec.assertEqWith s "CR 104.2a: had bob got out first, alice wins" (GameState.result beforeSettle) (Just (Result.Won S.alice))
    Spec.assertBool s (GameState.result atOwnPriority /= GameState.result beforeSettle) "the race decides the game, not its timing"

  Spec.it s "CR 104.3a concede does not use the stack: a spell on it never resolves" $ do
    -- A Lightning Bolt is on the stack targeting nothing in particular. alice
    -- concedes at her priority; the game ends without the stack resolving.
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (spellId, base) = S.spellOnStack lightningBolt S.alice (Setup.emptyGame S.bothPlayers)
        gs =
          base
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice
            }
        after = S.runPure (concedeAnswer S.alice) gs Engine.runStep
    Spec.assertEqWith s "bob wins" (GameState.result after) (Just (Result.Won S.bob))
    Spec.assertEqWith s "the spell never left the stack" (GameState.stack after) [spellId]

-- M5.6a / CR 800.4: the turn order and priority control flow around a departure
-- that does NOT end the game. Most cases here need three seats: at two players
-- a departure ends the game before the next thing happens, which is exactly why
-- these defects survived five milestones. One case below is the exception on
-- purpose -- it needs exactly two, to reach a state three seats cannot.
--
-- S.identityAnswer returns [] for Prompt.DeclareAttackers, so it cannot tell a
-- SKIPPED prompt from an ANSWERED one that legally attacks with nothing.
-- Records the PlayerId of every DeclareAttackers ask and attacks with
-- everything offered. The ASK is the observable, not the resulting attackers
-- map: once CR 800.4a removes a departed player's permanents there is nothing
-- for them to attack with, so an empty attackers map alone is no longer
-- evidence that the CR 703.4i guard fired (a two-player board sidesteps that
-- narrowing entirely -- see the load-bearing case below). No new Prompt
-- constructor lands in this phase, so the wildcard keeps this answerer out of
-- the -Werror exhaustiveness net.
declareAttackersAskAnswer :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
declareAttackersAskAnswer p = case p of
  Prompt.DeclareAttackers _ pid candidates -> do
    State.modify' (\seen -> seen <> [pid])
    pure candidates
  _ -> pure (S.identityAnswer p)

-- The single activated ability of a printing. Total: the fallback is
-- unreachable in this fixture. Duplicated per this suite's convention of
-- group-local helpers (CostSpec, ActivateSpec, ReplacementSpec each carry
-- their own).
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card
theAbility p = case Face.activatedAbilities (S.combinedFace p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Face.spell (S.combinedFace p)) ActivationTiming.AnyTime

-- Records every player asked Prompt.Concede, in order -- the
-- concedeOrderAnswer shape -- and drives alice through exactly one Activate
-- of Greed's ability, then Pass for everyone (including alice, on any later
-- ask: by then she can no longer afford the cost, so Greed's Activate is not
-- offered). Prompt.Concede is asked of the priority holder before anything
-- else, so the recorded order IS the order priority moved in -- including a
-- stale holder who has already departed, which is exactly what the CR 800.4a
-- guard below is for.
greedThenPassAnswer :: ObjectId.ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card -> Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
greedThenPassAnswer greedId ability p = case p of
  Prompt.Concede asked -> do
    State.modify' (\seen -> seen <> [asked])
    pure Concession.Continues
  Prompt.ChooseAction _ pid actions ->
    pure (if pid == S.alice && List.elem (A.Activate greedId ability) actions then A.Activate greedId ability else A.Pass)
  _ -> pure (S.identityAnswer p)

turnOrderSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
turnOrderSpec s registry = Spec.describe s "TurnOrder (CR 800.4)" $ do
  Spec.it s "CR 800.4k a departed player's turn does not begin" $ do
    let gone = Departure.depart Departure.Type.Conceded S.bob S.threePlayerGame
        after = S.runPure S.identityAnswer gone Engine.handoffTurn
    Spec.assertEqWith s "carol is active, bob is skipped" (GameState.activePlayer after) S.carol
    Spec.assertEqWith s "exactly one turn began" (GameState.turnNumber after) 2
    Spec.assertEqWith s "the seating roster is untouched" (GameState.turnOrder after) [S.alice, S.bob, S.carol]

  Spec.it s "CR 800.4k the walk is a loop: two consecutive departed seats are both skipped" $ do
    -- Proves the walk keeps walking rather than skipping exactly one seat.
    -- With bob and carol both gone, alice takes consecutive turns.
    let gone =
          Departure.depart
            Departure.Type.Conceded
            S.carol
            (Departure.depart Departure.Type.Conceded S.bob S.threePlayerGame)
        after = S.runPure S.identityAnswer gone Engine.handoffTurn
    Spec.assertEqWith s "alice takes the next turn too" (GameState.activePlayer after) S.alice
    Spec.assertEqWith s "and it is a new turn" (GameState.turnNumber after) 2

  Spec.it s "the seat walk terminates when every seat has departed" $ do
    -- Totality, pinned. A game with no survivors already has a Result, so
    -- this is unreachable while the game is running -- but the walk is
    -- bounded by the seat count and falls back rather than looping or
    -- reaching for a partial head.
    let gone =
          Departure.depart
            Departure.Type.Conceded
            S.carol
            ( Departure.depart
                Departure.Type.Conceded
                S.bob
                (Departure.depart Departure.Type.Conceded S.alice S.threePlayerGame)
            )
        after = S.runPure S.identityAnswer gone Engine.handoffTurn
    Spec.assertEqWith s "the active player is unchanged" (GameState.activePlayer after) S.alice
    Spec.assertEqWith s "no turn began" (GameState.turnNumber after) 1

  Spec.it s "CR 800.4b a pending control whose decider has left is not promoted" $ do
    -- bob Mindslavered carol; bob then left the game. When carol's turn
    -- begins, she is NOT controlled: "If a player would be controlled by a
    -- player who has left the game, they aren't."
    let armed = S.threePlayerGame {GameState.pendingControl = Map.singleton S.carol (Decider.MkDecider S.bob)}
        gone = Departure.depart Departure.Type.Conceded S.bob armed
        after = S.runPure S.identityAnswer gone Engine.handoffTurn
    Spec.assertEqWith s "CR 800.4a's second clause already cleared it at bob's departure" (GameState.pendingControl gone) Map.empty
    Spec.assertEqWith s "carol's turn began" (GameState.activePlayer after) S.carol
    Spec.assertEqWith s "she is uncontrolled" (GameState.activeControl after) Nothing
    Spec.assertEqWith s "and the stale entry is gone" (GameState.pendingControl after) Map.empty

  Spec.it s "CR 800.4b the promotion guard stands on its own: an entry armed AFTER the departure is still not promoted" $ do
    -- CR 800.4a's second clause now clears pendingControl at the moment its
    -- decider leaves, so the sibling case above is satisfied by two rules at
    -- once. This one isolates CR 800.4b's last sentence -- "If a player would
    -- be controlled by a player who has left the game, they aren't" -- by
    -- arming the entry after bob has already gone, a state CR 800.4a makes
    -- unreachable in play and which therefore only the promotion guard can
    -- answer for.
    let gone = Departure.depart Departure.Type.Conceded S.bob S.threePlayerGame
        armed = gone {GameState.pendingControl = Map.singleton S.carol (Decider.MkDecider S.bob)}
        after = S.runPure S.identityAnswer armed Engine.handoffTurn
    Spec.assertEqWith s "carol's turn began" (GameState.activePlayer after) S.carol
    Spec.assertEqWith s "she is uncontrolled" (GameState.activeControl after) Nothing
    Spec.assertEqWith s "and the stale entry is gone" (GameState.pendingControl after) Map.empty

  Spec.it s "CR 723.1b a pending control whose decider is still playing IS promoted" $ do
    -- The control assertion: the same board with bob still in the game. If
    -- this passed either way, the case above would prove nothing.
    let armed = S.threePlayerGame {GameState.pendingControl = Map.singleton S.bob (Decider.MkDecider S.alice)}
        after = S.runPure S.identityAnswer armed Engine.handoffTurn
    Spec.assertEqWith s "bob's turn began" (GameState.activePlayer after) S.bob
    Spec.assertEqWith s "alice controls him" (GameState.activeControl after) (Just (Decider.MkDecider S.alice))

  Spec.it s "CR 800.4a nextStillPlaying finds the successor of a player who has ALREADY departed" $ do
    -- The unit-level statement of #143's first half. Bob's seat is looked up
    -- in the FULL seating order, so his successor is carol -- not alice, the
    -- head of the filtered list.
    let gone = Departure.depart Departure.Type.Conceded S.bob S.threePlayerGame
    Spec.assertEqWith s "bob's successor is carol" (Engine.nextStillPlaying gone S.bob) S.carol
    Spec.assertEqWith s "carol's successor is alice (wraps)" (Engine.nextStillPlaying gone S.carol) S.alice
    Spec.assertEqWith s "alice's successor skips departed bob" (Engine.nextStillPlaying gone S.alice) S.carol

  Spec.it s "CR 800.4a/117.4 priority after a concede goes to the next seat, and the pass cycle restarts" $ do
    -- Both halves of #143 in one exact list, which is why it is asserted in
    -- full rather than with `take`:
    --
    --   * The THIRD ask proves the seat lookup. Alice passes, bob concedes at
    --     his own priority, and CR 800.4a's last sentence hands priority to
    --     CAROL -- the seat after bob's. Before the fix, nextStillPlaying
    --     could not find already-departed bob in the filtered order and fell
    --     through to alice, the head of the list.
    --   * The FOURTH ask proves the `passes` reset. CR 117.4 needs "all
    --     players pass without taking any actions in between"; bob's
    --     concession is an action, so alice's earlier pass no longer counts
    --     and BOTH survivors must pass again. Without the reset the list is
    --     three asks long -- carol's single pass would reach
    --     `passes >= length (stillPlaying gs)` and end the step.
    let gs = S.threePlayerGame {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice}
        ((_, after), asks) = State.runState (Engine.runGame (concedeOrderAnswer S.bob) gs Engine.priorityLoop) []
    Spec.assertEqWith s "alice, then bob, then CAROL, then alice again" asks [S.alice, S.bob, S.carol, S.alice]
    Spec.assertEqWith s "bob departed by conceding" (fmap Player.status (Map.lookup S.bob (GameState.players after))) (Just (Status.Departed Departure.Type.Conceded))
    Spec.assertEqWith s "the game continues" (GameState.result after) Nothing

  Spec.it s "CR 800.4j the active player having left does not stop the turn: priority starts at the next seat" $ do
    -- Alice left during her own turn. The turn continues to its completion
    -- WITHOUT an active player: alice is never asked, bob is asked first.
    let gone = Departure.depart Departure.Type.Conceded S.alice (S.threePlayerGame {GameState.phase = Phase.PrecombatMain})
        ((_, after), asks) = State.runState (Engine.runGame (concedeOrderAnswer S.carol) gone Engine.priorityLoop) []
    Spec.assertEqWith s "priorityHolder skips the departed active player" (Engine.priorityHolder gone) S.bob
    Spec.assertEqWith s "bob is asked first, alice never" (take 2 asks) [S.bob, S.carol]
    Spec.assertBool s (notElem S.alice asks) "alice was never asked"
    Spec.assertEqWith s "the turn still belongs to alice's seat" (GameState.activePlayer after) S.alice

  Spec.it s "CR 800.4j after a resolution, priority returns to the next seat, not the departed active player" $ do
    -- The second site: the post-resolution re-grant. Carol's Goblin Piker is
    -- on the stack, alice (the active player) has left. Bob and carol pass,
    -- the Piker resolves, and priority is re-granted to BOB. Without
    -- priorityHolder the third ask would be alice's.
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, withSpell) = S.spellOnStack piker S.carol S.threePlayerGame
        gone = Departure.depart Departure.Type.Conceded S.alice (withSpell {GameState.phase = Phase.PrecombatMain})
        ((_, after), asks) = State.runState (Engine.runGame (concedeOrderAnswer S.alice) gone Engine.priorityLoop) []
    Spec.assertEqWith s "bob, carol, then bob AGAIN -- never alice" asks [S.bob, S.carol, S.bob, S.carol]
    Spec.assertEqWith s "the spell resolved" (length (GameState.stack after)) 0
    Spec.assertEqWith s "carol's Piker is on the battlefield" (S.creaturesInPlay S.carol after) 1

  Spec.it s "CR 800.4j/703.4d a departed active player does not draw" $ do
    -- With no library, a draw flags drewFromEmpty -- so "did not draw" is
    -- directly observable without building a deck.
    let base = S.threePlayerGame {GameState.phase = Phase.Beginning BeginningStep.DrawStep, GameState.turnNumber = 2}
        gone = Departure.depart Departure.Type.Conceded S.alice base
        after = S.runPure S.identityAnswer gone S.drawStep
        control = S.runPure S.identityAnswer base S.drawStep
    Spec.assertBool s (not (Set.member S.alice (GameState.drewFromEmpty after))) "a departed active player never attempts the draw"
    Spec.assertBool s (Set.member S.alice (GameState.drewFromEmpty control)) "but a playing one does -- the guard is what did it"

  Spec.it s "CR 800.4j/703.4c a departed active player's untap step does nothing" $ do
    -- landPlayed is the observable part of the untap step that needs no
    -- permanents: it is cleared for the active player each untap.
    let base =
          S.threePlayerGame
            { GameState.phase = Phase.Beginning BeginningStep.Untap,
              GameState.landPlayed = Set.singleton S.alice
            }
        gone = Departure.depart Departure.Type.Conceded S.alice base
        after = S.runPure S.identityAnswer gone (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))
        control = S.runPure S.identityAnswer base (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))
    Spec.assertEqWith s "no untap-step action happened" (GameState.landPlayed after) (Set.singleton S.alice)
    Spec.assertEqWith s "a playing active player's land play IS cleared" (GameState.landPlayed control) Set.empty

  Spec.it s "CR 800.4j/703.4i a departed active player is not asked to declare attackers" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    -- The board sits at the declare attackers step, so CR 703.4h has already
    -- settled the defending player and it has to say who: alice is active, so
    -- bob is her first candidate (CR 506.2a). Without it Combat.defender is
    -- Nothing, no attack is possible for either run, and askedControl below
    -- would be [] for a reason that has nothing to do with the CR 800.4j guard.
    let seated =
          S.threePlayerGame
            { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
              GameState.combat = (GameState.combat S.threePlayerGame) {Combat.Type.defender = Just S.bob}
            }
        (pikerId, board) = S.addCreature piker S.alice seated
        gone = Departure.depart Departure.Type.Conceded S.alice board
        ((_, after), askedAfter) = State.runState (Engine.runGame declareAttackersAskAnswer gone (Engine.runTurnBasedActions (Phase.Combat CombatStep.DeclareAttackers))) []
        ((_, control), askedControl) = State.runState (Engine.runGame declareAttackersAskAnswer board (Engine.runTurnBasedActions (Phase.Combat CombatStep.DeclareAttackers))) []
    -- At three seats, CR 800.4a has already stripped her battlefield by the
    -- time this step runs -- checked directly below, not inferred from the
    -- ask -- so askedAfter == [] is over-determined: it holds with or
    -- without the hasActive guard in Engine.runTurnBasedActions, because
    -- Combat.declareAttackers has nothing to offer her regardless. The
    -- sibling case that isolates the guard itself needs a board CR 800.4a
    -- does NOT empty, which only happens at two seats -- see "the
    -- declare-attackers guard is load-bearing at two seats" below.
    Spec.assertEqWith s "CR 800.4a: her Piker left the game with her" (Game.lookupObject pikerId gone) Nothing
    Spec.assertEqWith s "so there is nothing left to ask her about" askedAfter []
    Spec.assertEqWith s "a playing active player WITH a real candidate IS asked" askedControl [S.alice]
    Spec.assertBool s (Map.null (Combat.Type.attackers (GameState.combat after))) "and nothing attacked"
    Spec.assertBool s (not (Map.null (Combat.Type.attackers (GameState.combat control)))) "while the control run did attack"

  Spec.it s "CR 800.4j/703.4i the declare-attackers guard is load-bearing at two seats, where the game loop cannot reach this state" $ do
    -- At two seats Departure.continuesAfterDeparture is False (CR 800.1's
    -- "more than two players"), so NONE of CR 800.4a's four clauses run:
    -- alice keeps her Piker, and Projection.controls still names her its
    -- controller -- checked directly below, the mirror of the three-seat
    -- case's Nothing. That state is unreachable through actual play: CR
    -- 104.2a ends a two-player game the instant alice leaves it, so
    -- Engine.priorityLoop and Engine.runStep can never call
    -- runTurnBasedActions for her again. This case calls it directly
    -- instead, the same way the CR 800.4b sibling further above isolates
    -- the promotion guard for a state CR 800.4a makes unreachable in play.
    -- Deleting the hasActive guard here does not just change what is
    -- asked -- her Piker actually attacks.
    --
    -- The stated defending player is what keeps that load-bearing: a board at
    -- the declare attackers step with Combat.defender still Nothing cannot
    -- attack at all (no attack is possible), so deleting the guard would leave
    -- this case green for the wrong reason. alice is active, so bob defends
    -- (CR 506.2's second sentence).
    piker <- S.printingOf s registry "Goblin Piker"
    let seated =
          (Setup.emptyGame S.bothPlayers)
            { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
              GameState.combat = (GameState.combat (Setup.emptyGame S.bothPlayers)) {Combat.Type.defender = Just S.bob}
            }
        (pikerId, board) = S.addCreature piker S.alice seated
        gone = Departure.depart Departure.Type.Conceded S.alice board
        ((_, after), askedAfter) = State.runState (Engine.runGame declareAttackersAskAnswer gone (Engine.runTurnBasedActions (Phase.Combat CombatStep.DeclareAttackers))) []
    Spec.assertEqWith s "she still controls her Piker -- CR 800.4a never ran at two seats" (Projection.controls S.alice gone) [pikerId]
    Spec.assertEqWith s "the guard is what keeps her from being asked, not an empty board" askedAfter []
    Spec.assertBool s (Map.null (Combat.Type.attackers (GameState.combat after))) "and nothing attacked"

  Spec.it s "M5.6a gate: a three-player game survives a concede, and the departed seat still ends its durations" $ do
    -- Alice, bob, carol. Bob arms an "until your next turn" player effect
    -- (Silence's shape: an ActivePlayerEffect with Expiry.AtTurnOf bob), then
    -- concedes mid-cycle at his own priority.
    --
    --   * CR 800.4a: priority passes to CAROL, the next seat still in the
    --     game -- not alice, the head of the order.
    --   * CR 104.2a: two survivors, so there is no result and the step runs
    --     to its end.
    --   * CR 800.4k: bob's turn does not begin at the handoff.
    --   * CR 800.4m: bob's effect ends at the seat where his turn WOULD have
    --     begun -- not when he left, and not never.
    let armed =
          S.addPlayerEffect
            (Expiry.Type.AtTurnOf S.bob)
            PlayerScope.Opponents
            PlayerEffect.Type.CantCastSpells
            S.bob
            (S.threePlayerGame {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice})
        ((_, afterStep), asks) = State.runState (Engine.runGame (concedeOrderAnswer S.bob) armed Engine.runStep) []
        afterHandoff = S.runPure S.identityAnswer afterStep Engine.handoffTurn
    Spec.assertEqWith s "carol received priority after bob's concession" asks [S.alice, S.bob, S.carol, S.alice]
    Spec.assertEqWith s "bob departed by conceding" (fmap Player.status (Map.lookup S.bob (GameState.players afterStep))) (Just (Status.Departed Departure.Type.Conceded))
    Spec.assertEqWith s "the game continues" (GameState.result afterStep) Nothing
    Spec.assertBool s (GameState.phase afterStep /= Phase.PrecombatMain) "the step ran to its end and advanced"
    Spec.assertEqWith s "bob's effect did NOT end when he left" (length (GameState.playerEffects afterStep)) 1
    Spec.assertEqWith s "carol takes the next turn, bob's seat is skipped" (GameState.activePlayer afterHandoff) S.carol
    Spec.assertEqWith s "and bob's effect ended at bob's seat -- not never" (GameState.playerEffects afterHandoff) []

  Spec.it s "CR 800.4a a player who departs paying a cost is not asked again" $ do
    -- Alice controls Greed and one Swamp at exactly 2 life. She activates
    -- Greed ({B}, Pay 2 life: Draw a card -- CR 119.4 makes paying her last
    -- 2 life legal), which leaves her at 0 life. Every Play/Cast/Activate
    -- arm writes GameState.priority BEFORE settleForPriority runs the SBA
    -- pass that departs her (CR 704.5a), so without the CR 800.4a guard the
    -- next `loop` iteration would find `priority = Just alice` and ask the
    -- departed alice again. With the guard it re-derives the holder via
    -- nextStillPlaying and asks bob, the next seat still in the game,
    -- instead.
    swamp <- S.printingOf s registry "Swamp"
    greed <- S.printingOf s registry "Greed"
    let (_, withSwamp) = S.addCreature swamp S.alice S.threePlayerGame
        (greedId, withGreed) = S.addCreature greed S.alice withSwamp
        gs =
          withGreed
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice,
              GameState.players = Map.adjust (\p -> p {Player.life = 2}) S.alice (GameState.players withGreed)
            }
        ((_, after), asks) = State.runState (Engine.runGame (greedThenPassAnswer greedId (theAbility greed)) gs Engine.priorityLoop) []
    Spec.assertEqWith s "alice activates Greed, then BOB is asked -- not alice again" (take 2 asks) [S.alice, S.bob]
    Spec.assertBool s (notElem S.alice (drop 1 asks)) "alice is never asked again after departing"
    Spec.assertEqWith
      s
      "alice departed by paying her last 2 life"
      (fmap Player.status (Map.lookup S.alice (GameState.players after)))
      (Just (Status.Departed Departure.Type.Lost))

  Spec.it s "M5.6d gate: attacking the monarch takes the crown and frees Palace Jailer's prisoner; attacking the other opponent does neither" $ do
    -- bob's Palace Jailer has entered: its first ETB made bob the monarch, and
    -- its second exiled carol's Piker until an opponent becomes the monarch.
    -- Carol's is the ONLY creature on the board when that ETB resolves, so the
    -- target is forced and the test does not depend on id ordering. alice's
    -- attacker is added afterwards.
    --
    -- CR 725.2: "Whenever a creature deals combat damage to the monarch, its
    -- controller becomes the monarch."
    --
    -- Palace Jailer's Gatherer ruling (2021-03-19) is why the prisoner is
    -- watching for a new monarch rather than for the Jailer: "Palace Jailer
    -- leaving the battlefield won't cause the exiled creature to return. The
    -- game will continue to watch for the next time an opponent becomes the
    -- monarch."
    piker <- S.printingOf s registry "Goblin Piker"
    palaceJailer <- S.printingOf s registry "Palace Jailer"
    let (victim, g1) = S.addCreature piker S.carol S.threePlayerGame
        (jailer, g2) = S.addCreature palaceJailer S.bob g1
        entered = ZoneChange.MkZoneChange jailer jailer Zone.Stack Zone.Battlefield
        g3 = S.withEvents [GameEvent.Moved entered (Projection.project jailer g2)] g2
        resolved = S.runPure S.identityAnswer (S.runPure S.identityAnswer g3 Engine.settleForPriority) Engine.priorityLoop
        -- CR 400.7: exiling the target gives it a new object identity, so the
        -- watch Palace Jailer's second ETB registers is keyed to a NEW id, not
        -- `victim`'s battlefield id. `victim` is kept only as a deterministic,
        -- total fallback -- unreachable once the size assertion below holds --
        -- so no partial function is needed to find it.
        prisoner = Maybe.fromMaybe victim (Maybe.listToMaybe (Map.keys (GameState.exiledUntilMonarch resolved)))
        (attacker, g4) = S.addCreature piker S.alice resolved
        board =
          g4
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
              GameState.remaining =
                Seq.fromList
                  [ Phase.Combat CombatStep.DeclareAttackers,
                    Phase.Combat CombatStep.DeclareBlockers,
                    Phase.Combat CombatStep.CombatDamage,
                    Phase.Combat CombatStep.EndOfCombat,
                    Phase.PostcombatMain
                  ]
            }
        -- Declines blocks rather than delegating to S.aggressiveAnswer's
        -- DeclareBlockers arm: in run A bob himself is the defending player,
        -- and his only creature is the Jailer, which aggressiveAnswer would
        -- happily throw in front of alice's attacker. That would zero out
        -- the combat damage to bob for a reason CR 509.1 (only the
        -- DEFENDING player declares blockers -- Task 4's own case, not this
        -- gate's) already covers -- not evidence about the chosen defender.
        -- Declining blocks keeps this test's damage assertions about the
        -- ONE thing it is meant to discriminate.
        attackTo who p = case p of
          Prompt.DeclareBlockers {} -> Map.empty
          _ -> S.attackTo who p
        hitBob = S.runCombat (attackTo S.bob) board
        hitCarol = S.runCombat (attackTo S.carol) board
    -- The fixture really is what the test claims.
    Spec.assertEqWith s "bob is the monarch before combat" (GameState.monarch board) (Just S.bob)
    Spec.assertEqWith s "exactly one creature is under the watch" (Map.size (GameState.exiledUntilMonarch board)) 1
    Spec.assertEqWith s "carol's Piker left the battlefield" (S.creaturesInPlay S.carol board) 0
    Spec.assertBool s (Combat.canAttack S.alice attacker board) "alice has an attacker"
    -- Run A: alice attacks the monarch.
    Spec.assertEqWith s "bob took 2" (S.lifeOf S.bob hitBob) (Just 18)
    Spec.assertEqWith s "alice is the monarch" (GameState.monarch hitBob) (Just S.alice)
    -- The next two assertions are ENTAILED by "alice is the monarch" just
    -- above: an entry is due iff the crown has CHANGED HANDS since the watch
    -- last looked and the new monarch is not the entry's controller (#171),
    -- and this combat is exactly such a change -- carol held the crown when
    -- the watch was armed, and alice has taken it from her. So carol's entry
    -- is due and nothing consistent with that fact can leave it
    -- undischarged. They are not a second, independent observation that the
    -- crown moved; what they add is real coverage of the return machinery
    -- itself (Resolve's ExileUntilMonarch arm, Event.changeZoneReturning,
    -- and this settle-loop return) running inside a full
    -- Engine.runStep-driven combat.
    Spec.assertEqWith s "the watch is discharged" (GameState.exiledUntilMonarch hitBob) Map.empty
    -- CR 400.7: the return is itself a zone change, so the returned
    -- permanent has yet another new object id -- `prisoner`'s id (the one it
    -- held while exiled) never appears on any battlefield. Carol's creature
    -- COUNT is what survives across that identity reset.
    Spec.assertEqWith s "and the prisoner is back on the battlefield" (S.creaturesInPlay S.carol hitBob) 1
    -- Run B: alice attacks the other opponent. Same board, same interpreter
    -- shape, one different answer. Unreachable under the head-of-list
    -- behaviour, which is what makes the pair evidence.
    Spec.assertEqWith s "carol took 2" (S.lifeOf S.carol hitCarol) (Just 18)
    Spec.assertEqWith s "bob was untouched, so he keeps the crown" (S.lifeOf S.bob hitCarol) (Just 20)
    Spec.assertEqWith s "bob is still the monarch" (GameState.monarch hitCarol) (Just S.bob)
    -- Entailed by "bob is still the monarch" just above, for the same
    -- reason as run A's pair: real coverage of the same return code path,
    -- not independent evidence that the crown followed the chosen defender.
    Spec.assertBool s (Map.member prisoner (GameState.exiledUntilMonarch hitCarol)) "the watch still stands"
    Spec.assertEqWith s "and the prisoner is still exiled, not back on carol's battlefield" (S.creaturesInPlay S.carol hitCarol) 0
    Spec.assertEqWith s "neither run ended the game" (GameState.result hitBob, GameState.result hitCarol) (Nothing, Nothing)

-- #219: Action.legalActions computes what is legal; the priority loop must not
-- act on anything else. Every gate the enumeration applies -- the controller
-- check, CR 302.6's tap-sickness gate, cost payability, CR 307.5 timing -- is
-- only as strong as this.
trustedActionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
trustedActionSpec s registry = Spec.describe s "TrustedActions" $ do
  -- CR 302.6: a summoning-sick creature's {T} ability cannot be activated.
  -- legalActions already refuses to offer it; this pins that NAMING it anyway
  -- does not work either.
  Spec.it s "#219 an activation that was never offered is refused" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (srcId, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
        -- Summoning sick, so the ability is genuinely illegal to activate.
        sick = g0 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) srcId (GameState.objects g0), GameState.priority = Just S.alice}
        ability = theAbility prodigalSorcerer
        offered = Action.legalActions S.alice sick
        after = snd (State.evalState (Engine.runGame (illegalActivationAnswer srcId ability) sick Engine.priorityLoop) False)
    Spec.assertBool s (notElem (A.Activate srcId ability) offered) "the engine really does not offer it"
    Spec.assertEqWith s "and naming it puts nothing on the stack" (GameState.stack after) []
    Spec.assertEqWith s "the Sorcerer is not tapped" (fmap Object.tapped (Game.lookupObject srcId after)) (Just TapState.Untapped)

  -- The control: the SAME interpreter, the same board, but settled -- so the
  -- activation is legal and must go through. Without this, a guard that
  -- refused every activation would pass the test above.
  Spec.it s "#219 the same activation IS honoured once it is legal" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let (srcId, g0) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
        settled = S.runPure S.identityAnswer g0 (Engine.settleAll S.alice)
        ready = settled {GameState.priority = Just S.alice}
        ability = theAbility prodigalSorcerer
        after = snd (State.evalState (Engine.runGame (illegalActivationAnswer srcId ability) ready Engine.priorityLoop) False)
    Spec.assertBool s (elem (A.Activate srcId ability) (Action.legalActions S.alice ready)) "it is offered now"
    Spec.assertEqWith s "so the Sorcerer taps" (fmap Object.tapped (Game.lookupObject srcId after)) (Just TapState.Tapped)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Game" $ do
  gameSpec s registry
  actionSpec s registry
  objectFactSpec s registry
  engineSpec s registry
  ruleSpec s registry
  restartReentrySpec s registry
  cleanupStepSpec s registry
  concedeSpec s registry
  turnOrderSpec s registry
  trustedActionSpec s registry

-- One Lightning Bolt in bob's hand.
handBobBolt :: Printing.Printing -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
handBobBolt lightningBolt gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = S.bob,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfCard lightningBolt,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled S.bob,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing
          }
   in (oid, gs2 {GameState.objects = Map.insert oid obj (GameState.objects gs2), GameState.hand = Map.insert S.bob (Seq.singleton oid) (GameState.hand gs2)})

namedIs :: CardName.CardName -> Maybe Object.Object -> Bool
namedIs wanted mo = case mo of
  Just o -> case Object.source o of
    Source.OfCard printing -> Face.name (S.combinedFace printing) == wanted
    Source.OfToken card -> S.nameOf card == wanted
    Source.OfAbility _ _ -> False
    Source.OfTrigger _ _ -> False
    Source.OfEmblem _ -> False
    Source.OfInherentTrigger _ _ -> False
  Nothing -> False

-- The controller's strategy: when asked to decide for bob (the CONTROLLED player,
-- routed because the prompt's Decider is alice), cast the Bolt at bob; otherwise
-- pass. A naive engine that ignored control would send the prompt with Decider =
-- bob, this interpreter would pass, and bob's life would stay 20 -- the falsifier.
slaveAnswer :: Prompt.Prompt r -> r
slaveAnswer p = case p of
  Prompt.ChooseAction (Decider.MkDecider d) player actions ->
    if player == S.bob && d == S.alice
      then case filter isCastAction actions of
        h : _ -> h
        [] -> A.Pass
      else A.Pass
  Prompt.ChooseTargets _ _ _ sets ->
    Map.mapMaybe
      (\s -> if Set.member (Recipient.ToPlayer S.bob) s then Just (Recipient.ToPlayer S.bob) else Set.lookupMin s)
      sets
  Prompt.Shuffle ids -> ids
  Prompt.RandomFirstPlayer order -> NonEmpty.head order
  Prompt.Concede _ -> Concession.Continues
  Prompt.ChooseDiscard _ _ ids n -> List.genericTake n ids
  Prompt.ChooseDefender _ _ candidates -> NonEmpty.head candidates
  -- Head is enough here: this interpreter exists to prove the DECIDER is honoured
  -- for ChooseAction under Mindslaver, and which land pays a cost is not part of
  -- that. Placed with the other incidental arms rather than above ChooseAction,
  -- so the one arm that reads the Decider stays first and legible.
  Prompt.ChooseManaSource _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseManaYield _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseProliferate {} -> (Set.empty, Set.empty)
  Prompt.ChooseLegend _ _ candidates -> NonEmpty.head candidates
  Prompt.DeclareAttackers {} -> []
  Prompt.ChooseAttackTarget _ _ _ options -> NonEmpty.head options
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter S.isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseLandTypeSwap {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.ChooseCreatureTypeSwap {} -> (Subtype.Frog, Subtype.Frog)
  Prompt.SearchLibrary {} -> Nothing
  Prompt.CastWhileSearching {} -> Nothing
  Prompt.ChooseX {} -> 0
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (List.genericTake count (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.ChooseColor {} -> Color.White
  Prompt.ChooseBasicLandType {} -> Subtype.Mountain
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  Prompt.OrderDamage _ _ events -> zipWith const [0 ..] events
  Prompt.ChooseReplacement {} -> 0
  Prompt.ChooseBoundToken _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseAttachment _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (List.genericTake count candidates)
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> List.genericTake count hand
  Prompt.MulliganAction {} -> Nothing
  Prompt.OpeningHandAction {} -> Nothing
  -- CR 603.5: declining a printed "may" is the least-eventful answer.
  Prompt.ChooseOptional {} -> OptionalDecision.Declines
  -- CR 118.13a: the head is a legal answer -- every offered route is payable --
  -- and is the least eventful default, matching Replay.defaultAnswer.
  Prompt.AnnouncePhyrexianPayment _ _ _ _ offers -> NonEmpty.head offers
  -- CR 702.42a: declining entwine is always legal, costs nothing and changes
  -- no mode, the least-eventful default (mirrors ChooseOptional -> Declines).
  Prompt.ChooseEntwine {} -> EntwineDecision.Declines

-- CR 723.5 combat: alice, controlling bob, declares bob's attackers. Attackers
-- are declared only when the prompt's Decider is alice for player bob; a naive
-- engine that sent the prompt with Decider = bob would fall to `[]` and no one
-- would attack. Damage from the lone unblocked attacker goes to its sole
-- recipient (the defending player, alice). Everything else delegates to
-- slaveAnswer (blocks: none; priority: pass).
controlCombatAnswer :: Prompt.Prompt r -> r
controlCombatAnswer p = case p of
  Prompt.DeclareAttackers (Decider.MkDecider d) player attackers ->
    if d == S.alice && player == S.bob then attackers else []
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case Map.keys thresholds of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  _ -> slaveAnswer p

-- An interpreter that names an action it was never offered. CR-wise this is not
-- a rules question at all: Action.legalActions computes what is legal, and the
-- priority loop must not act on anything else (#219). pawl's boundary is an
-- interpreter that may be an LLM, and a plausible-looking illegal action is
-- exactly what such an interpreter produces.
-- Stateful, and it must be: an answerer that returns Activate for EVERY
-- ChooseAction never lets the priority loop terminate. This one names the
-- activation once and passes forever after, which is also the honest model of an
-- interpreter that tries something illegal and then gives up.
illegalActivationAnswer :: ObjectId.ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card -> Prompt.Prompt r -> State.State Bool r
illegalActivationAnswer oid ability p = case p of
  Prompt.ChooseAction {} -> do
    tried <- State.get
    if tried
      then pure A.Pass
      else do
        State.put True
        pure (A.Activate oid ability)
  -- Aim Prodigal Sorcerer's ping at a PLAYER. S.identityAnswer takes the lowest
  -- recipient, which is the Sorcerer itself -- and a 1/1 pinging itself dies, so
  -- the board this test wants to inspect would not exist by the time it looked.
  Prompt.ChooseTargets _ _ _ sets ->
    pure (Map.mapMaybe (\candidates -> pickPlayerRecipient candidates) sets)
  _ -> pure (S.identityAnswer p)

-- The lowest ToPlayer recipient in a legal set, falling back to the lowest of
-- anything when the set holds no player.
isPlayerRecipient :: Recipient.Recipient -> Bool
isPlayerRecipient r = case r of
  Recipient.ToPlayer _ -> True
  Recipient.ToCreature _ -> False
  Recipient.ToPlaneswalker _ -> False
  Recipient.ToObject _ -> False

pickPlayerRecipient :: Set.Set Recipient.Recipient -> Maybe Recipient.Recipient
pickPlayerRecipient candidates =
  case filter isPlayerRecipient (Set.toList candidates) of
    r : _ -> Just r
    [] -> Set.lookupMin candidates

isCastAction :: A.Action -> Bool
isCastAction a = case a of
  A.Cast {} -> True
  _ -> False

-- Is this a legal-action Activate? On the gate board (a Mindslaver plus basic
-- lands, whose mana abilities are intrinsic and never surface as activated
-- abilities) the ONLY Activate action is Mindslaver's, so "the first Activate"
-- is unambiguously Mindslaver's control ability.
isActivateAction :: A.Action -> Bool
isActivateAction a = case a of
  A.Activate _ _ -> True
  _ -> False

-- CR 723 gate strategy. Alice, deciding for herself, fires Mindslaver (the only
-- activation on the board) at bob; once she is bob's decider (CR 723.5, the
-- prompt's Decider is alice while player is bob) she casts bob's Bolt; otherwise
-- pass. Non-ChooseAction prompts (targets, modes, shuffle, ...) delegate to
-- slaveAnswer, which targets bob. A naive engine ignoring control would send
-- bob's ChooseAction with Decider = bob; the else-branch would pass, bob would
-- keep 20 life, and the gate would fail -- the falsifier.
gateAnswer :: Prompt.Prompt r -> r
gateAnswer p = case p of
  Prompt.ChooseAction (Decider.MkDecider d) player actions ->
    case filter isActivateAction actions of
      activation : _ -> activation
      [] ->
        if player == S.bob && d == S.alice
          then case filter isCastAction actions of
            h : _ -> h
            [] -> A.Pass
          else A.Pass
  _ -> slaveAnswer p

-- CR 727 gate strategy. Whoever has priority activates the only activation on the
-- board -- the synthetic restart artifact (bob controls it) -- and otherwise
-- passes. Once the artifact is sacrificed as a cost there is no further
-- activation, so this fires exactly once; after the restart the artifact is in a
-- library, so no player can activate anything and everyone passes to termination.
-- Non-ChooseAction prompts (Shuffle during the rebuild, etc.) delegate to
-- identityAnswer.
restartAnswer :: Prompt.Prompt r -> r
restartAnswer p = case p of
  Prompt.ChooseAction _ _ actions ->
    case filter isActivateAction actions of
      activation : _ -> activation
      [] -> A.Pass
  _ -> S.identityAnswer p

-- CR 729 gate strategy. Whoever has priority casts the only castable spell on the
-- board -- the synthetic subgame sorcery ({0}, in alice's hand) -- and otherwise
-- passes. Inside the subgame the libraries are Mountains (lands are PLAYED, not
-- cast), so no cast is available there and everyone passes to termination (bob
-- decks) -- except the CR 729.6 nested gate, where the level-1 subgame's
-- library also holds a castable nested synthetic-subgame sorcery, and the same
-- cast-if-available strategy descends into it. Because subgame prompts are
-- UNTAGGED, the same answerer serves every level. Non-ChooseAction prompts
-- (Shuffle during setup, etc.) delegate to identityAnswer.
subgameAnswer :: Prompt.Prompt r -> r
subgameAnswer p = case p of
  Prompt.ChooseAction _ _ actions ->
    case filter isCastAction actions of
      cast : _ -> cast
      [] -> A.Pass
  _ -> S.identityAnswer p

-- #136 / CR 729.2: hands the subgame's first-player roll a fixed answer, so a
-- test can play the same fixture with each player starting. Every other prompt
-- delegates to identityAnswer (which would answer this one with the head of the
-- order -- the pre-#136 behaviour).
firstPlayerAnswer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
firstPlayerAnswer starter p = case p of
  Prompt.RandomFirstPlayer _ -> starter
  _ -> S.identityAnswer p

-- Take exactly one mulligan, then keep. CR 103.5c's free first mulligan is only
-- observable if someone actually takes one.
mulliganOnceAnswer :: Prompt.Prompt r -> r
mulliganOnceAnswer p = case p of
  Prompt.DeclareMulligan _ _ offer -> if MulliganOffer.taken offer < 1 then MulliganDecision.Mulligan else MulliganDecision.Keep
  _ -> S.identityAnswer p

-- Records the candidate list of every Prompt.RandomFirstPlayer, rolling the first
-- candidate, and reverses every shuffle. The reversal is what makes "whose
-- library was shuffled" observable at all: S.identityAnswer's Shuffle arm is the
-- identity, so a library that was shuffled looks exactly like one that was not.
subgameRosterAnswer :: Prompt.Prompt r -> State.State [[PlayerId.PlayerId]] r
subgameRosterAnswer p = case p of
  Prompt.RandomFirstPlayer order -> do
    State.modify' (<> [NonEmpty.toList order])
    pure (NonEmpty.head order)
  Prompt.Shuffle ids -> pure (reverse ids)
  _ -> pure (S.identityAnswer p)

addManyG :: Printing.Printing -> Int -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
addManyG mountain n pid gs =
  List.foldl' (\g _ -> snd (S.addCreature mountain pid g)) gs (replicate n ())

-- Put one printing into a player's library as a fresh object; return its id.
-- Mirrors S.addHandCard, then relocates hand -> library.
libraryCard :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
libraryCard printing pid gs =
  let (oid, gs1) = S.addHandCard printing pid gs
      onLibrary o = o {Object.zone = Zone.Library}
   in ( oid,
        gs1
          { GameState.objects = Map.adjust onLibrary oid (GameState.objects gs1),
            GameState.hand = Map.adjust (Seq.filter (/= oid)) pid (GameState.hand gs1),
            GameState.library = Map.insertWith (flip (Seq.><)) pid (Seq.singleton oid) (GameState.library gs1)
          }
      )

-- Append n Mountains to a player's library.
addToLibraryG :: Printing.Printing -> Int -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
addToLibraryG mountain n pid gs =
  List.foldl' (\g _ -> snd (libraryCard mountain pid g)) gs (replicate n ())

-- Move every object this player owns onto their library (mirror of
-- SetupSpec.poolToLibrary, adapted to GameSpec's imports): used to craft a
-- pre-shuffled library of a known size for a subgame/restart gate.
poolToLibraryG :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
poolToLibraryG pid gs =
  let mine = Map.keys (Map.filter (\o -> Object.owner o == pid) (GameState.objects gs))
      onLibrary o = o {Object.zone = Zone.Library}
   in gs
        { GameState.objects = List.foldl' (flip (Map.adjust onLibrary)) (GameState.objects gs) mine,
          GameState.battlefield = Set.difference (GameState.battlefield gs) (Set.fromList mine),
          GameState.library = Map.insert pid (Seq.fromList mine) (GameState.library gs)
        }

-- #134 / CR 727.4. A restart that resolves inside a LIVE Engine.runStep replaces
-- the game underneath the frames that are still running: the resolution, the
-- priority loop, and the step itself. Setup.restartGame leaves the rebuilt state
-- positioned just before turn 1's untap step with no player holding priority, so
-- every one of those frames has to unwind without touching it. The two things
-- that must NOT happen are a further priority grant ("No player has priority")
-- and Engine.advance, which would pop the FRESH `remaining` and skip turn 1's
-- untap step entirely.

-- A player's `n` owned cards, so the rebuilt game can deal a 7-card opening hand
-- without tripping the CR 727.3 short-deck loss.
ownedCards :: Printing.Printing -> Int -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
ownedCards mountain n pid gs =
  List.foldl' (\g _ -> snd (S.addCreature mountain pid g)) gs [1 .. n]

-- alice is active in her precombat main phase (a step that grants priority) with
-- bob's RestartGame ability already on the stack. Both players pass, the ability
-- resolves, and the restart fires mid-step.
restartOnStack :: Printing.Printing -> GameState.GameState
restartOnStack mountain =
  let g0 = Setup.emptyGame S.bothPlayers
      g1 = ownedCards mountain 10 S.alice g0
      g2 = ownedCards mountain 10 S.bob g1
      (abilId, g3) = Game.freshObjectId g2
      (ts, g4) = Game.freshTimestamp g3
      ability =
        ActivatedAbility.MkActivatedAbility
          { ActivatedAbility.cost =
              Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
            ActivatedAbility.modal =
              Modal.MkModal
                (Seq.singleton (Mode.MkMode (Seq.singleton Effect.RestartGame) Map.empty Optionality.Mandatory))
                (ModeSelection.ChooseExactly 1),
            ActivatedAbility.timing = ActivationTiming.AnyTime
          }
      abilObj =
        Object.MkObject
          { Object.owner = S.bob,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfAbility (ObjectId.MkObjectId 0) ability,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled S.bob,
            Object.bindings = Binding.fromChoices Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing
          }
   in g4
        { GameState.objects = Map.insert abilId abilObj (GameState.objects g4),
          GameState.stack = abilId : GameState.stack g4,
          GameState.activePlayer = S.alice,
          GameState.phase = Phase.PrecombatMain,
          GameState.remaining = Seq.fromList [Phase.PostcombatMain, Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]
        }

-- Run under a prompt-counting interpreter: how many times a player was asked to
-- act is exactly how CR 727.4's "No player has priority" is observed.
runCountingActions :: GameState.GameState -> Game.Type.Game a -> (GameState.GameState, Int)
runCountingActions gs act =
  let answer :: Prompt.Prompt r -> State.State Int r
      answer p = do
        case p of
          Prompt.ChooseAction {} -> State.modify' (+ 1)
          _ -> pure ()
        pure (S.identityAnswer p)
      ((_, gs1), n) = State.runState (Engine.runGame answer gs act) 0
   in (gs1, n)

restartReentrySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
restartReentrySpec s registry = Spec.describe s "restart re-entry (CR 727.4)" $ do
  Spec.it s "the step the restart fired in does not advance past turn 1's untap step" $ do
    mountain <- S.printingOf s registry "Mountain"
    let (after, _) = runCountingActions (restartOnStack mountain) Engine.runStep
    Spec.assertEqWith s "still positioned at the untap step" (GameState.phase after) Turn.firstPhase
    Spec.assertEqWith s "the fresh turn schedule is intact, not popped" (GameState.remaining after) Turn.laterPhases
    Spec.assertEqWith s "turn 1 of the new game" (GameState.turnNumber after) 1
    Spec.assertEqWith s "the new game is still being played" (GameState.result after) Nothing

  Spec.it s "no player receives priority after the restart resolves" $ do
    -- alice passes, bob passes, the ability resolves: two ChooseAction prompts
    -- and no more. A third means the priority loop kept running on a game that
    -- no longer exists.
    mountain <- S.printingOf s registry "Mountain"
    let (_, asked) = runCountingActions (restartOnStack mountain) Engine.runStep
    Spec.assertEqWith s "exactly the two passes that resolved the ability" asked 2

  Spec.it s "the next step runs the rebuilt turn 1's untap step" $ do
    mountain <- S.printingOf s registry "Mountain"
    let (afterRestart, _) = runCountingActions (restartOnStack mountain) Engine.runStep
        (afterUntap, _) = runCountingActions afterRestart Engine.runStep
    Spec.assertEqWith s "the untap step ran and handed on to upkeep" (GameState.phase afterUntap) (Phase.Beginning BeginningStep.Upkeep)
    Spec.assertEqWith s "still turn 1" (GameState.turnNumber afterUntap) 1

  Spec.it s "a live playGame survives the restart and plays the new game to a result" $ do
    -- An end-to-end liveness guard, not a discriminating one: the loop did
    -- not wedge before the fix either, it just played a turn 1 with no untap
    -- step. The three tests above are what actually catch that. This one
    -- pins the surrounding claim -- playGame keeps looping across the
    -- rebuild and returns the REBUILT game's result (CR 727.1: no player
    -- wins, loses or draws the game that was restarted). Terminating: the
    -- restart is a hand-built stack object, not a card in any library, so it
    -- cannot fire again and the rebuilt game decks out like any other.
    mountain <- S.printingOf s registry "Mountain"
    let (result, _) = Engine.runGamePure S.identityAnswer (restartOnStack mountain) Engine.playGame
    Spec.assertBool s (case result of Result.Won _ -> True; Result.Drawn -> True) "the new game reached a result"

-- alice is the active player in her cleanup step with `n` cards in hand and
-- nothing else scheduled; `others` are put onto the battlefield under bob's
-- control. Nothing is in either library, so no draw can happen and the only
-- event of the step is CR 514.1's discard.
cleanupBoard :: Printing.Printing -> Int -> [Printing.Printing] -> GameState.GameState
cleanupBoard filler n others =
  let base = List.foldl' (\g p -> snd (S.addCreature p S.bob g)) (Setup.emptyGame S.bothPlayers) others
      full = List.foldl' (\g _ -> snd (S.addHandCard filler S.alice g)) base [1 .. n]
   in full
        { GameState.activePlayer = S.alice,
          GameState.turnNumber = 1,
          GameState.phase = Phase.Ending EndingStep.Cleanup,
          GameState.remaining = Seq.empty
        }

-- CR 514.3a's extra cleanup step and its priority round.
--
-- Megrim, {2}{B} Enchantment: "Whenever an opponent discards a card, this
-- enchantment deals 2 damage to that player." bob controls it, so CR 109.5 fixes
-- its "you" as bob and "an opponent" as alice. alice is the active player with
-- eight cards in hand, so CR 514.1's turn-based discard is an opponent's discard
-- and a triggered ability is waiting DURING the cleanup step -- CR 514.3a's
-- condition exactly.
--
-- The discriminator is alice's life, read after ONE Engine.runStep. Under CR
-- 514.3 alone the trigger was placed by the settle in Engine.advance and then
-- sat on the stack across the handoff, to resolve at bob's first priority: alice
-- still on 20, bob active, turn 2. Under CR 514.3a it resolves inside alice's own
-- cleanup step: alice on 18, alice still active, and a second cleanup step
-- current.
cleanupStepSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cleanupStepSpec s registry = Spec.describe s "extra cleanup step (CR 514.3a)" $ do
  Spec.it s "CR 514.3a a trigger waiting during cleanup resolves in that cleanup" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    megrim <- S.printingOf s registry "Megrim"
    let after = S.runPure S.identityAnswer (cleanupBoard piker 8 [megrim]) Engine.runStep
    Spec.assertEqWith s "CR 514.1 trimmed alice to her maximum hand size" (length (Game.zoneMembers Zone.Hand S.alice after)) 7
    Spec.assertEqWith s "Megrim's trigger RESOLVED, in this cleanup step" (S.lifeOf S.alice after) (Just 18)
    Spec.assertEqWith s "and left the stack empty" (GameState.stack after) []
    Spec.assertEqWith s "the turn did not hand off" (GameState.activePlayer after) S.alice
    Spec.assertEqWith s "so it is still turn 1" (GameState.turnNumber after) 1
    Spec.assertEqWith s "and another cleanup step began" (GameState.phase after) (Phase.Ending EndingStep.Cleanup)

  -- The termination argument at Engine.cleanupException, pinned: the chain is
  -- two cleanup steps because CR 514.1 finds the hand already at its maximum
  -- the second time round and so has no discard to fire the Megrim with.
  Spec.it s "CR 514.3a the second cleanup step finds nothing waiting and ends the turn" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    megrim <- S.printingOf s registry "Megrim"
    let after = S.runPure S.identityAnswer (cleanupBoard piker 8 [megrim]) Engine.runStep
        (again, asked) = runCountingActions after Engine.runStep
    Spec.assertEqWith s "nobody was asked to act in the second cleanup step" asked 0
    Spec.assertEqWith s "the turn handed off" (GameState.activePlayer again) S.bob
    Spec.assertEqWith s "to turn 2" (GameState.turnNumber again) 2
    Spec.assertEqWith s "starting at the untap step" (GameState.phase again) Turn.firstPhase
    Spec.assertEqWith s "with a fresh schedule, not a third cleanup" (GameState.remaining again) Turn.laterPhases
    Spec.assertEqWith s "and alice took Megrim's 2 damage once, not twice" (S.lifeOf S.alice again) (Just 18)

  -- CR 514.3: "Normally, no player receives priority during the cleanup step,
  -- so no spells can be cast and no abilities can be activated." The same
  -- board one card apart: without the Megrim nothing is waiting, and the
  -- exception must not fire.
  Spec.it s "CR 514.3 a cleanup step with nothing waiting grants no priority" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (after, asked) = runCountingActions (cleanupBoard piker 8 []) Engine.runStep
    Spec.assertEqWith s "nobody was asked to act" asked 0
    Spec.assertEqWith s "the turn handed off at once" (GameState.activePlayer after) S.bob
    Spec.assertEqWith s "to turn 2" (GameState.turnNumber after) 2

  -- The count, not just the fact: one round of passes resolves the trigger,
  -- and a second empties the stack and ends the step (CR 514.3a's "once the
  -- stack is empty and all players pass in succession").
  Spec.it s "CR 514.3a the exception grants a real priority round" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    megrim <- S.printingOf s registry "Megrim"
    let (_, asked) = runCountingActions (cleanupBoard piker 8 [megrim]) Engine.runStep
    Spec.assertEqWith s "two passes to resolve the trigger, two to end the step" asked 4

  -- CR 514.3a's condition is "state-based actions ... AND/OR ... triggered
  -- abilities", and the cases above are all the second half. This is the
  -- first half on its own: alice's Goblin Piker (2/1) is enchanted by nothing
  -- and pumped to 5/4 by Giant Growth, and bob's Curse of Death's Hold on
  -- alice makes her creatures -1/-1, so it stands at 4/3. CR 514.2 ends the
  -- +3/+3, the Curse alone leaves it 1/0, and CR 704.5f buries it -- a
  -- state-based action performed BY the cleanup step, with no trigger
  -- anywhere on the board to accompany it.
  --
  -- CR 704.3's last sentence is what this pins: the outcome turns on "the
  -- step's FIRST check", so the check whose result decides the priority round
  -- must be the first thing to perform a state-based action here. An ordinary
  -- unlooped CR 704.3 check running ahead of it buries the Piker, and CR
  -- 514.3a then finds an already-settled board and grants nothing.
  Spec.it s "CR 514.3a a state-based action alone fires the exception" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    giantGrowth <- S.printingOf s registry "Giant Growth"
    curse <- S.printingOf s registry "Curse of Death's Hold"
    let (pikerId, withPiker) = S.addCreature piker S.alice (S.landsInPlay forest 1)
        (gs0, ggId) = S.handOne giantGrowth withPiker
        cast = S.runPure S.identityAnswer gs0 (S.cast S.alice ggId)
        pumped = S.runPure S.identityAnswer cast Stack.resolveTop
        (curseId, withCurse) = S.addCreature curse S.bob pumped
        atCleanup =
          (S.attachTo curseId (Recipient.ToPlayer S.alice) withCurse)
            { GameState.activePlayer = S.alice,
              GameState.turnNumber = 1,
              GameState.phase = Phase.Ending EndingStep.Cleanup,
              GameState.remaining = Seq.empty
            }
        (after, asked) = runCountingActions atCleanup Engine.runStep
    Spec.assertEqWith s "the pump kept the Piker alive up to the cleanup step" (Projection.powerOf pikerId atCleanup) (Just 4)
    Spec.assertBool s (List.notElem pikerId (Game.zoneMembers Zone.Battlefield S.alice after)) "it left the battlefield"
    -- By NAME, not by id: CR 400.7 makes the card in the graveyard a new
    -- object, so pikerId names nothing there. The Giant Growth is in the same
    -- graveyard, hence `any`.
    Spec.assertBool
      s
      (any (\i -> namedIs (CardName.MkCardName $ Text.pack "Goblin Piker") (Game.lookupObject i after)) (Game.zoneMembers Zone.Graveyard S.alice after))
      "CR 704.5f buried it once CR 514.2 ended the pump"
    Spec.assertEqWith s "nothing triggered, so the SBA alone bought the priority round" asked 2
    Spec.assertEqWith s "and another cleanup step began" (GameState.phase after) (Phase.Ending EndingStep.Cleanup)
    Spec.assertEqWith s "with the turn not yet handed off" (GameState.activePlayer after) S.alice
