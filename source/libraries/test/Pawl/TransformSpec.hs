{-# LANGUAGE GADTs #-}

-- Covers CR 701.27 transform end to end: Pawl.Types.Layout's Transforming arm
-- and the three Pawl.Engine.Card functions that read it (CR 712.8a/712.8d's
-- combined view, CR 712.11's castable half, CR 701.27a's turnedOver), the
-- Effect.Transform arm of Pawl.Engine.Resolve with CR 701.27f's already-turned
-- gate (alreadyTurnedFor over Object.turnedOverAt), and
-- Pawl.Engine.Game.manaCostFacesOf (CR 712.8e).
--
-- Also CR 712.14a's enter-transformed instruction, which reaches the same back
-- face by a different road: Pawl.Types.EntryRiders carries it and
-- Pawl.Engine.Event.changeZoneEntering applies it. See enterTransformedSpec.
--
-- Also CR 701.27e's "transforms into", the trigger condition a CARD names:
-- Pawl.Types.TriggerCondition's SelfTransformedInto against the
-- GameEvent.Transformed that Pawl.Engine.Event.recordTransformed writes. See
-- transformTriggerSpec, whose fixture is Blightreaper Thallid // Blightsower
-- Thallid, the Gargoyle printing no text on its back face to trigger with.
--
-- Also CR 702.162a's more than meets the eye -- CR 712.11a's "converted" cast,
-- offered by Pawl.Engine.Card.castableFaces (CR 712.11d) and priced by
-- Pawl.Engine.Cost.candidateCostsFor. See moreThanMeetsTheEyeSpec, which shares
-- convertSpec's Ratchet.
--
-- Also CR 702.146a's disturb, which reaches that same back face through that same
-- CR 712.11d exception and adds rule 702.146a's own zone: the CR 601.3 permission
-- is Pawl.Engine.Cast.permitsDisturb's and the price is the graveyard half of
-- Pawl.Engine.Cost.candidateCostsGiven's converted offer. See disturbSpec, whose
-- fixture is Baithook Angler // Hook-Haunt Drifter.
--
-- Also CR 701.28's convert and CR 702.161a's living metal, which land together
-- because no printed card carries one without the other: the Effect.Convert arm
-- of Pawl.Engine.Resolve (the same turnPermanentsOver the transform arm calls),
-- and the static ability Pawl.Engine.Keyword.livingMetal mints for
-- Pawl.Engine.Projection.View.staticAbilitiesOf. See convertSpec, whose fixture is
-- Ratchet, Field Medic // Ratchet, Rescue Racer.
--
-- Also CR 603.10's first sentence over the two axes CR 109.3 keeps out of an
-- object's characteristics -- who controlled the permanent that turned over and
-- what was attached to it -- which Pawl.Types.Transformed carries beside the
-- sample. See equippedTransformSpec, whose fixture is Neglected Heirloom //
-- Ashmouth Blade on that same Ratchet.
--
-- Also CR 701.27g's "transformed permanent", the phrase a CARD asks rather than
-- the engine: Pawl.Types.Filter's Transformed atom, filled by
-- Pawl.Engine.Projection.View.viewOfCharacteristics. See transformedPermanentSpec,
-- whose fixture is Tovolar and Mutagen Connoisseur rather than the Gargoyle.
--
-- Also CR 701.27f's SECOND sentence, which measures a DELAYED triggered
-- ability's transform from when that ability was created rather than from when
-- it reached the stack. Its pair of cases needs a permanent whose own delayed
-- ability turns it over and something else able to turn it over in between, so
-- they add Aang, at the Crossroads // Aang, Destined Savior and Moonmist. See
-- aangBoard.
--
-- Every case but those groups runs against the printed Thraben Gargoyle //
-- Stonewing Antagonizer, a nonmodal double-faced card (CR 712.2) whose front
-- face is a {1} 2/2 Artifact Creature -- Gargoyle with defender and "{6}:
-- Transform this creature", and whose back face is a 4/2 Artifact Creature --
-- Gargoyle Horror with flying and no text. It was picked because its whole text
-- IS the transform: every characteristic that differs across the two faces is
-- one pawl already reads, so a case that fails here fails about transform and
-- about nothing else. CR 712.14a's group needs an effect that instructs the
-- move, so it adds Befriending the Moths // Imperial Moth.
module Pawl.TransformSpec where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Resolve.Effect as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CastObligation as CastObligation
import qualified Pawl.Types.CastOffer as CastOffer
import qualified Pawl.Types.CastRepetition as CastRepetition
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Daytime as Daytime
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Destroy as Destroy
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PermissionVerb as PermissionVerb
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Transformed as Transformed
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.Zone as Zone

-- CR 701.27g's fixture, which is not the Gargoyle's: Tovolar, Dire Overlord //
-- Tovolar, the Midnight Scourge and Mutagen Connoisseur. See
-- transformedPermanentSpec.
tovolarFront, tovolarBack :: CardName.CardName
tovolarFront = CardName.MkCardName (Text.pack "Tovolar, Dire Overlord")
tovolarBack = CardName.MkCardName (Text.pack "Tovolar, the Midnight Scourge")

-- The two names the card prints. CR 712.8a gives the card only its front face's
-- characteristics off the battlefield, so the second names a face rather than
-- the card -- which CR 201.4d is what lets a player choose all the same.
gargoyleName, antagonizerName :: CardName.CardName
gargoyleName = CardName.MkCardName (Text.pack "Thraben Gargoyle")
antagonizerName = CardName.MkCardName (Text.pack "Stonewing Antagonizer")

-- Everything the two faces disagree about, read through the projection, as one
-- tuple: name, power and toughness, subtypes, defender, flying, and how many
-- activated abilities the permanent offers. Asserted whole so a case names the
-- face rather than six independent facts, and so a change that moved only one of
-- them cannot pass.
faceReadings ::
  ObjectId.ObjectId ->
  GameState.GameState ->
  (Set.Set CardName.CardName, Maybe (Integer, Integer), Set.Set Subtype.Subtype, Bool, Bool, Int)
faceReadings oid gs =
  ( Projection.namesOf oid gs,
    S.powerToughnessOf oid gs,
    Projection.subtypesOf oid gs,
    Projection.hasKeyword Keyword.Defender oid gs,
    Projection.hasKeyword Keyword.Flying oid gs,
    length (Projection.abilitiesOf oid gs)
  )

frontFace, backFace :: (Set.Set CardName.CardName, Maybe (Integer, Integer), Set.Set Subtype.Subtype, Bool, Bool, Int)
frontFace = (Set.singleton gargoyleName, Just (2, 2), Set.singleton Subtype.Gargoyle, True, False, 1)
backFace = (Set.singleton antagonizerName, Just (4, 2), Set.fromList [Subtype.Gargoyle, Subtype.Horror], False, True, 0)

-- CR 701.27f's SECOND sentence needs a permanent whose own DELAYED ability turns
-- it over, and something else able to turn it over in between. Aang, at the
-- Crossroads // Aang, Destined Savior is that permanent, and Moonmist is that
-- something else: of the Transform opcodes in `data/cards/`, Moonmist's is the
-- only one whose ObjectRef is not `InSlot "self"`, so it is what can turn a
-- permanent over without being an ability of it -- and it names Humans, which
-- Aang's front face is. A second such card in the corpus would give this
-- fixture a choice; today there is none.
--
-- WHY Aang and not Archangel Avacyn, which prints the same shape: Avacyn is an
-- Angel, and nothing in `data/cards/` can turn an Angel over, so a board built
-- on it agrees under both clocks. Scryfall
-- `o:transform o:"beginning of the next" include:extras`, 2026-08-24, returns
-- eight cards; of the four whose front face is a Human, Liliana, Heretical
-- Healer and Loyal Cathar return transformed from another zone rather than
-- transforming a permanent, and Sun-Blessed Guardian transforms through an
-- activated ability with no delayed one. A printing whose delayed ability
-- transforms it and whose front face shares a subtype with a corpus
-- transformer would refute the choice, not the rule.
--
-- Aang's back face also prints "at the beginning of combat on your turn,
-- earthbend 2"; it is transcribed, and Pawl.EarthbendSpec is what proves it. No
-- case here reads the back face for anything but its name and its
-- power/toughness, so nothing below can be that trigger's doing.
aangFront, aangBack :: CardName.CardName
aangFront = CardName.MkCardName (Text.pack "Aang, at the Crossroads")
aangBack = CardName.MkCardName (Text.pack "Aang, Destined Savior")

-- Which face of Aang is up: the name and the power/toughness, which are 3/3 on
-- the front and 4/4 on the back. Two readers rather than one, so a case that
-- reads the right face for its name and the wrong one for its size fails here.
aangReadings :: ObjectId.ObjectId -> GameState.GameState -> (Set.Set CardName.CardName, Maybe (Integer, Integer))
aangReadings oid gs = (Projection.namesOf oid gs, S.powerToughnessOf oid gs)

aangFrontUp, aangBackUp :: (Set.Set CardName.CardName, Maybe (Integer, Integer))
aangFrontUp = (Set.singleton aangFront, Just (3, 3))
aangBackUp = (Set.singleton aangBack, Just (4, 4))

-- alice's Aang with a Goblin Piker beside it, Moonmist in hand and two Forests
-- to cast it with. The Piker is the "another creature you control" whose
-- departure arms Aang's delayed ability; it is a Goblin rather than a Human, so
-- Moonmist reaches Aang and nothing else on the board.
aangBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
aangBoard aang piker moonmist forest =
  let (aangId, g0) = S.addPermanent aang S.alice (S.landsInPlay forest 2)
      (pikerId, g1) = S.addPermanent piker S.alice g0
      (moonmistId, g2) = S.addHandCard moonmist S.alice g1
   in (aangId, pikerId, moonmistId, g2 {GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice})

-- The Piker takes lethal damage and CR 704.5g destroys it, Aang's
-- leaves-the-battlefield trigger reaches the stack and resolves, and CR 603.7a
-- creates the delayed ability. Everything before the clock this unit is about.
armAangsDelayedAbility :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
armAangsDelayedAbility pikerId board =
  S.runPure S.identityAnswer (S.settleSba (S.markDamage pikerId 5 board)) (Engine.placePendingTriggers *> Stack.resolveTop)

-- The next upkeep arrives, the delayed ability triggers, and it resolves. The
-- ability object gets its CR 613.7d timestamp HERE, which is what the rule's
-- first sentence would measure from and its second sentence does not.
atNextUpkeep :: GameState.GameState -> GameState.GameState
atNextUpkeep gs =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice)) (gs {GameState.phase = upkeep})
   in S.runPure S.identityAnswer began (Engine.placePendingTriggers *> Stack.resolveTop)

-- "Transform all creatures", the shape CR 701.27a takes when a spell rather than
-- the permanent's own ability asks -- Moonmist's "transform all Humans" with a
-- wider filter. The two cases below that need a permanent turned over without
-- its own ability use this, because Stonewing Antagonizer prints no way back.
transformEveryCreature :: Effect.Effect card ability
transformEveryCreature = Effect.Transform (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature))

-- alice and bob, nothing on the battlefield: the base every case here builds on.
emptyBoard :: GameState.GameState
emptyBoard = Setup.emptyGame S.bothPlayers

-- The sweep as some named object's resolution. `resolving` is what CR 701.27f
-- asks about -- whether the thing turning the permanent over is an ABILITY of
-- that permanent -- so it is a parameter rather than baked in.
sweepFrom :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
sweepFrom resolving gs = S.runPure S.identityAnswer gs (Resolve.applyEffect resolving S.noSource S.alice Map.empty Map.empty transformEveryCreature)

-- The sweep as a resolution with no live object behind it at all -- S.noSource
-- names nothing, which is what an effect applied straight in a spec looks like.
sweep :: GameState.GameState -> GameState.GameState
sweep = sweepFrom S.noSource

-- CR 608.2d and CR 701.27c: "you may transform Aang, Master of Elements. If you
-- do, ..." is not offered to a Clone of him. A Clone copying the back face is
-- not a double-faced card, so the transform would do nothing, and a player can't
-- choose an impossible option. The pair is on one board. At alice's upkeep her
-- real Aang, back face up, and bob's Clone of it both trigger, and both players
-- answer yes. Hers turns over and pays out: she gains 4 and deals 4 to bob.
-- Bob's can't turn over, so he gains nothing and deals nothing to alice.
masterOfElementsCloneSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
masterOfElementsCloneSpec s registry = Spec.describe s "Aang, Master of Elements" $ do
  Spec.it s "CR 608.2d a Clone of Aang, Master of Elements is not offered the transform" $ do
    aang <- S.printingOf s registry "Avatar Aang"
    clone <- S.printingOf s registry "Clone"
    mountain <- S.printingOf s registry "Mountain"
    let (aangId, g0) = S.addPermanent aang S.alice emptyBoard
        (now, g1) = Game.freshTimestamp g0
        backUp = Game.turnFaceOver now aangId g1
        stocked = List.foldl' (\g pid -> List.foldl' (\h _ -> snd (S.addLibraryCard mountain pid h)) g [1 :: Int .. 5]) backUp [S.alice, S.bob]
        (_, staged) = S.spellOnStack clone S.bob stocked
        yes :: Prompt.Prompt r -> r
        yes p = case p of
          Prompt.ChooseCopyTarget _ _ _ legal -> List.find (== aangId) legal
          Prompt.ChooseOptional {} -> OptionalDecision.Exercises
          _ -> S.identityAnswer p
        cloned = S.runPure yes staged Stack.resolveTop
        upkeep = Phase.Beginning BeginningStep.Upkeep
        began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice)) (cloned {GameState.phase = upkeep, GameState.activePlayer = S.alice})
        after = S.runPure yes began (Engine.placePendingTriggers *> Stack.resolveTop *> Stack.resolveTop)
    Spec.assertEqWith s "CR 608.2d bob's Clone could not transform, so bob gained no life and only lost alice's 4" (S.lifeOf S.bob after) (Just 16)
    Spec.assertEqWith s "and alice took no damage from bob's Clone, only gaining her own 4" (S.lifeOf S.alice after) (Just 24)
    -- The preconditions, after the behaviour: the Clone really copied the back
    -- face, and alice's real Aang really turned over.
    Spec.assertEqWith s "the Clone copied Aang, Master of Elements" (fmap (`Projection.namesOf` after) (Game.zoneMembers Zone.Battlefield S.bob after)) [Set.singleton (CardName.MkCardName (Text.pack "Aang, Master of Elements"))]
    Spec.assertEqWith s "alice's Aang turned back to Avatar Aang" (Projection.namesOf aangId after) (Set.singleton (CardName.MkCardName (Text.pack "Avatar Aang")))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Transform" $ do
  masterOfElementsCloneSpec s registry
  enterTransformedSpec s registry
  transformedPermanentSpec s registry
  transformTriggerSpec s registry
  bystanderTransformSpec s registry
  equippedTransformSpec s registry
  convertSpec s registry
  moreThanMeetsTheEyeSpec s registry
  disturbSpec s registry
  spellsCastLastTurnSpec s registry
  restampSpec s registry
  -- CR 712.8d: "While a double-faced permanent has its front face up, it has
  -- only the characteristics of its front face." Nothing has turned this one
  -- over, so CR 712.8a's front face is what Pawl.Engine.Card.combined answers
  -- with.
  --
  -- The falsifier is CR 709.4's reading, which is what the pool's OTHER
  -- two-faced layouts would take: a combined view would name this permanent
  -- "Thraben Gargoyle//Stonewing Antagonizer", give it flying AND defender, and
  -- put Horror on its type line while it is still a Gargoyle.
  Spec.it s "CR 712.8d a double-faced permanent shows its front face and only that" $ do
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    let (oid, gs) = S.addPermanent gargoyle S.alice emptyBoard
    Spec.assertEqWith s "every reader sees Thraben Gargoyle" (faceReadings oid gs) frontFace
    Spec.assertEqWith
      s
      "and it is an artifact creature on both faces, so the type line is not what changes"
      (Projection.cardTypesOf oid gs)
      (Set.fromList [CardType.Artifact, CardType.Creature])
  -- CR 712.11: "A double-faced spell is cast with its front face up by default."
  -- ONE name offered from a hand, where CR 715.3's adventurer card offers two and
  -- CR 709.3's split card offers two -- the whole difference between the layouts
  -- at the point of casting.
  --
  -- Asserted TWICE, because the offer list alone cannot tell CR 712.11 from an
  -- accident: Stonewing Antagonizer prints no mana cost, and CR 202.1b's "having
  -- no mana cost represents an unpayable cost" is not a free one
  -- (Pawl.Engine.Cost.canPay's Nothing arm), so a back face proposed all the way
  -- to the cost gate would be dropped there and the list would read the same.
  -- The falsifier -- Pawl.Engine.Card.castableFaces answering with the whole
  -- NonEmpty, as it does for Split and Adventure -- is caught only by asking
  -- that function directly, so both are here and neither stands alone.
  --
  -- The Gargoyle and not Ratchet, because CR 712.11's "by default" has a printed
  -- exception and Ratchet prints it: a transforming card whose front face has
  -- more than meets the eye offers two names, which is CR 712.11d and is
  -- moreThanMeetsTheEyeSpec's. This case is about a card that has no such
  -- ability, which is what leaves CR 712.11's default the whole answer.
  Spec.it s "CR 712.11 only the front face is offered from a hand" $ do
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    island <- S.printingOf s registry "Island"
    let (gs, _) = S.handOne gargoyle (S.landsInPlay island 1)
        namesOffered = Maybe.mapMaybe (\action -> case action of A.Cast _ n _ -> Just n; _ -> Nothing) (Action.legalActions S.alice gs)
    Spec.assertEqWith
      s
      "CR 712.11: the card proposes its front face and no other"
      (fmap Face.name (Card.castableFaces (Printing.card gargoyle)))
      [gargoyleName]
    Spec.assertEqWith s "and the action list offers that one cast" namesOffered [gargoyleName]
  -- THE proving case. CR 701.27a: "To transform a permanent, turn it over so
  -- that its other face is up."
  --
  -- Played out from the card's own text: six Islands pay the {6}, the ability
  -- goes on the stack and resolves, and every characteristic reader is asked
  -- before and after. Each half of `faceReadings` is a different reader --
  -- Projection.namesOf, the layer 7b power/toughness fold, the layer 4 subtype
  -- set, the layer 6 keyword map, and Activate's own ability list -- so an
  -- engine that turned the permanent over for some of them and not others fails
  -- here rather than in one narrow assertion.
  Spec.it s "CR 701.27a the Gargoyle's own {6} turns it over, and every reader sees the back face" $ do
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    island <- S.printingOf s registry "Island"
    let (oid, g0) = S.addPermanent gargoyle S.alice (S.landsInPlay island 6)
        gs = g0 {GameState.priority = Just S.alice}
    case Activatable.abilitiesFor oid gs of
      [ability] -> do
        let activated = snd (Engine.runGamePure S.identityAnswer gs (Activate.activateAbility S.alice oid ability))
            after = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
        Spec.assertEqWith s "before: Thraben Gargoyle" (faceReadings oid gs) frontFace
        -- The ability is on the stack and has not resolved: CR 701.27a happens
        -- at RESOLUTION, so paying {6} is not what turns the permanent over.
        Spec.assertEqWith s "still the front face while the ability is on the stack" (faceReadings oid activated) frontFace
        Spec.assertEqWith s "six Islands paid the {6}" (S.tappedCount S.alice activated) 6
        Spec.assertEqWith s "after: Stonewing Antagonizer" (faceReadings oid after) backFace
        Spec.assertEqWith
          s
          "and it is still an artifact creature, which is what the two faces agree about"
          (Projection.cardTypesOf oid after)
          (Set.fromList [CardType.Artifact, CardType.Creature])
      abilities -> Spec.assertFailure s ("expected one activated ability, got " <> show (length abilities))
  -- CR 701.27f: "If an activated or triggered ability of a permanent that isn't a
  -- delayed triggered ability of that permanent tries to transform it, the
  -- permanent does so only if it hasn't transformed or converted since the
  -- ability was put onto the stack. ... if the permanent has already transformed
  -- or converted, an instruction to do either is ignored."
  --
  -- Twelve Islands pay for the {6} twice, so BOTH abilities are on the stack
  -- before either resolves and the second activation is legal -- the permanent is
  -- still the Gargoyle, and still offering the ability, while the first waits.
  --
  -- The falsifier is the whole point: without the rule the two resolutions turn
  -- the permanent over and then straight back, and the case ends on the FRONT
  -- face. `backFace` here is therefore an assertion about CR 701.27f and not a
  -- restatement of the case above.
  Spec.it s "CR 701.27f two of the Gargoyle's own abilities on the stack turn it over once" $ do
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    island <- S.printingOf s registry "Island"
    let (oid, g0) = S.addPermanent gargoyle S.alice (S.landsInPlay island 12)
        gs = g0 {GameState.priority = Just S.alice}
        activate g = case Activatable.abilitiesFor oid g of
          [ability] -> Right (snd (Engine.runGamePure S.identityAnswer g (Activate.activateAbility S.alice oid ability)))
          abilities -> Left (length abilities)
    case activate gs >>= activate of
      Left n -> Spec.assertFailure s ("expected one activated ability at each activation, got " <> show n)
      Right activated -> do
        let once = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
            twice = snd (Engine.runGamePure S.identityAnswer once Stack.resolveTop)
        Spec.assertEqWith s "both abilities are on the stack" (length (GameState.stack activated)) 2
        Spec.assertEqWith s "twelve Islands paid for two activations" (S.tappedCount S.alice activated) 12
        Spec.assertEqWith s "the first to resolve turns it over" (faceReadings oid once) backFace
        Spec.assertEqWith s "and the second is ignored, so it stays turned over" (faceReadings oid twice) backFace
        Spec.assertEqWith s "with nothing left on the stack" (length (GameState.stack twice)) 0
  -- The other half of CR 701.27f, and the reason it cannot be written as a
  -- once-per-anything: the gate is only for "an activated or triggered ability of
  -- a permanent ... [that] tries to transform IT". A SPELL is not one, so Moonmist
  -- turns a permanent over however recently its own ability did.
  --
  -- The Gargoyle's own {6} resolves first and stamps the turn; the sweep that
  -- follows names the PERMANENT as its resolving object, whose source is a card
  -- (CR 112.1's spell shape) rather than an ability, and must be exempt. Widening
  -- alreadyTurnedFor to fire for any resolving object leaves this case ending on
  -- the back face.
  Spec.it s "CR 701.27f the gate is only for the permanent's own abilities" $ do
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    island <- S.printingOf s registry "Island"
    let (oid, g0) = S.addPermanent gargoyle S.alice (S.landsInPlay island 6)
        gs = g0 {GameState.priority = Just S.alice}
    case Activatable.abilitiesFor oid gs of
      [ability] -> do
        let activated = snd (Engine.runGamePure S.identityAnswer gs (Activate.activateAbility S.alice oid ability))
            byItsOwnAbility = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
            thenBySomethingElse = sweepFrom oid byItsOwnAbility
        Spec.assertEqWith s "its own {6} turned it over" (faceReadings oid byItsOwnAbility) backFace
        Spec.assertEqWith s "and a spell turns it straight back, same turn" (faceReadings oid thenBySomethingElse) frontFace
      abilities -> Spec.assertFailure s ("expected one activated ability, got " <> show (length abilities))
  -- CR 701.27f's SECOND sentence: "If a delayed triggered ability of a permanent
  -- tries to transform that permanent, the permanent does so only if it hasn't
  -- transformed or converted since that delayed triggered ability was created."
  --
  -- Three moments, in order. The Piker dies and Aang's trigger CREATES the
  -- delayed ability. Moonmist then turns Aang over, so Object.turnedOverAt is
  -- later than that creation. Only at the next upkeep does the delayed ability
  -- reach the stack and take its own CR 613.7d timestamp, which is later still.
  --
  -- The two clocks therefore disagree, and this is the case that says which one
  -- the rule means: measured from the PLACEMENT stamp the turn-over is earlier
  -- and the instruction runs, flipping Aang back to the 3/3 front face;
  -- measured from CREATION it is later and the instruction is ignored, leaving
  -- the 4/4 back face up. The case below is its pair -- same board, no Moonmist
  -- -- and shows the delayed transform is not simply refused.
  Spec.it s "CR 701.27f a delayed transform is measured from when the ability was created" $ do
    aang <- S.printingOf s registry "Aang, at the Crossroads"
    piker <- S.printingOf s registry "Goblin Piker"
    moonmist <- S.printingOf s registry "Moonmist"
    forest <- S.printingOf s registry "Forest"
    let (aangId, pikerId, moonmistId, board) = aangBoard aang piker moonmist forest
        armed = armAangsDelayedAbility pikerId board
        turned = S.runPure S.identityAnswer armed (S.cast S.alice moonmistId *> Stack.resolveTop)
        fired = atNextUpkeep turned
    Spec.assertEqWith s "the Piker's death armed exactly one delayed ability" (Seq.length (GameState.delayedTriggers armed)) 1
    Spec.assertEqWith s "which left Aang on its front face" (aangReadings aangId armed) aangFrontUp
    Spec.assertEqWith s "and Moonmist then turned it over, Aang being a Human" (aangReadings aangId turned) aangBackUp
    Spec.assertEqWith
      s
      "CR 701.27f: Aang turned over since the delayed ability was created, so the instruction is ignored and the back face stays up"
      (aangReadings aangId fired)
      aangBackUp
    Spec.assertEqWith s "with the delayed ability spent and nothing left on the stack" (Seq.length (GameState.delayedTriggers fired), length (GameState.stack fired)) (0, 0)
  -- The pair to the case above, differing in exactly one thing: no Moonmist, so
  -- nothing turns Aang over between the delayed ability's creation and its
  -- resolution. CR 701.27f then has nothing to ignore and the delayed ability
  -- does transform the permanent.
  --
  -- Without this, a gate that refused EVERY delayed transform would pass the
  -- case above.
  Spec.it s "CR 701.27f a delayed transform of a permanent that has not turned over still happens" $ do
    aang <- S.printingOf s registry "Aang, at the Crossroads"
    piker <- S.printingOf s registry "Goblin Piker"
    moonmist <- S.printingOf s registry "Moonmist"
    forest <- S.printingOf s registry "Forest"
    let (aangId, pikerId, _, board) = aangBoard aang piker moonmist forest
        armed = armAangsDelayedAbility pikerId board
        fired = atNextUpkeep armed
    Spec.assertEqWith s "the Piker's death armed exactly one delayed ability" (Seq.length (GameState.delayedTriggers armed)) 1
    Spec.assertEqWith s "and nothing turned Aang over in the meantime" (Maybe.isNothing (Game.lookupObject aangId armed >>= Object.turnedOverAt)) True
    Spec.assertEqWith
      s
      "CR 701.27f: the delayed ability turns Aang over, so the 4/4 back face is up"
      (aangReadings aangId fired)
      aangBackUp
    Spec.assertEqWith s "with the delayed ability spent and nothing left on the stack" (Seq.length (GameState.delayedTriggers fired), length (GameState.stack fired)) (0, 0)
  -- CR 712.8e: "While a nonmodal double-faced permanent has its back face up, it
  -- has only the characteristics of its back face. However, its mana value is
  -- calculated using the mana cost of its front face." CR 202.3a exempts that
  -- back face by name from the mana value of 0 an object with no mana cost has.
  --
  -- Stonewing Antagonizer prints no mana cost, so the naive answer -- read the
  -- live face like every other characteristic -- is 0, and the rule's answer is
  -- the front face's 1. Both readers pawl has are asserted, because they are two
  -- call sites and only one of them feeds a card's Quantity.
  Spec.it s "CR 712.8e a transformed permanent keeps its front face's mana value" $ do
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    let (oid, before) = S.addPermanent gargoyle S.alice emptyBoard
        after = sweep before
    Spec.assertEqWith s "front face up: 1" (sum (fmap Quantity.manaValueOf (Game.manaCostFacesOf oid before))) 1
    Spec.assertEqWith s "it is the back face that is up" (faceReadings oid after) backFace
    Spec.assertEqWith s "back face up: still 1, not the 0 its own empty cost would give" (sum (fmap Quantity.manaValueOf (Game.manaCostFacesOf oid after))) 1
    Spec.assertEqWith s "and a filter reading a mana value agrees" (Filter.manaValue (Projection.viewOfObject oid after)) (Just 1)
  -- CR 701.27c: "If a spell or ability instructs a player to transform a
  -- permanent that isn't represented by a double-faced token or a double-faced
  -- card, nothing happens." CR 712.9 says it again and adds the Example this
  -- stands on: a Clone that copied a double-faced permanent still can't
  -- transform, because the CARD is what the rule asks about.
  --
  -- One sweep over "each creature", two creatures: the answer is a fact about
  -- each permanent's layout rather than about the effect, so both go through the
  -- same instruction and only one turns over.
  Spec.it s "CR 701.27c a creature that is not double-faced is not turned over" $ do
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gargoyleId, g0) = S.addPermanent gargoyle S.alice emptyBoard
        (pikerId, before) = S.addPermanent piker S.alice g0
        after = sweep before
    Spec.assertEqWith s "the Gargoyle turned over" (faceReadings gargoyleId after) backFace
    Spec.assertEqWith s "the Goblin Piker did not" (Projection.namesOf pikerId after) (Set.singleton (CardName.MkCardName (Text.pack "Goblin Piker")))
    Spec.assertEqWith
      s
      "and nothing was written on it: a one-faced card shows no face"
      (Game.lookupObject pikerId after >>= Object.face)
      Nothing
  -- CR 701.27a's "its other face" over the two faces CR 712.1 gives a
  -- double-faced card: a second transform turns the permanent back. Nothing
  -- remembers that it was ever the Gargoyle -- CR 701.27g is explicit that a
  -- permanent with its front face up is never a transformed permanent "even if
  -- it had its back face up previously" -- which transformedPermanentSpec below
  -- proves through a card that asks the question.
  --
  -- It takes an outside effect, because Stonewing Antagonizer prints no ability
  -- at all: the case above proves the {6} is gone with the front face.
  Spec.it s "CR 701.27a transforming twice returns the permanent to its front face" $ do
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    let (oid, before) = S.addPermanent gargoyle S.alice emptyBoard
        once = sweep before
        twice = sweep once
    Spec.assertEqWith s "once: the back face" (faceReadings oid once) backFace
    Spec.assertEqWith s "twice: the front face again, {6} and all" (faceReadings oid twice) frontFace
  -- CR 712.18 states it positively -- "when a double-faced permanent transforms
  -- or converts, it doesn't become a new object. Any effects that applied to that
  -- permanent will continue to apply to it" -- and CR 400.7 is the negative half:
  -- this is not a zone change, so nothing mints an incarnation. The permanent
  -- keeps its id and everything recorded against that id survives.
  --
  -- Damage and a +1/+1 counter are asserted, two of Object.newIncarnation's
  -- per-incarnation list and the two cheapest to place. The counter earns its
  -- place twice over: 4/2 plus it is 5/3, so it also proves the back face's P/T
  -- is a new BASE that layer 7d composes with, rather than a value that replaces
  -- what the layers had computed.
  --
  -- Worth asserting because the obvious wrong implementation is the one that
  -- reaches for the funnel every other change of what a permanent IS goes
  -- through: pawl's transform writes one field in place instead.
  Spec.it s "CR 400.7 does not fire: the turned-over permanent is the same object" $ do
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    let (oid, g0) = S.addPermanent gargoyle S.alice emptyBoard
        counted = g0 {GameState.objects = Map.adjust (\o -> o {Object.counters = Map.insert CounterKind.PlusOnePlusOne 1 (Object.counters o)}) oid (GameState.objects g0)}
        before = S.markDamage oid 1 counted
        after = sweep before
    Spec.assertEqWith s "the back face is up" (Projection.namesOf oid after) (Set.singleton antagonizerName)
    Spec.assertEqWith s "the 1 damage marked on the Gargoyle is still marked" (S.damageOf oid after) (Just 1)
    Spec.assertEqWith s "and its +1/+1 counter survived the turn" (fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid after)) (Just 1)
    -- 4/2 from the back face plus the counter layer 7d still applies (CR 712.18).
    Spec.assertEqWith s "so the back face reads 5/3, not the printed 4/2" (S.powerToughnessOf oid after) (Just (5, 3))
    Spec.assertEqWith s "and the battlefield holds one permanent, not a replacement" (length (Game.zoneMembers Zone.Battlefield S.alice after)) 1

-- The name of the one face that reaches the battlefield below. CR 712.8a keeps
-- it off every reading of the card in a hand or a graveyard, so a permanent that
-- answers to it can only have entered showing its back face.
mothName :: CardName.CardName
mothName = CardName.MkCardName (Text.pack "Imperial Moth")

-- A board sitting in `pid`'s precombat main phase, the moment CR 505.4 / 714.3c
-- puts a lore counter on each of their Sagas.
precombatMainOf :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
precombatMainOf pid gs =
  gs
    { GameState.phase = Phase.PrecombatMain,
      GameState.activePlayer = pid,
      GameState.priority = Just pid
    }

-- CR 712.14a: "If a spell or ability puts a double-faced card onto the
-- battlefield 'transformed' or 'converted', it enters the battlefield with its
-- back face up. If a player is instructed to put a card that isn't a
-- double-faced card onto the battlefield transformed or converted, that card
-- stays in its current zone."
--
-- Both sentences, and Pawl.Engine.Event.changeZoneEntering is where both live --
-- the rider itself is Pawl.Types.EntryRiders' `transformed`, which
-- Pawl.Engine.Resolve's MoveToZone arm hands to that door without reading.
--
-- The producer this group uses is Befriending the Moths // Imperial Moth, a
-- Kamigawa: Neon Dynasty Saga whose chapter III reads "Exile this Saga, then
-- return it to the battlefield transformed under your control" -- CR 712.14a's
-- wording on a card rather than on a spell, which is what makes it this rule's
-- producer and not CR 712.13a's. It is no longer the pool's only one: CR
-- 702.167a's craft mints the same instruction in the engine
-- (Pawl.Engine.Keyword.craft), which Pawl.ActivateSpec's Craft group proves. Its back face is a 2/4 white Enchantment Creature -- Insect with
-- flying, and every one of those readings belongs to that face alone: the front
-- face is a Saga enchantment with no power, no toughness, no flying and no
-- creature type. A case that passed with the FRONT face up would fail every line.
enterTransformedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
enterTransformedSpec s registry = Spec.describe s "Entering the battlefield transformed" $ do
  Spec.it s "CR 712.14a a Saga returned transformed comes back as its back face" $ do
    moths <- S.printingOf s registry "Befriending the Moths"
    let (sagaId, base) = S.addPermanent moths S.alice emptyBoard
        -- Two lore counters placed outright, so nothing has crossed a chapter
        -- yet: CR 714.3c's turn-based action then takes the count from two to
        -- three, and CR 714.2b's "was less than N and became at least N" makes
        -- chapter III the only one that fires.
        withCounters = S.addCounter CounterKind.Lore 2 sagaId base
        advanced = S.runPure S.identityAnswer (precombatMainOf S.alice withCounters) (Engine.runTurnBasedActions Phase.PrecombatMain)
        after = S.runPure S.identityAnswer advanced Engine.priorityLoop
    Spec.assertEqWith s "the turn-based action took it to its final chapter" (S.counterOf CounterKind.Lore sagaId advanced) 3
    -- CR 400.7: the exile and the return each mint a new object, so the id the
    -- Saga had is gone rather than turned over in place. That is the whole
    -- difference between this rule and CR 701.27a's transform.
    Spec.assertBool s (not (S.onBattlefield sagaId after)) "the Saga's own object is gone"
    case Game.zoneMembers Zone.Battlefield S.alice after of
      [oid] -> do
        Spec.assertEqWith s "the permanent is the BACK face" (Projection.namesOf oid after) (Set.singleton mothName)
        Spec.assertEqWith s "recorded as the face it shows" (fmap Object.face (Game.lookupObject oid after)) (Just (Just mothName))
        Spec.assertEqWith s "a 2/4, where the Saga face has no P/T box at all" (S.powerToughnessOf oid after) (Just (2, 4))
        Spec.assertEqWith s "an Insect, and no longer a Saga" (Projection.subtypesOf oid after) (Set.singleton Subtype.Insect)
        Spec.assertEqWith s "an enchantment CREATURE" (Projection.cardTypesOf oid after) (Set.fromList [CardType.Creature, CardType.Enchantment])
        Spec.assertBool s (Projection.hasKeyword Keyword.Flying oid after) "with the back face's flying"
      other -> Spec.assertFailure s ("expected exactly one permanent, got " <> show (length other))
  -- CR 712.14a's second sentence, which has no printing: every card that prints
  -- the "transformed" wording returns ITSELF, and each of them is double-faced.
  -- So the instruction is issued straight at the door, once per layout.
  --
  -- Goblin Piker is the single-faced card, and it appears TWICE -- once
  -- transformed and once not -- so "nothing was ever put onto the battlefield by
  -- this call" cannot be why the refusal reads as one. Thraben Gargoyle is the
  -- other control: the same call, the same board shape, a double-faced card, and
  -- it enters showing the face CR 712.14a names.
  Spec.it s "CR 712.14a a card that isn't double-faced stays in its current zone" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    let transformed = EntryRiders.defaultValue {EntryRiders.transformed = True}
        put riders printing =
          let (board, oid) = S.handOne printing emptyBoard
           in (oid, S.runPure S.identityAnswer board (Monad.void (Event.changeZoneEntering oid Zone.Battlefield LibraryPosition.defaultValue riders (Just S.alice))))
        (refusedId, refused) = put transformed piker
        (_, entered) = put EntryRiders.defaultValue piker
        (_, turned) = put transformed gargoyle
    Spec.assertEqWith s "the single-faced card is the same object, still in hand" (fmap Object.zone (Game.lookupObject refusedId refused)) (Just Zone.Hand)
    Spec.assertEqWith s "so nothing entered the battlefield" (Game.zoneMembers Zone.Battlefield S.alice refused) []
    Spec.assertEqWith s "the same card put there UNtransformed does enter" (fmap (\oid -> Projection.namesOf oid entered) (Game.zoneMembers Zone.Battlefield S.alice entered)) [Set.singleton (CardName.MkCardName (Text.pack "Goblin Piker"))]
    Spec.assertEqWith s "and a double-faced card enters showing its back face" (fmap (\oid -> Projection.namesOf oid turned) (Game.zoneMembers Zone.Battlefield S.alice turned)) [Set.singleton antagonizerName]

-- The face this permanent is showing, by name (CR 709.4a): Object.face is the
-- one field CR 701.27a writes, and CR 712.8d/e make every characteristic follow
-- it. Pawl.DaytimeSpec keeps its own copy for the same reason.
faceNameOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe CardName.CardName
faceNameOf oid gs = fmap Face.name (Game.faceOf oid gs)

-- CR 117.5's settle, where CR 702.145c/d/f are checked. Not S.settleSba: those
-- rules are explicitly not state-based actions.
settleDaytime :: GameState.GameState -> GameState.GameState
settleDaytime gs = S.runPure S.identityAnswer gs Engine.settleForPriority

-- The upkeep step of alice's turn, Pawl.DaytimeSpec's `upkeep` exactly: the
-- schedule loses its head so runStep advances OUT of the upkeep rather than back
-- into it.
aliceUpkeep :: GameState.GameState -> GameState.GameState
aliceUpkeep gs =
  gs
    { GameState.activePlayer = S.alice,
      GameState.phase = Phase.Beginning BeginningStep.Upkeep,
      GameState.priority = Just S.alice,
      GameState.remaining = Seq.drop 1 (GameState.remaining gs)
    }

-- CR 502.2's day/night check, run as the untap step's turn-based actions with
-- `n` spells on the previous turn's books.
untapStepAfter :: Natural.Natural -> GameState.GameState -> GameState.GameState
untapStepAfter n gs =
  S.runPure S.identityAnswer (gs {GameState.spellsCastLastTurn = n}) (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))

-- alice's board for CR 701.27g: Tovolar, two Russet Wolves, Mutagen Connoisseur
-- and a Thraben Gargoyle, settled so CR 702.145d has made it day.
--
-- Two Wolves and no more because CR 603.4's intervening "if" wants three Wolves
-- and/or Werewolves and Tovolar is himself the third; the Connoisseur and the
-- Gargoyle are neither, so they do not stand in for one.
transformedBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
transformedBoard tovolar wolf connoisseur gargoyle =
  let (tovolarId, withTovolar) = S.addPermanent tovolar S.alice emptyBoard
      withWolves = foldr (\_ g -> snd (S.addPermanent wolf S.alice g)) withTovolar [1 :: Int, 2]
      (connoisseurId, withConnoisseur) = S.addPermanent connoisseur S.alice withWolves
      (_, withGargoyle) = S.addPermanent gargoyle S.alice withConnoisseur
   in (tovolarId, connoisseurId, settleDaytime withGargoyle)

-- CR 701.27g, "transformed permanent", asked by a CARD rather than by the
-- engine: Mutagen Connoisseur's "this creature gets +1/+0 for each transformed
-- permanent you control" is `Filter.Transformed` conjoined with `ControlledBy
-- You` under a Count over the battlefield, so its power IS the tally and every
-- case here reads it.
--
-- Tovolar, Dire Overlord // Tovolar, the Midnight Scourge is the permanent that
-- moves. CR 702.145c/f turn him over and back through the rules alone, which is
-- the pool's only road to a permanent that is front face up AND has been back
-- face up before -- the one board on which CR 701.27g's first exclusion is
-- distinguishable from a reading off Object.turnedOverAt.
--
-- Thraben Gargoyle is on the board and never turns, so a reading of "transformed
-- permanent" as "double-faced permanent" answers 2 where the rule answers 0. The
-- two Russet Wolves are single-faced and contribute nothing under any reading;
-- they are there to reach Tovolar's trigger.
--
-- CR 701.27g's SECOND exclusion -- an object represented by more than one card
-- is never a transformed permanent -- is asserted in Pawl.MeldSpec instead, on
-- the board that reaches one: "CR 701.27g a melded permanent is not a
-- transformed permanent" puts this same Connoisseur beside a melded Hanweir and
-- a Thraben Gargoyle, and counts the Gargoyle alone. The rule's other half, a
-- MERGED permanent, takes the same Game.componentsOf read, and no case builds
-- that board: Cubwarden over a transformed Blightreaper Thallid would.
transformedPermanentSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
transformedPermanentSpec s registry = Spec.describe s "TransformedPermanent" $ do
  -- CR 701.27g's positive: a double-faced permanent on the battlefield with its
  -- BACK face up is a transformed permanent, and the Connoisseur counts exactly
  -- one of them although two double-faced permanents are on the board.
  Spec.it s "CR 701.27g a permanent with its back face up is a transformed permanent" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    wolf <- S.printingOf s registry "Russet Wolves"
    connoisseur <- S.printingOf s registry "Mutagen Connoisseur"
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    let (tovolarId, connoisseurId, day) = transformedBoard tovolar wolf connoisseur gargoyle
        night = S.runPure S.identityAnswer (aliceUpkeep day) Engine.runStep
    Spec.assertEqWith s "the Connoisseur counts the one transformed permanent" (S.powerToughnessOf connoisseurId night) (Just (1, 5))
    Spec.assertEqWith s "it is night" (GameState.daytime night) (Just Daytime.Night)
    Spec.assertEqWith s "and Tovolar is the permanent showing a back face" (faceNameOf tovolarId night) (Just tovolarBack)
  -- CR 701.27g's first sentence read the other way: with every double-faced
  -- permanent front face up the tally is zero, although the board holds two of
  -- them. The falsifier for "a double-faced permanent is a transformed
  -- permanent", which would answer 2 here.
  Spec.it s "CR 701.27g a permanent with its front face up is not one" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    wolf <- S.printingOf s registry "Russet Wolves"
    connoisseur <- S.printingOf s registry "Mutagen Connoisseur"
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    let (tovolarId, connoisseurId, day) = transformedBoard tovolar wolf connoisseur gargoyle
    Spec.assertEqWith s "the Connoisseur counts none" (S.powerToughnessOf connoisseurId day) (Just (0, 5))
    Spec.assertEqWith s "it is day" (GameState.daytime day) (Just Daytime.Day)
    Spec.assertEqWith s "and Tovolar shows his front face" (faceNameOf tovolarId day) (Just tovolarFront)
  -- CR 701.27g's second sentence: "even if it had its back face up previously".
  -- Tovolar goes day -> night -> day, so at the read he is front face up with
  -- Object.turnedOverAt set twice over. The falsifier for an answer read off
  -- that field instead of off Object.face, which would count him here.
  --
  -- The turnedOverAt assertion is the case's PRECONDITION and is read straight
  -- off the object, so no change to the atom can move it -- without it this case
  -- is the one above with extra steps.
  Spec.it s "CR 701.27g not one even if it had its back face up previously" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    wolf <- S.printingOf s registry "Russet Wolves"
    connoisseur <- S.printingOf s registry "Mutagen Connoisseur"
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    let (tovolarId, connoisseurId, day) = transformedBoard tovolar wolf connoisseur gargoyle
        night = S.runPure S.identityAnswer (aliceUpkeep day) Engine.runStep
        again = untapStepAfter 2 night
    Spec.assertEqWith s "Tovolar has turned over before" (fmap (Maybe.isJust . Object.turnedOverAt) (Game.lookupObject tovolarId again)) (Just True)
    Spec.assertEqWith s "yet the Connoisseur counts none" (S.powerToughnessOf connoisseurId again) (Just (0, 5))
    Spec.assertEqWith s "it is day again" (GameState.daytime again) (Just Daytime.Day)
    Spec.assertEqWith s "and Tovolar is back on his front face" (faceNameOf tovolarId again) (Just tovolarFront)

-- The two names Blightreaper Thallid // Blightsower Thallid prints, and the
-- token its back face makes. CR 701.27e's group reads all three.
thallidFront, thallidBack, saprolingToken :: CardName.CardName
thallidFront = CardName.MkCardName (Text.pack "Blightreaper Thallid")
thallidBack = CardName.MkCardName (Text.pack "Blightsower Thallid")
saprolingToken = CardName.MkCardName (Text.pack "Phyrexian Saproling Token")

-- What Cult of the Waxing Moon makes, and the quantity every bystander case
-- below asserts.
wolfToken :: CardName.CardName
wolfToken = CardName.MkCardName (Text.pack "Wolf Token")

-- The face Howlpack Piper turns INTO at nightfall, which is also the name its own
-- printed trigger condition asks about.
howlerName :: CardName.CardName
howlerName = CardName.MkCardName (Text.pack "Wildsong Howler")

-- alice's board for CR 701.27e: one Blightreaper Thallid and `n` Forests, in her
-- own precombat main phase with priority, since CR 307.5 is what the card's
-- "Activate only as a sorcery" rider asks for.
--
-- Forests rather than any land because {3}{G/P} wants a GREEN one: S.identityAnswer
-- declines Pawl.Types.Prompt's Phyrexian offer (CR 107.4f's life payment), so the
-- symbol is paid with mana and the board has to hold some.
thallidBoard :: Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
thallidBoard thallid forest n =
  let (oid, g0) = S.addPermanent thallid S.alice (S.landsInPlay forest n)
   in (oid, g0 {GameState.priority = Just S.alice, GameState.phase = Phase.PrecombatMain})

-- Activate the Thallid's one ability, or say how many it offered instead.
activateThallid :: ObjectId.ObjectId -> GameState.GameState -> Either Int GameState.GameState
activateThallid oid gs = case Activatable.abilitiesFor oid gs of
  [ability] -> Right (S.runPure S.identityAnswer gs (Activate.activateAbility S.alice oid ability))
  abilities -> Left (length abilities)

-- CR 117.5's settle, where CR 603.3 gathers what triggered and puts it on the
-- stack. Named apart from settleDaytime above because this group is about the
-- gather rather than about the day/night check inside it.
gather :: GameState.GameState -> GameState.GameState
gather gs = S.runPure S.identityAnswer gs Engine.settleForPriority

resolveTop :: GameState.GameState -> GameState.GameState
resolveTop gs = S.runPure S.identityAnswer gs Stack.resolveTop

-- Resolve EVERYTHING the gather put on the stack, not just its top. A count of
-- tokens after one resolveTop cannot tell "one trigger fired" from "two fired
-- and one is still waiting", which is exactly the pair the CR 701.27f case
-- exists to separate. One pass per object already there, since nothing these
-- boards resolve puts anything back.
resolveStack :: GameState.GameState -> GameState.GameState
resolveStack gs = foldr (\_ g -> resolveTop g) gs (GameState.stack gs)

-- Wildsong Howler's payload prints a "may" -- "You may reveal a creature card
-- from among them" -- so its fixture has to take it. A FIXED decision rather than
-- one read off the offer, Pawl.LibraryOrderSpec's `wildsAnswer` posture and for
-- its reason: an answerer deriving its answer from the prompt would still answer
-- legally after a mutation broke which cards were looked at.
takesTheMay :: Prompt.Prompt r -> r
takesTheMay p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- The same decision over S.castAnswer, for the case that CASTS the Piper.
castsAndTakesTheMay :: Prompt.Prompt r -> r
castsAndTakesTheMay p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.castAnswer p

-- `gather` and `resolveStack` under that answerer. Separate functions rather than
-- a parameter on those two: the answerer is a rank-2 argument, and this module
-- takes no language pragma to say so.
gatherTakingTheMay :: GameState.GameState -> GameState.GameState
gatherTakingTheMay gs = S.runPure takesTheMay gs Engine.settleForPriority

resolveStackTakingTheMay :: GameState.GameState -> GameState.GameState
resolveStackTakingTheMay gs = foldr (\_ g -> S.runPure takesTheMay g Stack.resolveTop) gs (GameState.stack gs)

-- alice's library, stocked from the TOP DOWN: S.addLibraryCard puts its card on
-- top, so the deepest is stocked first (Pawl.LibraryOrderSpec's wildsBoard).
stockLibrary :: [Printing.Printing] -> GameState.GameState -> GameState.GameState
stockLibrary printings gs = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) gs (reverse printings)

-- The nine cards Wildsong Howler's trigger reads, top down, shared by both of its
-- cases.
--
-- NINE, not six: the top six are what the trigger looks at, and the three beneath
-- them are what makes "on the BOTTOM" observable -- with a six-card library the
-- bottom and the top are the same set. Exactly one of the six is a creature card,
-- so the reveal has one legal answer and neither case is about who picks. Every
-- name distinct, so an assertion over the library reads positions rather than a
-- multiset of Forests.
howlerDeck :: [String]
howlerDeck = ["Forest", "Mountain", "Goblin Piker", "Island", "Swamp", "Plains", "Lightning Bolt", "Ancestral Recall", "Giant Growth"]

-- The names of alice's cards in a zone, in that zone's own order --
-- Pawl.LibraryOrderSpec's zoneNames, which this module keeps its own copy of
-- rather than hoisting to Pawl.Support.
zoneNames :: Zone.Zone -> GameState.GameState -> [String]
zoneNames zone gs =
  fmap
    (\oid -> maybe "?" (Text.unpack . CardName.unwrap . Face.name) (Game.faceOf oid gs))
    (Game.zoneMembers zone S.alice gs)

-- CR 701.27e, "transforms into", the phrase a CARD asks: Blightreaper Thallid //
-- Blightsower Thallid, {1}{B} 2/2 Creature -- Fungus with "{3}{G/P}: Transform
-- this creature. Activate only as a sorcery.", whose back face is a 3/3 Creature
-- -- Phyrexian Fungus reading "When this creature transforms into Blightsower
-- Thallid or dies, create a 1/1 green Phyrexian Saproling creature token."
--
-- The card is the producer rather than the Gargoyle because the Gargoyle's back
-- face prints no text at all, and this rule is about a trigger printed on the
-- face turned TO. That placement is the whole difficulty: Pawl.Engine.Card gives
-- a transforming permanent only the SHOWN face's abilities, so the trigger does
-- not exist until the turn has happened, and Pawl.Engine.Resolve's Transform arm
-- records its event after the fold for exactly that reason.
--
-- The token is the assertion in every case because it is a quantity a partial
-- fix cannot reach another way: the Thallid makes no token by any other road,
-- and counting alice's permanents instead would move if the Thallid itself were
-- duplicated.
--
-- The printed condition is an "or", so both limbs are exercised: the transform
-- one here, CR 700.4's ordinary SelfDies in the last case. Without that pair a
-- condition that fired on the wrong limb would pass.
--
-- The CR 702.145c/f road to the same event -- Pawl.Engine.Daytime's sweep, which
-- records through the same Event.recordTransformed -- reaches it too, on its own
-- fixture, since the Thallid is neither daybound nor nightbound: see the
-- nightfall case at the foot of this group, which is what proves the record on
-- that road rather than fencing it.
transformTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
transformTriggerSpec s registry = Spec.describe s "TransformsInto" $ do
  -- CR 701.27f from the event's side: the second resolution is IGNORED, so it is
  -- not an event and nothing triggers on it. Eight Forests put both activations
  -- on the stack before either resolves, TransformSpec's own CR 701.27f board.
  --
  -- The falsifier is an implementation that records the event for every victim
  -- the instruction NAMED rather than for the ones that turned: it makes two
  -- Saprolings here, and one everywhere else, so this is the only case that can
  -- tell the two apart.
  Spec.it s "CR 701.27f a turn that was ignored triggers nothing" $ do
    thallid <- S.printingOf s registry "Blightreaper Thallid"
    forest <- S.printingOf s registry "Forest"
    let (oid, gs) = thallidBoard thallid forest 8
    case activateThallid oid gs >>= activateThallid oid of
      Left n -> Spec.assertFailure s ("expected one activated ability at each activation, got " <> show n)
      Right activated -> do
        let twice = resolveTop (resolveTop activated)
            settled = gather twice
            after = resolveStack settled
        Spec.assertEqWith s "one turn, so one Saproling" (S.countOnBattlefieldByName saprolingToken S.alice after) 1
        Spec.assertEqWith s "the settle placed one trigger, not two" (length (GameState.stack settled)) 1
        Spec.assertEqWith s "both abilities were on the stack" (length (GameState.stack activated)) 2
        Spec.assertEqWith s "and the permanent is still on its back face" (faceNameOf oid twice) (Just thallidBack)
  -- CR 608.2f: one instruction turns both Thallids over at once, so there are
  -- two events in one group and two triggers -- one each, not one each per
  -- event. The falsifier is a match that dropped the bearer comparison: each
  -- Thallid would see the other's event too and the board would end on four
  -- Saprolings.
  --
  -- A SPELL does the turning (S.noSource resolving, TransformSpec's `sweep`), so
  -- CR 701.27f's gate is not what makes the count one apiece -- that rule is only
  -- for a permanent's own ability.
  Spec.it s "CR 608.2f two Thallids turned at once trigger once each" $ do
    thallid <- S.printingOf s registry "Blightreaper Thallid"
    let (first, g0) = S.addPermanent thallid S.alice emptyBoard
        (second, g1) = S.addPermanent thallid S.alice g0
        turned = sweep g1
        settled = gather turned
        after = resolveStack settled
    Spec.assertEqWith s "one Saproling apiece, not one per event apiece" (S.countOnBattlefieldByName saprolingToken S.alice after) 2
    Spec.assertEqWith s "the settle placed two triggers" (length (GameState.stack settled)) 2
    Spec.assertEqWith s "and both really turned over" (fmap (\oid -> faceNameOf oid turned) [first, second]) [Just thallidBack, Just thallidBack]
  -- CR 701.27e's "with a specified characteristic", asked of the match alone,
  -- because no BOARD can ask it: Pawl.Engine.Card gives a transforming permanent
  -- only the shown face's abilities, so a "transforms into X" trigger exists
  -- exactly when the permanent is showing X and every printed pair matches. The
  -- name becomes load-bearing under a copy effect, which is why the check is
  -- here rather than dropped -- a UNIT fence, stated as one, since the gameplay
  -- cases above stay green without it.
  Spec.it s "CR 701.27e the condition refuses an event naming a different face" $ do
    let board = emptyBoard
        bearer = S.noSource
        event into = GameEvent.Transformed Transformed.MkTransformed {Transformed.object = bearer, Transformed.characteristics = S.emptyCharacteristics {PC.names = Set.singleton into}, Transformed.controller = Nothing, Transformed.attachments = Set.empty}
        matches into = Event.matchesTrigger board bearer S.alice (TriggerCondition.SelfTransformedInto thallidBack) (event into)
    Spec.assertBool s (matches thallidBack) "the face it names matches"
    Spec.assertBool s (not (matches thallidFront)) "and the other face of the same card does not"
  -- The printed condition's OTHER limb, so the AnyOf is not proved by one side
  -- alone: CR 700.4's "or dies", against the same board with the same card. The
  -- Thallid transforms (one Saproling), then a destruction reaches it on its back
  -- face (a second).
  --
  -- The destruction names Fungus rather than every creature, so it cannot reach
  -- the Saproling the first limb made -- which would leave the count reading the
  -- same under an engine that fired neither limb.
  Spec.it s "CR 700.4 the same ability's other limb fires when it dies" $ do
    thallid <- S.printingOf s registry "Blightreaper Thallid"
    forest <- S.printingOf s registry "Forest"
    let (oid, gs) = thallidBoard thallid forest 4
    case activateThallid oid gs of
      Left n -> Spec.assertFailure s ("expected one activated ability, got " <> show n)
      Right activated -> do
        let fromTransform = resolveStack (gather (resolveTop activated))
            destroyed = S.runPure S.identityAnswer fromTransform (Resolve.applyEffect S.noSource S.noSource S.alice Map.empty Map.empty destroyEveryFungus)
            fromDeath = resolveStack (gather destroyed)
        Spec.assertEqWith s "the transform limb made one" (S.countOnBattlefieldByName saprolingToken S.alice fromTransform) 1
        Spec.assertEqWith s "and the death limb makes a second" (S.countOnBattlefieldByName saprolingToken S.alice fromDeath) 2
        Spec.assertEqWith s "the Thallid itself is gone" (faceNameOf oid fromDeath) Nothing
  -- CR 702.145c's road to the same event, and the reason this group needs a
  -- second fixture: no spell and no activated ability turns this permanent over.
  -- NIGHTFALL does, through Pawl.Engine.Daytime's sweep, which reaches
  -- Game.turnFaceOver directly and records CR 701.27a's event through the same
  -- Event.recordTransformed. The fixture is Howlpack Piper // Wildsong Howler, a
  -- {3}{G} 2/2 Creature -- Human Werewolf with daybound
  -- whose back face is a 4/4 Creature -- Werewolf with nightbound reading
  -- "Whenever this creature enters or transforms into Wildsong Howler, look at
  -- the top six cards of your library. You may reveal a creature card from among
  -- them and put it into your hand. Put the rest on the bottom of your library in
  -- a random order."
  --
  -- The Piper is PLACED rather than cast, so no enters event exists and the
  -- printed "or" cannot be firing on its SelfEnters limb -- the board proves the
  -- transform limb specifically. A later reader who "simplifies" this by casting
  -- the card destroys that.
  --
  -- `howlerDeck` above says why the library is nine cards and how they are
  -- chosen.
  Spec.it s "CR 702.145c/701.27e nightfall turns the Piper over and the back face's trigger fires" $ do
    piper <- S.printingOf s registry "Howlpack Piper"
    deck <- mapM (S.printingOf s registry) howlerDeck
    let (piperId, placed) = S.addPermanent piper S.alice emptyBoard
        -- CR 702.145d: alice controls a daybound permanent and it is neither day
        -- nor night, so the settle makes it day.
        day = settleDaytime (stockLibrary deck placed)
        -- CR 502.2: day, and no spells last turn, so it becomes night -- and CR
        -- 702.145c turns the Piper over as it does.
        night = untapStepAfter 0 day
        after = resolveStackTakingTheMay (gatherTakingTheMay night)
    Spec.assertEqWith s "the one creature card among the top six is in alice's hand" (zoneNames Zone.Hand after) ["Goblin Piker"]
    Spec.assertEqWith s "the three cards under the looked-at six are now the top three" (take 3 (zoneNames Zone.Library after)) ["Lightning Bolt", "Ancestral Recall", "Giant Growth"]
    -- CR 401.4 taken back by the printed "in a random order": the five arrive as
    -- a batch whose order no rule lets a player read, so this asserts the SET.
    Spec.assertEqWith s "and the other five went to the bottom" (Set.fromList (drop 3 (zoneNames Zone.Library after))) (Set.fromList ["Forest", "Mountain", "Island", "Swamp", "Plains"])
    Spec.assertEqWith s "the permanent really did turn over" (faceNameOf piperId after) (Just howlerName)
    Spec.assertEqWith s "it really is night" (GameState.daytime after) (Just Daytime.Night)
    Spec.assertEqWith s "and it was day before the untap step, so CR 502.2 had a designation to change" (GameState.daytime day) (Just Daytime.Day)
  -- The printed condition's OTHER limb, so the AnyOf is not proved by one side
  -- alone -- the pair the Thallid cases above make with CR 700.4's "or dies".
  -- CR 712.13a through CR 702.145b's first static ability gets there on the same
  -- card: the Piper is CAST at night, so it ENTERS as Wildsong Howler and the
  -- SelfEnters limb fires without the Piper ever transforming.
  --
  -- Tovolar is what gives the game a designation at all before the Piper is cast
  -- (CR 702.145d wants a daybound permanent on the battlefield, and the Piper is
  -- in hand), Pawl.DaytimeSpec's expertBoard exactly. He DOES transform at
  -- nightfall, so this board is not free of CR 701.27a events -- his names
  -- "Tovolar, the Midnight Scourge" and the condition is self-scoped besides, so
  -- it cannot reach the Piper. He triggers nothing else on the way either: his
  -- back face's abilities are an upkeep trigger and a combat-damage trigger.
  --
  -- The face assertion comes FIRST because the payload assertions cannot tell the
  -- limbs apart on their own: an engine that skipped CR 712.13a would put the
  -- Piper on the battlefield front face up, and the settle's CR 702.145c sweep
  -- would then turn it over and fire the SAME trigger on its transform limb.
  -- `entered` is read before that settle, and stripping the record from
  -- Daytime.turnDue leaves this case green, which is the other half of the
  -- separation.
  Spec.it s "CR 712.13a/701.27e the same trigger's enters limb fires on a Piper cast at night" $ do
    tovolar <- S.printingOf s registry "Tovolar, Dire Overlord"
    forest <- S.printingOf s registry "Forest"
    piper <- S.printingOf s registry "Howlpack Piper"
    deck <- mapM (S.printingOf s registry) howlerDeck
    let (_, withTovolar) = S.addPermanent tovolar S.alice (S.landsInPlay forest 4)
        (inHand, piperSpell) = S.handOne piper (stockLibrary deck withTovolar)
        night = untapStepAfter 0 (settleDaytime inHand)
        cast = S.runPure castsAndTakesTheMay night (S.cast S.alice piperSpell)
        entered = S.runPure castsAndTakesTheMay cast Stack.resolveTop
        after = resolveStackTakingTheMay (gatherTakingTheMay entered)
    -- Read on `entered`, the board the spell's resolution leaves, which is BEFORE
    -- the settle the CR 702.145c sweep runs in. The FACES, not
    -- S.countOnBattlefieldByName: that helper reads the card's name (CR 712.8a's
    -- front face), which cannot tell the two faces apart. The whole battlefield,
    -- so "Howlpack Piper" is asserted absent as well as "Wildsong Howler"
    -- present.
    Spec.assertEqWith s "the permanent was showing Wildsong Howler the moment it entered, and never its front face" (List.sort (zoneNames Zone.Battlefield entered)) ["Forest", "Forest", "Forest", "Forest", "Tovolar, the Midnight Scourge", "Wildsong Howler"]
    Spec.assertEqWith s "the one creature card among the top six is in alice's hand" (zoneNames Zone.Hand after) ["Goblin Piker"]
    Spec.assertEqWith s "the three cards under the looked-at six are now the top three" (take 3 (zoneNames Zone.Library after)) ["Lightning Bolt", "Ancestral Recall", "Giant Growth"]
    Spec.assertEqWith s "it was night when the spell resolved" (GameState.daytime night) (Just Daytime.Night)

-- CR 701.27e read by a BYSTANDER, which is the other half of that rule: "Some
-- triggered abilities trigger when an object 'transforms into' an object with a
-- specified characteristic." The fixture is Cult of the Waxing Moon, a {4}{G}
-- 5/4 Creature -- Human Shaman reading "Whenever a permanent you control
-- transforms into a non-Human creature, create a 2/2 green Wolf creature token."
--
-- It is the producer because its filter names all three axes the arm has to get
-- right at once -- one control question, which no ProjectedCharacteristics
-- carries (CR 109.3), and two characteristics, which the board can only be
-- trusted for while nothing has turned again -- and because the token is a
-- quantity nothing else on these boards can produce.
--
-- Every case turns the permanent over with the same `sweep` and asserts the same
-- count. The first three differ from each other in one thing apiece; the last is
-- the one board on which the sample and a live read part company.
--
-- The Cult itself is a creature the sweep names, and CR 701.27c leaves it alone:
-- it is not double-faced, so no event is recorded for it. Were one recorded, the
-- Cult is a Human and the filter would decline it anyway.
bystanderTransformSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bystanderTransformSpec s registry = Spec.describe s "TransformsIntoWatched" $ do
  -- The rule's own case. Aang is a Human on the face it turns FROM and not one on
  -- the face it turns INTO, so a match reading the pre-transform characteristics
  -- makes no Wolf at all -- which is what pins the sample to CR 701.27e's
  -- "immediately after it does so" rather than to the moment the instruction ran.
  Spec.it s "CR 701.27e a permanent alice controls turning into a non-Human creature makes the Wolf" $ do
    cult <- S.printingOf s registry "Cult of the Waxing Moon"
    aang <- S.printingOf s registry "Aang, at the Crossroads"
    let (_, withCult) = S.addPermanent cult S.alice emptyBoard
        (aangId, board) = S.addPermanent aang S.alice withCult
        turned = sweep board
        after = resolveStack (gather turned)
    Spec.assertEqWith s "the trigger resolved into one Wolf" (S.countOnBattlefieldByName wolfToken S.alice after) 1
    Spec.assertEqWith s "no Wolf exists before the trigger resolves" (S.countOnBattlefieldByName wolfToken S.alice turned) 0
    Spec.assertEqWith s "Aang was a Human before the turn and is not one after" (fmap (Set.member Subtype.Human . Projection.subtypesOf aangId) [board, turned]) [True, False]
    Spec.assertEqWith s "and Aang really did turn over" (faceNameOf aangId turned) (Just aangBack)
  -- The same board with the Aang moved one seat: CR 109.5's "you" is the Cult's
  -- controller, so a permanent BOB controls turning over is not a permanent alice
  -- controls. Control is no characteristic (CR 109.3), so it rides the event
  -- beside the sample rather than the snapshot; this board cannot tell the
  -- sampled read from a live one, nothing in data/cards/ having a way to change
  -- control between the turn and the CR 117.5 scan.
  Spec.it s "CR 109.5 a permanent bob controls turning over makes none" $ do
    cult <- S.printingOf s registry "Cult of the Waxing Moon"
    aang <- S.printingOf s registry "Aang, at the Crossroads"
    let (_, withCult) = S.addPermanent cult S.alice emptyBoard
        (aangId, board) = S.addPermanent aang S.bob withCult
        turned = sweep board
        after = resolveStack (gather turned)
    Spec.assertEqWith s "no Wolf" (S.countOnBattlefieldByName wolfToken S.alice after) 0
    Spec.assertEqWith s "bob got none either" (S.countOnBattlefieldByName wolfToken S.bob after) 0
    Spec.assertEqWith s "and the Aang really did turn over, so there WAS an event to decline" (faceNameOf aangId turned) (Just aangBack)
  -- The filter's other characteristic limb, and the mirror of the first case's
  -- timing: Ratchet is a creature on the face it turns FROM and, on an opponent's
  -- turn with CR 702.161a's living metal switched off, not one on the face it
  -- turns INTO. So a match reading the pre-transform characteristics makes a Wolf
  -- here, where the rule makes none.
  Spec.it s "CR 701.27e / 702.161a a permanent turning into a noncreature on bob's turn makes none" $ do
    cult <- S.printingOf s registry "Cult of the Waxing Moon"
    ratchet <- S.printingOf s registry "Ratchet, Field Medic"
    let (_, withCult) = S.addPermanent cult S.alice emptyBoard
        (ratchetId, board) = S.addPermanent ratchet S.alice withCult
        bobsTurn = S.runPure S.identityAnswer board Engine.handoffTurn
        turned = sweep bobsTurn
        after = resolveStack (gather turned)
    Spec.assertEqWith s "no Wolf" (S.countOnBattlefieldByName wolfToken S.alice after) 0
    Spec.assertEqWith s "Ratchet was a creature before the turn and is not one after" (fmap (Set.member CardType.Creature . Projection.cardTypesOf ratchetId) [bobsTurn, turned]) [True, False]
    Spec.assertEqWith s "it really did turn over" (faceNameOf ratchetId turned) (Just ratchetBack)
    Spec.assertEqWith s "and it really is bob's turn" (GameState.activePlayer turned) S.bob
  -- The only board that can tell the SAMPLE from a live read, and the one
  -- Pawl.Types.Transformed's own argument names: a permanent that turns twice
  -- before the CR 117.5 scan. Two turns is one resolution's two clauses -- CR
  -- 701.27f's gate is only for a permanent's own ability, and `sweep` is neither
  -- -- so both events wait for the same scan, at which the board shows the front
  -- face and nothing else.
  --
  -- CR 701.27e reads each event at its own turn, so exactly one of the two is a
  -- turn into a non-Human creature. An arm re-deriving the characteristics at the
  -- scan sees a Human twice over and makes no Wolf at all.
  Spec.it s "CR 701.27e each event is read at its own turn, not at the scan" $ do
    cult <- S.printingOf s registry "Cult of the Waxing Moon"
    aang <- S.printingOf s registry "Aang, at the Crossroads"
    let (_, withCult) = S.addPermanent cult S.alice emptyBoard
        (aangId, board) = S.addPermanent aang S.alice withCult
        turned = sweep (sweep board)
        after = resolveStack (gather turned)
    Spec.assertEqWith s "the one turn into a non-Human made one Wolf" (S.countOnBattlefieldByName wolfToken S.alice after) 1
    Spec.assertEqWith s "and the scan saw a Human, the permanent being back on its front face" (faceNameOf aangId turned) (Just aangFront)

-- CR 603.10's FIRST sentence over an axis CR 109.3 keeps out of an object's
-- characteristics: what was attached to the permanent that turned over. The
-- fixture is Neglected Heirloom // Ashmouth Blade, a {1} Artifact -- Equipment
-- reading "Equipped creature gets +1/+1. When equipped creature transforms,
-- transform this Equipment. Equip {1}", on the Ratchet convertSpec already uses.
--
-- It is the producer because the printed ruling states the outcome outright:
-- "If the equipped creature transforms into a noncreature permanent, Neglected
-- Heirloom will become unattached before it transforms into Ashmouth Blade." So
-- the Equipment falls off AND the ability fires, which is only possible if the
-- condition is checked against the board immediately after the turn rather than
-- against the one the CR 117.5 scan is handed.
--
-- The board is bystanderTransformSpec's CR 702.161a case with the Cult swapped
-- for the Heirloom: on bob's turn living metal is off, so Ratchet, Rescue Racer
-- is an Artifact -- Vehicle and no creature, CR 704.5n unattaches the Equipment,
-- and a live read of `HasAttached IsSource` finds nothing.
--
-- The negative is the same board with the attach left out, which is the ONE
-- thing the two differ in: the Heirloom is on the battlefield either way, Ratchet
-- turns over either way, and the CR 117.5 scan sees an unattached Equipment
-- either way.
equippedTransformSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
equippedTransformSpec s registry = Spec.describe s "EquippedCreatureTransforms" $ do
  Spec.it s "CR 603.10 / 704.5n the Heirloom turns over though the unattach beat the scan" $ do
    heirloom <- S.printingOf s registry "Neglected Heirloom"
    ratchet <- S.printingOf s registry "Ratchet, Field Medic"
    let (ratchetId, heirloomId, board) = heirloomBoard heirloom ratchet True
        bobsTurn = S.runPure S.identityAnswer board Engine.handoffTurn
        turned = sweep bobsTurn
        settled = gather turned
        after = resolveStack settled
    Spec.assertEqWith s "the Heirloom turned over into Ashmouth Blade" (faceNameOf heirloomId after) (Just ashmouthBlade)
    -- The three preconditions the behaviour rests on, read AFTER the assertion
    -- above so none of them can absorb a mutation aimed at it.
    Spec.assertEqWith s "the Equipment was attached when Ratchet turned and unattached by the scan" (fmap (hostOf heirloomId) [turned, settled]) [Just (Recipient.ToCreature ratchetId), Nothing]
    Spec.assertEqWith s "Ratchet was a creature before the turn and is not one after" (fmap (Set.member CardType.Creature . Projection.cardTypesOf ratchetId) [bobsTurn, turned]) [True, False]
    Spec.assertEqWith s "and it really is bob's turn, so living metal is off" (GameState.activePlayer turned) S.bob
  Spec.it s "CR 603.10 an Equipment attached to nothing reads no transform of its own" $ do
    heirloom <- S.printingOf s registry "Neglected Heirloom"
    ratchet <- S.printingOf s registry "Ratchet, Field Medic"
    let (ratchetId, heirloomId, board) = heirloomBoard heirloom ratchet False
        bobsTurn = S.runPure S.identityAnswer board Engine.handoffTurn
        turned = sweep bobsTurn
        after = resolveStack (gather turned)
    Spec.assertEqWith s "the Heirloom is still showing its front face" (faceNameOf heirloomId after) (Just heirloomFront)
    Spec.assertEqWith s "and Ratchet really did turn over, so there WAS an event to decline" (faceNameOf ratchetId turned) (Just ratchetBack)

-- The two names Neglected Heirloom // Ashmouth Blade prints.
heirloomFront, ashmouthBlade :: CardName.CardName
heirloomFront = CardName.MkCardName (Text.pack "Neglected Heirloom")
ashmouthBlade = CardName.MkCardName (Text.pack "Ashmouth Blade")

-- alice's Ratchet with alice's Heirloom beside it, equipped or not. CR 303.4b's
-- attach is S.attach rather than an equip activation: CR 702.6a makes equip a
-- sorcery-speed ability, and the turn is handed to bob before anything turns
-- over.
heirloomBoard :: Printing.Printing -> Printing.Printing -> Bool -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
heirloomBoard heirloom ratchet equipped =
  let (ratchetId, withRatchet) = S.addPermanent ratchet S.alice emptyBoard
      (heirloomId, placed) = S.addPermanent heirloom S.alice withRatchet
   in (ratchetId, heirloomId, if equipped then S.attach heirloomId ratchetId placed else placed)

-- CR 301.5a: what this permanent is attached to, off Object.attachedTo, which is
-- where CR 704.5n's unattach writes.
hostOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe Recipient.Recipient
hostOf oid gs = Game.lookupObject oid gs >>= Object.attachedTo

-- "Destroy each Fungus", which on this board is the Thallid alone -- the
-- Saproling the transform limb made is a Phyrexian Saproling and not one.
destroyEveryFungus :: Effect.Effect card ability
destroyEveryFungus =
  Effect.Destroy
    Destroy.MkDestroy
      { Destroy.ref = ObjectRef.EachMatching (Filter.Type.HasSubtype Subtype.Fungus),
        Destroy.regenerability = Regenerability.Regenerable,
        Destroy.slot = Nothing,
        Destroy.buried = Nothing,
        Destroy.permanents = Nothing
      }

-- `pid` casts that spell and it resolves. CR 601.2i files the SpellWasCast the
-- last-turn tally is folded from; resolving keeps the stack empty so the upkeep
-- step below has only the trigger on it.
castAndResolve :: PlayerId.PlayerId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
castAndResolve pid oid gs =
  let cast = S.runPure S.castAnswer gs (S.cast pid oid)
   in S.runPure S.castAnswer cast Stack.resolveTop

-- CR 603.2b / 603.4: Daybreak Ranger // Nightfall Predator's two upkeep triggers,
-- whose intervening "if" reads how many spells each player cast LAST turn.
--
-- The card is an Innistrad werewolf and carries NO daybound or nightbound
-- keyword, so it is not on CR 731's day/night road at all: GameState.daytime
-- stays Nothing, CR 502.2's untap check returns immediately, and every flip below
-- is the printed trigger's doing. That is a fixture constraint -- a daybound
-- permanent on this board would hand the flips to Pawl.Engine.Daytime -- so the
-- first case asserts the designation is absent.
--
-- Fog is the spell cast throughout: {G}, an instant, targetless, and its only
-- effect is a combat-damage replacement on a board that never reaches combat. So
-- "bob cast a spell" is the only thing a cast contributes.
spellsCastLastTurnSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
spellsCastLastTurnSpec s registry = Spec.describe s "SpellsCastLastTurn" $ do
  -- The snapshot the four reads above rest on, asserted at the state level: the
  -- handoff records a count PER SEAT, and CR 502.2's one-player scalar keeps
  -- answering about the outgoing active player alone. A unit-level fence, not the
  -- proof -- the case above is that.
  Spec.it s "CR 608.2i the handoff records what each player cast, per seat" $ do
    forest <- S.printingOf s registry "Forest"
    fog <- S.printingOf s registry "Fog"
    let withLands = S.landsFor forest S.bob 1 emptyBoard
        (bobFog, board) = S.addHandCard fog S.bob withLands
        handed = Engine.beginTurnOf S.bob (castAndResolve S.bob bobFog board)
    -- SPARSE: alice cast nothing and so has no entry at all, which every reader
    -- takes for 0.
    Spec.assertEqWith s "bob cast one during alice's turn and alice cast none" (GameState.castsLastTurn handed) (Map.fromList [(S.bob, 1)])
    Spec.assertEqWith s "and CR 502.2's scalar still answers about alice alone" (GameState.spellsCastLastTurn handed) 0

-- The two names Ratchet, Field Medic // Ratchet, Rescue Racer prints. CR 701.28
-- and CR 702.161's group reads both.
ratchetFront, ratchetBack :: CardName.CardName
ratchetFront = CardName.MkCardName (Text.pack "Ratchet, Field Medic")
ratchetBack = CardName.MkCardName (Text.pack "Ratchet, Rescue Racer")

-- Everything the two Ratchet faces disagree about, read through the projection:
-- name, power and toughness, subtypes and CARD TYPES. The last is what CR
-- 702.161a moves and the first three are what CR 701.28a moves, so one tuple
-- covers both rules and a change reaching only one of them cannot pass.
ratchetReadings ::
  ObjectId.ObjectId ->
  GameState.GameState ->
  (Set.Set CardName.CardName, Maybe (Integer, Integer), Set.Set Subtype.Subtype, Set.Set CardType.CardType)
ratchetReadings oid gs =
  ( Projection.namesOf oid gs,
    S.powerToughnessOf oid gs,
    Projection.subtypesOf oid gs,
    Projection.cardTypesOf oid gs
  )

-- alice's Ratchet with its BACK face up, which is the only board its printed
-- convert trigger functions on. Written onto Object.face directly rather than
-- played out, which keeps these cases about CR 701.28a and nothing else: one of
-- the two printed roads to a back-face-up Ratchet now exists -- CR 712.11a's cast
-- "converted", which moreThanMeetsTheEyeSpec below plays out of a hand -- and
-- routing the fixture through it would make every case here also a case about
-- casting. The other road, the front face's own optional convert, is played out
-- in data/scenarios/transform/, and routing this fixture through THAT one would
-- make every case here a case about gaining life as well.
--
-- Object.turnedOverAt is deliberately left unset. CR 701.27f -- which CR 701.28e
-- restates for convert -- ignores an instruction from an ability of a permanent
-- that has already turned over since that ability was put onto the stack, so a
-- fixture that stamped one would gate the very behaviour these cases assert.
racerBoard :: Printing.Printing -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
racerBoard ratchet gs =
  let (oid, placed) = S.addPermanent ratchet S.alice gs
      showBack o = o {Object.face = Just ratchetBack}
   in (oid, placed {GameState.objects = Map.adjust showBack oid (GameState.objects placed)})

-- CR 701.28's convert and CR 702.161a's living metal, on the one printed card
-- that carries both with nothing else pawl cannot say: Ratchet, Field Medic //
-- Ratchet, Rescue Racer. Its front face is a {2}{W} 2/4 Legendary Artifact
-- Creature -- Robot with lifelink; its back face a 1/4 Legendary Artifact --
-- Vehicle with lifelink, living metal, and "Whenever one or more nontoken
-- artifacts you control are put into a graveyard from the battlefield, convert
-- Ratchet. This ability triggers only once each turn."
--
-- The front face's third ability -- "Whenever you gain life, you may convert
-- Ratchet. When you do, return target artifact card with mana value less than or
-- equal to the amount of life you gained this turn from your graveyard to the
-- battlefield tapped" -- IS transcribed, so the card is now whole: nothing on
-- either face is elided. data/scenarios/transform/ is what proves it. More than
-- meets the eye {1}{W} is transcribed too; see moreThanMeetsTheEyeSpec.
convertSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
convertSpec s registry = Spec.describe s "Convert" $ do
  -- THE proving case. CR 701.28a: "To convert a permanent, turn it so that its
  -- other face is up."
  --
  -- Played out from the card's own text: alice's Icehide Golem, a nontoken
  -- artifact creature, takes CR 704.5g lethal damage, the CR 704.3 pass buries
  -- it, and the back face's trigger resolves. Every characteristic reader is
  -- asked before and after, so an engine that turned the permanent over for some
  -- of them and not others fails here rather than in one narrow assertion.
  Spec.it s "CR 701.28a the Racer's own trigger converts it, and every reader sees the front face" $ do
    ratchet <- S.printingOf s registry "Ratchet, Field Medic"
    golem <- S.printingOf s registry "Icehide Golem"
    let (ratchetId, withRatchet) = racerBoard ratchet emptyBoard
        (golemId, board) = S.addPermanent golem S.alice withRatchet
        settled = S.runPure S.identityAnswer (S.markDamage golemId 2 board) Engine.settleForPriority
        after = S.runPure S.identityAnswer settled Stack.resolveTop
    Spec.assertEqWith s "after: Ratchet, Field Medic" (ratchetReadings ratchetId after) ratchetFrontReadings
    Spec.assertEqWith s "before: Ratchet, Rescue Racer" (ratchetReadings ratchetId board) ratchetBackReadings
    -- CR 701.28a happens at RESOLUTION, so the death alone is not what turns the
    -- permanent over.
    Spec.assertEqWith s "still the back face while the trigger is on the stack" (ratchetReadings ratchetId settled) ratchetBackReadings
    Spec.assertEqWith s "the Golem died" (Maybe.isNothing (Game.lookupObject golemId settled)) True
    Spec.assertEqWith s "and exactly one trigger reached the stack" (length (GameState.stack settled)) 1
  -- CR 701.27b, which CR 701.28b restates for convert: turning over is its own
  -- game action, so it records CR 701.27a's event rather than a second one of its
  -- own -- the same GameEvent.Transformed a transform writes, which is what makes
  -- CR 701.27e's "transforms into" trigger fire on a convert.
  Spec.it s "CR 701.27e / 701.28a a convert records the transform event, not one of its own" $ do
    ratchet <- S.printingOf s registry "Ratchet, Field Medic"
    golem <- S.printingOf s registry "Icehide Golem"
    let (ratchetId, withRatchet) = racerBoard ratchet emptyBoard
        (golemId, board) = S.addPermanent golem S.alice withRatchet
        settled = S.runPure S.identityAnswer (S.markDamage golemId 2 board) Engine.settleForPriority
        after = S.runPure S.identityAnswer settled Stack.resolveTop
        transformedInto =
          Maybe.mapMaybe
            ( \event -> case event of
                GameEvent.Transformed t | Transformed.object t == ratchetId -> Just (PC.names (Transformed.characteristics t))
                _ -> Nothing
            )
            (S.eventsOf after)
    Spec.assertEqWith s "the convert recorded a Transformed naming the face it landed on" transformedInto [Set.singleton ratchetFront]
    Spec.assertEqWith s "and the permanent really is on that face" (faceNameOf ratchetId after) (Just ratchetFront)
  -- CR 702.161a: "During your turn, this permanent is an artifact creature in
  -- addition to its other types."
  --
  -- ONE board, handed over to bob. That is what tells the rule from a type
  -- granted outright: a permanent that were simply an artifact creature would
  -- read the same on alice's turn and differ on bob's.
  --
  -- "In addition to its other types" is the second half, and the Vehicle subtype
  -- surviving on both turns is what proves it -- CR 205.1b, the same reading crew
  -- gets.
  Spec.it s "CR 702.161a living metal makes the Vehicle a creature during its controller's turn only" $ do
    ratchet <- S.printingOf s registry "Ratchet, Field Medic"
    let (ratchetId, board) = racerBoard ratchet emptyBoard
        bobsTurn = S.runPure S.identityAnswer board Engine.handoffTurn
    Spec.assertEqWith
      s
      "during alice's turn it is an artifact creature"
      (Projection.cardTypesOf ratchetId board)
      (Set.fromList [CardType.Artifact, CardType.Creature])
    Spec.assertEqWith
      s
      "during bob's turn it is an artifact and not a creature"
      (Projection.cardTypesOf ratchetId bobsTurn)
      (Set.singleton CardType.Artifact)
    Spec.assertEqWith s "it is a Vehicle on both, the types being ADDED" (Projection.subtypesOf ratchetId bobsTurn) (Set.singleton Subtype.Vehicle)
    Spec.assertEqWith s "bob's turn began" (GameState.activePlayer bobsTurn) S.bob
    Spec.assertEqWith s "and nothing turned the permanent over" (faceNameOf ratchetId bobsTurn) (Just ratchetBack)
  -- The control the case above cannot supply on its own: the FRONT face is a
  -- printed artifact creature, so it stays one on bob's turn. Without this leg
  -- "not a creature during an opponent's turn" could be an answer about the card
  -- rather than about living metal, which only the back face has.
  Spec.it s "CR 702.161a the front face, which has no living metal, is a creature on either turn" $ do
    ratchet <- S.printingOf s registry "Ratchet, Field Medic"
    let (ratchetId, board) = S.addPermanent ratchet S.alice emptyBoard
        bobsTurn = S.runPure S.identityAnswer board Engine.handoffTurn
    Spec.assertEqWith
      s
      "the front face is an artifact creature during bob's turn"
      (Projection.cardTypesOf ratchetId bobsTurn)
      (Set.fromList [CardType.Artifact, CardType.Creature])
    Spec.assertEqWith s "and it is the front face that is up" (faceNameOf ratchetId bobsTurn) (Just ratchetFront)

-- The two readings ratchetReadings answers with, one per face. The back face's
-- card types are alice's-turn ones: CR 702.161a is why a Vehicle reads as an
-- artifact creature there, and its own case above is what proves that.
ratchetFrontReadings, ratchetBackReadings :: (Set.Set CardName.CardName, Maybe (Integer, Integer), Set.Set Subtype.Subtype, Set.Set CardType.CardType)
ratchetFrontReadings = (Set.singleton ratchetFront, Just (2, 4), Set.singleton Subtype.Robot, Set.fromList [CardType.Artifact, CardType.Creature])
ratchetBackReadings = (Set.singleton ratchetBack, Just (1, 4), Set.singleton Subtype.Vehicle, Set.fromList [CardType.Artifact, CardType.Creature])

-- The two names Baithook Angler // Hook-Haunt Drifter prints. CR 702.146's group
-- reads both.
anglerFront, anglerBack :: CardName.CardName
anglerFront = CardName.MkCardName (Text.pack "Baithook Angler")
anglerBack = CardName.MkCardName (Text.pack "Hook-Haunt Drifter")

-- CR 702.146a: disturb, on Baithook Angler // Hook-Haunt Drifter -- a {1}{U} 2/1
-- Creature -- Human Peasant whose entire front-face text is "Disturb {1}{U}",
-- against a 1/2 Creature -- Spirit back face with flying and "If Hook-Haunt
-- Drifter would be put into a graveyard from anywhere, exile it instead" (Oracle
-- text checked on Scryfall, 2026-09-13). Chosen for that emptiness: of the
-- disturb printings it is the one whose front face prints nothing else, so every
-- reading below is rule 702.146a alone.
--
-- More than meets the eye's near twin, one clause apart, and the clause is the
-- ZONE. Both rules put the BACK face on the stack (CR 712.11a) through the same
-- CR 712.11d exception, and CR 712.13 carries it onto the battlefield, which rule
-- 702.146b states again in its own words. Where rule 702.162a names no zone, rule
-- 702.146a names the graveyard -- so the disturb cast is a CR 601.3 permission as
-- well as a cost, and neither half reaches the front face.
--
-- TWO ISLANDS, which pay the disturb {1}{U} and the printed {1}{U} alike: the two
-- costs are equal on this printing, deliberately, so mana cannot be what tells
-- the graveyard cast from the hand cast and the FACE has to be.
disturbSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
disturbSpec s registry = Spec.describe s "Disturb" $ do
  Spec.it s "CR 702.146a from her graveyard alice may cast only the transformed half, and it arrives with its back face up" $ do
    angler <- S.printingOf s registry "Baithook Angler"
    island <- S.printingOf s registry "Island"
    let -- alice active in her own precombat main phase, holding priority. What
        -- S.handOne bakes in for a hand and what a graveyard board has to say
        -- for itself; without it CR 117.1a's sorcery window is shut and every
        -- reading below is about the phase rather than about rule 702.146a.
        onTurn gs = gs {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
        lands = S.landsInPlay island 2
        (buried, graveyardBoard) = fmap onTurn (S.addGraveyardCard angler S.alice lands)
        (held, handBoard) = fmap onTurn (S.addHandCard angler S.alice lands)
        -- Which halves of that object the player is actually offered, CR
        -- 712.11d's own reading: an assertion on Cost.costsFor alone would pass
        -- on a face nobody could propose.
        offeredNames oid gs =
          List.sort
            ( Maybe.mapMaybe
                ( \action -> case action of
                    A.Cast o n _ | o == oid -> Just n
                    _ -> Nothing
                )
                (Action.legalActions S.alice gs)
            )
        resolved = S.runPure S.identityAnswer (S.runPure S.identityAnswer graveyardBoard (Cast.castSpell S.manaPerformer S.alice buried anglerBack Facing.FaceUp)) Stack.resolveTop
        -- The permanent found by the CARD behind it rather than by a name, since
        -- which name it answers to is the thing under test.
        anglerIn gs = filter (\o -> fmap S.nameOf (Game.cardOf o gs) == Just (S.printingName angler)) (Game.zoneMembers Zone.Battlefield S.alice gs)
    Spec.assertEqWith s "CR 712.11a / 702.146b the disturbed spell arrives with its back face up: the 1/2 Spirit, not the 2/1 Human" (fmap (\o -> (Projection.namesOf o resolved, S.powerToughnessOf o resolved)) (anglerIn resolved)) [(Set.singleton anglerBack, Just (1, 2))]
    Spec.assertEqWith s "CR 702.146a / 712.11d the graveyard offers the back face and only it" (offeredNames buried graveyardBoard) [anglerBack]
    Spec.assertEqWith s "CR 712.11 her hand offers the front face and only it, the same two Islands paying either cost" (offeredNames held handBoard) [anglerFront]
  -- CR 613.1f in a graveyard: Yixlid Jailer ("Cards in graveyards lose all
  -- abilities.", api.scryfall.com 2026-09-30, whose ruling names a card's own
  -- flashback as stopped) takes disturb off the buried front face, so CR 601.3
  -- leaves nothing allowing the cast. A pair of boards differing only in
  -- whether bob's Jailer is in play.
  Spec.it s "CR 702.146a / 613.1f under Yixlid Jailer a buried Baithook Angler offers no disturb cast" $ do
    angler <- S.printingOf s registry "Baithook Angler"
    island <- S.printingOf s registry "Island"
    jailer <- S.printingOf s registry "Yixlid Jailer"
    let onTurn gs = gs {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
        (buried, without) = fmap onTurn (S.addGraveyardCard angler S.alice (S.landsInPlay island 2))
        underJailer = snd (S.addPermanent jailer S.bob without)
        offeredNames gs =
          Maybe.mapMaybe
            ( \action -> case action of
                A.Cast o n _ | o == buried -> Just n
                _ -> Nothing
            )
            (Action.legalActions S.alice gs)
    Spec.assertEqWith s "CR 613.1f under the Jailer the graveyard offers no cast" (offeredNames underJailer) []
    Spec.assertEqWith s "CR 702.146a without it the transformed cast is offered" (offeredNames without) [anglerBack]
  -- The same rule from the other side: a disturb GRANTED. Synthetic Restless
  -- Vigil {2}{U} Enchantment, "Creature cards in your graveyard have disturb
  -- {1}{U}", over Thraben Gargoyle {1} 2/2 // Stonewing Antagonizer 4/2, which
  -- prints no disturb. CR 613.1f gives the buried front face the ability, and CR
  -- 712.11d lets it reach the back face. A pair of boards differing only in the
  -- Vigil; two Islands pay the granted {1}{U}, and the Gargoyle's own {1} is no
  -- route out of a graveyard at all.
  Spec.it s "CR 702.146a a disturb granted to a card in a graveyard casts it transformed" $ do
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    island <- S.printingOf s registry "Island"
    vigil <- S.printingOf s registry "Synthetic Restless Vigil"
    let onTurn gs = gs {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
        (buried, without) = fmap onTurn (S.addGraveyardCard gargoyle S.alice (S.landsInPlay island 2))
        granted = snd (S.addPermanent vigil S.alice without)
        antagonizer = CardName.MkCardName (Text.pack "Stonewing Antagonizer")
        offeredNames gs = [n | A.Cast o n _ <- Action.legalActions S.alice gs, o == buried]
        resolved = S.runPure S.identityAnswer (S.runPure S.identityAnswer granted (Cast.castSpell S.manaPerformer S.alice buried antagonizer Facing.FaceUp)) Stack.resolveTop
        gargoyleIn gs = filter (\o -> fmap S.nameOf (Game.cardOf o gs) == Just (S.printingName gargoyle)) (Game.zoneMembers Zone.Battlefield S.alice gs)
    Spec.assertEqWith s "CR 712.11a / 702.146b it arrives back face up: the 4/2 Antagonizer" (fmap (\o -> (Projection.namesOf o resolved, S.powerToughnessOf o resolved)) (gargoyleIn resolved)) [(Set.singleton antagonizer, Just (4, 2))]
    Spec.assertEqWith s "CR 613.1f the Vigil offers the back face, and without it the graveyard offers nothing" (offeredNames granted, offeredNames without) ([antagonizer], [])

-- CR 702.162a: more than meets the eye, on the same Ratchet, Field Medic //
-- Ratchet, Rescue Racer the convert group runs on -- "More Than Meets the Eye
-- {1}{W}" on its front face, against a printed {2}{W}.
--
-- The rule has two limbs and a case that proves one is half a proof. The first is
-- a CHEAPER COST (CR 118.9, CR 601.2b's alternative-cost announcement); the
-- second is that what arrives is the BACK FACE (CR 712.11a: "if a double-faced
-- card ... is cast as a spell 'transformed' or 'converted,' it's put on the stack
-- with its back face up"), which CR 712.13 then carries onto the battlefield. So
-- both are asserted, and the cast for the PRINTED cost is the control that tells
-- the permission from the timing class: the same card, the same board, the same
-- seat, and a front-face permanent at the end of it.
--
-- CR 712.11d is what makes the converted cast an ordinary legal action rather
-- than something an effect has to offer: "if an ability of a double-faced card's
-- front face allows it to be cast ... 'converted,' that ability is also
-- considered when evaluating that spell to determine if it can be cast." The
-- action-list assertion is the one that reads that -- without it the two
-- played-out casts below would pass on Pawl.Engine.Cast.castSpell being handed a
-- face name directly, which no player ever does.
moreThanMeetsTheEyeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
moreThanMeetsTheEyeSpec s registry = Spec.describe s "MoreThanMeetsTheEye" $ do
  Spec.it s "CR 702.162a / 712.11a Ratchet cast converted costs {1}{W} and arrives with its back face up" $ do
    ratchet <- S.printingOf s registry "Ratchet, Field Medic"
    plains <- S.printingOf s registry "Plains"
    -- THREE Plains, so both costs are payable on ONE board: mana, seats, timing
    -- and stock cannot be the difference between the two casts below, and the
    -- {1}{W} is proved by what is left untapped rather than by what was affordable.
    let (board, oid) = S.handOne ratchet (S.landsInPlay plains 3)
        castAs name = S.runPure S.identityAnswer board (Cast.castSpell S.manaPerformer S.alice oid name Facing.FaceUp)
        resolveAs name = S.runPure S.identityAnswer (castAs name) Stack.resolveTop
        -- The Ratchet permanent among alice's lands, found by the card behind it
        -- rather than by a name, since which name it answers to is the thing
        -- under test.
        ratchetIn gs = filter (\o -> fmap S.nameOf (Game.cardOf o gs) == Just (S.printingName ratchet)) (Game.zoneMembers Zone.Battlefield S.alice gs)
        readingsIn gs = fmap (\o -> ratchetReadings o gs) (ratchetIn gs)
    Spec.assertEqWith s "converted: the back face is the permanent" (readingsIn (resolveAs ratchetBack)) [ratchetBackReadings]
    Spec.assertEqWith s "printed cost: the front face is the permanent" (readingsIn (resolveAs ratchetFront)) [ratchetFrontReadings]
    Spec.assertEqWith s "converted: {1}{W} tapped two of the three Plains" (S.tappedCount S.alice (castAs ratchetBack)) 2
    Spec.assertEqWith s "printed cost: {2}{W} tapped all three" (S.tappedCount S.alice (castAs ratchetFront)) 3
    -- CR 712.11d, and the reason CR 702.162a is a keyword rather than a rider on
    -- some opcode: the converted cast is offered to the player from the hand,
    -- beside the ordinary one.
    Spec.assertEqWith
      s
      "both casts are legal actions, the converted one included"
      ( List.sort
          ( Maybe.mapMaybe
              ( \action -> case action of
                  A.Cast o n _ | o == oid -> Just n
                  _ -> Nothing
              )
              (Action.legalActions S.alice board)
          )
      )
      (List.sort [ratchetFront, ratchetBack])
  -- CR 118.9a from the other side, and the pair the case above cannot make. An
  -- offered cast (CR 608.2g) that states NO alternative cost of its own leaves
  -- rule 702.162a's the only one the spell could take, so the converted face is
  -- still on offer and still costs {1}{W}; an offer that DOES state one --
  -- "without paying its mana cost" -- takes the spell's single alternative, so CR
  -- 712.11's default front face is the whole answer. Pawl.InvestigateSpec's "CR
  -- 118.9a a free offer does not also offer the converted face" plays the second
  -- of those out of a printed card, Wild Evocation.
  --
  -- ONE board and one field apart, CastOffer.withoutPayingManaCost: same seat,
  -- same card, same three Plains, same answerer asking for the back face.
  --
  -- The offer is BUILT here rather than played off a card: the field under test is
  -- CastOffer.withoutPayingManaCost, and a hand-built pair differing in that field
  -- alone is what isolates it. Harness the Storm is the printed producer nearest
  -- to it -- its slot names an instant or sorcery card sharing a name with the
  -- spell just cast, and every card printing the keyword is an artifact creature
  -- (Scryfall `keyword:"More Than Meets the Eye"`, 2026-08-28, fifteen cards,
  -- every front face Legendary Artifact Creature -- Robot). A printing whose
  -- alternative-free offer could name a converted face would be the card that
  -- replaces this construction.
  Spec.it s "CR 118.9a an offer stating no alternative cost still reaches the converted face" $ do
    ratchet <- S.printingOf s registry "Ratchet, Field Medic"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    let (handed, oid) = S.handOne ratchet (S.landsInPlay plains 3)
        -- Something for the offer to resolve alongside: Resolve.offerCast takes
        -- the objects the opcode's ObjectRef named, so the Ratchet is handed in
        -- directly. A spell sits on the stack so the board is one a resolution
        -- could be happening on; which spell is immaterial -- it is never
        -- resolved.
        (_, board) = S.spellOnStack piker S.alice handed
        -- Asks for the BACK face wherever CR 601.3's choice is put, and takes
        -- every offer. Resolve.offerCast rejects rather than repairs, so the leg
        -- that stops offering that face casts the front one instead of nothing.
        -- The OBJECT comes off the prompt, every option being a half of the one
        -- card handed in.
        wantingBack :: Prompt.Prompt r -> r
        wantingBack p = case p of
          Prompt.ChooseOfferedCastSpell _ _ options -> (fst (NonEmpty.head options), ratchetBack)
          Prompt.OfferedCast {} -> OptionalDecision.Exercises
          _ -> S.identityAnswer p
        offered = CastOffer.MkCastOffer {CastOffer.transformed = False, CastOffer.withoutPayingManaCost = False, CastOffer.payingInstead = Nothing, CastOffer.spending = ManaSpending.AsProduced, CastOffer.restriction = Nothing, CastOffer.offeredBy = Nothing}
        -- No restriction on either offer, so the context's source is immaterial
        -- here: what this group is about is which FACE a free offer reaches.
        under o = S.runPure wantingBack board (Resolve.offerCast Resolve.noSubgame (Filter.contextFor (Game.teams board) (Just S.alice) Nothing) (const Nothing) [oid] S.alice CastObligation.Optional PermissionVerb.Cast Nothing CastRepetition.Once False o)
        resolvedUnder o = S.runPure wantingBack (under o) Stack.resolveTop
        free = offered {CastOffer.withoutPayingManaCost = True}
        ratchetIn gs = filter (\o -> fmap S.nameOf (Game.cardOf o gs) == Just (S.printingName ratchet)) (Game.zoneMembers Zone.Battlefield S.alice gs)
        readingsIn gs = fmap (\o -> ratchetReadings o gs) (ratchetIn gs)
    Spec.assertEqWith s "no alternative on the offer: the converted face is reached and arrives" (readingsIn (resolvedUnder offered)) [ratchetBackReadings]
    Spec.assertEqWith s "a free offer reaches the front face, though the back was asked for" (readingsIn (resolvedUnder free)) [ratchetFrontReadings]
    Spec.assertEqWith s "rule 702.162a's {1}{W} was paid for the converted cast" (S.tappedCount S.alice (under offered)) 2
    Spec.assertEqWith s "and the free cast paid nothing" (S.tappedCount S.alice (under free)) 0

-- CR 613.7g: "a double-faced permanent receives a new timestamp each time it
-- transforms or converts". Rule 712.18 says the permanent is not a new object,
-- which settles the id and not the stamp.
--
-- WHAT READS THE STAMP. CR 701.60c's suspected grant is the rulebook's rather
-- than a card's, and Pawl.Engine.Projection.designationGathered gives it the
-- PERMANENT's own timestamp (CR 613.7a), so it is the one layer-6 effect on this
-- board whose order against an ability removal a transform can move. CR 701.60b
-- makes the designation neither an ability nor part of the copiable values, so it
-- rides through the turning-over untouched and only its ORDER changes.
--
-- Daybreak Ranger // Nightfall Predator is the permanent: its front face is a
-- Human, which is what Moonmist's "transform all Humans" reaches, and of the
-- Transform opcodes in `data/cards/` Moonmist's is the only one whose ObjectRef
-- is not `InSlot "self"` -- the only one, that is, that can turn a permanent over
-- without being an ability OF it. That matters here because the removal this case
-- needs has already taken the permanent's own abilities away.
--
-- Humility is the REMOVER, and it has to be a removal that spares SUBTYPES:
-- Pawl.FaceDownSpec's sibling pair uses Turn to Frog, whose "becomes a blue Frog"
-- would leave Moonmist nothing named Human to reach. Humility's "all creatures
-- lose all abilities" is layer 6 like the grant, timestamped when it entered (CR
-- 613.7a), and it enters AFTER the suspect, so before the transform the grant is
-- the older effect and is wiped.
--
-- THE PAIR IS TWO MOMENTS OF ONE BOARD rather than two boards: the suspect loses
-- rule 701.60c's ability to a Humility younger than it, and gets it back when the
-- transform restamps it past that Humility. Nothing else differs, so the flip is
-- the stamp's doing alone. Pawl.CombatSpec's SuspectedAbilityRemoval group is
-- where the "before" reading is proved on its own.
--
-- The second Goblin Piker is the anti-vacuity leg: it stands beside the suspect,
-- was never suspected, and is no Human for Moonmist to turn over, so a board on
-- which nothing can block fails at it rather than passing.
restampSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
restampSpec s registry = Spec.describe s "Timestamps (CR 613.7g)" $ do
  Spec.it s "CR 613.7g transforming restamps the permanent past a removal that had wiped its grant" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    ranger <- S.printingOf s registry "Daybreak Ranger"
    humility <- S.printingOf s registry "Humility"
    moonmist <- S.printingOf s registry "Moonmist"
    forest <- S.printingOf s registry "Forest"
    let (before, suspect, other, attacker) = suspectedRangerBoard piker ranger humility forest
        after = moonmisting moonmist before
    -- THE BEFORE moment, and the fixture's own control: Humility entered after the
    -- suspect, so CR 613.1f wipes the grant and both halves of rule 701.60c go.
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Menace suspect before)) "CR 613.1f before: the removal is younger than the suspect, so the menace half is gone"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton suspect (Set.singleton attacker)) before) "CR 613.1f before: and the can't-block half with it"
    -- THE assertion, gameplay level and ahead of every reading of the projection:
    -- the transform restamped the suspect past Humility, so rule 701.60c's "this
    -- creature can't block" applies again.
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton suspect (Set.singleton attacker)) after)) "CR 613.7g the permanent it transformed is stamped after the removal, so it can't block again"
    Spec.assertBool s (Projection.hasKeyword Keyword.Menace suspect after) "CR 613.7g and the menace half is back with it"
    Spec.assertEqWith s "CR 701.27a Moonmist turned the Human over, so the back face is up" (Projection.namesOf suspect after) (Set.singleton nightfallName)
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton other (Set.singleton attacker)) after) "while the Goblin Piker beside it, suspected of nothing, blocks"

nightfallName :: CardName.CardName
nightfallName = CardName.MkCardName (Text.pack "Nightfall Predator")

-- alice attacks with one Goblin Piker into bob's suspected Daybreak Ranger, a
-- second Piker beside it, two Forests to pay for the Moonmist and a Humility
-- younger than all of them. Returns the board with attackers declared, the
-- suspect, the Piker beside it and alice's attacker.
suspectedRangerBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
suspectedRangerBoard piker ranger humility forest =
  let (gs0, mine, _) = S.combatBoardOf [piker] []
      (suspect, gs1) = S.addPermanent ranger S.bob gs0
      (other, gs2) = S.addPermanent piker S.bob gs1
      gs3 = List.foldl' (\g p -> snd (S.addPermanent p S.bob g)) gs2 [forest, forest]
      gs4 = S.withHumility humility (suspecting suspect gs3)
      declared = S.runPure S.aggressiveAnswer gs4 (Combat.declareAttackers S.manaPerformer S.alice)
      attacker = case mine of
        a : _ -> a
        [] -> S.noSource
   in (declared, suspect, other, attacker)

-- CR 701.60b's designation, written straight onto the permanent -- a FIXTURE.
-- The road that sets it is Reasonable Doubt's, and Pawl.CombatSpec's
-- SuspectedAbilityRemoval group proves it; what this file needs is only that the
-- designation outlives a transform and carries the permanent's own timestamp into
-- layer 6.
suspecting :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
suspecting oid gs =
  gs
    { GameState.objects =
        Map.adjust (\o -> o {Object.designations = Set.insert Designation.Suspected (Object.designations o)}) oid (GameState.objects gs)
    }

-- bob casts one Moonmist, it resolves, and the board settles. CAST rather than
-- put straight onto the stack: Moonmist's text is a MODE, and a spell placed by
-- hand carries no chosen mode for the resolution to run, so its transform would
-- never happen. bob is the caster, since it is his creature the case is about;
-- the two Forests behind it are his.
moonmisting :: Printing.Printing -> GameState.GameState -> GameState.GameState
moonmisting moonmist gs =
  let (oid, withCard) = S.addHandCard moonmist S.bob gs
   in S.runPure S.identityAnswer withCard (S.cast S.bob oid >> Stack.resolveTop >> Engine.settleForPriority)
