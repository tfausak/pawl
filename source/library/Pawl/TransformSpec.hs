-- Covers CR 701.27 transform end to end: Pawl.Types.Layout's Transforming arm
-- and the three Pawl.Engine.Card functions that read it (CR 712.8a/712.8d's
-- combined view, CR 712.11's castable half, CR 701.27a's turnedOver), the
-- Effect.Transform arm of Pawl.Engine.Resolve with CR 701.27f's already-turned
-- gate (alreadyTurnedFor over Object.turnedOverAt), and
-- Pawl.Engine.Game.manaCostFaceOf (CR 712.8e).
--
-- Also CR 712.14a's enter-transformed instruction, which reaches the same back
-- face by a different road: Pawl.Types.EntryRiders carries it and
-- Pawl.Engine.Event.changeZoneEntering applies it. See enterTransformedSpec.
--
-- Every case but that group runs against the printed Thraben Gargoyle //
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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone

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

-- "Transform all creatures", the shape CR 701.27a takes when a spell rather than
-- the permanent's own ability asks -- Moonmist's "transform all Humans" with a
-- wider filter. The two cases below that need a permanent turned over without
-- its own ability use this, because Stonewing Antagonizer prints no way back.
transformEveryCreature :: Effect.Effect card
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

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Transform" $ do
  enterTransformedSpec s registry
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
    let (oid, gs) = S.addCreature gargoyle S.alice emptyBoard
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
  Spec.it s "CR 712.11 only the front face is offered from a hand" $ do
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    island <- S.printingOf s registry "Island"
    let (gs, _) = S.handOne gargoyle (S.landsInPlay island 1)
        namesOffered = [n | A.Cast _ n _ <- Action.legalActions S.alice gs]
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
    let (oid, g0) = S.addCreature gargoyle S.alice (S.landsInPlay island 6)
        gs = g0 {GameState.priority = Just S.alice}
    case Activate.abilitiesFor oid gs of
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
    let (oid, g0) = S.addCreature gargoyle S.alice (S.landsInPlay island 12)
        gs = g0 {GameState.priority = Just S.alice}
        activate g = case Activate.abilitiesFor oid g of
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
    let (oid, g0) = S.addCreature gargoyle S.alice (S.landsInPlay island 6)
        gs = g0 {GameState.priority = Just S.alice}
    case Activate.abilitiesFor oid gs of
      [ability] -> do
        let activated = snd (Engine.runGamePure S.identityAnswer gs (Activate.activateAbility S.alice oid ability))
            byItsOwnAbility = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
            thenBySomethingElse = sweepFrom oid byItsOwnAbility
        Spec.assertEqWith s "its own {6} turned it over" (faceReadings oid byItsOwnAbility) backFace
        Spec.assertEqWith s "and a spell turns it straight back, same turn" (faceReadings oid thenBySomethingElse) frontFace
      abilities -> Spec.assertFailure s ("expected one activated ability, got " <> show (length abilities))
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
    let (oid, before) = S.addCreature gargoyle S.alice emptyBoard
        after = sweep before
    Spec.assertEqWith s "front face up: 1" (fmap Quantity.manaValueOf (Game.manaCostFaceOf oid before)) (Just 1)
    Spec.assertEqWith s "it is the back face that is up" (faceReadings oid after) backFace
    Spec.assertEqWith s "back face up: still 1, not the 0 its own empty cost would give" (fmap Quantity.manaValueOf (Game.manaCostFaceOf oid after)) (Just 1)
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
    let (gargoyleId, g0) = S.addCreature gargoyle S.alice emptyBoard
        (pikerId, before) = S.addCreature piker S.alice g0
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
  -- it had its back face up previously".
  --
  -- It takes an outside effect, because Stonewing Antagonizer prints no ability
  -- at all: the case above proves the {6} is gone with the front face.
  Spec.it s "CR 701.27a transforming twice returns the permanent to its front face" $ do
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    let (oid, before) = S.addCreature gargoyle S.alice emptyBoard
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
    let (oid, g0) = S.addCreature gargoyle S.alice emptyBoard
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
-- The producer is Befriending the Moths // Imperial Moth, a Kamigawa: Neon
-- Dynasty Saga whose chapter III reads "Exile this Saga, then return it to the
-- battlefield transformed under your control" -- CR 712.14a's wording on a card
-- rather than on a spell, which is what makes it this rule's producer and not CR
-- 712.13a's. Its back face is a 2/4 white Enchantment Creature -- Insect with
-- flying, and every one of those readings belongs to that face alone: the front
-- face is a Saga enchantment with no power, no toughness, no flying and no
-- creature type. A case that passed with the FRONT face up would fail every line.
enterTransformedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
enterTransformedSpec s registry = Spec.describe s "Entering the battlefield transformed" $ do
  Spec.it s "CR 712.14a a Saga returned transformed comes back as its back face" $ do
    moths <- S.printingOf s registry "Befriending the Moths"
    let (sagaId, base) = S.addCreature moths S.alice emptyBoard
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
