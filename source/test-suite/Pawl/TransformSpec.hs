-- Covers CR 701.27 transform end to end: Pawl.Types.Layout's Transforming arm
-- and the three Pawl.Engine.Card functions that read it (CR 712.8a/712.8d's
-- combined view, CR 712.11's castable half, CR 701.27a's turnedOver), the
-- Effect.Transform arm of Pawl.Engine.Resolve, and Pawl.Engine.Game.manaCostFaceOf
-- (CR 712.8e).
--
-- Every case runs against the printed Thraben Gargoyle // Stonewing Antagonizer,
-- a nonmodal double-faced card (CR 712.2) whose front face is a {1} 2/2 Artifact
-- Creature -- Gargoyle with defender and "{6}: Transform this creature", and
-- whose back face is a 4/2 Artifact Creature -- Gargoyle Horror with flying and
-- no text. It was picked because its whole text IS the transform: every
-- characteristic that differs across the two faces is one pawl already reads, so
-- a case that fails here fails about transform and about nothing else.
module Pawl.TransformSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Engine as Engine
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
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
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
  (CardName.CardName, Maybe (Integer, Integer), Set.Set Subtype.Subtype, Bool, Bool, Int)
faceReadings oid gs =
  ( Projection.nameOf oid gs,
    S.powerToughnessOf oid gs,
    Projection.subtypesOf oid gs,
    Projection.hasKeyword Keyword.Defender oid gs,
    Projection.hasKeyword Keyword.Flying oid gs,
    length (Projection.abilitiesOf oid gs)
  )

frontFace, backFace :: (CardName.CardName, Maybe (Integer, Integer), Set.Set Subtype.Subtype, Bool, Bool, Int)
frontFace = (gargoyleName, Just (2, 2), Set.singleton Subtype.Gargoyle, True, False, 1)
backFace = (antagonizerName, Just (4, 2), Set.fromList [Subtype.Gargoyle, Subtype.Horror], False, True, 0)

-- "Transform all creatures", the shape CR 701.27a takes when a spell rather than
-- the permanent's own ability asks -- Moonmist's "transform all Humans" with a
-- wider filter. The two cases below that need a permanent turned over without
-- its own ability use this, because Stonewing Antagonizer prints no way back.
transformEveryCreature :: Effect.Effect card
transformEveryCreature = Effect.Transform (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature))

-- alice and bob, nothing on the battlefield: the base every case here builds on.
emptyBoard :: GameState.GameState
emptyBoard = Setup.emptyGame S.bothPlayers

sweep :: GameState.GameState -> GameState.GameState
sweep gs = S.runPure S.identityAnswer gs (Resolve.applyEffect S.noSource S.noSource S.alice Map.empty Map.empty transformEveryCreature)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Transform" $ do
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
  -- accident: Stonewing Antagonizer prints no mana cost, and CR 202.1's "some
  -- objects have no mana cost" is unpayable rather than free
  -- (Pawl.Engine.Cost.canPay's Nothing arm), so a back face proposed all the way
  -- to the cost gate would be dropped there and the list would read the same.
  -- The falsifier -- Pawl.Engine.Card.castableFaces answering with the whole
  -- NonEmpty, as it does for Split and Adventure -- is caught only by asking
  -- that function directly, so both are here and neither stands alone.
  Spec.it s "CR 712.11 only the front face is offered from a hand" $ do
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    island <- S.printingOf s registry "Island"
    let (gs, _) = S.handOne gargoyle (S.landsInPlay island 1)
        namesOffered = [n | A.Cast _ n <- Action.legalActions S.alice gs]
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
  -- Projection.nameOf, the layer 7b power/toughness fold, the layer 4 subtype
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
    Spec.assertEqWith s "the Goblin Piker did not" (Projection.nameOf pikerId after) (CardName.MkCardName (Text.pack "Goblin Piker"))
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
  -- Turning a permanent over is not a zone change, so CR 400.7 does not fire and
  -- the permanent is the SAME object: it keeps its id, and everything recorded
  -- against that id survives. Damage stands in for the whole per-incarnation set
  -- (Object.newIncarnation's list) because it is the cheapest of them to mark.
  --
  -- Worth asserting because the obvious wrong implementation is the one that
  -- reaches for the funnel every other change of what a permanent IS goes
  -- through: pawl's transform writes one field in place instead.
  Spec.it s "CR 400.7 does not fire: the turned-over permanent is the same object" $ do
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    let (oid, g0) = S.addCreature gargoyle S.alice emptyBoard
        before = S.markDamage oid 1 g0
        after = sweep before
    Spec.assertEqWith s "the back face is up" (faceReadings oid after) backFace
    Spec.assertEqWith s "the 1 damage marked on the Gargoyle is still marked" (S.damageOf oid after) (Just 1)
    Spec.assertEqWith s "and the battlefield holds one permanent, not a replacement" (length (Game.zoneMembers Zone.Battlefield S.alice after)) 1
