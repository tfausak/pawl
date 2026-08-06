-- Covers CR 712.3's modal double-faced cards end to end: Pawl.Types.Layout's
-- ModalDoubleFaced arm and the four Pawl.Engine.Card functions that read it (CR
-- 712.8a's combined view, CR 712.11b's castable faces, CR 712.13's entering
-- face, CR 712.8f's mana-value face), plus the carry-through in
-- Pawl.Engine.Stack that puts CR 712.13's face onto the permanent.
--
-- Every case runs against the printed Birgi, God of Storytelling // Harnfel,
-- Horn of Bounty: a {2}{R} 3/3 Legendary Creature -- God over a {4}{R} Legendary
-- Artifact. It was picked because the two faces share nothing a reader can see
-- -- different name, different cost, different card type, and only one of them
-- has a power and toughness -- so a permanent that entered showing the wrong
-- face is visible in every assertion here rather than in one narrow one.
module Pawl.ModalDoubleFacedSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
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
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Printing as Printing

-- The two names the card prints. CR 712.8a gives the card only its front face's
-- characteristics in a hand, so the second names a face rather than the card --
-- and CR 201.4d is what lets a player name it all the same.
birgiName, harnfelName, mountainName :: CardName.CardName
birgiName = CardName.MkCardName (Text.pack "Birgi, God of Storytelling")
harnfelName = CardName.MkCardName (Text.pack "Harnfel, Horn of Bounty")
mountainName = CardName.MkCardName (Text.pack "Mountain")

-- Everything the two faces disagree about, read through the projection, as one
-- tuple: name, card types, and power/toughness. Asserted whole so a case names
-- the face rather than three independent facts.
faceReadings ::
  ObjectId.ObjectId ->
  GameState.GameState ->
  (CardName.CardName, Set.Set CardType.CardType, Maybe (Integer, Integer))
faceReadings oid gs =
  ( Projection.nameOf oid gs,
    Projection.cardTypesOf oid gs,
    S.powerToughnessOf oid gs
  )

birgiReadings, harnfelReadings :: (CardName.CardName, Set.Set CardType.CardType, Maybe (Integer, Integer))
birgiReadings = (birgiName, Set.singleton CardType.Creature, Just (3, 3))
harnfelReadings = (harnfelName, Set.singleton CardType.Artifact, Nothing)

-- The one permanent alice controls that is not one of her Mountains. The cast
-- spell arrives on the battlefield as a NEW object (CR 400.7), so a case that
-- resolved one cannot follow the id it started with.
nonLand :: GameState.GameState -> [ObjectId.ObjectId]
nonLand gs = [o | o <- Set.toList (GameState.battlefield gs), Projection.nameOf o gs /= mountainName]

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "ModalDoubleFaced" $ do
  -- CR 712.8a: "While a double-faced card is outside the game or in a zone other
  -- than the battlefield or stack, it has only the characteristics of its front
  -- face."
  --
  -- The falsifier is CR 709.4's reading, which is what the pool's Split layout
  -- takes: a combined view would name this card
  -- "Birgi, God of Storytelling//Harnfel, Horn of Bounty", give it both card
  -- types, and price it at the concatenated {2}{R}{4}{R} for mana value 8.
  Spec.it s "CR 712.8a in a hand a modal double-faced card is only its front face" $ do
    birgi <- S.printingOf s registry "Birgi, God of Storytelling"
    let (gs, oid) = S.handOne birgi (Setup.emptyGame S.bothPlayers)
    case Game.faceOf oid gs of
      Nothing -> Spec.assertFailure s "expected a card in hand"
      Just face -> do
        Spec.assertEqWith s "every reader sees Birgi" (faceReadings oid gs) birgiReadings
        Spec.assertEqWith s "mana value 3, not the two faces' 8" (Quantity.manaValueOf face) 3
  -- CR 712.11b: "A player casting a modal double-faced card or a copy of a modal
  -- double-faced card as a spell chooses which face they are casting before
  -- putting it onto the stack."
  --
  -- Asserted TWICE, because the offer list alone cannot tell CR 712.11b from an
  -- accident of pricing: with only three Mountains the {4}{R} face is unpayable
  -- and would be dropped at the cost gate whatever castableFaces answered. So
  -- the three-Mountain board is the discriminator in the other direction --
  -- exactly one face is offered when only one is affordable -- and asking
  -- Pawl.Engine.Card.castableFaces directly is what catches the CR 712.11
  -- reading (front face only) that the pool's Transforming layout takes.
  Spec.it s "CR 712.11b both faces are offered from a hand" $ do
    birgi <- S.printingOf s registry "Birgi, God of Storytelling"
    mountain <- S.printingOf s registry "Mountain"
    let namesOffered n = [c | A.Cast _ c <- Action.legalActions S.alice (fst (S.handOne birgi (S.landsInPlay mountain n)))]
    Spec.assertEqWith
      s
      "the card proposes both of its faces"
      (fmap Face.name (Card.castableFaces (Printing.card birgi)))
      [birgiName, harnfelName]
    Spec.assertEqWith s "five Mountains: both faces" (namesOffered 5) [birgiName, harnfelName]
    Spec.assertEqWith s "three Mountains: only the {2}{R} face" (namesOffered 3) [birgiName]
  -- THE proving case, and the half of it CR 712.13 is about: "By default, a
  -- resolving double-faced spell that becomes a permanent is put onto the
  -- battlefield with the same face up that was face up on the stack."
  --
  -- Cast the BACK face and the artifact is what enters. The falsifier is a
  -- permanent whose face was dropped on the way out of the stack: CR 712.8a's
  -- front face is what every reader would then answer with, so the board would
  -- hold a 3/3 legendary creature named Birgi -- an object that shares no
  -- characteristic at all with the spell that was cast.
  Spec.it s "CR 712.11b/712.13 casting the back face puts the artifact onto the battlefield" $ do
    birgi <- S.printingOf s registry "Birgi, God of Storytelling"
    mountain <- S.printingOf s registry "Mountain"
    let (gs, oid) = S.handOne birgi (S.landsInPlay mountain 5)
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid harnfelName))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    -- CR 712.8f's first half: the SPELL on the stack has only the
    -- characteristics of the face that's up, which is already how a chosen half
    -- reaches the stack (CR 712.11c).
    case GameState.stack cast of
      [top] -> Spec.assertEqWith s "the spell on the stack is Harnfel" (faceReadings top cast) harnfelReadings
      other -> Spec.assertFailure s ("expected one spell on the stack, got " <> show (length other))
    Spec.assertEqWith s "all five Mountains paid {4}{R}" (S.tappedCount S.alice cast) 5
    case nonLand resolved of
      [permId] -> do
        -- CR 712.8f's second half: "or a modal double-faced permanent is on the
        -- battlefield, it has only the characteristics of the face that's up".
        Spec.assertEqWith s "and the permanent is Harnfel" (faceReadings permId resolved) harnfelReadings
        -- The stored half, which is what CR 712.13 actually writes; the readings
        -- above are what it buys.
        Spec.assertEqWith
          s
          "the permanent records the face that was cast"
          (fmap Object.face (Game.lookupObject permId resolved))
          (Just (Just harnfelName))
      other -> Spec.assertFailure s ("expected one nonland permanent, got " <> show (length other))
  -- The same claim from the other side, and not a restatement: an engine that
  -- carried the LAST face rather than the cast one, or that always answered with
  -- the back face, passes the case above and fails here.
  Spec.it s "CR 712.13 casting the front face puts the creature onto the battlefield" $ do
    birgi <- S.printingOf s registry "Birgi, God of Storytelling"
    mountain <- S.printingOf s registry "Mountain"
    let (gs, oid) = S.handOne birgi (S.landsInPlay mountain 5)
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid birgiName))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "three Mountains paid {2}{R}, and two are left" (S.tappedCount S.alice cast) 3
    case nonLand resolved of
      [permId] -> Spec.assertEqWith s "the permanent is Birgi" (faceReadings permId resolved) birgiReadings
      other -> Spec.assertFailure s ("expected one nonland permanent, got " <> show (length other))
  -- CR 712.8f, on the one characteristic CR 712.8e reads off another face for the
  -- NONMODAL kind: "While a modal double-faced spell is on the stack or a modal
  -- double-faced permanent is on the battlefield, it has only the
  -- characteristics of the face that's up." Mana value is a characteristic (CR
  -- 109.3) and 712.8f states no exception, where 712.8e states one -- so the
  -- back face's own {4}{R} is what a Harnfel on the battlefield is worth.
  --
  -- The falsifier is Transforming's arm applied here: the front face's {2}{R}
  -- would read 3. Both readers pawl has are asserted, for the reason
  -- Pawl.TransformSpec's CR 712.8e case gives -- they are two call sites and only
  -- one of them feeds a card's Quantity.
  Spec.it s "CR 712.8f the mana value is read off the face that's up" $ do
    birgi <- S.printingOf s registry "Birgi, God of Storytelling"
    mountain <- S.printingOf s registry "Mountain"
    let (gs, oid) = S.handOne birgi (S.landsInPlay mountain 5)
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid harnfelName))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    case nonLand resolved of
      [permId] -> do
        Spec.assertEqWith s "Harnfel's own {4}{R}, not Birgi's {2}{R}" (fmap Quantity.manaValueOf (Game.manaCostFaceOf permId resolved)) (Just 5)
        Spec.assertEqWith s "and a filter reading a mana value agrees" (Filter.manaValue (Projection.viewOfObject permId resolved)) (Just 5)
      other -> Spec.assertFailure s ("expected one nonland permanent, got " <> show (length other))
  -- CR 712.9: "Only permanents represented by double-faced tokens and
  -- double-faced cards that are not meld cards can transform or convert", which
  -- puts the modal kind on the permitted side alongside the nonmodal one -- and
  -- CR 712.3 says the same from the card's side, "they may have an ability that
  -- allows them to 'transform' or 'convert' on either face."
  --
  -- Birgi prints no such ability, so the instruction is a spell's -- Moonmist's
  -- "transform all Humans" with a wider filter, the same shape
  -- Pawl.TransformSpec applies. What is being asserted is that CR 701.27c does
  -- NOT swallow the instruction here, which is the whole of what the layout
  -- decides.
  Spec.it s "CR 712.9 a modal double-faced permanent turns over like any other" $ do
    birgi <- S.printingOf s registry "Birgi, God of Storytelling"
    mountain <- S.printingOf s registry "Mountain"
    let (gs, oid) = S.handOne birgi (S.landsInPlay mountain 5)
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid birgiName))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        transformAll = Effect.Transform (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature))
        turned = S.runPure S.identityAnswer resolved (Resolve.applyEffect S.noSource S.noSource S.alice Map.empty Map.empty transformAll)
    case nonLand resolved of
      [permId] -> do
        Spec.assertEqWith s "before: the creature face" (faceReadings permId resolved) birgiReadings
        Spec.assertEqWith s "after: the artifact face" (faceReadings permId turned) harnfelReadings
      other -> Spec.assertFailure s ("expected one nonland permanent, got " <> show (length other))
