{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 718 and CR 702.160 end to end: Pawl.Types.Keyword's Prototype arm and
-- the Pawl.Types.Prototype frame it carries, the candidate cost
-- Pawl.Engine.Cost.candidateCostsFor offers beside the printed one, the stamp
-- Pawl.Engine.Cast.stampPrototyped writes when CR 601.2b settles on it, the move
-- Pawl.Engine.Event.changeZoneAttaching lets it cross, and the swap
-- Pawl.Engine.Projection.View.withPrototype makes in the seed characteristics.
--
-- Gameplay-level throughout. Autonomous Assembler is the fixture: a {5} colourless
-- 4/5 Artifact Creature -- Assembly-Worker whose inset frame reads "Prototype
-- {1}{W} -- 2/2", plus vigilance and "{1}, {T}: Put a +1\/+1 counter on target
-- Assembly-Worker you control".
--
-- The card is chosen for CR 718.3b's colour clause: it is the only prototype
-- printing whose inset cost is a single colour over a colourless printed cost, so
-- one assertion tells "the cost was swapped" from "the characteristics were". Its
-- numbers are non-degenerate too -- 4\/5 against 2\/2, mana value 5 against 2 --
-- so no reading coincides with another.
module Pawl.PrototypeSpec where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Zone as Zone

-- The two costs the card offers (CR 718.3), written out so every board names the
-- same symbols the card file does.
printedCost, insetCost :: [ManaSymbol.ManaSymbol]
printedCost = [ManaSymbol.Generic 5]
insetCost = [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.White)]

-- CR 601.2b's announcement answered by NAMING a cost rather than an index: an
-- answerer that searched the offered list for a legal option would find the right
-- one again after a mutation.
castingFor :: [ManaSymbol.ManaSymbol] -> Prompt.Prompt r -> r
castingFor wanted p = case p of
  Prompt.ChooseCost _ _ _ candidates ->
    Maybe.fromMaybe (Cost.firstOffered candidates) (List.find ((== Just (ManaCost.MkManaCost wanted)) . Cost.Type.mana) candidates)
  _ -> S.identityAnswer p

-- The two answerers every discriminating pair below differs by, and the only thing
-- they differ by: Autonomous Assembler's inset {1}{W} against its printed {5}.
prototyping, payingPrinted :: Prompt.Prompt r -> r
prototyping = castingFor insetCost
payingPrinted = castingFor printedCost

topOfStack :: GameState.GameState -> Maybe ObjectId.ObjectId
topOfStack gs = case GameState.stack gs of
  oid : _ -> Just oid
  [] -> Nothing

-- The permanent that printing became, found by NAME rather than by the
-- characteristics under test: a cast that resolved and one that is still on the
-- stack must both be found, or the assertions would compare two Nothings.
assemblerOn :: Printing.Printing -> GameState.GameState -> Maybe ObjectId.ObjectId
assemblerOn printing gs =
  List.find
    (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (S.printingName printing))
    (Game.zoneMembers Zone.Battlefield S.alice gs)

-- CR 302.6: the permanent a resolved spell became is summoning sick, and the
-- Assembler's ability costs {T}. Named here rather than hidden in a fixture,
-- because the ability case below rests on it: the behaviour under test is CR
-- 718.5's "keeps its abilities", not rule 302.6.
settleUnderAlice :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
settleUnderAlice oid gs =
  gs
    { GameState.objects =
        Map.adjust (\o -> o {Object.sickness = Sickness.Settled S.alice}) oid (GameState.objects gs)
    }

-- CR 601.2c / CR 602.2b answered by FILTERING the offered set down to one
-- recipient rather than building one: a hand-built Recipient.ToObject of the same
-- object is a different recipient, and CR 608.2b's re-read drops it silently.
pinTarget :: Recipient.Recipient -> Prompt.Prompt r -> r
pinTarget recipient p = case p of
  Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, offered) -> Set.filter (== recipient) offered) asked
  _ -> S.identityAnswer p

-- Resolve the top of the stack and settle back to priority.
resolveOne :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
resolveOne answer gs = snd (Engine.runGamePure answer gs (Stack.resolveTop >> Engine.settleForPriority))

-- Every permanent named for that printing alice controls -- a LIST, so a copy
-- that never arrived and a copy that arrived with the wrong values are different
-- readings.
assemblersOn :: Printing.Printing -> GameState.GameState -> [ObjectId.ObjectId]
assemblersOn printing gs =
  filter
    (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (S.printingName printing))
    (Game.zoneMembers Zone.Battlefield S.alice gs)

plusOnePlusOne :: CounterKind.CounterKind Keyword.Keyword
plusOnePlusOne = CounterKind.PlusOnePlusOne

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Prototype" $ do
  -- CR 718.3b, the case this unit exists for: "both a prototyped spell and the
  -- permanent it becomes have only its alternative set of power, toughness, and
  -- mana cost characteristics. If that mana cost includes one or more colored
  -- mana symbols, the spell and the permanent it becomes are also that color or
  -- colors."
  --
  -- The COLOUR is the half that discriminates. An implementation that merely let
  -- the player pay {1}{W} and changed nothing else reaches the same battlefield,
  -- the same 4/5 and -- if it read the announced cost back -- even the same mana
  -- value; it cannot reach white. So the colour assertion comes first, ahead of
  -- every number.
  Spec.it s "CR 718.3b cast prototyped the Assembler is a white 2/2 with mana value 2; cast normally a colourless 4/5 with mana value 5" $ do
    assembler <- S.printingOf s registry "Autonomous Assembler"
    plains <- S.printingOf s registry "Plains"
    let (board, spellId) = S.handOne assembler (S.landsInPlay plains 5)
        prototyped = S.runPure prototyping board (S.cast S.alice spellId)
        printed = S.runPure payingPrinted board (S.cast S.alice spellId)
    -- Both candidates really are offered from the hand, so neither leg passes for
    -- want of the other, and the printed one comes FIRST (CR 118.9a).
    Spec.assertEqWith
      s
      "CR 702.160a: the printed cost and the inset cost are both offered from the hand"
      (fmap Cost.Type.mana (Cost.costsFor S.alice (S.printingName assembler) spellId board))
      [Just (ManaCost.MkManaCost printedCost), Just (ManaCost.MkManaCost insetCost)]
    -- CR 718.3b's colour clause, read off the SPELL on the stack.
    Spec.assertEqWith
      s
      "CR 718.3b: the prototyped spell is white"
      (fmap (\oid -> Projection.colorsOf oid prototyped) (topOfStack prototyped))
      (Just (Set.singleton Color.White))
    Spec.assertEqWith
      s
      "CR 202.2b: cast for its printed {5} the same spell is colourless"
      (fmap (\oid -> Projection.colorsOf oid printed) (topOfStack printed))
      (Just Set.empty)
    -- CR 718.3b's box and cost, on the stack.
    Spec.assertEqWith
      s
      "CR 718.3b: the prototyped spell is a 2/2"
      (topOfStack prototyped >>= \oid -> S.powerToughnessOf oid prototyped)
      (Just (2, 2))
    Spec.assertEqWith
      s
      "CR 718.3b: and its mana value is 2, not the printed 5"
      (fmap (\oid -> PC.manaValue (Projection.project oid prototyped)) (topOfStack prototyped))
      (Just (Just 2))
    Spec.assertEqWith
      s
      "CR 718.3b: its mana cost is the inset {1}{W}"
      (fmap (\oid -> PC.manaCost (Projection.project oid prototyped)) (topOfStack prototyped))
      (Just (Just (ManaCost.MkManaCost insetCost)))
    Spec.assertEqWith
      s
      "CR 718.4: cast normally it is the printed 4/5"
      (topOfStack printed >>= \oid -> S.powerToughnessOf oid printed)
      (Just (4, 5))
    Spec.assertEqWith
      s
      "CR 718.4: with mana value 5"
      (fmap (\oid -> PC.manaValue (Projection.project oid printed)) (topOfStack printed))
      (Just (Just 5))
    -- CR 718.3b's "and the permanent it becomes": the same four readings once the
    -- spell has resolved, which is the half CR 718.2's "while it is a permanent on
    -- the battlefield" adds over every other alternative-cost keyword.
    let resolvedPrototyped = S.settleSba (S.runPure prototyping prototyped Stack.resolveTop)
        resolvedPrinted = S.settleSba (S.runPure payingPrinted printed Stack.resolveTop)
    Spec.assertEqWith
      s
      "CR 718.3b: the permanent it becomes is white too"
      (fmap (\oid -> Projection.colorsOf oid resolvedPrototyped) (assemblerOn assembler resolvedPrototyped))
      (Just (Set.singleton Color.White))
    Spec.assertEqWith
      s
      "and a 2/2"
      (assemblerOn assembler resolvedPrototyped >>= \oid -> S.powerToughnessOf oid resolvedPrototyped)
      (Just (2, 2))
    Spec.assertEqWith
      s
      "CR 718.4: the permanent the printed cast became is a colourless 4/5"
      ( (,)
          <$> fmap (\oid -> Projection.colorsOf oid resolvedPrinted) (assemblerOn assembler resolvedPrinted)
          <*> (assemblerOn assembler resolvedPrinted >>= \oid -> S.powerToughnessOf oid resolvedPrinted)
      )
      (Just (Set.empty, (4, 5)))
  -- CR 718.5: "a prototype card's characteristics other than its power,
  -- toughness, and mana cost (and other than color) remain the same whether it
  -- was cast as a prototyped spell or cast normally." Vigilance and the printed
  -- activated ability are what the card has to show for it, and the ability is
  -- driven to RESOLUTION rather than merely offered -- a permanent that lost its
  -- ability and a permanent whose ability did nothing are two different bugs.
  Spec.it s "CR 718.5 the prototyped permanent keeps vigilance and its activated ability" $ do
    assembler <- S.printingOf s registry "Autonomous Assembler"
    plains <- S.printingOf s registry "Plains"
    let (board, spellId) = S.handOne assembler (S.landsInPlay plains 5)
        cast = S.runPure prototyping board (S.cast S.alice spellId)
        resolved = S.settleSba (S.runPure prototyping cast Stack.resolveTop)
    case assemblerOn assembler resolved of
      Nothing -> Spec.assertFailure s "the prototyped spell did not resolve onto the battlefield"
      Just oid -> do
        Spec.assertBool
          s
          (Map.member Keyword.Vigilance (Projection.keywordsOf oid resolved))
          "CR 718.5: the prototyped permanent still has vigilance"
        -- CR 302.6 is the fixture's, not the rule's: the ability costs {T}, so the
        -- permanent has to have been under its controller since the turn began
        -- before rule 718.5's question can be asked at all.
        let settled = settleUnderAlice oid resolved
        case Projection.abilitiesOf oid settled of
          [] -> Spec.assertFailure s "the prototyped permanent offers no activated ability"
          ability : _ -> do
            Spec.assertBool
              s
              (Activate.activatable S.alice oid ability settled)
              "CR 718.5: its printed activated ability is activatable"
            let activated = S.runPure prototyping settled (Activate.activateAbility S.alice oid ability)
                counted = S.settleSba (S.runPure prototyping activated Stack.resolveTop)
            -- The gameplay reading: 2/2 plus one +1/+1 counter is 3/3, which is
            -- neither the prototyped 2/2 nor the printed 4/5 nor the printed box
            -- plus a counter (5/6).
            Spec.assertEqWith
              s
              "CR 718.5 with CR 122.1a: the resolved ability makes the prototyped 2/2 a 3/3"
              (S.powerToughnessOf oid counted)
              (Just (3, 3))
            Spec.assertEqWith
              s
              "and the counter is on it"
              (fmap (Map.lookup plusOnePlusOne . Object.counters) (Game.lookupObject oid counted))
              (Just (Just 1))
  -- CR 718.3a: "while casting a prototyped spell, use only its alternative power,
  -- toughness, and mana cost when evaluating those characteristics to see if it
  -- can be cast." Read as a PAIR of boards differing in exactly one thing -- how
  -- many Plains alice has -- since a board that could pay neither cost and a board
  -- that could pay both would each pass for the wrong reason.
  Spec.it s "CR 718.3a two Plains pay the inset cost and offer the cast; one Plains pays neither" $ do
    assembler <- S.printingOf s registry "Autonomous Assembler"
    plains <- S.printingOf s registry "Plains"
    let (two, twoId) = S.handOne assembler (S.landsInPlay plains 2)
        (one, oneId) = S.handOne assembler (S.landsInPlay plains 1)
        (five, fiveId) = S.handOne assembler (S.landsInPlay plains 5)
    Spec.assertBool s (S.castable S.alice twoId two) "CR 718.3a: two Plains pay the inset {1}{W}, so the cast is offered"
    Spec.assertBool s (not (S.castable S.alice oneId one)) "one Plains pays neither cost, so it is not"
    Spec.assertBool s (S.castable S.alice fiveId five) "five Plains pay both"
    -- And the cast two Plains offer really is the prototyped one: an engine that
    -- offered the cast off the printed {5} and then could not pay it would pass
    -- the castability assertion above and fail here.
    let cast = S.runPure prototyping two (S.cast S.alice twoId)
    Spec.assertEqWith
      s
      "CR 718.3a: the spell two Plains put on the stack is the white 2/2"
      ( (,)
          <$> fmap (\oid -> Projection.colorsOf oid cast) (topOfStack cast)
          <*> (topOfStack cast >>= \oid -> S.powerToughnessOf oid cast)
      )
      (Just (Set.singleton Color.White, (2, 2)))
  -- CR 718.4: "in every zone except the stack or the battlefield, and while on the
  -- stack or the battlefield when not cast as a prototyped spell, a prototype card
  -- has only its normal characteristics."
  --
  -- The HAND is the zone that separates rule 718.4 from an implementation that
  -- read the inset frame off the card wherever it lay; the GRAVEYARD is the half
  -- that separates it from one that stamped the choice and never cleared it. Both
  -- readings are of the same card in the same game, one having been cast
  -- prototyped and died.
  Spec.it s "CR 718.4 in hand and in a graveyard the Assembler has only its printed characteristics" $ do
    assembler <- S.printingOf s registry "Autonomous Assembler"
    plains <- S.printingOf s registry "Plains"
    let (board, spellId) = S.handOne assembler (S.landsInPlay plains 5)
    Spec.assertEqWith
      s
      "CR 718.4: in hand it is a colourless 4/5"
      ((,) (Projection.colorsOf spellId board) (S.powerToughnessOf spellId board))
      (Set.empty, Just (4, 5))
    let cast = S.runPure prototyping board (S.cast S.alice spellId)
        resolved = S.settleSba (S.runPure prototyping cast Stack.resolveTop)
    case assemblerOn assembler resolved of
      Nothing -> Spec.assertFailure s "the prototyped spell did not resolve onto the battlefield"
      Just oid -> do
        let dead = S.settleSba (S.runPure prototyping resolved (Event.destroy Regenerability.Regenerable [oid]))
            inGraveyard = Game.zoneMembers Zone.Graveyard S.alice dead
        Spec.assertEqWith
          s
          "CR 718.4: the card in its owner's graveyard is a colourless 4/5 again"
          (fmap (\card -> (Projection.colorsOf card dead, S.powerToughnessOf card dead)) inGraveyard)
          [(Set.empty, Just (4, 5))]

  -- CR 718.2a: "the existence and values of these alternative characteristics are
  -- part of the object's copiable values", and CR 718.3c: "if a prototyped spell
  -- is copied, the copy is also a prototyped spell. It has the alternative power,
  -- toughness, and mana cost characteristics of the spell and not the normal
  -- power, toughness, and mana cost characteristics of the card."
  --
  -- Lithoform Engine's "{4}, {T}: Copy target spell you control" is the producer.
  -- The copy is a TOKEN with the spell's characteristics (CR 707.10f, CR 111.13),
  -- so a reading that copied the card rather than the object gives a colourless
  -- 4/5 and this pair of assertions separates the two.
  Spec.it s "CR 718.2a / 718.3c a copy of the prototyped spell is a white 2/2 too" $ do
    assembler <- S.printingOf s registry "Autonomous Assembler"
    plains <- S.printingOf s registry "Plains"
    engine <- S.printingOf s registry "Lithoform Engine"
    let (engineId, withEngine) = S.addPermanent engine S.alice (S.landsInPlay plains 6)
        (board, spellId) = S.handOne assembler withEngine
        cast = S.runPure prototyping board (S.cast S.alice spellId)
        ready = cast {GameState.priority = Just S.alice}
    case (topOfStack cast, List.find ((== Just (ManaCost.MkManaCost [ManaSymbol.Generic 4])) . Cost.Type.mana . ActivatedAbility.cost) (Projection.abilitiesOf engineId ready)) of
      (Just spell, Just copier) -> do
        let activated = S.runPure (pinTarget (Recipient.ToObject spell)) ready (Activate.activateAbility S.alice engineId copier)
            copied = resolveOne S.identityAnswer activated
            afterCopy = resolveOne S.identityAnswer copied
            afterBoth = resolveOne S.identityAnswer afterCopy
        Spec.assertEqWith
          s
          "CR 718.3c: the copy resolves as a WHITE Assembler"
          (fmap (\oid -> Projection.colorsOf oid afterCopy) (assemblersOn assembler afterCopy))
          [Set.singleton Color.White]
        Spec.assertEqWith
          s
          "CR 718.2a: with the inset box, a 2/2, not the printed 4/5"
          (fmap (\oid -> S.powerToughnessOf oid afterCopy) (assemblersOn assembler afterCopy))
          [Just (2, 2)]
        -- CR 608.3a: the spell itself then resolves beside its copy, so the board
        -- holds one token and one card -- which is what makes the readings above
        -- about the COPY rather than about the only Assembler in play.
        Spec.assertEqWith
          s
          "CR 707.10f: one token and one card, both white 2/2s"
          (List.sort (fmap (\oid -> (Game.isToken oid afterBoth, Projection.colorsOf oid afterBoth, S.powerToughnessOf oid afterBoth)) (assemblersOn assembler afterBoth)))
          [(False, Set.singleton Color.White, Just (2, 2)), (True, Set.singleton Color.White, Just (2, 2))]
      _ -> Spec.assertFailure s "the Assembler never reached the stack, or the Engine offered no {4} ability"
