{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Cast and Pawl.Engine.Stack: cast timing, the stack, discard, and
-- summoning sickness.
module Pawl.CastSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.CardSpec as CardSpec
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Concession as Concession
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EntwineDecision as EntwineDecision
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

sicknessOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe Sickness.Sickness
sicknessOf oid gs = fmap Object.sickness (Game.lookupObject oid gs)

sicknessSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
sicknessSpec s registry = Spec.describe s "Sickness" $ do
  Spec.it s "CR 302.6 a permanent entering the battlefield is summoning sick" $ do
    -- changeZone mints a new object, so the id to inspect is the new one.
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, oid) = S.pikerInHand mountain piker 3 Phase.PrecombatMain
        after = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Battlefield)
    case Game.zoneMembers Zone.Battlefield S.alice after of
      [] -> Spec.assertFailure s "expected a permanent"
      ids -> case filter (\o -> sicknessOf o after == Just Sickness.Sick) ids of
        [] -> Spec.assertFailure s "the new permanent should be Sick"
        _ -> pure ()
  Spec.it s "CR 302.6 the untap step settles the active player's permanents" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
        sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}
        after = snd (Engine.runGamePure S.identityAnswer sick (Engine.settleAll S.alice))
    Spec.assertEqWith s "settled" (sicknessOf oid after) (Just (Sickness.Settled S.alice))
  Spec.it s "CR 302.6 settling does not touch the other player's permanents" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}
        after = snd (Engine.runGamePure S.identityAnswer sick (Engine.settleAll S.alice))
    Spec.assertEqWith s "still sick" (sicknessOf oid after) (Just Sickness.Sick)

castGameState :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
castGameState s registry = do
  matchup <- S.redRed (S.printingOf s registry)
  pure (snd (Engine.runMatchPure S.castAnswer matchup))

castEngineSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
castEngineSpec s registry = Spec.describe s "CastEngine" $ do
  Spec.it s "a castable Piker is offered as a legal action" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, oid) = S.pikerInHand mountain piker 2 Phase.PrecombatMain
    Spec.assertBool s (elem (A.Cast oid (S.printingName piker)) (Action.legalActions S.alice gs)) "offered"
  Spec.it s "an unaffordable Piker is not offered" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, oid) = S.pikerInHand mountain piker 1 Phase.PrecombatMain
    Spec.assertBool s (not (any (S.isCastOf oid) (Action.legalActions S.alice gs))) "not offered"
  -- The mechanism, against Pawl.CardSpec's hand-built fixture: two Instant
  -- halves, Wax costing {G} and Wane costing {W}, and nothing else printed on
  -- either. waxWaneSpec below asserts the same rules against the printed Wax //
  -- Wane, which is what proves the card data; this pair says the gate is the
  -- layout's and does not depend on what those halves happen to do.
  Spec.it s "CR 709.3a both halves are offered, each priced from its own half" $ do
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    let waxWane = Printing.MkPrinting CardSpec.splitCard
        namesOffered gs = [n | A.Cast _ n <- Action.legalActions S.alice gs]
        (green, _) = S.handOne waxWane (S.landsInPlay forest 1)
        (both, _) = S.handOne waxWane (snd (S.addCreature plains S.alice (S.landsInPlay forest 1)))
    -- CR 709.3: "A player chooses which half of a split card they are casting
    -- before putting it onto the stack." Two actions, one card: the choice is
    -- offered, never made.
    Spec.assertEqWith
      s
      "a Forest and a Plains: both halves"
      (namesOffered both)
      [CardName.MkCardName (Text.pack "Wax"), CardName.MkCardName (Text.pack "Wane")]
    -- CR 709.3a: "Only the chosen half is evaluated to see if it can be cast."
    -- One Forest pays Wax's {G} and cannot pay Wane's {W}, so the halves are
    -- gated apart. Falsifier: an engine that priced either half from CR 709.4's
    -- combined {G}{W} would offer NEITHER, since one Forest pays neither.
    Spec.assertEqWith s "one Forest: only the affordable half" (namesOffered green) [CardName.MkCardName (Text.pack "Wax")]
  Spec.it s "CR 709.3a only the chosen half is put onto the stack" $ do
    forest <- S.printingOf s registry "Forest"
    let wax = CardName.MkCardName (Text.pack "Wax")
        (gs, oid) = S.handOne (Printing.MkPrinting CardSpec.splitCard) (S.landsInPlay forest 1)
        after = S.runPure S.identityAnswer gs (Cast.castSpell S.alice oid wax)
    case GameState.stack after of
      [] -> Spec.assertFailure s "expected the spell on the stack"
      top : _ -> do
        -- CR 709.3a: "Only that half is considered to be put onto the stack."
        -- Stamped by the cast rather than by hand, which is what makes CR
        -- 601.2b's previously made choice a fact about the stack object.
        Spec.assertEqWith s "the stack object records the half cast" (fmap Object.face (Game.lookupObject top after)) (Just (Just wax))
        -- CR 709.3b: "While on the stack, only the characteristics of the half
        -- being cast exist." Green alone, not the green-and-white CR 709.4
        -- gives the same card in hand.
        Spec.assertEqWith s "and it shows only that half's colour" (Projection.colorsOf top after) (Set.singleton Color.Green)
    -- Priced from Wax alone: the combined {G}{W} could not have been paid here.
    Spec.assertEqWith s "one Forest tapped to pay Wax's {G}" (S.tappedCount S.alice after) 1
  -- M3a: the red deck now carries Lightning Bolt, so castAnswer casts removal
  -- that clears the board -- creaturesInPlay at end is no longer a valid proxy
  -- for "a spell resolved". castAnswer never attacks, so the ONLY source of
  -- life loss is a resolved Bolt: a player below 20 proves an instant was cast
  -- AND resolved (not merely discarded). Deck-robust where creature-presence
  -- was not.
  Spec.it s "casting actually happens in a full game" $ do
    gs <- castGameState s registry
    Spec.assertBool
      s
      (any (\pl -> Player.life pl < Setup.startingLife) (Map.elems (GameState.players gs)))
      "a spell resolved and dealt damage"
  Spec.it s "a casting game still terminates" $ do
    gs <- castGameState s registry
    Spec.assertBool s (Maybe.isJust (GameState.result gs)) "has result"
  Spec.it s "a casting game conserves objects" $ do
    gs <- castGameState s registry
    Spec.assertEqWith s "objects" (Game.objectCount gs) 120
  -- CR 500.5 is no longer unconditional: Upwelling keeps unspent mana across
  -- every step and phase end. It holds here because none of the hand-tuned
  -- decks in Pawl.Cards plays one, so a future deck that does must expect
  -- this assertion to change rather than treat it as a regression.
  Spec.it s "CR 500.5 no mana floats at the end of a game" $ do
    gs <- castGameState s registry
    Spec.assertEqWith s "pools empty" (GameState.manaPool gs) Map.empty

-- A Piker cast and left on the stack, ready to resolve.
pikerOnStack :: Printing.Printing -> Printing.Printing -> GameState.GameState
pikerOnStack mountain piker =
  let (gs, oid) = S.pikerInHand mountain piker 3 Phase.PrecombatMain
   in snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice oid))

stackSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
stackSpec s registry = Spec.describe s "Stack" $ do
  Spec.it s "CR 608.3 a resolving creature spell becomes a permanent" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let after = snd (Engine.runGamePure S.identityAnswer (pikerOnStack mountain piker) Stack.resolveTop)
    Spec.assertEqWith s "stack empty" (length (GameState.stack after)) 0
    -- Four, not one: pikerInHand 3 leaves three Mountains in play.
    Spec.assertEqWith s "four permanents" (length (Game.zoneMembers Zone.Battlefield S.alice after)) 4
    Spec.assertEqWith s "one of them a creature" (S.creaturesInPlay S.alice after) 1
  Spec.it s "CR 400.7 the permanent is a new object" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let after = snd (Engine.runGamePure S.identityAnswer (pikerOnStack mountain piker) Stack.resolveTop)
    case GameState.stack (pikerOnStack mountain piker) of
      [] -> Spec.assertFailure s "fixture should have a spell on the stack"
      top : _ -> Spec.assertEqWith s "old id gone" (Game.lookupObject top after) Nothing
  Spec.it s "the permanent is a Piker on the battlefield" $ do
    -- The object the spell resolved INTO, not just any permanent: the
    -- fixture already has three Mountains in play, and zoneMembers is
    -- ordered by id, so the front of that list is Mountain id 0.
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let before = Game.zoneMembers Zone.Battlefield S.alice (pikerOnStack mountain piker)
        after = snd (Engine.runGamePure S.identityAnswer (pikerOnStack mountain piker) Stack.resolveTop)
        isNew o = notElem o before
        fresh = filter isNew (Game.zoneMembers Zone.Battlefield S.alice after)
    case fresh of
      [] -> Spec.assertFailure s "expected a new permanent"
      oid : _ -> case Game.lookupObject oid after of
        Nothing -> Spec.assertFailure s "battlefield id should resolve"
        Just obj -> do
          Spec.assertEqWith s "zone" (Object.zone obj) Zone.Battlefield
          case Object.source obj of
            Source.OfCard printing ->
              Spec.assertBool s (Card.isCreature (S.faceOf printing)) "creature"
            Source.OfToken _ -> Spec.assertFailure s "expected a card source"
            Source.OfAbility _ _ -> Spec.assertFailure s "expected a card source"
            Source.OfTrigger _ _ -> Spec.assertFailure s "expected a card source"
            Source.OfEmblem _ -> Spec.assertFailure s "expected a card source"
            Source.OfInherentTrigger _ _ -> Spec.assertFailure s "expected a card source"
  Spec.it s "resolving conserves objects" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    Spec.assertEqWith
      s
      "conserved"
      (Game.objectCount (snd (Engine.runGamePure S.identityAnswer (pikerOnStack mountain piker) Stack.resolveTop)))
      (Game.objectCount (pikerOnStack mountain piker))
  Spec.it s "resolving an empty stack is a no-op" $
    let gs = Setup.emptyGame S.bothPlayers
     in Spec.assertEqWith s "unchanged" (snd (Engine.runGamePure S.identityAnswer gs Stack.resolveTop)) gs
  Spec.it s "CR 601.3: cast Panglacial during Evolving Wilds' search, then it resolves 9/5" $ do
    evolvingWilds <- S.printingOf s registry "Evolving Wilds"
    forest <- S.printingOf s registry "Forest"
    panglacialWurm <- S.printingOf s registry "Panglacial Wurm"
    let g0 = Setup.emptyGame S.bothPlayers
        (ewId, g1) = S.addCreature evolvingWilds S.alice g0
        g2 = List.foldl' (\g _ -> snd (S.addCreature forest S.alice g)) g1 [1 .. (7 :: Int)]
        (_, g3) = S.addLibraryCard panglacialWurm S.alice g2
        g4 = g3 {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
    case Projection.abilitiesOf ewId g4 of
      ewAbility : _ ->
        let action = do
              Activate.activateAbility S.alice ewId ewAbility
              Stack.resolveTop -- Evolving Wilds' ability: cast Panglacial, then search + shuffle + cease
              Stack.resolveTop -- Panglacial resolves onto the battlefield
            after = snd (Engine.runGamePure castPanglacial g4 action)
         in do
              Spec.assertEqWith s "Panglacial is a 9/5 on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Panglacial Wurm") S.alice after) 1
              Spec.assertEqWith s "Panglacial left the library" (S.countByName (CardName.MkCardName $ Text.pack "Panglacial Wurm") S.alice after) 0
              Spec.assertEqWith s "seven Forests tapped for {5}{G}{G}" (S.tappedCount S.alice after) 7
      [] -> Spec.assertFailure s "Evolving Wilds should have an activated ability"
  Spec.it s "declining the cast resolves the search normally, Panglacial stays" $ do
    evolvingWilds <- S.printingOf s registry "Evolving Wilds"
    forest <- S.printingOf s registry "Forest"
    panglacialWurm <- S.printingOf s registry "Panglacial Wurm"
    let g0 = Setup.emptyGame S.bothPlayers
        (ewId, g1) = S.addCreature evolvingWilds S.alice g0
        g2 = List.foldl' (\g _ -> snd (S.addCreature forest S.alice g)) g1 [1 .. (7 :: Int)]
        (_, g3) = S.addLibraryCard panglacialWurm S.alice g2
        g4 = g3 {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
    case Projection.abilitiesOf ewId g4 of
      ewAbility : _ ->
        let after = snd (Engine.runGamePure S.identityAnswer g4 (do Activate.activateAbility S.alice ewId ewAbility; Stack.resolveTop))
         in Spec.assertEqWith s "Panglacial still in the library" (S.countByName (CardName.MkCardName $ Text.pack "Panglacial Wurm") S.alice after) 1
      [] -> Spec.assertFailure s "Evolving Wilds should have an activated ability"

castSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
castSpec s registry = Spec.describe s "Cast" $ do
  Spec.it s "a Piker is castable with two Mountains in a main phase" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, oid) = S.pikerInHand mountain piker 2 Phase.PrecombatMain
    Spec.assertBool s (S.castable S.alice oid gs) "castable"
  Spec.it s "a Piker is not castable with one Mountain" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, oid) = S.pikerInHand mountain piker 1 Phase.PrecombatMain
    Spec.assertBool s (not (S.castable S.alice oid gs)) "unaffordable"
  Spec.it s "CR 302.1 no creature spell in the upkeep" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, oid) = S.pikerInHand mountain piker 2 (Phase.Beginning BeginningStep.Upkeep)
    Spec.assertBool s (not (S.castable S.alice oid gs)) "wrong timing"
  Spec.it s "CR 302.1 no creature spell with a non-empty stack" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, oid) = S.pikerInHand mountain piker 2 Phase.PrecombatMain
        busy = gs {GameState.stack = [ObjectId.MkObjectId 999]}
    Spec.assertBool s (not (S.castable S.alice oid busy)) "stack not empty"
  Spec.it s "CR 302.1 a non-active player cannot cast at sorcery speed" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, oid) = S.pikerInHand mountain piker 2 Phase.PrecombatMain
        bobsTurn = gs {GameState.activePlayer = S.bob}
    Spec.assertBool s (not (S.castable S.alice oid bobsTurn)) "not active"
  Spec.it s "a Mountain in hand is not castable: lands have no mana cost" $ do
    mountain <- S.printingOf s registry "Mountain"
    Spec.assertBool s (not (S.castable S.alice (ObjectId.MkObjectId 0) (S.oneMountainState mountain Phase.PrecombatMain))) "no cost"
  Spec.it s "CR 601 casting puts a NEW object on the stack and taps two lands" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, oid) = S.pikerInHand mountain piker 3 Phase.PrecombatMain
        after = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice oid))
    Spec.assertEqWith s "stack depth" (length (GameState.stack after)) 1
    Spec.assertEqWith s "hand empty" (S.handSize S.alice after) 0
    Spec.assertEqWith s "lands tapped" (S.tappedCount S.alice after) 2
    Spec.assertEqWith s "conserved" (Game.objectCount after) (Game.objectCount gs)
    -- CR 400.7: the card on the stack is a new object, not the old id.
    Spec.assertEqWith s "old id gone" (Game.lookupObject oid after) Nothing
  Spec.it s "the stack object is still a Piker on the stack" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, oid) = S.pikerInHand mountain piker 3 Phase.PrecombatMain
        after = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice oid))
    case GameState.stack after of
      [] -> Spec.assertFailure s "expected one object on the stack"
      top : _ -> case Game.lookupObject top after of
        Nothing -> Spec.assertFailure s "stack id should resolve"
        Just obj -> do
          Spec.assertEqWith s "zone" (Object.zone obj) Zone.Stack
          case Object.source obj of
            Source.OfCard printing ->
              Spec.assertEqWith s "name" (Face.name (S.faceOf printing)) (CardName.MkCardName $ Text.pack "Goblin Piker")
            Source.OfToken _ -> Spec.assertFailure s "expected a card source"
            Source.OfAbility _ _ -> Spec.assertFailure s "expected a card source"
            Source.OfTrigger _ _ -> Spec.assertFailure s "expected a card source"
            Source.OfEmblem _ -> Spec.assertFailure s "expected a card source"
            Source.OfInherentTrigger _ _ -> Spec.assertFailure s "expected a card source"
  Spec.it s "CR 117.1a a Bolt is castable outside a main phase" $ do
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs, oid) = S.boltInHand mountain lightningBolt 1 (Phase.Beginning BeginningStep.Upkeep)
    Spec.assertBool s (S.castable S.alice oid gs) "instant speed"
  Spec.it s "CR 117.1a a Bolt is castable on the opponent's turn" $ do
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs, oid) = S.boltInHand mountain lightningBolt 1 Phase.PrecombatMain
        bobsTurn = gs {GameState.activePlayer = S.bob}
    Spec.assertBool s (S.castable S.alice oid bobsTurn) "not my turn, still castable"
  Spec.it s "CR 117.1a a Bolt is castable with a non-empty stack" $ do
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs, oid) = S.boltInHand mountain lightningBolt 1 Phase.PrecombatMain
        busy = gs {GameState.stack = [ObjectId.MkObjectId 999]}
    Spec.assertBool s (S.castable S.alice oid busy) "in response"
  Spec.it s "a Bolt in the graveyard is not castable" $ do
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs, oid) = S.boltInHand mountain lightningBolt 1 Phase.PrecombatMain
        buried = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Graveyard)
    Spec.assertEqWith s "nothing castable" (Cast.castableSpells S.alice buried) []
  Spec.it s "CR 601.2c casting a Bolt stamps the chosen target on the stack object" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (base, gs, _) = S.boltAtBobsPiker piker mountain lightningBolt
    case GameState.stack gs of
      [] -> Spec.assertFailure s "expected the Bolt on the stack"
      top : _ -> case Game.lookupObject top gs of
        Nothing -> Spec.assertFailure s "stack id should resolve"
        Just obj -> do
          Spec.assertEqWith s "one Mountain tapped" (S.tappedCount S.alice gs) 1
          Spec.assertEqWith
            s
            "the Piker is the target"
            (Binding.targetsOf (Object.bindings obj))
            (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Recipient.ToCreature (S.pikerOf base)))
  Spec.it s "casting a {X}{R} spell at X=3 stamps amount 3 and pays {3}{R}" $ do
    blaze <- S.printingOf s registry "Blaze"
    mountain <- S.printingOf s registry "Mountain"
    let (gs0, oid) = S.handOne blaze (S.landsInPlay mountain 4)
        after = snd (Engine.runGamePure answerX3 gs0 (S.cast S.alice oid))
    case GameState.stack after of
      [] -> Spec.assertFailure s "expected the spell on the stack"
      top : _ -> case Game.lookupObject top after of
        Nothing -> Spec.assertFailure s "stack id should resolve"
        Just obj -> do
          Spec.assertEqWith s "amount bound" (Binding.amountOf Binding.variableX (Object.bindings obj)) (Just 3)
          Spec.assertEqWith s "four mana spent (paid {3}{R})" (S.tappedCount S.alice after) 4
  Spec.it s "an illegal target answer makes the cast a no-op" $ do
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs, oid) = S.boltInHand mountain lightningBolt 1 Phase.PrecombatMain
        liar :: Prompt.Prompt r -> r
        liar p = case p of
          Prompt.ChooseTargets _ _ _ sets ->
            fmap (const (Recipient.ToCreature (ObjectId.MkObjectId 999))) sets
          _ -> S.identityAnswer p
        after = snd (Engine.runGamePure liar gs (S.cast S.alice oid))
    Spec.assertEqWith s "nothing on the stack" (length (GameState.stack after)) 0
    Spec.assertEqWith s "nothing paid" (S.tappedCount S.alice after) 0
  Spec.it s "Panglacial Wurm in the library is castable-while-searching with mana" $ do
    forest <- S.printingOf s registry "Forest"
    panglacialWurm <- S.printingOf s registry "Panglacial Wurm"
    let base = S.landsInPlay forest 7
        (_, gs) = S.addLibraryCard panglacialWurm S.alice base
    Spec.assertEqWith s "one castable-while-searching option" (length (Cast.castableWhileSearching S.alice gs)) 1
  Spec.it s "with too little mana, Panglacial is not castable-while-searching" $ do
    forest <- S.printingOf s registry "Forest"
    panglacialWurm <- S.printingOf s registry "Panglacial Wurm"
    let base = S.landsInPlay forest 3
        (_, gs) = S.addLibraryCard panglacialWurm S.alice base
    Spec.assertEqWith s "unaffordable, so no options" (length (Cast.castableWhileSearching S.alice gs)) 0
  Spec.it s "castWhileSearching casts Panglacial from the library onto the stack" $ do
    forest <- S.printingOf s registry "Forest"
    panglacialWurm <- S.printingOf s registry "Panglacial Wurm"
    let base = S.landsInPlay forest 7
        (_, gs) = S.addLibraryCard panglacialWurm S.alice base
        after = snd (Engine.runGamePure castFirstOption gs (Cast.castWhileSearching S.alice))
        onStack = length (filter (nameOnStack (CardName.MkCardName $ Text.pack "Panglacial Wurm") after) (GameState.stack after))
    Spec.assertEqWith s "Panglacial is on the stack" onStack 1
    Spec.assertEqWith s "Panglacial left the library" (S.countByName (CardName.MkCardName $ Text.pack "Panglacial Wurm") S.alice after) 0
    Spec.assertEqWith s "seven Forests tapped to pay {5}{G}{G}" (S.tappedCount S.alice after) 7
  Spec.it s "CR 601.2i casting a spell records a SpellCast event for the caster" $ do
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs, oid) = S.boltInHand mountain lightningBolt 1 Phase.PrecombatMain
        after = S.runPure S.identityAnswer gs (S.cast S.alice oid)
        casts = Maybe.mapMaybe Event.castOf (Foldable.toList (GameState.events after))
    Spec.assertEqWith s "no cast before" (Maybe.mapMaybe Event.castOf (Foldable.toList (GameState.events gs))) []
    Spec.assertEqWith s "exactly one cast, by alice" casts [S.alice]
  Spec.it s "CR 601.2i a cast that is rejected records nothing" $ do
    -- A Bolt with no mana available: legalActions would never offer it, and
    -- castSpell's payment fails, so no event is recorded.
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs, oid) = S.boltInHand mountain lightningBolt 0 Phase.PrecombatMain
        after = S.runPure S.identityAnswer gs (S.cast S.alice oid)
    Spec.assertEqWith s "no cast recorded" (Maybe.mapMaybe Event.castOf (Foldable.toList (GameState.events after))) []

-- Chooses X=3 and aims every target slot at bob; other prompts take the identity
-- fallback. Casing on a GADT prompt with an identityAnswer default is the liar
-- pattern from the illegal-target test.
answerX3 :: Prompt.Prompt r -> r
answerX3 p = case p of
  Prompt.ChooseX {} -> 3
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer S.bob)) sets
  _ -> S.identityAnswer p

-- Discards from the BACK of hand. Deliberately unlike every fallback, so the
-- CR 514.2 test proves the prompted choice is actually honored.
discardLastAnswer :: Prompt.Prompt r -> r
discardLastAnswer p = case p of
  Prompt.ChooseDefender _ _ candidates -> NonEmpty.head candidates
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
  Prompt.Shuffle ids -> ids
  Prompt.RandomFirstPlayer order -> NonEmpty.head order
  Prompt.ChooseAction {} -> A.Pass
  Prompt.Concede _ -> Concession.Continues
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  Prompt.ChooseDiscard _ _ ids n -> lastN n ids
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

lastN :: Natural -> [a] -> [a]
lastN n xs = reverse (List.genericTake n (reverse xs))

-- Bob draws to eight, then discards at cleanup under discardLastAnswer.
bobDiscardChoice :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, [ObjectId.ObjectId])
bobDiscardChoice s registry = do
  matchup <- S.redRed (S.printingOf s registry)
  let start = Setup.emptyGame S.bothPlayers
      steps = do
        Setup.newGame S.performer matchup
        State.modify' $ \gs -> gs {GameState.activePlayer = S.bob, GameState.turnNumber = 2}
        S.drawStep
        beforeCleanup <- State.gets (Game.zoneMembers Zone.Hand S.bob)
        Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)
        pure beforeCleanup
      (held, final) = Engine.runGamePure discardLastAnswer start steps
  pure (final, held)

discardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
discardSpec s registry = Spec.describe s "Discard" $ do
  Spec.it s "CR 514.2 discard trims to hand size" $ do
    (final, _held) <- bobDiscardChoice s registry
    Spec.assertEqWith s "hand" (S.handSize S.bob final) 7
  Spec.it s "CR 514.2 the prompted choice is honored" $ do
    (final, held) <- bobDiscardChoice s registry
    let kept = Game.zoneMembers Zone.Hand S.bob final
        -- discardLastAnswer pitched the last card, so the first seven of the
        -- pre-cleanup hand are exactly what survives. Ids are stable here:
        -- the kept cards never changed zones.
        expected = take 7 held
    Spec.assertEqWith s "kept the front seven" kept expected

-- Put one card of a printing into alice's hand over an existing board, in a main
-- phase with priority.
handInPlay :: Printing.Printing -> GameState.GameState -> (GameState.GameState, ObjectId.ObjectId)
handInPlay printing board =
  let (oid, g1) = Game.freshObjectId board
      (ts, g2) = Game.freshTimestamp g1
      obj =
        Object.MkObject
          { Object.owner = S.alice,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled S.alice,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing
          }
   in ( g2
          { GameState.objects = Map.insert oid obj (GameState.objects g2),
            GameState.hand = Map.insertWith (Seq.><) S.alice (Seq.singleton oid) (GameState.hand g2),
            GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        oid
      )

-- Targets a permanent (lookupMin picks the lowest ToObject id) and hacks
-- Mountain -> Island. The word swap is announced at RESOLUTION (CR 608.2d), so
-- this has to drive the resolution as well as the cast.
hackAnswer :: Prompt.Prompt r -> r
hackAnswer p = case p of
  Prompt.ChooseLandTypeSwap {} -> (Subtype.Mountain, Subtype.Island)
  _ -> S.identityAnswer p

magicalHackSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
magicalHackSpec s registry = Spec.describe s "MagicalHack" $ do
  Spec.it s "CR 612/305.6 a hacked basic Mountain taps for its new color" $ do
    -- alice: one Mountain to hack and one Island (blue for the {U}), plus a
    -- Magical Hack in hand. The Mountain is added FIRST so it has the lowest
    -- object id and identityAnswer's ChooseTargets (Set.lookupMin over the
    -- ToObject recipients) picks it, not the Island. Hack Mountain -> Island.
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    magicalHack <- S.printingOf s registry "Magical Hack"
    let (mountainId, g0) = S.addCreature mountain S.alice (Setup.emptyGame S.bothPlayers)
        (islandId, g1) = S.addCreature island S.alice g0
        (gs, hackId) = handInPlay magicalHack g1
        cast = snd (Engine.runGamePure hackAnswer gs (S.cast S.alice hackId))
        resolved = snd (Engine.runGamePure hackAnswer cast Stack.resolveTop)
    Spec.assertBool s (elem (ManaType.Colored Color.Blue) (Mana.manaTypesOf islandId resolved)) "island untouched, still blue"
    Spec.assertEqWith s "hacked Mountain projects Island" (Projection.subtypesOf mountainId resolved) (Set.singleton Subtype.Island)
    Spec.assertBool s (elem (ManaType.Colored Color.Blue) (Mana.manaTypesOf mountainId resolved)) "hacked Mountain taps blue"
    Spec.assertBool s (notElem (ManaType.Colored Color.Red) (Mana.manaTypesOf mountainId resolved)) "hacked Mountain no longer taps red"
  Spec.it s "CR 601.2c Magical Hack with no legal target is uncastable" $ do
    magicalHack <- S.printingOf s registry "Magical Hack"
    let (gs, hackId) = handInPlay magicalHack (Setup.emptyGame S.bothPlayers)
    -- Empty battlefield and stack: SpellOrPermanentTarget has no legal
    -- recipient (and there is no mana either), so it is uncastable.
    Spec.assertBool s (not (S.castable S.alice hackId gs)) "no target -> uncastable"

-- Aims every target slot at bob and chooses X=0; the X=0 castability floor.
answerX0 :: Prompt.Prompt r -> r
answerX0 p = case p of
  Prompt.ChooseX {} -> 0
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer S.bob)) sets
  _ -> S.identityAnswer p

-- Answers Prompt.ChooseX with the affordability bound the prompt carries, and
-- records that bound in the State. The log is how a test sees a payload nothing
-- on the board records; answering WITH it is what proves the bound is payable
-- rather than merely reported. Aims every target slot at bob, and takes the
-- identity fallback elsewhere (the liar pattern answerX3 uses).
answerAtBound :: Prompt.Prompt r -> State.State [Natural] r
answerAtBound p = case p of
  Prompt.ChooseX _ _ _ bound -> do
    State.modify' (\seen -> seen <> [bound])
    pure bound
  Prompt.ChooseTargets _ _ _ sets -> pure (fmap (const (Recipient.ToPlayer S.bob)) sets)
  _ -> pure (S.identityAnswer p)

-- Announces ONE MORE than the bound -- legal under CR 601.2b and unaffordable by
-- construction, whatever the board is.
answerAboveBound :: Prompt.Prompt r -> r
answerAboveBound p = case p of
  Prompt.ChooseX _ _ _ bound -> bound + 1
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer S.bob)) sets
  _ -> S.identityAnswer p

-- answerAtBound and answerAboveBound in one, COUNTING the CR 601.2c target
-- questions the cast asks: `offset` 0 announces the bound and 1 announces one
-- past it. The count is the only way a test can see a step the cast did NOT
-- take, since a cast reversed at CR 601.2h leaves a board identical to one
-- reversed earlier.
answerAtBoundOffsetCounting :: Natural -> Prompt.Prompt r -> State.State Int r
answerAtBoundOffsetCounting offset p = case p of
  Prompt.ChooseX _ _ _ bound -> pure (bound + offset)
  Prompt.ChooseTargets _ _ _ sets -> do
    State.modify' (+ 1)
    pure (fmap (const (Recipient.ToPlayer S.bob)) sets)
  _ -> pure (S.identityAnswer p)

-- Records the object each CR 601.2c target question is asked ABOUT, and aims
-- every slot at bob. The recorded id is what proves CR 601.2a ran first: it is
-- the spell's stack incarnation (CR 400.7), never the card that was in hand.
answerRecordingTargetObject :: Prompt.Prompt r -> State.State [ObjectId.ObjectId] r
answerRecordingTargetObject p = case p of
  Prompt.ChooseTargets _ _ oid sets -> do
    State.modify' (\seen -> seen <> [oid])
    pure (fmap (const (Recipient.ToPlayer S.bob)) sets)
  _ -> pure (S.identityAnswer p)

-- How many cards of this name sit in alice's hand (the reject-not-repair no-op
-- check: a cast that reverses leaves the card exactly where it was).
inHandNamed :: String -> GameState.GameState -> Int
inHandNamed name gs = length (filter (nameOnStack (CardName.MkCardName $ Text.pack name) gs) (Game.zoneMembers Zone.Hand S.alice gs))

blazeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
blazeSpec s registry = Spec.describe s "Blaze" $ do
  Spec.it s "Blaze at X=3 deals 3 to the opponent (CR 601.2b/f/h, 608.2)" $ do
    -- Falsifier: an engine that ignored the chosen value (treated X as 0, or
    -- as the {X} mana value) would leave bob at 20.
    blaze <- S.printingOf s registry "Blaze"
    mountain <- S.printingOf s registry "Mountain"
    let (gs0, oid) = S.handOne blaze (S.landsInPlay mountain 4)
        after = snd (Engine.runGamePure answerX3 gs0 (do S.cast S.alice oid; Stack.resolveTop))
    Spec.assertEqWith s "Bob at 17" (S.lifeOf S.bob after) (Just 17)
    Spec.assertEqWith s "four Mountains paid {3}{R}" (S.tappedCount S.alice after) 4
  Spec.it s "Blaze at X=0 is castable and deals nothing (the X=0 floor)" $ do
    -- Falsifier: a floor that required {X} > 0 would make Blaze uncastable off
    -- one Mountain, leaving it in hand.
    blaze <- S.printingOf s registry "Blaze"
    mountain <- S.printingOf s registry "Mountain"
    let (gs0, oid) = S.handOne blaze (S.landsInPlay mountain 1)
        after = snd (Engine.runGamePure answerX0 gs0 (do S.cast S.alice oid; Stack.resolveTop))
    Spec.assertEqWith s "Bob unharmed" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "one Mountain paid {R}" (S.tappedCount S.alice after) 1
    Spec.assertEqWith s "Blaze resolved out of hand" (inHandNamed "Blaze" after) 0
  Spec.it s "Blaze at an unaffordable X is a no-op (reject-not-repair)" $ do
    blaze <- S.printingOf s registry "Blaze"
    mountain <- S.printingOf s registry "Mountain"
    let (gs0, oid) = S.handOne blaze (S.landsInPlay mountain 1)
        after = snd (Engine.runGamePure answerX3 gs0 (S.cast S.alice oid))
    Spec.assertEqWith s "still in hand" (inHandNamed "Blaze" after) 1
    Spec.assertEqWith s "no mana spent" (S.tappedCount S.alice after) 0
    Spec.assertEqWith s "Bob unharmed" (S.lifeOf S.bob after) (Just 20)
  -- The bound is what the BOARD can pay, so it moves with the board: {X}{R}
  -- off four Mountains admits X=3, off six admits X=5, and off the one
  -- Mountain that only just makes Blaze castable admits nothing but CR
  -- 601.2b's floor. No constant, and nothing read off the printed cost,
  -- satisfies all three (#417).
  Spec.it s "CR 601.2b the ChooseX bound is the greatest X the board can pay" $ do
    blaze <- S.printingOf s registry "Blaze"
    mountain <- S.printingOf s registry "Mountain"
    let boundsOff n =
          let (gs0, oid) = S.handOne blaze (S.landsInPlay mountain n)
           in State.execState (Engine.runGame answerAtBound gs0 (S.cast S.alice oid)) []
    Spec.assertEqWith s "four Mountains bound X at 3" (boundsOff 4) [3]
    Spec.assertEqWith s "six Mountains bound X at 5" (boundsOff 6) [5]
    Spec.assertEqWith s "one Mountain bounds X at 0" (boundsOff 1) [0]
  -- The bound is PAYABLE and not merely reported: announcing exactly it casts
  -- the spell, pays every Mountain, and resolves. An off-by-one bound would
  -- reverse the cast here instead (CR 601.2) and leave bob at 20.
  Spec.it s "CR 601.2b announcing X at the bound casts Blaze and resolves it" $ do
    blaze <- S.printingOf s registry "Blaze"
    mountain <- S.printingOf s registry "Mountain"
    let (gs0, oid) = S.handOne blaze (S.landsInPlay mountain 4)
        after = snd (State.evalState (Engine.runGame answerAtBound gs0 (do S.cast S.alice oid; Stack.resolveTop)) [])
    Spec.assertEqWith s "Bob at 17, so the bound of 3 was announced and paid" (S.lifeOf S.bob after) (Just 17)
    Spec.assertEqWith s "all four Mountains paid {3}{R}" (S.tappedCount S.alice after) 4
    Spec.assertEqWith s "Blaze resolved out of hand" (inHandNamed "Blaze" after) 0
  -- The assertion that keeps the bound honest. It is ADVISORY: CR 601.2b lets
  -- the player announce the value of the variable freely, so one more than the
  -- bound is announced, is unaffordable, and reverses the whole casting (CR
  -- 601.2) exactly as any other unaffordable value does. A bound quietly
  -- turned into a clamp would deal 3 damage and tap four Mountains here.
  Spec.it s "CR 601.2b the bound does not clamp: X one above it is a no-op" $ do
    blaze <- S.printingOf s registry "Blaze"
    mountain <- S.printingOf s registry "Mountain"
    let (gs0, oid) = S.handOne blaze (S.landsInPlay mountain 4)
        after = snd (Engine.runGamePure answerAboveBound gs0 (S.cast S.alice oid))
    Spec.assertEqWith s "still in hand" (inHandNamed "Blaze" after) 1
    Spec.assertEqWith s "no mana spent" (S.tappedCount S.alice after) 0
    Spec.assertEqWith s "Bob unharmed" (S.lifeOf S.bob after) (Just 20)
  -- WHERE the reversal happens, which the no-op above cannot see. CR 601.2 puts
  -- the reversal at the step the player "is unable to comply with", and the
  -- announced value of X is the first step that can be one -- every candidate
  -- cost was measured at CR 601.2b's X=0 floor, since that is the only value
  -- castability can know before the announcement exists. So the cast ends there,
  -- and CR 601.2c's target question is never put to a player whose spell is
  -- already lost.
  --
  -- That is the posture castability itself takes -- pawl refuses to propose a
  -- cast it cannot pay rather than proposing and reversing at CR 601.2h -- and
  -- carrying it through to the announced X is what leaves Mana.announcePhyrexian
  -- with no cost it has no offer for (#417).
  Spec.it s "CR 601.2 an unaffordable X ends the cast before CR 601.2c's targets are asked for" $ do
    blaze <- S.printingOf s registry "Blaze"
    mountain <- S.printingOf s registry "Mountain"
    let (gs0, oid) = S.handOne blaze (S.landsInPlay mountain 4)
        asked offset = State.execState (Engine.runGame (answerAtBoundOffsetCounting offset) gs0 (S.cast S.alice oid)) 0
    Spec.assertEqWith s "at the bound the cast goes on and asks for its target" (asked 0) 1
    Spec.assertEqWith s "one above it, there is nothing left to target for" (asked 1) 0
  -- CR 601.2a: "To propose the casting of a spell, a player first moves that
  -- card (or that copy of a card) from where it is to the stack. It becomes the
  -- topmost object on the stack." FIRST -- before CR 601.2b's modes and cost and
  -- before CR 601.2c's targets -- so the object a player announces targets for
  -- is the spell on the stack, and not the card that was in their hand.
  --
  -- The two ids are distinguishable because CR 400.7 says they are: "each time an
  -- object moves ... it becomes a new object with no memory of ... its previous
  -- existence", which Event.changeZone implements by minting a fresh id. So the
  -- id the CR 601.2c prompt names being the id ON THE STACK is exactly the claim
  -- that rule 601.2a ran before rule 601.2c, and nothing weaker would be.
  Spec.it s "CR 601.2a the spell is already on the stack when CR 601.2c announces its targets" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    mountain <- S.printingOf s registry "Mountain"
    let (gs0, oid) = S.handOne bolt (S.landsInPlay mountain 1)
        ((_, after), asked) = State.runState (Engine.runGame answerRecordingTargetObject gs0 (S.cast S.alice oid)) []
    Spec.assertEqWith s "the Bolt is the one thing on the stack" (length (GameState.stack after)) 1
    Spec.assertEqWith s "and it is the object CR 601.2c was asked about" asked (GameState.stack after)
    Spec.assertBool s (notElem oid asked) "which is the CR 400.7 incarnation, not the card in hand"
  -- The bound is measured at CR 601.2f's TOTAL, not on the printed cost:
  -- Thalia's "noncreature spells cost {1} more" (EachPlayer-scoped, so her own
  -- controller pays it too) eats one of the four Mountains, and the board that
  -- admitted X=3 above admits only X=2.
  Spec.it s "CR 601.2f a cost increase lowers the ChooseX bound" $ do
    blaze <- S.printingOf s registry "Blaze"
    mountain <- S.printingOf s registry "Mountain"
    thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
    let (gs0, oid) = S.handOne blaze (snd (S.addCreature thalia S.alice (S.landsInPlay mountain 4)))
        cast = do S.cast S.alice oid; Stack.resolveTop
        bounds = State.execState (Engine.runGame answerAtBound gs0 cast) []
        after = snd (State.evalState (Engine.runGame answerAtBound gs0 cast) [])
    Spec.assertEqWith s "the taxed bound is 2" bounds [2]
    Spec.assertEqWith s "Bob at 18" (S.lifeOf S.bob after) (Just 18)
    Spec.assertEqWith s "four Mountains paid {2}{R} plus Thalia's {1}" (S.tappedCount S.alice after) 4

-- Corrosive Gale ({X}{G/P} Sorcery, "Corrosive Gale deals X damage to each
-- creature with flying"), the only card in the pool -- and one of only two ever
-- printed, the other being Postmortem Lunge -- carrying CR 107.3's {X} beside
-- CR 107.4f's Phyrexian symbol. That pairing is what makes CR 601.2b's two
-- announcements measure the same cost or visibly disagree, and #417 was the
-- disagreement: castability was gated at X=0 while the Phyrexian announcement
-- ran on the value the player named.
--
-- THE ARITHMETIC, stated once because every board below is Forests and nothing
-- else. At the announced X the total is {X}{G/P}, and CR 601.2b's two nonhybrid
-- equivalents of that are X+1 mana (paying the {G}) or X mana plus 2 life. Off n
-- untapped Forests at 20 life the mana route therefore admits X <= n-1 and the
-- life route X <= n, so the greatest payable X is n. A bound that counted only
-- mana -- the obvious wrong implementation -- would answer n-1 on every one of
-- them.
corrosiveGaleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
corrosiveGaleSpec s registry = Spec.describe s "CorrosiveGale" $ do
  Spec.it s "CR 601.2b the ChooseX bound counts CR 107.4f's 2-life route" $ do
    gale <- S.printingOf s registry "Corrosive Gale"
    forest <- S.printingOf s registry "Forest"
    let boundsOff n =
          let (gs0, oid) = S.handOne gale (S.landsInPlay forest n)
           in State.execState (Engine.runGame answerAtBound gs0 (S.cast S.alice oid)) []
    Spec.assertEqWith s "three Forests bound X at 3, not the 2 the mana alone buys" (boundsOff 3) [3]
    Spec.assertEqWith s "two Forests bound X at 2, not 1" (boundsOff 2) [2]
  -- The bound is PAYABLE and not merely reported, and at the bound the life route
  -- is the ONLY one left -- CR 601.2b's announcement has exactly one offer, so
  -- there is no prompt and no choice to make. Alice paying 2 life is what proves
  -- the bound was not an off-by-one dressed up as a life route.
  Spec.it s "CR 107.4f announcing X at the bound pays 2 life, the only route left" $ do
    gale <- S.printingOf s registry "Corrosive Gale"
    forest <- S.printingOf s registry "Forest"
    let (gs0, oid) = S.handOne gale (S.landsInPlay forest 3)
        after = snd (State.evalState (Engine.runGame answerAtBound gs0 (do S.cast S.alice oid; Stack.resolveTop)) [])
    Spec.assertEqWith s "all three Forests paid the {3}" (S.tappedCount S.alice after) 3
    Spec.assertEqWith s "and CR 119.4 took the 2 life for the {G/P}" (S.lifeOf S.alice after) (Just 18)
    Spec.assertEqWith s "Corrosive Gale resolved out of hand" (inHandNamed "Corrosive Gale" after) 0
  -- The CR 118.13a half of #417. One above the bound leaves NEITHER of CR
  -- 601.2b's two resolutions payable, so there is nothing to announce and the
  -- whole casting reverses (CR 601.2). Announcing it is legal all the same --
  -- CR 601.2b lets the player announce the value of the variable, and the bound
  -- is information rather than a clamp.
  --
  -- The life assertion is the one that carries the CR 118.13a claim: an engine
  -- that answered the unpayable announcement for the player would have committed
  -- one of the two routes, and the reversal has to give it back.
  Spec.it s "CR 118.13a X one above the bound announces nothing and reverses the cast" $ do
    gale <- S.printingOf s registry "Corrosive Gale"
    forest <- S.printingOf s registry "Forest"
    let (gs0, oid) = S.handOne gale (S.landsInPlay forest 3)
        after = snd (Engine.runGamePure answerAboveBound gs0 (S.cast S.alice oid))
    Spec.assertEqWith s "still in hand" (inHandNamed "Corrosive Gale" after) 1
    Spec.assertEqWith s "no Forest tapped" (S.tappedCount S.alice after) 0
    Spec.assertEqWith s "and no life paid for the {G/P}" (S.lifeOf S.alice after) (Just 20)

-- CR 700.2a: an illegal mode can't be chosen, so a modal spell is castable when
-- at least `count` of its modes are fillable -- not when every mode's slots are.
-- Chaos Charm has three modes (destroy target Wall / damage target creature /
-- give target creature haste); the falsifier is castability via the damage or
-- haste mode with no Wall on the board at all.
modalCastSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
modalCastSpec s registry = Spec.describe s "ModalCast" $ do
  Spec.it s "CR 700.2a Chaos Charm is castable off its non-Wall modes with no Wall in play" $ do
    chaosCharm <- S.printingOf s registry "Chaos Charm"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs0, oid) = S.handOne chaosCharm (S.landsInPlay mountain 1)
        (_, gs1) = S.addCreature piker S.alice gs0
    Spec.assertBool s (S.castable S.alice oid gs1) "castable via the damage/haste mode"
  Spec.it s "CR 700.2a Chaos Charm is not castable with no creature on the board at all" $ do
    chaosCharm <- S.printingOf s registry "Chaos Charm"
    mountain <- S.printingOf s registry "Mountain"
    let (gs0, oid) = S.handOne chaosCharm (S.landsInPlay mountain 1)
    Spec.assertBool s (not (S.castable S.alice oid gs0)) "no mode is fillable"

-- Dream's Grip's two modes, in printed order (CR 700.2 /
-- data/cards/dreams-grip.json):
--   0. "Tap target permanent."   -- slot "tapped"
--   1. "Untap target permanent." -- slot "untapped"
-- plus "Entwine {1}" (CR 702.42).
--
-- The board: alice has `islands` untapped Islands, bob has a Goblin Piker
-- (untapped) and a Wall of Stone (TAPPED), and Dream's Grip is in alice's hand.
-- The two victims start in OPPOSITE tap states on purpose -- an entwined cast
-- that tapped the Piker and untapped the Wall leaves a board that neither mode
-- alone can produce, and that a cast which fused the two slots could not produce
-- either.
entwineBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Int ->
  (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
entwineBoard island dreamsGrip piker wallOfStone islands =
  let (pikerId, gs1) = S.addCreature piker S.bob (S.landsInPlay island islands)
      (wallId, gs2) = S.addCreature wallOfStone S.bob gs1
      (gs, spellId) = S.handOne dreamsGrip (S.tapObject wallId gs2)
   in (gs, spellId, pikerId, wallId)

tapSlot :: SlotName.SlotName
tapSlot = SlotName.MkSlotName (Text.pack "tapped")

-- Answers CR 702.42a's entwine question with `decision`, aims mode 0's "tapped"
-- slot at `toTap` and every other slot -- which for Dream's Grip is mode 1's
-- "untapped", its only other one -- at `toUntap`, and defers everything else to
-- S.identityAnswer, so the MODE choice on a declined cast is identityAnswer's
-- (the first legal mode, which is the tap one).
grips ::
  EntwineDecision.EntwineDecision ->
  ObjectId.ObjectId ->
  ObjectId.ObjectId ->
  Prompt.Prompt r ->
  r
grips decision toTap toUntap p = case p of
  Prompt.ChooseEntwine {} -> decision
  Prompt.ChooseTargets _ _ _ sets ->
    Map.mapWithKey (\slot _ -> Recipient.ToObject (if slot == tapSlot then toTap else toUntap)) sets
  _ -> S.identityAnswer p

-- Was CR 702.42a's question actually put to the player, and what did they say?
entwineAnnouncements :: [Response.Response] -> [EntwineDecision.EntwineDecision]
entwineAnnouncements responses = [d | Response.AnnouncedEntwine d <- responses]

-- Cast and resolve in one go, keeping the transcript -- the ManaSpec shape, so
-- an assertion about a prompt reads the answers actually given rather than
-- reaching past the prompt.
castAndResolve ::
  (forall r. Prompt.Prompt r -> r) ->
  GameState.GameState ->
  ObjectId.ObjectId ->
  ([Response.Response], GameState.GameState)
castAndResolve answer gs oid =
  let ((_, cast), asked) = Replay.record answer gs (S.cast S.alice oid)
   in (asked, snd (S.runPureWith answer cast Stack.resolveTop))

-- CR 702.42: entwine, the first keyword that decides a modal spell's SELECTION
-- while it is being cast rather than reading it off the card.
entwineSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
entwineSpec s registry = Spec.describe s "Entwine" $ do
  -- CR 702.42a's "you MAY choose all modes": declining is a real answer, and
  -- it leaves the printed ChooseExactly 1 alone.
  Spec.it s "CR 702.42a declining entwine chooses one mode: the Piker is tapped, the Wall stays tapped" $ do
    island <- S.printingOf s registry "Island"
    dreamsGrip <- S.printingOf s registry "Dream's Grip"
    piker <- S.printingOf s registry "Goblin Piker"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    let (gs, spellId, pikerId, wallId) = entwineBoard island dreamsGrip piker wallOfStone 2
        (asked, after) = castAndResolve (grips EntwineDecision.Declines pikerId wallId) gs spellId
    Spec.assertEqWith s "the player was asked, and declined" (entwineAnnouncements asked) [EntwineDecision.Declines]
    Spec.assertEqWith s "mode 0 tapped the Piker" (tapStateOf pikerId after) (Just TapState.Tapped)
    Spec.assertEqWith s "mode 1 never ran, so the Wall is still tapped" (tapStateOf wallId after) (Just TapState.Tapped)
    Spec.assertEqWith s "only {U} was paid, so one Island is still untapped" (S.tappedCount S.alice after) 1
  -- CR 702.42a's "instead of just the number specified": paying widens the
  -- selection from the printed one to ALL of them.
  Spec.it s "CR 702.42a paying entwine chooses both modes: the Piker is tapped AND the Wall is untapped" $ do
    island <- S.printingOf s registry "Island"
    dreamsGrip <- S.printingOf s registry "Dream's Grip"
    piker <- S.printingOf s registry "Goblin Piker"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    let (gs, spellId, pikerId, wallId) = entwineBoard island dreamsGrip piker wallOfStone 2
        (asked, after) = castAndResolve (grips EntwineDecision.Entwines pikerId wallId) gs spellId
    Spec.assertEqWith s "the player was asked, and entwined" (entwineAnnouncements asked) [EntwineDecision.Entwines]
    Spec.assertEqWith s "mode 0 tapped the Piker" (tapStateOf pikerId after) (Just TapState.Tapped)
    Spec.assertEqWith s "mode 1 untapped the Wall" (tapStateOf wallId after) (Just TapState.Untapped)
    Spec.assertEqWith s "{U} plus the entwine {1}: both Islands are tapped" (S.tappedCount S.alice after) 2
  -- CR 702.42b: "If the entwine cost was paid, follow the text of each of the
  -- modes in the order written on the card when the spell resolves." Aiming
  -- BOTH slots at one untapped permanent is what makes the order observable:
  -- tap-then-untap ends untapped, and untap-then-tap ends tapped.
  Spec.it s "CR 702.42b both modes aimed at one untapped Piker resolve tap-then-untap, leaving it untapped" $ do
    island <- S.printingOf s registry "Island"
    dreamsGrip <- S.printingOf s registry "Dream's Grip"
    piker <- S.printingOf s registry "Goblin Piker"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    let (gs, spellId, pikerId, _) = entwineBoard island dreamsGrip piker wallOfStone 2
        (_, after) = castAndResolve (grips EntwineDecision.Entwines pikerId pikerId) gs spellId
    Spec.assertEqWith s "the untap mode ran last, as printed, so the Piker ends untapped" (tapStateOf pikerId after) (Just TapState.Untapped)
  -- CR 601.2b/601.2f-h: the additional cost is a real cost. With one Island
  -- there is {U} and nothing more, so entwining is not on offer at all -- and
  -- the ordinary modal cast still is.
  Spec.it s "CR 702.42a with only {U} available the entwine option is not offered, and the ordinary cast still is" $ do
    island <- S.printingOf s registry "Island"
    dreamsGrip <- S.printingOf s registry "Dream's Grip"
    piker <- S.printingOf s registry "Goblin Piker"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    let (gs, spellId, pikerId, wallId) = entwineBoard island dreamsGrip piker wallOfStone 1
        -- An interpreter that WOULD entwine: it never gets the chance, which
        -- is what makes this discriminating rather than a restatement of the
        -- answerer.
        (asked, after) = castAndResolve (grips EntwineDecision.Entwines pikerId wallId) gs spellId
    Spec.assertBool s (S.castable S.alice spellId gs) "the spell is still castable"
    Spec.assertEqWith s "no entwine question was put" (entwineAnnouncements asked) []
    Spec.assertEqWith s "mode 0 tapped the Piker" (tapStateOf pikerId after) (Just TapState.Tapped)
    Spec.assertEqWith s "and mode 1 never ran: the Wall is still tapped" (tapStateOf wallId after) (Just TapState.Tapped)
  -- The gate itself, asked directly, so the two arms of "is entwining
  -- available" are pinned apart from the cast that consumes them.
  Spec.it s "CR 702.42a Cast.entwineOffer is the {1} with two Islands and Nothing with one" $ do
    island <- S.printingOf s registry "Island"
    dreamsGrip <- S.printingOf s registry "Dream's Grip"
    piker <- S.printingOf s registry "Goblin Piker"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    let (rich, richSpell, _, _) = entwineBoard island dreamsGrip piker wallOfStone 2
        (poor, poorSpell, _, _) = entwineBoard island dreamsGrip piker wallOfStone 1
    Spec.assertEqWith
      s
      "two Islands: the additional cost is {1}"
      (Cast.entwineOffer S.alice richSpell (Cost.costsFor (S.printingName dreamsGrip) richSpell rich) rich)
      (Just (Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]), Cost.Type.components = []}))
    Spec.assertEqWith s "one Island: unaffordable, so not offered" (Cast.entwineOffer S.alice poorSpell (Cost.costsFor (S.printingName dreamsGrip) poorSpell poor) poor) Nothing
  -- A card with no entwine is never asked, which is the other half of "where
  -- the rules leave nothing to ask, don't prompt".
  Spec.it s "CR 702.42a a modal spell without entwine (Chaos Charm) is never offered one" $ do
    mountain <- S.printingOf s registry "Mountain"
    chaosCharm <- S.printingOf s registry "Chaos Charm"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs0, spellId) = S.handOne chaosCharm (S.landsInPlay mountain 3)
        (_, gs) = S.addCreature piker S.bob gs0
    Spec.assertEqWith s "no entwine cost to offer" (Cast.entwineOffer S.alice spellId (Cost.costsFor (S.printingName chaosCharm) spellId gs) gs) Nothing

-- CR 303.4a/601.2c: an Aura spell's target is its enchant slot, defined by the
-- card, not by a mode -- Unholy Strength (the Auras gate card) has one empty
-- mode and a Face.enchant of "target creature" (CardSpec's auraCardSpec).
-- Task 6 merges Card.enchantSpecs into allTargetSpecs/modesTargetSpecs and
-- teaches Target.fillableModes the extra slots a card declares outside its
-- modes, so castability sees the enchant slot too -- without either function
-- learning what an Aura is.
auraTargetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
auraTargetSpec s registry = Spec.describe s "AuraTarget" $ do
  Spec.it s "CR 303.4a: an Aura spell targets, so it prompts for the creature it enchants" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    let base = S.landsInPlay swamp 1
        (creature, withCreature) = S.addCreature piker S.bob base
        (gs, spellId) = S.handOne unholyStrength withCreature
        specs = Card.modesTargetSpecs (Set.singleton (ModeIndex.MkModeIndex 0)) (S.faceOf unholyStrength)
    Spec.assertEqWith s "one slot, the enchant slot" (Set.singleton Card.enchantSlot) (Map.keysSet specs)
    Spec.assertEqWith
      s
      "its legal set is the one creature"
      (Target.legalSets Nothing spellId specs gs)
      (Map.singleton Card.enchantSlot (Set.singleton (Recipient.ToCreature creature)))
  -- CR 601.2c: a spell whose required target has no legal choice cannot be
  -- cast at all. Reading only Mode.targetSpecs would call this castable and
  -- let it be countered on resolution instead.
  Spec.it s "CR 601.2c: an Aura with no creature on the battlefield is not castable" $ do
    swamp <- S.printingOf s registry "Swamp"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    let base = S.landsInPlay swamp 1
        (gs, spellId) = S.handOne unholyStrength base
    Spec.assertBool s (not (S.castable S.alice spellId gs)) "not castable with an empty board"

-- alice controls `n` untapped Mountains and has one card of `printing` wherever
-- `place` puts it, with priority in her own precombat main phase.
boardWith :: (Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)) -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
boardWith place mountain printing n =
  let (oid, gs) = place printing S.alice (S.landsInPlay mountain n)
   in ( oid,
        gs
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- The same board with the card in alice's HAND / in her GRAVEYARD.
inHandWith, inGraveyardWith :: Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
inHandWith = boardWith S.addHandCard
inGraveyardWith = boardWith S.addGraveyardCard

theRed :: ManaSymbol.ManaSymbol
theRed = ManaSymbol.OfType (ManaType.Colored Color.Red)

-- Firebolt {R} Sorcery: "Firebolt deals 2 damage to any target." / "Flashback
-- {4}{R}" -- rule 702.34a's TWO static abilities on one card, and the proving
-- card for casting from a zone other than the hand.
--
-- Three of its rulings are what this group encodes. "You must still follow any
-- timing restrictions and permissions, including those based on the card's type.
-- For instance, you can cast a sorcery using flashback only when you could
-- normally cast a sorcery." "A spell cast using flashback will always be exiled
-- afterward, whether it resolves, is countered, or leaves the stack in some
-- other way." "The mana value of the spell is determined only by its mana cost,
-- no matter what the total cost to cast the spell was."
fireboltSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fireboltSpec s registry = Spec.describe s "Firebolt" $ do
  -- The headline loop, end to end: hand -> stack -> graveyard -> stack ->
  -- EXILE. The exile is the discriminating assertion -- rule 702.34a's
  -- second static ability, and the half a bare alternative cost cannot
  -- reach.
  Spec.it s "CR 702.34a whole card: cast for {R}, then flash back for {4}{R} and be exiled" $ do
    mountain <- S.printingOf s registry "Mountain"
    firebolt <- S.printingOf s registry "Firebolt"
    let (fromHand, gs) = inHandWith mountain firebolt 6
        cast1 = S.runPure S.identityAnswer gs (S.cast S.alice fromHand)
        resolved1 = S.runPure S.identityAnswer cast1 Stack.resolveTop
    Spec.assertEqWith s "the hand cast dealt 2 (identityAnswer targets the lowest recipient)" (S.lifeOf S.alice resolved1) (Just 18)
    case Game.zoneMembers Zone.Graveyard S.alice resolved1 of
      [inGraveyard] -> do
        Spec.assertBool s (S.castable S.alice inGraveyard resolved1) "castable from the graveyard"
        Spec.assertBool s (elem (A.Cast inGraveyard (S.printingName firebolt)) (Action.legalActions S.alice resolved1)) "and offered as a legal action"
        let cast2 = S.runPure S.identityAnswer resolved1 (S.cast S.alice inGraveyard)
            resolved2 = S.runPure S.identityAnswer cast2 Stack.resolveTop
        Spec.assertEqWith s "the flashback cast dealt 2 more" (S.lifeOf S.alice resolved2) (Just 16)
        Spec.assertEqWith s "it did NOT go back to the graveyard" (Game.zoneMembers Zone.Graveyard S.alice resolved2) []
        Spec.assertEqWith s "it was exiled instead" (length (Game.zoneMembers Zone.Exile S.alice resolved2)) 1
      other -> Spec.assertFailure s ("expected one card in the graveyard, got " <> show (length other))
  -- Rule 702.34a again, on the OTHER exit from the stack: "A spell cast
  -- using flashback will always be exiled afterward, whether it resolves, is
  -- countered, or leaves the stack in some other way."
  Spec.it s "CR 702.34a a countered flashback spell is exiled too" $ do
    mountain <- S.printingOf s registry "Mountain"
    firebolt <- S.printingOf s registry "Firebolt"
    let (inGraveyard, gs) = inGraveyardWith mountain firebolt 5
        cast = S.runPure S.identityAnswer gs (S.cast S.alice inGraveyard)
    case GameState.stack cast of
      [] -> Spec.assertFailure s "expected the flashback spell on the stack"
      onStack : _ -> do
        let countered = S.runPure S.identityAnswer cast (Event.counter S.noSource S.bob onStack)
        Spec.assertEqWith s "not in the graveyard" (Game.zoneMembers Zone.Graveyard S.alice countered) []
        Spec.assertEqWith s "exiled" (length (Game.zoneMembers Zone.Exile S.alice countered)) 1
  -- The self-scoping in rule 702.34a's "exile THIS card". A flashback spell
  -- waiting on the stack must not exile every OTHER card of its controller's
  -- that heads for a graveyard while it sits there -- which is exactly what a
  -- destination-only pattern (Rest in Peace's shape) would do.
  Spec.it s "CR 702.34a the exile replacement is scoped to the spell itself" $ do
    mountain <- S.printingOf s registry "Mountain"
    firebolt <- S.printingOf s registry "Firebolt"
    piker <- S.printingOf s registry "Goblin Piker"
    let (inGraveyard, gs0) = inGraveyardWith mountain firebolt 5
        (bystander, gs1) = S.addCreature piker S.alice gs0
        cast = S.runPure S.identityAnswer gs1 (S.cast S.alice inGraveyard)
        buried = S.runPure S.identityAnswer cast (Event.changeZone bystander Zone.Graveyard)
    Spec.assertEqWith s "the Piker went to the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice buried)) 1
    Spec.assertEqWith s "and nothing was exiled" (Game.zoneMembers Zone.Exile S.alice buried) []
  -- Crux (a): flashback's cost is available only from the GRAVEYARD. A cost
  -- simply added to Face.alternativeCosts would make Firebolt castable from
  -- hand for {4}{R} as well, which rule 702.34a does not say.
  Spec.it s "CR 702.34a the flashback cost is offered only from the graveyard" $ do
    mountain <- S.printingOf s registry "Mountain"
    firebolt <- S.printingOf s registry "Firebolt"
    let (fromHand, handBoard) = inHandWith mountain firebolt 6
        (fromGraveyard, graveyardBoard) = inGraveyardWith mountain firebolt 6
        manaOf oid gs = fmap Cost.Type.mana (Cost.costsFor (S.printingName firebolt) oid gs)
    Spec.assertEqWith
      s
      "from hand, the printed {R} and nothing else"
      (manaOf fromHand handBoard)
      [Just (ManaCost.MkManaCost [theRed])]
    Spec.assertEqWith
      s
      "from the graveyard, the flashback {4}{R} and nothing else"
      (manaOf fromGraveyard graveyardBoard)
      [Just (ManaCost.MkManaCost [ManaSymbol.Generic 4, theRed])]
    -- CR 202.3 and the mana-value ruling ("The mana value of the spell is
    -- determined only by its mana cost, no matter what the total cost to
    -- cast the spell was"): neither of rule 702.34a's abilities touches the
    -- card's own mana cost.
    Spec.assertEqWith
      s
      "and the printed mana cost is still {R}"
      (Face.manaCost (S.faceOf firebolt))
      (Just (ManaCost.MkManaCost [theRed]))
  Spec.it s "CR 118.3 four Mountains do not pay the flashback {4}{R}; five do" $ do
    mountain <- S.printingOf s registry "Mountain"
    firebolt <- S.printingOf s registry "Firebolt"
    let (four, fourLands) = inGraveyardWith mountain firebolt 4
        (five, fiveLands) = inGraveyardWith mountain firebolt 5
    Spec.assertBool s (not (S.castable S.alice four fourLands)) "not castable with four"
    Spec.assertBool s (S.castable S.alice five fiveLands) "castable with five"
  -- The ruling: "you can cast a sorcery using flashback only when you could
  -- normally cast a sorcery." The permission is about the ZONE, not the
  -- timing.
  Spec.it s "CR 117.1a flashback does not lift the sorcery timing restriction" $ do
    mountain <- S.printingOf s registry "Mountain"
    firebolt <- S.printingOf s registry "Firebolt"
    let (inGraveyard, gs) = inGraveyardWith mountain firebolt 5
        upkeep = gs {GameState.phase = Phase.Beginning BeginningStep.Upkeep}
    Spec.assertBool s (S.castable S.alice inGraveyard gs) "castable in her own main phase"
    Spec.assertBool s (not (S.castable S.alice inGraveyard upkeep)) "not castable in the upkeep"
  -- The negative that keeps the permission a PERMISSION: an ordinary card in
  -- the graveyard stays uncastable. Lightning Bolt is affordable from six
  -- Mountains, so only the missing permission can be stopping it.
  Spec.it s "CR 601.3 a card without flashback is not castable from the graveyard" $ do
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (inGraveyard, gs) = inGraveyardWith mountain bolt 6
        (inHand, handBoard) = inHandWith mountain bolt 6
    Spec.assertBool s (S.castable S.alice inHand handBoard) "castable from hand"
    Spec.assertBool s (not (S.castable S.alice inGraveyard gs)) "not castable from the graveyard"
    Spec.assertBool s (not (any (S.isCastOf inGraveyard) (Action.legalActions S.alice gs))) "and not offered"

-- CR 205.4e: "A player can't cast a legendary instant or sorcery spell unless
-- that player controls a legendary creature or a legendary planeswalker." The
-- OTHER half of what the legendary supertype means -- CR 205.4d's legend rule
-- (CR 704.5j) is Pawl.Engine.Sba's, and this one is Pawl.Engine.Cast's.
--
-- The proving card is a LABELED SYNTHETIC: "Synthetic Legendary Sorcery", a {0}
-- legendary sorcery whose one effect is "you lose 3 life". No real legendary
-- instant or sorcery is expressible today (#328).
--
-- The three permanents in play across these cases are what make the assertions
-- discriminating. Thalia, Guardian of Thraben is the pool's only legendary
-- CREATURE. Mindslaver is a legendary ARTIFACT -- the case that fails if the
-- check reads the supertype and forgets the card type. And Thalia under bob's
-- control is the case that fails if the check forgets "that player controls".
legendarySpellSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
legendarySpellSpec s registry = Spec.describe s "LegendarySpell" $ do
  -- The control for every negative below: same board, same one Mountain,
  -- plus a legendary creature. Thalia taxes the sorcery {1} (her own
  -- "noncreature spells cost {1} more"), which the Mountain pays -- so the
  -- other cases, where nothing taxes it at all, are affordable a fortiori
  -- and only CR 205.4e can be stopping them.
  Spec.it s "CR 205.4e castable while its caster controls a legendary creature" $ do
    mountain <- S.printingOf s registry "Mountain"
    thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
    sorcery <- S.printingOf s registry "Synthetic Legendary Sorcery"
    let (oid, gs) = inHandWith mountain sorcery 1
        board = snd (S.addCreature thalia S.alice gs)
    Spec.assertBool s (S.castable S.alice oid board) "castable"
    Spec.assertBool s (elem (A.Cast oid (S.printingName sorcery)) (Action.legalActions S.alice board)) "and offered as a legal action"
  -- Rule 205.4e's SECOND disjunct, "or a legendary planeswalker". Jace
  -- Beleren is the pool's only one, and it is not a creature -- so this case
  -- fails for any reading that collapsed the rule onto the creature limb.
  Spec.it s "CR 205.4e castable while its caster controls a legendary planeswalker" $ do
    mountain <- S.printingOf s registry "Mountain"
    jace <- S.printingOf s registry "Jace Beleren"
    sorcery <- S.printingOf s registry "Synthetic Legendary Sorcery"
    let (oid, gs) = inHandWith mountain sorcery 1
        board = snd (S.addCreature jace S.alice gs)
    Spec.assertBool s (not (Card.isCreature (S.faceOf jace))) "not a creature"
    Spec.assertBool s (S.castable S.alice oid board) "castable"
    Spec.assertBool s (elem (A.Cast oid (S.printingName sorcery)) (Action.legalActions S.alice board)) "and offered as a legal action"
  Spec.it s "CR 205.4e not castable with no legendary permanent at all" $ do
    mountain <- S.printingOf s registry "Mountain"
    sorcery <- S.printingOf s registry "Synthetic Legendary Sorcery"
    let (oid, gs) = inHandWith mountain sorcery 1
    Spec.assertBool s (not (S.castable S.alice oid gs)) "not castable"
    Spec.assertBool s (not (any (S.isCastOf oid) (Action.legalActions S.alice gs))) "and not offered"
  -- The supertype alone is not the condition: rule 205.4e names a legendary
  -- CREATURE (or planeswalker), and Mindslaver is a legendary artifact.
  Spec.it s "CR 205.4e a legendary artifact does not satisfy it" $ do
    mountain <- S.printingOf s registry "Mountain"
    mindslaver <- S.printingOf s registry "Mindslaver"
    sorcery <- S.printingOf s registry "Synthetic Legendary Sorcery"
    let (oid, gs) = inHandWith mountain sorcery 1
        board = snd (S.addCreature mindslaver S.alice gs)
    Spec.assertBool s (not (S.castable S.alice oid board)) "not castable"
    Spec.assertBool s (not (any (S.isCastOf oid) (Action.legalActions S.alice board))) "and not offered"
  -- "unless THAT PLAYER controls": an opponent's legendary creature is no
  -- help. Bob's Thalia still taxes alice (her ability is EachPlayer-scoped),
  -- so a second Mountain keeps the spell affordable and CR 205.4e the only
  -- thing left to fail.
  Spec.it s "CR 205.4e an opponent's legendary creature does not satisfy it" $ do
    mountain <- S.printingOf s registry "Mountain"
    thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
    sorcery <- S.printingOf s registry "Synthetic Legendary Sorcery"
    let (oid, gs) = inHandWith mountain sorcery 2
        board = snd (S.addCreature thalia S.bob gs)
    Spec.assertBool s (not (S.castable S.alice oid board)) "not castable"
  -- The scope of the restriction, from the other side: rule 205.4e is about
  -- a legendary INSTANT OR SORCERY, so a legendary creature spell is cast
  -- from an empty board exactly as before. A check that read only the
  -- supertype would make Thalia uncastable until a Thalia was already out.
  Spec.it s "CR 205.4e does not touch a legendary creature spell" $ do
    plains <- S.printingOf s registry "Plains"
    thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
    let (oid, gs) = inHandWith plains thalia 2
    Spec.assertBool s (S.castable S.alice oid gs) "castable"
    Spec.assertBool s (elem (A.Cast oid (S.printingName thalia)) (Action.legalActions S.alice gs)) "and offered as a legal action"
  -- Gameplay level, through the stack: the permitted cast resolves and its
  -- effect lands, so the gate is a gate and not a silent no-op.
  Spec.it s "CR 205.4e the permitted cast resolves" $ do
    mountain <- S.printingOf s registry "Mountain"
    thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
    sorcery <- S.printingOf s registry "Synthetic Legendary Sorcery"
    let (oid, gs) = inHandWith mountain sorcery 1
        board = snd (S.addCreature thalia S.alice gs)
        cast = S.runPure S.identityAnswer board (S.cast S.alice oid)
        resolved = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertEqWith s "alice lost 3 life" (S.lifeOf S.alice resolved) (Just 17)

-- CR 601.3: "A player can begin to cast a spell only if a rule or effect allows
-- that player to cast it and no rule or effect prohibits that player from casting
-- it." The PROHIBITION half, printed on a card about itself -- the direction
-- opposite to every CastingPermission, which grants a cast the rules would refuse.
--
-- Rally the Troops ({W} instant, Portal Three Kingdoms) is the proving card:
-- "Cast this spell only during the declare attackers step and only if you've been
-- attacked this step. / Untap all creatures you control." Its payload is
-- Aggravated Assault's untap clause, so nothing but the restriction is new.
--
-- This fixture is the declare attackers step BEFORE the declaration: alice has
-- one Piker able to attack, bob defends (CR 506.2) with one TAPPED Piker and
-- holds Rally plus a Plains. Each test declares the attack itself, or does not,
-- which is what makes the "you've been attacked" clause separable from the step.
--
-- alice holds a Rally and a Plains of her own: she is in the same step, with the
-- same mana, and the only thing she lacks is having been attacked -- so a check
-- that read the step alone would offer her the cast.
rallyBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
rallyBoard piker plains rally =
  let (gs0, _, theirs) = S.combatBoardOf [piker] [piker]
      (_, gs1) = S.addCreature plains S.bob gs0
      (bobsRally, gs2) = S.addHandCard rally S.bob gs1
      (_, gs3) = S.addCreature plains S.alice gs2
      (alicesRally, gs4) = S.addHandCard rally S.alice gs3
      tapped = foldr S.tapObject gs4 theirs
   in case theirs of
        bobsPiker : _ -> (bobsRally, alicesRally, bobsPiker, tapped)
        -- combatBoardOf returns one id per printing given, so this is
        -- unreachable; a bogus id fails the assertions rather than the suite.
        [] -> (bobsRally, alicesRally, S.noSource, tapped)

-- CR 508.1b: is this offered attack target a planeswalker? The predicate the
-- Rally case picks its announcement with; CombatSpec carries the same one for
-- the combat side of this rule.
isPlaneswalkerTarget :: AttackTarget.AttackTarget -> Bool
isPlaneswalkerTarget target = case target of
  AttackTarget.OfPlaneswalker _ -> True
  AttackTarget.OfPlayer _ -> False

printedCastingRestrictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
printedCastingRestrictionSpec s registry = Spec.describe s "PrintedCastingRestriction" $ do
  -- Both clauses satisfied: bob is the defending player (CR 506.2), attackers
  -- have joined (CR 508.8), and the game is in the declare attackers step.
  Spec.it s "CR 601.3 castable once bob has been attacked in the declare attackers step" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    rally <- S.printingOf s registry "Rally the Troops"
    let (bobsRally, _, _, board) = rallyBoard piker plains rally
        attacked = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.alice)
    Spec.assertBool s (S.castable S.bob bobsRally attacked) "castable"
    Spec.assertBool s (elem (A.Cast bobsRally (S.printingName rally)) (Action.legalActions S.bob attacked)) "and offered as a legal action"
  -- CR 306.6 / CR 508.1b: the same board, with the attack aimed at bob's
  -- planeswalker instead of at bob. Eightfold Maze's ruling is the reading being
  -- pinned -- "If all the attacking creatures attack your planeswalkers, you
  -- can't cast Eightfold Maze. To cast it, a creature needs to have attacked
  -- _you_" -- so this is the case that says "you've been attacked" is a question
  -- about the ATTACK TARGET and not about whether a declaration happened.
  --
  -- Its own control is the test above: one Piker, one step, one declaration; the
  -- only difference is what it was announced as attacking.
  Spec.it s "CR 601.3 not castable when the only attacker attacked bob's planeswalker instead" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    rally <- S.printingOf s registry "Rally the Troops"
    jace <- S.printingOf s registry "Jace Beleren"
    let (bobsRally, _, _, board) = rallyBoard piker plains rally
        (jaceId, withJace) = S.addCreature jace S.bob board
        loyal = S.addCounter CounterKind.Loyalty 3 jaceId withJace
        atPlaneswalker :: Prompt.Prompt r -> r
        atPlaneswalker p = case p of
          Prompt.ChooseAttackTarget _ _ _ options -> case filter isPlaneswalkerTarget (NonEmpty.toList options) of
            target : _ -> target
            [] -> NonEmpty.head options
          _ -> S.aggressiveAnswer p
        attacked = S.runPure atPlaneswalker loyal (Combat.declareAttackers S.alice)
    Spec.assertEqWith
      s
      "the Piker really was declared, attacking the planeswalker"
      (Map.elems (Combat.Type.attackers (GameState.combat attacked)))
      [AttackTarget.OfPlaneswalker jaceId]
    Spec.assertBool s (not (S.castable S.bob bobsRally attacked)) "not castable"
    Spec.assertBool s (not (any (S.isCastOf bobsRally) (Action.legalActions S.bob attacked))) "and not offered"
  -- The "only if you've been attacked this step" clause, isolated: the step is
  -- right and nobody has attacked yet.
  Spec.it s "CR 601.3 not castable in the declare attackers step before attackers are declared" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    rally <- S.printingOf s registry "Rally the Troops"
    let (bobsRally, _, _, board) = rallyBoard piker plains rally
    Spec.assertBool s (not (S.castable S.bob bobsRally board)) "not castable"
    Spec.assertBool s (not (any (S.isCastOf bobsRally) (Action.legalActions S.bob board))) "and not offered"
  -- The same clause from the other side, and the reason the check cannot be a
  -- question about the step alone: Eightfold Maze's ruling is "To cast it, a
  -- creature needs to have attacked _you_", and nothing attacked alice.
  Spec.it s "CR 601.3 the ATTACKING player is not offered it in the same step" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    rally <- S.printingOf s registry "Rally the Troops"
    let (_, alicesRally, _, board) = rallyBoard piker plains rally
        attacked = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.alice)
    Spec.assertBool s (not (S.castable S.alice alicesRally attacked)) "not castable"
    Spec.assertBool s (not (any (S.isCastOf alicesRally) (Action.legalActions S.alice attacked))) "and not offered"
  -- The "only during the declare attackers step" clause, isolated: bob HAS
  -- been attacked -- CR 511.3 keeps the combat record live until the end of
  -- combat step ends -- and the window has passed.
  --
  -- Carries its own control, in the same step and for the same player: bob's
  -- Bolt is still offered, so what stops the Rally is the clause and not the
  -- step being closed to bob altogether.
  Spec.it s "CR 601.3 not castable in the declare blockers step, though bob was attacked" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    rally <- S.printingOf s registry "Rally the Troops"
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (bobsRally, _, _, board) = rallyBoard piker plains rally
        (boltId, withBolt) = S.addHandCard bolt S.bob (snd (S.addCreature mountain S.bob board))
        attacked = S.runPure S.aggressiveAnswer withBolt (Combat.declareAttackers S.alice)
        later = attacked {GameState.phase = Phase.Combat CombatStep.DeclareBlockers}
    Spec.assertBool s (Set.member (AttackTarget.OfPlayer S.bob) (Combat.Type.attacked (GameState.combat later))) "still attacked"
    Spec.assertBool s (not (S.castable S.bob bobsRally later)) "not castable"
    Spec.assertBool s (not (any (S.isCastOf bobsRally) (Action.legalActions S.bob later))) "and not offered"
    Spec.assertBool s (elem (A.Cast boltId (S.printingName bolt)) (Action.legalActions S.bob later)) "bob's unrestricted instant still is"
  -- CR 508.4's last-but-one sentence: "Such creatures are 'attacking' but, for
  -- the purposes of trigger events and effects, they never 'attacked.'" The
  -- words that reach a printed casting restriction are "AND EFFECTS" -- a
  -- restriction is an effect, not a trigger. CR 508.3b works the same principle
  -- out for the trigger case ("Whenever [a player, planeswalker, or battle] is
  -- attacked" "won't trigger if a creature is put onto the battlefield attacking
  -- that player or permanent"), so it corroborates rather than governs.
  --
  -- So Rally's "only if you've been attacked this step" must NOT be satisfied by
  -- a creature that arrived attacking. A DIRECT call, the shape TurnSpec's CR
  -- 508.8 case takes for the same clause, so the rule is stated with no card in
  -- the way; the pool reaches it through Meandering Towershell, whose delayed
  -- ability returns it attacking on a turn its controller declares nothing.
  --
  -- The two records this separates are both live: CR 508.8's skip counts a
  -- creature put onto the battlefield attacking ("or put onto the battlefield
  -- attacking"), and this rule does not. Asserting both here is what keeps a fix
  -- from collapsing them again.
  Spec.it s "CR 508.4 a creature put onto the battlefield attacking never attacked, so Rally stays uncastable" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    rally <- S.printingOf s registry "Rally the Troops"
    let (bobsRally, _, _, board) = rallyBoard piker plains rally
        mine = Projection.controls S.alice board
        joined = S.runPure S.identityAnswer board (Foldable.traverse_ Combat.putOntoBattlefieldAttacking mine)
        combat = GameState.combat joined
    Spec.assertEqWith s "nothing was DECLARED" (S.attackerDeclarationsOf joined) []
    -- CR 508.8's record does count it -- that is the rule's second clause, and
    -- the skip depends on it.
    Spec.assertBool s (Set.member (AttackTarget.OfPlayer S.bob) (Combat.Type.attacked combat)) "CR 508.8 counts it: something is attacking bob"
    -- CR 508.3b/508.4's record does not.
    Spec.assertBool s (not (Set.member (AttackTarget.OfPlayer S.bob) (Combat.Type.declaredAttacked combat))) "but bob was never DECLARED-attacked"
    Spec.assertBool s (not (S.castable S.bob bobsRally joined)) "so Rally is not castable"
    Spec.assertBool s (not (any (S.isCastOf bobsRally) (Action.legalActions S.bob joined))) "and not offered"
    -- The control twin: the SAME board with a real declaration makes it
    -- castable, so what the assertions above measure is the declaration and not
    -- something else about the fixture.
    let declared = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.alice)
    Spec.assertBool s (S.castable S.bob bobsRally declared) "declared instead, it IS castable"

  -- CR 117.1a is not what is stopping it: an unrestricted instant with the
  -- same cost, in the same hand, in the same step, is castable. Without this
  -- the negatives above would also pass on an engine that refused every cast
  -- in the declare attackers step.
  Spec.it s "CR 117.1a an unrestricted instant is castable in the same step" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    rally <- S.printingOf s registry "Rally the Troops"
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (_, _, _, board) = rallyBoard piker plains rally
        (boltId, withBolt) = S.addHandCard bolt S.alice (snd (S.addCreature mountain S.alice board))
    Spec.assertBool s (S.castable S.alice boltId withBolt) "castable"
    Spec.assertBool s (elem (A.Cast boltId (S.printingName bolt)) (Action.legalActions S.alice withBolt)) "and offered as a legal action"
  -- Gameplay level, through the stack: the permitted cast resolves and its
  -- effect lands, so the gate is a gate and not a silent no-op.
  Spec.it s "CR 601.3 the permitted cast resolves and untaps bob's creatures" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    rally <- S.printingOf s registry "Rally the Troops"
    let (bobsRally, _, bobsPiker, board) = rallyBoard piker plains rally
        attacked = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.alice)
        cast = S.runPure S.identityAnswer attacked (S.cast S.bob bobsRally)
        resolved = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertEqWith s "tapped before" (tapStateOf bobsPiker attacked) (Just TapState.Tapped)
    Spec.assertEqWith s "untapped after" (tapStateOf bobsPiker resolved) (Just TapState.Untapped)
    -- "creatures YOU control" is the CASTER's, not everyone's. CR 508.1f taps
    -- alice's attacker as it is declared, and alice's only other permanent is
    -- an untapped Plains, so her tapped count is exactly her attacker -- before
    -- the spell and after it.
    Spec.assertEqWith s "alice's attacker was tapped to attack" (S.tappedCount S.alice attacked) 1
    Spec.assertEqWith s "and Rally did not untap it" (S.tappedCount S.alice resolved) 1

tapStateOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)

-- alice holds one Pouncing Cheetah and one War Mammoth, with four untapped
-- Forests -- enough for either one alone ({2}{G} and {3}{G}), so nothing below
-- turns on affordability. Returns the Cheetah's hand id and the Mammoth's.
--
-- The Mammoth is the CONTROL, and it is in the same hand and the same state on
-- purpose: it is a green creature spell whose only difference from the Cheetah
-- is the keyword, so a case that passed for both would be the timing gate
-- opening for every creature rather than for flash.
cheetahAndMammothInHand ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
cheetahAndMammothInHand forest cheetah warMammoth =
  let (gs0, cheetahId) = S.handOne cheetah (S.landsInPlay forest 4)
      (mammothId, gs1) = S.addHandCard warMammoth S.alice gs0
   in (gs1, cheetahId, mammothId)

-- CR 702.8a: "Flash is a static ability that functions in any zone from which
-- you could play the card it's on. 'Flash' means 'You may play this card any
-- time you could cast an instant.'"
--
-- Pouncing Cheetah is the whole producer: a {2}{G} 3/2 Cat whose entire rules
-- text is the keyword, so every case here is the keyword and nothing else.
flashSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
flashSpec s registry = Spec.describe s "Flash" $ do
  -- The baseline both halves start from: with no flash in the question at all,
  -- alice's own main phase and an empty stack is a window BOTH creatures pass.
  -- Without this the negatives below would also hold on an engine that refused
  -- the Mammoth everywhere.
  Spec.it s "CR 302.1 the control: in alice's own main phase with an empty stack, both are castable" $ do
    forest <- S.printingOf s registry "Forest"
    pouncingCheetah <- S.printingOf s registry "Pouncing Cheetah"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (gs, cheetahId, mammothId) = cheetahAndMammothInHand forest pouncingCheetah warMammoth
    Spec.assertBool s (S.castable S.alice cheetahId gs) "the Cheetah"
    Spec.assertBool s (S.castable S.alice mammothId gs) "and the Mammoth"
  -- CR 302.1's "during a main phase of THEIR turn", lifted for the Cheetah and
  -- not for the Mammoth.
  Spec.it s "CR 702.8a castable on an opponent's turn, where a creature without flash is not" $ do
    forest <- S.printingOf s registry "Forest"
    pouncingCheetah <- S.printingOf s registry "Pouncing Cheetah"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (gs, cheetahId, mammothId) = cheetahAndMammothInHand forest pouncingCheetah warMammoth
        bobsTurn = gs {GameState.activePlayer = S.bob}
    Spec.assertBool s (S.castable S.alice cheetahId bobsTurn) "the Cheetah is castable"
    Spec.assertBool s (elem (A.Cast cheetahId (S.printingName pouncingCheetah)) (Action.legalActions S.alice bobsTurn)) "and offered as a legal action"
    Spec.assertBool s (not (S.castable S.alice mammothId bobsTurn)) "the Mammoth is not"
    Spec.assertBool s (not (any (S.isCastOf mammothId) (Action.legalActions S.alice bobsTurn))) "and is not offered"
  -- CR 302.1's "when the stack is empty", same pair.
  Spec.it s "CR 702.8a castable with a non-empty stack, where a creature without flash is not" $ do
    forest <- S.printingOf s registry "Forest"
    pouncingCheetah <- S.printingOf s registry "Pouncing Cheetah"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (gs, cheetahId, mammothId) = cheetahAndMammothInHand forest pouncingCheetah warMammoth
        busy = gs {GameState.stack = [ObjectId.MkObjectId 999]}
    Spec.assertBool s (S.castable S.alice cheetahId busy) "the Cheetah is castable"
    Spec.assertBool s (not (S.castable S.alice mammothId busy)) "the Mammoth is not"
  -- CR 302.1's "during a MAIN PHASE", same pair.
  Spec.it s "CR 702.8a castable in the upkeep, where a creature without flash is not" $ do
    forest <- S.printingOf s registry "Forest"
    pouncingCheetah <- S.printingOf s registry "Pouncing Cheetah"
    warMammoth <- S.printingOf s registry "War Mammoth"
    let (gs, cheetahId, mammothId) = cheetahAndMammothInHand forest pouncingCheetah warMammoth
        upkeep = gs {GameState.phase = Phase.Beginning BeginningStep.Upkeep}
    Spec.assertBool s (S.castable S.alice cheetahId upkeep) "the Cheetah is castable"
    Spec.assertBool s (not (S.castable S.alice mammothId upkeep)) "the Mammoth is not"
  -- Rule 702.8a's second sentence is about WHEN, and says nothing about WHERE:
  -- it lets a player play the card any time they could cast an instant, not
  -- from anywhere they could not already. A graveyard needs a CR 601.3
  -- permission (flashback's), which flash is not.
  Spec.it s "CR 601.3 flash is a timing window and not a zone permission: a buried Cheetah is uncastable" $ do
    forest <- S.printingOf s registry "Forest"
    pouncingCheetah <- S.printingOf s registry "Pouncing Cheetah"
    let (gs, cheetahId) = S.handOne pouncingCheetah (S.landsInPlay forest 4)
        buried = S.runPure S.identityAnswer gs (Event.changeZone cheetahId Zone.Graveyard)
    Spec.assertBool s (S.castable S.alice cheetahId gs) "castable from the hand"
    Spec.assertEqWith s "and nothing castable once it is in the graveyard" (Cast.castableSpells S.alice buried) []
  -- Flash moves the window the cast is PROPOSED in and nothing else. Two rules
  -- say what is left untouched, and they are two:
  --
  --   * CR 601.2a, the stack half: "To propose the casting of a spell, a player
  --     first moves that card ... from where it is to the stack. It becomes the
  --     topmost object on the stack." So the Cheetah is a spell before it is a
  --     permanent, exactly as a sorcery-speed creature spell is.
  --   * CR 117.3c, the response half: "If a player has priority when they cast a
  --     spell ... that player receives priority afterward" -- and then CR 117.1a
  --     lets the opponent cast an instant when priority reaches them.
  Spec.it s "CR 601.2a / 117.3c an instant-speed creature spell still uses the stack and can be responded to" $ do
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    pouncingCheetah <- S.printingOf s registry "Pouncing Cheetah"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs0, cheetahId) = S.handOne pouncingCheetah (S.landsInPlay forest 4)
        (boltId, gs1) = S.addHandCard lightningBolt S.bob (snd (S.addCreature mountain S.bob gs0))
        bobsTurn = gs1 {GameState.activePlayer = S.bob}
        cast = S.runPure S.identityAnswer bobsTurn (S.cast S.alice cheetahId)
        resolved = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertEqWith s "one object on the stack" (length (GameState.stack cast)) 1
    Spec.assertEqWith s "and not on the battlefield yet" (S.creaturesInPlay S.alice cast) 0
    Spec.assertBool s (S.castable S.bob boltId cast) "bob may respond to it"
    Spec.assertEqWith s "it resolves into a creature like any other" (S.creaturesInPlay S.alice resolved) 1
  -- Pawl.Engine.Cast reads the PRINTED keyword. This is the case that says the
  -- CR 613 projection agrees with it for a card in a hand, so the reading is not
  -- a shortcut that a projected read would have caught.
  --
  -- Humility is why that agreement is the RIGHT answer rather than a
  -- coincidence: CR 109.2 makes its "all creatures" mean permanents on the
  -- battlefield, and a card in a hand is not one of them, so the window stays
  -- open and the projection says so.
  --
  -- Nothing in the pool could close it either way -- no effect can put a
  -- keyword-changing modification on a card in a hand at all (#160).
  -- Pawl.Engine.Keyword.hasFlash carries that argument in full.
  Spec.it s "CR 702.8a the projection of a card in hand carries flash, and Humility does not reach it" $ do
    forest <- S.printingOf s registry "Forest"
    pouncingCheetah <- S.printingOf s registry "Pouncing Cheetah"
    warMammoth <- S.printingOf s registry "War Mammoth"
    humility <- S.printingOf s registry "Humility"
    let (gs, cheetahId, mammothId) = cheetahAndMammothInHand forest pouncingCheetah warMammoth
        humbled = (S.withHumility humility gs) {GameState.activePlayer = S.bob}
    Spec.assertBool s (Projection.hasKeyword Keyword.Flash cheetahId humbled) "the Cheetah projects flash"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flash mammothId humbled)) "the Mammoth does not"
    Spec.assertBool s (S.castable S.alice cheetahId humbled) "and it is still castable on bob's turn"

-- The two names Wax // Wane prints (CR 709.4a). Neither of them is "Wax//Wane",
-- which is the combined view's stand-in and not a name the card has.
waxName, waneName :: CardName.CardName
waxName = CardName.MkCardName (Text.pack "Wax")
waneName = CardName.MkCardName (Text.pack "Wane")

-- The pool's first split card (CR 709.1), and so the first case here that
-- exercises CR 709.3-709.4 against a printed card rather than a fixture: Wax is
-- {G} "Target creature gets +2/+2 until end of turn", Wane is {W} "Destroy
-- target enchantment".
--
-- Every case names the half and calls Pawl.Engine.Cast directly. S.cast and
-- S.castable route through S.soleFaceName, which errors on a card with more
-- than one castable half precisely so a split card cannot silently exercise
-- nothing here.
waxWaneSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
waxWaneSpec s registry = Spec.describe s "WaxWane" $ do
  -- CR 709.3: "A player chooses which half of a split card they are casting
  -- before putting it onto the stack." Falsifier: an engine that cast the
  -- COMBINED view would have no single spell payload to resolve, and one that
  -- always cast the first face would pass this and fail the Wane case below.
  Spec.it s "CR 709.3 casting Wax gives the targeted creature +2/+2" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    waxWane <- S.printingOf s registry "Wax // Wane"
    let (pikerId, withPiker) = S.addCreature piker S.alice (S.landsInPlay forest 1)
        (gs, oid) = S.handOne waxWane withPiker
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid waxName))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "the 2/1 Piker is a 4/3" (S.powerToughnessOf pikerId resolved) (Just (4, 3))
  -- The other half, and the case that makes the one above discriminating: this
  -- board has no creature at all, so an engine that always cast the first face
  -- would find Wax no legal target here.
  Spec.it s "CR 709.3 casting Wane destroys the targeted enchantment" $ do
    plains <- S.printingOf s registry "Plains"
    ghostlyPrison <- S.printingOf s registry "Ghostly Prison"
    waxWane <- S.printingOf s registry "Wax // Wane"
    let (prisonId, withPrison) = S.addCreature ghostlyPrison S.alice (S.landsInPlay plains 1)
        (gs, oid) = S.handOne waxWane withPrison
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid waneName))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertBool s (S.onBattlefield prisonId gs) "the Prison starts on the battlefield"
    Spec.assertBool s (not (S.onBattlefield prisonId resolved)) "and Wane destroys it"
  -- CR 709.4: "In every zone except the stack, the characteristics of a split
  -- card are those of its two halves combined", and CR 709.4b: "The mana cost of
  -- a split card is the combined mana costs of its two halves. A split card's
  -- colors and mana value are determined from its combined mana cost." Read
  -- through the game state rather than off the card, so this is the combined
  -- view a resting object actually projects.
  Spec.it s "CR 709.4b a split card in a graveyard is green and white with mana value 2" $ do
    waxWane <- S.printingOf s registry "Wax // Wane"
    let (oid, gs) = S.addGraveyardCard waxWane S.alice (Setup.emptyGame S.bothPlayers)
    case Game.faceOf oid gs of
      Nothing -> Spec.assertFailure s "expected a card in the graveyard"
      Just face -> do
        Spec.assertEqWith s "both colours" (Projection.printedColorsOf face) (Set.fromList [Color.Green, Color.White])
        Spec.assertEqWith s "mana value 2" (Quantity.manaValueOf face) 2
  -- CR 709.3b: "While on the stack, only the characteristics of the half being
  -- cast exist. The other half's characteristics are treated as though they
  -- didn't exist." The same card in a hand is CR 709.4's combined view instead,
  -- which is the contrast that makes the stack reading a narrowing rather than
  -- the only answer the engine has.
  Spec.it s "CR 709.3b the Wax on the stack is named Wax, where the card in hand is not" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    waxWane <- S.printingOf s registry "Wax // Wane"
    let (_, withPiker) = S.addCreature piker S.alice (S.landsInPlay forest 1)
        (gs, oid) = S.handOne waxWane withPiker
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid waxName))
    -- CR 709.4a gives the card two names and no joined one, so the string a
    -- single CardName can carry here is a stand-in (#650) rather than a name
    -- the card has -- and it is emphatically not "Wax".
    Spec.assertEqWith s "in hand, the combined view" (Projection.nameOf oid gs) (CardName.MkCardName (Text.pack "Wax//Wane"))
    case GameState.stack cast of
      [] -> Spec.assertFailure s "expected the spell on the stack"
      top : _ -> Spec.assertEqWith s "on the stack, the half being cast" (Projection.nameOf top cast) waxName
  Spec.it s "CR 709.3a each half is offered and gated on its own" $ do
    waxWane <- S.printingOf s registry "Wax // Wane"
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    ghostlyPrison <- S.printingOf s registry "Ghostly Prison"
    -- Both halves need a legal target, or targeting gates them BOTH out and the
    -- offered list is empty for a reason that has nothing to do with mana. Wax
    -- wants a creature; Wane wants an enchantment.
    let targets g = snd (S.addCreature ghostlyPrison S.alice (snd (S.addCreature piker S.alice g)))
        namesOffered gs = [n | A.Cast _ n <- Action.legalActions S.alice gs]
        (green, _) = S.handOne waxWane (targets (S.landsInPlay forest 1))
        (both, _) = S.handOne waxWane (targets (snd (S.addCreature plains S.alice (S.landsInPlay forest 1))))
    -- CR 709.3a: "Only the chosen half is evaluated to see if it can be cast."
    -- One Forest pays Wax's {G} and cannot pay Wane's {W}. Falsifier: an engine
    -- pricing either half from CR 709.4b's combined {G}{W} would offer NEITHER.
    Spec.assertEqWith s "one Forest: only the affordable half" (namesOffered green) [waxName]
    -- The other direction, without which "each half is gated on its own" is
    -- indistinguishable from "the first face wins": with both halves payable,
    -- both are offered and CR 709.3's choice is left to the player.
    Spec.assertEqWith s "a Forest and a Plains: both halves" (namesOffered both) [waxName, waneName]

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Cast" $ do
  castSpec s registry
  castEngineSpec s registry
  stackSpec s registry
  discardSpec s registry
  sicknessSpec s registry
  magicalHackSpec s registry
  blazeSpec s registry
  corrosiveGaleSpec s registry
  waxWaneSpec s registry
  modalCastSpec s registry
  entwineSpec s registry
  auraTargetSpec s registry
  fireboltSpec s registry
  legendarySpellSpec s registry
  printedCastingRestrictionSpec s registry
  flashSpec s registry

-- Casts the first offered option, then declines (the loop re-offers until empty).
castFirstOption :: Prompt.Prompt r -> r
castFirstOption p = case p of
  Prompt.CastWhileSearching _ _ options -> case options of
    oid : _ -> Just oid
    [] -> Nothing
  Prompt.Shuffle ids -> ids
  Prompt.RandomFirstPlayer order -> NonEmpty.head order
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  Prompt.ChooseAction {} -> A.Pass
  Prompt.Concede _ -> Concession.Continues
  Prompt.ChooseDiscard _ _ ids n -> List.genericTake n ids
  Prompt.ChooseDefender _ _ candidates -> NonEmpty.head candidates
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

nameOnStack :: CardName.CardName -> GameState.GameState -> ObjectId.ObjectId -> Bool
nameOnStack wanted gs oid = case Game.lookupObject oid gs of
  Just o -> case Object.source o of
    Source.OfCard printing -> Face.name (S.faceOf printing) == wanted
    Source.OfToken card -> S.nameOf card == wanted
    Source.OfAbility _ _ -> False
    Source.OfTrigger _ _ -> False
    Source.OfEmblem _ -> False
    Source.OfInherentTrigger _ _ -> False
  Nothing -> False

castPanglacial :: Prompt.Prompt r -> r
castPanglacial p = case p of
  Prompt.CastWhileSearching _ _ options -> case options of
    oid : _ -> Just oid
    [] -> Nothing
  Prompt.SearchLibrary {} -> Nothing
  Prompt.Shuffle ids -> ids
  Prompt.RandomFirstPlayer order -> NonEmpty.head order
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  Prompt.ChooseAction {} -> A.Pass
  Prompt.Concede _ -> Concession.Continues
  Prompt.ChooseDiscard _ _ ids n -> List.genericTake n ids
  Prompt.ChooseDefender _ _ candidates -> NonEmpty.head candidates
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
