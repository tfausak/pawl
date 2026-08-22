{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Cast and Pawl.Engine.Stack: cast timing, the stack, discard, and
-- summoning sickness.
module Pawl.CastSpec where

import qualified Control.Monad as Monad
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
import qualified Pawl.Engine.Keyword as Keyword.Engine
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
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CastingPermission as CastingPermission
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DiscardCards as DiscardCards
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EntwineDecision as EntwineDecision
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KickerDecision as KickerDecision
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.SpellWasCast as SpellWasCast
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
    Spec.assertBool s (elem (A.Cast oid (S.printingName piker) Facing.FaceUp) (Action.legalActions S.alice gs)) "offered"
  Spec.it s "an unaffordable Piker is not offered" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, oid) = S.pikerInHand mountain piker 1 Phase.PrecombatMain
    Spec.assertBool s (not (any (S.isCastOf oid) (Action.legalActions S.alice gs))) "not offered"
  -- The mechanism, against Pawl.CardSpec's hand-built fixture: Wax a green
  -- instant costing {G}, Wane a white sorcery costing {W}, and each half
  -- carrying inert text that only Pawl.CardSpec's CR 709.4 merge test reads.
  -- waxWaneSpec below asserts the same rules against the printed Wax // Wane,
  -- which is what proves the card data; this pair says the gate is the layout's
  -- and does not depend on what those halves happen to do.
  Spec.it s "CR 709.3a both halves are offered, each priced from its own half" $ do
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    let waxWane = Printing.MkPrinting CardSpec.splitCard
        namesOffered gs = [n | A.Cast _ n _ <- Action.legalActions S.alice gs]
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
        after = S.runPure S.identityAnswer gs (Cast.castSpell S.alice oid wax Facing.FaceUp)
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
      (any (\pl -> Player.life pl < Setup.startingLife Nothing) (Map.elems (GameState.players gs)))
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
            Source.OfCard printingId ->
              Spec.assertBool s (maybe False (Card.isCreature . Card.combined) (Game.cardOfPrinting printingId after)) "creature"
            Source.OfToken _ -> Spec.assertFailure s "expected a card source"
            Source.OfAbility _ -> Spec.assertFailure s "expected a card source"
            Source.OfTrigger _ -> Spec.assertFailure s "expected a card source"
            Source.OfEmblem _ -> Spec.assertFailure s "expected a card source"
            Source.OfInherentTrigger _ -> Spec.assertFailure s "expected a card source"
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
            after = snd (Engine.runGamePure castFirstOption g4 action)
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
            Source.OfCard printingId ->
              Spec.assertEqWith s "name" (fmap S.nameOf (Game.cardOfPrinting printingId after)) (Just (CardName.MkCardName $ Text.pack "Goblin Piker"))
            Source.OfToken _ -> Spec.assertFailure s "expected a card source"
            Source.OfAbility _ -> Spec.assertFailure s "expected a card source"
            Source.OfTrigger _ -> Spec.assertFailure s "expected a card source"
            Source.OfEmblem _ -> Spec.assertFailure s "expected a card source"
            Source.OfInherentTrigger _ -> Spec.assertFailure s "expected a card source"
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
          -- The `you` entry alongside it is CR 109.5's, stamped for every spell
          -- rather than chosen -- so these two are the whole of the recipients the
          -- cast bound, and nothing was asked for to get the second.
          Spec.assertEqWith
            s
            "the Piker is the target, and alice is CR 109.5's you"
            (Binding.targetsOf (Object.bindings obj))
            ( Map.fromList
                [ (SlotName.MkSlotName (Text.pack "target"), Set.singleton (Recipient.ToCreature (S.pikerOf base))),
                  (Binding.you, Set.singleton (Recipient.ToPlayer S.alice))
                ]
            )
  Spec.it s "casting a {X}{R} spell at X=3 stamps amount 3 and pays {3}{R}" $ do
    blaze <- S.printingOf s registry "Blaze"
    mountain <- S.printingOf s registry "Mountain"
    let (gs0, oid) = S.handOne blaze (S.landsInPlay mountain 4)
        after = snd (Engine.runGamePure (answerXOf 3) gs0 (S.cast S.alice oid))
    case GameState.stack after of
      [] -> Spec.assertFailure s "expected the spell on the stack"
      top : _ -> case Game.lookupObject top after of
        Nothing -> Spec.assertFailure s "stack id should resolve"
        Just obj -> do
          Spec.assertEqWith s "amount bound" (Binding.amountOf Binding.variableX (Object.bindings obj)) (Just 3)
          Spec.assertEqWith s "four mana spent (paid {3}{R})" (S.tappedCount S.alice after) 4
  -- CR 601.2g: a legal answer that cannot pay. Birds of Paradise offers all five
  -- colours, so answering green against Lightning Bolt's {R} is a choice the
  -- engine must honour (Cost.payMana argues why) and then cannot pay with. This
  -- is the reachable mid-announcement failure castSpell's haddock calls
  -- deliberate, and the class #418 did NOT remove -- the player chose it.
  --
  -- What is asserted is the REWIND: CR 601.2's own remedy returns the game to the
  -- state before CR 601.2a moved the card, so the Bolt is in hand and the Birds
  -- is untapped again. The tap is the sharp half -- it proves the cost payment
  -- was undone rather than merely abandoned. The prompts already issued are not
  -- recalled (#741); the game state is.
  Spec.it s "CR 601.2 a mis-coloured mana answer unwinds the whole cast" $ do
    birds <- S.printingOf s registry "Birds of Paradise"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (_, withBirds) = S.addCreature birds S.alice (Setup.emptyGame S.bothPlayers)
        (oid, gs0) = S.addHandCard lightningBolt S.alice withBirds
        gs = gs0 {GameState.phase = Phase.PrecombatMain}
        -- Green whenever the colour choice is offered; everything else default.
        picksGreen :: Prompt.Prompt r -> r
        picksGreen p = case p of
          Prompt.ChooseManaYield _ _ _ candidates ->
            S.optionYielding (Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Green, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing}]) candidates
          _ -> S.identityAnswer p
        after = snd (Engine.runGamePure picksGreen gs (S.cast S.alice oid))
    Spec.assertEqWith s "nothing on the stack" (length (GameState.stack after)) 0
    Spec.assertEqWith s "the Bolt is back in alice's hand" (length (Game.zoneMembers Zone.Hand S.alice after)) 1
    Spec.assertEqWith s "and the Birds is untapped again" (S.tappedCount S.alice after) 0

  -- The discriminating sibling of the test above: same board, same prompts, one
  -- colour different. Without it the no-op assertions would pass for a board
  -- where the cast never reached CR 601.2g at all -- an untargetable Bolt or a
  -- Birds that could not be tapped would satisfy every one of them.
  Spec.it s "CR 601.2 the same cast with the right colour succeeds" $ do
    birds <- S.printingOf s registry "Birds of Paradise"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (_, withBirds) = S.addCreature birds S.alice (Setup.emptyGame S.bothPlayers)
        (oid, gs0) = S.addHandCard lightningBolt S.alice withBirds
        gs = gs0 {GameState.phase = Phase.PrecombatMain}
        picksRed :: Prompt.Prompt r -> r
        picksRed p = case p of
          Prompt.ChooseManaYield _ _ _ candidates ->
            S.optionYielding (Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Red, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing}]) candidates
          _ -> S.identityAnswer p
        after = snd (Engine.runGamePure picksRed gs (S.cast S.alice oid))
    Spec.assertEqWith s "the Bolt is on the stack" (length (GameState.stack after)) 1
    Spec.assertEqWith s "alice's hand is empty" (length (Game.zoneMembers Zone.Hand S.alice after)) 0
    Spec.assertEqWith s "and the Birds paid, so it is tapped" (S.tappedCount S.alice after) 1

  Spec.it s "an illegal target answer makes the cast a no-op" $ do
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs, oid) = S.boltInHand mountain lightningBolt 1 Phase.PrecombatMain
        liar :: Prompt.Prompt r -> r
        liar p = case p of
          Prompt.ChooseTargets _ _ _ sets ->
            fmap (const (Set.singleton (Recipient.ToCreature (ObjectId.MkObjectId 999)))) sets
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
  -- CR 709.3 ("A player chooses which half of a split card they are casting
  -- before putting it onto the stack") does not stop at the library door, and CR
  -- 601.3 grants a permission to CAST rather than a narrower one: a split card
  -- printing the Panglacial permission on each half offers TWO options during a
  -- search, and picking between them is the player's choice.
  Spec.it s "CR 709.3 a split card offers BOTH halves during a search" $ do
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    split <- S.printingOf s registry "Synthetic Glacial Half"
    let (_, withMountain) = S.addCreature mountain S.alice (S.landsInPlay forest 1)
        (_, gs) = S.addLibraryCard split S.alice withMountain
    Spec.assertEqWith s "both halves offered, by name" (fmap snd (Cast.castableWhileSearching S.alice gs)) [glacialHalf, volcanicHalf]
  -- The control, one Mountain apart from the pair above: CR 709.3a's "Only the
  -- chosen half is evaluated to see if it can be cast" gates each half on its
  -- OWN cost, so a lone Forest reaches {G} and not {R}. Without this, "both
  -- halves offered" is indistinguishable from "the search offers whatever it
  -- finds".
  Spec.it s "CR 709.3a a lone Forest reaches only the {G} half" $ do
    forest <- S.printingOf s registry "Forest"
    split <- S.printingOf s registry "Synthetic Glacial Half"
    let (_, gs) = S.addLibraryCard split S.alice (S.landsInPlay forest 1)
    Spec.assertEqWith s "only the affordable half" (fmap snd (Cast.castableWhileSearching S.alice gs)) [glacialHalf]
  -- WHICH half the player picked is what reaches the stack, not the first one
  -- the engine happened to enumerate: the answerer takes the LAST option, and
  -- the Volcanic half is what lands there. CR 709.3b -- "While on the stack,
  -- only the characteristics of the half being cast exist" -- is what makes the
  -- 2/2 an assertion and not a restatement of the name.
  --
  -- Gameplay-level: Evolving Wilds is activated and its ability resolves, and
  -- the cast happens inside that resolution (CR 601.3), where no player has
  -- priority.
  Spec.it s "CR 709.3b the half the player chose is the half on the stack" $ do
    evolvingWilds <- S.printingOf s registry "Evolving Wilds"
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    split <- S.printingOf s registry "Synthetic Glacial Half"
    let g0 = Setup.emptyGame S.bothPlayers
        (ewId, g1) = S.addCreature evolvingWilds S.alice g0
        (_, g2) = S.addCreature mountain S.alice (snd (S.addCreature forest S.alice g1))
        (_, g3) = S.addLibraryCard split S.alice g2
        g4 = g3 {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
    case Projection.abilitiesOf ewId g4 of
      ewAbility : _ ->
        let action = do
              Activate.activateAbility S.alice ewId ewAbility
              Stack.resolveTop -- the search, with the cast made inside it
            after = snd (Engine.runGamePure castLastOption g4 action)
         in do
              Spec.assertEqWith s "the card left the library" (S.countByName glacialHalf S.alice after) 0
              -- No assertion on WHICH land paid: castLastOption answers
              -- ChooseManaSource with the head of the candidates, so it taps the
              -- Forest for a {G} that {R} cannot use before reaching the
              -- Mountain. That is the answerer being naive, not the cast.
              case GameState.stack after of
                [] -> Spec.assertFailure s "expected the chosen half on the stack"
                top : _ -> do
                  Spec.assertEqWith s "the Volcanic half, not the Glacial one" (Projection.namesOf top after) (Set.singleton volcanicHalf)
                  Spec.assertEqWith s "and CR 709.3b's 2/2, not the Glacial half's 1/1" (Projection.powerOf top after) (Just 2)
      [] -> Spec.assertFailure s "Evolving Wilds should have an activated ability"
  -- CR 601.3's subject is "a spell or ability". The offer used to be made from
  -- Stack's Source.OfAbility arm alone, so a searching SPELL never got it; it now
  -- lives in the Search effect itself, which both paths reach (#57).
  Spec.it s "CR 601.3 a searching SPELL offers the cast too" $ do
    forest <- S.printingOf s registry "Forest"
    rampantGrowth <- S.printingOf s registry "Rampant Growth"
    panglacialWurm <- S.printingOf s registry "Panglacial Wurm"
    let base = S.landsInPlay forest 9
        (_, withWurm) = S.addLibraryCard panglacialWurm S.alice base
        (_, withLand) = S.addLibraryCard forest S.alice withWurm
        (growthId, gs) = S.addHandCard rampantGrowth S.alice withLand
        after = S.runPure castFirstOption gs (S.cast S.alice growthId >> Stack.resolveTop)
    Spec.assertEqWith s "Panglacial left the library, so the search offered it" (S.countByName (CardName.MkCardName $ Text.pack "Panglacial Wurm") S.alice after) 0

  Spec.it s "CR 601.2i casting a spell records a SpellCast event for the caster" $ do
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs, oid) = S.boltInHand mountain lightningBolt 1 Phase.PrecombatMain
        after = S.runPure S.identityAnswer gs (S.cast S.alice oid)
        casts = fmap SpellWasCast.player (Maybe.mapMaybe Game.castOf (S.eventsOf after))
    Spec.assertEqWith s "no cast before" (Maybe.mapMaybe Game.castOf (S.eventsOf gs)) []
    Spec.assertEqWith s "exactly one cast, by alice" casts [S.alice]
  Spec.it s "CR 601.2i a cast that is rejected records nothing" $ do
    -- A Bolt with no mana available: legalActions would never offer it, and
    -- castSpell's payment fails, so no event is recorded.
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (gs, oid) = S.boltInHand mountain lightningBolt 0 Phase.PrecombatMain
        after = S.runPure S.identityAnswer gs (S.cast S.alice oid)
    Spec.assertEqWith s "no cast recorded" (Maybe.mapMaybe Game.castOf (S.eventsOf after)) []

-- Chooses this value of X and aims every target slot at bob; other prompts take
-- the identity fallback. Casing on a GADT prompt with an identityAnswer default
-- is the liar pattern from the illegal-target test.
answerXOf :: Natural -> Prompt.Prompt r -> r
answerXOf n p = case p of
  Prompt.ChooseX {} -> n
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
  _ -> S.identityAnswer p

-- Discards from the BACK of hand. Deliberately unlike every fallback, so the
-- CR 514.2 test proves the prompted choice is actually honored.
discardLastAnswer :: Prompt.Prompt r -> r
discardLastAnswer p = case p of
  Prompt.ChooseDiscard _ _ ids n -> lastN n ids
  _ -> S.identityAnswer p

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
  let (printingId, boardP) = Game.intern printing board
      (oid, g1) = Game.freshObjectId boardP
      (ts, g2) = Game.freshTimestamp g1
      obj =
        Object.MkObject
          { Object.owner = S.alice,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfCard printingId,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Settled S.alice,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.classLevel = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.phyrexianLifePaid = 0,
            Object.manaSpent = Mana.MkMana [],
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.goadedBy = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
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

-- Answers Prompt.ChooseX with the affordability bound the prompt carries, and
-- records that bound in the State. The log is how a test sees a payload nothing
-- on the board records; answering WITH it is what proves the bound is payable
-- rather than merely reported. Aims every target slot at bob, and takes the
-- identity fallback elsewhere (the liar pattern answerXOf uses).
answerAtBound :: Prompt.Prompt r -> State.State [Natural] r
answerAtBound p = case p of
  Prompt.ChooseX _ _ _ bound -> do
    State.modify' (\seen -> seen <> [bound])
    pure bound
  Prompt.ChooseTargets _ _ _ sets -> pure (fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets)
  _ -> pure (S.identityAnswer p)

-- Announces ONE MORE than the bound -- legal under CR 601.2b and unaffordable by
-- construction, whatever the board is.
answerAboveBound :: Prompt.Prompt r -> r
answerAboveBound p = case p of
  Prompt.ChooseX _ _ _ bound -> bound + 1
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
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
    pure (fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets)
  _ -> pure (S.identityAnswer p)

-- Records the object each CR 601.2c target question is asked ABOUT, and aims
-- every slot at bob. The recorded id is what proves CR 601.2a ran first: it is
-- the spell's stack incarnation (CR 400.7), never the card that was in hand.
answerRecordingTargetObject :: Prompt.Prompt r -> State.State [ObjectId.ObjectId] r
answerRecordingTargetObject p = case p of
  Prompt.ChooseTargets _ _ oid sets -> do
    State.modify' (\seen -> seen <> [oid])
    pure (fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets)
  _ -> pure (S.identityAnswer p)

-- How many cards of this name sit in alice's hand (the reject-not-repair no-op
-- check: a cast that reverses leaves the card exactly where it was).
inHandNamed :: String -> GameState.GameState -> Int
inHandNamed name gs = length (filter (nameOnStack (CardName.MkCardName $ Text.pack name) gs) (Game.zoneMembers Zone.Hand S.alice gs))

-- Aims every CR 601.2c target slot at BOB THE PLAYER, and takes the identity
-- fallback elsewhere.
--
-- Bob and not alice, and the player and not their creature, for the same reason:
-- Char's two damage instructions name different recipients, and every wrong
-- wiring that collapses them onto one recipient has to be distinguishable. A
-- test that aimed the target slot at alice would see 20 -> 14 whether CR 109.5's
-- `you` bound anything or not.
answerTargetingBob :: Prompt.Prompt r -> r
answerTargetingBob p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
  _ -> S.identityAnswer p

charSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
charSpec s registry = Spec.describe s "Char" $ do
  -- CR 109.5 on a SPELL: "The words 'you' and 'your' on an object refer to the
  -- object's controller". Char -- "Char deals 4 damage to any target and 2 damage
  -- to you" -- is the shape that cannot be answered without a binding: CR 115.4's
  -- "any target" is an ordinary chosen slot, but the second instruction names a
  -- player nothing chose, and pawl's damage opcode reaches a player only through a
  -- bound recipient. So the cast has to stamp the caster under the `you` slot, as
  -- CR 109.5's activated- and triggered-ability sentences already make
  -- Pawl.Engine.Activate.activateAbility and Pawl.Engine.Engine's trigger
  -- placement do.
  --
  -- CR 608.2c has the controller follow the instructions "in the order written",
  -- and these two are separate instructions naming different recipients rather
  -- than one naming both -- which is what the untouched Piker below pins. Their
  -- ORDER is unobservable here: neither instruction can change what the other
  -- does, so this case makes no claim about it.
  Spec.it s "CR 109.5/120.3a Char deals 4 to bob and 2 to its caster" $ do
    char <- S.printingOf s registry "Char"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (pikerId, board) = S.addCreature piker S.bob (S.landsInPlay mountain 3)
        (gs0, oid) = S.handOne char board
        after = snd (Engine.runGamePure answerTargetingBob gs0 (do S.cast S.alice oid; Stack.resolveTop))
    -- CR 120.3a: damage dealt to a player makes them lose that much life.
    Spec.assertEqWith s "bob took the 4 aimed at him" (S.lifeOf S.bob after) (Just 16)
    -- The whole point of the unit. Without the stamp the second instruction's
    -- recipient lookup misses and the instruction is a silent no-op, leaving alice
    -- at 20 -- a Char strictly better for its controller than the printed card.
    Spec.assertEqWith s "alice took the 2 Char deals to its controller" (S.lifeOf S.alice after) (Just 18)
    -- Neither instruction went anywhere near bob's creature. 4 damage would have
    -- killed a 2/1 outright, so a battlefield that still holds it is the check
    -- that the target slot was answered with the PLAYER, and 0 marked damage is
    -- the check that the `you` instruction did not spill onto it.
    Spec.assertEqWith s "bob's Piker took no damage" (S.damageOf pikerId after) (Just 0)
    Spec.assertEqWith s "bob's Piker is still on the battlefield" (S.onBattlefield pikerId after) True
    Spec.assertEqWith s "Char resolved out of hand" (inHandNamed "Char" after) 0
    Spec.assertEqWith s "three Mountains paid {2}{R}" (S.tappedCount S.alice after) 3

blazeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
blazeSpec s registry = Spec.describe s "Blaze" $ do
  Spec.it s "Blaze at X=3 deals 3 to the opponent (CR 601.2b/f/h, 608.2)" $ do
    -- Falsifier: an engine that ignored the chosen value (treated X as 0, or
    -- as the {X} mana value) would leave bob at 20.
    blaze <- S.printingOf s registry "Blaze"
    mountain <- S.printingOf s registry "Mountain"
    let (gs0, oid) = S.handOne blaze (S.landsInPlay mountain 4)
        after = snd (Engine.runGamePure (answerXOf 3) gs0 (do S.cast S.alice oid; Stack.resolveTop))
    Spec.assertEqWith s "Bob at 17" (S.lifeOf S.bob after) (Just 17)
    Spec.assertEqWith s "four Mountains paid {3}{R}" (S.tappedCount S.alice after) 4
  Spec.it s "Blaze at X=0 is castable and deals nothing (the X=0 floor)" $ do
    -- Falsifier: a floor that required {X} > 0 would make Blaze uncastable off
    -- one Mountain, leaving it in hand.
    blaze <- S.printingOf s registry "Blaze"
    mountain <- S.printingOf s registry "Mountain"
    let (gs0, oid) = S.handOne blaze (S.landsInPlay mountain 1)
        after = snd (Engine.runGamePure (answerXOf 0) gs0 (do S.cast S.alice oid; Stack.resolveTop))
    Spec.assertEqWith s "Bob unharmed" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "one Mountain paid {R}" (S.tappedCount S.alice after) 1
    Spec.assertEqWith s "Blaze resolved out of hand" (inHandNamed "Blaze" after) 0
  Spec.it s "Blaze at an unaffordable X is a no-op (reject-not-repair)" $ do
    blaze <- S.printingOf s registry "Blaze"
    mountain <- S.printingOf s registry "Mountain"
    let (gs0, oid) = S.handOne blaze (S.landsInPlay mountain 1)
        after = snd (Engine.runGamePure (answerXOf 3) gs0 (S.cast S.alice oid))
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
  -- carrying it through to the announced X is what leaves Mana.announce
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

-- Vitalizing Cascade ({X}{G}{W} Instant, "You gain X plus 3 life"), the pool's
-- first card whose X is not a bare X: the life-gain's quantity is
-- Plus X (Literal 3), so CR 601.2b's announced value is one summand of a
-- compound quantity rather than the whole of it.
--
-- THE ARITHMETIC, and it is why X=2 and X=1 rather than any other pair. Off five
-- lands 20 life becomes 25 and 24, and no other reading of the printed quantity
-- lands on either: "X plus X" gives 24 and 22, the literal 3 alone gives 23 and
-- 23, and X alone gives 22 and 21. Two values pin both the slope and the
-- intercept, which one cannot -- and X=3 would have been the vacuous choice,
-- since "X plus 3", "X plus X" and "3 plus 3" all reach 26 there.
--
-- Five lands, not the four the announcement costs: CR 601.2b's bound off this
-- board is X=3, so the life total is the value alice NAMED and not the largest
-- one she could have paid for.
vitalizingCascadeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
vitalizingCascadeSpec s registry = Spec.describe s "VitalizingCascade" $ do
  Spec.it s "CR 119.3 gains X plus 3 life, the announced X inside a sum (CR 107.3a)" $ do
    cascade <- S.printingOf s registry "Vitalizing Cascade"
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    let board = snd (S.addCreature plains S.alice (snd (S.addCreature plains S.alice (S.landsInPlay forest 3))))
        (gs0, oid) = S.handOne cascade board
        gainedAt x = snd (Engine.runGamePure (answerXOf x) gs0 (do S.cast S.alice oid; Stack.resolveTop))
        atTwo = gainedAt 2
    Spec.assertEqWith s "alice at 25, so X=2 was read as one summand of X+3" (S.lifeOf S.alice atTwo) (Just 25)
    Spec.assertEqWith s "and at 24 for X=1, which fixes the summand as X and not another 3" (S.lifeOf S.alice (gainedAt 1)) (Just 24)
    Spec.assertEqWith s "bob untouched, the gain being the controller's own" (S.lifeOf S.bob atTwo) (Just 20)
    Spec.assertEqWith s "four of the five lands paid {2}{G}{W}" (S.tappedCount S.alice atTwo) 4
    Spec.assertEqWith s "and it resolved out of hand" (inHandNamed "Vitalizing Cascade" atTwo) 0

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
-- The board: alice has `lands` untapped `land`s, bob has a Goblin Piker
-- (untapped) and a Wall of Stone (TAPPED), and `modal` is in alice's hand.
-- The two victims start in OPPOSITE tap states on purpose -- an entwined cast
-- that tapped the Piker and untapped the Wall leaves a board that neither mode
-- alone can produce, and that a cast which fused the two slots could not produce
-- either.
--
-- `land` and `modal` are parameters rather than Island and Dream's Grip because
-- Synthetic Twofold Braid prints Dream's Grip's two modes over Forests, so the
-- two-entwine-ability group below reads its board off this one.
entwineBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Int ->
  (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
entwineBoard land modal piker wallOfStone lands =
  let (pikerId, gs1) = S.addCreature piker S.bob (S.landsInPlay land lands)
      (wallId, gs2) = S.addCreature wallOfStone S.bob gs1
      (gs, spellId) = S.handOne modal (S.tapObject wallId gs2)
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
    Map.mapWithKey (\slot _ -> Set.singleton (Recipient.ToObject (if slot == tapSlot then toTap else toUntap))) sets
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
      (Cast.entwineOffer ManaSpending.AsProduced S.alice richSpell (Cost.costsFor (S.printingName dreamsGrip) richSpell rich) rich)
      (Just (Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]), Cost.Type.components = []}))
    Spec.assertEqWith s "one Island: unaffordable, so not offered" (Cast.entwineOffer ManaSpending.AsProduced S.alice poorSpell (Cost.costsFor (S.printingName dreamsGrip) poorSpell poor) poor) Nothing
  -- CR 702.42 states no limit on how many entwine abilities an object has --
  -- contrast CR 702.41b for affinity and CR 702.43b for modular, which each say
  -- what multiple instances do -- and CR 118.8a's "any number of additional
  -- costs may be applied to a spell as it's being cast" makes two of them a SUM,
  -- not a choice. No printing has two (Scryfall keyword:entwine, 2026-08-21:
  -- every hit prints a single entwine ability), so Synthetic Twofold Braid
  -- (data/cards/synthetic-twofold-braid.json) is Dream's Grip's two modes for
  -- {G}, printing "Entwine {2}" AND "Entwine {1}{G}".
  --
  -- Entwining therefore costs {G} plus {1}{G} plus {2}: five mana, two green.
  -- Five Forests pay it exactly, so the tapped count is what tells the sum from
  -- either cost alone -- {G} plus one of them is three mana, and a board of five
  -- would leave two Forests standing.
  Spec.it s "CR 118.8a two entwine abilities sum: entwining the Braid taps all five Forests" $ do
    forest <- S.printingOf s registry "Forest"
    braid <- S.printingOf s registry "Synthetic Twofold Braid"
    piker <- S.printingOf s registry "Goblin Piker"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    let (gs, spellId, pikerId, wallId) = entwineBoard forest braid piker wallOfStone 5
        (asked, after) = castAndResolve (grips EntwineDecision.Entwines pikerId wallId) gs spellId
    Spec.assertEqWith s "{G} plus the entwine {1}{G} plus the entwine {2}: all five Forests are tapped" (S.tappedCount S.alice after) 5
    Spec.assertEqWith s "mode 0 tapped the Piker" (tapStateOf pikerId after) (Just TapState.Tapped)
    Spec.assertEqWith s "mode 1 untapped the Wall" (tapStateOf wallId after) (Just TapState.Untapped)
    Spec.assertEqWith s "the player was asked once, at the combined price, and entwined" (entwineAnnouncements asked) [EntwineDecision.Entwines]
  -- The negative, one Forest away from the board above and identical otherwise:
  -- four mana pays {G} plus EITHER entwine cost but not both, so under CR 118.8a
  -- entwining is not on offer. An engine that read one of the two costs would
  -- offer it here, which is what makes this the discriminating case.
  Spec.it s "CR 118.8a with four Forests neither entwine cost alone is enough, so nothing is offered" $ do
    forest <- S.printingOf s registry "Forest"
    braid <- S.printingOf s registry "Synthetic Twofold Braid"
    piker <- S.printingOf s registry "Goblin Piker"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    let (gs, spellId, pikerId, wallId) = entwineBoard forest braid piker wallOfStone 4
        -- An interpreter that WOULD entwine, entwineSpec's shape above: it
        -- never gets the chance.
        (asked, after) = castAndResolve (grips EntwineDecision.Entwines pikerId wallId) gs spellId
    Spec.assertEqWith s "mode 1 never ran, so the Wall is still tapped" (tapStateOf wallId after) (Just TapState.Tapped)
    Spec.assertEqWith s "mode 0 tapped the Piker, so the ordinary cast did happen" (tapStateOf pikerId after) (Just TapState.Tapped)
    Spec.assertEqWith s "only {G} was paid: three Forests are still untapped" (S.tappedCount S.alice after) 1
    Spec.assertBool s (S.castable S.alice spellId gs) "the spell is still castable"
    Spec.assertEqWith s "no entwine question was put" (entwineAnnouncements asked) []
  -- The gate itself, so the summed cost is pinned rather than inferred from the
  -- mana it consumed. The mana parts CONCATENATE in ascending Set order
  -- (Cost.plus), which puts {1}{G} before {2}.
  Spec.it s "CR 601.2f Cast.entwineOffer is the two entwine costs summed" $ do
    forest <- S.printingOf s registry "Forest"
    braid <- S.printingOf s registry "Synthetic Twofold Braid"
    piker <- S.printingOf s registry "Goblin Piker"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    let (gs, spellId, _, _) = entwineBoard forest braid piker wallOfStone 5
    Spec.assertEqWith
      s
      "five Forests: the additional cost is {1}{G} plus {2}"
      (Cast.entwineOffer ManaSpending.AsProduced S.alice spellId (Cost.costsFor (S.printingName braid) spellId gs) gs)
      ( Just
          ( Cost.Type.MkCost
              { Cost.Type.mana =
                  Just
                    ( ManaCost.MkManaCost
                        [ ManaSymbol.Generic 1,
                          ManaSymbol.OfType (ManaType.Colored Color.Green),
                          ManaSymbol.Generic 2
                        ]
                    ),
                Cost.Type.components = []
              }
          )
      )
  -- A card with no entwine is never asked, which is the other half of "where
  -- the rules leave nothing to ask, don't prompt".
  Spec.it s "CR 702.42a a modal spell without entwine (Chaos Charm) is never offered one" $ do
    mountain <- S.printingOf s registry "Mountain"
    chaosCharm <- S.printingOf s registry "Chaos Charm"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs0, spellId) = S.handOne chaosCharm (S.landsInPlay mountain 3)
        (_, gs) = S.addCreature piker S.bob gs0
    Spec.assertEqWith s "no entwine cost to offer" (Cast.entwineOffer ManaSpending.AsProduced S.alice spellId (Cost.costsFor (S.printingName chaosCharm) spellId gs) gs) Nothing

-- Burst Lightning's one mode is "Burst Lightning deals 2 damage to any target",
-- slot "target" (CR 702.33 / data/cards/burst-lightning.json), plus "Kicker {4}"
-- and the CR 702.33e ability that reads it -- "if this spell was kicked, it deals
-- 4 damage instead", written as two clauses conditioned on Quantity.WasKicked.
--
-- The board: alice has `mountains` untapped Mountains, bob has a Hill Giant, and
-- Burst Lightning is in alice's hand. THREE toughness is what makes the kicked and
-- unkicked readings distinct: 2 damage marks the Giant and leaves it alive, 4
-- destroys it (CR 704.5g), so no single board state answers for both.
kickerBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Int ->
  (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
kickerBoard mountain burstLightning hillGiant mountains =
  let (giantId, gs1) = S.addCreature hillGiant S.bob (S.landsInPlay mountain mountains)
      (gs, spellId) = S.handOne burstLightning gs1
   in (gs, spellId, giantId)

-- Answers CR 702.33a's kicker question with `decision` and aims the one target
-- slot at `victim` -- PINNED to that id rather than searched for, so a mutation
-- cannot be repaired by an answerer that finds another legal target. Everything
-- else defers to S.identityAnswer.
bursts ::
  KickerDecision.KickerDecision ->
  ObjectId.ObjectId ->
  Prompt.Prompt r ->
  r
bursts decision victim p = case p of
  Prompt.ChooseKicker {} -> decision
  Prompt.ChooseTargets _ _ _ sets -> Map.map (const (Set.singleton (Recipient.ToCreature victim))) sets
  _ -> S.identityAnswer p

-- Was CR 702.33a's question actually put to the player, and what did they say?
kickerAnnouncements :: [Response.Response] -> [KickerDecision.KickerDecision]
kickerAnnouncements responses = [d | Response.AnnouncedKicker d <- responses]

-- CR 702.33: kicker, the first OPTIONAL ADDITIONAL COST whose payoff is read back
-- during resolution rather than settled while the spell is cast.
kickerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
kickerSpec s registry = Spec.describe s "Kicker" $ do
  -- CR 702.33a's "you MAY pay": declining is a real answer, and the spell then
  -- deals its printed 2.
  Spec.it s "CR 702.33a declining the kicker deals 2: the Hill Giant survives, and one Mountain is tapped" $ do
    mountain <- S.printingOf s registry "Mountain"
    burstLightning <- S.printingOf s registry "Burst Lightning"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (gs, spellId, giantId) = kickerBoard mountain burstLightning hillGiant 5
        (asked, after) = castAndResolve (bursts KickerDecision.Declines giantId) gs spellId
        settled = S.settleSba after
    Spec.assertEqWith s "the player was asked, and declined" (kickerAnnouncements asked) [KickerDecision.Declines]
    Spec.assertEqWith s "2 damage is marked on the Giant" (S.damageOf giantId settled) (Just 2)
    Spec.assertEqWith s "which is not lethal, so it is still on the battlefield" (S.countOnBattlefieldByName (S.printingName hillGiant) S.bob settled) 1
    Spec.assertEqWith s "only {R} was paid, so one Mountain is tapped" (S.tappedCount S.alice settled) 1
  -- CR 702.33d's designation, read back by the card's own CR 702.33e ability: the
  -- SAME board and the SAME answerer but for the one answer, and the spell deals 4.
  Spec.it s "CR 702.33d paying the kicker deals 4 instead: the Hill Giant is destroyed, and five Mountains are tapped" $ do
    mountain <- S.printingOf s registry "Mountain"
    burstLightning <- S.printingOf s registry "Burst Lightning"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (gs, spellId, giantId) = kickerBoard mountain burstLightning hillGiant 5
        (asked, after) = castAndResolve (bursts KickerDecision.Kicks giantId) gs spellId
        settled = S.settleSba after
    Spec.assertEqWith s "the player was asked, and kicked" (kickerAnnouncements asked) [KickerDecision.Kicks]
    Spec.assertEqWith s "4 damage is lethal, so CR 704.5g destroyed the Giant" (S.countOnBattlefieldByName (S.printingName hillGiant) S.bob settled) 0
    Spec.assertEqWith s "and it is gone, so nothing carries its damage" (S.damageOf giantId settled) Nothing
    Spec.assertEqWith s "{R} plus the kicker {4}: all five Mountains are tapped" (S.tappedCount S.alice settled) 5
  -- CR 601.2b/601.2f-h: the additional cost is a real cost. With four Mountains
  -- there is {R} and three more, one short of the kicker, so kicking is not on
  -- offer at all -- and the ordinary cast still is.
  Spec.it s "CR 702.33a with four Mountains the kicker is not offered, and the ordinary cast still is" $ do
    mountain <- S.printingOf s registry "Mountain"
    burstLightning <- S.printingOf s registry "Burst Lightning"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (gs, spellId, giantId) = kickerBoard mountain burstLightning hillGiant 4
        -- An interpreter that WOULD kick: it never gets the chance, which is what
        -- makes this discriminating rather than a restatement of the answerer.
        (asked, after) = castAndResolve (bursts KickerDecision.Kicks giantId) gs spellId
        settled = S.settleSba after
    Spec.assertBool s (S.castable S.alice spellId gs) "the spell is still castable"
    Spec.assertEqWith s "no kicker question was put" (kickerAnnouncements asked) []
    Spec.assertEqWith s "so it dealt its printed 2" (S.damageOf giantId settled) (Just 2)
    Spec.assertEqWith s "and only {R} was paid" (S.tappedCount S.alice settled) 1
  -- The gate itself, asked directly, so the two arms of "is kicking available" are
  -- pinned apart from the cast that consumes them.
  Spec.it s "CR 702.33a Cast.kickerOffer is the {4} with five Mountains and Nothing with four" $ do
    mountain <- S.printingOf s registry "Mountain"
    burstLightning <- S.printingOf s registry "Burst Lightning"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (rich, richSpell, _) = kickerBoard mountain burstLightning hillGiant 5
        (poor, poorSpell, _) = kickerBoard mountain burstLightning hillGiant 4
    Spec.assertEqWith
      s
      "five Mountains: the additional cost is {4}"
      (Cast.kickerOffer ManaSpending.AsProduced S.alice richSpell (Cost.costsFor (S.printingName burstLightning) richSpell rich) rich)
      (Just (Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]), Cost.Type.components = []}))
    Spec.assertEqWith s "four Mountains: unaffordable, so not offered" (Cast.kickerOffer ManaSpending.AsProduced S.alice poorSpell (Cost.costsFor (S.printingName burstLightning) poorSpell poor) poor) Nothing
  -- A card with no kicker is never asked, which is the other half of "where the
  -- rules leave nothing to ask, don't prompt".
  Spec.it s "CR 702.33a a spell without kicker (Lightning Bolt) is never offered one" $ do
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (gs, spellId, _) = kickerBoard mountain lightningBolt hillGiant 5
    Spec.assertEqWith s "no kicker cost to offer" (Cast.kickerOffer ManaSpending.AsProduced S.alice spellId (Cost.costsFor (S.printingName lightningBolt) spellId gs) gs) Nothing

-- CR 303.4a/601.2c: an Aura spell's target is its enchant slot, defined by the
-- card, not by a mode -- Unholy Strength (the Auras gate card) has one empty
-- mode and a Face.enchant of "target creature" (CardSpec's auraCardSpec).
-- Task 6 merges Card.enchantSlotMap into allTargetSlots/modesTargetSlots and
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
        slots = Card.modesTargetSlots (Seq.singleton (ModeIndex.MkModeIndex 0)) (S.combinedFace unholyStrength)
    Spec.assertEqWith s "one slot, the enchant slot" (Set.singleton Card.enchantSlot) (Map.keysSet slots)
    Spec.assertEqWith
      s
      "its legal set is the one creature"
      (Target.legalSets Nothing Map.empty spellId slots gs)
      (Map.singleton Card.enchantSlot (Set.singleton (Recipient.ToCreature creature)))
  -- CR 601.2c: a spell whose required target has no legal choice cannot be
  -- cast at all. Reading only Mode.targetSlots would call this castable and
  -- let it be countered on resolution instead.
  Spec.it s "CR 601.2c: an Aura with no creature on the battlefield is not castable" $ do
    swamp <- S.printingOf s registry "Swamp"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    let base = S.landsInPlay swamp 1
        (gs, spellId) = S.handOne unholyStrength base
    Spec.assertBool s (not (S.castable S.alice spellId gs)) "not castable with an empty board"

-- alice active in her own precombat main phase, holding priority. boardWith's
-- tail, split out so the three-seat board below sets up the same way.
aliceOnTurn :: GameState.GameState -> GameState.GameState
aliceOnTurn gs =
  gs
    { GameState.phase = Phase.PrecombatMain,
      GameState.activePlayer = S.alice,
      GameState.priority = Just S.alice
    }

-- alice controls `n` untapped copies of `land` and has one card of `printing`
-- wherever `place` puts it, with priority in her own precombat main phase.
boardWith :: (Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)) -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
boardWith place land printing n =
  let (oid, gs) = place printing S.alice (S.landsInPlay land n)
   in (oid, aliceOnTurn gs)

-- The same board with the card in alice's HAND / in her GRAVEYARD.
inHandWith, inGraveyardWith :: Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
inHandWith = boardWith S.addHandCard
inGraveyardWith = boardWith S.addGraveyardCard

-- inGraveyardWith at THREE seats: the same lands, the same card in alice's
-- graveyard, the same phase and priority, with bob and carol both opposing her.
-- CR 800.1's board, and the smallest one on which "an opponent" and "your
-- opponent" are different questions.
inGraveyardWithThree :: Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
inGraveyardWithThree land printing n =
  let (oid, gs) = S.addGraveyardCard printing S.alice (S.landsFor land S.alice n S.threePlayerGame)
   in (oid, aliceOnTurn gs)

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
        Spec.assertBool s (elem (A.Cast inGraveyard (S.printingName firebolt) Facing.FaceUp) (Action.legalActions S.alice resolved1)) "and offered as a legal action"
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
  -- Rule 702.34a's "anywhere ELSE", the half a pattern pinned to one destination
  -- cannot say. Reprieve {1}{W} returns the flashed-back Firebolt to its owner's
  -- HAND, a destination that is neither graveyard nor exile, so the two readings
  -- of the rule put the card in different zones.
  Spec.it s "CR 702.34a a flashback spell bounced off the stack is exiled, not returned to hand" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    firebolt <- S.printingOf s registry "Firebolt"
    reprieve <- S.printingOf s registry "Reprieve"
    let (inGraveyard, gs0) = inGraveyardWith mountain firebolt 5
        gs1 = S.landsFor plains S.alice 2 gs0
        (bounce, gs2) = S.addHandCard reprieve S.alice gs1
        -- Reprieve's second sentence draws, and CR 104.3c would lose alice the
        -- game out from under the assertion on an empty library.
        (_, gs3) = S.addLibraryCard mountain S.alice gs2
        cast1 = S.runPure S.identityAnswer gs3 (S.cast S.alice inGraveyard)
    case GameState.stack cast1 of
      [] -> Spec.assertFailure s "expected the flashback spell on the stack"
      onStack : _ -> do
        -- The offered set is FILTERED rather than hand-built: Reprieve is on the
        -- stack beside its own target once CR 601.2a has put it there, so an
        -- answerer taking the smallest recipient could aim at the wrong spell.
        let aimAt :: Prompt.Prompt r -> r
            aimAt p = case p of
              Prompt.ChooseTargets _ _ _ sets -> S.preferring ((==) (Just onStack) . Recipient.objectOf) sets
              _ -> S.identityAnswer p
            cast2 = S.runPure aimAt cast1 (S.cast S.alice bounce)
            bounced = S.runPure aimAt cast2 Stack.resolveTop
            boltsIn zone gs = length (filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (S.printingName firebolt)) (Game.zoneMembers zone S.alice gs))
        Spec.assertEqWith s "CR 702.34a: the flashback card was NOT returned to its owner's hand" (boltsIn Zone.Hand bounced) 0
        Spec.assertEqWith s "it was exiled instead" (boltsIn Zone.Exile bounced) 1
        Spec.assertEqWith s "and not put into the graveyard either" (boltsIn Zone.Graveyard bounced) 0
        Spec.assertEqWith s "Reprieve's own sentences both happened: the spell left the stack and alice drew" (length (GameState.stack bounced), length (Game.zoneMembers Zone.Hand S.alice bounced)) (0, 1)
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
      (Face.manaCost (S.combinedFace firebolt))
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

-- CR 702.34a's conditional: "You may cast this card from your graveyard IF THE
-- RESULTING SPELL IS AN INSTANT OR SORCERY SPELL by paying [cost] rather than
-- paying its mana cost." The clause is a real condition on the CR 601.3
-- permission, not a restatement of where flashback is printed, and it is the one
-- half of rule 702.34a no printing can reach.
--
-- The proving card is a LABELED SYNTHETIC: "Synthetic Flashback Creature", a
-- {R} 2/1 creature with "Flashback {4}{R}". Nothing in the CR forbids the
-- printing -- rule 702.34a's "appears on some instants and sorceries" describes
-- the pool rather than restricting it, and the clause would be dead text if the
-- restriction were structural. No real card reaches it: every printed card with
-- flashback is an instant or a sorcery, every printed effect that GRANTS
-- flashback restricts itself to instant and sorcery cards (Snapcaster Mage), and
-- no effect in this pool changes a card's types while it sits in a graveyard.
--
-- What makes the negative discriminating is that its costs are Firebolt's
-- exactly, {R} with flashback {4}{R}, on Firebolt's own six-Mountain board. So
-- the two directions below differ in the type line and in nothing else, and the
-- from-hand case rules out mana, timing and the card itself as the reason the
-- graveyard cast is missing.
-- CR 702.34a's flashback GRANTED rather than printed, which is the whole of what
-- separates this group from Firebolt's above: the permission and the cost are
-- both read through the CR 613 projection of the card as it lies in the
-- graveyard, so a keyword nothing printed reaches them (#1385).
--
-- Viral Spawning {2}{G} Sorcery is the producer: "Create a 3/3 green Phyrexian
-- Beast creature token with toxic 1." plus "Corrupted -- As long as an opponent
-- has three or more poison counters and this card is in your graveyard, it has
-- flashback {2}{G}." Its static ability functions in the graveyard by CR 113.6b,
-- which the card's own text states ("this card is in your graveyard") and its
-- functionsFrom carries; CR 113.6f would put the same ability there anyway,
-- since the keyword it grants is one that says which zone the card may be cast
-- from, but CR 113.6b's "only" is what also keeps it OFF the stack while the
-- card is a spell.
--
-- The pair of boards differs in ONE number, the opponent's poison count, so the
-- branch cannot flip on mana, timing, the stack or the card. Three poison
-- counters is the rule's own threshold and two is one short. The case after it
-- adds a third seat, which is what makes the clause's "an opponent" observable
-- as an existential rather than as a name for the one other player.
--
-- What Viral Spawning cannot prove is WHICH cost was paid -- its flashback cost
-- and its mana cost are both {2}{G} -- so the second half of the group grants a
-- flashback that differs from the printed cost and pays that instead.
grantedFlashbackSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
grantedFlashbackSpec s registry = Spec.describe s "GrantedFlashback" $ do
  Spec.it s "CR 702.34a/113.6f a granted flashback is castable from the graveyard, and exiles the card" $ do
    forest <- S.printingOf s registry "Forest"
    spawning <- S.printingOf s registry "Viral Spawning"
    let (inGraveyard, board) = inGraveyardWith forest spawning 3
        poisoned n = S.addPlayerCounter PlayerCounterKind.Poison n S.bob board
        corrupted = poisoned 3
        uncorrupted = poisoned 2
    Spec.assertBool s (not (S.castable S.alice inGraveyard uncorrupted)) "two poison counters: no flashback, so not castable"
    Spec.assertBool s (not (any (S.isCastOf inGraveyard) (Action.legalActions S.alice uncorrupted))) "and not offered"
    Spec.assertBool s (S.castable S.alice inGraveyard corrupted) "three poison counters: castable"
    Spec.assertBool s (any (S.isCastOf inGraveyard) (Action.legalActions S.alice corrupted)) "and offered"
    let cast = S.runPure S.identityAnswer corrupted (S.cast S.alice inGraveyard)
        resolved = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertEqWith s "the spell resolved and made its token" (length (filter (\o -> Projection.subtypesOf o resolved == Set.fromList [Subtype.Beast, Subtype.Phyrexian]) (Game.zoneMembers Zone.Battlefield S.alice resolved))) 1
    -- CR 702.34a's SECOND static ability, and the assertion that tells a real
    -- flashback cast from a bare permission: the card must not come back.
    Spec.assertEqWith s "it did NOT go back to the graveyard" (Game.zoneMembers Zone.Graveyard S.alice resolved) []
    Spec.assertEqWith s "it was exiled instead" (length (Game.zoneMembers Zone.Exile S.alice resolved)) 1
  -- CR 122.1 / 102.2: "an opponent has three or more poison counters" is an
  -- EXISTENTIAL over the opponents, and three seats are the smallest board that
  -- tells it from "your opponent has" -- at two seats the two readings name the
  -- same player, which is why the case above cannot prove this one.
  --
  -- The three boards differ in the two opponents' poison counts and in nothing
  -- else. 2/2 is the negative: no opponent is at three, and it also rules out a
  -- SUM reading, which would total 4 and offer the cast. 2/4 and 4/2 are the two
  -- positives, so neither "the first opponent" nor "the last" is what is being
  -- read. No two of 2, 4 and the rule's own 3 coincide.
  Spec.it s "CR 122.1 the clause holds when ANY opponent is at three poison, at three seats" $ do
    forest <- S.printingOf s registry "Forest"
    spawning <- S.printingOf s registry "Viral Spawning"
    let (inGraveyard, board) = inGraveyardWithThree forest spawning 3
        poisoned b c = S.addPlayerCounter PlayerCounterKind.Poison c S.carol (S.addPlayerCounter PlayerCounterKind.Poison b S.bob board)
        neither = poisoned 2 2
        carolCorrupted = poisoned 2 4
        bobCorrupted = poisoned 4 2
    Spec.assertBool s (not (S.castable S.alice inGraveyard neither)) "two poison each: no opponent is at three, so no flashback"
    Spec.assertBool s (not (any (S.isCastOf inGraveyard) (Action.legalActions S.alice neither))) "and not offered"
    Spec.assertBool s (S.castable S.alice inGraveyard carolCorrupted) "carol at four: castable"
    Spec.assertBool s (any (S.isCastOf inGraveyard) (Action.legalActions S.alice carolCorrupted)) "and offered"
    Spec.assertBool s (S.castable S.alice inGraveyard bobCorrupted) "bob at four: castable too"
    Spec.assertBool s (any (S.isCastOf inGraveyard) (Action.legalActions S.alice bobCorrupted)) "and offered"
  -- CR 601.2b: the GRANTED cost is the one offered, and the printed mana cost is
  -- not. Lightning Bolt {R} carries no flashback of its own, so every candidate
  -- below came from the grant; the two costs share no symbol, so no board can
  -- pay one by paying the other.
  Spec.it s "CR 702.34a the granted cost, not the printed one, is what a graveyard cast pays" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let granted = Modification.GainKeyword (Keyword.Flashback (Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, ManaSymbol.OfType (ManaType.Colored Color.Blue)])) []))
        withGrant oid = S.withEffect oid granted
        (onIslands, islandBoard) = inGraveyardWith island bolt 3
        (onMountains, mountainBoard) = inGraveyardWith mountain bolt 3
        blueBoard = withGrant onIslands islandBoard
        redBoard = withGrant onMountains mountainBoard
        manaOf oid gs = fmap Cost.Type.mana (Cost.costsFor (S.printingName bolt) oid gs)
    Spec.assertEqWith
      s
      "the flashback {2}{U} is the only candidate offered"
      (manaOf onIslands blueBoard)
      [Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, ManaSymbol.OfType (ManaType.Colored Color.Blue)])]
    Spec.assertEqWith s "and with no grant there is no candidate at all" (manaOf onIslands islandBoard) []
    Spec.assertBool s (S.castable S.alice onIslands blueBoard) "three Islands pay the granted {2}{U}"
    -- Three Mountains pay the PRINTED {R} and cannot pay {2}{U}. The cast is
    -- permitted on this board -- the grant is the same one -- so only the cost
    -- can be refusing it.
    Spec.assertBool s (not (S.castable S.alice onMountains redBoard)) "three Mountains do not"
    Spec.assertBool s (uncurry (S.castable S.alice) (inHandWith mountain bolt 3)) "though they pay the printed {R} from hand"
    -- CR 702.34a's second static ability off a grant that CANNOT survive the
    -- move: this one is stored against the graveyard object's id, and CR 601.2a
    -- mints a new one, so the replacement has to be armed from the keywords the
    -- card held where it lay.
    let cast = S.runPure S.identityAnswer blueBoard (S.cast S.alice onIslands)
        resolved = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertEqWith s "the flashed-back Bolt did not return to the graveyard" (Game.zoneMembers Zone.Graveyard S.alice resolved) []
    Spec.assertEqWith s "it was exiled" (length (Game.zoneMembers Zone.Exile S.alice resolved)) 1

-- CR 702.34a's OTHER conditional, the one on its second static ability: "IF THE
-- FLASHBACK COST WAS PAID, exile this card instead of putting it anywhere else
-- any time it would leave the stack." A card that has flashback and is cast
-- from a graveyard for some other cost is not exiled, and the clause is the
-- only thing that says so.
--
-- The producer is a LABELED SYNTHETIC, "Synthetic Grave Recital" {2}{B} Sorcery:
-- "Until end of turn, you may cast instant and sorcery cards from your
-- graveyard." That is Yawgmoth's Will's first sentence with its second one --
-- the rider that exiles everything of yours heading for a graveyard this turn --
-- left off, and the rider is exactly why the pool's own printing cannot observe
-- this: with it, the right answer and the wrong answer are both exile.
--
-- Harness the Storm ({2}{R} Enchantment, "Whenever you cast an instant or sorcery
-- spell from your hand, you may cast target card with the same name as that spell
-- from your graveyard") is now in the pool and does NOT replace this synthetic,
-- which is worth writing down because it reads as though it should. That card's
-- clause is an Effect.OfferCast -- CR 601.3's "effect" giving ONE cast during a
-- resolution -- so it never reaches PlayerEffect.CastFromGraveyard, the standing
-- permission this group is about. See harnessTheStormSpec below.
--
-- The pair of boards is ONE board and two answerers, so mana, seats, timing and
-- stock cannot be the difference: the permission and the flashback cost are both
-- available and both payable, and the only thing that varies is which candidate
-- CR 601.2b's announcement settles on.
graveRecitalSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
graveRecitalSpec s registry = Spec.describe s "GraveRecital" $ do
  Spec.it s "CR 702.34a the flashback cost exiles the card; the permission's printed cost does not" $ do
    mountain <- S.printingOf s registry "Mountain"
    swamp <- S.printingOf s registry "Swamp"
    firebolt <- S.printingOf s registry "Firebolt"
    recital <- S.printingOf s registry "Synthetic Grave Recital"
    let base = S.landsFor swamp S.alice 3 (S.landsInPlay mountain 7)
        (inGraveyard, gs1) = S.addGraveyardCard firebolt S.alice base
        (inHand, gs2) = S.addHandCard recital S.alice gs1
        board = aliceOnTurn gs2
        permitted = S.runPure S.identityAnswer (S.runPure S.identityAnswer board (S.cast S.alice inHand)) Stack.resolveTop
        -- CR 601.2b's announcement, answered by naming a cost rather than an
        -- index: the flashback {4}{R} and the printed {R} share no reading.
        paying :: [ManaSymbol.ManaSymbol] -> Prompt.Prompt r -> r
        paying wanted p = case p of
          Prompt.ChooseCost _ _ _ candidates ->
            Maybe.fromMaybe (Cost.firstOffered candidates) (List.find ((== Just (ManaCost.MkManaCost wanted)) . Cost.Type.mana) candidates)
          _ -> S.identityAnswer p
        payingFlashback, payingPrinted :: Prompt.Prompt r -> r
        payingFlashback = paying [ManaSymbol.Generic 4, theRed]
        payingPrinted = paying [theRed]
        castFor, resolveWith :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState
        castFor answer = S.runPure answer permitted (S.cast S.alice inGraveyard)
        resolveWith answer = S.runPure answer (castFor answer) Stack.resolveTop
        flashedBack = resolveWith payingFlashback
        boughtBack = resolveWith payingPrinted
        boltsIn zone gs = length (filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (S.printingName firebolt)) (Game.zoneMembers zone S.alice gs))
    Spec.assertEqWith
      s
      "both costs are on offer from the graveyard"
      (fmap Cost.Type.mana (Cost.costsFor (S.printingName firebolt) inGraveyard permitted))
      [Just (ManaCost.MkManaCost [ManaSymbol.Generic 4, theRed]), Just (ManaCost.MkManaCost [theRed])]
    Spec.assertEqWith s "the flashback cast dealt its 2" (S.lifeOf S.alice flashedBack) (Just 18)
    Spec.assertEqWith s "and the card was exiled (CR 702.34a)" (boltsIn Zone.Exile flashedBack) 1
    Spec.assertEqWith s "not put into the graveyard" (boltsIn Zone.Graveyard flashedBack) 0
    Spec.assertEqWith s "the printed-cost cast dealt its 2 as well" (S.lifeOf S.alice boughtBack) (Just 18)
    Spec.assertEqWith s "and the card was NOT exiled, since the flashback cost was not paid" (boltsIn Zone.Exile boughtBack) 0
    Spec.assertEqWith s "it went to the graveyard" (boltsIn Zone.Graveyard boughtBack) 1

-- CR 702.34a states no limit on how many flashback abilities an object has, and
-- CR 601.2b's "a player can't apply two alternative methods of casting or two
-- alternative costs to a single spell" is what makes two of them a CHOICE rather
-- than a sum.
--
-- The Fugitive Doctor {3}{R}{G} is the pool's one producer of a graveyard card
-- holding two, and the only printing whose grant of one is a LITERAL cost and
-- nothing else. Archmage's Newt and Iroh, Grand Lotus each state a literal cost
-- for one class of card and "the flashback cost is equal to that card's mana
-- cost" for another; every remaining granter states only the second, which
-- Modification.GainKeyword's literal Keyword cannot express (#1981).
--
-- Firebolt's printed {4}{R} and the granted {2}{R}{G} share no reading, and
-- WHICH of them is the unreachable one is decided by Keyword's derived Ord:
-- Generic 2 sorts under Generic 4, so the GRANTED cost is the lesser and the
-- PRINTED one is the second. Ten lands pay either, so no assertion below turns
-- on mana.
--
-- Not implemented: CR 603.12's reflexive triggered ability. "You may sacrifice a
-- Clue. When you do, target instant or sorcery card in your graveyard gains
-- flashback {2}{R}{G} until end of turn" is transcribed as one clause with a CR
-- 118.12 pay gate, so the target is chosen as the attack trigger goes on the
-- stack rather than after the sacrifice (#1982).
fugitiveDoctorAnswer :: Prompt.Prompt r -> r
fugitiveDoctorAnswer p = case p of
  -- The Clue is worth spending: without the sacrifice the pay gate's IfPaid
  -- branch never runs and no second flashback is granted.
  Prompt.ChooseToPay {} -> PaymentDecision.Pays
  -- The graveyard holds ONE instant-or-sorcery card, so taking every legal
  -- recipient takes exactly the Firebolt, and the slot's count is satisfied.
  Prompt.ChooseTargets _ _ _ sets -> fmap snd sets
  _ -> S.aggressiveAnswer p

-- alice attacks with The Fugitive Doctor, sacrifices the Clue its own enters
-- trigger made, and grants the graveyard Firebolt a second flashback. The board
-- returned sits in the postcombat main phase, where a sorcery may be cast.
fugitiveDoctorBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, ObjectId.ObjectId)
fugitiveDoctorBoard s registry = do
  mountain <- S.printingOf s registry "Mountain"
  forest <- S.printingOf s registry "Forest"
  firebolt <- S.printingOf s registry "Firebolt"
  doctor <- S.printingOf s registry "The Fugitive Doctor"
  let (combat, _, _) = S.combatBoardOf [] []
      lands = S.landsFor forest S.alice 3 (S.landsFor mountain S.alice 7 combat)
      (inGraveyard, buried) = S.addGraveyardCard firebolt S.alice lands
      -- entersWithTrigger rather than addCreature: the Clue this ability's
      -- pay gate spends is the Doctor's OWN CR 701.16a investigate, so the
      -- fixture makes it the way the card does.
      (_, entered) = S.entersWithTrigger doctor S.alice buried
      withClue = S.runPure S.identityAnswer entered (Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
  pure (S.runCombat fugitiveDoctorAnswer withClue, inGraveyard)

fugitiveDoctorSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
fugitiveDoctorSpec s registry = Spec.describe s "FugitiveDoctor" $ do
  Spec.it s "CR 702.34a/601.2b two flashback abilities offer two costs, and either one exiles the card" $ do
    firebolt <- S.printingOf s registry "Firebolt"
    (board, inGraveyard) <- fugitiveDoctorBoard s registry
    let granted = ManaCost.MkManaCost [ManaSymbol.Generic 2, theRed, ManaSymbol.OfType (ManaType.Colored Color.Green)]
        printed = ManaCost.MkManaCost [ManaSymbol.Generic 4, theRed]
        -- graveRecitalSpec's announcement, answered by naming a cost: the two
        -- flashback costs share no reading, so no answer here is an index.
        paying :: ManaCost.ManaCost -> Prompt.Prompt r -> r
        paying wanted p = case p of
          Prompt.ChooseCost _ _ _ candidates ->
            Maybe.fromMaybe (Cost.firstOffered candidates) (List.find ((== Just wanted) . Cost.Type.mana) candidates)
          -- Firebolt's own "any target", aimed at alice so that its 2 damage
          -- reports which cast resolved rather than which permanent died.
          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.alice))) sets
          _ -> S.identityAnswer p
        resolveWith :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState
        resolveWith answer = S.runPure answer (S.runPure answer board (S.cast S.alice inGraveyard)) Stack.resolveTop
        boltsIn zone gs = length (filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (S.printingName firebolt)) (Game.zoneMembers zone S.alice gs))
    Spec.assertEqWith s "the fixture reached the postcombat main phase" (GameState.phase board) Phase.PostcombatMain
    Spec.assertEqWith
      s
      "CR 601.2b: both flashback costs are on offer, the granted one first by Ord"
      (fmap Cost.Type.mana (Cost.costsFor (S.printingName firebolt) inGraveyard board))
      [Just granted, Just printed]
    -- CR 702.34a's SECOND static ability, asked of the cost a first-only read
    -- never returns: paying the PRINTED {4}{R} must exile the card too.
    Spec.assertEqWith s "the printed cost's cast dealt its 2" (S.lifeOf S.alice (resolveWith (paying printed))) (Just 18)
    Spec.assertEqWith s "and exiled the card (CR 702.34a)" (boltsIn Zone.Exile (resolveWith (paying printed))) 1
    Spec.assertEqWith s "not put it into the graveyard" (boltsIn Zone.Graveyard (resolveWith (paying printed))) 0
    Spec.assertEqWith s "the granted cost's cast dealt its 2 as well" (S.lifeOf S.alice (resolveWith (paying granted))) (Just 18)
    Spec.assertEqWith s "and exiled the card too" (boltsIn Zone.Exile (resolveWith (paying granted))) 1
    Spec.assertEqWith s "not put it into the graveyard either" (boltsIn Zone.Graveyard (resolveWith (paying granted))) 0

-- Harness the Storm {2}{R} Enchantment (data/cards/harness-the-storm.json):
-- "Whenever you cast an instant or sorcery spell from your hand, you may cast
-- target card with the same name as that spell from your graveyard."
--
-- Two axes at once, and they are the two the card needs that nothing else in the
-- pool asks for: CR 601.2a's ZONE on the trigger condition
-- (Pawl.Types.SpellCast.zone) and CR 709.4a's name comparison against a bound
-- object on the target slot (Filter.SameNameAsBound over Binding.castSpell).
--
-- ONE BOARD, TWO CASTS. Both cases below build exactly the same state -- six
-- Mountains, the enchantment, two Firebolts and a Lightning Bolt in the
-- graveyard, a third Firebolt in hand -- and differ only in WHICH Firebolt is
-- cast, which is to say only in the zone it was cast from. So the negative
-- cannot be turning on mana, timing, seats or an empty target set: the
-- graveyard's other Firebolt would be a legal target if the trigger fired.
--
-- The Lightning Bolt is the name decoy: also alice's, also in the graveyard,
-- also a one-mana red damage spell, so the ONLY axis separating it from a
-- Firebolt is CR 201.2's name. Two Firebolts rather than one because a
-- one-candidate slot is answered without a prompt at all, and because a set
-- assertion over a single member cannot tell "matched by name" from "matched
-- everything the pool held".
harnessBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, [ObjectId.ObjectId])
harnessBoard s registry = do
  mountain <- S.printingOf s registry "Mountain"
  firebolt <- S.printingOf s registry "Firebolt"
  bolt <- S.printingOf s registry "Lightning Bolt"
  harness <- S.printingOf s registry "Harness the Storm"
  let (_, g1) = S.addCreature harness S.alice (S.landsInPlay mountain 6)
      (buried1, g2) = S.addGraveyardCard firebolt S.alice g1
      (buried2, g3) = S.addGraveyardCard firebolt S.alice g2
      (_, g4) = S.addGraveyardCard bolt S.alice g3
      (inHand, g5) = S.addHandCard firebolt S.alice g4
  pure (aliceOnTurn g5, inHand, buried1, [buried1, buried2])

-- Records what Harness the Storm's own slot was offered and which casts it went
-- on to offer, taking every offer. The recording is the point: what a cast FINDS
-- cannot tell a candidate set computed by name from one that admitted every card
-- in the graveyard.
--
-- "twin" is the slot the card's own JSON declares, which is what keeps Firebolt's
-- "any target" out of the recording: that prompt names its own slot and this one
-- looks the card's up.
harnessAnswer :: Prompt.Prompt r -> State.State ([Set.Set Recipient.Recipient], [CardName.CardName]) r
harnessAnswer p = case p of
  Prompt.ChooseTargets _ _ _ sets -> do
    Monad.forM_
      (Map.lookup (SlotName.MkSlotName (Text.pack "twin")) sets)
      (\(_, rs) -> State.modify' (\(ts, os) -> (ts <> [rs], os)))
    pure (S.identityAnswer p)
  Prompt.OfferedCast _ _ _ name -> do
    State.modify' (\(ts, os) -> (ts, os <> [name]))
    pure OptionalDecision.Exercises
  _ -> pure (S.identityAnswer p)

runHarness :: GameState.GameState -> ObjectId.ObjectId -> ([Set.Set Recipient.Recipient], [CardName.CardName])
runHarness gs oid = snd (State.runState (Engine.runGame harnessAnswer gs (do S.cast S.alice oid; Engine.priorityLoop)) ([], []))

harnessTheStormSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
harnessTheStormSpec s registry = Spec.describe s "HarnessTheStorm" $ do
  Spec.it s "CR 201.2 the trigger's slot offers the same-named graveyard cards and no other" $ do
    (board, inHand, _, buried) <- harnessBoard s registry
    let (offered, offers) = runHarness board inHand
    Spec.assertEqWith s "alice's graveyard held three cards" (length (Game.zoneMembers Zone.Graveyard S.alice board)) 3
    -- Identity, not count. The Lightning Bolt is in the same graveyard, is the
    -- same colour and costs the same, so a filter reading anything but the name
    -- puts it in this set.
    Spec.assertEqWith s "both Firebolts, and not the Lightning Bolt" offered [Set.fromList (fmap Recipient.ToObject buried)]
    -- CR 601.3: the offer IS the permission, so a Firebolt with no flashback
    -- would be cast this way just the same -- and the cast it makes is from the
    -- GRAVEYARD, which the trigger does not watch, so exactly one offer is made
    -- rather than a chain.
    Spec.assertEqWith s "one cast offered, of the named card" offers [CardName.MkCardName (Text.pack "Firebolt")]
  Spec.it s "CR 601.2a the same board casting the same card from the GRAVEYARD does not trigger" $ do
    -- The pair. Everything is the board above; only the cast Firebolt's zone
    -- moves, and it is affordable there because rule 702.34a's flashback {4}{R}
    -- is within the same six Mountains.
    (board, _, fromGraveyard, _) <- harnessBoard s registry
    let (offered, offers) = runHarness board fromGraveyard
    Spec.assertEqWith s "the trigger's slot was never offered" offered []
    Spec.assertEqWith s "and no cast was offered" offers []

flashbackCardTypeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
flashbackCardTypeSpec s registry = Spec.describe s "FlashbackCardType" $ do
  Spec.it s "CR 702.34a a creature card with flashback is not castable from the graveyard" $ do
    mountain <- S.printingOf s registry "Mountain"
    creature <- S.printingOf s registry "Synthetic Flashback Creature"
    let (inGraveyard, graveyardBoard) = inGraveyardWith mountain creature 6
        (inHand, handBoard) = inHandWith mountain creature 6
    Spec.assertBool s (not (Card.isInstant (S.combinedFace creature))) "not an instant"
    Spec.assertBool s (not (Card.isSorcery (S.combinedFace creature))) "not a sorcery"
    Spec.assertBool s (S.castable S.alice inHand handBoard) "castable from her hand, so the board affords it"
    Spec.assertBool s (not (S.castable S.alice inGraveyard graveyardBoard)) "not castable from the graveyard"
    Spec.assertBool s (not (any (S.isCastOf inGraveyard) (Action.legalActions S.alice graveyardBoard))) "and not offered"
  -- The other direction, on the same board: the clause holds for a card that
  -- meets it, so the check is not simply refusing every flashback cast.
  Spec.it s "CR 702.34a a sorcery card with the same keyword is castable from the graveyard" $ do
    mountain <- S.printingOf s registry "Mountain"
    firebolt <- S.printingOf s registry "Firebolt"
    let (inGraveyard, gs) = inGraveyardWith mountain firebolt 6
    Spec.assertBool s (Card.isSorcery (S.combinedFace firebolt)) "a sorcery"
    Spec.assertBool s (S.castable S.alice inGraveyard gs) "castable from the graveyard"
    Spec.assertBool s (any (S.isCastOf inGraveyard) (Action.legalActions S.alice gs)) "and offered"
  -- Rule 702.34a's clause is a DISJUNCTION, and Think Twice is its instant limb:
  -- {1}{U} "Draw a card." with "Flashback {2}{U}". Without this case a check that
  -- asked only about sorceries would still pass everything above.
  Spec.it s "CR 702.34a an instant card with flashback is castable from the graveyard too" $ do
    island <- S.printingOf s registry "Island"
    thinkTwice <- S.printingOf s registry "Think Twice"
    let (inGraveyard, gs) = inGraveyardWith island thinkTwice 3
    Spec.assertBool s (Card.isInstant (S.combinedFace thinkTwice)) "an instant"
    Spec.assertBool s (S.castable S.alice inGraveyard gs) "castable from the graveyard"
    Spec.assertBool s (any (S.isCastOf inGraveyard) (Action.legalActions S.alice gs)) "and offered"

-- CR 702.133a's two static abilities, on Direct Current {1}{R}{R} Sorcery,
-- "Direct Current deals 2 damage to any target." plus jump-start.
--
-- Firebolt's twin by design: the same 2 damage from a graveyard, differing only
-- in how the cast is paid for. That is what these cases are about -- rule
-- 702.133a's cost is ADDITIONAL where rule 702.34a's is alternative, so the
-- printed {1}{R}{R} is still owed and a card leaves the hand on top of it.
--
-- EVERY BOARD BELOW CARRIES THREE MOUNTAINS, positives and negatives alike, so
-- the only thing a negative can be turning on is the discard.
jumpStartSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
jumpStartSpec s registry = Spec.describe s "JumpStart" $ do
  -- The whole card end to end. The hand shrinking and the sorcery landing in
  -- EXILE are the two discriminating readings: a cast that skipped the
  -- additional cost leaves two cards in hand, and one that skipped rule
  -- 702.133a's second ability leaves the sorcery in the graveyard to be cast
  -- again.
  --
  -- What this case does NOT prove is the PERMISSION: S.cast goes straight to
  -- Cast.castSpell, so deleting rule 702.133a's permission leaves it green. The
  -- next case is where the permission is asked.
  Spec.it s "CR 702.133a cast from the graveyard for {1}{R}{R} plus a discard, then exiled" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    directCurrent <- S.printingOf s registry "Direct Current"
    let (inGraveyard, board) = inGraveyardWith mountain directCurrent 3
        (_, oneInHand) = S.addHandCard piker S.alice board
        -- TWO cards in hand, not one: the payment prompt short-circuits when the
        -- candidates equal the count, so a one-card hand would prove the discard
        -- happened without proving anybody was asked which card.
        (_, gs) = S.addHandCard piker S.alice oneInHand
        cast = S.runPure S.identityAnswer gs (S.cast S.alice inGraveyard)
        resolved = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertEqWith s "two cards in hand before" (length (Game.zoneMembers Zone.Hand S.alice gs)) 2
    Spec.assertEqWith s "it dealt 2 (identityAnswer targets the lowest recipient)" (S.lifeOf S.alice resolved) (Just 18)
    Spec.assertEqWith s "one card left in hand, so the discard was paid" (length (Game.zoneMembers Zone.Hand S.alice resolved)) 1
    Spec.assertEqWith s "the graveyard holds the discarded card alone" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 1
    Spec.assertEqWith s "and the sorcery was exiled" (length (Game.zoneMembers Zone.Exile S.alice resolved)) 1
  -- The negative and its control, one card apart. Both boards afford the mana,
  -- both hold the same sorcery in the same graveyard; only the hand differs.
  Spec.it s "CR 702.133a an empty hand cannot pay the additional cost" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    directCurrent <- S.printingOf s registry "Direct Current"
    let (inGraveyard, emptyHand) = inGraveyardWith mountain directCurrent 3
        (_, oneInHand) = S.addHandCard piker S.alice emptyHand
    Spec.assertBool s (not (S.castable S.alice inGraveyard emptyHand)) "not castable with an empty hand"
    Spec.assertBool s (not (any (S.isCastOf inGraveyard) (Action.legalActions S.alice emptyHand))) "and not offered"
    Spec.assertBool s (S.castable S.alice inGraveyard oneInHand) "castable once there is a card to discard"
    Spec.assertBool s (any (S.isCastOf inGraveyard) (Action.legalActions S.alice oneInHand)) "and offered"
  -- Crux: ADDITIONAL, not alternative. The mana part is the printed cost in both
  -- zones -- unlike flashback, which replaces it -- and the discard rides only
  -- the graveyard candidate.
  Spec.it s "CR 702.133a the discard is an additional cost, and only from the graveyard" $ do
    mountain <- S.printingOf s registry "Mountain"
    directCurrent <- S.printingOf s registry "Direct Current"
    let (fromHand, handBoard) = inHandWith mountain directCurrent 3
        (fromGraveyard, graveyardBoard) = inGraveyardWith mountain directCurrent 3
        costsOf oid gs = fmap (\c -> (Cost.Type.mana c, Cost.Type.components c)) (Cost.costsFor (S.printingName directCurrent) oid gs)
        printed = Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, theRed, theRed])
    Spec.assertEqWith s "from hand, the printed {1}{R}{R} and no components" (costsOf fromHand handBoard) [(printed, [])]
    Spec.assertEqWith
      s
      "from the graveyard, the same {1}{R}{R} plus one discard"
      (costsOf fromGraveyard graveyardBoard)
      [(printed, [CostComponent.DiscardCards (DiscardCards.MkDiscardCards 1 (Filter.And []))])]
  -- Rule 702.133a's "if the resulting spell is an instant or sorcery spell",
  -- flashback's clause word for word. No printing can reach the failing side --
  -- jump-start appears only on instants and sorceries -- so this is asserted at
  -- the function rather than over a board, and is a regression fence rather than
  -- a card's behaviour.
  Spec.it s "CR 702.133a the permission is gated on the card's types" $ do
    Spec.assertEqWith
      s
      "a sorcery gets the permission"
      (Keyword.Engine.permissionsFor (Set.singleton CardType.Sorcery) Keyword.JumpStart)
      [CastingPermission.CastFromGraveyard]
    Spec.assertEqWith
      s
      "an instant does too"
      (Keyword.Engine.permissionsFor (Set.singleton CardType.Instant) Keyword.JumpStart)
      [CastingPermission.CastFromGraveyard]
    Spec.assertEqWith
      s
      "a creature does not"
      (Keyword.Engine.permissionsFor (Set.singleton CardType.Creature) Keyword.JumpStart)
      []

-- CR 205.4e: "A player can't cast a legendary instant or sorcery spell unless
-- that player controls a legendary creature or a legendary planeswalker." The
-- OTHER half of what the legendary supertype means -- CR 205.4d's legend rule
-- (CR 704.5j) is Pawl.Engine.Sba's, and this one is Pawl.Engine.Cast's.
--
-- The proving card is Urza's Ruinous Blast, {4}{W} Legendary Sorcery, "Exile all
-- nonland permanents that aren't legendary." (Its parenthetical is reminder text
-- for this very rule and is not transcribed.) Its sweep is an
-- ObjectRef.EachMatching over And [Not (HasCardType Land), Not (HasSupertype
-- Legendary)] moved to exile -- the shape Evacuation already carries with a
-- different destination and a simpler filter, so nothing new was built for it.
--
-- EVERY BOARD BELOW CARRIES SIX PLAINS, the positives and the negatives alike.
-- That is what keeps the negatives from passing vacuously: a cast gate reads
-- False for a dozen reasons that are not CR 205.4e, and the DEAREST case here is
-- the one with a Thalia on it -- she taxes the sorcery {1} of her own
-- ("noncreature spells cost {1} more"), so {5}{W} is the ceiling and six Plains
-- pay it. Delete the Legendary supertype from the card and all three negatives
-- go green, which is how the set was shown to turn on the rule and not on mana.
--
-- The three permanents in play across these cases are what make the assertions
-- discriminating. Thalia, Guardian of Thraben is a legendary CREATURE, and
-- Mindslaver a legendary ARTIFACT -- the case that fails if the
-- check reads the supertype and forgets the card type. And Thalia under bob's
-- control is the case that fails if the check forgets "that player controls".
legendarySpellSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
legendarySpellSpec s registry = Spec.describe s "LegendarySpell" $ do
  -- The control for every negative below: same six Plains, plus a legendary
  -- creature. Thalia's tax makes this the DEAREST board of the group, so every
  -- other case is affordable a fortiori and only CR 205.4e can be stopping them.
  Spec.it s "CR 205.4e castable while its caster controls a legendary creature" $ do
    plains <- S.printingOf s registry "Plains"
    thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
    sorcery <- S.printingOf s registry "Urza's Ruinous Blast"
    let (oid, gs) = inHandWith plains sorcery 6
        board = snd (S.addCreature thalia S.alice gs)
    Spec.assertBool s (S.castable S.alice oid board) "castable"
    Spec.assertBool s (elem (A.Cast oid (S.printingName sorcery) Facing.FaceUp) (Action.legalActions S.alice board)) "and offered as a legal action"
  -- Rule 205.4e's SECOND disjunct, "or a legendary planeswalker". Jace
  -- Beleren is one, and it is not a creature -- so this case fails for any
  -- reading that collapsed the rule onto the creature limb.
  Spec.it s "CR 205.4e castable while its caster controls a legendary planeswalker" $ do
    plains <- S.printingOf s registry "Plains"
    jace <- S.printingOf s registry "Jace Beleren"
    sorcery <- S.printingOf s registry "Urza's Ruinous Blast"
    let (oid, gs) = inHandWith plains sorcery 6
        board = snd (S.addCreature jace S.alice gs)
    Spec.assertBool s (not (Card.isCreature (S.combinedFace jace))) "not a creature"
    Spec.assertBool s (S.castable S.alice oid board) "castable"
    Spec.assertBool s (elem (A.Cast oid (S.printingName sorcery) Facing.FaceUp) (Action.legalActions S.alice board)) "and offered as a legal action"
  Spec.it s "CR 205.4e not castable with no legendary permanent at all" $ do
    plains <- S.printingOf s registry "Plains"
    sorcery <- S.printingOf s registry "Urza's Ruinous Blast"
    let (oid, gs) = inHandWith plains sorcery 6
    Spec.assertBool s (not (S.castable S.alice oid gs)) "not castable"
    Spec.assertBool s (not (any (S.isCastOf oid) (Action.legalActions S.alice gs))) "and not offered"
  -- The supertype alone is not the condition: rule 205.4e names a legendary
  -- CREATURE (or planeswalker), and Mindslaver is a legendary artifact.
  Spec.it s "CR 205.4e a legendary artifact does not satisfy it" $ do
    plains <- S.printingOf s registry "Plains"
    mindslaver <- S.printingOf s registry "Mindslaver"
    sorcery <- S.printingOf s registry "Urza's Ruinous Blast"
    let (oid, gs) = inHandWith plains sorcery 6
        board = snd (S.addCreature mindslaver S.alice gs)
    Spec.assertBool s (not (S.castable S.alice oid board)) "not castable"
    Spec.assertBool s (not (any (S.isCastOf oid) (Action.legalActions S.alice board))) "and not offered"
  -- "unless THAT PLAYER controls": an opponent's legendary creature is no
  -- help. Bob's Thalia still taxes alice (her ability is EachPlayer-scoped), so
  -- this board is the positive one's cost exactly -- {5}{W} against six Plains
  -- -- and CR 205.4e is the only thing left to fail.
  Spec.it s "CR 205.4e an opponent's legendary creature does not satisfy it" $ do
    plains <- S.printingOf s registry "Plains"
    thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
    sorcery <- S.printingOf s registry "Urza's Ruinous Blast"
    let (oid, gs) = inHandWith plains sorcery 6
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
    Spec.assertBool s (elem (A.Cast oid (S.printingName thalia) Facing.FaceUp) (Action.legalActions S.alice gs)) "and offered as a legal action"
  -- Gameplay level, through the stack: the permitted cast RESOLVES, and what it
  -- does is read off the board rather than assumed. Three survivors-or-not,
  -- one per clause of the filter: the Goblin Piker is a nonland nonlegendary
  -- permanent and is exiled, Thalia is legendary and stays, and the Plains are
  -- land and stay. Nothing here turns on whose permanent it is -- the card says
  -- "all", and CR 109.2 puts that set on the battlefield without regard to
  -- controller, so the one Piker is alice's only because the fixture is hers.
  --
  -- The three are deliberately INDEPENDENT rather than one exile-contents
  -- equality: an assertion failure ends the case, so a single list comparison
  -- would be the only thing any of the three mutations ever tripped. Counting
  -- just the Piker leaves the survivors to be asserted on their own, and each
  -- clause of the filter then has an assertion that only it can turn red.
  Spec.it s "CR 205.4e the permitted cast resolves, exiling by CR 109.2's set" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
    sorcery <- S.printingOf s registry "Urza's Ruinous Blast"
    let (oid, gs) = inHandWith plains sorcery 6
        (thaliaId, withThalia) = S.addCreature thalia S.alice gs
        board = snd (S.addCreature piker S.alice withThalia)
        cast = S.runPure S.identityAnswer board (S.cast S.alice oid)
        resolved = S.runPure S.identityAnswer cast Stack.resolveTop
        exiled = fmap (`Projection.namesOf` resolved) (Game.zoneMembers Zone.Exile S.alice resolved)
    Spec.assertEqWith s "the nonlegendary nonland permanent is in exile" (length (filter (Set.member (S.printingName piker)) exiled)) 1
    Spec.assertBool s (S.onBattlefield thaliaId resolved) "the legendary creature is still on the battlefield"
    Spec.assertEqWith s "and the lands are all still on the battlefield" (S.countOnBattlefieldByName (S.printingName plains) S.alice resolved) 6

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
  AttackTarget.OfBattle _ -> False

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
    Spec.assertBool s (elem (A.Cast bobsRally (S.printingName rally) Facing.FaceUp) (Action.legalActions S.bob attacked)) "and offered as a legal action"
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
  -- Both of Rally's clauses fail in the declare blockers step, and this case
  -- pins the wider one: CR 511.3 keeps the PHASE-scoped record live until the
  -- end of combat step ends, so bob is still on it, and the window has passed.
  -- (The step-scoped record is already empty by then -- the case below is what
  -- proves that -- so this is a conjunction failing rather than one clause
  -- isolated.)
  --
  -- Carries its own control, in the same step and for the same player: bob's
  -- Bolt is still offered, so what stops the Rally is the clauses and not the
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
    Spec.assertBool s (elem (A.Cast boltId (S.printingName bolt) Facing.FaceUp) (Action.legalActions S.bob later)) "bob's unrestricted instant still is"
  -- CR 508.6 on CR 500.1's span: "you've been attacked this step" asks about ONE
  -- STEP, and no printed card tells that from "this combat phase" -- Scryfall
  -- `o:"been attacked this step"`, 2026-08-21, returns fifteen cards and every
  -- one of them also prints "only during the declare attackers step", where the
  -- two spans coincide. So the discriminating card is Synthetic Belated Rally,
  -- Rally the Troops with the DuringPhase clause removed and nothing else
  -- changed; a printing that drops that clause would refute this and replace it.
  --
  -- The boundary is crossed by RUNNING the engine rather than by writing
  -- GameState.phase, as the case above does: what makes the answer flip is a
  -- reset at the end of every step (Pawl.Engine.Combat.clearAttackedThisStep),
  -- which a hand-set phase never reaches.
  Spec.it s "CR 508.6 / 500.1 the record is empty in the declare blockers step" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    rally <- S.printingOf s registry "Rally the Troops"
    belated <- S.printingOf s registry "Synthetic Belated Rally"
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (_, _, _, board) = rallyBoard piker plains rally
        (bobsBelated, withBelated) = S.addHandCard belated S.bob board
        (boltId, withBolt) = S.addHandCard bolt S.bob (snd (S.addCreature mountain S.bob withBelated))
        attacked = S.runPure S.aggressiveAnswer withBolt (Combat.declareAttackers S.alice)
        later = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer withBolt
    Spec.assertBool s (S.castable S.bob bobsBelated attacked) "castable in the step the attack was declared"
    Spec.assertEqWith s "the engine reached the next step" (GameState.phase later) (Phase.Combat CombatStep.DeclareBlockers)
    Spec.assertBool s (not (S.castable S.bob bobsBelated later)) "not castable once that step has ended"
    Spec.assertBool s (not (any (S.isCastOf bobsBelated) (Action.legalActions S.bob later))) "and not offered"
    -- What separates "the step ended" from "combat ended": the phase-scoped
    -- record still names bob, CR 511.3 emptying it only as the end of combat step
    -- ends, so the answer changed because of the step and nothing else.
    Spec.assertBool s (Set.member (AttackTarget.OfPlayer S.bob) (Combat.Type.declaredAttacked (GameState.combat later))) "bob is still on the phase-scoped record"
    Spec.assertBool s (Set.null (Combat.Type.declaredAttackedThisStep (GameState.combat later))) "and off the step-scoped one"
    -- CR 117.1a, as for Rally above: the step is not closed to bob.
    Spec.assertBool s (elem (A.Cast boltId (S.printingName bolt) Facing.FaceUp) (Action.legalActions S.bob later)) "bob's unrestricted instant is castable there"
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
    Spec.assertBool s (elem (A.Cast boltId (S.printingName bolt) Facing.FaceUp) (Action.legalActions S.alice withBolt)) "and offered as a legal action"
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

  necrologiaSpec s registry

-- Necrologia {3}{B}{B} Instant: "Cast this spell only during your end step. As
-- an additional cost to cast this spell, pay X life. Draw X cards."
--
-- The card for the TURN axis of CastingRestriction.DuringPhase (CR 109.5's
-- "your"), where Rally the Troops above is the card for a window every player
-- shares.
--
-- alice holds Necrologia and a Lightning Bolt, with five Swamps and a Mountain
-- untapped and three cards in her library. The Bolt is the CONTROL on every
-- board below: an unrestricted instant in the same hand at the same moment, so a
-- board that refuses Necrologia for want of priority or mana refuses the Bolt
-- too, and the negative cases would not pass for that reason unnoticed.
necrologiaBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Phase.Phase -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
necrologiaBoard swamp mountain necrologia bolt ph =
  let (base, necrologiaId) = S.boltInHand swamp necrologia 5 ph
      (boltId, withBolt) = S.addHandCard bolt S.alice (snd (S.addCreature mountain S.alice base))
      stocked = List.foldl' (\gs _ -> snd (S.addLibraryCard swamp S.alice gs)) withBolt [1 :: Int, 2, 3]
   in (necrologiaId, boltId, stocked)

-- Announces this X for Necrologia; every other prompt takes the identity
-- fallback. CostSpec's answerHatredXOf, for the other card whose only X is a
-- CostComponent.PayLifeX.
answerNecrologiaXOf :: Natural -> Prompt.Prompt r -> r
answerNecrologiaXOf n p = case p of
  Prompt.ChooseX {} -> n
  _ -> S.identityAnswer p

necrologiaSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
necrologiaSpec s registry = Spec.describe s "Necrologia" $ do
  -- CR 512.1 / CR 513.1: the end step is a step of the ending phase, and alice
  -- is the active player, so both conjuncts hold.
  Spec.it s "CR 601.3 castable in its controller's own end step" $ do
    swamp <- S.printingOf s registry "Swamp"
    mountain <- S.printingOf s registry "Mountain"
    necrologia <- S.printingOf s registry "Necrologia"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (oid, boltId, board) = necrologiaBoard swamp mountain necrologia bolt (Phase.Ending EndingStep.EndStep)
    Spec.assertBool s (S.castable S.alice oid board) "castable"
    Spec.assertBool s (any (S.isCastOf oid) (Action.legalActions S.alice board)) "and offered as a legal action"
    Spec.assertBool s (S.castable S.alice boltId board) "the control instant is castable too"
  -- CR 109.5: the TURN half, isolated. The same board with ONE field changed --
  -- bob is the active player. alice still holds priority, still has the same
  -- five Swamps, and the game is still in an end step.
  Spec.it s "CR 109.5 the same card is NOT castable in an opponent's end step" $ do
    swamp <- S.printingOf s registry "Swamp"
    mountain <- S.printingOf s registry "Mountain"
    necrologia <- S.printingOf s registry "Necrologia"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (oid, boltId, board) = necrologiaBoard swamp mountain necrologia bolt (Phase.Ending EndingStep.EndStep)
        bobsTurn = board {GameState.activePlayer = S.bob}
    Spec.assertBool s (not (S.castable S.alice oid bobsTurn)) "TurnScope.ControllersTurn refuses it"
    Spec.assertBool s (not (any (S.isCastOf oid) (Action.legalActions S.alice bobsTurn))) "and it is not offered"
    Spec.assertBool s (S.castable S.alice boltId bobsTurn) "though the control instant still is"
  -- CR 500.1: the WINDOW half, isolated. alice's own turn, wrong phase.
  Spec.it s "CR 601.3 not castable in its controller's precombat main phase" $ do
    swamp <- S.printingOf s registry "Swamp"
    mountain <- S.printingOf s registry "Mountain"
    necrologia <- S.printingOf s registry "Necrologia"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (oid, boltId, board) = necrologiaBoard swamp mountain necrologia bolt Phase.PrecombatMain
    Spec.assertBool s (not (S.castable S.alice oid board)) "the end-step window is closed"
    Spec.assertBool s (S.castable S.alice boltId board) "though the control instant is castable"
  -- CR 512.1: the cleanup step is the OTHER step of the same phase, so a reader
  -- comparing PhaseSelector.EndingPhase rather than the step would admit it.
  -- This is the case that keeps Turn.inWindow's containment honest for a Step.
  Spec.it s "CR 512.1 not castable in the cleanup step of the same phase" $ do
    swamp <- S.printingOf s registry "Swamp"
    mountain <- S.printingOf s registry "Mountain"
    necrologia <- S.printingOf s registry "Necrologia"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (oid, boltId, board) = necrologiaBoard swamp mountain necrologia bolt (Phase.Ending EndingStep.Cleanup)
    Spec.assertBool s (not (S.castable S.alice oid board)) "a Step window names one step"
    Spec.assertBool s (S.castable S.alice boltId board) "though the control instant is castable"
  -- Gameplay level, through the stack: the permitted cast resolves, CR 119.4
  -- takes the announced life and CR 121.3 draws that many, so the gate admits a
  -- card that then plays. Falsifiers: an X read as 0 leaves 20 life and one card
  -- drawn short of nothing; an X paid but not read back leaves 18 life and no
  -- draw.
  Spec.it s "CR 601.2b/107.3a the permitted cast pays 2 life and draws 2" $ do
    swamp <- S.printingOf s registry "Swamp"
    mountain <- S.printingOf s registry "Mountain"
    necrologia <- S.printingOf s registry "Necrologia"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (oid, _, board) = necrologiaBoard swamp mountain necrologia bolt (Phase.Ending EndingStep.EndStep)
        after = S.runPure (answerNecrologiaXOf 2) board (do S.cast S.alice oid; Stack.resolveTop)
    Spec.assertEqWith s "CR 119.4 subtracted the announced 2" (S.lifeOf S.alice after) (Just 18)
    -- One Bolt left in hand plus the two drawn; Necrologia itself has left it.
    Spec.assertEqWith s "two cards drawn" (S.handSize S.alice after) 3
    Spec.assertEqWith s "and the library is two shorter" (length (Game.zoneMembers Zone.Library S.alice after)) 1

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
--
-- The CAST half of rule 702.8a's "you may play this card". The other half is CR
-- 116.2a's land play, which CR 601.1a makes the same sentence reach: those cases
-- are Pawl.GameSpec's Action group, where Teferi grants flash to Dryad Arbor in
-- a hand and the gate is Action.legalActions rather than Cast.timingOk.
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
    Spec.assertBool s (elem (A.Cast cheetahId (S.printingName pouncingCheetah) Facing.FaceUp) (Action.legalActions S.alice bobsTurn)) "and offered as a legal action"
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
  -- Cast.instantSpeed reads the CR 613 projection, and this is the case that says
  -- the printed keyword still reaches it: a card whose flash is printed rather
  -- than granted is castable on the same board.
  --
  -- Humility is why the projection is the RIGHT reader rather than a coincidence
  -- here: CR 109.2 makes its "all creatures" mean permanents on the battlefield,
  -- and a card in a hand is not one of them, so the window stays open and the
  -- projection says so.
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

  -- CR 613.1f layer 6 over a card in a HAND. Teferi, Mage of Zhalfir's "creature
  -- cards you own that aren't on the battlefield have flash" is the pool's first
  -- effect to change a card's keywords while it sits in a hand, which is what
  -- makes Cast.instantSpeed's projected read observable at all.
  --
  -- A PAIR of boards differing in exactly one thing: whether Teferi is on the
  -- battlefield. Same four Forests, same Mammoth in the same hand, same seat
  -- active, and S.castAnswer takes whatever cast it is OFFERED -- so on the bare
  -- board alice is offered none and passes.
  --
  -- Three readings the pair separates. War Mammoth prints no flash, so "the card
  -- always had it" would put it onto the battlefield on the bare board too. It is
  -- in the hand before Teferi arrives, so "the effect applied as it was drawn"
  -- puts it there on neither. Only an effect applying to a card SITTING in a hand
  -- puts it there on exactly one.
  --
  -- Not implemented, so the card file omits it: Teferi's third clause, "each
  -- opponent can cast spells only any time they could cast a sorcery" (#1860).
  -- Bob casts nothing here, so nothing below turns on it.
  Spec.it s "CR 702.8a/613.1f Teferi gives a creature card in hand flash, and it is cast on bob's turn" $ do
    forest <- S.printingOf s registry "Forest"
    warMammoth <- S.printingOf s registry "War Mammoth"
    teferi <- S.printingOf s registry "Teferi, Mage of Zhalfir"
    let (mammothId, bare) = teferiBoard forest warMammoth Nothing
        withTeferi = snd (teferiBoard forest warMammoth (Just teferi))
        play gs = S.runPure S.castAnswer gs Engine.priorityLoop
        after = play withTeferi
    Spec.assertEqWith s "the Mammoth is on the battlefield" (S.countOnBattlefieldByName (S.printingName warMammoth) S.alice after) 1
    Spec.assertEqWith s "and without Teferi it never left her hand" (S.countOnBattlefieldByName (S.printingName warMammoth) S.alice (play bare)) 0
    Spec.assertEqWith s "bob was the active player throughout" (GameState.activePlayer after) S.bob
    Spec.assertBool s (Projection.hasKeyword Keyword.Flash mammothId withTeferi) "the card in hand projects flash"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flash mammothId bare)) "and does not without Teferi"

  -- The arm's own gate, read from the other side. Teferi's set is
  -- Affected.MatchingOffBattlefield, so a creature ON the battlefield is outside
  -- it -- which is the whole difference between that arm and MatchingAnywhere,
  -- and CR 702.8a is a permission to PLAY a card, so nothing else on the board
  -- would show it.
  --
  -- TWO War Mammoths on ONE board differing in exactly one thing: which zone each
  -- is in. Same printing, same owner, same Teferi, so a set that ignored the zone
  -- would answer alike for both.
  Spec.it s "CR 613.1f Teferi's off-battlefield set reaches the Mammoth in hand and not the one in play" $ do
    forest <- S.printingOf s registry "Forest"
    warMammoth <- S.printingOf s registry "War Mammoth"
    teferi <- S.printingOf s registry "Teferi, Mage of Zhalfir"
    let (inHand, gs0) = teferiBoard forest warMammoth (Just teferi)
        (inPlay, gs) = S.addCreature warMammoth S.alice gs0
    Spec.assertBool s (Projection.hasKeyword Keyword.Flash inHand gs) "the one in hand has flash"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flash inPlay gs)) "the one on the battlefield does not"

-- Four Forests and a War Mammoth in alice's hand, on BOB's turn with alice
-- holding priority, and Teferi on the battlefield or not. The Mammoth is {3}{G},
-- which the four Forests pay exactly, so affordability is identical either way
-- and the only thing that varies is the printing this takes.
teferiBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Maybe Printing.Printing ->
  (ObjectId.ObjectId, GameState.GameState)
teferiBoard forest warMammoth mTeferi =
  let (gs0, mammothId) = S.handOne warMammoth (S.landsInPlay forest 4)
      gs1 = maybe gs0 (\teferi -> snd (S.addCreature teferi S.alice gs0)) mTeferi
   in ( mammothId,
        gs1
          { GameState.activePlayer = S.bob,
            GameState.priority = Just S.alice
          }
      )

-- The two names Wax // Wane prints (CR 709.4a). Neither of them is "Wax//Wane",
-- which is the combined view's stand-in and not a name the card has.
waxName, waneName :: CardName.CardName
waxName = CardName.MkCardName (Text.pack "Wax")
waneName = CardName.MkCardName (Text.pack "Wane")

onwardName, victoryName :: CardName.CardName
onwardName = CardName.MkCardName (Text.pack "Onward")
victoryName = CardName.MkCardName (Text.pack "Victory")

-- The two halves of
-- data/cards/synthetic-glacial-half-synthetic-volcanic-half.json, a split card
-- printing Panglacial Wurm's permission on each half. Synthetic because no
-- printing carries that permission on a multi-face card: an api.scryfall.com
-- sweep with include_extras for o:"searching your library" and
-- oracle:/you may cast .* from your library/ returns Panglacial Wurm and
-- Infernal Spawn of Infernal Spawn of Evil, both single-faced.
glacialHalf, volcanicHalf :: CardName.CardName
glacialHalf = CardName.MkCardName (Text.pack "Synthetic Glacial Half")
volcanicHalf = CardName.MkCardName (Text.pack "Synthetic Volcanic Half")

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
  -- before putting it onto the stack."
  --
  -- Falsifier: an engine pricing the cast from CR 709.4b's COMBINED {G}{W}
  -- fails here. One Forest cannot pay it, so no candidate survives
  -- castProposed's payability filter, the cast rewinds (CR 601.2e), and the
  -- Piker stays a 2/1.
  --
  -- What this case does NOT catch, despite the shape suggesting it: an engine
  -- resolving the combined view's PAYLOAD. Pawl.Engine.Card.merge2 deliberately
  -- leaves Face.spell as the left half's, so the combined view carries Wax's
  -- effect and would pass. Nor does it catch one that always casts the first
  -- face, which Wax already is. The Wane case below is what discriminates
  -- against both.
  Spec.it s "CR 709.3 casting Wax gives the targeted creature +2/+2" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    waxWane <- S.printingOf s registry "Wax"
    let (pikerId, withPiker) = S.addCreature piker S.alice (S.landsInPlay forest 1)
        (gs, oid) = S.handOne waxWane withPiker
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid waxName Facing.FaceUp))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "the 2/1 Piker is a 4/3" (S.powerToughnessOf pikerId resolved) (Just (4, 3))
  -- The half that carries the weight. One Plains, an enchantment, and no
  -- creature at all -- so three broken engines fail here, two of which the Wax
  -- case above lets through:
  --
  --   * one that always casts the FIRST face resolves Wax, which has no legal
  --     target on a creatureless board;
  --   * one that resolves the COMBINED view's payload finds the same, since
  --     Pawl.Engine.Card.merge2 keeps Face.spell as the left half's -- Wax's;
  --   * one that prices from the combined {G}{W} cannot pay it with a Plains.
  --
  -- Each rewinds the cast (CR 601.2e) and leaves the Prison standing.
  Spec.it s "CR 709.3 casting Wane destroys the targeted enchantment" $ do
    plains <- S.printingOf s registry "Plains"
    ghostlyPrison <- S.printingOf s registry "Ghostly Prison"
    waxWane <- S.printingOf s registry "Wane"
    let (prisonId, withPrison) = S.addCreature ghostlyPrison S.alice (S.landsInPlay plains 1)
        (gs, oid) = S.handOne waxWane withPrison
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid waneName Facing.FaceUp))
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
    waxWane <- S.printingOf s registry "Wax"
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
    waxWane <- S.printingOf s registry "Wax"
    let (_, withPiker) = S.addCreature piker S.alice (S.landsInPlay forest 1)
        (gs, oid) = S.handOne waxWane withPiker
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid waxName Facing.FaceUp))
    -- CR 709.4a: "Each split card has two names." BOTH of them, asked one at a
    -- time, and the "Wax//Wane" the combined Face renders them as is not among
    -- them -- that string is how the CR's own Examples write the pair and not a
    -- name the card has.
    Spec.assertEqWith s "in hand, the combined view has both names" (Projection.namesOf oid gs) (Set.fromList [waxName, waneName])
    Spec.assertEqWith s "and has Wax" (Projection.hasName waxName oid gs) True
    Spec.assertEqWith s "and has Wane" (Projection.hasName waneName oid gs) True
    Spec.assertEqWith s "and does NOT have the two joined" (Projection.hasName (CardName.MkCardName (Text.pack "Wax//Wane")) oid gs) False
    case GameState.stack cast of
      [] -> Spec.assertFailure s "expected the spell on the stack"
      top : _ -> do
        -- CR 709.3b narrows the pair to one: the half being cast.
        Spec.assertEqWith s "on the stack, the half being cast" (Projection.namesOf top cast) (Set.singleton waxName)
        Spec.assertEqWith s "so the OTHER half's name is not one of the spell's" (Projection.hasName waneName top cast) False
  Spec.it s "CR 709.3a each half is offered and gated on its own" $ do
    waxWane <- S.printingOf s registry "Wax"
    forest <- S.printingOf s registry "Forest"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    ghostlyPrison <- S.printingOf s registry "Ghostly Prison"
    -- Both halves need a legal target, or targeting gates them BOTH out and the
    -- offered list is empty for a reason that has nothing to do with mana. Wax
    -- wants a creature; Wane wants an enchantment.
    let targets g = snd (S.addCreature ghostlyPrison S.alice (snd (S.addCreature piker S.alice g)))
        namesOffered gs = [n | A.Cast _ n _ <- Action.legalActions S.alice gs]
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
  -- The observer for CR 709.3a's half being part of the CR 601.2a MOVE rather
  -- than a stamp applied once the move has landed.
  --
  -- Synthetic Stack Interdiction is "If a green card would be put onto the stack,
  -- exile it instead" -- a CR 614.1a replacement of the one zone change CR 601.2a
  -- makes. Nothing printed watches that event: a sweep of Scryfall's oracle bulk
  -- data (38,542 cards) for "onto the stack" returns two, and neither replaces
  -- anything -- Grip of Chaos is a TRIGGER on the same moment, and Ertai's
  -- Meddling only says where a delayed card goes. So the redirect is synthetic
  -- while the split card it reads is a printing.
  --
  -- Two readings of the same card, one case:
  --
  --   * BEFORE the move, CR 616.1 asks the pattern about the card as it still
  --     sits in the hand, where CR 709.4 gives it both halves combined. The half
  --     being cast is WANE, which is white; the pattern names GREEN, which only
  --     Wax is. An engine reading CR 709.3b's chosen half here would find no
  --     green card, decline the redirect, and leave Wane on the stack.
  --   * AFTER it, the card is in EXILE and was never put onto the stack at all,
  --     so CR 709.3a ("only that half is considered to be put onto the stack")
  --     has nothing to say about it and CR 709.4's combined view is what it
  --     shows -- both names, and the colours of the combined mana cost (CR
  --     709.4b).
  --
  -- CR 614.6 is what makes the second reading follow from the first: the
  -- modified event is the event that happens, so the destination the CR 616.1
  -- loop settled on is the destination the move must answer for. Only a writer
  -- INSIDE the move can, which is what this proves -- restoring the pre-#781
  -- ordering (Event.changeZoneAttaching setting no face, Cast.castSpell stamping
  -- the chosen half onto whatever the move handed back) exiles a card named
  -- "Wane" whose only colour is white, and fails this case.
  Spec.it s "CR 709.4 a cast redirected off the stack keeps both halves" $ do
    plains <- S.printingOf s registry "Plains"
    ghostlyPrison <- S.printingOf s registry "Ghostly Prison"
    interdiction <- S.printingOf s registry "Synthetic Stack Interdiction"
    waxWane <- S.printingOf s registry "Wax"
    -- The Prison is Wane's target (CR 601.2c), so the cast does not rewind for
    -- want of one before it ever reaches the move.
    let (_, withPrison) = S.addCreature ghostlyPrison S.alice (S.landsInPlay plains 1)
        (_, withInterdiction) = S.addCreature interdiction S.alice withPrison
        (gs, oid) = S.handOne waxWane withInterdiction
        after = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid waneName Facing.FaceUp))
    -- Not asserted: what CR 601.2b-i do afterwards. castSpell announces, prices
    -- and pays for a spell the redirect has already moved off the stack, and
    -- which of the two defensible readings is right is not implemented (#816).
    Spec.assertEqWith s "the redirect fired, so nothing reached the stack" (length (GameState.stack after)) 0
    case Game.zoneMembers Zone.Exile S.alice after of
      [exiled] -> do
        Spec.assertEqWith s "in exile, CR 709.4's combined view has both names" (Projection.namesOf exiled after) (Set.fromList [waxName, waneName])
        Spec.assertEqWith s "and CR 709.4b's combined colours" (Projection.colorsOf exiled after) (Set.fromList [Color.Green, Color.White])
      _ -> Spec.assertFailure s "expected the redirected card in exile"

-- Victor Mancha, Runaway {5} Legendary Artifact Creature -- Human Hero 4/4:
-- "When Victor Mancha enters, exile target card from your graveyard. You may
-- play it for as long as you control Victor Mancha." The pool's only
-- Effect-granted permission to play a card from exile (CR 601.3), and the only
-- one of either kind with a STATED duration -- CR 715.3d's states none, so
-- Pawl.AdventureSpec cannot reach the sweep this group exercises.
--
-- Its target slot names CardsInGraveyard with no filter, so the permitted card
-- may be a LAND -- which is played and never cast (CR 305.1), and is the last
-- two cases here.
victorName, benalishHeroName, swampName :: CardName.CardName
victorName = CardName.MkCardName (Text.pack "Victor Mancha, Runaway")
benalishHeroName = CardName.MkCardName (Text.pack "Benalish Hero")
swampName = CardName.MkCardName (Text.pack "Swamp")

-- The adventurer card the last case below permits out of exile. Named here
-- rather than imported from Pawl.AdventureSpec, which is the module's own
-- convention for every other card name in this file.
shieldbreakerName, battleDisplayName :: CardName.CardName
shieldbreakerName = CardName.MkCardName (Text.pack "Embereth Shieldbreaker")
battleDisplayName = CardName.MkCardName (Text.pack "Battle Display")

-- The battlefield objects answering to a name -- how a test reaches the
-- permanent a cast produced, whose id is neither the card's in hand nor the
-- spell's on the stack (CR 400.7).
namedOnBattlefield :: CardName.CardName -> GameState.GameState -> [ObjectId.ObjectId]
namedOnBattlefield name gs = filter (\o -> Projection.hasName name o gs) (Set.toList (GameState.battlefield gs))

-- Six Plains, `victim` in alice's graveyard, and Victor Mancha cast out of her
-- hand and resolved, with his ETB waiting on the stack.
--
-- FIVE Plains pay Victor's {5} and the SIXTH is left untapped. That is the whole
-- answer to the cast-gate vacuity trap: every assertion below about the
-- graveyard card being castable or not is made on a board where the {W} for the
-- Benalish Hero is untapped and the window is the same precombat main phase, so
-- a missing offer is about the permission and can be about nothing else.
--
-- `victim` is the ONLY card in alice's graveyard, so CR 603.3d's target choice is
-- forced and S.identityAnswer suffices. The cast cases pass a Benalish Hero, a
-- 1/1 whose only text is banding, so nothing it prints can be the reason a cast
-- succeeds or fails; the land-play cases pass a Swamp, which is the same board
-- with a card type the cast path cannot serve.
--
-- Returns the victim's graveyard id, the battlefield objects named Victor Mancha
-- (a list, so a case that removes him can assert it emptied), and the state.
victorTriggered :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
victorTriggered plains victor victim =
  let (heroId, board) = S.addGraveyardCard victim S.alice (S.landsInPlay plains 6)
      (gs, victorCard) = S.handOne victor board
      cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice victorCard victorName Facing.FaceUp)
      entered = S.runPure S.identityAnswer cast Stack.resolveTop
      -- CR 603.3b: the enters trigger goes onto the stack the next time a player
      -- would receive priority.
      placed = S.runPure S.identityAnswer entered Engine.settleForPriority
   in (heroId, namedOnBattlefield victorName placed, placed)

-- Whether alice is offered the cast of this object under this name.
offeredCast :: ObjectId.ObjectId -> CardName.CardName -> GameState.GameState -> Bool
offeredCast oid name gs = elem (A.Cast oid name Facing.FaceUp) (Action.legalActions S.alice gs)

-- The land plays this player is offered, in the engine's own order. A list
-- rather than a membership test, so a negative below is read off something that
-- is never empty (see the land-play case's board).
offeredPlays :: PlayerId.PlayerId -> GameState.GameState -> [A.Action]
offeredPlays pid gs =
  let isPlay action = case action of
        A.Play {} -> True
        A.Pass -> False
        A.Cast {} -> False
        A.Activate _ _ -> False
        A.TurnFaceUp {} -> False
        A.Unlock _ _ -> False
        A.DiscardFromHand _ -> False
        A.Plot _ -> False
        A.Foretell _ -> False
        A.Ignore _ -> False
        A.ActivateManaAbility _ -> False
   in filter isPlay (Action.legalActions pid gs)

-- The board victorTriggered leaves, moved to alice's precombat main phase with
-- the stack empty: CR 305.1's window, which that fixture's untap step is not.
inHerMainPhase :: GameState.GameState -> GameState.GameState
inHerMainPhase gs =
  gs
    { GameState.phase = Phase.PrecombatMain,
      GameState.activePlayer = S.alice,
      GameState.priority = Just S.alice
    }

victorManchaSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
victorManchaSpec s registry = Spec.describe s "VictorMancha" $ do
  -- CR 608.2c: two instructions in one clause, in written order -- the exile
  -- happens and the permission is written over the incarnation CR 400.7 minted
  -- at the destination, which is the slot the move bound.
  Spec.it s "CR 601.3 the exiled card becomes castable from exile, and only by the permitted player" $ do
    plains <- S.printingOf s registry "Plains"
    victor <- S.printingOf s registry "Victor Mancha, Runaway"
    hero <- S.printingOf s registry "Benalish Hero"
    let (heroId, victors, placed) = victorTriggered plains victor hero
        after = S.runPure S.identityAnswer placed Stack.resolveTop
    Spec.assertEqWith s "Victor is on the battlefield" (length victors) 1
    Spec.assertEqWith s "the graveyard is empty" (Game.zoneMembers Zone.Graveyard S.alice after) []
    case Game.zoneMembers Zone.Exile S.alice after of
      [exiledId] -> do
        Spec.assertBool s (exiledId /= heroId) "CR 400.7: exile holds a new incarnation"
        -- The ACTION LIST, not the field: a field read would pass against an
        -- engine where Pawl.Engine.Cast never consulted the permission at all.
        Spec.assertBool s (offeredCast exiledId benalishHeroName after) "alice is offered the cast from exile"
        -- Five Plains paid Victor's {5}; the sixth is still untapped, which is
        -- what the negative cases below rest on.
        Spec.assertEqWith s "one Plains is still untapped" (S.tappedCount S.alice after) 5
        case Game.faceOf exiledId after of
          Nothing -> Spec.assertFailure s "expected a face on the exiled card"
          Just face -> do
            -- CR 109.5: the permission names the ability's controller and no one
            -- else. Asked of permitsCastFromExile directly, because bob's cast
            -- would be refused for two further reasons on this board -- he has no
            -- mana, and Game.zoneMembers files exile by OWNER so he cannot reach
            -- alice's card at all (#668) -- so a gameplay-level negative here
            -- would be over-determined and would discriminate nothing.
            Spec.assertBool s (Cast.permitsCastFromExile S.alice exiledId face after) "alice is permitted"
            Spec.assertBool s (not (Cast.permitsCastFromExile S.bob exiledId face after)) "bob is not"
      other -> Spec.assertFailure s ("expected exactly one exiled card, got " <> show (length other))
  -- CR 611.2b's "for as long as": the duration ends when its condition stops
  -- holding, and the permission goes with it. The SAME board and the SAME step as
  -- the case above, differing only in whether Victor is on the battlefield.
  Spec.it s "CR 611.2b the permission ends when its source leaves, and the card stays in exile" $ do
    plains <- S.printingOf s registry "Plains"
    victor <- S.printingOf s registry "Victor Mancha, Runaway"
    hero <- S.printingOf s registry "Benalish Hero"
    let (_, victors, placed) = victorTriggered plains victor hero
        after = S.runPure S.identityAnswer placed Stack.resolveTop
        dead = S.runPure S.identityAnswer after (Event.destroy Regenerability.Regenerable victors)
        swept = S.runPure S.identityAnswer dead Engine.settleForPriority
    case Game.zoneMembers Zone.Exile S.alice after of
      [exiledId] -> do
        Spec.assertBool s (offeredCast exiledId benalishHeroName after) "offered while Victor stands"
        Spec.assertEqWith s "Victor is gone" (namedOnBattlefield victorName swept) []
        -- Both halves matter. Still in exile says the offer's disappearance is
        -- the permission ending and not the card moving, which CR 400.7 would
        -- have ended for free.
        Spec.assertBool s (elem exiledId (Game.zoneMembers Zone.Exile S.alice swept)) "the card is still in exile"
        Spec.assertBool s (not (offeredCast exiledId benalishHeroName swept)) "and no longer offered"
        -- The mana did not move either, so the absent offer is not about cost.
        Spec.assertEqWith s "the same Plains is still untapped" (S.tappedCount S.alice swept) 5
      other -> Spec.assertFailure s ("expected exactly one exiled card, got " <> show (length other))
  -- CR 611.2b's first sentence: "if the 'for as long as' duration never starts,
  -- the effect does nothing". Victor dies while his own trigger is on the stack,
  -- so the trigger still resolves (its target is legal, CR 608.2b) and still
  -- exiles the card -- but no permission is ever written.
  --
  -- The discriminator a sweep cannot launder: this state is reached without any
  -- sweep running over a stored permission, so a green result here means nothing
  -- was ever stored rather than that nothing survived.
  Spec.it s "CR 611.2b a duration that never starts stores no permission, and the exile still happens" $ do
    plains <- S.printingOf s registry "Plains"
    victor <- S.printingOf s registry "Victor Mancha, Runaway"
    hero <- S.printingOf s registry "Benalish Hero"
    let (_, victors, placed) = victorTriggered plains victor hero
        dead = S.runPure S.identityAnswer placed (Event.destroy Regenerability.Regenerable victors)
        after = S.runPure S.identityAnswer dead Stack.resolveTop
    Spec.assertBool s (not (null (GameState.stack placed))) "the trigger really was on the stack"
    Spec.assertEqWith s "Victor died before it resolved" (namedOnBattlefield victorName after) []
    case filter (\o -> Projection.hasName benalishHeroName o after) (Game.zoneMembers Zone.Exile S.alice after) of
      [exiledId] -> do
        Spec.assertEqWith
          s
          "the card was exiled with no permission on it"
          (fmap Object.playableFromExile (Game.lookupObject exiledId after))
          (Just Nothing)
        Spec.assertBool s (not (offeredCast exiledId benalishHeroName after)) "and it is not castable from exile"
      other -> Spec.assertFailure s ("expected exactly one exiled Hero, got " <> show (length other))
  -- CR 715.3d's verb is PLAY, and CR 305.1 makes playing a land a special action
  -- rather than a cast (CR 601.1 is the rule that "play" once meant casting).
  -- Victor's target slot is CardsInGraveyard with no filter, so a land card is a
  -- legal target and this is the board that follows.
  --
  -- The SAME fixture with a Swamp where the Hero was, and a Mountain added to
  -- each seat's hand: every list below therefore holds that seat's own land
  -- play whatever the permission does, so an absent Swamp is the permission and
  -- not an empty menu.
  Spec.it s "CR 305.1 an exiled LAND is offered as a land play, and only to the permitted player" $ do
    plains <- S.printingOf s registry "Plains"
    victor <- S.printingOf s registry "Victor Mancha, Runaway"
    swamp <- S.printingOf s registry "Swamp"
    mountain <- S.printingOf s registry "Mountain"
    let (_, _, placed) = victorTriggered plains victor swamp
        resolved = S.runPure S.identityAnswer placed Stack.resolveTop
        (herMountain, withHers) = S.addHandCard mountain S.alice (inHerMainPhase resolved)
        (hisMountain, after) = S.addHandCard mountain S.bob withHers
        bobsTurn = after {GameState.activePlayer = S.bob, GameState.priority = Just S.bob}
    case Game.zoneMembers Zone.Exile S.alice after of
      [exiledId] -> do
        Spec.assertEqWith
          s
          "her hand's Mountain and the exiled Swamp, in that order"
          (offeredPlays S.alice after)
          [A.Play herMountain Nothing, A.Play exiledId Nothing]
        -- The permission is to PLAY it, and a land has no castable face, so the
        -- cast path offers it nothing. Both readings of "play" would pass the
        -- assertion above; only this one separates them.
        Spec.assertBool s (not (offeredCast exiledId swampName after)) "and it is not offered as a cast"
        -- CR 109.5 again: exile is a SHARED zone, so bob can reach the object --
        -- what stops him is that the permission names alice.
        Spec.assertEqWith s "bob is offered his own hand and nothing else" (offeredPlays S.bob bobsTurn) [A.Play hisMountain Nothing]
      other -> Spec.assertFailure s ("expected exactly one exiled card, got " <> show (length other))
  -- The negative of the pair, and the same one the cast side takes above: Victor
  -- leaves, CR 611.2b's duration ends, and the same board with the same Swamp in
  -- the same zone offers nothing but the hand.
  Spec.it s "CR 611.2b the land play goes with the permission" $ do
    plains <- S.printingOf s registry "Plains"
    victor <- S.printingOf s registry "Victor Mancha, Runaway"
    swamp <- S.printingOf s registry "Swamp"
    mountain <- S.printingOf s registry "Mountain"
    let (_, victors, placed) = victorTriggered plains victor swamp
        resolved = S.runPure S.identityAnswer placed Stack.resolveTop
        (herMountain, after) = S.addHandCard mountain S.alice (inHerMainPhase resolved)
        dead = S.runPure S.identityAnswer after (Event.destroy Regenerability.Regenerable victors)
        swept = S.runPure S.identityAnswer dead Engine.settleForPriority
    case Game.zoneMembers Zone.Exile S.alice after of
      [exiledId] -> do
        Spec.assertBool s (elem (A.Play exiledId Nothing) (offeredPlays S.alice after)) "offered while Victor stands"
        Spec.assertBool s (elem exiledId (Game.zoneMembers Zone.Exile S.alice swept)) "the Swamp is still in exile"
        Spec.assertEqWith s "and only her hand is offered now" (offeredPlays S.alice swept) [A.Play herMountain Nothing]
      other -> Spec.assertFailure s ("expected exactly one exiled card, got " <> show (length other))
  -- CR 715.3d's closing clause: "it can't be cast as an Adventure THIS WAY,
  -- although other effects that allow a player to cast it may allow a player to
  -- cast it as an Adventure". Victor is one of those other effects, so the
  -- adventurer card he exiles offers BOTH halves -- CR 715.3 having the player
  -- choose between them wherever the card is playable.
  --
  -- The paired negative is Pawl.AdventureSpec's "CR 715.3d from exile the
  -- creature is castable and the Adventure is not": the SAME card, in exile, in
  -- a sorcery window, asked of by the same player -- differing only in which
  -- rule wrote the permission. Neither board alone tells "the origin is read"
  -- from "the exclusion was dropped".
  Spec.it s "CR 715.3d another effect's permission allows the Adventure half" $ do
    mountain <- S.printingOf s registry "Mountain"
    victor <- S.printingOf s registry "Victor Mancha, Runaway"
    shieldbreaker <- S.printingOf s registry "Embereth Shieldbreaker"
    let (_, victors, placed) = victorTriggered mountain victor shieldbreaker
        resolved = S.runPure S.identityAnswer placed Stack.resolveTop
        -- Mountains rather than the fixture's Plains, and a SEVENTH added: five
        -- paid Victor's {5}, Battle Display's {R} comes off the sixth, and the
        -- creature half's {1}{R} needs the seventh. Both halves are therefore
        -- affordable, so a missing offer of either is about the permission --
        -- Cast.castable gates on payability, which is the trap this dodges.
        after = S.landsFor mountain S.alice 1 resolved
    -- Battle Display targets an artifact, and Victor is a Legendary ARTIFACT
    -- Creature standing on this board -- so CR 601.2c is satisfied and cannot be
    -- the reason for an absent offer either.
    Spec.assertEqWith s "Victor is on the battlefield, and is the artifact Battle Display can target" (length victors) 1
    case Game.zoneMembers Zone.Exile S.alice after of
      [exiledId] -> do
        Spec.assertEqWith s "two Mountains untapped, so neither half is priced out" (S.tappedCount S.alice after) 5
        Spec.assertBool s (offeredCast exiledId shieldbreakerName after) "the creature half is offered, as it would be under CR 715.3d's own permission too"
        Spec.assertBool s (offeredCast exiledId battleDisplayName after) "and so is the Adventure half, which CR 715.3d's own permission would refuse"
      other -> Spec.assertFailure s ("expected exactly one exiled card, got " <> show (length other))

-- Dire Fleet Daredevil {1}{R} Creature -- Human Pirate 2/1: "First strike. When
-- this creature enters, exile target instant or sorcery card from an opponent's
-- graveyard. You may cast it this turn ..." The pool's only card that lets a
-- player cast a card somebody else OWNS, which is the one board where CR 405.4's
-- controller and CR 108.3's owner name different players (#83).
--
-- Both riders are expressed. "If that spell would be put into a graveyard, exile
-- it instead" is a floating CR 614.1a redirect whose pattern names the object
-- the ability's own MoveToZone bound, and CR 400.7h is what carries that name
-- from the exiled card to the spell it becomes; the case below proves it.
--
-- The other rider: "and mana of any type can be spent to cast that
-- spell" is CR 118.14, carried by the grant as ManaSpending.AnyType, and the two
-- cases at the end of this group are what prove it.
daredevilName, renewedFaithName :: CardName.CardName
daredevilName = CardName.MkCardName (Text.pack "Dire Fleet Daredevil")
renewedFaithName = CardName.MkCardName (Text.pack "Renewed Faith")

-- Three seats, because "its controller" and "its owner" collapse onto the two
-- seats of a duel: carol holds nothing and is asked for nothing, so an engine
-- that credited the spell to any player but alice is visible whichever wrong
-- player it picked.
--
-- alice holds the Daredevil and five lands -- two Mountains for its {1}{R} and
-- three Plains for Renewed Faith's {2}{W}. Two Mountains is the whole answer to
-- the cast-gate vacuity trap: {R} can only come from a Mountain, so the worst
-- payment for the Daredevil leaves a Mountain and two Plains, which pays {2}{W}
-- on any split. bob's graveyard holds Renewed Faith ({2}{W} Instant, "You gain 6
-- life") and nothing else, so CR 603.3d's target choice is forced and
-- S.identityAnswer cannot re-find a different one after a mutation.
--
-- Returns the board with the Daredevil's enters trigger already resolved: the
-- Faith exiled, and alice permitted to cast it until end of turn.
daredevilExiled :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> GameState.GameState
daredevilExiled mountain plains daredevil faith =
  let lands = S.landsFor plains S.alice 3 (S.landsFor mountain S.alice 2 S.threePlayerGame)
      (_, stocked) = S.addGraveyardCard faith S.bob lands
      (handId, board) = S.addHandCard daredevil S.alice stocked
      cast = S.runPure S.identityAnswer board (Cast.castSpell S.alice handId daredevilName Facing.FaceUp)
      entered = S.runPure S.identityAnswer cast Stack.resolveTop
      -- CR 603.3b/603.3d: the enters trigger goes onto the stack the next time a
      -- player would receive priority, and its target is chosen there.
      placed = S.runPure S.identityAnswer entered Engine.settleForPriority
   in S.runPure S.identityAnswer placed Stack.resolveTop

-- The one object named `name` in the shared exile zone. Not Game.zoneMembers,
-- which files exile by OWNER -- the whole point of this board is that the owner
-- is not the player doing anything with the card.
exiledNamed :: CardName.CardName -> GameState.GameState -> [ObjectId.ObjectId]
exiledNamed name gs = filter (\o -> Projection.hasName name o gs) (Set.toList (GameState.exile gs))

-- daredevilExiled with the Faith cast and still on the stack: alice's SPELL off
-- bob's CARD. Exported because Pawl.DepartureSpec wants the same board -- CR
-- 800.4a's fourth clause is about an object whose controller is not its owner,
-- and this is the only one in the pool that is on the stack.
daredevilFaithCast :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> GameState.GameState
daredevilFaithCast mountain plains daredevil faith =
  let exiled = daredevilExiled mountain plains daredevil faith
   in case exiledNamed renewedFaithName exiled of
        [exiledId] -> S.runPure S.identityAnswer exiled (Cast.castSpell S.alice exiledId renewedFaithName Facing.FaceUp)
        -- Left to the caller's own assertion about the stack: a board that never
        -- exiled the card is one where nothing was cast either.
        _ -> exiled

-- daredevilExiled's board with the white mana taken away and a SECOND Renewed
-- Faith put into alice's hand -- CR 118.14's board, and the pair the negative
-- rests on.
--
-- alice's five lands are all Mountains: two pay the Daredevil's {1}{R} and the
-- three that are left are the only mana she has for a {2}{W}. Nothing on this
-- board can make white (asserted, rather than assumed, in the case below), so
-- the exiled Faith is payable only under the permission's rider and the copy in
-- her hand is payable not at all.
--
-- THE PAIR IS ON ONE BOARD, which is what makes it a pair: the two Faiths are
-- the same card at the same cost, held by the same player, in the same step with
-- the same empty stack, and both are instants so CR 117.1a permits either at
-- this moment. The single difference is that one of them is being cast under CR
-- 118.14's permission. Returns the exiled card's id, the hand copy's, and the
-- board.
daredevilRedOnly :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (Maybe ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
daredevilRedOnly mountain daredevil faith =
  let lands = S.landsFor mountain S.alice 5 S.threePlayerGame
      (_, stocked) = S.addGraveyardCard faith S.bob lands
      (handFaithId, withFaith) = S.addHandCard faith S.alice stocked
      (handId, board) = S.addHandCard daredevil S.alice withFaith
      cast = S.runPure S.identityAnswer board (Cast.castSpell S.alice handId daredevilName Facing.FaceUp)
      entered = S.runPure S.identityAnswer cast Stack.resolveTop
      placed = S.runPure S.identityAnswer entered Engine.settleForPriority
      after = S.runPure S.identityAnswer placed Stack.resolveTop
   in (Maybe.listToMaybe (exiledNamed renewedFaithName after), handFaithId, after)

-- daredevilExiled's board with a SECOND Renewed Faith in alice's hand and enough
-- lands to cast both: nine, since the Daredevil takes two and each Faith takes
-- three. Three Mountains, so the worst payment for its {1}{R} still leaves six
-- Plains for two {2}{W}. Returns the hand copy's id and the board.
--
-- The hand copy is what makes the exiled card's exile-instead a claim about ONE
-- OBJECT rather than about a card name: bob owns the exiled Faith and alice owns
-- the one in her hand, so the two also differ in which graveyard CR 608.2n would
-- reach.
daredevilTwoFaiths :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
daredevilTwoFaiths mountain plains daredevil faith =
  let lands = S.landsFor plains S.alice 6 (S.landsFor mountain S.alice 3 S.threePlayerGame)
      (_, stocked) = S.addGraveyardCard faith S.bob lands
      (handFaithId, withFaith) = S.addHandCard faith S.alice stocked
      (handId, board) = S.addHandCard daredevil S.alice withFaith
      cast = S.runPure S.identityAnswer board (Cast.castSpell S.alice handId daredevilName Facing.FaceUp)
      entered = S.runPure S.identityAnswer cast Stack.resolveTop
      placed = S.runPure S.identityAnswer entered Engine.settleForPriority
   in (handFaithId, S.runPure S.identityAnswer placed Stack.resolveTop)

-- One symbol of the colour the board cannot make, as a cost to ask canPay about.
whiteCost :: ManaCost.ManaCost
whiteCost = ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.White)]

direFleetDaredevilSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
direFleetDaredevilSpec s registry = Spec.describe s "DireFleetDaredevil" $ do
  -- CR 601.3: the permission names a player, so the search for castable cards in
  -- exile has to consult it rather than filtering the zone by owner first (#668).
  Spec.it s "CR 601.3 a player is offered a card in exile that an opponent owns" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    daredevil <- S.printingOf s registry "Dire Fleet Daredevil"
    faith <- S.printingOf s registry "Renewed Faith"
    let after = daredevilExiled mountain plains daredevil faith
    Spec.assertEqWith s "bob's graveyard is empty" (Game.zoneMembers Zone.Graveyard S.bob after) []
    case exiledNamed renewedFaithName after of
      [exiledId] -> do
        Spec.assertEqWith s "CR 108.3: bob still owns it" (fmap Object.owner (Game.lookupObject exiledId after)) (Just S.bob)
        -- The ACTION LIST, not the permission field: a field read would pass
        -- against an engine that never let alice reach the card.
        Spec.assertBool s (offeredCast exiledId renewedFaithName after) "alice is offered the cast"
        case Game.faceOf exiledId after of
          Nothing -> Spec.assertFailure s "expected a face on the exiled card"
          Just face -> do
            -- CR 109.5: the permission names the granting ability's controller
            -- and nobody else -- asked of the gate directly, because bob's cast
            -- would also fail for want of mana on this board and a gameplay-level
            -- negative would discriminate nothing.
            Spec.assertBool s (Cast.permitsCastFromExile S.alice exiledId face after) "alice is permitted"
            Spec.assertBool s (not (Cast.permitsCastFromExile S.bob exiledId face after)) "its owner is not"
      other -> Spec.assertFailure s ("expected exactly one exiled Faith, got " <> show (length other))
  -- CR 405.4: "the controller of a spell is the player who cast it". The spell
  -- says "YOU gain 6 life" (CR 109.5), so the life total that moves is the whole
  -- assertion -- and it moves on a board where the caster and the owner are
  -- different players, which is the only board the two readings disagree on.
  Spec.it s "CR 405.4 a spell cast off an opponent's card resolves under its caster" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    daredevil <- S.printingOf s registry "Dire Fleet Daredevil"
    faith <- S.printingOf s registry "Renewed Faith"
    let cast = daredevilFaithCast mountain plains daredevil faith
        after = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertBool s (not (null (GameState.stack cast))) "the Faith really was cast"
    Spec.assertEqWith s "alice cast it, so alice gains the life" (S.lifeOf S.alice after) (Just 26)
    Spec.assertEqWith s "its owner gains nothing" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "and the third seat is untouched" (S.lifeOf S.carol after) (Just 20)
    -- CR 608.2n would send the card to its OWNER's graveyard, and the card's own
    -- replacement sends it to exile instead (the case below) -- either way it
    -- lands under bob, which is what keeps the life assertion above from being
    -- readable as "owner and controller are the same player after all".
    Spec.assertEqWith s "the card ends up under bob, not under its caster" (length (Game.zoneMembers Zone.Exile S.bob after)) 1
    Spec.assertEqWith s "and alice's own zones hold none of it" (length (Game.zoneMembers Zone.Exile S.alice after)) 0
  -- CR 400.7h with CR 614.1a: "If that spell would be put into a graveyard,
  -- exile it instead". The clause names the SPELL, which CR 400.7 made a new
  -- object when the card was cast -- so the printed sentence is a claim about an
  -- id that did not exist when the ability resolved.
  --
  -- The pair is on ONE board: the same card, at the same cost, cast by the same
  -- player in the same step, differing only in whether it was cast off the
  -- Daredevil's exile or out of alice's hand.
  Spec.it s "CR 400.7h a spell cast off the exiled card is exiled as it resolves, and one from hand is not" $ do
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    daredevil <- S.printingOf s registry "Dire Fleet Daredevil"
    faith <- S.printingOf s registry "Renewed Faith"
    let (handFaithId, board) = daredevilTwoFaiths mountain plains daredevil faith
    case exiledNamed renewedFaithName board of
      [exiledId] -> do
        let cast = S.runPure S.identityAnswer board (Cast.castSpell S.alice exiledId renewedFaithName Facing.FaceUp)
            after = S.runPure S.identityAnswer cast Stack.resolveTop
        Spec.assertBool s (not (null (GameState.stack cast))) "the exiled Faith really was cast"
        Spec.assertEqWith s "and it resolved, so alice gained its 6 life" (S.lifeOf S.alice after) (Just 26)
        Spec.assertEqWith s "CR 400.7h: the SPELL was exiled, so its owner's graveyard is empty" (Game.zoneMembers Zone.Graveyard S.bob after) []
        Spec.assertEqWith s "and the card the spell became is in exile" (length (Game.zoneMembers Zone.Exile S.bob after)) 1
        -- The other half of the pair, cast from alice's hand on this same board:
        -- the replacement names one object, not the card's name, so this Faith
        -- goes where CR 608.2n sends it.
        let fromHand = S.runPure S.identityAnswer after (Cast.castSpell S.alice handFaithId renewedFaithName Facing.FaceUp)
            resolved = S.runPure S.identityAnswer fromHand Stack.resolveTop
        Spec.assertBool s (not (null (GameState.stack fromHand))) "the hand Faith really was cast"
        Spec.assertEqWith s "CR 608.2n: the hand copy goes to alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 1
        Spec.assertEqWith s "and alice gained the second Faith's life too" (S.lifeOf S.alice resolved) (Just 32)
      other -> Spec.assertFailure s ("expected exactly one exiled Faith, got " <> show (length other))
  -- CR 118.14: "mana of any type can be spent to cast that spell". The offer
  -- side, on a board with no white mana on it at all -- and the same board's
  -- second Renewed Faith, in alice's hand, is the negative: same cost, same
  -- player, same step, no permission.
  Spec.it s "CR 118.14 an off-colour spell in exile is offered where the same card in hand is not" $ do
    mountain <- S.printingOf s registry "Mountain"
    daredevil <- S.printingOf s registry "Dire Fleet Daredevil"
    faith <- S.printingOf s registry "Renewed Faith"
    let (exiled, handFaithId, after) = daredevilRedOnly mountain daredevil faith
    -- The board's own claim, asked of the mana engine rather than assumed from
    -- the land names: nothing alice controls can pay {W}.
    Spec.assertBool s (not (Mana.canPay Cost.manaActivations S.alice whiteCost after)) "alice can make no white mana"
    Spec.assertBool s (Mana.canPay Cost.manaActivations S.alice (ManaCost.MkManaCost [ManaSymbol.Generic 3]) after) "but she has three mana"
    case exiled of
      Nothing -> Spec.assertFailure s "expected the Faith to be exiled"
      Just exiledId -> do
        Spec.assertBool s (offeredCast exiledId renewedFaithName after) "the exiled Faith is offered: CR 118.14 pays its {W} with red"
        Spec.assertBool s (not (offeredCast handFaithId renewedFaithName after)) "the copy in her hand is not, and nothing but the rider tells them apart"
  -- CR 609.4b: the permission "affects only how the player may pay a cost. It
  -- doesn't change that cost, and it doesn't change what mana was actually
  -- spent" -- so the payment goes through, and what pays it is three RED mana
  -- off three Mountains.
  Spec.it s "CR 609.4b the off-colour cost is paid with red mana and resolves" $ do
    mountain <- S.printingOf s registry "Mountain"
    daredevil <- S.printingOf s registry "Dire Fleet Daredevil"
    faith <- S.printingOf s registry "Renewed Faith"
    let (exiled, _, board) = daredevilRedOnly mountain daredevil faith
    case exiled of
      Nothing -> Spec.assertFailure s "expected the Faith to be exiled"
      Just exiledId -> do
        Spec.assertEqWith s "two Mountains paid for the Daredevil" (S.tappedCount S.alice board) 2
        let cast = S.runPure S.identityAnswer board (Cast.castSpell S.alice exiledId renewedFaithName Facing.FaceUp)
            after = S.runPure S.identityAnswer cast Stack.resolveTop
        Spec.assertBool s (not (null (GameState.stack cast))) "the Faith really was cast"
        Spec.assertEqWith s "CR 118.14: the {2}{W} was paid, and alice gains the 6 life" (S.lifeOf S.alice after) (Just 26)
        -- WHAT WAS SPENT, which is rule 609.4b's second clause: three more
        -- Mountains, so the mana that paid the {W} was red and stayed red.
        Spec.assertEqWith s "all five Mountains are tapped" (S.tappedCount S.alice after) 5
        Spec.assertEqWith s "and nothing is left floating" (Game.poolOf S.alice after) (Mana.Type.MkMana [])

-- alice with `n` untapped Swamps and one spell in hand, a Goblin Piker under BOB
-- for the spells that target a creature, and Drought under bob when one is
-- passed. The positive and the negative differ in that Maybe and in nothing
-- else: same seats, same permanents, same Swamps.
--
-- Drought sits with BOB however the cast goes, because its sentence is symmetric
-- ("Spells cost an additional ...", no possessive, PlayerScope.EachPlayer) --
-- the board that proves that is the one where the caster does not control it.
droughtBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Maybe Printing.Printing ->
  Int ->
  (GameState.GameState, ObjectId.ObjectId)
droughtBoard swamp piker spell mDrought n =
  let withPiker = snd (S.addCreature piker S.bob (S.landsInPlay swamp n))
      board = maybe withPiker (\drought -> snd (S.addCreature drought S.bob withPiker)) mDrought
   in S.handOne spell board

-- Drought {2}{W}{W} Enchantment (ICE), Oracle text checked against Scryfall:
-- "At the beginning of your upkeep, sacrifice this enchantment unless you pay
-- {W}{W}. / Spells cost an additional \"Sacrifice a Swamp\" to cast for each
-- black mana symbol in their mana costs. / Activated abilities cost an
-- additional \"Sacrifice a Swamp\" to activate for each black mana symbol in
-- their activation costs."
--
-- The SPELL sentence, which is CR 118.8's "or applied to a spell or ability from
-- another effect" -- the half a spell's own card text cannot state -- reaching CR
-- 601.2f's total. The activation sentence is Pawl.ActivateSpec's droughtSpec;
-- line one is droughtUpkeepSpec below.
--
-- STRICTLY MORE Swamps than any case consumes on every board, so a cast that
-- succeeded for lack of anything to sacrifice cannot pass: the assertion is the
-- SURVIVOR count and never zero.
--
-- NO SINGLE CASE HERE DISCRIMINATES. An implementation that adds the component
-- unconditionally passes the one-symbol case and fails the zero-symbol one; one
-- that adds it exactly once for a black spell passes both and fails the
-- two-symbol case; one that saturates at two fails only Stalker Hag's three. The
-- ladder 0, 1, 2, 3 is the proof, and the counting RULE is what Dismember and
-- the Hag add on top of it.
droughtSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
droughtSpec s registry = Spec.describe s "Drought" $ do
  -- ONE black mana symbol, so one Swamp. Doom Blade is {1}{B}, and the generic
  -- half is what shows the count is over SYMBOLS OF A COLOUR and not over the
  -- cost's size.
  Spec.it s "CR 601.2f a spell with one black symbol costs a Swamp to cast" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    drought <- S.printingOf s registry "Drought"
    blade <- S.printingOf s registry "Doom Blade"
    let (taxed, taxedId) = droughtBoard swamp piker blade (Just drought) 5
        (free, freeId) = droughtBoard swamp piker blade Nothing 5
        after = S.runPure S.identityAnswer taxed (S.cast S.alice taxedId)
        control = S.runPure S.identityAnswer free (S.cast S.alice freeId)
    Spec.assertEqWith s "one of the five Swamps was sacrificed" (S.countOnBattlefieldByName swampName S.alice after) 4
    Spec.assertEqWith s "where the same cast without Drought keeps all five" (S.countOnBattlefieldByName swampName S.alice control) 5
    Spec.assertEqWith s "and the Blade is on the stack, not refused" (length (GameState.stack after)) 1
  -- ZERO black mana symbols, so nothing at all -- not a Sacrifice component of
  -- count zero. Bonesplitter is {1}, and the board is carried far enough that a
  -- component added regardless of the count would have taken a Swamp.
  Spec.it s "CR 601.2f a spell with no black symbol sacrifices nothing" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    drought <- S.printingOf s registry "Drought"
    splitter <- S.printingOf s registry "Bonesplitter"
    let (taxed, taxedId) = droughtBoard swamp piker splitter (Just drought) 5
        after = S.runPure S.identityAnswer taxed (S.cast S.alice taxedId)
    Spec.assertEqWith s "all five Swamps survive" (S.countOnBattlefieldByName swampName S.alice after) 5
    Spec.assertEqWith s "and the Bonesplitter is on the stack" (length (GameState.stack after)) 1
  -- TWO black mana symbols, so two Swamps: the multiplier, which the one-symbol
  -- case above cannot tell from "add it once". Sign in Blood is {B}{B}.
  Spec.it s "CR 601.2f two black symbols cost two Swamps" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    drought <- S.printingOf s registry "Drought"
    sign <- S.printingOf s registry "Sign in Blood"
    let (taxed, taxedId) = droughtBoard swamp piker sign (Just drought) 5
        (free, freeId) = droughtBoard swamp piker sign Nothing 5
        after = S.runPure S.identityAnswer taxed (S.cast S.alice taxedId)
        control = S.runPure S.identityAnswer free (S.cast S.alice freeId)
    Spec.assertEqWith s "two of the five Swamps were sacrificed" (S.countOnBattlefieldByName swampName S.alice after) 3
    Spec.assertEqWith s "where the same cast without Drought keeps all five" (S.countOnBattlefieldByName swampName S.alice control) 5
    Spec.assertEqWith s "and Sign in Blood is on the stack" (length (GameState.stack after)) 1
  -- CR 107.4f: "Phyrexian mana symbols are colored mana symbols ... {B/P} is
  -- black", so Dismember's {1}{B/P}{B/P} holds two BLACK mana symbols and
  -- demands two Swamps -- where an implementation counting only CR 107.4a's
  -- five primary symbols reads it as zero.
  Spec.it s "CR 107.4f two Phyrexian black symbols cost two Swamps too" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    drought <- S.printingOf s registry "Drought"
    dismember <- S.printingOf s registry "Dismember"
    let (taxed, taxedId) = droughtBoard swamp piker dismember (Just drought) 5
        (free, freeId) = droughtBoard swamp piker dismember Nothing 5
        after = S.runPure S.identityAnswer taxed (S.cast S.alice taxedId)
        control = S.runPure S.identityAnswer free (S.cast S.alice freeId)
    Spec.assertEqWith s "two of the five Swamps were sacrificed" (S.countOnBattlefieldByName swampName S.alice after) 3
    Spec.assertEqWith s "where the same cast without Drought keeps all five" (S.countOnBattlefieldByName swampName S.alice control) 5
    Spec.assertEqWith s "and Dismember is on the stack" (length (GameState.stack after)) 1
  -- Drought's own 2008-08-01 ruling, on the symbols it was written about: "A
  -- hybrid symbol that is both black and another type is a black mana symbol,
  -- regardless of what cost is paid for it." Stalker Hag is {B/G}{B/G}{B/G}, so
  -- it is THREE black mana symbols -- and the ruling's last clause is what the
  -- board pins: every one of them is paid with black mana here (alice has only
  -- Swamps), and the count would be the same off Forests, because CR 107.4e
  -- makes a hybrid symbol all of its component colours whatever pays it.
  --
  -- THREE, so this is also the multiplier past two: a scale that saturated at
  -- one or two would leave a Swamp standing.
  Spec.it s "CR 107.4e three black hybrid symbols cost three Swamps" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    drought <- S.printingOf s registry "Drought"
    hag <- S.printingOf s registry "Stalker Hag"
    let (taxed, taxedId) = droughtBoard swamp piker hag (Just drought) 6
        (free, freeId) = droughtBoard swamp piker hag Nothing 6
        after = S.runPure S.identityAnswer taxed (S.cast S.alice taxedId)
        control = S.runPure S.identityAnswer free (S.cast S.alice freeId)
    Spec.assertEqWith s "three of the six Swamps were sacrificed" (S.countOnBattlefieldByName swampName S.alice after) 3
    Spec.assertEqWith s "where the same cast without Drought keeps all six" (S.countOnBattlefieldByName swampName S.alice control) 6
    Spec.assertEqWith s "and the Hag is on the stack" (length (GameState.stack after)) 1

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Cast" $ do
  castSpec s registry
  castEngineSpec s registry
  droughtSpec s registry
  stackSpec s registry
  discardSpec s registry
  sicknessSpec s registry
  magicalHackSpec s registry
  blazeSpec s registry
  vitalizingCascadeSpec s registry
  charSpec s registry
  corrosiveGaleSpec s registry
  waxWaneSpec s registry
  aftermathSpec s registry
  modalCastSpec s registry
  entwineSpec s registry
  kickerSpec s registry
  auraTargetSpec s registry
  fireboltSpec s registry
  flashbackCardTypeSpec s registry
  grantedFlashbackSpec s registry
  graveRecitalSpec s registry
  fugitiveDoctorSpec s registry
  harnessTheStormSpec s registry
  jumpStartSpec s registry
  legendarySpellSpec s registry
  printedCastingRestrictionSpec s registry
  flashSpec s registry
  victorManchaSpec s registry
  direFleetDaredevilSpec s registry
  upToOneTargetSpec s registry
  multiTargetCastSpec s registry
  soulImmolationSpec s registry

-- CR 115.6's "up to one target", read at cast time. Rat Out {B} Instant is "Up
-- to one target creature gets -1/-1 until end of turn. You create a 1/1 black
-- Rat creature token ...", and Dismember is the falsifier: {1}{B/P}{B/P} "Target
-- creature gets -5/-5 until end of turn", the same clause with the slot
-- REQUIRED. One creatureless board tells the two apart, and the same board with
-- a creature on it is what proves the mana was never the reason.
upToOneTargetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
upToOneTargetSpec s registry = Spec.describe s "UpToOneTargetCast" $ do
  Spec.it s "CR 115.6 a slot that may be left empty does not gate castability" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    ratOut <- S.printingOf s registry "Rat Out"
    dismember <- S.printingOf s registry "Dismember"
    -- One base board and one card apiece: S.handOne replaces the hand, so the
    -- two spells cannot sit in it together.
    let base = S.landsInPlay swamp 3
        (bareRat, rat) = S.handOne ratOut base
        (bareCut, cut) = S.handOne dismember base
        (_, peopledCut) = S.addCreature piker S.bob bareCut
    Spec.assertBool s (S.castable S.alice rat bareRat) "up to one target: castable with no creature"
    Spec.assertBool s (not (S.castable S.alice cut bareCut)) "one required target: not castable"
    Spec.assertBool s (S.castable S.alice cut peopledCut) "and castable once a creature exists, so the mana was fine"
  -- CR 601.2c's count announcement is a real choice when the slot has a
  -- candidate, and no question at all when it has none.
  Spec.it s "CR 601.2c the number of targets is announced only when there is one to choose" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    ratOut <- S.printingOf s registry "Rat Out"
    let (bare, spellId) = S.handOne ratOut (S.landsInPlay swamp 1)
        (_, peopled) = S.addCreature piker S.bob bare
        announced gs = any isAnnouncement (snd (Replay.record S.identityAnswer gs (S.cast S.alice spellId)))
        isAnnouncement r = case r of
          Response.AnnouncedTargets _ -> True
          _ -> False
    Spec.assertBool s (not (announced bare)) "no candidate, no question"
    Spec.assertBool s (announced peopled) "a candidate, so the caster is asked"

-- CR 601.2c's count above one, read at cast time. Hearts on Fire {1}{R} Instant
-- is "One or two target creatures each get +2/+1 until end of turn": the number
-- is a real question with two creatures to choose between and no question at all
-- with one, because "in some cases, the number of targets will be defined by the
-- spell's text" -- and here the board defines it just as firmly.
multiTargetCastSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
multiTargetCastSpec s registry = Spec.describe s "MultiTargetCast" $ do
  Spec.it s "CR 601.2c one candidate fixes the number, so nothing is announced" $ do
    (one_, _, spellId) <- heartsBoards s registry
    Spec.assertEqWith s "nothing asked" (announcedCounts spellId one_) []
  Spec.it s "CR 601.2c a second candidate makes the number a question" $ do
    (_, two, spellId) <- heartsBoards s registry
    Spec.assertEqWith s "asked, and answered here with the maximum" (announcedCounts spellId two) [2]

-- One Hearts on Fire in alice's hand over two Mountains, on two boards that
-- differ only in whether bob has a second creature.
heartsBoards :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, GameState.GameState, ObjectId.ObjectId)
heartsBoards s registry = do
  mountain <- S.printingOf s registry "Mountain"
  piker <- S.printingOf s registry "Goblin Piker"
  rats <- S.printingOf s registry "Typhoid Rats"
  hearts <- S.printingOf s registry "Hearts on Fire"
  let (one_, spellId) = S.handOne hearts (snd (S.addCreature piker S.bob (S.landsInPlay mountain 2)))
      (_, two) = S.addCreature rats S.bob one_
  pure (one_, two, spellId)

-- Every number CR 601.2c's announcement carried while casting this spell.
announcedCounts :: ObjectId.ObjectId -> GameState.GameState -> [Natural]
announcedCounts spellId gs =
  [ n
  | Response.AnnouncedTargets counts <- snd (Replay.record S.identityAnswer gs (S.cast S.alice spellId)),
    n <- Map.elems counts
  ]

-- Casts the first offered option, then declines (the loop re-offers until empty).
castFirstOption :: Prompt.Prompt r -> r
castFirstOption p = case p of
  Prompt.CastWhileSearching _ _ options -> case options of
    oid : _ -> Just oid
    [] -> Nothing
  _ -> S.identityAnswer p

-- Does the object with this id show this name? CR 709.3b: through Game.faceOf,
-- so a spell with a half singled out is matched on THAT half's name rather than
-- on CR 709.4's joined one. No caller passes a split card today, so the two
-- readings agree on every id this sees.
nameOnStack :: CardName.CardName -> GameState.GameState -> ObjectId.ObjectId -> Bool
nameOnStack wanted gs oid = case Game.lookupObject oid gs of
  Just o -> case Object.source o of
    Source.OfCard _ -> fmap Face.name (Game.faceOf oid gs) == Just wanted
    Source.OfToken printingId -> fmap S.nameOf (Game.cardOfPrinting printingId gs) == Just wanted
    Source.OfAbility _ -> False
    Source.OfTrigger _ -> False
    Source.OfEmblem _ -> False
    Source.OfInherentTrigger _ -> False
  Nothing -> False

-- castFirstOption answering the search offer with the LAST option rather than
-- the first. That difference is the whole point: a test asserting WHICH half of
-- a split card reached the stack proves nothing if the answerer and the engine
-- agree on "the first one" by accident (CR 709.3).
castLastOption :: Prompt.Prompt r -> r
castLastOption p = case p of
  Prompt.CastWhileSearching _ _ options -> case reverse options of
    choice : _ -> Just choice
    [] -> Nothing
  _ -> castFirstOption p

-- CR 702.127a, aftermath, all three of the static abilities the one word stands
-- for -- on Onward // Victory, where the keyword is printed on the RIGHT half
-- only. That asymmetry is the point: every assertion here would pass vacuously on
-- a card whose halves agreed.
-- A board that can pay for EITHER half and has something for either to target:
-- four Mountains for Onward's {2}{R}, four Plains for Victory's {2}{W}, and a
-- Goblin Piker.
--
-- Both colours on purpose. With Mountains alone every "Victory is not castable"
-- assertion below would hold because {W} could not be paid, which is the vacuous
-- pass a cast gate invites -- the negatives have to fail on rule 702.127a and
-- nothing else.
aftermathBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
aftermathBoard s registry = do
  mountain <- S.printingOf s registry "Mountain"
  plains <- S.printingOf s registry "Plains"
  piker <- S.printingOf s registry "Goblin Piker"
  let withPlains = foldr (\_ g -> snd (S.addCreature plains S.alice g)) (S.landsInPlay mountain 4) [1 :: Int .. 4]
  pure (snd (S.addCreature piker S.alice withPlains))

aftermathSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
aftermathSpec s registry = Spec.describe s "Aftermath" $ do
  -- Rule 702.127a's SECOND ability: "this half of this split card can't be cast
  -- from any zone other than a graveyard". A hand is where every other card in
  -- the pool is castable from, so this is the prohibition doing real work -- and
  -- "this HALF" is why Onward, off the same card in the same hand, still is.
  Spec.it s "CR 702.127a an aftermath half can't be cast from a hand, and its sibling can" $ do
    onwardVictory <- S.printingOf s registry "Onward"
    board <- aftermathBoard s registry
    let (gs, oid) = S.handOne onwardVictory board
    Spec.assertEqWith s "Onward is castable from the hand" (Cast.castable S.alice oid onwardName Facing.FaceUp gs) True
    Spec.assertEqWith s "Victory is not" (Cast.castable S.alice oid victoryName Facing.FaceUp gs) False
  -- Rule 702.127a's FIRST ability: "you may cast this half of this split card from
  -- your graveyard". The mirror of the case above, and the falsifier for a
  -- prohibition that swallowed the permission too.
  Spec.it s "CR 702.127a an aftermath half can be cast from a graveyard, and its sibling cannot" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    onwardVictory <- S.printingOf s registry "Onward"
    board <- aftermathBoard s registry
    -- A main phase with priority: Victory is a SORCERY (CR 307.1), so without it
    -- this would fail on timing rather than on rule 702.127a.
    let (gs0, _) = S.handOne piker board
        (oid, gs) = S.addGraveyardCard onwardVictory S.alice gs0
    Spec.assertEqWith s "Victory is castable from the graveyard" (Cast.castable S.alice oid victoryName Facing.FaceUp gs) True
    -- Onward has no permission of its own, so the graveyard is closed to it --
    -- CR 702.127a grants the half that PRINTS aftermath, not the card.
    Spec.assertEqWith s "Onward is not" (Cast.castable S.alice oid onwardName Facing.FaceUp gs) False

-- Soul Immolation {3}{R}{R} Sorcery (data/cards/soul-immolation.json): "As an
-- additional cost to cast this spell, blight X. X can't be greater than the
-- greatest toughness among creatures you control. Soul Immolation deals X damage
-- to each opponent and each creature they control." Name, cost, type line and
-- oracle text checked against Scryfall 2026-08-20.
--
-- The pool's card for CR 101.1 read against CR 601.2b: the card's own sentence
-- overrides the rule that would otherwise leave the announced X free, and CR
-- 101.2 fixes the direction, the sentence being a "can't". CR 107.3a is only
-- where the announcement happens.
--
-- THREE SEATS, because "each opponent" and "each player" name the same set on a
-- two-seat board once alice is excluded from neither -- carol is what makes the
-- difference between reaching every opponent and reaching one observable.
--
-- The board: alice has five Mountains (exactly {3}{R}{R}, so nothing below turns
-- on mana), a Goblin Piker (2/1) and a Palace Guard (1/4); bob has Russet Wolves
-- (3/3); carol has nothing but a life total. The ceiling is therefore 4, and the
-- Piker is deliberately the LOWER-numbered object: a "least toughness" reading
-- and a "first creature" reading both answer 1, so both refuse an X of 4 that
-- CR 101.1 permits.
--
-- Nothing here turns on affordability, and that is the point: {3}{R}{R} carries
-- no {X}, so every X is equally payable and the ceiling is the only thing that
-- can refuse one.
soulImmolationBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
soulImmolationBoard mountain piker guard wolves immolation =
  let withLands = S.landsFor mountain S.alice 5 S.threePlayerGame
      (pikerId, withPiker) = S.addCreature piker S.alice withLands
      (guardId, withGuard) = S.addCreature guard S.alice withPiker
      (wolvesId, withWolves) = S.addCreature wolves S.bob withGuard
      (spellId, withSpell) = S.addHandCard immolation S.alice withWolves
   in ( spellId,
        pikerId,
        guardId,
        wolvesId,
        withSpell
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      )

-- Announces this X, and pays the blight onto the Palace Guard -- alice controls
-- two creatures, so CR 701.68a's choice is a real prompt and pinning it keeps
-- the counters off the creature the ceiling is read from being an accident.
answerSoulImmolation :: ObjectId.ObjectId -> Natural -> Prompt.Prompt r -> r
answerSoulImmolation guardId n p = case p of
  Prompt.ChooseX {} -> n
  Prompt.ChooseBlight {} -> guardId
  _ -> S.identityAnswer p

soulImmolationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
soulImmolationSpec s registry = Spec.describe s "Soul Immolation" $ do
  -- The PROVING case. Five is one more than the greatest toughness among
  -- alice's creatures, so CR 101.1 refuses the announcement and CR 601.2
  -- returns the game to before the casting was proposed. An engine that
  -- honoured the answer would deal five to each opponent.
  Spec.it s "CR 101.1 an X above the card's stated maximum reverses the cast" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    guard <- S.printingOf s registry "Palace Guard"
    wolves <- S.printingOf s registry "Russet Wolves"
    immolation <- S.printingOf s registry "Soul Immolation"
    let (spellId, _, guardId, wolvesId, board) = soulImmolationBoard mountain piker guard wolves immolation
        after = S.runPure (answerSoulImmolation guardId 5) board (do S.cast S.alice spellId; Stack.resolveTop)
    Spec.assertEqWith s "bob took nothing" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "carol took nothing" (S.lifeOf S.carol after) (Just 20)
    Spec.assertEqWith s "and bob's Wolves took nothing" (S.damageOf wolvesId after) (Just 0)
    -- CR 601.2e's rewind reaches the additional cost as well as the damage.
    Spec.assertEqWith s "no blight counters were paid" (S.counterOf CounterKind.MinusOneMinusOne guardId after) 0
    Spec.assertEqWith s "and the card is still in alice's hand" (S.handSize S.alice after) 1
  -- The CONTROL, and the same board with one thing changed: the answer. Four IS
  -- the greatest toughness among alice's creatures, so CR 101.1 permits it and
  -- everything the case above found missing happens.
  Spec.it s "CR 101.1 an X equal to the stated maximum is announced and paid" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    guard <- S.printingOf s registry "Palace Guard"
    wolves <- S.printingOf s registry "Russet Wolves"
    immolation <- S.printingOf s registry "Soul Immolation"
    let (spellId, _, guardId, wolvesId, board) = soulImmolationBoard mountain piker guard wolves immolation
        after = S.runPure (answerSoulImmolation guardId 4) board (do S.cast S.alice spellId; Stack.resolveTop)
    Spec.assertEqWith s "bob took four" (S.lifeOf S.bob after) (Just 16)
    Spec.assertEqWith s "carol took four" (S.lifeOf S.carol after) (Just 16)
    Spec.assertEqWith s "and bob's Wolves took four" (S.damageOf wolvesId after) (Just 4)
    Spec.assertEqWith s "the blight put four counters on the Palace Guard" (S.counterOf CounterKind.MinusOneMinusOne guardId after) 4
    Spec.assertEqWith s "and the card left alice's hand" (S.handSize S.alice after) 0
  -- Gameplay level, under the ceiling on both sides, so nothing here is about
  -- the ceiling: what it proves is that CR 601.2b's announced X is the number
  -- the blight charges and the number the damage deals, and that "each opponent
  -- and each creature they control" reaches neither alice nor her creatures.
  Spec.it s "CR 107.3a the announced X is blighted, dealt to each opponent, and dealt to their creatures" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    guard <- S.printingOf s registry "Palace Guard"
    wolves <- S.printingOf s registry "Russet Wolves"
    immolation <- S.printingOf s registry "Soul Immolation"
    let (spellId, pikerId, guardId, wolvesId, board) = soulImmolationBoard mountain piker guard wolves immolation
        after = S.runPure (answerSoulImmolation guardId 2) board (do S.cast S.alice spellId; Stack.resolveTop)
    Spec.assertEqWith s "bob took two" (S.lifeOf S.bob after) (Just 18)
    Spec.assertEqWith s "carol took two" (S.lifeOf S.carol after) (Just 18)
    -- CR 109.5: alice is not her own opponent, and neither of her creatures is
    -- one an opponent controls. An ObjectRef.EachPlayer in either instruction's
    -- place would have taken two from her and two from each of them.
    Spec.assertEqWith s "alice took nothing" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "her Palace Guard took no damage" (S.damageOf guardId after) (Just 0)
    Spec.assertEqWith s "her Goblin Piker took no damage" (S.damageOf pikerId after) (Just 0)
    Spec.assertEqWith s "bob's Wolves took two" (S.damageOf wolvesId after) (Just 2)
    -- CR 601.2f/601.2h: the additional cost was paid with the same X, on the
    -- creature the prompt was answered with rather than on the first candidate.
    Spec.assertEqWith s "two -1/-1 counters on the Palace Guard" (S.counterOf CounterKind.MinusOneMinusOne guardId after) 2
    Spec.assertEqWith s "and none on the Goblin Piker" (S.counterOf CounterKind.MinusOneMinusOne pikerId after) 0
