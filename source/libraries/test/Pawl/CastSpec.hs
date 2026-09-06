{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Cast and Pawl.Engine.Stack: cast timing, the stack, discard, and
-- summoning sickness.
module Pawl.CastSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
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
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword.Engine
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as View
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
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
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
    let (oid, gs) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}
        after = snd (Engine.runGamePure S.identityAnswer sick (Engine.settleAll S.alice))
    Spec.assertEqWith s "settled" (sicknessOf oid after) (Just (Sickness.Settled S.alice))
  Spec.it s "CR 302.6 settling does not touch the other player's permanents" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
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
        (both, _) = S.handOne waxWane (snd (S.addPermanent plains S.alice (S.landsInPlay forest 1)))
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
        after = S.runPure S.identityAnswer gs (Cast.castSpell S.manaPerformer S.alice oid wax Facing.FaceUp)
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
      (any (\pl -> Player.life pl < Setup.startingLife (GameState.settings gs) (length (GameState.turnOrder gs)) Nothing 0) (Map.elems (GameState.players gs)))
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
            Source.OfMeld _ -> Spec.assertFailure s "expected a single card source"
            Source.OfToken _ -> Spec.assertFailure s "expected a card source"
            Source.OfAbility _ -> Spec.assertFailure s "expected a card source"
            Source.OfTrigger _ -> Spec.assertFailure s "expected a card source"
            Source.OfEmblem _ -> Spec.assertFailure s "expected a card source"
            Source.OfSpellCopy _ -> Spec.assertFailure s "expected a card source"
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
        (ewId, g1) = S.addPermanent evolvingWilds S.alice g0
        g2 = List.foldl' (\g _ -> snd (S.addPermanent forest S.alice g)) g1 [1 .. (7 :: Int)]
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
        (ewId, g1) = S.addPermanent evolvingWilds S.alice g0
        g2 = List.foldl' (\g _ -> snd (S.addPermanent forest S.alice g)) g1 [1 .. (7 :: Int)]
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
            Source.OfMeld _ -> Spec.assertFailure s "expected a single card source"
            Source.OfToken _ -> Spec.assertFailure s "expected a card source"
            Source.OfAbility _ -> Spec.assertFailure s "expected a card source"
            Source.OfTrigger _ -> Spec.assertFailure s "expected a card source"
            Source.OfEmblem _ -> Spec.assertFailure s "expected a card source"
            Source.OfSpellCopy _ -> Spec.assertFailure s "expected a card source"
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
    let (_, withBirds) = S.addPermanent birds S.alice (Setup.emptyGame S.bothPlayers)
        (oid, gs0) = S.addHandCard lightningBolt S.alice withBirds
        gs = gs0 {GameState.phase = Phase.PrecombatMain}
        -- Green whenever the colour choice is offered; everything else default.
        picksGreen :: Prompt.Prompt r -> r
        picksGreen p = case p of
          Prompt.ChooseManaYield _ _ _ candidates ->
            S.optionYielding (Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Green, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}]) candidates
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
    let (_, withBirds) = S.addPermanent birds S.alice (Setup.emptyGame S.bothPlayers)
        (oid, gs0) = S.addHandCard lightningBolt S.alice withBirds
        gs = gs0 {GameState.phase = Phase.PrecombatMain}
        picksRed :: Prompt.Prompt r -> r
        picksRed p = case p of
          Prompt.ChooseManaYield _ _ _ candidates ->
            S.optionYielding (Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Red, ManaUnit.tags = Set.empty, ManaUnit.retention = ManaRetention.Ordinary, ManaUnit.restriction = Nothing, ManaUnit.rider = Nothing}]) candidates
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
        after = snd (Engine.runGamePure castFirstOption gs (Cast.castWhileSearching S.manaPerformer S.alice))
        onStack = length (filter (nameOnStack (CardName.MkCardName $ Text.pack "Panglacial Wurm") after) (GameState.stack after))
    Spec.assertEqWith s "Panglacial is on the stack" onStack 1
    Spec.assertEqWith s "Panglacial left the library" (S.countByName (CardName.MkCardName $ Text.pack "Panglacial Wurm") S.alice after) 0
    Spec.assertEqWith s "seven Forests tapped to pay {5}{G}{G}" (S.tappedCount S.alice after) 7
  -- CR 733.1 reverses a move from a library TO THE STACK; only a move to any
  -- other zone, a shuffle or a reveal stands. Cost.keepingLibraryActions once
  -- kept the failed state's library wholesale, which left a refused library cast
  -- in no zone at all.
  Spec.it s "CR 733.1 a refused library cast puts Panglacial back in the library" $ do
    forest <- S.printingOf s registry "Forest"
    panglacialWurm <- S.printingOf s registry "Panglacial Wurm"
    let base = S.landsInPlay forest 7
        (wurmId, gs) = S.addLibraryCard panglacialWurm S.alice base
        -- Accepts the offer ONCE: with the Wurm back in the library the search
        -- offers it again, and CR 601.3 lets alice keep declining.
        refuse :: Prompt.Prompt r -> State.State Bool r
        refuse p = case p of
          Prompt.CastWhileSearching _ _ options -> do
            offered <- State.get
            State.put True
            pure (if offered then Nothing else Maybe.listToMaybe options)
          Prompt.ChooseManaSource {} -> pure Nothing
          _ -> pure (S.identityAnswer p)
        after = snd (State.evalState (Engine.runGame refuse gs (Cast.castWhileSearching S.manaPerformer S.alice)) False)
    Spec.assertEqWith s "CR 733.1 Panglacial is back in the library" (Game.zoneMembers Zone.Library S.alice after) [wurmId]
    Spec.assertEqWith s "nothing on the stack" (length (GameState.stack after)) 0
    Spec.assertEqWith s "no Forest tapped" (S.tappedCount S.alice after) 0
    Spec.assertEqWith s "CR 400.7 the same object, never moved" (Game.objectCount after) (Game.objectCount gs)
  -- CR 709.3 ("A player chooses which half of a split card they are casting
  -- before putting it onto the stack") does not stop at the library door, and CR
  -- 601.3 grants a permission to CAST rather than a narrower one: a split card
  -- printing the Panglacial permission on each half offers TWO options during a
  -- search, and picking between them is the player's choice.
  Spec.it s "CR 709.3 a split card offers BOTH halves during a search" $ do
    forest <- S.printingOf s registry "Forest"
    mountain <- S.printingOf s registry "Mountain"
    split <- S.printingOf s registry "Synthetic Glacial Half"
    let (_, withMountain) = S.addPermanent mountain S.alice (S.landsInPlay forest 1)
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
        (ewId, g1) = S.addPermanent evolvingWilds S.alice g0
        (_, g2) = S.addPermanent mountain S.alice (snd (S.addPermanent forest S.alice g1))
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
            Object.kicked = Map.empty,
            Object.bestowed = False,
            Object.phyrexianLifePaid = 0,
            Object.manaSpent = Mana.MkMana [],
            Object.announcedX = Nothing,
            Object.castFrom = Nothing,
            Object.detainedUntil = Set.empty,
            Object.goadedBy = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty,
            Object.activatedOnce = Set.empty
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
    let (mountainId, g0) = S.addPermanent mountain S.alice (Setup.emptyGame S.bothPlayers)
        (islandId, g1) = S.addPermanent island S.alice g0
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
-- Char's two damage clauses name different recipients, and every wrong
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
  -- CR 608.2f makes the sentence ONE action over both recipients, so Char is one
  -- instruction over two clauses -- 4 at the chosen target and 2 at its caster --
  -- and neither clause's amount reaches the other's recipient, which is what the
  -- untouched Piker below pins. That the two are dealt at once rather than in
  -- sequence is unobservable here and is Pawl.ReplacementSpec's Char case.
  Spec.it s "CR 109.5/120.3a Char deals 4 to bob and 2 to its caster" $ do
    char <- S.printingOf s registry "Char"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (pikerId, board) = S.addPermanent piker S.bob (S.landsInPlay mountain 3)
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
    let (gs0, oid) = S.handOne blaze (snd (S.addPermanent thalia S.alice (S.landsInPlay mountain 4)))
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
    let board = snd (S.addPermanent plains S.alice (snd (S.addPermanent plains S.alice (S.landsInPlay forest 3))))
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
        (_, gs1) = S.addPermanent piker S.alice gs0
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
  let (pikerId, gs1) = S.addPermanent piker S.bob (S.landsInPlay land lands)
      (wallId, gs2) = S.addPermanent wallOfStone S.bob gs1
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
      (Cast.entwineOffer ManaSpending.AsProduced S.alice richSpell (Cost.costsFor S.alice (S.printingName dreamsGrip) richSpell rich) rich)
      (Just (Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]), Cost.Type.components = []}))
    Spec.assertEqWith s "one Island: unaffordable, so not offered" (Cast.entwineOffer ManaSpending.AsProduced S.alice poorSpell (Cost.costsFor S.alice (S.printingName dreamsGrip) poorSpell poor) poor) Nothing
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
      (Cast.entwineOffer ManaSpending.AsProduced S.alice spellId (Cost.costsFor S.alice (S.printingName braid) spellId gs) gs)
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
        (_, gs) = S.addPermanent piker S.bob gs0
    Spec.assertEqWith s "no entwine cost to offer" (Cast.entwineOffer ManaSpending.AsProduced S.alice spellId (Cost.costsFor S.alice (S.printingName chaosCharm) spellId gs) gs) Nothing

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
  let (giantId, gs1) = S.addPermanent hillGiant S.bob (S.landsInPlay mountain mountains)
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
-- ONE entry per kicker cost the spell offered, in the order they were asked.
kickerAnnouncements :: [Response.Response] -> [KickerDecision.KickerDecision]
kickerAnnouncements responses = [d | Response.AnnouncedKicker d <- responses]

-- CR 702.33c: answers every kicker question with the same count. Everything else
-- defers to S.identityAnswer.
kicksTimes :: Natural -> Prompt.Prompt r -> r
kicksTimes times p = case p of
  Prompt.ChooseKicker {} -> KickerDecision.MkKickerDecision times
  _ -> S.identityAnswer p

-- CR 702.33b's board: alice has three Plains, two Forests and three Islands --
-- enough for {2}{W} plus BOTH of Sunscape Battlemage's kicker costs, so neither
-- question is withheld for want of mana -- two cards in her library for the {2}{U}
-- payoff to draw, an empty hand but for the Battlemage, and bob has a Bird Maiden
-- (a 1/2 flier) for the {1}{G} payoff to destroy.
battlemageBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m BattlemageBoard
battlemageBoard s registry = do
  plains <- S.printingOf s registry "Plains"
  forest <- S.printingOf s registry "Forest"
  island <- S.printingOf s registry "Island"
  battlemage <- S.printingOf s registry "Sunscape Battlemage"
  birdMaiden <- S.printingOf s registry "Bird Maiden"
  let lands = S.landsFor island S.alice 3 (S.landsFor forest S.alice 2 (S.landsInPlay plains 3))
      (_, withFlier) = S.addPermanent birdMaiden S.bob lands
      (_, withLibrary1) = S.addLibraryCard plains S.alice withFlier
      (_, withLibrary2) = S.addLibraryCard forest S.alice withLibrary1
      (gs, spellId) = S.handOne battlemage withLibrary2
  pure (MkBattlemageBoard gs spellId (S.printingName birdMaiden))

-- The board `battlemageBoard` built, its Battlemage and the flier's name to count.
data BattlemageBoard = MkBattlemageBoard GameState.GameState ObjectId.ObjectId CardName.CardName

birdMaidenName :: BattlemageBoard -> CardName.CardName
birdMaidenName (MkBattlemageBoard _ _ name) = name

-- Casts the Battlemage with `answer`, then puts CR 603.3's entry triggers on the
-- stack and resolves them. Twice, because the card prints two and only one of them
-- ever triggers here; resolveTop on an empty stack does nothing.
castBattlemage :: (forall r. Prompt.Prompt r -> r) -> BattlemageBoard -> ([Response.Response], GameState.GameState)
castBattlemage answer (MkBattlemageBoard gs spellId _) =
  let (asked, after) = castAndResolve answer gs spellId
      drained = S.runPure answer after (Monad.replicateM_ 2 (Engine.settleForPriority >> Stack.resolveTop))
   in (asked, S.settleSba drained)

-- CR 702.33f: kicks ONE of the two costs, told apart by the Cost the prompt
-- carries -- which is the whole of what makes the two questions distinguishable.
kicksGreen :: Prompt.Prompt r -> r
kicksGreen = kicksMatching greenKicker

kicksBlue :: Prompt.Prompt r -> r
kicksBlue = kicksMatching blueKicker

kicksMatching :: Cost.Type.Cost Keyword.Keyword -> Prompt.Prompt r -> r
kicksMatching wanted p = case p of
  Prompt.ChooseKicker _ _ _ cost _ ->
    KickerDecision.MkKickerDecision (if cost == wanted then 1 else 0)
  -- One legal target per slot, FILTERED out of the offered set rather than built
  -- by hand, so CR 608.2b sees the recipient the prompt offered.
  Prompt.ChooseTargets _ _ _ sets -> Map.map (Set.take 1 . snd) sets
  _ -> S.identityAnswer p

greenKicker :: Cost.Type.Cost Keyword.Keyword
greenKicker = manaOnly [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Green)]

blueKicker :: Cost.Type.Cost Keyword.Keyword
blueKicker = manaOnly [ManaSymbol.Generic 2, ManaSymbol.OfType (ManaType.Colored Color.Blue)]

manaOnly :: [ManaSymbol.ManaSymbol] -> Cost.Type.Cost Keyword.Keyword
manaOnly symbols = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost symbols), Cost.Type.components = []}

-- How many cards this player holds, which is where the {2}{U} payoff shows up.
handSize :: PlayerId.PlayerId -> GameState.GameState -> Int
handSize pid gs = length (Game.zoneMembers Zone.Hand pid gs)

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
        (asked, after) = castAndResolve (bursts (KickerDecision.MkKickerDecision 0) giantId) gs spellId
        settled = S.settleSba after
    Spec.assertEqWith s "the player was asked, and declined" (kickerAnnouncements asked) [KickerDecision.MkKickerDecision 0]
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
        (asked, after) = castAndResolve (bursts (KickerDecision.MkKickerDecision 1) giantId) gs spellId
        settled = S.settleSba after
    Spec.assertEqWith s "the player was asked, and kicked" (kickerAnnouncements asked) [KickerDecision.MkKickerDecision 1]
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
        (asked, after) = castAndResolve (bursts (KickerDecision.MkKickerDecision 1) giantId) gs spellId
        settled = S.settleSba after
    Spec.assertBool s (S.castable S.alice spellId gs) "the spell is still castable"
    Spec.assertEqWith s "no kicker question was put" (kickerAnnouncements asked) []
    Spec.assertEqWith s "so it dealt its printed 2" (S.damageOf giantId settled) (Just 2)
    Spec.assertEqWith s "and only {R} was paid" (S.tappedCount S.alice settled) 1
  -- What the CARD offers, asked directly, so the cost and CR 702.33a's limit of one
  -- are pinned apart from the cast that consumes them.
  Spec.it s "CR 702.33a Burst Lightning's one kicker cost is the {4}, payable once" $ do
    mountain <- S.printingOf s registry "Mountain"
    burstLightning <- S.printingOf s registry "Burst Lightning"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (gs, spellId, _) = kickerBoard mountain burstLightning hillGiant 5
    Spec.assertEqWith
      s
      "one cost, {4}, and rule 702.33a's limit of one payment"
      (fmap (Keyword.Engine.kickerCosts . Face.keywords) (Game.faceOf spellId gs))
      (Just [(Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 4]), Cost.Type.components = []}, Just 1)])
  -- A card with no kicker is never asked, which is the other half of "where the
  -- rules leave nothing to ask, don't prompt".
  Spec.it s "CR 702.33a a spell without kicker (Lightning Bolt) is never offered one" $ do
    mountain <- S.printingOf s registry "Mountain"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (gs, spellId, giantId) = kickerBoard mountain lightningBolt hillGiant 5
        (asked, _) = castAndResolve (bursts (KickerDecision.MkKickerDecision 1) giantId) gs spellId
    Spec.assertEqWith s "no kicker cost to offer" (fmap (Keyword.Engine.kickerCosts . Face.keywords) (Game.faceOf spellId gs)) (Just [])
    Spec.assertEqWith s "so no kicker question was put, on a board that could pay one" (kickerAnnouncements asked) []
  -- CR 702.33a's "an additional cost", singular: kicker is payable once, so an
  -- answer of two is text Burst Lightning does not have. Nine Mountains, which is
  -- {R} plus the {4} twice over, so nothing but the LIMIT can reject this cast --
  -- with five the second payment would be unaffordable and CR 601.2's payability
  -- would answer instead.
  Spec.it s "CR 702.33a paying the kicker TWICE is not an answer Burst Lightning offers: the cast is rejected" $ do
    mountain <- S.printingOf s registry "Mountain"
    burstLightning <- S.printingOf s registry "Burst Lightning"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (gs, spellId, giantId) = kickerBoard mountain burstLightning hillGiant 9
        (_, after) = castAndResolve (bursts (KickerDecision.MkKickerDecision 2) giantId) gs spellId
        settled = S.settleSba after
    Spec.assertEqWith s "no damage was dealt, so the spell never resolved" (S.damageOf giantId settled) (Just 0)
    Spec.assertEqWith s "CR 601.2e rewound the cast, so no Mountain is tapped" (S.tappedCount S.alice settled) 0
    Spec.assertEqWith s "and the card is back in alice's hand" (S.countByName (S.printingName burstLightning) S.alice settled) 1
  -- CR 702.33c: multikicker, the same announcement asked as a COUNT. Gnarlid Pack
  -- {1}{G} Creature -- Beast 2/2, whole text "Multikicker {1}{G}" plus "this
  -- creature enters with a +1/+1 counter on it for each time it was kicked"
  -- (Scryfall, 2026-08-31) -- so the count is visible as the permanent's P/T and
  -- nowhere else, and a Bool would make the twice-kicked Pack a 3/3.
  Spec.it s "CR 702.33c the multikicker paid TWICE: Gnarlid Pack enters a 4/4, and six Forests are tapped" $ do
    forest <- S.printingOf s registry "Forest"
    gnarlidPack <- S.printingOf s registry "Gnarlid Pack"
    let (gs, spellId) = S.handOne gnarlidPack (S.landsInPlay forest 6)
        (asked, after) = castAndResolve (kicksTimes 2) gs spellId
        settled = S.settleSba after
    Spec.assertEqWith s "two counters, so the 2/2 is a 4/4" (S.powerToughnessOf (S.creatureId settled) settled) (Just (4, 4))
    Spec.assertEqWith s "the player was asked once and answered twice-kicked" (kickerAnnouncements asked) [KickerDecision.MkKickerDecision 2]
    Spec.assertEqWith s "{1}{G} plus the kicker twice: all six Forests are tapped" (S.tappedCount S.alice settled) 6
  -- The same board and the same answerer but for the count, which is what makes
  -- the case above about the NUMBER rather than about kicking at all.
  Spec.it s "CR 702.33c declining the multikicker leaves Gnarlid Pack a 2/2, and taps two Forests" $ do
    forest <- S.printingOf s registry "Forest"
    gnarlidPack <- S.printingOf s registry "Gnarlid Pack"
    let (gs, spellId) = S.handOne gnarlidPack (S.landsInPlay forest 6)
        (asked, after) = castAndResolve (kicksTimes 0) gs spellId
        settled = S.settleSba after
    Spec.assertEqWith s "no counters, so it is the printed 2/2" (S.powerToughnessOf (S.creatureId settled) settled) (Just (2, 2))
    Spec.assertEqWith s "the player was asked, and declined" (kickerAnnouncements asked) [KickerDecision.MkKickerDecision 0]
    Spec.assertEqWith s "only {1}{G} was paid" (S.tappedCount S.alice settled) 2
  -- CR 702.33b: two kicker costs on one spell, each with its own CR 702.33f payoff.
  -- Sunscape Battlemage {2}{W} Creature -- Human Wizard 2/2, "Kicker {1}{G} and/or
  -- {2}{U}", with an enters trigger per cost (Scryfall, 2026-08-31). The board pays
  -- for BOTH, so each case answers one question yes and the other no on the same
  -- board -- which is what a single kicked bit cannot tell apart.
  Spec.it s "CR 702.33f kicked with its {1}{G} kicker only: the flier dies and no cards are drawn" $ do
    board <- battlemageBoard s registry
    let (asked, settled) = castBattlemage kicksGreen board
    Spec.assertEqWith s "the {1}{G} trigger destroyed bob's Bird Maiden" (S.countOnBattlefieldByName (birdMaidenName board) S.bob settled) 0
    Spec.assertEqWith s "and the {2}{U} trigger did not run, so alice's hand is empty" (handSize S.alice settled) 0
    Spec.assertEqWith s "both questions were put, and only the green one kicked" (kickerAnnouncements asked) [KickerDecision.MkKickerDecision 1, KickerDecision.MkKickerDecision 0]
  Spec.it s "CR 702.33f kicked with its {2}{U} kicker only: two cards are drawn and the flier lives" $ do
    board <- battlemageBoard s registry
    let (asked, settled) = castBattlemage kicksBlue board
    Spec.assertEqWith s "the {2}{U} trigger drew two cards" (handSize S.alice settled) 2
    Spec.assertEqWith s "and the {1}{G} trigger did not run, so bob's Bird Maiden lives" (S.countOnBattlefieldByName (birdMaidenName board) S.bob settled) 1
    Spec.assertEqWith s "both questions were put, and only the blue one kicked" (kickerAnnouncements asked) [KickerDecision.MkKickerDecision 0, KickerDecision.MkKickerDecision 1]

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
        (creature, withCreature) = S.addPermanent piker S.bob base
        (gs, spellId) = S.handOne unholyStrength withCreature
        slots = Card.modesTargetSlots (Seq.singleton (ModeIndex.MkModeIndex 0)) (S.combinedFace unholyStrength)
    Spec.assertEqWith s "one slot, the enchant slot" (Set.singleton Card.enchantSlot) (Map.keysSet slots)
    Spec.assertEqWith
      s
      "its legal set is the one creature"
      (Target.legalSets Nothing False Map.empty spellId slots gs)
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
        (bystander, gs1) = S.addPermanent piker S.alice gs0
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
        manaOf oid gs = fmap Cost.Type.mana (Cost.costsFor S.alice (S.printingName firebolt) oid gs)
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
-- The EXILE assertions below are CR 400.7g's, and this group is where that rule
-- is proved: "if an effect grants a nonland card an ability that allows it to be
-- cast, that ability will continue to apply to the new object that card became
-- after it moved to the stack". Neither grant survives CR 601.2a's move -- one
-- is derived from the card as it lies in the graveyard and the other is anchored
-- to that object's id, and the move mints a new one either way -- so only the
-- carry across it (Pawl.Engine.Cast.keywordsBefore) can still arm rule 702.34a's
-- second static ability on the spell.
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
        manaOf oid gs = fmap Cost.Type.mana (Cost.costsFor S.alice (S.printingName bolt) oid gs)
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
    -- CR 400.7g, and CR 702.34a's second static ability off a grant that CANNOT
    -- survive the move: this one is stored against the graveyard object's id, and
    -- CR 601.2a mints a new one, so the replacement has to be armed from the
    -- keywords the card held where it lay. Lightning Bolt prints no flashback, so
    -- the grant is the only thing that can be exiling it.
    let cast = S.runPure S.identityAnswer blueBoard (S.cast S.alice onIslands)
        resolved = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertEqWith s "CR 400.7g: the flashed-back Bolt did not return to the graveyard" (Game.zoneMembers Zone.Graveyard S.alice resolved) []
    Spec.assertEqWith s "CR 400.7g: it was exiled" (length (Game.zoneMembers Zone.Exile S.alice resolved)) 1
  -- CR 107.3a / 601.2b: the {X} the grant copies is a COST's {X}, announced as
  -- the spell is cast. CR 107.3g's zero is about a card's own mana cost where it
  -- lies, which is what fixes its mana VALUE off the stack (CR 202.3e); the
  -- alternative cost here is paid at CR 601.2f, by which point CR 601.2a has put
  -- the spell on the stack. Snapcaster Mage's ruling says the same in as many
  -- words: "If you cast an instant or sorcery with {X} in its mana cost this way,
  -- you still choose the value of X as part of casting the spell and pay that
  -- cost."
  --
  -- Lier, Disciple of the Drowned {3}{U}{U} is the granter and Blaze {X}{R}
  -- Sorcery ("Blaze deals X damage to any target") the receiver, so the flashback
  -- cost is {X}{R} and X is free. Six Mountains, X announced at 4: an arm that
  -- dropped the {X}, or fixed it at zero, would price the cast at {R}, raise no
  -- CR 601.2b announcement at all, tap one Mountain and leave bob at 20.
  Spec.it s "CR 107.3a a granted flashback {X}{R} announces X rather than treating it as 0" $ do
    mountain <- S.printingOf s registry "Mountain"
    blaze <- S.printingOf s registry "Blaze"
    lier <- S.printingOf s registry "Lier, Disciple of the Drowned"
    let (inGraveyard, board) = inGraveyardWith mountain blaze 6
        (_, granted) = S.addPermanent lier S.alice board
        after = S.runPure (answerXOf 4) granted (do S.cast S.alice inGraveyard; Stack.resolveTop)
    -- The gameplay assertion, and it ahead of every proxy: Blaze deals the
    -- announced X, so 16 is the announcement having survived into the resolution.
    Spec.assertEqWith s "bob took the 4 that was announced" (S.lifeOf S.bob after) (Just 16)
    -- {4}{R} is five Mountains, and the sixth is spare: 4 is neither the floor CR
    -- 601.2b measures castability at nor the most the board could pay, so an
    -- engine that clamped to the affordable maximum would have announced 5 and
    -- left bob at 15.
    Spec.assertEqWith s "five Mountains paid the {4}{R}" (S.tappedCount S.alice after) 5
    -- CR 702.34a's second static ability, which says the cast really was a
    -- flashback cast rather than some other permission.
    Spec.assertEqWith s "the flashed-back Blaze did not return to the graveyard" (Game.zoneMembers Zone.Graveyard S.alice after) []
    Spec.assertEqWith s "it was exiled" (length (Game.zoneMembers Zone.Exile S.alice after)) 1
    -- The cost itself, read where the grant built it: one candidate, and it
    -- carries CR 107.4's {X} verbatim off Blaze's mana cost.
    Spec.assertEqWith
      s
      "the granted flashback cost is {X}{R}"
      (fmap Cost.Type.mana (Cost.costsFor S.alice (S.printingName blaze) inGraveyard granted))
      [Just (ManaCost.MkManaCost [ManaSymbol.Variable, theRed])]

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
      (fmap Cost.Type.mana (Cost.costsFor S.alice (S.printingName firebolt) inGraveyard permitted))
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
-- The Fugitive Doctor {3}{R}{G} is the only printing in the pool whose grant of a
-- flashback ability is a LITERAL cost and nothing else. Lier, Disciple of the
-- Drowned is the other producer of a graveyard card holding two -- its grant
-- states rule 702.34a's cost as "equal to that card's mana cost"
-- (Modification.GainFlashbackAtManaCost), which lierSpec below proves, and which
-- Archmage's Newt and Iroh, Grand Lotus each state for one class of card beside
-- a literal cost for another.
--
-- Firebolt's printed {4}{R} and the granted {2}{R}{G} share no reading, and
-- WHICH of them is the unreachable one is decided by Keyword's derived Ord:
-- Generic 2 sorts under Generic 4, so the GRANTED cost is the lesser and the
-- PRINTED one is the second. Ten lands pay either, so no assertion below turns
-- on mana.
--
-- CR 603.12: "you may sacrifice a Clue. When you do, target instant or sorcery
-- card in your graveyard gains flashback {2}{R}{G} until end of turn" is a CR
-- 118.12 pay gate whose IfPaid branch ARMS a reflexive ability
-- (TriggerCondition.Reflexive) rather than granting the flashback itself, so the
-- target belongs to that ability and is chosen as IT goes on the stack. The
-- group's first case proves the difference is observable.
fugitiveDoctorAnswer :: Prompt.Prompt r -> r
fugitiveDoctorAnswer p = case p of
  -- The Clue is worth spending: without the sacrifice the pay gate's IfPaid
  -- branch never runs and no second flashback is granted.
  Prompt.ChooseToPay {} -> PaymentDecision.Pays
  -- Both boards below leave exactly ONE instant-or-sorcery card in alice's
  -- graveyard, so taking every legal recipient takes exactly that card and the
  -- slot's count is satisfied.
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
      -- entersWithTrigger rather than addPermanent: the Clue this ability's
      -- pay gate spends is the Doctor's OWN CR 701.16a investigate, so the
      -- fixture makes it the way the card does.
      (_, entered) = S.entersWithTrigger doctor S.alice buried
      withClue = S.runPure S.identityAnswer entered (Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
  pure (S.runCombat fugitiveDoctorAnswer withClue, inGraveyard)

-- fugitiveDoctorBoard's discriminating twin. One thing differs: alice's
-- graveyard is EMPTY, and the card the reflexive ability will target is in her
-- hand instead -- same lands, same Doctor, same Clue from the same CR 701.16a
-- investigate, same seats. So nothing below can turn on mana, timing or stock;
-- only on WHEN the card reaches the graveyard.
--
-- Lightning Bolt rather than Firebolt for two reasons: it is an INSTANT, so it
-- can be cast in response to the attack trigger, and it prints no flashback of
-- its own, so a flashback cost offered for it in the graveyard came from the
-- grant and from nothing else.
emptyGraveyardDoctorBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, ObjectId.ObjectId)
emptyGraveyardDoctorBoard s registry = do
  mountain <- S.printingOf s registry "Mountain"
  forest <- S.printingOf s registry "Forest"
  bolt <- S.printingOf s registry "Lightning Bolt"
  doctor <- S.printingOf s registry "The Fugitive Doctor"
  let (combat, _, _) = S.combatBoardOf [] []
      lands = S.landsFor forest S.alice 3 (S.landsFor mountain S.alice 7 combat)
      (inHand, held) = S.addHandCard bolt S.alice lands
      (_, entered) = S.entersWithTrigger doctor S.alice held
      withClue = S.runPure S.identityAnswer entered (Engine.settleForPriority >> Stack.resolveTop >> Engine.settleForPriority)
  pure (withClue, inHand)

-- fugitiveDoctorAnswer plus one action: cast the instant alice holds, the first
-- time it is offered. The board sits AT declare attackers, so her first priority
-- of the run is the one CR 508.2b gives her with the attack trigger already on the
-- stack -- which is the window the reflexive ability's target has to be chosen
-- after. A pure answerer suffices because the action is offered exactly once:
-- once cast, the card is in her graveyard and no ChooseAction offers it again.
respondingDoctorAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> r
respondingDoctorAnswer inHand p = case p of
  Prompt.ChooseAction _ _ actions -> Maybe.fromMaybe A.Pass (List.find (S.isCastOf inHand) actions)
  -- Lightning Bolt's own "any target", pinned to bob. The reflexive ability's
  -- slot offers graveyard CARDS and never a player, so this cannot answer that
  -- prompt by accident -- and the reflexive's is left to fugitiveDoctorAnswer.
  Prompt.ChooseTargets _ _ _ sets
    | any (Set.member (Recipient.ToPlayer S.bob) . snd) sets ->
        fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
  _ -> fugitiveDoctorAnswer p

fugitiveDoctorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fugitiveDoctorSpec s registry = Spec.describe s "FugitiveDoctor" $ do
  -- CR 603.12 routes the reflexive ability through CR 603.7, so it goes on the
  -- stack in its own right (CR 603.3) and announces its own target there (CR
  -- 603.3d / 601.2c). Collapsed into the attack trigger, the same target would be
  -- announced as THAT ability was placed -- and on this board no legal choice
  -- could be made for it, so CR 603.3d would remove the attack trigger from the
  -- stack and alice would never be offered the sacrifice at all.
  Spec.it s "CR 603.12/603.3d a reflexive ability's target is chosen as IT goes on the stack, after the payment" $ do
    (board, inHand) <- emptyGraveyardDoctorBoard s registry
    let after = S.runCombat (respondingDoctorAnswer inHand) board
        buried = Game.zoneMembers Zone.Graveyard S.alice after
        boltName = CardName.MkCardName (Text.pack "Lightning Bolt")
        granted = ManaCost.MkManaCost [ManaSymbol.Generic 2, theRed, ManaSymbol.OfType (ManaType.Colored Color.Green)]
        -- alice owns and controls the Clue and nothing here moves it, so the
        -- OWNER-indexed count answers the control question too.
        clues = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Clue Token")) S.alice
    Spec.assertEqWith s "the fixture's graveyard starts empty, so no target exists when the attack trigger is placed" (Game.zoneMembers Zone.Graveyard S.alice board) []
    Spec.assertEqWith s "and the Clue the payment will spend is on the battlefield" (clues board) 1
    Spec.assertEqWith s "she cast it in response, so it lay in the graveyard alone by the time the reflexive was placed" (fmap (fmap S.nameOf . flip Game.cardOf after) buried) [Just boltName]
    -- The gameplay-level assertion, and first among the three that read `after`:
    -- the grant reached a card that was not in the graveyard when the creating
    -- ability went on the stack. Under the collapse this list is empty, Lightning
    -- Bolt printing no flashback of its own.
    Spec.assertEqWith
      s
      "CR 603.3d: the reflexive ability targeted it, so it has flashback {2}{R}{G}"
      (fmap (\o -> fmap Cost.Type.mana (Cost.costsFor S.alice boltName o after)) buried)
      [[Just granted]]
    Spec.assertBool s (all (\o -> S.castable S.alice o after) buried) "and alice may cast it from her graveyard"
    -- CR 118.12's payment really happened, which the collapse never reaches: the
    -- attack trigger would have been removed from the stack before the offer.
    Spec.assertEqWith s "the Clue was sacrificed" (clues after) 0
    -- CR 603.12a's second sentence, and CR 603.7b: one arming, one firing. A
    -- second would have wanted a second target and found none.
    Spec.assertEqWith s "and the reflexive ability fired once, leaving the delayed store empty" (length (GameState.delayedTriggers after)) 0
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
      (fmap Cost.Type.mana (Cost.costsFor S.alice (S.printingName firebolt) inGraveyard board))
      [Just granted, Just printed]
    -- CR 702.34a's SECOND static ability, asked of the cost a first-only read
    -- never returns: paying the PRINTED {4}{R} must exile the card too.
    Spec.assertEqWith s "the printed cost's cast dealt its 2" (S.lifeOf S.alice (resolveWith (paying printed))) (Just 18)
    Spec.assertEqWith s "and exiled the card (CR 702.34a)" (boltsIn Zone.Exile (resolveWith (paying printed))) 1
    Spec.assertEqWith s "not put it into the graveyard" (boltsIn Zone.Graveyard (resolveWith (paying printed))) 0
    Spec.assertEqWith s "the granted cost's cast dealt its 2 as well" (S.lifeOf S.alice (resolveWith (paying granted))) (Just 18)
    Spec.assertEqWith s "and exiled the card too" (boltsIn Zone.Exile (resolveWith (paying granted))) 1
    Spec.assertEqWith s "not put it into the graveyard either" (boltsIn Zone.Graveyard (resolveWith (paying granted))) 0

-- Lier, Disciple of the Drowned {3}{U}{U} (data/cards/lier-disciple-of-the-drowned.json):
-- "Spells can't be countered. Each instant and sorcery card in your graveyard
-- has flashback. The flashback cost is equal to that card's mana cost."
--
-- CR 702.34a's [cost] read off the RECEIVING card rather than written on the
-- granter, which is Modification.GainFlashbackAtManaCost's whole reason: the
-- grant is card data and cannot name the mana cost of a card it has not met, so
-- Pawl.Engine.Projection.applyModification materialises the keyword against the
-- object it is applying to.
--
-- Firebolt is the receiver because it already PRINTS Flashback {4}{R}, so the
-- board holds two flashback abilities whose costs share no reading -- and the
-- board holds ONE Mountain, which is what makes the two readings differ: the
-- granted {R} is payable and the printed {4}{R} is not. With ten lands, both
-- readings end with Firebolt exiled and 2 damage dealt, and every assertion
-- below would be vacuous.
--
-- The second clause is transcribed as well (PlayerEffect.CantBeCountered under
-- PlayerScope.EachPlayer, Spider-Punk's shape), so pawl's Lier is not weaker
-- than the printed card; the last case holds it to that scope.
lierBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (GameState.GameState, ObjectId.ObjectId)
lierBoard s registry withLier = do
  mountain <- S.printingOf s registry "Mountain"
  firebolt <- S.printingOf s registry "Firebolt"
  lier <- S.printingOf s registry "Lier, Disciple of the Drowned"
  let -- ONE Mountain: see the header. Everything else about the two boards is
      -- identical, so nothing below can turn on mana, timing or seats.
      lands = S.landsInPlay mountain 1
      seated = if withLier then snd (S.addPermanent lier S.alice lands) else lands
      (inGraveyard, buried) = S.addGraveyardCard firebolt S.alice seated
  pure (aliceOnTurn buried, inGraveyard)

lierSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lierSpec s registry = Spec.describe s "Lier" $ do
  Spec.it s "CR 702.34a a granted flashback priced at the card's own mana cost is payable for that cost" $ do
    firebolt <- S.printingOf s registry "Firebolt"
    mountain <- S.printingOf s registry "Mountain"
    (board, inGraveyard) <- lierBoard s registry True
    let granted = ManaCost.MkManaCost [theRed]
        printed = ManaCost.MkManaCost [ManaSymbol.Generic 4, theRed]
        -- CR 601.2b's announcement, answered by NAMING a cost rather than by
        -- index: which of the two sorts first is Keyword's derived Ord, not the
        -- order they were granted in, and an index would pick the unpayable
        -- {4}{R} on the fixed board and read exactly like the broken one.
        paying :: ManaCost.ManaCost -> Prompt.Prompt r -> r
        paying wanted p = case p of
          Prompt.ChooseCost _ _ _ candidates ->
            Maybe.fromMaybe (Cost.firstOffered candidates) (List.find ((== Just wanted) . Cost.Type.mana) candidates)
          -- Firebolt's own "any target", FILTERED from the offered set rather
          -- than built, and aimed at alice so its 2 damage reports which cast
          -- resolved. Lier is on this battlefield and is a legal target too, so
          -- an unpinned answer would report a creature's damage instead.
          Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToPlayer S.alice) . snd) sets
          _ -> S.identityAnswer p
        after = S.runPure (paying granted) (S.runPure (paying granted) board (S.cast S.alice inGraveyard)) Stack.resolveTop
        boltsIn zone gs = length (filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (S.printingName firebolt)) (Game.zoneMembers zone S.alice gs))
    -- The precondition the whole group rests on, asserted rather than assumed:
    -- one Mountain cannot pay the printed {4}{R}.
    Spec.assertEqWith s "alice has exactly one Mountain, which cannot pay the printed {4}{R}" (S.countOnBattlefieldByName (S.printingName mountain) S.alice board) 1
    -- Gameplay level, and FIRST: under the broken reading Firebolt never leaves
    -- the graveyard at all, Lier granting no cast-from-graveyard permission of
    -- its own beyond rule 702.34a's.
    Spec.assertEqWith s "the granted {R} paid for the cast, which dealt its 2" (S.lifeOf S.alice after) (Just 18)
    Spec.assertEqWith s "and CR 702.34a exiled the card as it resolved" (boltsIn Zone.Exile after) 1
    Spec.assertEqWith s "so it is not back in the graveyard" (boltsIn Zone.Graveyard after) 0
    -- Supporting, and AFTER the three above so it cannot absorb a mutation.
    Spec.assertEqWith
      s
      "both flashback costs are on offer, the granted {R} beside the printed {4}{R}"
      (fmap Cost.Type.mana (Cost.costsFor S.alice (S.printingName firebolt) inGraveyard board))
      [Just printed, Just granted]
  -- The discriminating twin: same Mountain, same Firebolt, same graveyard, same
  -- phase and priority. One thing differs, and it is Lier -- so the grant is a
  -- continuous static ability read live (CR 613.1f / 604.2) rather than a stamp
  -- the card carried into the graveyard.
  Spec.it s "CR 613.1f without Lier the printed {4}{R} is the only flashback, and one Mountain cannot pay it" $ do
    firebolt <- S.printingOf s registry "Firebolt"
    (board, inGraveyard) <- lierBoard s registry False
    Spec.assertBool s (not (S.castable S.alice inGraveyard board)) "CR 601.2b: no candidate cost is payable, so the card cannot be cast at all"
    Spec.assertEqWith
      s
      "and the granted cost is gone with the granter"
      (fmap Cost.Type.mana (Cost.costsFor S.alice (S.printingName firebolt) inGraveyard board))
      [Just (ManaCost.MkManaCost [ManaSymbol.Generic 4, theRed])]
  -- The other half of rule 702.34a: its FIRST static ability is a casting
  -- permission (Keyword.permissionsFor), so a card printing no flashback of its
  -- own cannot be cast from a graveyard at all until the grant arrives. Lightning
  -- Bolt is that card, and its mana cost is the same {R} the Mountain pays -- so
  -- the whole offer here, permission and price alike, comes from Lier.
  Spec.it s "CR 702.34a/113.6f the grant creates the cast-from-graveyard permission, not just a price" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    mountain <- S.printingOf s registry "Mountain"
    lier <- S.printingOf s registry "Lier, Disciple of the Drowned"
    let lands = S.landsInPlay mountain 1
        (_, seated) = S.addPermanent lier S.alice lands
        (inGraveyard, buried) = S.addGraveyardCard bolt S.alice seated
        board = aliceOnTurn buried
        (withoutLier, unhelped) = S.addGraveyardCard bolt S.alice lands
        bare = aliceOnTurn unhelped
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToPlayer S.alice) . snd) sets
          _ -> S.identityAnswer p
        after = S.runPure answer (S.runPure answer board (S.cast S.alice inGraveyard)) Stack.resolveTop
        boltsIn zone gs = length (filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (S.printingName bolt)) (Game.zoneMembers zone S.alice gs))
    Spec.assertEqWith s "Lightning Bolt prints no flashback, so without Lier the graveyard offers no cost at all" (Cost.costsFor S.alice (S.printingName bolt) withoutLier bare) []
    Spec.assertEqWith s "the granted flashback paid {R} and the spell dealt its 3" (S.lifeOf S.alice after) (Just 17)
    Spec.assertEqWith s "and CR 702.34a exiled the card" (boltsIn Zone.Exile after) 1
    Spec.assertEqWith s "rather than returning it to the graveyard" (boltsIn Zone.Graveyard after) 0
  -- Lier's other clause, which is Spider-Punk's sentence and takes its scope:
  -- "spells can't be countered" has no possessive, so PlayerScope.EachPlayer
  -- rather than CR 109.5's You. Here to hold LIER's transcription to that
  -- reading -- the machinery is already proved by CR 701.6a's group below --
  -- since a You arm would leave an opponent's spells counterable and pawl's
  -- Lier weaker than the printed card.
  Spec.it s "CR 701.6a/109.5 Lier's first clause protects every player's spells, not only its controller's" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    (withLier, _) <- lierBoard s registry True
    (without, _) <- lierBoard s registry False
    let (bobs, protected) = S.addHandCard bolt S.bob withLier
        (bobs', unprotected) = S.addHandCard bolt S.bob without
    Spec.assertBool s (PlayerEffect.cantBeCountered S.bob bobs protected) "an opponent's spell is uncounterable while Lier is out"
    Spec.assertBool s (not (PlayerEffect.cantBeCountered S.bob bobs' unprotected)) "and counterable without it"

-- Synthetic Mirror of the Fallen {U}{U} Sorcery
-- (data/cards/synthetic-mirror-of-the-fallen.json): "Target card in your
-- graveyard becomes a copy of target card in an opponent's graveyard."
--
-- SYNTHETIC because no printing turns a card in a graveyard into a copy of
-- anything. Scryfall `o:/copy of/ o:graveyard -t:token`, 2026-08-29: every hit
-- copies FROM a graveyard card into a token, a permanent or an entering creature
-- (Body Double, Dimir Doppelganger, Lazav, Shifting Woodland, Feldon), none onto
-- one; the card that would refute it reads "target card in a graveyard becomes a
-- copy of ...". Rule 707 bars no zone -- CR 707.1's object is a "spell,
-- permanent, or card" -- and CR 400.7 is not in play, nothing here changing zone.
--
-- BOTH SLOTS name graveyard cards, where a permanent would read more naturally
-- as the original: Lier is the pool's only granter of
-- Modification.GainFlashbackAtManaCost and its static ability reaches instant and
-- sorcery cards only (CR 702.34a), so a subject that copied a CREATURE would lose
-- the grant and the board could show nothing at all. Opposite graveyards (CR
-- 400.1) rather than an "another target card" restriction, since that is what
-- keeps one slot from naming the other's card.
--
-- Acidic Soil {2}{R} and Lightning Bolt {R} differ in AMOUNT and not only in
-- colour, which is what the one untapped Mountain left after the Mirror is cast
-- discriminates: it pays the copied cost and cannot pay the printed one.
--
-- BOB CONTROLS NO LAND, which is what makes the two spells' resolutions readable
-- apart: Acidic Soil's own clause deals alice 3 for her three lands and bob
-- nothing, where the Lightning Bolt it copies deals its one target 3. So the CR
-- 400.7 case below cannot pass by resolving the wrong spell.
mirrorBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
mirrorBoard s registry = do
  island <- S.printingOf s registry "Island"
  mountain <- S.printingOf s registry "Mountain"
  soil <- S.printingOf s registry "Acidic Soil"
  bolt <- S.printingOf s registry "Lightning Bolt"
  lier <- S.printingOf s registry "Lier, Disciple of the Drowned"
  mirror <- S.printingOf s registry "Synthetic Mirror of the Fallen"
  -- TWO Islands and ONE Mountain: the Islands are exactly the Mirror's {U}{U}
  -- and the Mountain is exactly Lightning Bolt's {R}, so the flashback cast that
  -- follows has one land to pay with. Three lands altogether is also what Acidic
  -- Soil's own clause counts when it resolves.
  let lands = S.landsFor island S.alice 2 (S.landsInPlay mountain 1)
      (_, seated) = S.addPermanent lier S.alice lands
      (subject, withSubject) = S.addGraveyardCard soil S.alice seated
      (original, withOriginal) = S.addGraveyardCard bolt S.bob withSubject
      (inHand, held) = S.addHandCard mirror S.alice withOriginal
  pure (aliceOnTurn held, inHand, subject, original)

-- Both of the Mirror's slots, FILTERED from the offered set rather than built, so
-- CR 608.2b's re-read at resolution finds the recipient the engine offered. One
-- predicate for both because the pools are disjoint: alice's graveyard holds only
-- the subject and bob's only the original.
--
-- Rule 707.2's "any target" is deliberately NOT answered here. The copied
-- Lightning Bolt's slot is never announced (CR 400.7, the case below), and an
-- answerer that filled it would hide that by leaving a legal cast either way.
mirrorAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
mirrorAnswer subject original p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    fmap (Set.filter (\r -> r == Recipient.ToObject subject || r == Recipient.ToObject original) . snd) sets
  _ -> S.identityAnswer p

-- mirrorAnswer, recording the SLOT NAMES each ChooseTargets offered, in order.
-- What CR 601.2c announces is not readable off the finished board: a slot never
-- offered and a slot whose target did nothing leave the same life totals.
mirrorRecordingAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> State.State [Set.Set SlotName.SlotName] r
mirrorRecordingAnswer subject original p = case p of
  Prompt.ChooseTargets _ _ _ sets -> do
    State.modify' (<> [Map.keysSet sets])
    pure (mirrorAnswer subject original p)
  _ -> pure (mirrorAnswer subject original p)

-- Cast and resolve the Mirror, then flashback-cast the card it turned into a copy
-- and resolve that, recording every slot announced along the way.
runMirror :: (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId) -> (GameState.GameState, [Set.Set SlotName.SlotName])
runMirror (board, inHand, subject, original) =
  let (((), after), offered) =
        State.runState
          ( Engine.runGame (mirrorRecordingAnswer subject original) board $ do
              S.cast S.alice inHand
              Stack.resolveTop
              S.cast S.alice subject
              Stack.resolveTop
          )
          []
   in (after, offered)

-- The Mirror's own two slots, which the copy's cast is measured against below.
mirrorSlots :: Set.Set SlotName.SlotName
mirrorSlots = Set.fromList (fmap (SlotName.MkSlotName . Text.pack) ["archetype", "mirrored"])

mirrorOfTheFallenSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mirrorOfTheFallenSpec s registry = Spec.describe s "MirrorOfTheFallen" $ do
  Spec.it s "CR 707.2 a graveyard card that is a copy is priced at the copy's mana cost" $ do
    soil <- S.printingOf s registry "Acidic Soil"
    mountain <- S.printingOf s registry "Mountain"
    (board, inHand, subject, original) <- mirrorBoard s registry
    let answer :: Prompt.Prompt r -> r
        answer = mirrorAnswer subject original
        printed = ManaCost.MkManaCost [ManaSymbol.Generic 2, theRed]
        copied = ManaCost.MkManaCost [theRed]
        onStack = S.runPure answer board (S.cast S.alice inHand)
        copiedBoard = S.runPure answer onStack Stack.resolveTop
        resolved = S.runPure answer (S.runPure answer copiedBoard (S.cast S.alice subject)) Stack.resolveTop
        soilsIn zone gs = length (filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (S.printingName soil)) (Game.zoneMembers zone S.alice gs))
    -- The preconditions, read off boards the copy has not reached, so none of
    -- them can absorb a mutation of the arm under test.
    Spec.assertEqWith s "alice has exactly one Mountain, the Mirror's {U}{U} having spent both Islands" (S.countOnBattlefieldByName (S.printingName mountain) S.alice onStack) 1
    -- The discriminating twin, and it is the SAME board one resolution earlier:
    -- same lands, same seats, same graveyards, the Mirror already paid for. One
    -- thing differs, and it is whether the copy has happened.
    Spec.assertEqWith
      s
      "with the Mirror still on the stack the grant prices the card at its own printed {2}{R}"
      (fmap Cost.Type.mana (Cost.costsFor S.alice (S.printingName soil) subject onStack))
      [Just printed]
    -- Gameplay level, and FIRST among the reads of `resolved`: one Mountain
    -- cannot pay {2}{R}, so under the printed reading there is no cast at all and
    -- alice stays at 20.
    Spec.assertEqWith s "the copied {R} paid for the flashback cast, and Acidic Soil dealt her 3 for her three lands" (S.lifeOf S.alice resolved) (Just 17)
    Spec.assertEqWith s "and CR 702.34a exiled the card as it resolved" (soilsIn Zone.Exile resolved) 1
    Spec.assertEqWith s "so it is not back in her graveyard" (soilsIn Zone.Graveyard resolved) 0
    -- Supporting, and AFTER the three above so it cannot absorb the mutation.
    Spec.assertEqWith
      s
      "CR 707.2: once it is a copy, the only flashback cost offered is Lightning Bolt's {R}"
      (fmap Cost.Type.mana (Cost.costsFor S.alice (S.printingName soil) subject copiedBoard))
      [Just copied]
    -- CR 707.2b: the original is untouched -- bob's Lightning Bolt is still in
    -- his graveyard, so nothing above turned on the copy having consumed it.
    Spec.assertEqWith s "and bob's Lightning Bolt is still in his graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob resolved)) 1
  -- CR 400.7 against CR 707.2, on the one board where the two disagree. The copy
  -- lasts until the end of the game (CR 611.2a, no duration stated) and it is
  -- rule 707.2's copiable rules text, so while the card lies in the graveyard it
  -- IS Lightning Bolt -- which is what the flashback price above reads. Casting
  -- it moves it to the stack, and CR 400.7 makes the spell there a NEW object
  -- that the copy effect's fixed set of affected objects (CR 611.2c) does not
  -- reach. So the spell is Acidic Soil again, in both of the places CR 707.2
  -- would otherwise show: the target slot CR 601.2c announces and the effects CR
  -- 608.2c resolves.
  --
  -- The same reading a Clone dying gets: the card in the graveyard is Clone.
  Spec.it s "CR 400.7 the copy cast from the graveyard is a new object, so it announces and resolves Acidic Soil" $ do
    board <- mirrorBoard s registry
    let (resolved, offered) = runMirror board
    -- Gameplay first, and the two seats disagree: Acidic Soil deals alice 3 for
    -- her three lands and bob nothing for his none, where the Lightning Bolt in
    -- bob's graveyard would have dealt one target 3 and the other nothing.
    Spec.assertEqWith s "Acidic Soil's own clause dealt alice 3 for her three lands" (S.lifeOf S.alice resolved) (Just 17)
    Spec.assertEqWith s "and bob nothing for his none" (S.lifeOf S.bob resolved) (Just 20)
    -- CR 601.2c, which the life totals cannot see: Lightning Bolt's "any target"
    -- was never offered, so the cast was announced off Acidic Soil too.
    Spec.assertEqWith s "the Mirror announced its own two slots and the cast announced none" offered [mirrorSlots]

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
  let (_, g1) = S.addPermanent harness S.alice (S.landsInPlay mountain 6)
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

-- Tinybones, the Pickpocket {B} Legendary Creature -- Skeleton Rogue 1/1 (oracle
-- text checked against Scryfall, 2026-09-05): "Deathtouch. Whenever Tinybones
-- deals combat damage to a player, you may cast target nonland permanent card
-- from that player's graveyard, and mana of any type can be spent to cast that
-- spell."
--
-- THE FIRST CARD IN data/cards/ that casts a spell out of a graveyard its caster
-- does not own. CR 400.3 puts a card only in its OWNER's graveyard, so on every
-- other board pawl can build the caster, the zone's owner and the card's owner
-- are one seat; here they are two. Pawl.Engine.Quantity's WasCastFrom arm is
-- what reads them apart, and Pawl.ConditionSpec's Breathless Knight case is
-- where the two references disagree.
--
-- CR 608.2g is the road, and it grants CR 601.3's permission itself: the effect
-- naming the object IS it, so Cast.castableWhenOffered asks no zone at all and
-- nothing has to grant alice a cast from bob's graveyard. That was only half
-- true until this card arrived -- Cost.candidateCostsFor priced a graveyard card
-- at nothing without a permission it could FIND on the board, so the offer was
-- made and then withdrawn for want of a cost; Cost.candidateCostsGiven is where
-- the offer now says so. Havengul Lich would take the CR 601.3 road instead,
-- which still has no one-object permission to write.
--
-- Not implemented: "mana of any type can be spent to cast that spell" (CR
-- 118.14). Effect.OfferCast carries no spending rider where
-- Effect.GrantPlayFromExile does, so pawl's Tinybones is STRICTER than printed
-- -- the caster owes the stolen card's own coloured mana, which is why the
-- Swamps below and the {B} Vessel are chosen to match (#3251).

-- alice attacks with a Tinybones and holds two Swamps; bob's graveyard holds an
-- Archfiend's Vessel, a Goblin Piker, a Swamp and a Dryad Arbor, and alice's own
-- graveyard holds a second Vessel.
--
-- Four decoys, one per way the offer could be wrong. The Piker is the COUNT
-- decoy: a one-candidate slot is answered with no prompt at all, so a set
-- assertion over a single member could not tell "bob's graveyard" from "the one
-- card anywhere". The Swamp is the CARD TYPE decoy: a land alone is kept out by
-- the "nonland" and by the permanent-card Or's silence about lands alike, so it
-- cannot tell the two readings apart. The Dryad Arbor is what can, and it is the
-- decoy none of the others stands in for -- a Land Creature satisfies the
-- permanent-card half (CR 110.4a) and only CR 305.9's "nonland" subtracts it.
-- alice's Vessel is the ZONE decoy -- the same printing, the same card types, a
-- different pile -- so the only thing that keeps it out of the offer is whose
-- graveyard it is in, which is the axis this whole group is about.
pickpocketBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId], Printing.Printing)
pickpocketBoard s registry = do
  swamp <- S.printingOf s registry "Swamp"
  tinybones <- S.printingOf s registry "Tinybones, the Pickpocket"
  vessel <- S.printingOf s registry "Archfiend's Vessel"
  piker <- S.printingOf s registry "Goblin Piker"
  arbor <- S.printingOf s registry "Dryad Arbor"
  let (combat, _, _) = S.combatBoardOf [tinybones] []
      (stolen, g1) = S.addGraveyardCard vessel S.bob (S.landsFor swamp S.alice 2 combat)
      (decoy, g2) = S.addGraveyardCard piker S.bob g1
      (_, g3) = S.addGraveyardCard swamp S.bob g2
      (_, g4) = S.addGraveyardCard arbor S.bob g3
      (_, g5) = S.addGraveyardCard vessel S.alice g4
  pure (g5, stolen, [stolen, decoy], vessel)

-- pickpocketBoard with the two ordinary permanent cards taken out of bob's
-- graveyard, so the Dryad Arbor is the only card there that a bare "permanent
-- card" would admit. The Swamp stays, which is what keeps the difference from
-- the board above to the CARDS rather than to the pile being empty.
--
-- Returns the Arbor's id, so the answerer below can be pointed AT it: the case
-- has to fail loudly if the slot offers it, not quietly pick something else.
arborOnlyBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, ObjectId.ObjectId)
arborOnlyBoard s registry = do
  swamp <- S.printingOf s registry "Swamp"
  tinybones <- S.printingOf s registry "Tinybones, the Pickpocket"
  arbor <- S.printingOf s registry "Dryad Arbor"
  let (combat, _, _) = S.combatBoardOf [tinybones] []
      (_, g1) = S.addGraveyardCard swamp S.bob (S.landsFor swamp S.alice 2 combat)
      (arborId, g2) = S.addGraveyardCard arbor S.bob g1
  pure (g2, arborId)

-- Records what Tinybones' own slot was offered and which casts it went on to
-- offer, pinning the target to `wanted` and taking the offer.
--
-- The target is PINNED by identity rather than searched for: an answerer that
-- picked whatever was legal would find the Vessel again after a mutation, and
-- the recording is what tells a candidate set drawn from bob's graveyard from
-- one drawn from every graveyard on the board.
pickpocketAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State ([Set.Set Recipient.Recipient], [CardName.CardName]) r
pickpocketAnswer wanted p = case p of
  Prompt.ChooseTargets _ _ _ sets -> do
    Monad.forM_
      (Map.lookup (SlotName.MkSlotName (Text.pack "stolen")) sets)
      (\(_, rs) -> State.modify' (\(ts, os) -> (ts <> [rs], os)))
    pure (fmap (\(_, legal) -> Set.filter ((== Just wanted) . Recipient.objectOf) legal) sets)
  Prompt.OfferedCast _ _ _ name -> do
    State.modify' (\(ts, os) -> (ts, os <> [name]))
    pure OptionalDecision.Exercises
  _ -> pure (S.attackTo S.bob p)

-- S.runCombat with a recording answerer: run whole steps while combat lasts.
runPickpocket :: ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, ([Set.Set Recipient.Recipient], [CardName.CardName]))
runPickpocket wanted =
  let go n g
        | n <= (0 :: Int) = pure g
        | not (S.inCombatPhase (GameState.phase g)) = pure g
        | otherwise = do
            (_, g') <- Engine.runGame (pickpocketAnswer wanted) g Engine.runStep
            go (n - 1) g'
   in \gs -> State.runState (go 24 gs) ([], [])

pickpocketSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
pickpocketSpec s registry =
  Spec.describe s "Pickpocket"
    . Spec.it s "CR 601.2a alice casts a spell out of bob's graveyard"
    $ do
      (board, stolen, candidates, vessel) <- pickpocketBoard s registry
      let (after, (offered, offers)) = runPickpocket stolen board
          vessels owner =
            [ oid
            | oid <- Set.toList (GameState.battlefield after),
              S.soleFaceName oid after == S.printingName vessel,
              fmap Object.owner (Game.lookupObject oid after) == Just owner
            ]
      -- The gameplay reading, first: a card BOB owns is a permanent ALICE
      -- controls, and it got there by being cast. CR 400.3 is why the two seats
      -- are the whole point -- the Vessel was never in a pile of alice's.
      case vessels S.bob of
        [taken] -> Spec.assertEqWith s "CR 601.2a alice controls the Vessel she cast out of bob's graveyard" (View.controllerOf taken after) (Just S.alice)
        other -> Spec.assertFailure s ("expected exactly one Vessel bob owns on the battlefield, got " <> show (length other))
      Spec.assertEqWith s "and alice's own copy stayed in her graveyard" (length (vessels S.alice)) 0
      -- Then the two proxies. The candidate set is the pool's reading of "that
      -- player's graveyard": identity, not count, since the Piker and the Vessel
      -- are both bob's and the Swamp and alice's Vessel are not offered at all.
      Spec.assertEqWith s "the offer reached bob's graveyard, nonland permanent cards only" offered [Set.fromList (fmap Recipient.ToObject candidates)]
      Spec.assertEqWith s "and one cast was offered, of the targeted card" offers [S.printingName vessel]

-- CR 305.9's subtraction on its own board: Dryad Arbor (Land Creature -- Forest
-- Dryad) is the only card in bob's graveyard that a bare "permanent card" would
-- admit, so with the printed "nonland" read there is no legal choice for the
-- slot at all and CR 603.3d removes the trigger from the stack.
--
-- The case above cannot make this claim: there the Arbor sits beside two cards
-- that ARE legal, so the trigger goes on the stack either way and only the
-- offered SET moves. Here the trigger's existence is what moves.
--
-- Why the cast being refused is not the same fact: Dryad Arbor prints no mana
-- cost, so CR 118.6 makes every candidate unpayable and
-- Cast.castableWhenOffered would decline the cast even if the target had been
-- announced. The divergence is at CR 601.2c, one step earlier -- a player
-- offered an illegal target, and a trigger spent on it.
arborTargetSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
arborTargetSpec s registry =
  Spec.describe s "PickpocketNonland"
    . Spec.it s "CR 305.9 a land creature card in that graveyard is no legal target"
    $ do
      (board, arborId) <- arborOnlyBoard s registry
      let (after, (offered, offers)) = runPickpocket arborId board
      -- CR 603.3d: no legal choice can be made for the slot, so the ability is
      -- removed from the stack and no target prompt is ever raised.
      Spec.assertEqWith s "CR 305.9 the slot was offered nothing, so the trigger had no legal target" offered []
      Spec.assertEqWith s "and no cast was offered off it" offers []
      -- The preconditions, AFTER the behaviour so neither can absorb a mutation
      -- aimed at the filter: the combat damage really was dealt, and the Arbor
      -- really was sitting in bob's graveyard to be offered.
      Spec.assertEqWith s "off the combat damage the trigger watches for" (S.lifeOf S.bob after) (fmap (subtract 1) (S.lifeOf S.bob board))
      Spec.assertBool s (elem arborId (Game.zoneMembers Zone.Graveyard S.bob after)) "with the Arbor still in bob's graveyard"

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
        costsOf oid gs = fmap (\c -> (Cost.Type.mana c, Cost.Type.components c)) (Cost.costsFor S.alice (S.printingName directCurrent) oid gs)
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
        board = snd (S.addPermanent thalia S.alice gs)
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
        board = snd (S.addPermanent jace S.alice gs)
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
        board = snd (S.addPermanent mindslaver S.alice gs)
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
        board = snd (S.addPermanent thalia S.bob gs)
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
        (thaliaId, withThalia) = S.addPermanent thalia S.alice gs
        board = snd (S.addPermanent piker S.alice withThalia)
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
      (_, gs1) = S.addPermanent plains S.bob gs0
      (bobsRally, gs2) = S.addHandCard rally S.bob gs1
      (_, gs3) = S.addPermanent plains S.alice gs2
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

tapStateOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)

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

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Cast" $ do
  castSpec s registry
  castEngineSpec s registry
  stackSpec s registry
  discardSpec s registry
  sicknessSpec s registry
  magicalHackSpec s registry
  blazeSpec s registry
  vitalizingCascadeSpec s registry
  charSpec s registry
  corrosiveGaleSpec s registry
  modalCastSpec s registry
  entwineSpec s registry
  kickerSpec s registry
  auraTargetSpec s registry
  fireboltSpec s registry
  flashbackCardTypeSpec s registry
  grantedFlashbackSpec s registry
  graveRecitalSpec s registry
  fugitiveDoctorSpec s registry
  lierSpec s registry
  mirrorOfTheFallenSpec s registry
  harnessTheStormSpec s registry
  pickpocketSpec s registry
  arborTargetSpec s registry
  jumpStartSpec s registry
  legendarySpellSpec s registry

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
    -- CR 712.8g: the combined back face's name, which Game.faceOf already
    -- resolves to. No caller reaches one -- CR 701.42a never puts it on the
    -- stack -- so this is the OfCard read rather than a second rule.
    Source.OfMeld _ -> fmap Face.name (Game.faceOf oid gs) == Just wanted
    Source.OfToken printingId -> fmap S.nameOf (Game.cardOfPrinting printingId gs) == Just wanted
    Source.OfAbility _ -> False
    Source.OfTrigger _ -> False
    Source.OfEmblem _ -> False
    -- CR 112.1a: a copy of a spell has a name, the copied spell's, and the same
    -- read finds it. No caller reaches one today.
    Source.OfSpellCopy _ -> fmap Face.name (Game.faceOf oid gs) == Just wanted
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
