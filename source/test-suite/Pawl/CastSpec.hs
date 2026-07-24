{-# LANGUAGE GADTs #-}

-- Covers Pawl.Cast and Pawl.Stack: cast timing, the stack, discard, and
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
import qualified Pawl.Action as Action
import qualified Pawl.Activate as Activate
import qualified Pawl.Binding as Binding
import qualified Pawl.Card as Card
import qualified Pawl.Cards as Cards
import qualified Pawl.Cast as Cast
import qualified Pawl.Cost as Cost
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Projection as Projection
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.Action as A
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Concession as Concession
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.MulliganDecision as MulliganDecision
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

sicknessOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe Sickness.Sickness
sicknessOf oid gs = fmap Object.sickness (Game.lookupObject oid gs)

sicknessTests :: Cards.Cards -> Tasty.TestTree
sicknessTests cards =
  Tasty.testGroup
    "Sickness"
    [ HU.testCase "CR 302.6 a permanent entering the battlefield is summoning sick" $
        -- changeZone mints a new object, so the id to inspect is the new one.
        let (gs, oid) = S.pikerInHand cards 3 Phase.PrecombatMain
            after = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Battlefield)
         in case Game.zoneMembers Zone.Battlefield S.alice after of
              [] -> HU.assertFailure "expected a permanent"
              ids -> case filter (\o -> sicknessOf o after == Just Sickness.Sick) ids of
                [] -> HU.assertFailure "the new permanent should be Sick"
                _ -> pure (),
      HU.testCase "CR 302.6 the untap step settles the active player's permanents" $
        let (oid, gs) = S.addPiker cards S.alice (Setup.emptyGame S.bothPlayers)
            sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}
            after = snd (Engine.runGamePure S.identityAnswer sick (Engine.settleAll S.alice))
         in HU.assertEqual "settled" (Just Sickness.Settled) (sicknessOf oid after),
      HU.testCase "CR 302.6 settling does not touch the other player's permanents" $
        let (oid, gs) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}
            after = snd (Engine.runGamePure S.identityAnswer sick (Engine.settleAll S.alice))
         in HU.assertEqual "still sick" (Just Sickness.Sick) (sicknessOf oid after)
    ]

castGameState :: Cards.Cards -> GameState.GameState
castGameState cards =
  snd (Engine.runMatchPure S.castAnswer (S.redRed cards))

castEngineTests :: Cards.Cards -> Tasty.TestTree
castEngineTests cards =
  Tasty.testGroup
    "CastEngine"
    [ HU.testCase "a castable Piker is offered as a legal action" $
        let (gs, oid) = S.pikerInHand cards 2 Phase.PrecombatMain
         in HU.assertBool "offered" (elem (A.Cast oid) (Action.legalActions S.alice gs)),
      HU.testCase "an unaffordable Piker is not offered" $
        let (gs, oid) = S.pikerInHand cards 1 Phase.PrecombatMain
         in HU.assertBool "not offered" (notElem (A.Cast oid) (Action.legalActions S.alice gs)),
      -- M3a: the red deck now carries Lightning Bolt, so castAnswer casts removal
      -- that clears the board -- creaturesInPlay at end is no longer a valid proxy
      -- for "a spell resolved". castAnswer never attacks, so the ONLY source of
      -- life loss is a resolved Bolt: a player below 20 proves an instant was cast
      -- AND resolved (not merely discarded). Deck-robust where creature-presence
      -- was not.
      HU.testCase "casting actually happens in a full game" $
        HU.assertBool
          "a spell resolved and dealt damage"
          (any (\pl -> Player.life pl < Setup.startingLife) (Map.elems (GameState.players (castGameState cards)))),
      HU.testCase "a casting game still terminates" $
        HU.assertBool "has result" (Maybe.isJust (GameState.result (castGameState cards))),
      HU.testCase "a casting game conserves objects" $
        HU.assertEqual "objects" 120 (Game.objectCount (castGameState cards)),
      HU.testCase "CR 500.4 no mana floats at the end of a game" $
        HU.assertEqual "pools empty" Map.empty (GameState.manaPool (castGameState cards))
    ]

-- A Piker cast and left on the stack, ready to resolve.
pikerOnStack :: Cards.Cards -> GameState.GameState
pikerOnStack cards =
  let (gs, oid) = S.pikerInHand cards 3 Phase.PrecombatMain
   in snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid))

stackTests :: Cards.Cards -> Tasty.TestTree
stackTests cards =
  Tasty.testGroup
    "Stack"
    [ HU.testCase "CR 608.3 a resolving creature spell becomes a permanent" $
        let after = snd (Engine.runGamePure S.identityAnswer (pikerOnStack cards) Stack.resolveTop)
         in do
              HU.assertEqual "stack empty" 0 (length (GameState.stack after))
              -- Four, not one: pikerInHand 3 leaves three Mountains in play.
              HU.assertEqual "four permanents" 4 (length (Game.zoneMembers Zone.Battlefield S.alice after))
              HU.assertEqual "one of them a creature" 1 (S.creaturesInPlay S.alice after),
      HU.testCase "CR 400.7 the permanent is a new object" $
        let after = snd (Engine.runGamePure S.identityAnswer (pikerOnStack cards) Stack.resolveTop)
         in case GameState.stack (pikerOnStack cards) of
              [] -> HU.assertFailure "fixture should have a spell on the stack"
              top : _ -> HU.assertEqual "old id gone" Nothing (Game.lookupObject top after),
      HU.testCase "the permanent is a Piker on the battlefield" $
        -- The object the spell resolved INTO, not just any permanent: the
        -- fixture already has three Mountains in play, and zoneMembers is
        -- ordered by id, so the front of that list is Mountain id 0.
        let before = Game.zoneMembers Zone.Battlefield S.alice (pikerOnStack cards)
            after = snd (Engine.runGamePure S.identityAnswer (pikerOnStack cards) Stack.resolveTop)
            isNew o = notElem o before
            fresh = filter isNew (Game.zoneMembers Zone.Battlefield S.alice after)
         in case fresh of
              [] -> HU.assertFailure "expected a new permanent"
              oid : _ -> case Game.lookupObject oid after of
                Nothing -> HU.assertFailure "battlefield id should resolve"
                Just obj -> do
                  HU.assertEqual "zone" Zone.Battlefield (Object.zone obj)
                  case Object.source obj of
                    Source.OfCard printing ->
                      HU.assertBool "creature" (Card.isCreature (Printing.card printing))
                    Source.OfToken _ -> HU.assertFailure "expected a card source"
                    Source.OfAbility _ _ -> HU.assertFailure "expected a card source"
                    Source.OfTrigger _ _ -> HU.assertFailure "expected a card source"
                    Source.OfEmblem _ -> HU.assertFailure "expected a card source"
                    Source.OfInherentTrigger _ _ -> HU.assertFailure "expected a card source",
      HU.testCase "resolving conserves objects" $
        HU.assertEqual
          "conserved"
          (Game.objectCount (pikerOnStack cards))
          (Game.objectCount (snd (Engine.runGamePure S.identityAnswer (pikerOnStack cards) Stack.resolveTop))),
      HU.testCase "resolving an empty stack is a no-op" $
        let gs = Setup.emptyGame S.bothPlayers
         in HU.assertEqual "unchanged" gs (snd (Engine.runGamePure S.identityAnswer gs Stack.resolveTop)),
      HU.testCase "CR 601.3: cast Panglacial during Evolving Wilds' search, then it resolves 9/5" $
        let g0 = Setup.emptyGame S.bothPlayers
            (ewId, g1) = S.addCreature (Cards.evolvingWildsPrinting cards) S.alice g0
            g2 = List.foldl' (\g _ -> snd (S.addCreature (Cards.forestPrinting cards) S.alice g)) g1 [1 .. (7 :: Int)]
            (_, g3) = S.addLibraryCard (Cards.panglacialWurmPrinting cards) S.alice g2
            g4 = g3 {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
         in case Projection.abilitiesOf ewId g4 of
              ewAbility : _ ->
                let action = do
                      Activate.activateAbility S.alice ewId ewAbility
                      Stack.resolveTop -- Evolving Wilds' ability: cast Panglacial, then search + shuffle + cease
                      Stack.resolveTop -- Panglacial resolves onto the battlefield
                    after = snd (Engine.runGamePure castPanglacial g4 action)
                 in do
                      HU.assertEqual "Panglacial is a 9/5 on the battlefield" 1 (S.countOnBattlefieldByName (Text.pack "Panglacial Wurm") S.alice after)
                      HU.assertEqual "Panglacial left the library" 0 (S.countByName (Text.pack "Panglacial Wurm") S.alice after)
                      HU.assertEqual "seven Forests tapped for {5}{G}{G}" 7 (S.tappedCount S.alice after)
              [] -> HU.assertFailure "Evolving Wilds should have an activated ability",
      HU.testCase "declining the cast resolves the search normally, Panglacial stays" $
        let g0 = Setup.emptyGame S.bothPlayers
            (ewId, g1) = S.addCreature (Cards.evolvingWildsPrinting cards) S.alice g0
            g2 = List.foldl' (\g _ -> snd (S.addCreature (Cards.forestPrinting cards) S.alice g)) g1 [1 .. (7 :: Int)]
            (_, g3) = S.addLibraryCard (Cards.panglacialWurmPrinting cards) S.alice g2
            g4 = g3 {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
         in case Projection.abilitiesOf ewId g4 of
              ewAbility : _ ->
                let after = snd (Engine.runGamePure S.identityAnswer g4 (do Activate.activateAbility S.alice ewId ewAbility; Stack.resolveTop))
                 in HU.assertEqual "Panglacial still in the library" 1 (S.countByName (Text.pack "Panglacial Wurm") S.alice after)
              [] -> HU.assertFailure "Evolving Wilds should have an activated ability"
    ]

castTests :: Cards.Cards -> Tasty.TestTree
castTests cards =
  Tasty.testGroup
    "Cast"
    [ HU.testCase "a Piker is castable with two Mountains in a main phase" $
        let (gs, oid) = S.pikerInHand cards 2 Phase.PrecombatMain
         in HU.assertBool "castable" (Cast.castable S.alice oid gs),
      HU.testCase "a Piker is not castable with one Mountain" $
        let (gs, oid) = S.pikerInHand cards 1 Phase.PrecombatMain
         in HU.assertBool "unaffordable" (not (Cast.castable S.alice oid gs)),
      HU.testCase "CR 601.3a no creature spell in the upkeep" $
        let (gs, oid) = S.pikerInHand cards 2 (Phase.Beginning BeginningStep.Upkeep)
         in HU.assertBool "wrong timing" (not (Cast.castable S.alice oid gs)),
      HU.testCase "CR 601.3a no creature spell with a non-empty stack" $
        let (gs, oid) = S.pikerInHand cards 2 Phase.PrecombatMain
            busy = gs {GameState.stack = [ObjectId.MkObjectId 999]}
         in HU.assertBool "stack not empty" (not (Cast.castable S.alice oid busy)),
      HU.testCase "CR 601.3a a non-active player cannot cast at sorcery speed" $
        let (gs, oid) = S.pikerInHand cards 2 Phase.PrecombatMain
            bobsTurn = gs {GameState.activePlayer = S.bob}
         in HU.assertBool "not active" (not (Cast.castable S.alice oid bobsTurn)),
      HU.testCase "a Mountain in hand is not castable: lands have no mana cost" $
        HU.assertBool "no cost" $
          not (Cast.castable S.alice (ObjectId.MkObjectId 0) (S.oneMountainState cards Phase.PrecombatMain)),
      HU.testCase "CR 601 casting puts a NEW object on the stack and taps two lands" $
        let (gs, oid) = S.pikerInHand cards 3 Phase.PrecombatMain
            after = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid))
         in do
              HU.assertEqual "stack depth" 1 (length (GameState.stack after))
              HU.assertEqual "hand empty" 0 (S.handSize S.alice after)
              HU.assertEqual "lands tapped" 2 (S.tappedCount S.alice after)
              HU.assertEqual "conserved" (Game.objectCount gs) (Game.objectCount after)
              -- CR 400.7: the card on the stack is a new object, not the old id.
              HU.assertEqual "old id gone" Nothing (Game.lookupObject oid after),
      HU.testCase "the stack object is still a Piker on the stack" $
        let (gs, oid) = S.pikerInHand cards 3 Phase.PrecombatMain
            after = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid))
         in case GameState.stack after of
              [] -> HU.assertFailure "expected one object on the stack"
              top : _ -> case Game.lookupObject top after of
                Nothing -> HU.assertFailure "stack id should resolve"
                Just obj -> do
                  HU.assertEqual "zone" Zone.Stack (Object.zone obj)
                  case Object.source obj of
                    Source.OfCard printing ->
                      HU.assertEqual "name" (Text.pack "Goblin Piker") (Card.Type.name (Printing.card printing))
                    Source.OfToken _ -> HU.assertFailure "expected a card source"
                    Source.OfAbility _ _ -> HU.assertFailure "expected a card source"
                    Source.OfTrigger _ _ -> HU.assertFailure "expected a card source"
                    Source.OfEmblem _ -> HU.assertFailure "expected a card source"
                    Source.OfInherentTrigger _ _ -> HU.assertFailure "expected a card source",
      HU.testCase "CR 117.1a a Bolt is castable outside a main phase" $
        let (gs, oid) = S.boltInHand cards 1 (Phase.Beginning BeginningStep.Upkeep)
         in HU.assertBool "instant speed" (Cast.castable S.alice oid gs),
      HU.testCase "CR 117.1a a Bolt is castable on the opponent's turn" $
        let (gs, oid) = S.boltInHand cards 1 Phase.PrecombatMain
            bobsTurn = gs {GameState.activePlayer = S.bob}
         in HU.assertBool "not my turn, still castable" (Cast.castable S.alice oid bobsTurn),
      HU.testCase "CR 117.1a a Bolt is castable with a non-empty stack" $
        let (gs, oid) = S.boltInHand cards 1 Phase.PrecombatMain
            busy = gs {GameState.stack = [ObjectId.MkObjectId 999]}
         in HU.assertBool "in response" (Cast.castable S.alice oid busy),
      HU.testCase "a Bolt in the graveyard is not castable" $
        let (gs, oid) = S.boltInHand cards 1 Phase.PrecombatMain
            buried = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Graveyard)
         in HU.assertEqual "nothing castable" [] (Cast.castableSpells S.alice buried),
      HU.testCase "CR 601.2c casting a Bolt stamps the chosen target on the stack object" $
        let (base, gs, _) = S.boltAtBobsPiker cards
         in case GameState.stack gs of
              [] -> HU.assertFailure "expected the Bolt on the stack"
              top : _ -> case Game.lookupObject top gs of
                Nothing -> HU.assertFailure "stack id should resolve"
                Just obj -> do
                  HU.assertEqual "one Mountain tapped" 1 (S.tappedCount S.alice gs)
                  HU.assertEqual
                    "the Piker is the target"
                    (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Recipient.ToCreature (S.pikerOf base)))
                    (Binding.targetsOf (Object.bindings obj)),
      HU.testCase "casting a {X}{R} spell at X=3 stamps amount 3 and pays {3}{R}" $
        let (gs0, oid) = S.handOne (Cards.blazePrinting cards) (S.mountainsInPlay cards 4)
            after = snd (Engine.runGamePure answerX3 gs0 (Cast.castSpell S.alice oid))
         in case GameState.stack after of
              [] -> HU.assertFailure "expected the spell on the stack"
              top : _ -> case Game.lookupObject top after of
                Nothing -> HU.assertFailure "stack id should resolve"
                Just obj -> do
                  HU.assertEqual "amount bound" (Just 3) (Binding.amountOf Binding.variableX (Object.bindings obj))
                  HU.assertEqual "four mana spent (paid {3}{R})" 4 (S.tappedCount S.alice after),
      HU.testCase "an illegal target answer makes the cast a no-op" $
        let (gs, oid) = S.boltInHand cards 1 Phase.PrecombatMain
            liar :: Prompt.Prompt r -> r
            liar p = case p of
              Prompt.ChooseTargets _ _ _ sets ->
                Map.map (const (Recipient.ToCreature (ObjectId.MkObjectId 999))) sets
              _ -> S.identityAnswer p
            after = snd (Engine.runGamePure liar gs (Cast.castSpell S.alice oid))
         in do
              HU.assertEqual "nothing on the stack" 0 (length (GameState.stack after))
              HU.assertEqual "nothing paid" 0 (S.tappedCount S.alice after),
      HU.testCase "Panglacial Wurm in the library is castable-while-searching with mana" $
        let base = S.landsInPlay (Cards.forestPrinting cards) 7
            (_, gs) = S.addLibraryCard (Cards.panglacialWurmPrinting cards) S.alice base
         in HU.assertEqual "one castable-while-searching option" 1 (length (Cast.castableWhileSearching S.alice gs)),
      HU.testCase "with too little mana, Panglacial is not castable-while-searching" $
        let base = S.landsInPlay (Cards.forestPrinting cards) 3
            (_, gs) = S.addLibraryCard (Cards.panglacialWurmPrinting cards) S.alice base
         in HU.assertEqual "unaffordable, so no options" 0 (length (Cast.castableWhileSearching S.alice gs)),
      HU.testCase "castWhileSearching casts Panglacial from the library onto the stack" $
        let base = S.landsInPlay (Cards.forestPrinting cards) 7
            (_, gs) = S.addLibraryCard (Cards.panglacialWurmPrinting cards) S.alice base
            after = snd (Engine.runGamePure castFirstOption gs (Cast.castWhileSearching S.alice))
            onStack = length (filter (nameOnStack (Text.pack "Panglacial Wurm") after) (GameState.stack after))
         in do
              HU.assertEqual "Panglacial is on the stack" 1 onStack
              HU.assertEqual "Panglacial left the library" 0 (S.countByName (Text.pack "Panglacial Wurm") S.alice after)
              HU.assertEqual "seven Forests tapped to pay {5}{G}{G}" 7 (S.tappedCount S.alice after),
      HU.testCase "CR 601.2i casting a spell records a SpellCast event for the caster" $
        let (gs, oid) = S.boltInHand cards 1 Phase.PrecombatMain
            after = S.runPure S.identityAnswer gs (Cast.castSpell S.alice oid)
            casts = Maybe.mapMaybe Event.castOf (Foldable.toList (GameState.events after))
         in do
              HU.assertEqual "no cast before" [] (Maybe.mapMaybe Event.castOf (Foldable.toList (GameState.events gs)))
              HU.assertEqual "exactly one cast, by alice" [S.alice] casts,
      HU.testCase "CR 601.2i a cast that is rejected records nothing" $
        -- A Bolt with no mana available: legalActions would never offer it, and
        -- castSpell's payment fails, so no event is recorded.
        let (gs, oid) = S.boltInHand cards 0 Phase.PrecombatMain
            after = S.runPure S.identityAnswer gs (Cast.castSpell S.alice oid)
         in HU.assertEqual "no cast recorded" [] (Maybe.mapMaybe Event.castOf (Foldable.toList (GameState.events after)))
    ]

-- Chooses X=3 and aims every target slot at bob; other prompts take the identity
-- fallback. Casing on a GADT prompt with an identityAnswer default is the liar
-- pattern from the illegal-target test.
answerX3 :: Prompt.Prompt r -> r
answerX3 p = case p of
  Prompt.ChooseX {} -> 3
  Prompt.ChooseTargets _ _ _ sets -> Map.map (const (Recipient.ToPlayer S.bob)) sets
  _ -> S.identityAnswer p

-- Discards from the BACK of hand. Deliberately unlike every fallback, so the
-- CR 514.2 test proves the prompted choice is actually honored.
discardLastAnswer :: Prompt.Prompt r -> r
discardLastAnswer p = case p of
  Prompt.DeclareAttackers {} -> []
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
  Prompt.ChooseDiscard _ _ ids n -> lastN (fromIntegral n) ids
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> Nothing
  Prompt.CastWhileSearching {} -> Nothing
  Prompt.ChooseX {} -> 0
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (take (fromIntegral count) (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.OrderTriggers _ _ sources -> map fromIntegral (take (length sources) [0 :: Int ..])
  Prompt.ChooseReplacement {} -> 0
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (take (fromIntegral count) candidates)
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> take (fromIntegral count) hand

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

-- Bob draws to eight, then discards at cleanup under discardLastAnswer.
bobDiscardChoice :: Cards.Cards -> (GameState.GameState, [ObjectId.ObjectId])
bobDiscardChoice cards =
  let start = Setup.emptyGame S.bothPlayers
      steps = do
        Setup.newGame (S.redRed cards)
        State.modify' $ \gs -> gs {GameState.activePlayer = S.bob, GameState.turnNumber = 2}
        S.drawStep
        beforeCleanup <- State.gets (Game.zoneMembers Zone.Hand S.bob)
        Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)
        pure beforeCleanup
      (held, final) = Engine.runGamePure discardLastAnswer start steps
   in (final, held)

discardTests :: Cards.Cards -> Tasty.TestTree
discardTests cards =
  Tasty.testGroup
    "Discard"
    [ HU.testCase "CR 514.2 discard trims to hand size" $
        HU.assertEqual "hand" 7 (S.handSize S.bob (fst (bobDiscardChoice cards))),
      HU.testCase "CR 514.2 the prompted choice is honored" $
        let (final, held) = bobDiscardChoice cards
            kept = Game.zoneMembers Zone.Hand S.bob final
            -- discardLastAnswer pitched the last card, so the first seven of the
            -- pre-cleanup hand are exactly what survives. Ids are stable here:
            -- the kept cards never changed zones.
            expected = take 7 held
         in HU.assertEqual "kept the front seven" expected kept
    ]

-- Put one card of a printing into alice's hand over an existing board, in a main
-- phase with priority.
handInPlay :: Printing.Printing -> GameState.GameState -> (GameState.GameState, ObjectId.ObjectId)
handInPlay printing board =
  let (oid, g1) = Game.freshObjectId board
      (ts, g2) = Game.freshTimestamp g1
      obj =
        Object.MkObject
          { Object.owner = S.alice,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = ts
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

-- Casts, targeting a permanent (lookupMin picks the lowest ToObject id) and
-- hacking Mountain -> Island.
hackAnswer :: Prompt.Prompt r -> r
hackAnswer p = case p of
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Island)
  _ -> S.identityAnswer p

magicalHackTests :: Cards.Cards -> Tasty.TestTree
magicalHackTests cards =
  Tasty.testGroup
    "MagicalHack"
    [ HU.testCase "CR 612/305.6 a hacked basic Mountain taps for its new color" $
        -- alice: one Mountain to hack and one Island (blue for the {U}), plus a
        -- Magical Hack in hand. The Mountain is added FIRST so it has the lowest
        -- object id and identityAnswer's ChooseTargets (Set.lookupMin over the
        -- ToObject recipients) picks it, not the Island. Hack Mountain -> Island.
        let (mountainId, g0) = S.addCreature (Cards.mountainPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (islandId, g1) = S.addCreature (Cards.islandPrinting cards) S.alice g0
            (gs, hackId) = handInPlay (Cards.magicalHackPrinting cards) g1
            cast = snd (Engine.runGamePure hackAnswer gs (Cast.castSpell S.alice hackId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in do
              HU.assertBool "island untouched, still blue" (elem (ManaType.Colored Color.Blue) (Mana.manaTypesOf islandId resolved))
              HU.assertEqual "hacked Mountain projects Island" (Set.singleton Subtype.Island) (Projection.subtypesOf mountainId resolved)
              HU.assertBool "hacked Mountain taps blue" (elem (ManaType.Colored Color.Blue) (Mana.manaTypesOf mountainId resolved))
              HU.assertBool "hacked Mountain no longer taps red" (notElem (ManaType.Colored Color.Red) (Mana.manaTypesOf mountainId resolved)),
      HU.testCase "CR 601.2c Magical Hack with no legal target is uncastable" $
        let (gs, hackId) = handInPlay (Cards.magicalHackPrinting cards) (Setup.emptyGame S.bothPlayers)
         in -- Empty battlefield and stack: SpellOrPermanentTarget has no legal
            -- recipient (and there is no mana either), so it is uncastable.
            HU.assertBool "no target -> uncastable" (not (Cast.castable S.alice hackId gs))
    ]

-- Aims every target slot at bob and chooses X=0; the X=0 castability floor.
answerX0 :: Prompt.Prompt r -> r
answerX0 p = case p of
  Prompt.ChooseX {} -> 0
  Prompt.ChooseTargets _ _ _ sets -> Map.map (const (Recipient.ToPlayer S.bob)) sets
  _ -> S.identityAnswer p

-- How many Blazes sit in alice's hand (the reject-not-repair no-op check).
blazeInHand :: GameState.GameState -> Int
blazeInHand gs = length (filter (nameOnStack (Text.pack "Blaze") gs) (Game.zoneMembers Zone.Hand S.alice gs))

blazeTests :: Cards.Cards -> Tasty.TestTree
blazeTests cards =
  Tasty.testGroup
    "Blaze"
    [ HU.testCase "Blaze at X=3 deals 3 to the opponent (CR 601.2b/f/h, 608.2)" $
        -- Falsifier: an engine that ignored the chosen value (treated X as 0, or
        -- as the {X} mana value) would leave bob at 20.
        let (gs0, oid) = S.handOne (Cards.blazePrinting cards) (S.mountainsInPlay cards 4)
            after = snd (Engine.runGamePure answerX3 gs0 (do Cast.castSpell S.alice oid; Stack.resolveTop))
         in do
              HU.assertEqual "Bob at 17" (Just 17) (S.lifeOf S.bob after)
              HU.assertEqual "four Mountains paid {3}{R}" 4 (S.tappedCount S.alice after),
      HU.testCase "Blaze at X=0 is castable and deals nothing (the X=0 floor)" $
        -- Falsifier: a floor that required {X} > 0 would make Blaze uncastable off
        -- one Mountain, leaving it in hand.
        let (gs0, oid) = S.handOne (Cards.blazePrinting cards) (S.mountainsInPlay cards 1)
            after = snd (Engine.runGamePure answerX0 gs0 (do Cast.castSpell S.alice oid; Stack.resolveTop))
         in do
              HU.assertEqual "Bob unharmed" (Just 20) (S.lifeOf S.bob after)
              HU.assertEqual "one Mountain paid {R}" 1 (S.tappedCount S.alice after)
              HU.assertEqual "Blaze resolved out of hand" 0 (blazeInHand after),
      HU.testCase "Blaze at an unaffordable X is a no-op (reject-not-repair)" $
        let (gs0, oid) = S.handOne (Cards.blazePrinting cards) (S.mountainsInPlay cards 1)
            after = snd (Engine.runGamePure answerX3 gs0 (Cast.castSpell S.alice oid))
         in do
              HU.assertEqual "still in hand" 1 (blazeInHand after)
              HU.assertEqual "no mana spent" 0 (S.tappedCount S.alice after)
              HU.assertEqual "Bob unharmed" (Just 20) (S.lifeOf S.bob after)
    ]

-- CR 700.2a: an illegal mode can't be chosen, so a modal spell is castable when
-- at least `count` of its modes are fillable -- not when every mode's slots are.
-- Chaos Charm has three modes (destroy target Wall / damage target creature /
-- give target creature haste); the falsifier is castability via the damage or
-- haste mode with no Wall on the board at all.
modalCastTests :: Cards.Cards -> Tasty.TestTree
modalCastTests cards =
  Tasty.testGroup
    "ModalCast"
    [ HU.testCase "CR 700.2a Chaos Charm is castable off its non-Wall modes with no Wall in play" $
        let (gs0, oid) = S.handOne (Cards.chaosCharmPrinting cards) (S.mountainsInPlay cards 1)
            (_, gs1) = S.addPiker cards S.alice gs0
         in HU.assertBool "castable via the damage/haste mode" (Cast.castable S.alice oid gs1),
      HU.testCase "CR 700.2a Chaos Charm is not castable with no creature on the board at all" $
        let (gs0, oid) = S.handOne (Cards.chaosCharmPrinting cards) (S.mountainsInPlay cards 1)
         in HU.assertBool "no mode is fillable" (not (Cast.castable S.alice oid gs0))
    ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Cast"
    [castTests cards, castEngineTests cards, stackTests cards, discardTests cards, sicknessTests cards, magicalHackTests cards, blazeTests cards, modalCastTests cards]

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
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter S.isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> Nothing
  Prompt.ChooseX {} -> 0
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (take (fromIntegral count) (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.OrderTriggers _ _ sources -> map fromIntegral (take (length sources) [0 :: Int ..])
  Prompt.ChooseReplacement {} -> 0
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (take (fromIntegral count) candidates)
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> take (fromIntegral count) hand

nameOnStack :: Text.Text -> GameState.GameState -> ObjectId.ObjectId -> Bool
nameOnStack wanted gs oid = case Game.lookupObject oid gs of
  Just o -> case Object.source o of
    Source.OfCard printing -> Card.Type.name (Printing.card printing) == wanted
    Source.OfToken card -> Card.Type.name card == wanted
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
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter S.isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.ChooseX {} -> 0
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (take (fromIntegral count) (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.OrderTriggers _ _ sources -> map fromIntegral (take (length sources) [0 :: Int ..])
  Prompt.ChooseReplacement {} -> 0
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (take (fromIntegral count) candidates)
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> take (fromIntegral count) hand
