{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Trigger over leaves-the-battlefield triggers and their look-back
-- (CR 603.10, CR 400.7e): returning to hand, the card a permanent became,
-- intervening ifs over last known information, undying, afterlife, and the
-- bystander cases from Fabricate to Ivory Gargoyle. Split out of
-- Pawl.ZoneTriggerSpec, which keeps the machinery.
module Pawl.LeavesTriggerSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Event.Binding as Event
import qualified Pawl.Engine.Event.Trigger as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.PermanentSacrificed as PermanentSacrificed
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange
import Pawl.ZoneTriggerSpec (everyTriggerCondition, gathered, paysFor, representativeDeparted, representativeEvents)

-- CR 603.6c's first written form read by a BYSTANDER -- Super Shredder {1}{B}
-- Legendary Creature -- Mutant Ninja Human 1/1, "Whenever another permanent
-- leaves the battlefield, put a +1/+1 counter on Super Shredder."
--
-- leavesBattlefieldSpec above is the SELF-scoped half of the same rule, and
-- permanentDiesSpec is this same bystander scoping one rule narrower. What this
-- group has to prove that neither of those does is that the watcher sees a
-- departure it had no part in, to a destination CR 700.4 does not reach: the
-- printed word "another" is Not IsSource in the condition's own Filter, and
-- nothing else narrows it -- "permanent" is no predicate here, since
-- matchesTrigger has already required the battlefield as the origin.
--
-- The bearer is bob's, the departing permanent alice's, so a condition that had
-- quietly read "you control" would answer these cases 0.
permanentLeavesTheBattlefieldSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
permanentLeavesTheBattlefieldSpec s registry =
  let -- bob's Super Shredder watching, alice's Goblin Piker as the victim, and
      -- `lands` of alice's to pay with. The Shredder is bob's so that no case
      -- here can pass on a controller check the condition does not make.
      shredderBoard lands = do
        shredder <- S.printingOf s registry "Super Shredder"
        piker <- S.printingOf s registry "Goblin Piker"
        let (shredderId, withShredder) = S.addCreature shredder S.bob lands
            (pikerId, gs) = S.addCreature piker S.alice withShredder
        pure (shredderId, pikerId, gs)
      -- Cast the one spell in hand at `oid`, resolve it, settle (CR 117.5 is
      -- where the scan sees the departure), then resolve the trigger the settle
      -- placed. Aimed by ID: S.identityAnswer takes the least Recipient, and
      -- with two creatures on the board that is whichever id sorts first rather
      -- than the one the case is about.
      castAt :: ObjectId.ObjectId -> (GameState.GameState, ObjectId.ObjectId) -> (GameState.GameState, GameState.GameState)
      castAt oid (gs, spellId) =
        let answer :: Prompt.Prompt r -> r
            answer p = case p of
              -- FILTERED rather than built: which Recipient constructor a pool
              -- offers is the pool's business (Angelic Edict's is Permanents,
              -- Unsummon's is Creatures), and a hand-built one of the other
              -- shape is silently dropped at CR 608.2b's re-read.
              Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just oid) . Recipient.objectOf) . snd) sets
              _ -> S.identityAnswer p
            cast = S.runPure answer gs (S.cast S.alice spellId)
            resolved = S.runPure answer cast Stack.resolveTop
            settled = S.runPure answer resolved Engine.settleForPriority
         in (settled, S.runPure answer settled Stack.resolveTop)
      -- CR 113.7a's source id for every triggered ability currently on the stack.
      triggerSourcesOn gs =
        Maybe.mapMaybe
          ( \oid -> case fmap Object.source (Game.lookupObject oid gs) of
              Just (Source.OfTrigger triggered) -> Just (TriggeredAbilitySource.source triggered)
              _ -> Nothing
          )
          (GameState.stack gs)
   in Spec.describe s "PermanentLeavesTheBattlefield" $ do
        -- The gameplay-level proof, and the destination that makes this
        -- condition rather than PermanentDies the one under test: CR 400.2
        -- makes exile a public zone that is not a graveyard, so a dies trigger
        -- would stay silent here.
        Spec.it s "CR 603.6c whole card: alice's Piker is exiled and bob's Super Shredder grows" $ do
          plains <- S.printingOf s registry "Plains"
          edict <- S.printingOf s registry "Angelic Edict"
          (shredderId, pikerId, board) <- shredderBoard (S.landsInPlay plains 5)
          let (settled, after) = castAt pikerId (S.handOne edict board)
          Spec.assertEqWith s "the Shredder is a 1/1 before anything leaves" (Projection.powerOf shredderId board, Projection.toughnessOf shredderId board) (Just 1, Just 1)
          Spec.assertEqWith s "the Piker really was exiled, not destroyed" (fmap Object.zone (Game.lookupObject pikerId settled)) Nothing
          Spec.assertEqWith s "and it is bob's Shredder that grew, off alice's permanent" (Projection.powerOf shredderId after, Projection.toughnessOf shredderId after) (Just 2, Just 2)
          Spec.assertEqWith s "one counter, from one departure" (fmap (Map.lookup CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject shredderId after)) (Just (Just 1))
        -- CR 400.2's hidden half of the same rule: a bounce reaches a HAND, and
        -- the watcher still sees it. The bearer reads the departing permanent
        -- from CR 608.2h last known information, which is what makes this
        -- answerable at all -- there is no public incarnation to read.
        Spec.it s "CR 603.6c whole card: Unsummon bounces the Piker to a hidden zone and the Shredder still grows" $ do
          island <- S.printingOf s registry "Island"
          unsummon <- S.printingOf s registry "Unsummon"
          (shredderId, pikerId, board) <- shredderBoard (S.landsInPlay island 1)
          let (settled, after) = castAt pikerId (S.handOne unsummon board)
          Spec.assertEqWith s "the Piker is in its owner's hand" (Game.lookupObject pikerId settled) Nothing
          Spec.assertEqWith s "the Shredder grew on a departure to a hidden zone" (Projection.powerOf shredderId after, Projection.toughnessOf shredderId after) (Just 2, Just 2)
        -- CR 603.10a's look-back, which this condition needs for the reason
        -- PermanentDies needs it and one step further: the WATCHER can be gone
        -- too. alice's Day of Judgment destroys her Piker and bob's Shredder in
        -- one CR 704.3 batch, so by the CR 117.5 boundary the ability's own
        -- bearer is a card in a graveyard -- and CR 603.10a reads "the existence
        -- of those abilities ... immediately prior to the event", so it triggers
        -- anyway.
        --
        -- The stack is what this case can read: the counter the trigger puts on
        -- "Super Shredder" lands on a card in a graveyard and changes nothing
        -- observable, so what the rule decides here is whether the ability
        -- reached the stack at all. ONE trigger, not two -- the Shredder's own
        -- departure is in the same batch and the printed "another" declines it.
        Spec.it s "CR 603.10a a Shredder swept alongside the Piker still sees the Piker go" $ do
          plains <- S.printingOf s registry "Plains"
          dayOfJudgment <- S.printingOf s registry "Day of Judgment"
          (shredderId, _, board) <- shredderBoard (S.landsInPlay plains 4)
          let (withSpell, spell) = S.handOne dayOfJudgment board
              afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
              swept = S.runPure S.identityAnswer afterCast Stack.resolveTop
              settled = S.runPure S.identityAnswer swept Engine.settleForPriority
          Spec.assertEqWith s "the sweep left no creatures, the watcher included" (Set.size (Set.filter (`Projection.isCreatureOf` settled) (GameState.battlefield settled))) 0
          -- CR 113.7a: the placed ability's source is the id the Shredder had on
          -- the battlefield, which is what makes this a statement about WHOSE
          -- ability reached the stack rather than about how many did.
          Spec.assertEqWith s "the Shredder's own ability still reached the stack, exactly once" (triggerSourcesOn settled) [shredderId]

        -- The printed "another", with the two Filters side by side on ONE
        -- departure so the exclusion is the only thing that differs. Without it
        -- a Super Shredder that left the battlefield would see itself go.
        Spec.it s "CR 603.6c the printed \"another\" is the only thing declining the bearer's own departure" $ do
          plains <- S.printingOf s registry "Plains"
          (shredderId, _, board) <- shredderBoard (S.landsInPlay plains 0)
          let gone = S.runPure S.identityAnswer board (Event.changeZone shredderId Zone.Exile)
              moves = filter (\e -> case e of GameEvent.Moved {} -> True; _ -> False) (S.eventsOf gone)
          case moves of
            [departure] -> do
              Spec.assertBool s (Event.matchesTrigger gone shredderId S.bob (TriggerCondition.PermanentLeavesTheBattlefield (Filter.Type.And [])) departure) "a Filter without the exclusion admits the Shredder's own departure"
              Spec.assertBool s (not (Event.matchesTrigger gone shredderId S.bob (TriggerCondition.PermanentLeavesTheBattlefield (Filter.Type.Not Filter.Type.IsSource)) departure)) "so the printed \"another\" is the only thing declining it"
            other -> Spec.assertFailure s ("expected exactly one zone change, got " <> show (length other))

-- CR 603.6c's leaves-the-battlefield family with the DESTINATION named --
-- Justice, Vance Astrovik {2}{U} Legendary Creature -- Mutant Hero 2/2,
-- "Whenever another nonland permanent you control is returned to its owner's
-- hand, put a +1/+1 counter on Justice."
--
-- permanentLeavesTheBattlefieldSpec above is the same bystander scoping with no
-- destination at all, and telling the two apart is what this group exists for:
-- a permanent DESTROYED fires that condition and must not fire this one. The
-- pair of boards below differ in the destination and in nothing else.
--
-- The second thing it proves is CR 603.2c's scoping. The printed singular is one
-- occurrence per permanent, so one spell returning two of them triggers twice --
-- unlike PermanentsDie's "one or more", which Event.batchScoped collapses.
permanentReturnedToHandSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
permanentReturnedToHandSpec s registry =
  let -- alice's Justice watching, and a Goblin Piker for each entry in `owners`.
      -- Justice is alice's because the printed condition says "you control", so a
      -- Piker of bob's is the board that tells that clause apart from no clause.
      justiceBoard lands owners = do
        justice <- S.printingOf s registry "Justice, Vance Astrovik"
        piker <- S.printingOf s registry "Goblin Piker"
        let (justiceId, withJustice) = S.addCreature justice S.alice lands
            step (ids, gs) owner = let (oid, gs') = S.addCreature piker owner gs in (ids <> [oid], gs')
            (pikerIds, board) = Foldable.foldl' step ([], withJustice) owners
        pure (justiceId, pikerIds, board)
      -- Move one permanent to `zone` by hand, let CR 117.5's scan place whatever
      -- triggered, then resolve it. The narrowest path that shows the behaviour:
      -- no spell, no targeting, so the destination is the only variable.
      moveTo oid zone gs =
        let gone = S.runPure S.identityAnswer gs (Event.changeZone oid zone)
            settled = S.runPure S.identityAnswer gone Engine.settleForPriority
         in S.runPure S.identityAnswer settled Stack.resolveTop
      -- How big Justice is, which is the whole of what the +1/+1 counter is
      -- observable as. Read by ID, and the id is safe here: Justice never moves
      -- in these cases, so CR 400.7's fresh id for the RETURNED permanent is
      -- never the one being asked about.
      sizeOf oid gs = (Projection.powerOf oid gs, Projection.toughnessOf oid gs)
      -- CR 113.7a's source id for every triggered ability currently on the stack.
      triggerSourcesOn gs =
        Maybe.mapMaybe
          ( \oid -> case fmap Object.source (Game.lookupObject oid gs) of
              Just (Source.OfTrigger triggered) -> Just (TriggeredAbilitySource.source triggered)
              _ -> Nothing
          )
          (GameState.stack gs)
   in Spec.describe s "PermanentReturnedToHand" $ do
        -- The whole card through a real spell: alice casts Unsummon at her own
        -- Piker. Targeted by ID -- S.identityAnswer takes the least Recipient,
        -- and with two creatures out that is whichever id sorts first rather
        -- than the one this case is about.
        Spec.it s "CR 603.6c whole card: Unsummon returns alice's Piker and Justice grows" $ do
          island <- S.printingOf s registry "Island"
          unsummon <- S.printingOf s registry "Unsummon"
          (justiceId, pikerIds, board) <- justiceBoard (S.landsInPlay island 1) [S.alice]
          case pikerIds of
            [pikerId] -> do
              let (withSpell, spellId) = S.handOne unsummon board
                  answer :: Prompt.Prompt r -> r
                  answer p = case p of
                    Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just pikerId) . Recipient.objectOf) . snd) sets
                    _ -> S.identityAnswer p
                  cast = S.runPure answer withSpell (S.cast S.alice spellId)
                  resolved = S.runPure answer cast Stack.resolveTop
                  settled = S.runPure answer resolved Engine.settleForPriority
                  after = S.runPure answer settled Stack.resolveTop
              Spec.assertEqWith s "Justice grew on the Piker's return" (sizeOf justiceId after) (Just 3, Just 3)
              Spec.assertEqWith s "one counter, from one return" (fmap (Map.lookup CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject justiceId after)) (Just (Just 1))
              Spec.assertEqWith s "the Piker really left the battlefield" (Game.lookupObject pikerId settled) Nothing
              Spec.assertEqWith s "Justice was a 2/2 before it went" (sizeOf justiceId board) (Just 2, Just 2)
            other -> Spec.assertFailure s ("expected one Piker, got " <> show (length other))
        -- The destination, and nothing else. ONE board, moved two ways: a hand
        -- fires this condition and a graveyard does not, which is what a Filter
        -- over the object alone could never say -- CR 603.6c admits every zone
        -- but the battlefield, so PermanentLeavesTheBattlefield takes the two
        -- alike and only a condition naming the destination can tell them apart.
        Spec.it s "CR 603.6c a Piker destroyed rather than returned leaves Justice a 2/2" $ do
          island <- S.printingOf s registry "Island"
          (justiceId, pikerIds, board) <- justiceBoard (S.landsInPlay island 1) [S.alice]
          case pikerIds of
            [pikerId] -> do
              Spec.assertEqWith s "the graveyard leaves Justice a 2/2" (sizeOf justiceId (moveTo pikerId Zone.Graveyard board)) (Just 2, Just 2)
              Spec.assertEqWith s "the hand, on the same board, makes it a 3/3" (sizeOf justiceId (moveTo pikerId Zone.Hand board)) (Just 3, Just 3)
            other -> Spec.assertFailure s ("expected one Piker, got " <> show (length other))
        -- The printed "nonland", which is Not (HasCardType Land) inside the
        -- condition's own Filter and the one clause the destination does not
        -- already cover: an Island bounced to hand is returned to its owner's
        -- hand every bit as much as the Piker beside it on this board.
        Spec.it s "CR 603.6c an Island returned to hand leaves Justice a 2/2" $ do
          island <- S.printingOf s registry "Island"
          (justiceId, pikerIds, board) <- justiceBoard (S.landsInPlay island 1) [S.alice]
          let lands = Set.toList (Set.filter (Set.member CardType.Land . (`Projection.cardTypesOf` board)) (GameState.battlefield board))
          case (lands, pikerIds) of
            ([landId], [pikerId]) -> do
              Spec.assertEqWith s "the Island leaves Justice a 2/2" (sizeOf justiceId (moveTo landId Zone.Hand board)) (Just 2, Just 2)
              Spec.assertEqWith s "the Piker, on the same board and the same destination, makes it a 3/3" (sizeOf justiceId (moveTo pikerId Zone.Hand board)) (Just 3, Just 3)
            other -> Spec.assertFailure s ("expected one Island and one Piker, got " <> show other)
        -- The printed "you control", read from CR 608.2h last known information:
        -- by the time CR 117.5's scan runs the Piker is a card in a hand, which
        -- CR 108.4 gives no controller at all. Same board shape as the case
        -- above, the owner of the Piker the only difference.
        Spec.it s "CR 603.10a bob's Piker returned to hand leaves alice's Justice a 2/2" $ do
          island <- S.printingOf s registry "Island"
          (justiceId, bobPikers, bobBoard) <- justiceBoard (S.landsInPlay island 1) [S.bob]
          (aliceJusticeId, alicePikers, aliceBoard) <- justiceBoard (S.landsInPlay island 1) [S.alice]
          case (bobPikers, alicePikers) of
            ([bobPiker], [alicePiker]) -> do
              Spec.assertEqWith s "bob's Piker is not one alice controls" (sizeOf justiceId (moveTo bobPiker Zone.Hand bobBoard)) (Just 2, Just 2)
              Spec.assertEqWith s "alice's is, on the board that differs in nothing else" (sizeOf aliceJusticeId (moveTo alicePiker Zone.Hand aliceBoard)) (Just 3, Just 3)
            other -> Spec.assertFailure s ("expected one Piker each, got " <> show other)
        -- CR 603.2c: "it can trigger repeatedly if one event contains multiple
        -- occurrences". Evacuation returns every creature in one resolution, so
        -- two of alice's Pikers are two occurrences and Justice's ability
        -- triggers twice -- where PermanentsDie's batch scoping would place one.
        --
        -- Counted on the STACK rather than in counters: Evacuation takes Justice
        -- too, so what its own triggers resolve onto is a card in a hand. CR
        -- 603.10a is what lets them trigger at all, and CR 113.7a's source id is
        -- the one Justice had on the battlefield.
        Spec.it s "CR 603.2c Evacuation returns two Pikers and Justice triggers twice" $ do
          island <- S.printingOf s registry "Island"
          evacuation <- S.printingOf s registry "Evacuation"
          (justiceId, pikerIds, board) <- justiceBoard (S.landsInPlay island 5) [S.alice, S.alice]
          let (withSpell, spellId) = S.handOne evacuation board
              cast = S.runPure S.identityAnswer withSpell (S.cast S.alice spellId)
              resolved = S.runPure S.identityAnswer cast Stack.resolveTop
              settled = S.runPure S.identityAnswer resolved Engine.settleForPriority
          Spec.assertEqWith s "Justice's ability reached the stack once per Piker" (length (filter (== justiceId) (triggerSourcesOn settled))) 2
          Spec.assertEqWith s "and Evacuation really emptied the battlefield of creatures" (Set.size (Set.filter (`Projection.isCreatureOf` settled) (GameState.battlefield settled))) 0
          Spec.assertEqWith s "two Pikers were on the board to be returned" (length pikerIds) 2

-- CR 603.2c's FIRST sentence on the event the group above proves the second
-- for: "whenever ONE OR MORE noncreature permanents are returned to hand" fires
-- once for the batch however many moved --
-- TriggerCondition.PermanentsReturnedToHand, beside PermanentReturnedToHand the
-- way PermanentsDie stands beside PermanentDies.
--
-- Tameshi, Reality Architect {2}{U} Legendary Creature -- Moonfolk Wizard 2/3 is
-- the card (data/cards/tameshi-reality-architect.json): "Whenever one or more
-- noncreature permanents are returned to hand, draw a card. This ability
-- triggers only once each turn. {X}{W}, Return a land you control to its
-- owner's hand: Return target artifact or enchantment card with mana value X or
-- less from your graveyard to the battlefield. Activate only as a sorcery."
-- Name, cost, type line and Oracle text checked against Scryfall 2026-09-03.
-- The activated ability is authored: its target's "mana value X or less" is a
-- slot bound reading the announced X, and Pawl.ActivateSpec's "CR 601.2c/602.2b
-- whole card: Tameshi at X=1 returns the mana value 1 artifact card" is what
-- proves the value reaches it. Retract {U} Instant, "Return all artifacts you
-- control to their owner's hand" (data/cards/retract.json, same check), is the
-- one effect returning two: CR 608.2f's sweep, which Pawl.Engine.Resolve
-- brackets as one Pawl.Types.EventGroup.
--
-- WHY A SYNTHETIC BESIDE THE PRINTING. Tameshi's "triggers only once each turn"
-- makes the batch reading indistinguishable from the per-permanent one on any
-- board: two triggers with the second declined by the limit and one trigger
-- leave the same game. Scryfall o:"one or more" o:"returned to", 2026-09-02,
-- matches Tameshi alone, and o:/whenever one or more [^.]* hands?/ adds no
-- other returned-to-hand printing; a printing of Tameshi's condition without
-- the rider is the card that refutes Synthetic Return Ledger {1}{U}
-- Enchantment, "Whenever one or more noncreature permanents are returned to
-- hand, draw a card" (data/cards/synthetic-return-ledger.json) -- CR 603.2c's
-- first sentence, which nothing in the CR forbids printing bare.
--
-- The discrimination is alice's hand once Retract has returned two artifacts:
-- four cards (both artifacts, Tameshi's one draw and the Ledger's one) is the
-- batch reading, five is the per-permanent one -- the Ledger drawing twice
-- where Tameshi's limit hides the second -- and two is silence. Justice, Vance
-- Astrovik stands on the same board reading the same event the singular way and
-- grows by two -- the two readings side by side off one event group, which the
-- group count pins as the precondition the way permanentsDieSpec does.
permanentsReturnedToHandSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
permanentsReturnedToHandSpec s registry =
  let -- alice: Tameshi, the Ledger, Justice, Sol Ring, Bonesplitter and a Goblin
      -- Piker on the battlefield, one Island for Retract, Retract alone in hand,
      -- and a library to draw from.
      board = do
        tameshi <- S.printingOf s registry "Tameshi, Reality Architect"
        ledger <- S.printingOf s registry "Synthetic Return Ledger"
        justice <- S.printingOf s registry "Justice, Vance Astrovik"
        solRing <- S.printingOf s registry "Sol Ring"
        bonesplitter <- S.printingOf s registry "Bonesplitter"
        piker <- S.printingOf s registry "Goblin Piker"
        island <- S.printingOf s registry "Island"
        retract <- S.printingOf s registry "Retract"
        let (tameshiId, g0) = S.addCreature tameshi S.alice (S.landsInPlay island 1)
            (_, g1) = S.addCreature ledger S.alice g0
            (justiceId, g2) = S.addCreature justice S.alice g1
            (solRingId, g3) = S.addCreature solRing S.alice g2
            (bonesplitterId, g4) = S.addCreature bonesplitter S.alice g3
            (pikerId, g5) = S.addCreature piker S.alice g4
            stocked = List.foldl' (\gs _ -> snd (S.addLibraryCard island S.alice gs)) g5 [1 .. 3 :: Int]
            (withRetract, retractId) = S.handOne retract stocked
        pure (tameshiId, justiceId, solRingId, bonesplitterId, pikerId, retractId, withRetract)
      resolveWholeStack gs =
        if null (GameState.stack gs)
          then gs
          else resolveWholeStack (S.runPure S.identityAnswer gs Stack.resolveTop)
      -- Move one permanent to hand by hand, let CR 117.5's scan place what
      -- triggered, and resolve the whole stack.
      returnByHand oid gs =
        let gone = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Hand)
         in resolveWholeStack (S.runPure S.identityAnswer gone Engine.settleForPriority)
      sizeOf oid gs = (Projection.powerOf oid gs, Projection.toughnessOf oid gs)
      -- The distinct EventGroups the log's battlefield-to-hand moves carry.
      returnGroups gs =
        List.nub
          ( Maybe.mapMaybe
              ( \logged -> case LoggedEvent.event logged of
                  GameEvent.Moved (Moved.MkMoved zc _ _) | ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc == Zone.Hand -> Just (LoggedEvent.group logged)
                  _ -> Nothing
              )
              (Foldable.toList (GameState.events gs))
          )
   in Spec.describe s "PermanentsReturnedToHand" $ do
        -- The proving case: Retract returns two artifacts as one event group.
        Spec.it s "CR 603.2c Retract returning two artifacts draws one card each for Tameshi and the Ledger, and grows Justice twice" $ do
          (_, justiceId, _, _, _, retractId, withRetract) <- board
          let cast = S.runPure S.identityAnswer withRetract (S.cast S.alice retractId)
              resolved = S.runPure S.identityAnswer cast Stack.resolveTop
              settled = S.runPure S.identityAnswer resolved Engine.settleForPriority
              after = resolveWholeStack settled
          Spec.assertEqWith s "alice holds the two artifacts and ONE draw each from Tameshi and the Ledger, not two from the Ledger" (S.handSize S.alice after) 4
          Spec.assertEqWith s "Justice grew once per artifact, the singular reading of the same event" (sizeOf justiceId after) (Just 4, Just 4)
          Spec.assertEqWith s "CR 608.2f: the two returns were one event group" (length (returnGroups settled)) 1
          Spec.assertEqWith s "four triggers reached the stack: Tameshi's and the Ledger's once each, Justice's twice" (length (GameState.stack settled)) 4
          Spec.assertEqWith s "alice's hand was Retract alone before" (S.handSize S.alice withRetract) 1
        -- The printed "noncreature", the condition's own Filter: one board, two
        -- permanents returned one at a time, and only the artifact draws.
        Spec.it s "CR 603.2c a creature returned to hand draws nothing where an artifact does" $ do
          (_, _, solRingId, _, pikerId, _, withRetract) <- board
          Spec.assertEqWith s "the Piker's return leaves alice with Retract and the Piker" (S.handSize S.alice (returnByHand pikerId withRetract)) 2
          Spec.assertEqWith s "the Sol Ring's, on the same board, adds Tameshi's draw and the Ledger's" (S.handSize S.alice (returnByHand solRingId withRetract)) 4
        -- The printed "only once each turn", Pawl.Types.TriggerLimit's
        -- OncePerTurn on Tameshi's condition: a second batch in the same turn is a
        -- second trigger event (permanentsDieSpec's point), which the Ledger
        -- answers again and Tameshi's limit declines.
        Spec.it s "CR 603.2c a second batch in the same turn draws for the Ledger and not for Tameshi" $ do
          (_, _, solRingId, bonesplitterId, _, _, withRetract) <- board
          let first = returnByHand solRingId withRetract
          Spec.assertEqWith s "the second return adds the Bonesplitter and the Ledger's draw, not Tameshi's" (S.handSize S.alice (returnByHand bonesplitterId first)) 6
          Spec.assertEqWith s "the first drew for both" (S.handSize S.alice first) 4
        -- CR 603.10a: a leaves-the-battlefield ability looks back, so Tameshi
        -- swept up in the same batch as the Sol Ring still sees it go and still
        -- draws -- Event.looksBack's batch arm, PermanentsDie's own Example in
        -- that rule. Bracketed by hand, the narrowest path that makes the two
        -- returns one event group.
        Spec.it s "CR 603.10a Tameshi returned in the same batch as an artifact still draws" $ do
          (tameshiId, _, solRingId, _, _, _, withRetract) <- board
          let gone = S.runPure S.identityAnswer withRetract (Event.simultaneously (Event.changeZone tameshiId Zone.Hand >> Event.changeZone solRingId Zone.Hand))
              after = resolveWholeStack (S.runPure S.identityAnswer gone Engine.settleForPriority)
          Spec.assertEqWith s "alice holds Retract, Tameshi, the Sol Ring, Tameshi's draw and the Ledger's" (S.handSize S.alice after) 5
          Spec.assertEqWith s "CR 608.2f: the two returns were one event group" (length (returnGroups gone)) 1

-- CR 603.10a's third look-back family: Kishla Skimmer {G}{U} Creature -- Bird
-- Scout, 2/2, "Flying / Whenever a card leaves your graveyard during your turn,
-- draw a card. This ability triggers only once each turn."
-- (data/cards/kishla-skimmer.json; name, cost, type line and Oracle text checked
-- against Scryfall 2026-09-04.)
--
-- Reassembling Skeleton supplies the departure -- "{1}{B}: Return this card from
-- your graveyard to the battlefield tapped" -- because it is one activation from
-- the graveyard at instant speed, which is what lets all three boards below share
-- an event and differ in one thing each: the second in whose turn it is, the
-- third in whose graveyard the card left.
--
-- THE LOOK-BACK is the point of the first case. CR 400.7 minted the returned card
-- a fresh battlefield id and the graveyard no longer holds anything, so a
-- condition that scanned the graveyard for the card would find nothing; the
-- assertion that the Skeleton is standing on the battlefield when the draw
-- happens is what says the trigger read the departure rather than the zone.
kishlaSkimmerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
kishlaSkimmerSpec s registry =
  let settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority
      resolveTop gs = S.runPure S.identityAnswer gs Stack.resolveTop
      skeletonName = CardName.MkCardName (Text.pack "Reassembling Skeleton")
      -- alice's Kishla Skimmer on the battlefield and one card in her library, so
      -- a draw is observable and CR 104.3c cannot decide the game first. The
      -- Skeleton and the two Swamps that pay for it belong to `owner`, whose turn
      -- it is when `active` says so.
      board skimmer skeleton swamp forest active owner =
        let base =
              (S.landsFor swamp owner 2 (Setup.emptyGame S.bothPlayers))
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = active,
                  GameState.priority = Just owner
                }
            withSkimmer = snd (S.addCreature skimmer S.alice base)
         in S.addGraveyardCard skeleton owner (snd (S.addLibraryCard forest S.alice withSkimmer))
      -- Activate the Skeleton's graveyard ability, resolve it so the card leaves
      -- the graveyard, settle so CR 603.3 places whatever triggered, and resolve
      -- that too.
      raise active owner k = do
        skimmer <- S.printingOf s registry "Kishla Skimmer"
        skeleton <- S.printingOf s registry "Reassembling Skeleton"
        swamp <- S.printingOf s registry "Swamp"
        forest <- S.printingOf s registry "Forest"
        let (gyId, staged) = board skimmer skeleton swamp forest active owner
        case Activate.abilitiesFor gyId staged of
          [ability] ->
            let returned = resolveTop (S.runPure S.identityAnswer staged (Activate.activateAbility owner gyId ability))
             in k returned (resolveTop (settle returned))
          abilities -> Spec.assertEqWith s "exactly one ability to activate" (length abilities) 1
   in Spec.describe s "CardLeavesGraveyard" $ do
        -- The proving case.
        Spec.it s "CR 603.10a a card leaving alice's graveyard on her own turn draws her a card"
          . raise S.alice S.alice
          $ \returned after -> do
            Spec.assertEqWith s "CR 603.10a the Skimmer's trigger drew alice a card" (S.handSize S.alice after) 1
            -- The preconditions, AFTER the assertion so neither can absorb a
            -- mutation aimed at the condition.
            Spec.assertEqWith s "the card that left is on the battlefield, not in the graveyard the trigger would have scanned" (S.countOnBattlefieldByName skeletonName S.alice returned) 1
            Spec.assertEqWith s "nothing was in alice's hand before the trigger resolved" (S.handSize S.alice returned) 0
            Spec.assertEqWith s "and the trigger was the one object on the stack" (length (GameState.stack (settle returned))) 1
        -- "During your turn", one difference from the case above: bob is the
        -- active player, and alice activates her own Skeleton at instant speed.
        Spec.it s "CR 603.10a the same departure on bob's turn draws nothing"
          . raise S.bob S.alice
          $ \returned after -> do
            Spec.assertEqWith s "the Skimmer's trigger did not fire, so alice drew nothing" (S.handSize S.alice after) 0
            Spec.assertEqWith s "off the same departure the case above draws on" (S.countOnBattlefieldByName skeletonName S.alice returned) 1
            Spec.assertEqWith s "and nothing reached the stack" (length (GameState.stack (settle returned))) 0
        -- "Your graveyard", one difference from the proving case: the card leaves
        -- bob's graveyard, on alice's turn, while alice's Skimmer watches.
        Spec.it s "CR 400.3 a card leaving bob's graveyard on alice's turn draws nothing"
          . raise S.alice S.bob
          $ \returned after -> do
            Spec.assertEqWith s "the Skimmer watches its controller's graveyard alone, so alice drew nothing" (S.handSize S.alice after) 0
            Spec.assertEqWith s "off the same departure, out of bob's graveyard" (S.countOnBattlefieldByName skeletonName S.bob returned) 1
            Spec.assertEqWith s "and nothing reached the stack" (length (GameState.stack (settle returned))) 0

-- What PermanentReturnedToHand's bindings are FOR -- Warped Devotion {2}{B}
-- Enchantment, "Whenever a permanent is returned to a player's hand, that
-- player discards a card" (data/cards/warped-devotion.json; name, cost, type
-- line and Oracle text checked against Scryfall 2026-09-02). "That player" is
-- Binding.triggerPlayer, which Event.eventBindings stamps with the returned
-- permanent's OWNER: CR 400.3 sends it to its owner's hand whoever controlled
-- it, and a stolen permanent is what tells the owner apart from CR 109.5's
-- controller.
--
-- The discrimination is WHOSE graveyard grows. bob's Piker under alice's
-- control goes back to bob's hand and bob discards; a controller reading
-- (PlayerRef.ControllerOfBound) would have alice discard, and the board that
-- differs in the Piker's owner alone shows alice discarding for her own.
warpedDevotionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
warpedDevotionSpec s registry =
  let -- alice's Warped Devotion and one Island; a Goblin Piker owned by `owner`,
      -- under alice's control when `stolen`; three cards in alice's hand, and
      -- enough in bob's that CR 701.9b's choice is a real prompt once the Piker
      -- has landed there (or not, on the token leg).
      -- `asToken` mints a CR 111.1 token copy of the Piker in place of the card.
      -- That is the leg where the returned object is GONE by the time the
      -- trigger is placed: Engine.performSettle runs CR 704.5d's state-based
      -- action before placePendingTriggers.
      board owner stolen asToken = do
        devotion <- S.printingOf s registry "Warped Devotion"
        piker <- S.printingOf s registry "Goblin Piker"
        pikerCard <- S.cardOf s registry "Goblin Piker"
        island <- S.printingOf s registry "Island"
        let (_, g1) = S.addCreature devotion S.alice (S.landsInPlay island 1)
            (pikerId, g2) = (if asToken then S.addToken pikerCard else S.addCreature piker) owner g1
            g3 = if stolen then S.giveControl pikerId S.alice g2 else g2
            g4 = List.foldl' (\gs _ -> snd (S.addHandCard island S.alice gs)) g3 [1 .. 3 :: Int]
            -- Two for bob on the token leg, one on the card leg: CR 701.9b's
            -- choice is a real prompt either way, since the returned CARD joins
            -- bob's hand before he discards and the token does not.
            g5 = List.foldl' (\gs _ -> snd (S.addHandCard island S.bob gs)) g4 [1 .. if asToken then 2 else 1 :: Int]
        pure (pikerId, g5)
      returnByHand oid gs =
        let gone = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Hand)
            settled = S.runPure S.identityAnswer gone Engine.settleForPriority
         in S.runPure S.identityAnswer settled Stack.resolveTop
      graveyardSize pid gs = length (Game.zoneMembers Zone.Graveyard pid gs)
   in Spec.describe s "Warped Devotion" $ do
        Spec.it s "CR 400.3 bob's Piker under alice's control returned to hand makes BOB discard" $ do
          (pikerId, gs) <- board S.bob True False
          let after = returnByHand pikerId gs
          Spec.assertEqWith s "bob discarded" (graveyardSize S.bob after) 1
          Spec.assertEqWith s "and alice, its controller, did not" (graveyardSize S.alice after) 0
          Spec.assertEqWith s "bob kept one of his card and the Piker" (S.handSize S.bob after) 1
          Spec.assertEqWith s "alice's hand is untouched" (S.handSize S.alice after) 3
          Spec.assertEqWith s "the Piker really was alice's to control" (Projection.controllerOf pikerId gs) (Just S.alice)
        Spec.it s "CR 400.3 alice's own Piker, on the board that differs in its owner alone, makes alice discard" $ do
          (pikerId, gs) <- board S.alice True False
          let after = returnByHand pikerId gs
          Spec.assertEqWith s "alice discarded" (graveyardSize S.alice after) 1
          Spec.assertEqWith s "and bob did not" (graveyardSize S.bob after) 0
        -- CR 111.7's parenthetical -- "if a token changes zones, applicable
        -- triggered abilities will trigger before the token ceases to exist" --
        -- against the order Engine.performSettle actually runs in: CR 704.5d's
        -- state-based action deletes the token BEFORE placePendingTriggers, so
        -- the object the move minted in bob's hand is already gone when
        -- Event.eventBindings computes `thatPlayer`. Reading CR 400.3's owner
        -- off that arrival leaves the slot unbound and the discard silently
        -- empty; CR 608.2h's record of the departed id is what still answers.
        --
        -- One difference from the first case: the Piker is a token. Same owner,
        -- same theft, same bounce.
        Spec.it s "CR 111.7 bob's Piker TOKEN under alice's control still makes bob discard" $ do
          (pikerId, gs) <- board S.bob True True
          let after = returnByHand pikerId gs
          Spec.assertEqWith s "bob discarded" (graveyardSize S.bob after) 1
          Spec.assertEqWith s "and alice, its controller, did not" (graveyardSize S.alice after) 0
          Spec.assertEqWith s "bob is left with one of his two, the token having ceased rather than joined them" (S.handSize S.bob after) 1
          Spec.assertBool s (not (Set.member pikerId (GameState.battlefield after))) "the token left the battlefield"
        -- The whole card through a real spell: alice casts Unsummon at the Piker
        -- bob both owns and controls, targeted by id.
        Spec.it s "CR 603.2 whole card: Unsummon on bob's Piker makes bob discard" $ do
          (pikerId, gs) <- board S.bob False False
          unsummon <- S.printingOf s registry "Unsummon"
          let (withSpell, spellId) = S.handOne unsummon gs
              answer :: Prompt.Prompt r -> r
              answer p = case p of
                Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just pikerId) . Recipient.objectOf) . snd) sets
                _ -> S.identityAnswer p
              cast = S.runPure answer withSpell (S.cast S.alice spellId)
              resolved = S.runPure answer cast Stack.resolveTop
              settled = S.runPure answer resolved Engine.settleForPriority
              after = S.runPure answer settled Stack.resolveTop
          Spec.assertEqWith s "bob discarded" (graveyardSize S.bob after) 1
          Spec.assertEqWith s "alice's graveyard holds Unsummon alone" (graveyardSize S.alice after) 1
          Spec.assertEqWith s "the Piker left the battlefield" (Game.lookupObject pikerId settled) Nothing
        -- The bindings themselves, off the recorded event: the owner under
        -- thatPlayer, the departed id under thatDepartedPermanent, and no
        -- `became` -- CR 400.7e withholds it for a hand (CR 400.2).
        Spec.it s "CR 603.10a eventBindings binds the owner and the departed permanent, and withholds became" $ do
          (pikerId, gs) <- board S.bob True False
          let gone = S.runPure S.identityAnswer gs (Event.changeZone pikerId Zone.Hand)
              moves =
                Maybe.mapMaybe
                  ( \logged -> case LoggedEvent.event logged of
                      ev@(GameEvent.Moved (Moved.MkMoved zc _ _)) | ZoneChange.departed zc == pikerId -> Just ev
                      _ -> Nothing
                  )
                  (Foldable.toList (GameState.events gone))
          case moves of
            [ev] ->
              Spec.assertEqWith
                s
                "the owner and the departed permanent, nothing else"
                (Event.eventBindings gone Nothing S.bob (TriggerCondition.PermanentReturnedToHand (Filter.Type.And [])) ev)
                (Map.fromList [(Binding.triggerPlayer, Binding.toPlayer S.bob), (Binding.departedPermanent, Binding.toObject pikerId)])
            other -> Spec.assertFailure s ("expected one move of the Piker, got " <> show (length other))

-- CR 603.6c's penultimate sentence -- "An ability that attempts to do something
-- to the card that left the battlefield checks for it only in the first zone
-- that it went to" -- said positively by CR 400.7e: "Abilities that trigger when
-- an object moves from one zone to another ... can find the new object that it
-- became in the zone it moved to when the ability triggered, if that zone is a
-- public zone."
--
-- Endless Cockroaches, {1}{B}{B} Creature -- Insect 1/1, "When this creature
-- dies, return it to its owner's hand." Two different objects hide inside that
-- one printed word "it": the ability's SOURCE (CR 113.7a -- the permanent that
-- died, which CR 603.10a's look-back reads from CR 608.2h last known
-- information) and the CARD it became in the graveyard, which is what the
-- effect has to move. CR 400.7 minted a fresh id for the second, so the two are
-- not interchangeable and they are not one slot.
--
-- Also the home of the pin on Event.eventBindingSlots (the last case): that
-- classification restates in one dimension what eventBindings says in two, so
-- the two are compared here rather than trusted to agree.
--
-- The structural twin is Narcomoeba's `MoveToZone "self" Battlefield` in
-- graveyardTriggerSpec: same opcode, same slot shape. There "self" IS the
-- arriving card, because SelfPutIntoGraveyardFromLibrary matches on the
-- ARRIVING incarnation; here it is not, because CR 603.10a makes this condition
-- match on the DEPARTING one. That contrast is why there are two slots.
becameSlotSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
becameSlotSpec s registry =
  let -- alice: one Mountain (Lightning Bolt's {R}), the Cockroaches in play, and
      -- the Bolt in hand. S.identityAnswer targets the least Recipient, and
      -- Recipient.ToCreature sorts before Recipient.ToPlayer, so the one
      -- creature on the board is the target without a bespoke interpreter.
      roachBoard = do
        mountain <- S.printingOf s registry "Mountain"
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        cockroaches <- S.printingOf s registry "Endless Cockroaches"
        let (roachId, withRoaches) = S.addCreature cockroaches S.alice (S.landsInPlay mountain 1)
        pure (roachId, S.handOne lightningBolt withRoaches)
      -- Cast the Bolt, resolve it (3 damage marked on a 1/1), settle -- CR
      -- 704.5g destroys it and the same CR 117.5 settle's trigger scan sees the
      -- death -- then resolve the trigger.
      boltIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            damaged = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer damaged Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))
      roachName = CardName.MkCardName $ Text.pack "Endless Cockroaches"
   in Spec.describe s "CR 400.7e the card it became" $ do
        -- The gameplay-level proof, cast to resolution. The discriminating
        -- assertion is the HAND: an effect reading the trigger's source would
        -- name the dead battlefield id and move nothing, leaving the card in
        -- the graveyard where the state-based action put it.
        Spec.it s "CR 603.6c whole card: Lightning Bolt kills Endless Cockroaches and its dies trigger returns the card to hand" $ do
          (_, board) <- roachBoard
          let (settled, after) = boltIt board
          Spec.assertEqWith s "the trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          Spec.assertBool s (Set.member roachName (namesIn Zone.Graveyard S.alice settled)) "the card is in the graveyard when the trigger is placed"
          Spec.assertBool s (Set.member roachName (namesIn Zone.Hand S.alice after)) "and in hand once it resolves"
          Spec.assertBool s (not (Set.member roachName (namesIn Zone.Graveyard S.alice after))) "no longer in the graveyard"
          -- The Bolt itself is the graveyard's only remaining tenant, so the
          -- assertion above cannot be passing because the graveyard is read
          -- from the wrong player's zone.
          Spec.assertEqWith s "only the Bolt is left there" (namesIn Zone.Graveyard S.alice after) (Set.singleton (CardName.MkCardName $ Text.pack "Lightning Bolt"))
        -- The two slots, side by side on the placed trigger. CR 113.7a's
        -- source is the id that DIED and no longer resolves; CR 400.7e's
        -- "became" is the graveyard card, which does.
        Spec.it s "CR 113.7a the self slot keeps the departed id while became names the graveyard card" $ do
          (roachId, board) <- roachBoard
          let (settled, _) = boltIt board
              bindingsOn oid = maybe Map.empty Object.bindings (Game.lookupObject oid settled)
              slots = concatMap (Map.toList . Map.mapMaybe Binding.onlyOne . Binding.targetsOf . bindingsOn) (GameState.stack settled)
              slotFor name = lookup name slots
          Spec.assertEqWith s "self is the permanent that died" (slotFor Binding.triggerSource) (Just (Recipient.ToObject roachId))
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject roachId settled)) "and that id is gone (CR 400.7)"
          case slotFor Binding.became of
            Just (Recipient.ToObject graveyardId) -> do
              Spec.assertBool s (graveyardId /= roachId) "became is a different id"
              Spec.assertEqWith s "and it is the graveyard card" (fmap Face.name (Game.faceOf graveyardId settled)) (Just roachName)
              -- The spent Bolt is in that graveyard too, so membership is the
              -- assertion rather than the whole zone.
              Spec.assertBool s (elem graveyardId (Game.zoneMembers Zone.Graveyard S.alice settled)) "in alice's graveyard"
            other -> Spec.assertFailure s ("expected became to name an object, got " <> show other)
        -- eventBindings in isolation, so the binding is pinned to the rule
        -- rather than to one card's payload. CR 400.7e's "the new object that
        -- it became in the zone it moved to" is ZoneChange.object, never
        -- ZoneChange.departed, which is what matchesTrigger matched on.
        Spec.it s "CR 400.7e eventBindings binds the ARRIVING id, not the departed one" $ do
          let departed = ObjectId.MkObjectId 1
              arrived = ObjectId.MkObjectId 2
              died = GameEvent.Moved (Moved.moved (ZoneChange.MkZoneChange departed arrived Zone.Battlefield Zone.Graveyard) S.emptyCharacteristics)
          Spec.assertEqWith s "became names the graveyard incarnation" (Event.eventBindings (Setup.emptyGame S.bothPlayers) Nothing S.alice TriggerCondition.SelfDies died) (Map.singleton Binding.became (Binding.toObject arrived))
        -- A condition that is not a look-back gets no such slot: Narcomoeba's
        -- bearer IS the arriving card, so binding it again would be a second
        -- name for the same object.
        Spec.it s "CR 113.6k a library-to-graveyard trigger binds nothing" $ do
          let oid = ObjectId.MkObjectId 1
              milled = GameEvent.Moved (Moved.moved (ZoneChange.MkZoneChange oid oid Zone.Library Zone.Graveyard) S.emptyCharacteristics)
          Spec.assertEqWith s "no became slot" (Event.eventBindings (Setup.emptyGame S.bothPlayers) Nothing S.alice TriggerCondition.SelfPutIntoGraveyardFromLibrary milled) Map.empty
        -- CR 400.7f's arm, the one place the slot comes from something other than
        -- the event: the BEARER's own arrival, which eventTriggers computes off
        -- the batch. Pinned in isolation so the rule is stated once here and once
        -- on the board in screamsFromWithinSpec, and so that the absent case --
        -- CR 704.5n's Equipment bearer, or an Aura sent anywhere but its owner's
        -- graveyard -- is visible as the empty map rather than as a wrong id.
        Spec.it s "CR 400.7f eventBindings binds the BEARER's graveyard incarnation, not the event's" $ do
          let departed = ObjectId.MkObjectId 1
              arrived = ObjectId.MkObjectId 2
              bearerArrived = ObjectId.MkObjectId 3
              hostDied = GameEvent.Moved (Moved.moved (ZoneChange.MkZoneChange departed arrived Zone.Battlefield Zone.Graveyard) S.emptyCharacteristics)
          Spec.assertEqWith
            s
            "became names the Aura's incarnation, not the host's"
            (Event.eventBindings (Setup.emptyGame S.bothPlayers) (Just bearerArrived) S.alice TriggerCondition.AttachedCreatureDies hostDied)
            (Map.fromList [(Binding.became, Binding.toObject bearerArrived), (Binding.departedPermanent, Binding.toObject departed)])
          -- CR 303.4b's half stands alone where the bearer reached no graveyard:
          -- the host's departure is the EVENT's datum and CR 704.5n's Equipment
          -- shape does not withhold it, which is why eventBindingSlots claims the
          -- two slots on different arguments.
          Spec.assertEqWith
            s
            "and the host alone where the bearer reached no graveyard"
            (Event.eventBindings (Setup.emptyGame S.bothPlayers) Nothing S.alice TriggerCondition.AttachedCreatureDies hostDied)
            (Map.singleton Binding.departedPermanent (Binding.toObject departed))
        -- The pin on Event.eventBindingSlots, the per-CONDITION slot set the
        -- card lint asks (CardSpec's "every slot a triggered ability reads is
        -- bound for its condition"). That function is a second statement of
        -- what eventBindings already says, and eventBindings cases on
        -- (condition, event) PAIRS, so nothing in the types keeps the two
        -- agreeing: a new binding arm added there and forgotten here would
        -- silently un-lint the new slot. Comparing the keys eventBindings
        -- actually produces against what the classification claims is what
        -- makes the drift a failing test.
        --
        -- The INTERSECTION over the events a condition admits, because the
        -- classification answers the guaranteed floor rather than the union: a
        -- slot the lint says is available must be bound for every event that
        -- could have placed the trigger. A few conditions have a list longer
        -- than one -- SelfLeavesTheBattlefield, where the two destinations
        -- disagree about `became`; SelfIsDealtDamage, where CR 120.3's two
        -- damage kinds agree on `thatMuch` and so make the floor a real one;
        -- CreatureBecomesBlockedByAtLeast, where rule 509.3e's two producers
        -- agree on `attackingCreature` off two different event constructors; and
        -- SelfBecomesBlockedByOneOrMore, whose two producers agree on binding
        -- nothing at all. For every other the intersection is exactly that
        -- single event's keyset.
        --
        -- The BEARER ARRIVAL argument is held constant at a present one, and is
        -- not a second dimension of the intersection: it is CR 400.7f's datum
        -- rather than a shape the event can take, and eventBindingSlots'
        -- AttachedCreatureDies arm claims the slot on CR 704.5m's Aura, whose
        -- arrival is present for every event that condition admits. Holding it
        -- Nothing instead would pin the OTHER reading, under which no card could
        -- read `became` there at all -- and CR 704.5n's Equipment bearer, which
        -- has no arrival, is the over-claim that arm's own comment records.
        Spec.it s "CR 603.2 eventBindingSlots names exactly the keys eventBindings stamps for EVERY event a condition admits" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          let bearerBecame = Just (ObjectId.MkObjectId 3)
              -- The state for the one arm that reads one. PermanentReturnedToHand
              -- looks CR 400.3's owner up off CR 608.2h's record of the DEPARTED
              -- id, and GameState.objects is left EMPTY here on purpose: CR 111.7
              -- and CR 704.5d delete a token that has reached a hand, and
              -- Engine.performSettle runs that state-based action before
              -- placePendingTriggers, so on a real board the arriving object can
              -- already be gone when eventBindings runs. An arm that bound a slot
              -- off a live Game.lookupObject would be partial in play, and a
              -- pinState holding the arrival would let it pass here anyway.
              --
              -- The record is the ZONE-CHANGE FUNNEL's own, lifted off a real
              -- bounce and re-filed under the id representativeEvents uses, so the
              -- pin cannot be satisfied by a record shaped to suit it. Nothing
              -- lands under the departed id in `objects`, which is what
              -- Projection.lastKnownOf's liveness guard requires.
              (pikerId, placed) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
              bounced = S.runPure S.identityAnswer placed (Event.changeZone pikerId Zone.Hand)
              empty = Setup.emptyGame S.bothPlayers
              pinState = case Map.lookup pikerId (GameState.lastKnown bounced) of
                Just lk -> empty {GameState.lastKnown = Map.singleton representativeDeparted lk}
                Nothing -> empty
          Spec.assertBool s (not (Map.null (GameState.lastKnown pinState))) "the funnel filed a last-known record for the bounced Piker"
          mapM_
            ( \cond ->
                let stamped = fmap (Map.keysSet . Event.eventBindings pinState bearerBecame S.alice cond) (representativeEvents cond)
                 in Spec.assertEqWith s ("the slots bound for " <> show cond) (Event.eventBindingSlots cond) (foldr Set.intersection (NonEmpty.head stamped) (NonEmpty.tail stamped))
            )
            everyTriggerCondition

-- CR 400.7e's slot under a BYSTANDER's dies trigger, the last of the four
-- conditions that bind it (SelfDies in becameSlotSpec above,
-- SelfLeavesTheBattlefield in leavesBattlefieldSpec, PermanentEnters in
-- aetherFlashSpec below, and this): "Abilities that trigger when an object moves
-- from one zone to another ... can find the new object that it became in the
-- zone it moved to when the ability triggered, if that zone is a public zone."
--
-- Promise of Tomorrow, {2}{W} Enchantment, "Whenever a creature you control
-- dies, exile it." The bearer is a THIRD object -- neither the creature that
-- died nor its graveyard incarnation -- which is what makes "it" unambiguous
-- here where becameSlotSpec's Endless Cockroaches had to keep two incarnations
-- of one card apart. The card's OTHER ability, which reads CR 607.2a's link back
-- off what this one exiled, is promiseOfTomorrowReturnSpec below.
--
-- The discriminating assertion is WHICH id the payload moves. CR 603.10a makes
-- Event.matchesTrigger's PermanentDies arm match on ZoneChange.departed, so
-- "you control" is answerable from CR 608.2h last known information; but CR
-- 400.7 deleted that id when the creature died, so the effect has to be handed
-- ZoneChange.object -- the card now in the graveyard -- instead. Reading
-- `departed` here would leave the creature sitting in the graveyard, which is
-- exactly what assertions (a) and (b) below rule out.
--
-- CR 400.7e's public-zone proviso needs no guard: the PermanentDies arm has
-- already required battlefield-to-graveyard, and CR 400.2 lists the graveyard
-- among the public zones. SelfLeavesTheBattlefield is the condition where the
-- proviso does real work, and it is guarded there.
--
-- Goblin Piker is the victim rather than a token, deliberately: CR 111.7 makes
-- a token in a zone other than the battlefield cease to exist, so a token
-- exiled out of a graveyard would leave nothing to observe and (a) would pass
-- for the wrong reason.
--
-- Two seats, because "a creature YOU control" needs a creature somebody else
-- controls to be separated from. Bob's Ogre Sentry standing untouched is that
-- separation.
promiseOfTomorrowSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
promiseOfTomorrowSpec s registry =
  let promiseBoard = do
        promise <- S.printingOf s registry "Promise of Tomorrow"
        piker <- S.printingOf s registry "Goblin Piker"
        sentry <- S.printingOf s registry "Ogre Sentry"
        let empty = Setup.emptyGame S.bothPlayers
            (_, withPromise) = S.addCreature promise S.alice empty
            (pikerId, withPiker) = S.addCreature piker S.alice withPromise
            (sentryId, withSentry) = S.addCreature sentry S.bob withPiker
        pure (pikerId, sentryId, withSentry)
      -- Destroy alice's creature outright (CR 701.8a), settle so the CR 117.5
      -- boundary scans the death and places the trigger, then resolve it.
      killIt oid gs =
        let killed = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [oid])
            settled = S.runPure S.identityAnswer killed Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      namesIn zone pid gs =
        fmap Face.name (Maybe.mapMaybe (\oid -> Game.faceOf oid gs) (Game.zoneMembers zone pid gs))
      pikerName = CardName.MkCardName $ Text.pack "Goblin Piker"
   in Spec.describe s "CR 400.7e the card a BYSTANDER's dies trigger names" $ do
        Spec.it s "CR 700.4 whole card: alice's Goblin Piker dies and Promise of Tomorrow exiles the graveyard card" $ do
          (pikerId, sentryId, board) <- promiseBoard
          let (settled, after) = killIt pikerId board
          Spec.assertEqWith s "the trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject pikerId settled)) "the battlefield id is gone (CR 400.7)"
          Spec.assertEqWith s "and the card is in the graveyard when the trigger is placed" (namesIn Zone.Graveyard S.alice settled) [pikerName]
          -- (a), (b) and (c) as one tuple. (a) and (b) are the same fact from
          -- both sides: an effect handed ZoneChange.departed would move an id
          -- CR 400.7 deleted, so the exile would be empty and the graveyard
          -- would still hold the Piker. (c) is the "you control" separation.
          Spec.assertEqWith
            s
            "exiled, out of the graveyard, and bob's creature untouched"
            (namesIn Zone.Exile S.alice after, namesIn Zone.Graveyard S.alice after, Set.member sentryId (GameState.battlefield after))
            ([pikerName], [], True)
        -- eventBindings in isolation, so the binding is pinned to CR 400.7e
        -- rather than to Promise of Tomorrow's payload. The contrast with the
        -- SelfDies arm above is only in which object the BEARER is; the slot
        -- names ZoneChange.object either way.
        Spec.it s "CR 400.7e eventBindings binds the ARRIVING id for PermanentDies too" $ do
          let departed = ObjectId.MkObjectId 1
              arrived = ObjectId.MkObjectId 2
              died = GameEvent.Moved (Moved.moved (ZoneChange.MkZoneChange departed arrived Zone.Battlefield Zone.Graveyard) S.emptyCharacteristics)
          Spec.assertEqWith s "became names the graveyard incarnation and departedPermanent the battlefield one" (Event.eventBindings (Setup.emptyGame S.bothPlayers) Nothing S.alice (TriggerCondition.PermanentDies (Filter.Type.HasCardType CardType.Creature)) died) (Map.fromList [(Binding.became, Binding.toObject arrived), (Binding.departedPermanent, Binding.toObject departed)])

-- CR 603.10a's departed permanent under a BYSTANDER's DIES trigger, which
-- promiseOfTomorrowSpec above deliberately does not read: Promise of Tomorrow
-- says "exile it" and means CR 400.7e's graveyard card, where this card says
-- "for each counter on IT" and means the permanent as it last existed on the
-- battlefield -- an object CR 122.2 stripped of its counters on the way out, so
-- only CR 608.2h's record still answers.
--
-- Cleopatra, Exiled Pharaoh, {2}{B}{G} Legendary Creature -- Human Noble 2/4,
-- second ability: "Whenever a legendary creature with counters on it dies, draw
-- a card for each counter on it. You lose 2 life."
--
-- Three separations, so the draw count can only be the right one:
--
--   * The dead creature, NOT the trigger's source. Cleopatra herself carries
--     five counters and bob's Jedit Ojanen three, so a payload that read
--     CR 113.7a's source slot would draw five.
--
--   * The dead creature, NOT nothing. An unbound slot reads zero counters and
--     draws no cards, which is what the mutation of the eventBindings arm
--     produces.
--
--   * "a legendary creature", not "a creature". Carol's Goblin Piker dies under
--     three counters of its own and Cleopatra says nothing, which is the second
--     case below.
--
-- Bob's Jedit rather than alice's own: the printed condition has no "you
-- control", and three seats keep the controller of the dead creature, the
-- controller of the trigger and the bystander apart.
--
-- Alice's library is stocked past the largest wrong answer, so a draw of five
-- would be observable rather than a CR 104.3c loss.
cleopatraSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cleopatraSpec s registry =
  let cleopatraBoard = do
        cleopatra <- S.printingOf s registry "Cleopatra, Exiled Pharaoh"
        jedit <- S.printingOf s registry "Jedit Ojanen"
        piker <- S.printingOf s registry "Goblin Piker"
        let (cleoId, g1) = S.addCreature cleopatra S.alice S.threePlayerGame
            (jeditId, g2) = S.addCreature jedit S.bob g1
            (pikerId, g3) = S.addCreature piker S.carol g2
            stocked = List.foldl' (\gs _ -> snd (S.addLibraryCard piker S.alice gs)) g3 [1 :: Int .. 6]
            countered = S.addCounter CounterKind.PlusOnePlusOne 3 pikerId (S.addCounter CounterKind.PlusOnePlusOne 3 jeditId (S.addCounter CounterKind.PlusOnePlusOne 5 cleoId stocked))
        pure (jeditId, pikerId, countered)
      -- promiseOfTomorrowSpec's road, and for its reason: one route to the
      -- death, so CR 117.5 places the trigger before anything else runs.
      killIt oid gs =
        let killed = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [oid])
            settled = S.runPure S.identityAnswer killed Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
   in Spec.describe s "CR 603.10a the permanent a BYSTANDER's dies trigger says \"it\" about" $ do
        Spec.it s "CR 122.2 / 608.2h bob's Jedit Ojanen dies under three counters and alice draws three cards" $ do
          (jeditId, _, board) <- cleopatraBoard
          let (settled, after) = killIt jeditId board
          Spec.assertEqWith
            s
            "three cards drawn -- the dead creature's counters, not Cleopatra's five and not nothing -- and two life lost"
            (S.handSize S.alice after, S.lifeOf S.alice after)
            (3, Just 18)
          Spec.assertEqWith s "alice held nothing before the trigger resolved" (S.handSize S.alice settled) 0
          Spec.assertEqWith s "and the trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject jeditId after)) "the battlefield id is gone (CR 400.7)"
        -- The control the case above cannot be read without, and the ONE thing
        -- that differs is the dying creature's supertype: carol's Piker carries
        -- the same three counters and dies on the same road.
        Spec.it s "CR 205.4a a NONLEGENDARY creature dying with counters says nothing" $ do
          (_, pikerId, board) <- cleopatraBoard
          let (settled, after) = killIt pikerId board
          Spec.assertEqWith s "no cards drawn and no life lost" (S.handSize S.alice after, S.lifeOf S.alice after) (0, Just 20)
          Spec.assertEqWith s "because nothing triggered at all" (length (GameState.stack settled)) 0

-- CR 607.2a's linked pair read from the SECOND ability, which is the half
-- promiseOfTomorrowSpec above could not reach: "At the beginning of each end
-- step, if you control no creatures, sacrifice this enchantment and return all
-- cards exiled with it to the battlefield under your control."
--
-- Three separations, each a different way the return could be wrong:
--
--   * IDENTITY, not count. Wall of Stone sits in exile from the start, put there
--     by no ability at all, so GameState.exiledWith files it against nothing.
--     A return that swept exile rather than the linked set would bring it back.
--
--   * CONTROL, not ownership. The Ogre Sentry is BOB's card under alice's
--     control when it dies, so CR 110.2a's "unless the effect states otherwise"
--     is observable: "under your control" hands it to alice, while
--     EntryRiders.underOwner would hand it to bob. The Goblin Piker, which alice
--     owns, cannot tell those apart on its own.
--
--   * "YOU control no creatures", not "no creatures". Bob's Hill Giant is on the
--     battlefield in BOTH legs, so a CR 603.4 clause reading the whole
--     battlefield would never fire at all.
--
-- The negative leg is the same board with the Hill Giant under ALICE instead of
-- bob -- one difference, and it is the clause's subject. Alice then still
-- controls a creature when the end step begins, the ability does not trigger,
-- and nothing moves: Promise stays on the battlefield and both cards stay in
-- exile.
--
-- The two deaths are dealt one at a time so that exactly one trigger is on the
-- stack per settle; two at once would make CR 603.3b's ordering a question the
-- fixture answerer would have to answer, and the order is not what is under
-- test.
--
-- WHAT IS NOT COVERED: an exiled card that leaves exile by some other means
-- before the return. Resolve.recordExiledWith drops such a key on its next
-- window (its own comment carries the argument, and CR 400.7 mints the departed
-- card a new id besides), but no card in the pool can pull a card out of exile
-- between Promise's two abilities, so there is nothing to drive it with. The
-- source leaving the battlefield first IS covered, and by construction: the
-- sacrifice runs BEFORE the return in the same resolution, so every assertion
-- below is already read off a board where the linking object is in the
-- graveyard.
promiseOfTomorrowReturnSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
promiseOfTomorrowReturnSpec s registry =
  let board giantUnder = do
        promise <- S.printingOf s registry "Promise of Tomorrow"
        piker <- S.printingOf s registry "Goblin Piker"
        sentry <- S.printingOf s registry "Ogre Sentry"
        giant <- S.printingOf s registry "Hill Giant"
        wall <- S.printingOf s registry "Wall of Stone"
        let empty = Setup.emptyGame S.bothPlayers
            (promiseId, withPromise) = S.addCreature promise S.alice empty
            (pikerId, withPiker) = S.addCreature piker S.alice withPromise
            -- Bob's card, alice's permanent (CR 108.4).
            (sentryId, withSentry) = S.addCreature sentry S.bob withPiker
            stolen = S.giveControl sentryId S.alice withSentry
            (giantId, withGiant) = S.addCreature giant S.bob stolen
            seated = if giantUnder == S.alice then S.giveControl giantId S.alice withGiant else withGiant
            (wallId, withWall) = S.addExiledCard wall S.alice seated
        pure (promiseId, pikerId, sentryId, wallId, withWall)
      -- Destroy one permanent (CR 701.8a), settle so the CR 117.5 boundary scans
      -- the death and places Promise's dies trigger, then resolve that trigger.
      killIt oid gs =
        let killed = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [oid])
            settled = S.runPure S.identityAnswer killed Engine.settleForPriority
         in S.runPure S.identityAnswer settled Stack.resolveTop
      -- Record the end step's beginning, place what it gathers (CR 603.3), and
      -- resolve the one ability that can be there. BOTH states come back: the
      -- stack is empty after the resolution either way, so "the ability did not
      -- trigger" is only readable at the placement.
      endStepOf gs =
        let began = S.withEvents [GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) S.alice)] gs
            placed = S.runPure S.identityAnswer began Engine.settleForPriority
         in (placed, S.runPure S.identityAnswer placed Stack.resolveTop)
      nameOf oid gs = fmap Face.name (Game.faceOf oid gs)
      -- The battlefield permanents whose PROJECTED controller is pid, by name.
      -- Sorted, since GameState.battlefield is a set and the returned cards are
      -- minted fresh ids in no order the card states.
      controlledNames pid gs =
        List.sort (Maybe.mapMaybe (\oid -> if Projection.controllerOf oid gs == Just pid then nameOf oid gs else Nothing) (Set.toList (GameState.battlefield gs)))
      -- Everything in exile by name, whoever owns it -- CR 400.1 makes exile one
      -- shared zone, and the Sentry there is bob's card.
      exiledNames gs = List.sort (Maybe.mapMaybe (`nameOf` gs) (Set.toList (GameState.exile gs)))
      -- Both graveyards, since CR 400.1 makes the graveyard a per-player zone and
      -- the Sentry is bob's card.
      namesInGraveyards gs = List.sort (concatMap (\pid -> Maybe.mapMaybe (`nameOf` gs) (Game.zoneMembers Zone.Graveyard pid gs)) (NonEmpty.toList S.bothPlayers))
      named = CardName.MkCardName . Text.pack
   in Spec.describe s "CR 607.2a Promise of Tomorrow returns the cards it exiled" $ do
        Spec.it s "CR 700.4 whole card: the linked cards come back under alice, the unlinked one stays in exile" $ do
          (promiseId, pikerId, sentryId, wallId, board0) <- board S.bob
          let exiled = killIt sentryId (killIt pikerId board0)
          -- The premise, stated so a failure below cannot be a failure of the
          -- first ability wearing the second's clothes.
          Spec.assertEqWith s "both deaths were exiled, alongside the decoy" (exiledNames exiled) (fmap named ["Goblin Piker", "Ogre Sentry", "Wall of Stone"])
          Spec.assertEqWith s "and alice controls no creatures" (controlledNames S.alice exiled) [named "Promise of Tomorrow"]
          Spec.assertEqWith s "while bob still has one on the battlefield" (controlledNames S.bob exiled) [named "Hill Giant"]
          let (placed, after) = endStepOf exiled
          Spec.assertEqWith s "CR 603.4: the ability triggered" (length (GameState.stack placed)) 1
          Spec.assertBool s (not (S.onBattlefield promiseId after)) "CR 701.21a: the enchantment sacrificed itself"
          Spec.assertEqWith s "and it is in alice's graveyard" (fmap Face.name (Maybe.mapMaybe (`Game.faceOf` after) (Game.zoneMembers Zone.Graveyard S.alice after))) [named "Promise of Tomorrow"]
          -- The three separations, as one tuple so a failure says which.
          Spec.assertEqWith
            s
            "the linked pair returned under alice, the decoy stayed, bob kept only his Giant"
            (controlledNames S.alice after, exiledNames after, controlledNames S.bob after)
            (fmap named ["Goblin Piker", "Ogre Sentry"], [named "Wall of Stone"], [named "Hill Giant"])
          -- CR 400.7: what came back is a NEW object, which is why the assertion
          -- above reads names rather than ids. `pikerId` and `sentryId` would
          -- have been the wrong ids twice over -- the battlefield incarnations
          -- died two moves ago -- so the ids compared here are the ones exile
          -- actually held.
          Spec.assertBool s (Set.disjoint (GameState.exile exiled) (GameState.battlefield after)) "no exiled id is on the battlefield"
          Spec.assertBool s (Maybe.isJust (Game.lookupObject wallId after)) "and the decoy, which never moved, keeps its id"
        -- The one-difference control: bob's Hill Giant moves to alice, so alice
        -- controls a creature at the beginning of the end step and CR 603.4 stops
        -- the ability triggering at all.
        Spec.it s "CR 603.4 control: alice keeping ANY creature leaves the exiled cards where they are" $ do
          (promiseId, pikerId, sentryId, _, board0) <- board S.alice
          let exiled = killIt sentryId (killIt pikerId board0)
          Spec.assertEqWith s "exile holds the same three cards" (exiledNames exiled) (fmap named ["Goblin Piker", "Ogre Sentry", "Wall of Stone"])
          Spec.assertEqWith s "but alice still controls the Giant" (controlledNames S.alice exiled) (fmap named ["Hill Giant", "Promise of Tomorrow"])
          let (placed, after) = endStepOf exiled
          Spec.assertEqWith s "CR 603.4: nothing was placed on the stack" (length (GameState.stack placed)) 0
          Spec.assertBool s (S.onBattlefield promiseId after) "the enchantment is still there"
          Spec.assertEqWith s "and exile is unchanged" (exiledNames after) (fmap named ["Goblin Piker", "Ogre Sentry", "Wall of Stone"])
        -- CR 607.2b: the SAME resolution, with Rest in Peace on the battlefield
        -- so that the sacrifice in Promise's own effect list is replaced by an
        -- exile. That exile is a "direct result of a replacement event caused
        -- by" Rest in Peace, so the Promise CARD links to Rest in Peace and not
        -- to the Promise permanent whose ability is resolving; CR 607.2a's link
        -- is scoped to cards exiled "as a result of an instruction to exile them
        -- in the first ability", which this is not. Filed against the resolving
        -- source, the enchantment would return itself to the battlefield out of
        -- its own exile.
        --
        -- Rest in Peace is placed AFTER both deaths, not on the starting board:
        -- its replacement would otherwise exile the Piker and the Sentry on the
        -- way to the graveyard, so neither would die, Promise's first ability
        -- would never trigger, and the linked set would be empty -- which is
        -- also the control this leg needs, since an over-narrowed fix that filed
        -- nothing at all would leave the battlefield just as empty of returns.
        -- Placed rather than cast, so its own "exile all graveyards" trigger
        -- does not fire and file a second set of arrivals.
        Spec.it s "CR 607.2b the card Rest in Peace's replacement exiles is linked to IT, not to the resolving enchantment" $ do
          (promiseId, pikerId, sentryId, wallId, board0) <- board S.bob
          rip <- S.printingOf s registry "Rest in Peace"
          let exiled = killIt sentryId (killIt pikerId board0)
              (ripId, guarded) = S.addCreature rip S.alice exiled
              (placed, after) = endStepOf guarded
          Spec.assertEqWith s "CR 603.4: the ability still triggered" (length (GameState.stack placed)) 1
          Spec.assertBool s (not (S.onBattlefield promiseId after)) "CR 701.21a: the enchantment sacrificed itself"
          -- The discriminating pair. Under the ambient diff the Promise card is
          -- filed against its own permanent, so it comes back on the battlefield
          -- and leaves exile; under CR 607.2b it is Rest in Peace's, so it stays
          -- in exile beside the decoy and only the two cards Promise itself
          -- exiled return.
          Spec.assertEqWith
            s
            "only the cards Promise itself exiled returned, and the replaced sacrifice stayed in exile"
            (controlledNames S.alice after, exiledNames after)
            (fmap named ["Goblin Piker", "Ogre Sentry", "Rest in Peace"], fmap named ["Promise of Tomorrow", "Wall of Stone"])
          -- The falsifier: with Rest in Peace out, nothing can be in a graveyard
          -- at all, so a green result above cannot be a sacrifice that quietly
          -- went to the graveyard instead of being replaced.
          Spec.assertEqWith s "CR 614.6: the sacrifice was replaced, so no graveyard holds anything" (namesInGraveyards after) []
          Spec.assertBool s (Maybe.isJust (Game.lookupObject wallId after)) "and the decoy, which never moved, keeps its id"
          Spec.assertBool s (S.onBattlefield ripId after) "Rest in Peace itself never moved"

-- CR 603.4's intervening "if" on a LOOK-BACK trigger, which is the one shape
-- where the clause has to be read against an object that no longer exists:
-- "When the trigger event occurs, the ability checks whether the stated
-- condition is true. The ability triggers only if it is." CR 608.2a repeats the
-- check as the ability resolves.
--
-- Deathknell Berserker, {1}{B} Creature -- Elf Berserker 2/2: "When this
-- creature dies, if its power was 3 or greater, create a 2/2 black Zombie
-- Berserker creature token." Both readings of "its power" that are available
-- without CR 608.2h are wrong -- the id is gone, so a live projection describes
-- nothing at all, and the card sitting in the graveyard has its printed 2.
-- Only last known information answers 3.
--
-- Bad Moon supplies the third power, and it does so through the LAYERS
-- (CR 613.4c, layer 7c), which is what makes the graveyard card's printed value
-- visibly the wrong answer rather than merely a different route to the same one.
lookBackInterveningSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lookBackInterveningSpec s registry =
  let berserkerBoard withBadMoon = do
        mountain <- S.printingOf s registry "Mountain"
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        berserker <- S.printingOf s registry "Deathknell Berserker"
        badMoon <- S.printingOf s registry "Bad Moon"
        let lands = S.landsInPlay mountain 1
            moonAdded = if withBadMoon then snd (S.addCreature badMoon S.alice lands) else lands
            (berserkerId, withBerserker) = S.addCreature berserker S.alice moonAdded
        pure (berserkerId, S.handOne lightningBolt withBerserker)
      -- The Bolt targets the least Recipient, and S.addCreature hands out
      -- ascending ids, so Bad Moon (added first) would sort before the
      -- Berserker if it were a legal target -- it is an enchantment, and
      -- Lightning Bolt's pool is AnyTarget, so the Berserker is the only
      -- creature and the Bolt finds it.
      boltIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            damaged = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer damaged Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      tokensOf pid gs =
        filter
          -- CR 111.4: the name is BOTH subtypes plus "Token", which is exactly
          -- the rule's own Dwarven Reinforcements example.
          (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack "Zombie Berserker Token"))
          (Game.zoneMembers Zone.Battlefield pid gs)
   in Spec.describe s "CR 603.4 an intervening if over last known information" $ do
        Spec.it s "CR 603.4 with Bad Moon the Berserker died at power 3 and its trigger fires" $ do
          (berserkerId, board) <- berserkerBoard True
          let (settled, after) = boltIt board
          Spec.assertEqWith s "it was a 3/3 while it lived" (Projection.powerOf berserkerId (fst board)) (Just 3)
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack settled)) 1
          case tokensOf S.alice after of
            [token] -> do
              -- Printed 2/2, and 3/3 on this board: the token is black, so
              -- the same Bad Moon that made its maker a 3/3 pumps it in turn
              -- (CR 613.4c, layer 7c). Asserting the projection rather than
              -- the printed pair is what keeps the two facts from being
              -- confused for one another.
              Spec.assertEqWith s "printed 2/2" (maybe (Nothing, Nothing) (\f -> (Face.power f, Face.toughness f)) (Game.faceOf token after)) (Just (Power.MkPower (Quantity.Type.Literal 2)), Just (Toughness.MkToughness (Quantity.Type.Literal 2)))
              Spec.assertEqWith s "3/3 under Bad Moon" (Projection.powerOf token after, Projection.toughnessOf token after) (Just 3, Just 3)
              Spec.assertEqWith s "black" (Projection.colorsOf token after) (Set.singleton Color.Black)
              Spec.assertEqWith s "Zombie Berserker" (Projection.subtypesOf token after) (Set.fromList [Subtype.Zombie, Subtype.Berserker])
            other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
        -- CR 603.4's "otherwise it does nothing" -- and "does nothing" means
        -- the ability never reaches the stack at all, not that it resolves to
        -- no effect.
        Spec.it s "CR 603.4 without Bad Moon it died at power 2 and does not trigger at all" $ do
          (berserkerId, board) <- berserkerBoard False
          let (settled, after) = boltIt board
          Spec.assertEqWith s "a 2/2 while it lived" (Projection.powerOf berserkerId (fst board)) (Just 2)
          Spec.assertEqWith s "nothing reached the stack" (GameState.stack settled) []
          Spec.assertEqWith s "and no token was made" (tokensOf S.alice after) []

-- The same rule read against what CR 613 does NOT leave behind. CR 122.1a folds
-- a +1/+1 counter into the object's power and toughness at layer 7c, so a
-- projection taken as the creature died records the RESULT of the counter and
-- not the counter -- and "if it had a +1/+1 counter on it" is a question about
-- the counter. LastKnown.counters is where the counter itself is filed, and
-- Quantity.ObjectCounters is what reads it back.
--
-- Promising Duskmage, {2}{B} Creature -- Human Warlock 2/3: "When this creature
-- dies, if it had a +1/+1 counter on it, draw a card."
--
-- Murder rather than a damage spell does the killing on purpose: the counter
-- moves the Duskmage from 2/3 to 3/4, so any lethal-damage removal would need a
-- different amount per leg and the two legs would stop being one fixture. CR
-- 701.8a's destroy does not care what the toughness is.
--
-- Both of CR 603.4's reads are covered, and separately:
--
--   * the GATHER read (Event.interveningHolds) by the two legs -- the trigger
--     reaches the stack with the counter and does not reach it without.
--   * the RESOLUTION read (CR 608.2a, Pawl.Engine.Stack's OfTrigger arm) by the
--     third case, which lets the trigger onto the stack and then empties the
--     record it reads, so a wired re-check removes the ability and an unwired
--     one draws anyway.
counterLookBackSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
counterLookBackSpec s registry =
  let duskmageBoard withCounter = do
        swamp <- S.printingOf s registry "Swamp"
        murder <- S.printingOf s registry "Murder"
        duskmage <- S.printingOf s registry "Promising Duskmage"
        let lands = S.landsInPlay swamp 3
            (duskmageId, withDuskmage) = S.addCreature duskmage S.alice lands
            countered =
              if withCounter
                then S.addCounter CounterKind.PlusOnePlusOne 1 duskmageId withDuskmage
                else withDuskmage
            -- CR 104.3c: five cards is more library than any leg can draw
            -- through, so nothing here loses the game before the assertion runs.
            -- No leg advances a turn either, so no draw step spends one.
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard swamp S.alice g)) countered [1 .. 5 :: Int]
        pure (duskmageId, S.handOne murder stocked)
      -- Murder's pool is Pool.Creatures and the Duskmage is the only creature on
      -- the board, so identityAnswer's least Recipient is it.
      murderIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            destroyed = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer destroyed Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      librarySize pid gs = length (Game.zoneMembers Zone.Library pid gs)
      -- The COUNTER, not the power it produces. CR 122.1a makes 3/4 the visible
      -- consequence of the +1/+1 counter and this trigger asks about neither the
      -- 3 nor the 4, so the two facts are asserted apart.
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
   in Spec.describe s "CR 603.4 an intervening if over last known COUNTERS" $ do
        Spec.it s "CR 122.1 with a +1/+1 counter the Duskmage's death trigger draws a card" $ do
          (duskmageId, board) <- duskmageBoard True
          let (settled, after) = murderIt board
          Spec.assertEqWith s "it had one +1/+1 counter while it lived" (Map.lookup CounterKind.PlusOnePlusOne (countersOn duskmageId (fst board))) (Just 1)
          Spec.assertEqWith s "and CR 122.1a made it a 3/4, which is a DIFFERENT fact" (Projection.powerOf duskmageId (fst board), Projection.toughnessOf duskmageId (fst board)) (Just 3, Just 4)
          Spec.assertEqWith s "one card in hand and five in library to start" (S.handSize S.alice (fst board), librarySize S.alice (fst board)) (1, 5)
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "the Duskmage is in the graveyard" (Set.member duskmageId (GameState.battlefield after)) False
          -- The DELTA: the Murder left the hand and one card arrived from the
          -- library. Zero in hand and five in library is the other leg's answer,
          -- so the two cannot be satisfied by one implementation.
          Spec.assertEqWith s "one card drawn" (S.handSize S.alice after, librarySize S.alice after) (1, 4)
        -- CR 603.4's "otherwise it does nothing": without the counter the
        -- ability never triggers at all.
        Spec.it s "CR 603.4 with no counter the same death draws nothing" $ do
          (duskmageId, board) <- duskmageBoard False
          let (settled, after) = murderIt board
          Spec.assertEqWith s "no counters on it at all" (countersOn duskmageId (fst board)) Map.empty
          Spec.assertEqWith s "a plain 2/3" (Projection.powerOf duskmageId (fst board), Projection.toughnessOf duskmageId (fst board)) (Just 2, Just 3)
          Spec.assertEqWith s "one card in hand and five in library to start" (S.handSize S.alice (fst board), librarySize S.alice (fst board)) (1, 5)
          Spec.assertEqWith s "nothing reached the stack" (GameState.stack settled) []
          Spec.assertEqWith s "the Duskmage is in the graveyard just the same" (Set.member duskmageId (GameState.battlefield after)) False
          Spec.assertEqWith s "no card drawn" (S.handSize S.alice after, librarySize S.alice after) (0, 5)

        -- CR 608.2a on its own. The record the resolution re-check reads is
        -- emptied while the trigger sits on the stack -- something no rule can
        -- do to last known information, which is exactly why it isolates the
        -- second read: only a wired re-check can notice.
        Spec.it s "CR 608.2a the intervening if is checked AGAIN as the ability resolves" $ do
          (duskmageId, board) <- duskmageBoard True
          let cast = S.runPure S.identityAnswer (fst board) (S.cast S.alice (snd board))
              destroyed = S.runPure S.identityAnswer cast Stack.resolveTop
              settled = S.runPure S.identityAnswer destroyed Engine.settleForPriority
              forgotten =
                settled
                  { GameState.lastKnown =
                      Map.adjust
                        (\lk -> lk {LastKnown.counters = Map.empty})
                        duskmageId
                        (GameState.lastKnown settled)
                  }
              after = S.runPure S.identityAnswer forgotten Stack.resolveTop
          Spec.assertEqWith s "the trigger reached the stack on the gather read" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "and was removed on the resolution read, drawing nothing" (S.handSize S.alice after, librarySize S.alice after) (0, 5)
          Spec.assertEqWith s "the stack is empty either way" (GameState.stack after) []

-- CR 702.93a undying and CR 702.79a persist: one sentence in two counter kinds,
-- minted by Pawl.Engine.Keyword.returns.
--
-- Young Wolf, {G} Creature -- Wolf 1/1, and Putrid Goblin, {1}{B} Creature --
-- Zombie Goblin 2/2. Each keyword is the card's whole text box, so nothing else
-- printed there can be producing the return.
--
-- KILLED TWICE, rather than the counter being placed by hand as
-- counterLookBackSpec's Duskmage does: the second death is what proves the
-- counter actually arrived, since CR 603.4's "if it had no counters" reads it off
-- CR 608.2h last known information. Murder rather than damage, for that spec's
-- reason -- the two deaths are at different toughnesses.
--
-- The two keywords are one group because rules 702.79a and 702.93a are one
-- sentence: what separates the legs is the counter kind, and persist's is the one
-- that makes the returned permanent SMALLER (CR 122.1a).
undyingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
undyingSpec s registry =
  let -- The named creature on alice's board with two Murders in hand and six
      -- Swamps, which is {1}{B}{B} twice with nothing left over. No untap step
      -- runs between the two casts.
      board name = do
        swamp <- S.printingOf s registry "Swamp"
        murder <- S.printingOf s registry "Murder"
        creature <- S.printingOf s registry name
        let (oid, withCreature) = S.addCreature creature S.alice (S.landsInPlay swamp 6)
            (gs1, firstMurder) = S.handOne murder withCreature
            (secondMurder, gs2) = S.addHandCard murder S.alice gs1
        pure (oid, gs2, firstMurder, secondMurder)
      -- Cast the Murder, resolve it, settle -- CR 704.5g buries the creature and
      -- the same CR 117.5 scan sees the death -- then resolve what it placed.
      murderWith spellId gs =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            destroyed = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer destroyed Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      named name gs = filter (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack name))) (Set.toList (GameState.battlefield gs))
      inGraveyard name pid gs = elem (CardName.MkCardName (Text.pack name)) (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers Zone.Graveyard pid gs))
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
      -- One leg per keyword: die, come back with the counter at the stated size,
      -- die again, stay dead.
      twiceKilled name kind power toughness =
        Spec.it s ("CR 702 " <> name <> " returns once, with its counter, and not a second time") $ do
          (firstId, gs, m1, m2) <- board name
          let (settled, after) = murderWith m1 gs
          Spec.assertEqWith s "the dies trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject firstId after)) "CR 400.7: the permanent that died is a spent id"
          case named name after of
            [backId] -> do
              Spec.assertEqWith s "it entered with exactly one counter, of that kind" (countersOn backId after) (Map.singleton kind 1)
              Spec.assertEqWith s "CR 122.1a resizes it" (Projection.powerOf backId after, Projection.toughnessOf backId after) (Just power, Just toughness)
              Spec.assertEqWith s "CR 110.2a: under its owner's control" (Projection.controllerOf backId after) (Just S.alice)
              Spec.assertBool s (not (inGraveyard name S.alice after)) "and no longer in the graveyard"
              let (settled2, after2) = murderWith m2 after
              Spec.assertEqWith s "CR 603.4: with the counter on it, the second death triggers nothing" (GameState.stack settled2) []
              Spec.assertEqWith s "so nothing comes back" (named name after2) []
              Spec.assertBool s (inGraveyard name S.alice after2) "and it stays in the graveyard"
            other -> Spec.assertFailure s ("expected exactly one " <> name <> " back on the battlefield, got " <> show other)
   in Spec.describe s "CR 702.93 undying and CR 702.79 persist" $ do
        twiceKilled "Young Wolf" CounterKind.PlusOnePlusOne 2 2
        twiceKilled "Putrid Goblin" CounterKind.MinusOneMinusOne 1 1
        -- CR 122.1 and CR 614.1's passive subject in one board. Vizier of
        -- Remedies is "if one or more -1\/-1 counters would be put on a creature
        -- you control, that many minus one" -- Pawl.Types.Scaling.Subtract 1 --
        -- so persist's ONE counter scales to ZERO, and a zero-count placement is
        -- REMOVED rather than resized (Pawl.Engine.Event.settleCounters' guard).
        -- The Goblin comes back bare, so CR 603.4's intervening "if" is true
        -- again and rule 702.79a returns it a second time.
        --
        -- The ENTRY path, not a battlefield placement: CR 122.6 gives these
        -- counters as the permanent enters, so the scaling has to reach the entry
        -- row that Pawl.Engine.Event.flushEnteringCounters settles.
        Spec.it s "CR 614.1 Vizier of Remedies takes persist's counter to zero, so the Goblin returns bare and persists again" $ do
          swamp <- S.printingOf s registry "Swamp"
          murder <- S.printingOf s registry "Murder"
          goblin <- S.printingOf s registry "Putrid Goblin"
          vizier <- S.printingOf s registry "Vizier of Remedies"
          let (_, withVizier) = S.addCreature vizier S.alice (S.landsInPlay swamp 6)
              (goblinId, withGoblin) = S.addCreature goblin S.alice withVizier
              (gs1, firstMurder) = S.handOne murder withGoblin
              (secondMurder, gs2) = S.addHandCard murder S.alice gs1
              -- Aimed by id and FILTERED, castAt's posture above and for its
              -- reason: alice controls two creatures here, so an unaimed Murder
              -- would kill whichever id sorts first.
              killing :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
              killing oid spellId gs =
                let answer :: Prompt.Prompt r -> r
                    answer p = case p of
                      Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just oid) . Recipient.objectOf) . snd) sets
                      _ -> S.identityAnswer p
                    cast = S.runPure answer gs (S.cast S.alice spellId)
                    destroyed = S.runPure answer cast Stack.resolveTop
                    settled = S.runPure answer destroyed Engine.settleForPriority
                 in S.runPure answer settled Stack.resolveTop
              first = killing goblinId firstMurder gs2
          Spec.assertEqWith s "the Vizier is still there, so the Murder went where it was aimed" (length (named "Vizier of Remedies" first)) 1
          case named "Putrid Goblin" first of
            [backId] -> do
              Spec.assertEqWith s "CR 122.1: one counter minus one is none, so it returns with no counter at all" (countersOn backId first) Map.empty
              Spec.assertEqWith s "and at its printed size" (Projection.powerOf backId first, Projection.toughnessOf backId first) (Just 2, Just 2)
              let second = killing backId secondMurder first
              case named "Putrid Goblin" second of
                [againId] -> do
                  Spec.assertEqWith s "CR 603.4: it had no counter, so persist returns it a second time" (countersOn againId second) Map.empty
                  Spec.assertBool s (not (inGraveyard "Putrid Goblin" S.alice second)) "and it is not in the graveyard"
                other -> Spec.assertFailure s ("expected the Goblin back a second time, got " <> show other)
            other -> Spec.assertFailure s ("expected exactly one Putrid Goblin back, got " <> show other)
        -- CR 110.2a's "unless the effect states otherwise". Alice steals bob's
        -- Wolf and kills it, so CR 603.3a hands ALICE the dies trigger -- which
        -- is what makes this discriminating, since a return under the ability's
        -- controller and a return under the owner are the same board at one
        -- seat. Lightning Bolt does the killing so the whole fixture is red.
        Spec.it s "CR 110.2a a stolen Young Wolf comes back under its OWNER's control" $ do
          mountain <- S.printingOf s registry "Mountain"
          treason <- S.printingOf s registry "Act of Treason"
          bolt <- S.printingOf s registry "Lightning Bolt"
          wolf <- S.printingOf s registry "Young Wolf"
          let (wolfId, withWolf) = S.addCreature wolf S.bob (S.landsInPlay mountain 4)
              (gs1, treasonId) = S.handOne treason withWolf
              (boltId, gs2) = S.addHandCard bolt S.alice gs1
              stolen = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs2 (S.cast S.alice treasonId)) Stack.resolveTop
              burned = S.runPure S.identityAnswer (S.runPure S.identityAnswer stolen (S.cast S.alice boltId)) Stack.resolveTop
              settled = S.runPure S.identityAnswer burned Engine.settleForPriority
              after = S.runPure S.identityAnswer settled Stack.resolveTop
          Spec.assertEqWith s "alice controls the Wolf when it dies" (Projection.controllerOf wolfId stolen) (Just S.alice)
          Spec.assertEqWith s "CR 603.3a: so the dies trigger is alice's" (fmap (\oid -> Projection.controllerOf oid settled) (GameState.stack settled)) [Just S.alice]
          case named "Young Wolf" after of
            [backId] -> do
              Spec.assertEqWith s "and it returns under bob's" (Projection.controllerOf backId after) (Just S.bob)
              Spec.assertEqWith s "with its +1/+1 counter" (countersOn backId after) (Map.singleton CounterKind.PlusOnePlusOne 1)
            other -> Spec.assertFailure s ("expected the Wolf back on the battlefield, got " <> show other)

-- CR 702.135a afterlife N: "When this permanent is put into a graveyard from the
-- battlefield, create N 1/1 white and black Spirit creature tokens with flying."
-- Minted by Pawl.Engine.Keyword.afterlife on the same TriggerCondition.SelfDies
-- undying and persist take, and the FIRST minted keyword ability that creates a
-- token -- so what is under test is a whole card, minted in the engine rather
-- than read from card data.
--
-- Ministrant of Obligation, {2}{W} Creature -- Human Cleric 2/1, whose entire
-- text box is "Afterlife 2". Nothing else printed on it can be making tokens,
-- and N is 2 rather than 1 so a mint that ignored the keyword's payload and
-- created one token would fail.
--
-- Every characteristic rule 702.135a states is asserted, because the mint writes
-- each of them out by hand and a wrong one compiles: 1/1, both colours, the
-- Spirit creature type and flying. Both colours matter most -- the pool's other
-- Spirit token, Doomed Traveler's, is white alone, so a mint copied from it
-- would pass everything else.
afterlifeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
afterlifeSpec s registry =
  let spiritsAfterKilling name = do
        swamp <- S.printingOf s registry "Swamp"
        murder <- S.printingOf s registry "Murder"
        creature <- S.printingOf s registry name
        let (oid, withCreature) = S.addCreature creature S.alice (S.landsInPlay swamp 3)
            (gs, spellId) = S.handOne murder withCreature
            cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            destroyed = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer destroyed Engine.settleForPriority
        pure (oid, settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      spirits gs =
        filter
          (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Spirit Token")))
          (Set.toList (GameState.battlefield gs))
   in Spec.describe s "CR 702.135 afterlife" $ do
        Spec.it s "CR 702.135a a dying Ministrant of Obligation leaves two 1/1 white and black flying Spirits" $ do
          (ministrantId, settled, after) <- spiritsAfterKilling "Ministrant of Obligation"
          Spec.assertEqWith s "the dies trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject ministrantId after)) "CR 400.7: the permanent that died is a spent id"
          case spirits after of
            [first, second] -> do
              let describes oid =
                    ( Projection.powerOf oid after,
                      Projection.toughnessOf oid after,
                      Projection.colorsOf oid after,
                      Projection.subtypesOf oid after,
                      Projection.cardTypesOf oid after,
                      Projection.hasKeyword Keyword.Type.Flying oid after,
                      Projection.controllerOf oid after
                    )
                  expected =
                    ( Just (1 :: Integer),
                      Just (1 :: Integer),
                      Set.fromList [Color.White, Color.Black],
                      Set.singleton Subtype.Spirit,
                      Set.singleton CardType.Creature,
                      True,
                      Just S.alice
                    )
              Spec.assertEqWith s "the first is rule 702.135a's token exactly" (describes first) expected
              Spec.assertEqWith s "and so is the second" (describes second) expected
            other -> Spec.assertFailure s ("expected exactly two Spirit tokens, got " <> show other)
        -- The other half of the same board: a creature WITHOUT afterlife dying to
        -- the same Murder leaves nothing behind, so the tokens above are the
        -- keyword's doing and not the fixture's.
        Spec.it s "CR 702.135a a dying creature without afterlife leaves none" $ do
          (pikerId, settled, after) <- spiritsAfterKilling "Goblin Piker"
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject pikerId after)) "the Piker died to the same Murder"
          Spec.assertEqWith s "but nothing triggered" (GameState.stack settled) []
          Spec.assertEqWith s "and no tokens were created" (spirits after) []
        -- CR 702.135b: "if a permanent has multiple instances of afterlife, each
        -- triggers separately". Asserted of the MINT, as renown's multiplicity
        -- is, no card in the pool printing afterlife twice. A permanent that
        -- HOLDS two -- one printed, one Afterlife Insurance granted -- is a
        -- gameplay-level board, and Pawl.CounterspellSpec's "an evolved Ministrant
        -- plus a granted afterlife" is where it is played out.
        Spec.it s "CR 702.135b each instance of afterlife is its own ability" $ do
          Spec.assertEqWith s "afterlife 2 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Afterlife 2) 2)) [Keyword.afterlife 2, Keyword.afterlife 2]
          Spec.assertEqWith s "and afterlife 3 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Afterlife 3) 1)) [Keyword.afterlife 3]
          Spec.assertBool s (Keyword.afterlife 2 /= Keyword.afterlife 3) "and the N reaches the minted ability"

-- The pay-or-not answers in a transcript, in order.
payResponses :: [Response.Response] -> [Response.Response]
payResponses = filter isPayResponse

isPayResponse :: Response.Response -> Bool
isPayResponse response = case response of
  Response.ChoseToPay _ -> True
  _ -> False

-- CR 702.123 fabricate N: "When this permanent enters, you may put N +1/+1
-- counters on it. If you don't, create N 1/1 colorless Servo artifact creature
-- tokens." Rule 702.123a prints CR 118.12a's rewriting already done, so the
-- minted clause is one PayGate over
-- CostComponent.PutPlusOneCountersOnThis and the tokens are its "if you don't"
-- branch -- afterlife's mint with a gate on it, and the first minted keyword
-- ability that offers a COST at resolution. (Soulshift's and provoke's clauses
-- ask a question there too, but a printed "may" rather than a cost.)
--
-- Glint-Sleeve Artisan, {2}{W} Creature -- Dwarf Artificer 2/2, whose entire
-- text box is "Fabricate 1". Every reading is a different board: 3/3 with the
-- counter, 2/2 plus a Servo without it, 4/4 under Hardened Scales.
--
-- The first two cases start from the SAME board and the SAME settled trigger and
-- differ in NOTHING but alice's answer.
fabricateSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fabricateSpec s registry =
  let named name gs = filter (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack name))) (Set.toList (GameState.battlefield gs))
      artisansOn = named "Glint-Sleeve Artisan"
      -- The bearer cast from alice's hand off three lands and resolved, with its
      -- CR 603.6a enters trigger settled onto the stack but NOT resolved.
      -- `others` go onto alice's battlefield first.
      entersOnStack land bearer others =
        let base = List.foldl' (\g p -> snd (S.addCreature p S.alice g)) (S.landsInPlay land 3) others
            (gs, spellId) = S.handOne bearer base
            cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
         in S.runPure S.identityAnswer cast (Stack.resolveTop >> Engine.settleForPriority)
      boardOf landName bearerName others = do
        land <- S.printingOf s registry landName
        bearer <- S.printingOf s registry bearerName
        rest <- traverse (S.printingOf s registry) others
        pure (entersOnStack land bearer rest)
      board = boardOf "Plains" "Glint-Sleeve Artisan"
   in Spec.describe s "CR 702.123 fabricate" $ do
        Spec.it s "CR 702.123a paying the counter leaves the Artisan a 3/3 and makes no Servo" $ do
          onStack <- board []
          let ((_, after), transcript) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
          case artisansOn onStack of
            [artisanId] -> do
              -- The controls: the Artisan really entered, and its trigger really
              -- reached the stack, before anything below is read.
              Spec.assertEqWith s "it entered as a 2/2 with no counters" (S.powerToughnessOf artisanId onStack, S.counterOf CounterKind.PlusOnePlusOne artisanId onStack) (Just (2, 2), 0)
              Spec.assertEqWith s "CR 603.6a: its enters trigger is on the stack" (length (GameState.stack onStack)) 1
              Spec.assertEqWith s "alice was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
              Spec.assertEqWith s "CR 122.6: one +1/+1 counter went on" (S.counterOf CounterKind.PlusOnePlusOne artisanId after) 1
              Spec.assertEqWith s "CR 613.4c: so it reads 3/3" (S.powerToughnessOf artisanId after) (Just (3, 3))
              Spec.assertEqWith s "CR 118.12a: the paid branch made no Servo" (S.tokensOf after) []
            other -> Spec.assertFailure s ("expected one Glint-Sleeve Artisan, got " <> show (length other))
        -- Every characteristic rule 702.123a states is asserted, because the mint
        -- writes each of them out by hand and a wrong one compiles. Colorless
        -- matters most: the pool's other minted token, afterlife's Spirit, is
        -- white and black, so a mint copied from it would pass everything else.
        Spec.it s "CR 702.123a declining creates a 1/1 colorless Servo artifact creature" $ do
          onStack <- board []
          let ((_, after), transcript) = Replay.record S.identityAnswer onStack Stack.resolveTop
          case artisansOn onStack of
            [artisanId] -> do
              Spec.assertEqWith s "alice was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
              Spec.assertEqWith s "no counter went on" (S.counterOf CounterKind.PlusOnePlusOne artisanId after) 0
              Spec.assertEqWith s "so it is still a 2/2" (S.powerToughnessOf artisanId after) (Just (2, 2))
              case S.tokensOf after of
                [servoId] ->
                  Spec.assertEqWith
                    s
                    "the token is rule 702.123a's exactly"
                    ( fmap Face.name (Game.faceOf servoId after),
                      S.powerToughnessOf servoId after,
                      Projection.colorsOf servoId after,
                      Projection.cardTypesOf servoId after,
                      Projection.subtypesOf servoId after,
                      Projection.controllerOf servoId after
                    )
                    ( -- CR 111.4 names it, CR 105.2 makes an object with no mana
                      -- cost and no colour indicator colorless, and CR 111.2 gives
                      -- it to the ability's controller.
                      Just (CardName.MkCardName (Text.pack "Servo Token")),
                      Just (1, 1),
                      Set.empty,
                      Set.fromList [CardType.Artifact, CardType.Creature],
                      Set.singleton Subtype.Servo,
                      Just S.alice
                    )
                other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
            other -> Spec.assertFailure s ("expected one Glint-Sleeve Artisan, got " <> show (length other))
        -- CR 614.1 over a cost paid DURING a resolution: the board differs from
        -- the first case in nothing but the Hardened Scales, and what it proves
        -- is that fabricate's payment places its counter through CR 122.6's
        -- funnel rather than writing it onto the object directly.
        --
        -- NOT the payment moment. Hardened Scales is CR 614.1's passive subject,
        -- which reaches a placement at either moment; the moment's own split is
        -- pinned by Pawl.ReplacementSpec's blight pair and by
        -- Pawl.PlaneswalkerSpec's loyalty case, all three of them CR 614.16
        -- subjects (Doubling Season).
        Spec.it s "CR 614.1 Hardened Scales sees fabricate's counter, so the Artisan reads 4/4" $ do
          onStack <- board ["Hardened Scales"]
          let after = S.runPure (paysFor S.alice) onStack Stack.resolveTop
          case artisansOn onStack of
            [artisanId] -> do
              Spec.assertEqWith s "it still entered as a 2/2" (S.powerToughnessOf artisanId onStack) (Just (2, 2))
              Spec.assertEqWith s "one counter became two" (S.counterOf CounterKind.PlusOnePlusOne artisanId after) 2
              Spec.assertEqWith s "so it reads 4/4" (S.powerToughnessOf artisanId after) (Just (4, 4))
              Spec.assertEqWith s "and still no Servo" (S.tokensOf after) []
            other -> Spec.assertFailure s ("expected one Glint-Sleeve Artisan, got " <> show (length other))
        -- The keyword's N, at gameplay level and on BOTH halves. Weaponcraft
        -- Enthusiast, {2}{B} Creature -- Aetherborn Artificer 0/1, whose entire
        -- text box is "Fabricate 2": a mint that dropped the payload would put
        -- one counter on (1/2, not 2/3) and make one Servo, and 0/1 keeps every
        -- reading a different number from the Artisan's.
        Spec.it s "CR 702.123a fabricate 2 puts two counters on the Enthusiast" $ do
          onStack <- boardOf "Swamp" "Weaponcraft Enthusiast" []
          let after = S.runPure (paysFor S.alice) onStack Stack.resolveTop
          case named "Weaponcraft Enthusiast" onStack of
            [enthusiastId] -> do
              Spec.assertEqWith s "it entered as a 0/1" (S.powerToughnessOf enthusiastId onStack) (Just (0, 1))
              Spec.assertEqWith s "two +1/+1 counters went on" (S.counterOf CounterKind.PlusOnePlusOne enthusiastId after) 2
              Spec.assertEqWith s "so it reads 2/3" (S.powerToughnessOf enthusiastId after) (Just (2, 3))
              Spec.assertEqWith s "and no Servo" (S.tokensOf after) []
            other -> Spec.assertFailure s ("expected one Weaponcraft Enthusiast, got " <> show (length other))
        Spec.it s "CR 702.123a declining fabricate 2 creates two Servos" $ do
          onStack <- boardOf "Swamp" "Weaponcraft Enthusiast" []
          let after = S.runPure S.identityAnswer onStack Stack.resolveTop
          case named "Weaponcraft Enthusiast" onStack of
            [enthusiastId] -> do
              Spec.assertEqWith s "no counter went on" (S.counterOf CounterKind.PlusOnePlusOne enthusiastId after) 0
              Spec.assertEqWith s "so it is still a 0/1" (S.powerToughnessOf enthusiastId after) (Just (0, 1))
              Spec.assertEqWith s "and there are two Servos" (length (S.tokensOf after)) 2
            other -> Spec.assertFailure s ("expected one Weaponcraft Enthusiast, got " <> show (length other))
        -- CR 702.123b: "if a permanent has multiple instances of fabricate, each
        -- triggers separately". Asserted of the MINT, as afterlife's multiplicity
        -- is, no card in the pool printing fabricate twice.
        Spec.it s "CR 702.123b each instance of fabricate is its own ability" $ do
          Spec.assertEqWith s "fabricate 1 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Fabricate 1) 2)) [Keyword.fabricate 1, Keyword.fabricate 1]
          Spec.assertEqWith s "and fabricate 2 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Fabricate 2) 1)) [Keyword.fabricate 2]
          Spec.assertBool s (Keyword.fabricate 1 /= Keyword.fabricate 2) "and the N reaches the minted ability"

-- CR 702.21a ward [cost]: "Whenever this permanent becomes the target of a spell
-- or ability an opponent controls, counter that spell or ability unless that
-- player pays [cost]." A TRIGGER and not a targeting restriction, which is what
-- the first case proves: the spell is announced and paid for normally, and the
-- ability goes on the stack over it.
--
-- Tomakul Honor Guard, {1}{G} Creature -- Human Soldier 3/1, whose entire text
-- box is "Ward {2}", and Giant Growth as the thing that targets it: +3/+3 makes
-- the two outcomes 6/4 and 3/1, and a stack that never resolved reads 3/1 too --
-- which is why every case asserts the stack's height as well.
--
-- THREE SEATS. carol is neither the ward's controller nor the caster, so "an
-- opponent controls it" and "bob controls it" are different sentences.
--
-- THE PAIR: bob's Giant Growth and alice's are on the SAME board, aimed at the
-- SAME permanent, off the same three Forests each. The only difference is who
-- casts, which is the whole of rule 702.21a's "an opponent controls" -- and both
-- casters can afford the ward cost afterwards, so a spell that survives did so
-- because the ability never fired rather than because nobody could have paid.
--
-- `paysFor S.bob` throughout, which is what makes the payer assertion real:
-- alice is never paid for, so an engine that offered rule 702.21a's cost to the
-- ward's own controller falls through to Declines and counters the spell.
wardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
wardSpec s registry =
  let board forest guard growth =
        let withLands = S.landsFor forest S.bob 3 (S.landsFor forest S.alice 3 S.threePlayerGame)
            (guardId, withGuard) = S.addCreature guard S.alice withLands
            (bobsGrowth, withBobs) = S.addHandCard growth S.bob withGuard
            (alicesGrowth, gs) = S.addHandCard growth S.alice withBobs
         in ( guardId,
              bobsGrowth,
              alicesGrowth,
              gs
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
      -- The Honor Guard is the board's only creature, so identityAnswer's
      -- lowest-id choice of target is the only choice there is -- nothing here
      -- searches for a permanent that makes the assertion pass.
      castAndSettle caster oid gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast caster oid)) Engine.settleForPriority
      boardOf = do
        forest <- S.printingOf s registry "Forest"
        guard <- S.printingOf s registry "Tomakul Honor Guard"
        growth <- S.printingOf s registry "Giant Growth"
        pure (board forest guard growth)
   in Spec.describe s "CR 702.21 ward" $ do
        Spec.it s "CR 702.21a an opponent's spell fires ward, and declining counters it" $ do
          (guardId, bobsGrowth, _, gs) <- boardOf
          let onStack = castAndSettle S.bob bobsGrowth gs
              ((_, after), transcript) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
          -- The controls: the spell really was cast -- rule 702.21a is not a CR
          -- 115 targeting restriction -- and the ability really reached the
          -- stack over it.
          Spec.assertEqWith s "the Growth and the ward trigger are both on the stack" (length (GameState.stack onStack)) 2
          Spec.assertEqWith s "the Guard is still a 3/1 with the Growth unresolved" (S.powerToughnessOf guardId onStack) (Just (3, 1))
          -- alice is the one paid for, and she is never asked: the offer went to
          -- bob, who declined through the identity fallback.
          Spec.assertEqWith s "bob was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
          Spec.assertEqWith s "CR 701.6a: the Growth was countered, so the stack is empty" (length (GameState.stack after)) 0
          Spec.assertEqWith s "and it is in bob's graveyard" (Seq.length (Map.findWithDefault Seq.empty S.bob (GameState.graveyard after))) 1
          Spec.assertEqWith s "so the Guard is still a 3/1" (S.powerToughnessOf guardId after) (Just (3, 1))
        -- The same board and the same cast as the case above, differing in
        -- NOTHING but bob's answer.
        Spec.it s "CR 702.21a paying the ward cost leaves the spell to resolve" $ do
          (guardId, bobsGrowth, _, gs) <- boardOf
          let onStack = castAndSettle S.bob bobsGrowth gs
              ((_, after), transcript) = Replay.record (paysFor S.bob) onStack (Stack.resolveTop >> Engine.settleForPriority >> Stack.resolveTop)
          Spec.assertEqWith s "bob was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
          Spec.assertEqWith s "nothing was countered, so the Growth resolved" (S.powerToughnessOf guardId after) (Just (6, 4))
          Spec.assertEqWith s "and bob's graveyard holds the spent Growth" (Seq.length (Map.findWithDefault Seq.empty S.bob (GameState.graveyard after))) 1
        -- The relation, moved on its own: alice casts her OWN Giant Growth at
        -- her own warded creature, off her own three Forests. Rule 702.21a's
        -- condition is "a spell or ability AN OPPONENT controls", so nothing
        -- triggers -- and `paysFor S.bob` would leave a Declines in the
        -- transcript if the ability had fired and offered alice the cost.
        Spec.it s "CR 702.21a the ward controller's OWN spell fires nothing" $ do
          (guardId, _, alicesGrowth, gs) <- boardOf
          let onStack = castAndSettle S.alice alicesGrowth gs
              ((_, after), transcript) = Replay.record (paysFor S.bob) onStack Stack.resolveTop
          Spec.assertEqWith s "only the Growth is on the stack" (length (GameState.stack onStack)) 1
          Spec.assertEqWith s "nobody was offered a ward cost" (payResponses transcript) []
          Spec.assertEqWith s "and the Growth resolved" (S.powerToughnessOf guardId after) (Just (6, 4))
        -- The BEARER, moved on its own, and the pair's third axis after the
        -- relation: bob's Giant Growth names an ordinary creature standing
        -- beside the Honor Guard instead of the Guard. Rule 702.21a's subject is
        -- "this permanent", so a BecameTarget naming anything else must not fire
        -- it -- and with the Guard the board's only creature (every case above)
        -- an engine that fired on ANY targeted object would be green throughout.
        --
        -- The Piker is added here rather than in `board` so the cases above keep
        -- the one-creature board their identityAnswer targeting relies on, and
        -- this case pins its own target instead.
        Spec.it s "CR 702.21a a spell naming a DIFFERENT permanent fires nothing" $ do
          (guardId, bobsGrowth, _, gs) <- boardOf
          piker <- S.printingOf s registry "Goblin Piker"
          let (pikerId, withPiker) = S.addCreature piker S.alice gs
              aimedAtPiker :: Prompt.Prompt r -> r
              aimedAtPiker p = case p of
                Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, candidates) -> Set.filter (== Recipient.ToCreature pikerId) candidates) sets
                _ -> paysFor S.bob p
              onStack = S.runPure aimedAtPiker (S.runPure aimedAtPiker withPiker (S.cast S.bob bobsGrowth)) Engine.settleForPriority
              ((_, after), transcript) = Replay.record aimedAtPiker onStack Stack.resolveTop
          Spec.assertEqWith s "only the Growth is on the stack" (length (GameState.stack onStack)) 1
          Spec.assertEqWith s "nobody was offered a ward cost" (payResponses transcript) []
          Spec.assertEqWith s "and the Growth resolved onto the PIKER" (S.powerToughnessOf pikerId after) (Just (5, 4))
          Spec.assertEqWith s "leaving the Guard a 3/1" (S.powerToughnessOf guardId after) (Just (3, 1))
        -- Rule 702.21 states no "each instance" sentence, so two instances are
        -- two abilities for CR 603.2's general reason. Asserted of the MINT, as
        -- fabricate's multiplicity is: no printing carries ward twice.
        Spec.it s "CR 603.2 each instance of ward is its own ability" $ do
          let cost n = Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) []
          Spec.assertEqWith s "ward {2} held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Ward (cost 2)) 2)) [Keyword.ward (cost 2), Keyword.ward (cost 2)]
          Spec.assertBool s (Keyword.ward (cost 2) /= Keyword.ward (cost 3)) "and the cost reaches the minted ability"

-- CR 118.12a over MORE THAN ONE PAYER: "[Do something] unless [a player does
-- something else]" means "[A player may do something else]. If [that player
-- doesn't], [do something]", and a card that says "each opponent" makes that
-- offer once per opponent -- in CR 101.4's APNAP order, each answer gating that
-- opponent's own instruction and nobody else's.
--
-- Rishadan Cutpurse, {2}{U} Creature -- Human Pirate 1/1, whose entire text box
-- is "When this creature enters, each opponent sacrifices a permanent of their
-- choice unless they pay {1}." The gate's payer is PlayerRef.Relative Opponent
-- and its IfNotPaid branch binds the seats that declined under
-- Binding.gatePlayers, which the Effect.PlayerSacrifices reads as "they".
--
-- THREE SEATS, and the point of them: with two, "each opponent" and "the one
-- opponent" are the same sentence, so an engine that offered the cost once and
-- applied one answer to everybody would be green. Here bob PAYS and carol does
-- not, so the two opponents must end the resolution on different boards.
--
-- The observable is HOW MANY PERMANENTS EACH SEAT CONTROLS, never a prompt
-- count: bob keeps his (paying taps a Forest, it does not lose him one) and
-- carol loses one. The three seats are stocked to three DIFFERENT sizes so no
-- two readings of the rule produce the same triple.
cutpurseSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cutpurseSpec s registry =
  let controls pid gs = length (Projection.controls pid gs)
      board island forest swamp piker cutpurse =
        let withLands = S.landsFor swamp S.carol 3 (S.landsFor forest S.bob 2 (S.landsFor island S.alice 3 S.threePlayerGame))
            withPikers = List.foldl' (\acc pid -> snd (S.addCreature piker pid acc)) withLands [S.bob, S.carol, S.carol]
            (gs, cutpurseId) = S.handOne cutpurse withPikers
         in (cutpurseId, gs)
      -- The Cutpurse cast off alice's three Islands and resolved, with its CR
      -- 603.6a enters trigger settled onto the stack but NOT resolved.
      entersOnStack cutpurseId gs =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice cutpurseId)
         in S.runPure S.identityAnswer cast (Stack.resolveTop >> Engine.settleForPriority)
      boardOf = do
        island <- S.printingOf s registry "Island"
        forest <- S.printingOf s registry "Forest"
        swamp <- S.printingOf s registry "Swamp"
        piker <- S.printingOf s registry "Goblin Piker"
        cutpurse <- S.printingOf s registry "Rishadan Cutpurse"
        let (cutpurseId, gs) = board island forest swamp piker cutpurse
        pure (entersOnStack cutpurseId gs)
   in Spec.describe s "CR 118.12 a gate offered to each opponent" $ do
        Spec.it s "CR 118.12a bob pays and carol does not, so only carol sacrifices" $ do
          onStack <- boardOf
          let ((_, after), transcript) = Replay.record (paysFor S.bob) onStack Stack.resolveTop
          -- The controls: the Cutpurse really entered and its trigger really
          -- reached the stack, and the three seats really start apart.
          Spec.assertEqWith s "CR 603.6a: its enters trigger is on the stack" (length (GameState.stack onStack)) 1
          Spec.assertEqWith s "alice, bob and carol start with 4, 3 and 5 permanents" (controls S.alice onStack, controls S.bob onStack, controls S.carol onStack) (4, 3, 5)
          -- THE BEHAVIOUR: carol alone lost a permanent. bob's answer bought him
          -- out of an edict carol's answer did not buy her out of, and alice --
          -- not an opponent of herself -- was never in it.
          Spec.assertEqWith s "CR 118.12a: carol alone sacrificed" (controls S.alice after, controls S.bob after, controls S.carol after) (4, 3, 4)
          -- The supporting check: two offers, in CR 101.4's APNAP order, with
          -- alice never asked.
          Spec.assertEqWith s "CR 101.4: bob was asked first and paid, then carol declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays, Response.ChoseToPay PaymentDecision.Declines]
        -- The same board and the same trigger as the case above, differing in
        -- NOTHING but who is paid for: bob now declines too, so both edicts land
        -- and the pair tells "carol was asked" apart from "carol was swept up in
        -- bob's answer".
        Spec.it s "CR 118.12a nobody pays, so both opponents sacrifice" $ do
          onStack <- boardOf
          let ((_, after), transcript) = Replay.record S.identityAnswer onStack Stack.resolveTop
          Spec.assertEqWith s "CR 118.12a: both opponents sacrificed and alice did not" (controls S.alice after, controls S.bob after, controls S.carol after) (4, 2, 4)
          Spec.assertEqWith s "both were asked, and both declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines, Response.ChoseToPay PaymentDecision.Declines]
        -- The other limb, moved on its own: both opponents pay, so the IfNotPaid
        -- branch selects nobody and the clause does nothing at all.
        Spec.it s "CR 118.12a both opponents pay, so nobody sacrifices" $ do
          onStack <- boardOf
          let paysForEither p = case p of
                Prompt.ChooseToPay {} -> PaymentDecision.Pays
                _ -> S.identityAnswer p
              after = S.runPure paysForEither onStack Stack.resolveTop
          Spec.assertEqWith s "CR 118.12a: the paid branch sacrificed nothing" (controls S.alice after, controls S.bob after, controls S.carol after) (4, 3, 5)

-- CR 601.2c read from the TARGETED PLAYER's side: "the chosen objects and/or
-- players each become a target of that spell". Dormant Gomazoa, {1}{U}{U}
-- Creature -- Jellyfish 5/5: "Flying / This creature enters tapped. / This
-- creature doesn't untap during your untap step. / Whenever you become the
-- target of a spell, you may untap this creature."
--
-- The first card in the pool that watches a PLAYER become a target, and the
-- reason GameEvent.BecameTarget carries a Recipient rather than an ObjectId.
--
-- "A SPELL", not "a spell or ability" -- which is why BecameTarget also carries
-- a StackObjectKind, and why the Ravenous Rats leg is here: CR 602.2b and CR
-- 603.3d route an ability through the same rule 601.2c, and the matcher is pure
-- with no GameState to look that stack object up in.
--
-- TAPPEDNESS IS THE SIGNAL, never power or toughness: CR 502.3 keeps this
-- creature from untapping in its own untap step while CR 701.26b's Untap action
-- is what the trigger performs, so the two answers "the trigger fired" and "it
-- did not" are exactly untapped and tapped.
--
-- Every leg runs under `taking`, an ACCEPTING answerer. S.identityAnswer
-- declines a CR 603.5 "may" (Replay.defaultAnswer), which would leave the
-- Gomazoa tapped in every leg and make the three absences below prove nothing.
gomazoaSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gomazoaSpec s registry =
  let -- Accepts CR 603.5's "may" and pins every target slot at one recipient.
      -- The offered set is FILTERED rather than replaced, so a leg that names a
      -- recipient the engine never offered chooses nothing instead of quietly
      -- passing a target the CR 608.2b re-read would drop.
      taking :: Recipient.Recipient -> Prompt.Prompt r -> r
      taking r p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, candidates) -> Set.filter (== r) candidates) sets
        _ -> S.identityAnswer p
      tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)
      -- THREE SEATS: alice controls the Gomazoa, bob casts everything, and carol
      -- is a target that is neither. Two seats would collapse "a player who is
      -- not you" onto the caster.
      board = do
        island <- S.printingOf s registry "Island"
        swamp <- S.printingOf s registry "Swamp"
        forest <- S.printingOf s registry "Forest"
        piker <- S.printingOf s registry "Goblin Piker"
        gomazoa <- S.printingOf s registry "Dormant Gomazoa"
        fatigue <- S.printingOf s registry "Fatigue"
        rats <- S.printingOf s registry "Ravenous Rats"
        growth <- S.printingOf s registry "Giant Growth"
        -- Three colors because the three producers are {1}{U}, {1}{B} and {G};
        -- one leg casts one spell, so the lands never compete.
        let g0 = S.landsFor forest S.bob 1 (S.landsFor swamp S.bob 2 (S.landsFor island S.bob 2 S.threePlayerGame))
            (gomazoaId, g1) = S.addCreature gomazoa S.alice g0
            -- alice's control for the untap-step leg: an ordinary creature under
            -- the same player, tapped the same way, differing only in the
            -- restriction.
            (pikerId, g2) = S.addCreature piker S.alice g1
            (fatigueId, g3) = S.addHandCard fatigue S.bob g2
            (ratsId, g4) = S.addHandCard rats S.bob g3
            (growthId, g5) = S.addHandCard growth S.bob g4
            -- One card in alice's hand, which is the Ravenous Rats leg's control:
            -- the ability really did name her.
            (_, g6) = S.addHandCard island S.alice g5
            -- CR 104.3c: nothing here draws, but an empty library is a loss
            -- waiting for any leg that advances.
            stocked = List.foldl' (\g pid -> snd (S.addLibraryCard island pid g)) g6 [S.alice, S.bob, S.carol, S.alice, S.bob, S.carol]
            tapped = S.tapObject pikerId (S.tapObject gomazoaId stocked)
        pure
          ( gomazoaId,
            pikerId,
            fatigueId,
            ratsId,
            growthId,
            tapped
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.bob,
                GameState.priority = Just S.bob
              }
          )
      -- Cast one spell at `r`, settle so any trigger reaches the stack, then
      -- resolve the top under the same accepting answerer. `taking r` is written
      -- out at each use rather than bound once: it is polymorphic in the prompt's
      -- answer type, and a let-bound copy would be pinned to one.
      castThenResolve r caster oid gs =
        let onStack = S.runPure (taking r) (S.runPure (taking r) gs (S.cast caster oid)) Engine.settleForPriority
            ((_, after), transcript) = Replay.record (taking r) onStack Stack.resolveTop
         in (onStack, after, transcript)
   in Spec.describe s "CR 601.2c a player becoming the target of a spell" $ do
        -- The positive the three absences below rest on. Fatigue rather than
        -- Ancestral Recall on purpose: it draws nothing, so CR 704.5b cannot end
        -- the game mid-leg and no life total moves.
        Spec.it s "CR 601.2c whole card: a spell targeting the Gomazoa's controller untaps it" $ do
          (gomazoaId, _, fatigueId, _, _, gs) <- board
          let (onStack, after, transcript) = castThenResolve (Recipient.ToPlayer S.alice) S.bob fatigueId gs
          Spec.assertEqWith s "the Gomazoa starts tapped" (tapStateOf gomazoaId gs) (Just TapState.Tapped)
          Spec.assertEqWith s "CR 701.26b the trigger untapped it" (tapStateOf gomazoaId after) (Just TapState.Untapped)
          Spec.assertEqWith s "CR 603.5 its controller was asked the may exactly once" (optionalResponses transcript) [Response.ChoseOptional OptionalDecision.Exercises]
          Spec.assertEqWith s "the Fatigue and the trigger were both on the stack" (length (GameState.stack onStack)) 2
        -- CR 109.5: the same board and the same spell, differing in NOTHING but
        -- which player it names. carol is neither the Gomazoa's controller nor
        -- the caster.
        Spec.it s "CR 109.5 a spell targeting another player leaves it tapped" $ do
          (gomazoaId, _, fatigueId, _, _, gs) <- board
          let (onStack, after, transcript) = castThenResolve (Recipient.ToPlayer S.carol) S.bob fatigueId gs
          Spec.assertEqWith s "the Gomazoa is still tapped" (tapStateOf gomazoaId after) (Just TapState.Tapped)
          Spec.assertEqWith s "and nobody was offered the may" (optionalResponses transcript) []
          Spec.assertEqWith s "only the Fatigue was on the stack" (length (GameState.stack onStack)) 1
        -- CR 112.1 against CR 113.3, which is the whole of BecameTarget.kind:
        -- Ravenous Rats' SPELL targets nothing and its enters-trigger targets an
        -- opponent, so the only BecameTarget on this board is an ability's.
        Spec.it s "CR 112.1 an ABILITY targeting the Gomazoa's controller leaves it tapped" $ do
          (gomazoaId, _, _, ratsId, _, gs) <- board
          let atAlice = Recipient.ToPlayer S.alice
              entered = S.runPure (taking atAlice) (S.runPure (taking atAlice) gs (S.cast S.bob ratsId)) Stack.resolveTop
              triggerOnStack = S.runPure (taking atAlice) entered Engine.settleForPriority
              ((_, after), transcript) = Replay.record (taking atAlice) triggerOnStack Stack.resolveTop
          Spec.assertEqWith s "the Gomazoa is still tapped" (tapStateOf gomazoaId after) (Just TapState.Tapped)
          Spec.assertEqWith s "and nobody was offered the may" (optionalResponses transcript) []
          -- The controls, which are what stop this leg passing because nothing
          -- happened at all: the ability reached the stack and really named her.
          Spec.assertEqWith s "CR 603.3d the Rats' enters trigger reached the stack" (length (GameState.stack triggerOnStack)) 1
          Spec.assertEqWith s "and alice discarded the card it targeted her for" (S.handSize S.alice after) 0
        -- CR 115.1's other kind of target on the same event: the Gomazoa itself
        -- becomes one, which is SelfBecomesTargeted's condition and not this one.
        Spec.it s "CR 115.1 the Gomazoa becoming a target itself is not its controller becoming one" $ do
          (gomazoaId, _, _, _, growthId, gs) <- board
          let (onStack, after, transcript) = castThenResolve (Recipient.ToCreature gomazoaId) S.bob growthId gs
          Spec.assertEqWith s "the Gomazoa is still tapped" (tapStateOf gomazoaId after) (Just TapState.Tapped)
          Spec.assertEqWith s "and nobody was offered the may" (optionalResponses transcript) []
          Spec.assertEqWith s "only the Growth was on the stack" (length (GameState.stack onStack)) 1
          Spec.assertEqWith s "and it resolved onto the Gomazoa" (S.powerToughnessOf gomazoaId after) (Just (8, 8))
        -- The card's third line, which is what makes the trigger worth printing:
        -- CR 502.3's turn-based untap passes the Gomazoa over while CR 701.26b's
        -- Untap action above does not. The Piker is the pair's other half.
        Spec.it s "CR 502.3 the Gomazoa does not untap in its controller's untap step, and an ordinary creature does" $ do
          (gomazoaId, pikerId, _, _, _, gs) <- board
          let untapped = S.runPure S.identityAnswer (gs {GameState.activePlayer = S.alice}) (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))
          Spec.assertEqWith s "the Gomazoa is still tapped" (tapStateOf gomazoaId untapped) (Just TapState.Tapped)
          Spec.assertEqWith s "and the Piker untapped" (tapStateOf pikerId untapped) (Just TapState.Untapped)

-- CR 601.2c read from the targeted PLAYER's side again, with the two narrowings
-- Dormant Gomazoa above does not print. Amulet of Safekeeping, {2} Artifact:
-- "Whenever you become the target of a spell or ability an opponent controls,
-- counter that spell or ability unless its controller pays {1}. / Creature
-- tokens get -1/-0."
--
-- What is new over the Gomazoa is the whole of the condition's payload: the
-- ABILITY half of CR 601.2c (its "a spell or ability" narrows to neither limb,
-- so CR 602.2b's and CR 603.3d's road counts too), and a PlayerRelation over CR
-- 405.4's controller of the targeting object. On the payload side it is the
-- first CARD-authored payGate whose payer is a reserved BINDING slot rather than
-- a target slot -- rule 702.21a's ward mints that shape, and Amulet writes it.
--
-- LIFE AND HAND SIZE ARE THE SIGNALS, never a stack height or a graveyard
-- length: a countered Lightning Bolt is exactly "alice is still at 20", and a
-- countered discard ability is exactly "alice still holds her card". The stack
-- and graveyard readings are controls, and they come AFTER.
--
-- THREE SEATS: alice controls the Amulet, bob casts everything, and carol is a
-- targeted player who is neither.
amuletSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
amuletSpec s registry =
  let -- Pins every target slot at one recipient AND pays wherever `who` is
      -- offered a cost. The offered set is FILTERED rather than replaced,
      -- gomazoaSpec's reason: a leg naming a recipient the engine never offered
      -- chooses nothing instead of quietly passing a target CR 608.2b's re-read
      -- would drop.
      aimedPaying :: Recipient.Recipient -> PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimedPaying r who p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, candidates) -> Set.filter (== r) candidates) sets
        _ -> paysFor who p
      goblinTokens gs = filter (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Goblin Token"))) (Set.toList (GameState.battlefield gs))
      board = do
        mountain <- S.printingOf s registry "Mountain"
        swamp <- S.printingOf s registry "Swamp"
        island <- S.printingOf s registry "Island"
        amulet <- S.printingOf s registry "Amulet of Safekeeping"
        piker <- S.printingOf s registry "Goblin Piker"
        bolt <- S.printingOf s registry "Lightning Bolt"
        rats <- S.printingOf s registry "Ravenous Rats"
        fodder <- S.printingOf s registry "Dragon Fodder"
        -- bob's five lands cover every leg's spell AND the {1} on top of it:
        -- {R} plus one for the Bolt, {1}{B} plus one for the Rats, {1}{R} for
        -- the Fodder. alice's one Mountain covers her own Bolt and nothing else.
        let g0 = S.landsFor swamp S.bob 2 (S.landsFor mountain S.bob 3 (S.landsFor mountain S.alice 1 S.threePlayerGame))
            (amuletId, g1) = S.addCreature amulet S.alice g0
            -- The static leg's other half: a NONTOKEN creature standing beside
            -- the tokens, which is what makes Filter.IsToken load-bearing.
            (pikerId, g2) = S.addCreature piker S.bob g1
            (bobsBolt, g3) = S.addHandCard bolt S.bob g2
            (ratsId, g4) = S.addHandCard rats S.bob g3
            (fodderId, g5) = S.addHandCard fodder S.bob g4
            -- alice's ONE card in hand, which is both the Ravenous Rats leg's
            -- precondition -- a hand to discard from -- and the relation leg's
            -- spell.
            (alicesBolt, g6) = S.addHandCard bolt S.alice g5
            -- CR 104.3c: nothing here draws, but an empty library is a loss
            -- waiting for any leg that advances.
            stocked = List.foldl' (\g pid -> snd (S.addLibraryCard island pid g)) g6 [S.alice, S.bob, S.carol, S.alice, S.bob, S.carol]
        pure
          ( amuletId,
            pikerId,
            bobsBolt,
            ratsId,
            fodderId,
            alicesBolt,
            stocked
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.bob,
                GameState.priority = Just S.bob
              }
          )
      -- Cast one spell at `r`, settle so any trigger reaches the stack, then
      -- resolve the stack DOWN under the same answerer. The answerer is written
      -- out at each use rather than bound once: it is polymorphic in the
      -- prompt's answer type, and a let-bound copy would be pinned to one.
      --
      -- TWO resolutions, not one, and that is what makes the life readings
      -- load-bearing: a trigger that counters nothing leaves the Bolt sitting on
      -- the stack, and one resolution cannot tell that apart from a Bolt that
      -- was countered. Stack.resolveTop is a no-op on an empty stack, so the
      -- second is harmless when the first emptied it.
      resolveDown = Stack.resolveTop >> Engine.settleForPriority >> Stack.resolveTop
      castThenResolve r payer caster oid gs =
        let onStack = S.runPure (aimedPaying r payer) (S.runPure (aimedPaying r payer) gs (S.cast caster oid)) Engine.settleForPriority
            ((_, after), transcript) = Replay.record (aimedPaying r payer) onStack resolveDown
         in (onStack, after, transcript)
   in Spec.describe s "CR 601.2c a player becoming the target of a spell OR an ability" $ do
        -- The positive, and the whole point of the card.
        Spec.it s "CR 601.2c whole card: an opponent's SPELL naming alice is countered when its controller declines" $ do
          (_, _, bobsBolt, _, _, _, gs) <- board
          let (onStack, after, transcript) = castThenResolve (Recipient.ToPlayer S.alice) S.carol S.bob bobsBolt gs
          Spec.assertEqWith s "CR 701.6a the Bolt never resolved, so alice is still at 20" (S.lifeOf S.alice after) (Just 20)
          Spec.assertEqWith s "CR 405.4 bob, the Bolt's controller, was asked exactly once and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
          -- The controls: the spell really was cast, and the trigger really
          -- reached the stack over it.
          Spec.assertEqWith s "the Bolt and the trigger were both on the stack" (length (GameState.stack onStack)) 2
          Spec.assertEqWith s "CR 701.6a and the countered Bolt is in bob's graveyard" (Seq.length (Map.findWithDefault Seq.empty S.bob (GameState.graveyard after))) 1
        -- The same board and the same cast as the case above, differing in
        -- NOTHING but bob's answer.
        Spec.it s "CR 118.12a paying the {1} leaves the spell to resolve" $ do
          (_, _, bobsBolt, _, _, _, gs) <- board
          let (_, after, transcript) = castThenResolve (Recipient.ToPlayer S.alice) S.bob S.bob bobsBolt gs
          Spec.assertEqWith s "the Bolt resolved onto alice" (S.lifeOf S.alice after) (Just 17)
          Spec.assertEqWith s "bob was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
        -- The ABILITY half, which is the whole of the payload's `kind = Nothing`
        -- and the leg Dormant Gomazoa's printing cannot reach: Ravenous Rats'
        -- SPELL targets nothing, and its CR 603.3d enters trigger targets alice.
        Spec.it s "CR 113.3c an opponent's TRIGGERED ABILITY naming alice is countered too" $ do
          (_, _, _, ratsId, _, _, gs) <- board
          let atAlice = Recipient.ToPlayer S.alice
              entered = S.runPure (aimedPaying atAlice S.carol) (S.runPure (aimedPaying atAlice S.carol) gs (S.cast S.bob ratsId)) Stack.resolveTop
              triggerOnStack = S.runPure (aimedPaying atAlice S.carol) entered Engine.settleForPriority
              ((_, after), transcript) = Replay.record (aimedPaying atAlice S.carol) triggerOnStack resolveDown
          Spec.assertEqWith s "alice starts with one card in hand" (S.handSize S.alice gs) 1
          Spec.assertEqWith s "CR 701.6a the discard ability was countered, so alice still holds it" (S.handSize S.alice after) 1
          Spec.assertEqWith s "bob was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
          -- The controls that stop this leg passing because nothing happened at
          -- all: the Rats' SPELL resolved, and its ability really reached the
          -- stack under the Amulet's trigger.
          Spec.assertEqWith s "the Rats are on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Ravenous Rats")) S.bob after) 1
          Spec.assertEqWith s "CR 603.3d the Rats' trigger and the Amulet's were both on the stack" (length (GameState.stack triggerOnStack)) 2
        -- The RELATION, moved on its own: the same board and the same spell at
        -- the same seat, differing only in who casts it. CR 405.4's controller
        -- is then alice herself, whom PlayerRelation.Opponent excludes.
        Spec.it s "CR 405.4 the Amulet controller's OWN spell naming her fires nothing" $ do
          (_, _, _, _, _, alicesBolt, gs) <- board
          let (onStack, after, transcript) = castThenResolve (Recipient.ToPlayer S.alice) S.carol S.alice alicesBolt gs
          Spec.assertEqWith s "her own Bolt resolved onto her" (S.lifeOf S.alice after) (Just 17)
          Spec.assertEqWith s "and nobody was offered the {1}" (payResponses transcript) []
          Spec.assertEqWith s "only the Bolt was on the stack" (length (GameState.stack onStack)) 1
        -- The RECIPIENT, moved on its own: the same cast by the same player, one
        -- seat over. CR 109.5's "you" is the Amulet's controller, and carol is
        -- neither that nor the caster.
        Spec.it s "CR 109.5 an opponent's spell naming ANOTHER player fires nothing" $ do
          (_, _, bobsBolt, _, _, _, gs) <- board
          let (onStack, after, transcript) = castThenResolve (Recipient.ToPlayer S.carol) S.carol S.bob bobsBolt gs
          Spec.assertEqWith s "the Bolt resolved onto carol" (S.lifeOf S.carol after) (Just 17)
          Spec.assertEqWith s "and alice is untouched" (S.lifeOf S.alice after) (Just 20)
          Spec.assertEqWith s "and nobody was offered the {1}" (payResponses transcript) []
          Spec.assertEqWith s "only the Bolt was on the stack" (length (GameState.stack onStack)) 1
        -- The card's second line, CR 111.6 read through CR 613.1g's layer 7:
        -- Dragon Fodder's two 1/1 Goblin TOKENS against a nontoken Goblin Piker
        -- standing on the same board. CR 704.5f does not reach the tokens -- a
        -- 0/1 is alive -- so this stays a power/toughness reading.
        Spec.it s "CR 111.6 creature TOKENS get -1/-0 and nontoken creatures do not" $ do
          (_, pikerId, _, _, fodderId, _, gs) <- board
          let resolved = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast S.bob fodderId)) Stack.resolveTop
          Spec.assertEqWith s "CR 613.1g the two Goblin tokens are 0/1" (fmap (\oid -> S.powerToughnessOf oid resolved) (goblinTokens resolved)) [Just (0, 1), Just (0, 1)]
          Spec.assertEqWith s "and the nontoken Piker is untouched" (S.powerToughnessOf pikerId resolved) (Just (2, 1))

-- The CR 603.5 "may" answers in a transcript, in order -- paysFor's
-- payResponses one Response constructor over. A transcript with no
-- ChooseOptional in it says the prompt was never raised, which is what makes an
-- absence leg above assert more than a tap state.
optionalResponses :: [Response.Response] -> [Response.Response]
optionalResponses = filter isOptionalResponse

isOptionalResponse :: Response.Response -> Bool
isOptionalResponse response = case response of
  Response.ChoseOptional _ -> True
  _ -> False

-- Blind Hunter, a Creature -- Bat: "Flying / Haunt (When this creature dies,
-- exile it haunting target creature.) / When this creature enters or the
-- creature it haunts dies, target player loses 2 life and you gain 2 life."
-- The pool's witness for an ability that functions FROM EXILE (CR 113.6k, CR
-- 702.55c), and the card rule 113.6k's own example is written about.
--
-- THREE SEATS, because the rule names three different players: alice owns the
-- Blind Hunter and so controls both of its triggers (CR 603.3a on the
-- battlefield, CR 108.4a in exile), bob controls the creature it haunts, and
-- carol is what "target player loses 2 life" names. A two-player board would
-- collapse the last two and could not tell "the haunted creature's controller"
-- from "the target".
--
-- The board is played out rather than assembled: alice bolts her own Blind
-- Hunter, haunt's dies trigger exiles the card haunting bob's Piker, and a
-- second bolt kills that Piker. Only then is the exile-zone ability asked for.
hauntSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
hauntSpec s registry =
  let -- Every target slot pinned at one recipient, never "the least legal one":
      -- both Pikers are legal haunt targets and all three players are legal
      -- "target player"s, so S.identityAnswer would answer by sort order and the
      -- assertions could not tell which object the effect reached.
      aimAt :: Recipient.Recipient -> Prompt.Prompt r -> r
      aimAt r p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton r)) sets
        _ -> S.identityAnswer p
      board = do
        mountain <- S.printingOf s registry "Mountain"
        bolt <- S.printingOf s registry "Lightning Bolt"
        hunter <- S.printingOf s registry "Blind Hunter"
        piker <- S.printingOf s registry "Goblin Piker"
        let g0 = S.landsFor mountain S.alice 2 S.threePlayerGame
            (hunterId, g1) = S.addCreature hunter S.alice g0
            (victimId, g2) = S.addCreature piker S.bob g1
            -- An IDENTICAL second Piker under the same player, never haunted:
            -- the pair is what makes the link observable, since the two differ
            -- in nothing else.
            (bystanderId, g3) = S.addCreature piker S.bob g2
            (g4, firstBolt) = S.handOne bolt g3
            (secondBolt, g5) = S.addHandCard bolt S.alice g4
            -- CR 104.3c: nothing here draws, but an empty library is a loss
            -- waiting for any leg that advances a turn.
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard mountain S.alice g)) g5 [1 .. 5 :: Int]
        pure (hunterId, victimId, bystanderId, firstBolt, secondBolt, stocked)
      -- Cast a Lightning Bolt at one creature and resolve it. The bolt's own
      -- target is pinned here; whatever the death triggers ask is pinned by the
      -- caller's own answerer, since the two choices are different questions.
      boltAt :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      boltAt oid spell gs =
        let cast = S.runPure (aimAt (Recipient.ToCreature oid)) gs (S.cast S.alice spell)
         in S.runPure (aimAt (Recipient.ToCreature oid)) cast Stack.resolveTop
      -- CR 704 kills the creature and CR 603.3 puts the death trigger on the
      -- stack, choosing its targets (CR 603.3d) under `answer`; then the trigger
      -- resolves.
      settleAndResolve :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> (GameState.GameState, GameState.GameState)
      settleAndResolve answer gs =
        let settled = S.runPure answer gs Engine.settleForPriority
         in (settled, S.runPure answer settled Stack.resolveTop)
      -- The whole first half: the Blind Hunter dies and haunt exiles it haunting
      -- bob's Piker.
      haunted (hunterId, victimId, _, firstBolt, _, gs) =
        settleAndResolve (aimAt (Recipient.ToCreature victimId)) (boltAt hunterId firstBolt gs)
      -- The one difference between the pair below and the board above: the
      -- haunting card sits in a graveyard instead of exile. CR 400.7 mints a
      -- fresh id for it, so rule 702.55b's link is re-filed under that id --
      -- otherwise the two boards would differ in the LINK as well as in the zone
      -- and the negative would pass for the wrong reason.
      fromExileToGraveyard gs = case Set.toList (GameState.exile gs) of
        [oid] ->
          let (mNew, moved) = S.runPureWith S.identityAnswer gs (Event.changeZoneReturning oid Zone.Graveyard)
              link = Map.lookup oid (GameState.haunting gs)
           in case (Foldable.toList mNew, link) of
                (newId : _, Just hauntedId) -> moved {GameState.haunting = Map.insert newId hauntedId (Map.delete oid (GameState.haunting moved))}
                _ -> moved
        _ -> gs
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
   in Spec.describe s "CR 702.55 haunt" $ do
        -- The proving case for the whole unit: the ability fires from EXILE.
        Spec.it s "CR 702.55c whole card: the haunting card in exile sees the creature it haunts die" $ do
          fixture@(_, victimId, _, _, secondBolt, _) <- board
          let (_, exiled) = haunted fixture
              killed = boltAt victimId secondBolt exiled
              (placed, after) = settleAndResolve (aimAt (Recipient.ToPlayer S.carol)) killed
          Spec.assertEqWith s "one card is in exile" (Set.size (GameState.exile exiled)) 1
          Spec.assertEqWith s "and it haunts exactly one object" (Map.size (GameState.haunting exiled)) 1
          Spec.assertEqWith s "haunt itself changed nobody's life total" (lives exiled) (Just 20, Just 20, Just 20)
          Spec.assertEqWith s "the exile-zone trigger reached the stack in that settle" (length (GameState.stack placed)) 1
          -- Three different answers on three different seats, which is what the
          -- three seats are for: the target loses, the haunting card's OWNER
          -- gains (CR 108.4a gives a card in exile no other controller), and the
          -- player whose creature died is untouched.
          Spec.assertEqWith s "carol lost 2, alice gained 2, bob is untouched" (lives after) (Just 22, Just 20, Just 18)
        -- The link, not the zone: same board, same exile, and the creature that
        -- dies is the OTHER Piker.
        Spec.it s "CR 702.55b only the creature the card haunts fires it" $ do
          fixture@(_, _, bystanderId, _, secondBolt, _) <- board
          let (_, exiled) = haunted fixture
              killed = boltAt bystanderId secondBolt exiled
              (placed, after) = settleAndResolve (aimAt (Recipient.ToPlayer S.carol)) killed
          Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack placed)) 0
          Spec.assertEqWith s "and no life total moved" (lives after) (Just 20, Just 20, Just 20)
        -- The zone, not the link: the same haunting card, still haunting the same
        -- Piker, moved to a graveyard. CR 113.6k puts this ability in exile alone,
        -- so a graveyard reading of it must find nothing -- which is what
        -- eventTriggers' `inExile` source is for, since `inGraveyards` filters on
        -- the same functionsIn call and rejects it.
        Spec.it s "CR 113.6k the same ability does not fire from a graveyard" $ do
          fixture@(_, victimId, _, _, secondBolt, _) <- board
          let (_, exiled) = haunted fixture
              moved = fromExileToGraveyard exiled
              killed = boltAt victimId secondBolt moved
              (placed, after) = settleAndResolve (aimAt (Recipient.ToPlayer S.carol)) killed
          Spec.assertEqWith s "the card is out of exile" (Set.size (GameState.exile moved)) 0
          Spec.assertEqWith s "and still haunts the Piker" (Map.size (GameState.haunting moved)) 1
          Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack placed)) 0
          Spec.assertEqWith s "and no life total moved" (lives after) (Just 20, Just 20, Just 20)

soulshiftSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
soulshiftSpec s registry =
  let ancestorName = CardName.MkCardName (Text.pack "Disowned Ancestor")
      -- Rule 702.46a's "you may", exercised. S.identityAnswer declines it, which
      -- is what the declining leg below rides.
      exercising :: Prompt.Prompt r -> r
      exercising p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      board = do
        swamp <- S.printingOf s registry "Swamp"
        murder <- S.printingOf s registry "Murder"
        kami <- S.printingOf s registry "Kami of Empty Graves"
        piker <- S.printingOf s registry "Goblin Piker"
        shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
        ancestor <- S.printingOf s registry "Disowned Ancestor"
        let (kamiId, g1) = S.addCreature kami S.alice (S.landsInPlay swamp 3)
            (pikerId, g2) = S.addGraveyardCard piker S.alice g1
            (shimatsuId, g3) = S.addGraveyardCard shimatsu S.alice g2
            (theirsId, g4) = S.addGraveyardCard ancestor S.bob g3
            (mineId, g5) = S.addGraveyardCard ancestor S.alice g4
            -- CR 104.3c: nothing here draws, but a stocked library keeps a leg
            -- from ending on an empty one.
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard swamp S.alice g)) g5 [1 .. 5 :: Int]
        pure (kamiId, (pikerId, shimatsuId, theirsId, mineId), S.handOne murder stocked)
      -- Cast the Murder, resolve it (the Kami dies), settle so the death trigger
      -- is gathered and its target chosen (CR 603.3d), then resolve the trigger.
      murderIt :: (forall r. Prompt.Prompt r -> r) -> (GameState.GameState, ObjectId.ObjectId) -> (GameState.GameState, GameState.GameState)
      murderIt answer (gs, spellId) =
        let cast = S.runPure answer gs (S.cast S.alice spellId)
            destroyed = S.runPure answer cast Stack.resolveTop
            settled = S.runPure answer destroyed Engine.settleForPriority
         in (settled, S.runPure answer settled Stack.resolveTop)
      graveyardOf = Game.zoneMembers Zone.Graveyard
   in Spec.describe s "Soulshift" $ do
        -- The proving case.
        Spec.it s "CR 702.46a whole card: the dead Kami returns the one Spirit card its N reaches" $ do
          (kamiId, (pikerId, shimatsuId, theirsId, mineId), gs) <- board
          let (settled, after) = murderIt exercising gs
          Spec.assertEqWith s "the dies trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertBool s (not (S.onBattlefield kamiId after)) "and the Kami is gone"
          Spec.assertEqWith s "alice's hand holds the Ancestor" (S.countByName ancestorName S.alice after) 1
          Spec.assertBool s (notElem mineId (graveyardOf S.alice after)) "the id it was targeted under has left her graveyard (CR 400.7)"
          Spec.assertBool s (elem pikerId (graveyardOf S.alice after)) "the Piker stayed: not a Spirit"
          Spec.assertBool s (elem shimatsuId (graveyardOf S.alice after)) "Shimatsu stayed: mana value 4 against soulshift 3"
          Spec.assertBool s (elem theirsId (graveyardOf S.bob after)) "bob's identical Ancestor stayed: the wrong graveyard (CR 400.1)"
          Spec.assertEqWith s "so does the dead Kami itself, a Spirit of mana value 4, beside the spent Murder" (length (graveyardOf S.alice after)) 4
        -- CR 603.5's "may" is a real fork, and the control for the case above --
        -- same board, same Murder, and the trigger still reaches the stack.
        Spec.it s "CR 603.5 declining the may returns nothing" $ do
          (_, (_, _, _, mineId), gs) <- board
          let (settled, after) = murderIt S.identityAnswer gs
          Spec.assertEqWith s "the trigger reached the stack all the same" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "but alice's hand is empty" (S.countByName ancestorName S.alice after) 0
          Spec.assertBool s (elem mineId (graveyardOf S.alice after)) "and the Ancestor is where it was"
        -- CR 702.46b: "if a permanent has multiple instances of soulshift, each
        -- triggers separately". Asserted of the MINT, as afterlife's multiplicity
        -- is: Forked-Branch Garami prints "soulshift 4, soulshift 4" and is not in
        -- the pool.
        Spec.it s "CR 702.46b each instance of soulshift is its own ability" $ do
          Spec.assertEqWith s "soulshift 3 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Soulshift 3) 2)) [Keyword.soulshift 3, Keyword.soulshift 3]
          Spec.assertEqWith s "and soulshift 4 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Soulshift 4) 1)) [Keyword.soulshift 4]
          Spec.assertBool s (Keyword.soulshift 3 /= Keyword.soulshift 4) "and the N reaches the minted ability"

-- Radiant Fountain, a Land: "When this land enters, you gain 2 life. / {T}: Add
-- {C}." A nonbasic land whose whole text box is one triggered ability and one
-- activated one, which is what makes it the pool's witness for CR 305.7's
-- "It loses all abilities generated from its rules text" reaching a TRIGGER.
--
-- The entry is staged the way Pawl.TriggerSpec's other entry fixtures stage it:
-- the permanent is placed, its Moved event appended to the log directly, and the
-- scan run at the next settle. CR 603.6a checks every battlefield permanent against
-- the event, and it reads each one's PROJECTION -- so a Blood Moon that has already
-- made the Fountain a Mountain leaves nothing there to trigger. The appended event
-- carries no sampled board, so this is the live-fallback reading in
-- Event.eventTriggers rather than the per-group one; the Blood Moon was standing
-- before the entry either way, so the two agree here.
strippedTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
strippedTriggerSpec s registry =
  let settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      entering oid gs =
        let moved = ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield
         in resolveAll (settle (S.withEvents [GameEvent.Moved (Moved.moved moved (Projection.project oid gs))] gs))
   in Spec.describe s "CR 305.7 strips a triggered ability" $ do
        Spec.it s "CR 603.6a Radiant Fountain's entry trigger gains its controller 2 life" $ do
          radiantFountain <- S.printingOf s registry "Radiant Fountain"
          let (fountainId, gs) = S.addCreature radiantFountain S.alice (Setup.emptyGame S.bothPlayers)
              after = entering fountainId gs
          Spec.assertEqWith s "20 + 2" (S.lifeOf S.alice after) (Just 22)
          Spec.assertBool s (ManaType.Colorless `elem` Mana.manaTypesOf fountainId after) "and it taps for colorless"
        Spec.it s "CR 305.7 under Blood Moon the same entry triggers nothing" $ do
          radiantFountain <- S.printingOf s registry "Radiant Fountain"
          bloodMoon <- S.printingOf s registry "Blood Moon"
          let (_, withMoon) = S.addCreature bloodMoon S.alice (Setup.emptyGame S.bothPlayers)
              (fountainId, gs) = S.addCreature radiantFountain S.alice withMoon
              after = entering fountainId gs
          Spec.assertBool s (Set.member Subtype.Mountain (Projection.subtypesOf fountainId after)) "it entered as a Mountain"
          Spec.assertEqWith s "nothing reached the stack" (GameState.stack after) []
          Spec.assertEqWith s "and no life was gained" (S.lifeOf S.alice after) (Just 20)
          -- CR 305.7's last clause, on the same board: the printed mana ability
          -- goes and the new basic land type's replaces it.
          Spec.assertBool s (ManaType.Colored Color.Red `elem` Mana.manaTypesOf fountainId after) "red instead"
          Spec.assertBool s (ManaType.Colorless `notElem` Mana.manaTypesOf fountainId after) "colorless gone"

-- CR 702.19b: assign each blocker exactly its lethal threshold and let the
-- excess trample through to the defending player. S.aggressiveAnswer cannot be
-- used here -- its AssignCombatDamage arm dumps the whole amount onto the first
-- CREATURE recipient it finds, so nothing would ever reach a player and the
-- trigger under test would never have an event to match.
tramplingAnswer :: Prompt.Prompt r -> r
tramplingAnswer p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    let blockerEntries = Map.toList (Map.filterWithKey (\r _ -> S.isCreatureRecipient r) thresholds)
        spent = sum (fmap snd blockerEntries)
        leftover = if n >= spent then n - spent else 0
        toBlockers = Map.fromList blockerEntries
     in case filter (not . S.isCreatureRecipient) (Map.keys thresholds) of
          d : _ -> Map.insert d leftover toBlockers
          [] -> toBlockers
  _ -> S.aggressiveAnswer p

-- CR 603.10's FIRST sentence for a BYSTANDER: "objects that exist immediately
-- after an event are checked to see if the event matched any trigger
-- conditions". The scan runs once, at the CR 117.5 boundary, after CR 704.3's
-- state-based actions -- so a permanent that was on the battlefield when some
-- OTHER event in the same batch happened, and is gone by the time the scan
-- looks, has to be recovered from CR 608.2h last known information exactly as
-- CR 603.10a's look-back already recovers a departure event's own permanent.
--
-- Lightning Skelemental is the card: {B}{R}{R} Creature -- Elemental Skeleton
-- 6/1, "Trample, haste / Whenever this creature deals combat damage to a
-- player, that player discards two cards. / At the beginning of the end step,
-- sacrifice this creature." Its 1 toughness and CR 702.19b's trample are what
-- put the trigger's event and the bearer's death in ONE batch: the excess
-- reaches bob while the blocker's damage kills the Skelemental at the very next
-- CR 704.5g check, before any player gets priority.
bystanderSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bystanderSpec s registry =
  Spec.describe s "Bystander" $ do
    -- Which side of CR 603.10 each condition falls on, asserted directly.
    -- Event.looksBack is a TOTAL case, so -Werror already forces a new condition
    -- to be classified; what it cannot force is the classification being the
    -- RIGHT one, and the four arms below are the ones a plausible wrong reading
    -- would flip. Each is the rule read against the constructor's own printed
    -- sentence:
    --
    --   * CR 603.10a names leaves-the-battlefield abilities, so "dies" is one
    --     (CR 700.4 narrows the destination, which does not leave the family);
    --   * and it names sacrifice triggers in as many words;
    --   * CR 603.6c's own last sentence puts "put into a graveyard from
    --     anywhere" OUTSIDE that family, which is the arm a wildcard would have
    --     gotten wrong in the expensive direction;
    --   * CR 708.8 leaves a permanent turned face up ON the battlefield, so
    --     there is no departure for a look-back to recover.
    Spec.it s "CR 603.10a the look-back families are the ones that rule lists" $ do
      Spec.assertBool s (Event.looksBack (TriggerCondition.PermanentDies (Filter.Type.And []))) "a dies trigger is a leaves-the-battlefield ability"
      Spec.assertBool s (Event.looksBack TriggerCondition.SelfLeavesTheBattlefield) "and so is the wider written form"
      Spec.assertBool s (Event.looksBack (TriggerCondition.PermanentSacrificed (PermanentSacrificed.MkPermanentSacrificed PlayerRelation.AnyPlayer (Filter.Type.And [])))) "a sacrifice trigger is named outright"
      Spec.assertBool s (not (Event.looksBack TriggerCondition.SelfPutIntoGraveyardFromAnywhere)) "CR 603.6c says put-into-a-graveyard-from-anywhere is not one"
      Spec.assertBool s (not (Event.looksBack (TriggerCondition.PermanentTurnedFaceUp (Filter.Type.And [])))) "and CR 708.8 leaves a turned-up permanent on the battlefield"
      -- CR 603.1b: one ability, several conditions -- it looks back if any of
      -- them does, and the pair below differ only in whether one does.
      Spec.assertBool s (Event.looksBack (TriggerCondition.AnyOf [TriggerCondition.SelfEnters, TriggerCondition.SelfDies])) "an AnyOf containing one looks back"
      Spec.assertBool s (not (Event.looksBack (TriggerCondition.AnyOf [TriggerCondition.SelfEnters, TriggerCondition.SelfTurnedFaceUp]))) "and one containing none does not"
    -- The proving test. bob holds THREE cards, so "discarded once" (one left)
    -- is distinguishable from "discarded twice" (none) and from "not at all"
    -- (three).
    Spec.it s "CR 603.10 whole cards: Lightning Skelemental dies to its blocker and STILL makes bob discard two" $ do
      skelemental <- S.printingOf s registry "Lightning Skelemental"
      piker <- S.printingOf s registry "Goblin Piker"
      case S.combatBoardOf [skelemental] [piker] of
        (base, [attacker], [blocker]) -> do
          let gs = List.foldl' (\g _ -> snd (S.addHandCard piker S.bob g)) base [(), (), ()]
              after = S.runCombat tramplingAnswer gs
          Spec.assertEqWith s "bob starts with three cards" (S.handSize S.bob gs) 3
          Spec.assertEqWith s "CR 702.19b: five trampled through to bob" (S.lifeOf S.bob after) (Just 15)
          Spec.assertBool s (not (S.onBattlefield attacker after)) "CR 704.5g: the Piker's two killed the 6/1"
          Spec.assertBool s (not (S.onBattlefield blocker after)) "and the Piker died to its one"
          Spec.assertEqWith s "the Skelemental is in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
          Spec.assertEqWith s "and bob discarded two, exactly once" (S.handSize S.bob after) 1
        _ -> Spec.assertFailure s "fixture should give alice one attacker and bob one blocker"
    -- The control leg, which passes with or without the bystander recovery:
    -- unblocked, the Skelemental is still on the battlefield at the boundary,
    -- so `onBattlefield` carries it and the same trigger fires from the
    -- ordinary candidate source. It is what makes the card data and the
    -- reserved "that player" slot innocent when the leg above fails.
    Spec.it s "CR 510.1b control: an UNBLOCKED Skelemental survives and makes bob discard two the ordinary way" $ do
      skelemental <- S.printingOf s registry "Lightning Skelemental"
      piker <- S.printingOf s registry "Goblin Piker"
      case S.combatBoardOf [skelemental] [] of
        (base, [attacker], []) -> do
          let gs = List.foldl' (\g _ -> snd (S.addHandCard piker S.bob g)) base [(), (), ()]
              after = S.runCombat tramplingAnswer gs
          Spec.assertBool s (S.onBattlefield attacker after) "the Skelemental is still on the battlefield"
          Spec.assertEqWith s "bob took all six" (S.lifeOf S.bob after) (Just 14)
          Spec.assertEqWith s "and discarded two" (S.handSize S.bob after) 1
        _ -> Spec.assertFailure s "fixture should give alice one attacker and bob no blockers"
    -- The OTHER shape the rule reaches, at the gather rather than through a
    -- whole turn: a CR 603.2b step trigger whose bearer is gone by the
    -- boundary. Khabál Ghoul ("At the beginning of each end step, put a +1/+1
    -- counter on Khabál Ghoul for each creature that died this turn") is the
    -- bearer; the end step's beginning and the Ghoul's own death are two
    -- events in one unscanned batch, and the step event comes FIRST, so
    -- nothing about the Ghoul's own departure event can be what recovers it.
    Spec.it s "CR 603.10 a StepBegins bearer that dies later in the same batch still triggers" $ do
      khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
      let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
          began = S.withEvents [GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) S.alice)] gs0
          dead = S.runPure S.identityAnswer began (Event.destroy Regenerability.Regenerable [ghoul])
          triggers = gathered dead
      Spec.assertEqWith s "the Ghoul really did leave the battlefield" (Game.lookupObject ghoul dead) Nothing
      Spec.assertEqWith s "its step trigger still fired" (fmap PendingTrigger.source triggers) [TriggerSource.OfObject ghoul]
      Spec.assertEqWith s "under alice, who controlled it as it left (CR 603.3a)" (fmap PendingTrigger.controller triggers) [S.alice]
    -- The discriminating twin: a bearer that left the battlefield BEFORE the
    -- step began did not exist immediately after that event, and gets nothing.
    -- Same board, same two events, opposite order.
    Spec.it s "CR 603.10 a bearer that had already left before the event does NOT trigger" $ do
      khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
      let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
          dead = S.runPure S.identityAnswer gs0 (Event.destroy Regenerability.Regenerable [ghoul])
          began = S.runPure S.identityAnswer dead (State.modify' (Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Ending EndingStep.EndStep) S.alice))))
          triggers = gathered began
      Spec.assertEqWith s "the Ghoul is gone" (Game.lookupObject ghoul began) Nothing
      Spec.assertEqWith s "and nothing triggered" (fmap PendingTrigger.source triggers) []

-- CR 113.6m read off a BYSTANDER: the half of CR 603.10's first sentence
-- `bystanderSpec` above recovers, asked of a permanent whose ability functions
-- only in a graveyard.
--
-- The rule the recovery is not allowed to lose: "an ability whose cost or effect
-- specifies that it moves the object it's on out of a particular zone functions
-- only in that zone". A bystander is recovered from CR 608.2h last known
-- information, but what it is recovered AS is a permanent that was ON THE
-- BATTLEFIELD when the event happened -- so one of its abilities that functions
-- only in a graveyard was no more watching then than it would be now.
--
-- CR 603.10a is deliberately NOT this case. There the rule's own "unless its
-- trigger condition ... specifies that the object is put into that zone" arm
-- decides -- `Pawl.Engine.Event.conditionPutsSelfInto`, read by
-- `Pawl.Engine.Event.leftBattlefield`'s own filter -- and the Aura half beside
-- it is read, in `screamsFromWithinSpec` below; a bystander carries any
-- condition at all, so nothing about either reaches here.
--
-- The pair, chosen so that ONE derivation is the only difference between them:
--
--   * Squee, Goblin Nabob ({2}{R} Legendary Creature -- Goblin 1/1, "At the
--     beginning of your upkeep, you may return this card from your graveyard to
--     your hand"). CR 113.6k cannot reach it -- an upkeep condition triggers
--     perfectly well from the battlefield -- so only the effect's own words say
--     graveyard.
--   * Bitterblossom ({1}{B} Kindred Enchantment -- Faerie, "At the beginning of
--     your upkeep, create a 1/1 black Faerie Rogue creature token with flying and
--     you lose 1 life") as the control: the SAME trigger condition, on the same
--     battlefield, leaving in the same batch, with an effect that names no zone.
--     CR 113.6's default keeps it functioning on the battlefield.
--
-- (Both names, costs, type lines, P/T and oracle texts checked against Scryfall.)
--
-- Both leave the battlefield AFTER the upkeep begins and inside one unscanned
-- batch, which is `bystanderSpec`'s Khabál Ghoul shape: the step event comes
-- first, so nothing about either permanent's own departure event can be what
-- offers it.
bystanderZoneSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bystanderZoneSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      squeeName = CardName.MkCardName (Text.pack "Squee, Goblin Nabob")
      faerieToken = CardName.MkCardName (Text.pack "Faerie Rogue Token")
      -- Takes every "you may", so a trigger that fires is observable as the card
      -- it moved rather than as a prompt count.
      exercising p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      -- alice's upkeep begins with Squee and Bitterblossom on her battlefield;
      -- `remove` then takes both off inside the same batch. Answers with the two
      -- battlefield ids and the sources the gather produced.
      board remove = do
        squee <- S.printingOf s registry "Squee, Goblin Nabob"
        bitterblossom <- S.printingOf s registry "Bitterblossom"
        let (squeeId, g1) = S.addCreature squee S.alice (Setup.emptyGame S.bothPlayers)
            (blossomId, g2) = S.addCreature bitterblossom S.alice g1
            began =
              S.withEvents
                [GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice)]
                (g2 {GameState.phase = upkeep, GameState.activePlayer = S.alice})
            after = S.runPure S.identityAnswer began (remove [squeeId, blossomId])
        pure (squeeId, blossomId, after, fmap PendingTrigger.source (gathered after))
   in Spec.describe s "BystanderZone" $ do
        -- The proving leg. EXILE rather than a graveyard on purpose: it leaves
        -- the bystander reading as the only source that could offer Squee's
        -- ability at all, so the assertion cannot pass on the strength of the
        -- graveyard filtering that already landed with CR 113.6m's trigger half.
        Spec.it s "CR 113.6m a bystander's graveyard-functioning trigger does not fire from the battlefield it just left" $ do
          (squeeId, blossomId, after, sources) <- board (mapM_ (\oid -> Event.changeZone oid Zone.Exile))
          Spec.assertEqWith s "Squee really left the battlefield" (Game.lookupObject squeeId after) Nothing
          Spec.assertEqWith s "and so did the Bitterblossom" (Game.lookupObject blossomId after) Nothing
          Spec.assertBool s (null (Game.zoneMembers Zone.Graveyard S.alice after)) "neither card is in a graveyard, so no graveyard reading can be doing this"
          Spec.assertEqWith
            s
            "only the Bitterblossom, whose effect names no zone, is recovered as a bystander"
            sources
            [TriggerSource.OfObject blossomId]
        -- The same board with the ordinary destination. The battlefield
        -- incarnation still gets nothing, which is what this change is; the
        -- graveyard incarnation CR 400.7 mints is a different object under a
        -- different id, and CR 603.10's first sentence is what withholds THAT one
        -- from an event that predates its arrival -- the leg below is where that
        -- is proved, so this one still reads only the battlefield id.
        Spec.it s "CR 113.6m the same holds when the bystander dies to a graveyard" $ do
          (squeeId, blossomId, after, sources) <- board (Event.destroy Regenerability.Regenerable)
          Spec.assertEqWith s "Squee really left the battlefield" (Game.lookupObject squeeId after) Nothing
          Spec.assertBool s (TriggerSource.OfObject squeeId `notElem` sources) "the battlefield incarnation triggered nothing"
          Spec.assertBool s (TriggerSource.OfObject blossomId `elem` sources) "and the control still did"
        -- The ARRIVAL side of the same rule, driven through the turn machinery
        -- rather than through a hand-built log. CR 603.2b's step event is
        -- recorded as alice's upkeep begins, and only then does CR 704.3's check
        -- bury a Squee that CR 122.1a's -1/-1 counter had already taken to zero
        -- toughness (CR 704.5f) -- a strictly later event group of the same
        -- CR 117.5 batch. So the graveyard incarnation CR 400.7 minted did not
        -- exist immediately after the step began, and CR 603.10's first sentence
        -- checks an event against the objects that did: its CR 113.6m upkeep
        -- ability is no witness to an upkeep that had already begun.
        Spec.it s "CR 603.10 a card that reaches a graveyard mid-batch is no witness to an earlier event" $ do
          squee <- S.printingOf s registry "Squee, Goblin Nabob"
          bitterblossom <- S.printingOf s registry "Bitterblossom"
          let (squeeId, g1) = S.addCreature squee S.alice (Setup.emptyGame S.bothPlayers)
              (_, g2) = S.addCreature bitterblossom S.alice g1
              doomed = S.addCounter CounterKind.MinusOneMinusOne 1 squeeId g2
              atUpkeep =
                doomed
                  { GameState.phase = upkeep,
                    GameState.activePlayer = S.alice,
                    GameState.priority = Just S.alice,
                    GameState.remaining = Seq.drop 1 (GameState.remaining doomed)
                  }
              after = S.runPure exercising atUpkeep Engine.runStep
              squeesIn zone = filter (\oid -> fmap Face.name (Game.faceOf oid after) == Just squeeName) (Game.zoneMembers zone S.alice after)
          -- The discriminating assertion, and gameplay-level: the trigger that
          -- must not fire is the only thing that could move this card, and where
          -- the card ends up is what a player would see. `exercising` takes the
          -- "you may", so a fired trigger is a Squee in the hand.
          Spec.assertEqWith s "no Squee reached alice's hand" (length (squeesIn Zone.Hand)) 0
          -- The board really is the one the claim is about: a Squee IS in the
          -- graveyard at the boundary, under the fresh id CR 400.7 minted, so a
          -- live read of that zone would have offered it.
          Spec.assertEqWith s "and one Squee is in the graveyard" (length (squeesIn Zone.Graveyard)) 1
          Spec.assertBool s (squeeId `notElem` squeesIn Zone.Graveyard) "under a fresh id, not the battlefield one"
          -- The control, and the falsifier for over-narrowing: Bitterblossom's
          -- own upkeep trigger saw the same step event and fired, so "no Squee
          -- trigger" is not what a silenced batch looks like.
          Spec.assertEqWith s "the Bitterblossom's upkeep trigger still fired" (S.countOnBattlefieldByName faerieToken S.alice after) 1

-- CR 400.7e's slot read from the OTHER direction of a zone change: an entry.
-- "Abilities that trigger when an object moves from one zone to another ... can
-- find the new object that it became in the zone it moved to when the ability
-- triggered, if that zone is a public zone" -- and CR 400.2 lists the
-- battlefield among the public zones, so an enters trigger's payload may name
-- the entrant with no proviso to check.
--
-- Aether Flash, {2}{R}{R} Enchantment, "Whenever a creature enters, this
-- enchantment deals 2 damage to it." Soul Warden proved the CONDITION
-- (Pawl.TriggerSpec's permanentEntersSpec); its "you gain 1 life" names nothing
-- about the creature that entered. This is the first card whose EFFECT refers back to the
-- entrant.
--
-- The contrast with becameSlotSpec is the point of reusing one slot name.
-- There the bearer and the entrant are two incarnations of ONE card, and
-- `became` is the second of them; here the bearer is the enchantment and the
-- entrant is a different card entirely. CR 400.7e distinguishes neither
-- situation: it names "the new object that IT became", where "it" is whatever
-- moved, and the moved object being the bearer is a fact about the condition
-- rather than about the slot.
--
-- Goblin Piker ({1}{R} Creature -- Goblin Warrior 2/1) and Ogre Sentry ({1}{R}
-- Creature -- Ogre Warrior 3/3, defender) are the pair: identical costs and
-- colors, so two Mountains cast either, and the ONLY difference the test can be
-- reading is the toughness the 2 damage is measured against.
aetherFlashSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
aetherFlashSpec s registry =
  let -- alice: Aether Flash already on the battlefield and two Mountains, with
      -- one creature card in hand. Casting it is the only thing on offer, so
      -- S.identityAnswer needs no bespoke interpreter.
      flashBoard creature = do
        mountain <- S.printingOf s registry "Mountain"
        aetherFlash <- S.printingOf s registry "Aether Flash"
        entrant <- S.printingOf s registry creature
        let (flashId, withFlash) = S.addCreature aetherFlash S.alice (S.landsInPlay mountain 2)
        pure (flashId, S.handOne entrant withFlash)
      castIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
         in S.runPure S.identityAnswer cast Engine.priorityLoop
      namesIn zone pid gs =
        fmap Face.name (Maybe.mapMaybe (\oid -> Game.faceOf oid gs) (Game.zoneMembers zone pid gs))
      damageEventsIn gs = Maybe.mapMaybe Event.damageOf (S.eventsOf gs)
      -- CR 120.3e's marked damage on the one battlefield permanent with this
      -- name. Nothing if it is not there, or if there is more than one of it.
      markedOn name gs =
        case filter (\oid -> fmap Face.name (Game.faceOf oid gs) == Just name) (Set.toList (GameState.battlefield gs)) of
          [oid] -> fmap Object.damage (Game.lookupObject oid gs)
          _ -> Nothing
      pikerName = CardName.MkCardName $ Text.pack "Goblin Piker"
      sentryName = CardName.MkCardName $ Text.pack "Ogre Sentry"
   in Spec.describe s "CR 400.7e the entrant an enters trigger names" $ do
        -- The gameplay-level proof, cast to resolution. The discriminating
        -- assertion is the GRAVEYARD: an ability whose `became` slot went
        -- unbound would resolve, find nothing under it and silently deal no
        -- damage, leaving a live 2/1 on the battlefield.
        Spec.it s "CR 603.6a whole card: a Goblin Piker enters and Aether Flash's 2 damage kills it (CR 704.5g)" $ do
          (flashId, board) <- flashBoard "Goblin Piker"
          let after = castIt board
          Spec.assertEqWith s "the Piker is not on the battlefield" (S.countOnBattlefieldByName pikerName S.alice after) 0
          Spec.assertEqWith s "it is in the graveyard, once" (namesIn Zone.Graveyard S.alice after) [pikerName]
          -- Falsifiers. The damage went to the creature, not to a player
          -- (CR 120.1a admits only battles, creatures and planeswalkers), and
          -- Aether Flash did not damage itself into the graveyard either.
          Spec.assertEqWith s "alice's life is untouched" (S.lifeOf S.alice after) (Just 20)
          Spec.assertEqWith s "bob's too" (S.lifeOf S.bob after) (Just 20)
          Spec.assertBool s (Set.member flashId (GameState.battlefield after)) "and the enchantment is still on the battlefield"
          Spec.assertEqWith s "exactly one damage event, of 2" (fmap DamageEvent.amount (damageEventsIn after)) [2]
        -- The control, differing only in the entrant's toughness: 2 damage
        -- marked on a 3/3 is not lethal (CR 704.5g compares the total marked
        -- against toughness), so the creature stays and CARRIES the mark. The
        -- marked damage is what proves the effect landed at all -- without it
        -- "still on the battlefield" would also be what a no-op looks like.
        Spec.it s "CR 704.5g the control: an Ogre Sentry survives the same 2 damage, marked" $ do
          (_, board) <- flashBoard "Ogre Sentry"
          let after = castIt board
          Spec.assertEqWith s "the Sentry is on the battlefield" (S.countOnBattlefieldByName sentryName S.alice after) 1
          Spec.assertEqWith s "the graveyard is empty" (namesIn Zone.Graveyard S.alice after) []
          Spec.assertEqWith s "with 2 damage marked on it" (markedOn sentryName after) (Just 2)
        -- eventBindings in isolation, so the binding is pinned to CR 400.7e
        -- rather than to Aether Flash's payload. The entrant is
        -- ZoneChange.object -- for an ENTRY the arriving incarnation is what
        -- the event is about, so `departed` would be the pre-move id of a card
        -- that is not on the battlefield at all.
        Spec.it s "CR 400.7e eventBindings binds the ENTRANT under became" $ do
          let castCard = ObjectId.MkObjectId 1
              entered = ObjectId.MkObjectId 2
              entry = GameEvent.Moved (Moved.moved (ZoneChange.MkZoneChange castCard entered Zone.Stack Zone.Battlefield) S.emptyCharacteristics)
          Spec.assertEqWith s "became names the permanent that entered" (Event.eventBindings (Setup.emptyGame S.bothPlayers) Nothing S.alice (TriggerCondition.PermanentEnters (Filter.Type.HasCardType CardType.Creature)) entry) (Map.singleton Binding.became (Binding.toObject entered))
        -- CR 603.6a's "EACH TIME an event puts one or more permanents onto
        -- the battlefield" met with a per-entrant payload: Dragon Fodder
        -- ({1}{R} Sorcery, "create two 1/1 red Goblin creature tokens") makes
        -- two entrants in one event, so one Aether Flash places two triggers
        -- and each has to name ITS OWN. Both tokens dying is what says so --
        -- two triggers sharing one binding would kill one token twice and
        -- leave the other standing.
        --
        -- A token is also the one entrant whose Moved event is
        -- battlefield-to-battlefield (Event.recordTokenEntry's pseudo-move,
        -- where `departed` and `object` are the same fresh id), so this is the
        -- shape where reading either field would look identical. It is here as
        -- the reminder that the fields agree for a token and only for a token.
        -- CR 704.5d then removes the dead tokens from the graveyard, so the
        -- damage's proof is the DamageDealt log, not a graveyard census.
        Spec.it s "CR 603.6a two tokens enter together and each trigger names its own" $ do
          mountain <- S.printingOf s registry "Mountain"
          aetherFlash <- S.printingOf s registry "Aether Flash"
          dragonFodder <- S.printingOf s registry "Dragon Fodder"
          let (_, withFlash) = S.addCreature aetherFlash S.alice (S.landsInPlay mountain 2)
              (gs, spellId) = S.handOne dragonFodder withFlash
              after = castIt (gs, spellId)
          Spec.assertEqWith s "no Goblin token survived" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Token") S.alice after) 0
          Spec.assertEqWith s "two damage events of 2, one per token" (fmap DamageEvent.amount (damageEventsIn after)) [2, 2]
          Spec.assertEqWith s "and they were dealt to two different objects" (Set.size (Set.fromList (fmap DamageEvent.target (damageEventsIn after)))) 2
        -- CR 608.2h, the case Aether Flash makes reachable with no second
        -- card: "if the effect requires information from a specific object
        -- ... the effect uses the current information of that object if it's
        -- in the public zone it was expected to be in". Two Aether Flashes,
        -- one 2/1 entrant, two triggers -- and the first one's damage kills it
        -- at the next state-based-action check, so the second resolves with
        -- its entrant already gone from the battlefield it was expected to be
        -- on. CR 400.7 minted a fresh id for the graveyard card, so the effect
        -- does not follow it there.
        Spec.it s "CR 608.2h a second Aether Flash resolves with the entrant already dead, and deals nothing" $ do
          mountain <- S.printingOf s registry "Mountain"
          aetherFlash <- S.printingOf s registry "Aether Flash"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, oneFlash) = S.addCreature aetherFlash S.alice (S.landsInPlay mountain 2)
              (_, twoFlashes) = S.addCreature aetherFlash S.alice oneFlash
              (gs, spellId) = S.handOne piker twoFlashes
              cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
              -- The creature spell resolves and enters; the settle's CR 117.5
              -- scan places both triggers.
              entered = S.runPure S.identityAnswer cast Stack.resolveTop
              placed = S.runPure S.identityAnswer entered Engine.settleForPriority
              -- One trigger resolves for 2, and the settle after it is where
              -- CR 704.5g destroys the 2/1.
              hit = S.runPure S.identityAnswer placed Stack.resolveTop
              buried = S.runPure S.identityAnswer hit Engine.settleForPriority
              -- The second trigger, resolving against an id CR 400.7 deleted.
              after = S.runPure S.identityAnswer buried Stack.resolveTop
          Spec.assertEqWith s "both triggers were placed" (length (GameState.stack placed)) 2
          Spec.assertEqWith s "the Piker is dead before the second resolves" (S.countOnBattlefieldByName pikerName S.alice buried) 0
          Spec.assertEqWith s "one damage event so far" (fmap DamageEvent.amount (damageEventsIn buried)) [2]
          Spec.assertEqWith s "the second trigger did resolve" (GameState.stack after) []
          Spec.assertEqWith s "and dealt nothing: still one damage event" (fmap DamageEvent.amount (damageEventsIn after)) [2]
          Spec.assertEqWith s "the card is in the graveyard once, not twice" (namesIn Zone.Graveyard S.alice after) [pikerName]

-- Bitterblossom {1}{B} Kindred Enchantment -- Faerie: "At the beginning of your
-- upkeep, you lose 1 life and create a 1/1 black Faerie Rogue creature token
-- with flying." The pool's first KINDRED card (CR 308), and so the first object
-- of any kind that carries a creature type without being a creature.
kindredSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
kindredSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      beginUpkeep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice)) (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      -- Bitterblossom on alice's battlefield, alice's upkeep begun. The middle
      -- state is the one where the trigger is on the stack (CR 603.3b); the last
      -- is after it has resolved, so a Faerie Rogue token stands beside it.
      board bitterblossom =
        let (oid, gs) = S.addCreature bitterblossom S.alice (Setup.emptyGame S.bothPlayers)
            onStack = settle (beginUpkeep gs)
         in (oid, onStack, resolveAll onStack)
      slot = SlotName.MkSlotName (Text.pack "target")
      -- "Target Faerie ...", drawn from a pool: the same Pool + Filter machinery
      -- every printed target slot goes through (Pawl.ResolveSpec's "target
      -- Wall" is the same shape over a creature type that behaves ordinarily).
      faeriesIn pool gs =
        Map.findWithDefault
          Set.empty
          slot
          (Target.legalSets (Just S.alice) False Map.empty S.noSource (Map.singleton slot (TargetSlot.required pool (Just (Filter.Type.HasSubtype Subtype.Faerie)))) gs)
   in Spec.describe s "Kindred" $ do
        -- The proving test for CR 308. CR 308.2 makes the kindred subtypes
        -- "the same as the set of creature subtypes", so the ENCHANTMENT
        -- answers a creature-type filter -- and CR 110.4's six permanent types
        -- do not include kindred, so it still answers no to being a creature.
        -- The token it makes is the control: a real Faerie creature, in both
        -- pools, which is what keeps the enchantment's absence from the
        -- creature pool from being a fixture that simply produced nothing.
        Spec.it s "CR 308.2 a Kindred Enchantment is a legal \"target Faerie permanent\", and CR 110.4 keeps it out of \"target Faerie creature\"" $ do
          bitterblossom <- S.printingOf s registry "Bitterblossom"
          let (blossomId, _, after) = board bitterblossom
              permanents = faeriesIn Pool.Permanents after
              creatures = faeriesIn Pool.Creatures after
          Spec.assertBool s (Set.member Subtype.Faerie (Projection.subtypesOf blossomId after)) "the enchantment carries the creature type Faerie"
          Spec.assertBool s (not (Projection.isCreatureOf blossomId after)) "and is not a creature"
          Spec.assertBool s (Set.member (Recipient.ToObject blossomId) permanents) "so \"target Faerie permanent\" offers it"
          Spec.assertEqWith s "alongside the token it made, and nothing else" (Set.size permanents) 2
          Spec.assertBool s (not (Set.member (Recipient.ToCreature blossomId) creatures)) "\"target Faerie creature\" does not"
          Spec.assertEqWith s "the token is the only one of those" (Set.size creatures) 1
        -- CR 308.1: "casting and resolving a kindred card follows the rules for
        -- ... the other card type", and here the other type is Enchantment, so
        -- nothing about the trigger is kindred-specific. What this pins is that
        -- the whole printed ability runs -- CR 603.3a's "your upkeep" (the
        -- ability controller's, CR 109.5), the life payment, and CR 111.1's
        -- token -- through the ordinary priority loop.
        Spec.it s "CR 603.3a Bitterblossom's upkeep trigger costs its controller 1 life and mints a 1/1 flying black Faerie Rogue" $ do
          bitterblossom <- S.printingOf s registry "Bitterblossom"
          let (blossomId, onStack, after) = board bitterblossom
          Spec.assertBool s (not (null (GameState.stack onStack))) "the upkeep trigger really reached the stack"
          Spec.assertEqWith s "no life was lost before it resolved" (S.lifeOf S.alice onStack) (Just 20)
          Spec.assertEqWith s "alice paid the 1 life" (S.lifeOf S.alice after) (Just 19)
          case filter (/= blossomId) (Set.toList (GameState.battlefield after)) of
            [token] -> do
              Spec.assertEqWith s "1/1" (Projection.powerOf token after, Projection.toughnessOf token after) (Just 1, Just 1)
              -- CR 202.2b/202.2e: a token has no mana cost, so the colour
              -- indicator is the only thing making it black.
              Spec.assertEqWith s "black" (Projection.colorsOf token after) (Set.singleton Color.Black)
              Spec.assertEqWith s "Faerie Rogue" (Projection.subtypesOf token after) (Set.fromList [Subtype.Faerie, Subtype.Rogue])
              Spec.assertBool s (Map.member Keyword.Type.Flying (Projection.keywordsOf token after)) "with flying"
              Spec.assertBool s (Projection.isCreatureOf token after) "and it, unlike its maker, IS a creature"
            other -> Spec.assertFailure s ("expected exactly one token beside Bitterblossom, got " <> show (length other) <> " other permanents")

-- CR 113.6m's Aura clause, over a whole card. The rule pins an ability whose
-- effect moves its own object out of a zone to that zone, "unless its trigger
-- condition ... specifies that ... the object it enchants leaves the
-- battlefield" -- and Screams from Within ({1}{B}{B} Enchantment -- Aura,
-- "Enchant creature / Enchanted creature gets -1/-1. / When enchanted creature
-- dies, return this card from your graveyard to the battlefield") is the
-- printing that turns on it. (Name, cost, type line and oracle text checked
-- against Scryfall.)
--
-- The card would break under a reading that pinned it to the graveyard:
-- `Event.functionsIn Zone.Battlefield` would be False, `eventTriggers`'
-- `battlefieldAbilitiesOf` filter would drop the ability, and the Aura would sit
-- in the graveyard CR 704.5m put it in. With the clause `zoneFunctionedFrom`
-- answers Nothing, `zonesTriggeredFrom` gives the battlefield, and the trigger
-- goes on the stack.
--
-- The clause is not what CARRIES that here, though, and the legs below do not
-- prove it: this card's payload names Binding.became rather than the trigger
-- source, and Pawl.Engine.EffectZone.zoneFunctionedFrom reads a zone off no
-- other slot, so the fold answers Nothing whichever way the clause goes. The
-- clause is proved by `widowedBladeSpec` below instead, on the one card in the
-- pool whose payload names its own source.
--
-- CR 700.4 is what makes the printed "dies" one of the departures the clause
-- names; CR 603.10a is what lets the trigger see a host that has already left;
-- and CR 608.2h's Pawl.Types.LastKnown.attached -- the HOST's record of what was
-- attached to it -- is what lets the MATCH see the link, CR 704.5m having taken
-- the Aura off the battlefield in the same CR 117.5 batch.
--
-- AND CR 400.7f is what lets the PAYLOAD act. Its effect moves "this card" out
-- of the graveyard, and CR 113.7a's source slot carries the battlefield id CR
-- 400.7 has already replaced -- so the card names Binding.became instead, which
-- Event.eventBindings stamps with the incarnation the Aura's own burial minted.
-- Both sentences of the rule are exercised below: the CR 704.5m burial at a
-- later SBA pass in the first leg, and "at the same time the enchanted permanent
-- left the battlefield" -- one wrath, one EventGroup -- in the second.
--
-- WHEN THE BOARD IS READ. CR 704.5m fires AGAIN on the Aura the moment it
-- arrives attached to nothing, so a reading taken after the next SBA pass finds
-- it back in the graveyard whether the rule is implemented or not. The zone
-- assertions below are therefore taken between Stack.resolveTop and that pass,
-- and say so.
--
-- The board. alice: the enchanted Hill Giant, plus a second Hill Giant. bob: a
-- Goblin Piker. Hill Giants rather than Pikers on alice's side because the
-- Aura's own -1/-1 would take a 2/1 to zero toughness (CR 704.5f) and bury the
-- enchanted creature before the test acts. The census below is per NAME and by
-- COUNT rather than by presence, which is what tells the Aura's own arrival from
-- the other one this batch mints: an arm binding the HOST's graveyard
-- incarnation instead returns a second Hill Giant and no Aura.
screamsFromWithinSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
screamsFromWithinSpec s registry =
  let board = do
        screams <- S.printingOf s registry "Screams from Within"
        hillGiant <- S.printingOf s registry "Hill Giant"
        piker <- S.printingOf s registry "Goblin Piker"
        let (enchanted, g1) = S.addCreature hillGiant S.alice (Setup.emptyGame S.bothPlayers)
            (_, g2) = S.addCreature hillGiant S.alice g1
            (_, g3) = S.addCreature piker S.bob g2
            (aura, g4) = S.addCreature screams S.alice g3
        pure (enchanted, aura, screams, S.attach aura enchanted g4)
      -- Kill the enchanted creature, then run CR 117.5's own settle: the SBA
      -- pass buries the host and the now-unattached Aura, and the trigger is put
      -- on the stack after it.
      kill victim gs = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [victim] >> Engine.settleForPriority)
      -- Resolve the placed trigger and stop, WITHOUT settling again: CR 704.5m
      -- would bury the returned Aura on the next pass, and a board read after
      -- that cannot tell the fixed engine from the broken one.
      resolve gs = S.runPure S.identityAnswer gs Stack.resolveTop
      screamsName = CardName.MkCardName (Text.pack "Screams from Within")
      -- The battlefield as three counts, one per printing: the Aura back, the
      -- surviving Hill Giant, and bob's untouched Piker. Game.zoneMembers indexes
      -- the battlefield by OWNER (CR 108.3), which is the seat each was made
      -- under here.
      census gs =
        fmap
          (\(name, pid) -> S.countOnBattlefieldByName (CardName.MkCardName (Text.pack name)) pid gs)
          [("Screams from Within", S.alice), ("Hill Giant", S.alice), ("Goblin Piker", S.bob)]
      inGraveyard gs = length (filter (\oid -> fmap Face.name (Game.faceOf oid gs) == Just screamsName) (Game.zoneMembers Zone.Graveyard S.alice gs))
   in Spec.describe s "ScreamsFromWithin" $ do
        -- The proving leg. The discriminating quantity is what the stack holds:
        -- the Aura's trigger with the clause, nothing without it.
        Spec.it s "CR 113.6m an Aura whose trigger watches its host's death functions from the battlefield" $ do
          (enchanted, aura, screams, gs) <- board
          let after = kill enchanted gs
          Spec.assertEqWith
            s
            "CR 113.6m the Aura's death trigger is on the stack"
            (fmap (\oid -> fmap Object.source (Game.lookupObject oid after)) (GameState.stack after))
            (fmap (\ab -> Just (Source.OfTrigger TriggeredAbilitySource.MkTriggeredAbilitySource {TriggeredAbilitySource.source = aura, TriggeredAbilitySource.ability = ab, TriggeredAbilitySource.createdAt = Nothing})) (Face.triggeredAbilities (S.combinedFace screams)))
          -- The preconditions the assertion above rests on, AFTER it so neither
          -- can absorb a mutation aimed at the clause.
          Spec.assertEqWith s "the enchanted Hill Giant really died" (Game.lookupObject enchanted after) Nothing
          Spec.assertEqWith s "and CR 704.5m really took the Aura off the battlefield" (Game.lookupObject aura after) Nothing
          Spec.assertEqWith
            s
            "so the trigger was gathered off the HOST's CR 608.2h record of what was attached to it"
            (fmap LastKnown.attached (Map.lookup enchanted (GameState.lastKnown after)))
            (Just (Set.singleton aura))
        -- CR 603.10a's own case, and the leg that makes `looksBack`'s arm
        -- load-bearing: the Aura leaves the battlefield in the SAME event group
        -- as its host -- a wrath -- so the live board holds neither, and the
        -- trigger can only be gathered from `sameGroup`, which is narrowed to the
        -- look-back conditions. CR 400.7f's proviso names this case in as many
        -- words ("put into that graveyard at the same time the enchanted
        -- permanent left the battlefield").
        Spec.it s "CR 603.10a the trigger still fires when the Aura dies in the same batch as its host" $ do
          (enchanted, aura, screams, gs) <- board
          let after = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [enchanted, aura] >> Engine.settleForPriority)
          Spec.assertEqWith
            s
            "CR 603.10a the Aura's death trigger is still on the stack"
            (fmap (\oid -> fmap Object.source (Game.lookupObject oid after)) (GameState.stack after))
            (fmap (\ab -> Just (Source.OfTrigger TriggeredAbilitySource.MkTriggeredAbilitySource {TriggeredAbilitySource.source = aura, TriggeredAbilitySource.ability = ab, TriggeredAbilitySource.createdAt = Nothing})) (Face.triggeredAbilities (S.combinedFace screams)))
          Spec.assertEqWith s "and both really left the battlefield in one batch" (fmap (`Game.lookupObject` after) [enchanted, aura]) [Nothing, Nothing]
        -- CR 400.7f's second sentence, and the unit's own proving leg: the Aura
        -- reached the graveyard "as a result of being put there as a state-based
        -- action for not being attached to a permanent", at a LATER SBA pass than
        -- its host's death, and the payload finds it there.
        --
        -- The census runs FIRST so no precondition can absorb a mutation, and it
        -- is the quantity no partial fix reaches by another route: the effect
        -- moves an object out of a graveyard, and only the right id puts an Aura
        -- on the battlefield rather than a Hill Giant or nothing.
        Spec.it s "CR 400.7f the payload finds the Aura's graveyard incarnation and returns it" $ do
          (enchanted, aura, _, gs) <- board
          let placed = kill enchanted gs
              after = resolve placed
          Spec.assertEqWith s "CR 400.7f the Aura is back on the battlefield, and only the Aura came back" (census after) [1, 1, 1]
          -- CR 400.7: a THIRD incarnation, so the returned permanent is neither
          -- the id that stood on the battlefield nor the one that was in the
          -- graveyard. The preconditions follow it.
          Spec.assertEqWith s "under a new id, the battlefield one being gone" (Game.lookupObject aura after) Nothing
          Spec.assertEqWith s "and alice's graveyard no longer holds it" (inGraveyard after) 0
          Spec.assertEqWith s "the trigger really resolved" (length (GameState.stack after)) 0
          Spec.assertEqWith s "and the board before it resolved held no Aura, one in the graveyard" (census placed, inGraveyard placed) ([0, 1, 1], 1)
        -- The same rule's FIRST sentence, on the wrath board above: host and Aura
        -- reach their graveyards in one EventGroup, "at the same time the
        -- enchanted permanent left the battlefield", and the payload finds it
        -- there too. Same reading moment and same census as the leg above.
        Spec.it s "CR 400.7f the payload finds it when the Aura died in the same batch as its host" $ do
          (enchanted, aura, _, gs) <- board
          let placed = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [enchanted, aura] >> Engine.settleForPriority)
              after = resolve placed
          Spec.assertEqWith s "CR 400.7f the simultaneously buried Aura is back on the battlefield" (census after) [1, 1, 1]
          Spec.assertEqWith s "under a new id here too" (Game.lookupObject aura after) Nothing
        -- The over-rejection leg, and the reason the clause is a case on the
        -- CONDITION rather than an unconditional Nothing: Squee, Goblin Nabob's
        -- upkeep trigger says nothing about an enchanted object, so CR 113.6m's
        -- main sentence still pins it to the graveyard.
        Spec.it s "CR 113.6m control: an ordinary graveyard-recursion trigger is still pinned to the graveyard" $ do
          squee <- S.printingOf s registry "Squee, Goblin Nabob"
          screams <- S.printingOf s registry "Screams from Within"
          Spec.assertEqWith s "Squee's ability functions only in the graveyard" (fmap (Event.zoneFunctionedFrom (TypeLine.subtypes (Face.typeLine (S.combinedFace squee))) (Face.delayedAbilities (S.combinedFace squee))) (Face.triggeredAbilities (S.combinedFace squee))) [Just Zone.Graveyard]
          Spec.assertEqWith s "the Aura's names no zone at all" (fmap (Event.zoneFunctionedFrom (TypeLine.subtypes (Face.typeLine (S.combinedFace screams))) (Face.delayedAbilities (S.combinedFace screams))) (Face.triggeredAbilities (S.combinedFace screams))) [Nothing]

-- CR 113.6m's Aura clause read the other way round: the exception is granted
-- "if the object is an Aura", so the SAME trigger condition on a non-Aura gets
-- the rule's main sentence instead. CR 303.4m is what makes the pair possible --
-- "an ability of a permanent that refers to the enchanted [object] refers to
-- whatever object that permanent is attached to, even if the permanent with the
-- ability isn't an Aura" -- so an Equipment's "equipped creature dies" is the
-- same attachment link and the same TriggerCondition.AttachedCreatureDies.
--
-- Synthetic Widowed Blade ({2} Artifact -- Equipment, "Equipped creature gets
-- +2/+0. / Whenever equipped creature dies, return Synthetic Widowed Blade from
-- your graveyard to the battlefield. / Equip {2}",
-- data/cards/synthetic-widowed-blade.json) is the producer. Synthetic because no
-- printing discriminates the two readings: Scryfall `o:/equipped creature dies/
-- -t:aura` (2026-09-02) returns the Equipment family -- Eater of Virtue, Sword
-- of the Realms, Resurrection Orb, Oathkeeper, Forebear's Blade and the rest --
-- and every one of them moves the equipped CREATURE or re-attaches itself, never
-- moving the Equipment out of a zone, which is what CR 113.6m is about. An
-- Equipment printed with a graveyard-recursion effect on this condition is the
-- card that would replace this one.
--
-- The move names the TRIGGER SOURCE slot rather than Screams from Within's
-- `became`, and that is the modelling the rule forces rather than a difference
-- of convenience: the Equipment is pinned to the graveyard, so the object its
-- ability moves is the graveyard card itself, where CR 400.7's replacement is
-- what leaves an Aura triggering from the battlefield naming a different
-- incarnation. It is also why this card is the FIRST in the pool to reach
-- CR 113.6m's Aura clause at all -- Pawl.Engine.EffectZone.zoneFunctionedFrom
-- answers Nothing for a move that names any other slot, so every printed Aura
-- with this condition answers Nothing through the fold whether the clause is
-- read or not, and only a bearer whose ability names its own source can tell
-- the exception's two sides apart.
--
-- CR 704.5n rather than CR 704.5m is what keeps the Equipment on the battlefield
-- when its host dies, so its ability is read there, by `eventTriggers`'
-- `battlefieldAbilitiesOf` filter, off the live projection. That the CONDITION
-- would match is skullclampSpec's, on the printing that reaches it.
widowedBladeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
widowedBladeSpec s registry =
  let board = do
        blade <- S.printingOf s registry "Synthetic Widowed Blade"
        hillGiant <- S.printingOf s registry "Hill Giant"
        piker <- S.printingOf s registry "Goblin Piker"
        let (host, g1) = S.addCreature hillGiant S.alice (Setup.emptyGame S.bothPlayers)
            (_, g2) = S.addCreature hillGiant S.alice g1
            (_, g3) = S.addCreature piker S.bob g2
            (equipment, g4) = S.addCreature blade S.alice g3
        pure (host, equipment, blade, S.attach equipment host g4)
      kill victim gs = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [victim] >> Engine.settleForPriority)
   in Spec.describe s "WidowedBlade" $ do
        -- The gameplay board, and the leg that proves the gate: the condition
        -- DOES match for a surviving Equipment bearer -- skullclampSpec below is
        -- that behaviour on its own card -- so what keeps the stack empty here
        -- is CR 113.6m alone, and granting this Equipment the Aura exception
        -- puts the trigger on it. The leg below is the same gate one level down.
        Spec.it s "CR 113.6m an Equipment gets no Aura exception, so its ability never triggers from the battlefield" $ do
          (host, equipment, _, gs) <- board
          let after = kill host gs
          Spec.assertEqWith s "CR 113.6m the Equipment's ability is pinned to the graveyard, so the stack is empty" (fmap (\oid -> fmap Object.source (Game.lookupObject oid after)) (GameState.stack after)) []
          -- The preconditions the assertion rests on, AFTER it so neither can
          -- absorb a mutation aimed at the subtype gate.
          Spec.assertEqWith s "the equipped Hill Giant really died" (Game.lookupObject host after) Nothing
          Spec.assertBool s (Maybe.isJust (Game.lookupObject equipment after)) "and CR 704.5n left the Equipment on the battlefield, where its ability was read"
        -- The pair, one level down and differing in exactly one thing: the same
        -- ability, read once with the card's own subtypes and once with Aura
        -- added. Nothing else about the ability moves.
        Spec.it s "CR 113.6m the same ability answers differently once its bearer is an Aura" $ do
          blade <- S.printingOf s registry "Synthetic Widowed Blade"
          let face = S.combinedFace blade
              zonesWith subtypes = fmap (Event.zoneFunctionedFrom subtypes (Face.delayedAbilities face)) (Face.triggeredAbilities face)
          Spec.assertEqWith s "as an Equipment the ability functions only in the graveyard" (zonesWith (TypeLine.subtypes (Face.typeLine face))) [Just Zone.Graveyard]
          Spec.assertEqWith s "as an Aura the exception applies and it names no zone" (zonesWith (Set.insert Subtype.Aura (TypeLine.subtypes (Face.typeLine face)))) [Nothing]
          Spec.assertBool s (not (Set.member Subtype.Aura (TypeLine.subtypes (Face.typeLine face)))) "and the printed card really is no Aura"

-- CR 603.10a's look-back at an attachment, on the EQUIPMENT side. Skullclamp
-- {1} Artifact -- Equipment, "Equipped creature gets +1/-1. / Whenever equipped
-- creature dies, draw two cards. / Equip {1}", is the printing. (Name, cost,
-- type line and oracle text checked against Scryfall.)
--
-- CR 303.4m makes "equipped creature" the same attachment link and the same
-- TriggerCondition.AttachedCreatureDies an Aura's "enchanted creature dies"
-- uses; CR 704.5n makes it a different QUESTION. The Equipment becomes
-- unattached and REMAINS on the battlefield in the same CR 117.5 batch that
-- buried its host, so at the moment triggers are placed there is neither a live
-- link to read nor a CR 608.2h record of the Equipment -- it never ceased. The
-- reading that survives is the host's own record of what was attached to it
-- (Pawl.Types.LastKnown.attached), which is what CR 603.10a asks for: the
-- appearance of the objects immediately prior to the event.
--
-- Three legs. The pair differs in exactly one thing -- WHICH of alice's two
-- creatures is destroyed -- so the negative cannot pass for want of a trigger
-- event or of a card to draw; the third is the board this card is famous for,
-- where nothing is destroyed at all and the Equipment's own +1/-1 does it.
--
-- alice's library is stocked so that CR 104.3c decks nobody before the
-- assertions run, and Hill Giant is the host rather than the Piker because a
-- 3/3 equipped is a 4/2 and survives to be killed on purpose.
skullclampSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
skullclampSpec s registry =
  let board = do
        clamp <- S.printingOf s registry "Skullclamp"
        hillGiant <- S.printingOf s registry "Hill Giant"
        piker <- S.printingOf s registry "Goblin Piker"
        let (host, g1) = S.addCreature hillGiant S.alice (Setup.emptyGame S.bothPlayers)
            (bystander, g2) = S.addCreature piker S.alice g1
            (equipment, g3) = S.addCreature clamp S.alice g2
        pure (host, bystander, equipment, S.attach equipment host (stock hillGiant g3))
      -- Three cards, one more than the trigger draws, so an empty library is
      -- never what the hand size is reporting.
      stock printing gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing S.alice g)) gs [1 :: Int, 2, 3]
      -- CR 117.5's own settle: the SBA pass buries the victim and detaches the
      -- Equipment, and the trigger is put on the stack after it.
      kill victim gs = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [victim] >> Engine.settleForPriority)
      settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority
      resolve gs = S.runPure S.identityAnswer gs Stack.resolveTop
   in Spec.describe s "Skullclamp" $ do
        -- The proving leg. The discriminating quantity is alice's hand: the
        -- trigger draws two, and a matcher that cannot see the attachment places
        -- no trigger and draws nothing.
        Spec.it s "CR 603.10a an Equipment's dies trigger fires though CR 704.5n left it standing" $ do
          (host, _, equipment, gs) <- board
          let placed = kill host gs
              after = resolve placed
          Spec.assertEqWith s "CR 603.10a alice drew two cards" (S.handSize S.alice after) 2
          -- The preconditions the assertion rests on, AFTER it so none of them
          -- can absorb a mutation aimed at the look-back.
          Spec.assertEqWith s "the equipped Hill Giant really died" (Game.lookupObject host after) Nothing
          Spec.assertEqWith s "CR 704.5n left the Equipment on the battlefield with its link cleared" (fmap Object.attachedTo (Game.lookupObject equipment after)) (Just Nothing)
          Spec.assertEqWith s "so the Equipment filed no record of its own" (Map.lookup equipment (GameState.lastKnown after)) Nothing
          Spec.assertEqWith s "the host's record is the one that names it" (fmap LastKnown.attached (Map.lookup host (GameState.lastKnown after))) (Just (Set.singleton equipment))
          Spec.assertEqWith s "alice held nothing before the trigger resolved" (S.handSize S.alice placed) 0
          Spec.assertEqWith s "and the trigger really was on the stack" (length (GameState.stack placed)) 1
        -- The negative, one creature over: the same board, the same removal, the
        -- same library -- and a permanent the Equipment was never attached to.
        Spec.it s "CR 303.4m another creature of the same controller dying is not the equipped creature dying" $ do
          (_, bystander, _, gs) <- board
          let placed = kill bystander gs
          Spec.assertEqWith s "no trigger is placed" (length (GameState.stack placed)) 0
          Spec.assertEqWith s "so alice draws nothing" (S.handSize S.alice placed) 0
          Spec.assertEqWith s "and the Goblin Piker really died" (Game.lookupObject bystander placed) Nothing
        -- CR 603.10a's own case: the Equipment leaves in the SAME batch as its
        -- host, and ahead of it in the batch's order. The host's record must be
        -- taken from the pre-batch board, or the live board has already
        -- forgotten the link by the time the host files it.
        Spec.it s "CR 603.10a the Equipment dying in the same batch, ahead of its host, still fires" $ do
          (host, _, equipment, gs) <- board
          let placed = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [equipment, host] >> Engine.settleForPriority)
              after = resolve placed
          Spec.assertEqWith s "CR 603.10a alice drew two cards" (S.handSize S.alice after) 2
          Spec.assertEqWith s "the host's record still names the Equipment" (fmap LastKnown.attached (Map.lookup host (GameState.lastKnown after))) (Just (Set.singleton equipment))
          Spec.assertEqWith s "and both really left the battlefield in one batch" (fmap (`Game.lookupObject` after) [host, equipment]) [Nothing, Nothing]
        -- Skullclamp's own board, with no removal in it: CR 613.4c's layer 7c
        -- makes the equipped 2/1 Piker a 3/0 and CR 704.5f buries it, which is
        -- the same look-back reached through the card's static half.
        Spec.it s "CR 704.5f the printed +1/-1 kills the equipped creature and the trigger still fires" $ do
          clamp <- S.printingOf s registry "Skullclamp"
          hillGiant <- S.printingOf s registry "Hill Giant"
          piker <- S.printingOf s registry "Goblin Piker"
          let (host, g1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
              (equipment, g2) = S.addCreature clamp S.alice g1
              placed = settle (S.attach equipment host (stock hillGiant g2))
              after = resolve placed
          Spec.assertEqWith s "CR 704.5f alice drew two cards off the toughness her own Equipment took away" (S.handSize S.alice after) 2
          Spec.assertEqWith s "the 3/0 Goblin Piker really died" (Game.lookupObject host after) Nothing
          Spec.assertBool s (Maybe.isJust (Game.lookupObject equipment after)) "and CR 704.5n left the Equipment behind"

-- CR 113.6m's FINAL sentence, over a whole card: "the same is true if the effect
-- of that ability creates a delayed triggered ability whose effect moves the
-- object out of a particular zone". Prized Amalgam {1}{U}{B} Creature -- Zombie
-- 3/3, "Whenever a creature enters, if it entered from your graveyard or you cast
-- it from your graveyard, return this card from your graveyard to the battlefield
-- tapped at the beginning of the next end step", is the printing that turns on
-- it. (Name, cost, type line and oracle text checked against Scryfall.)
--
-- The ability's only zone-relevant content sits INSIDE the delayed ability its
-- arm creates, so without the sentence Event.zoneFunctionedFrom answers Nothing
-- off the arm, CR 113.6's own default puts the ability on the battlefield, and
-- the card gets both halves wrong at once: it never fires from the graveyard,
-- where all its work is, and it fires from the battlefield, where CR 113.6m says
-- it does not function at all. One leg each below.
--
-- The two boards differ in ONE thing, the zone the Amalgam starts in. The two
-- Swamps, the Skeleton in the graveyard, the activation and the end step are
-- shared, so the negative cannot pass for want of mana or of a trigger event.
--
-- Reassembling Skeleton supplies the entry, for
-- Pawl.ConditionSpec.interveningRecheckSpec's reason: "{1}{B}: Return this card
-- from your graveyard to the battlefield tapped" is one activation, so a creature
-- enters out of a graveyard with no reanimation spell in the way and the
-- Amalgam's intervening "if it entered from your graveyard" is true.
--
-- The negative's quantity is what the STACK holds, as it is for
-- screamsFromWithinSpec's proving leg. "Functions only in that zone" is a claim
-- about whether the ability triggers at all, and the end-step board cannot make
-- it: the armed move names the graveyard, an Amalgam standing on the battlefield
-- is not in one, and the board after the end step looks the same either way.
prizedAmalgamSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
prizedAmalgamSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      -- TriggerSpec's tokenSetSpec's step: the phase written and the CR 513.1
      -- event recorded, which is what a "beginning of the next end step" delayed
      -- ability watches for.
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)) (gs {GameState.phase = endStep})
      settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority
      resolveTop gs = S.runPure S.identityAnswer gs Stack.resolveTop
      amalgamName = CardName.MkCardName (Text.pack "Prized Amalgam")
      skeletonName = CardName.MkCardName (Text.pack "Reassembling Skeleton")
      -- CR 400.7 mints a fresh id for a returned card, so both censuses go by
      -- name. Game.zoneMembers indexes the battlefield by OWNER (CR 108.3), which
      -- is the seat everything below is made under, and alice owns all of it.
      onBattlefieldNamed name gs = filter (\oid -> S.soleFaceName oid gs == name) (Set.toList (GameState.battlefield gs))
      -- The tap state of every Prized Amalgam on the battlefield. The printed
      -- "tapped" is what tells the delayed return from an Amalgam that was
      -- already standing there.
      amalgamTapStates gs = fmap (\oid -> fmap Object.tapped (Game.lookupObject oid gs)) (onBattlefieldNamed amalgamName gs)
      -- alice holding priority in her own main phase with two untapped Swamps --
      -- the {1}{B} the Skeleton's ability costs -- plus a Reassembling Skeleton in
      -- her graveyard, and the Amalgam wherever `place` puts it.
      board place amalgam skeleton swamp =
        let base =
              (S.landsInPlay swamp 2)
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
         in S.addGraveyardCard skeleton S.alice (snd (place amalgam S.alice base))
      -- Activate the Skeleton's graveyard ability, resolve it, and settle, so
      -- CR 603.3 has placed whatever the entry triggered.
      raiseWith place amalgam skeleton swamp k =
        let (gyId, staged) = board place amalgam skeleton swamp
         in case Activate.abilitiesFor gyId staged of
              [ability] -> k (settle (resolveTop (S.runPure S.identityAnswer staged (Activate.activateAbility S.alice gyId ability))))
              abilities -> Spec.assertEqWith s "exactly one ability to activate" (length abilities) 1
      -- Into the end step and all the way down: the delayed ability triggers, is
      -- placed, and resolves.
      throughEndStep gs = S.runPure S.identityAnswer (settle (beginEndStep gs)) Engine.priorityLoop
   in Spec.describe s "PrizedAmalgam" $ do
        -- The proving leg. CR 113.6m's final sentence is the whole of why the
        -- ability is read from the graveyard at all.
        Spec.it s "CR 113.6m an ability whose delayed trigger returns it from the graveyard functions there" $ do
          amalgam <- S.printingOf s registry "Prized Amalgam"
          skeleton <- S.printingOf s registry "Reassembling Skeleton"
          swamp <- S.printingOf s registry "Swamp"
          raiseWith S.addGraveyardCard amalgam skeleton swamp $ \raised -> do
            let after = throughEndStep (resolveTop raised)
            Spec.assertEqWith s "CR 113.6m the Amalgam came back from the graveyard, tapped" (amalgamTapStates after) [Just TapState.Tapped]
            -- The preconditions the assertion rests on, AFTER it so none of them
            -- can absorb a mutation aimed at the sentence.
            Spec.assertEqWith s "the Skeleton really entered from the graveyard" (length (onBattlefieldNamed skeletonName raised)) 1
            Spec.assertEqWith s "the Amalgam's trigger was on the stack" (length (GameState.stack raised)) 1
            Spec.assertEqWith s "and resolving it armed exactly one delayed ability" (Seq.length (GameState.delayedTriggers (resolveTop raised))) 1
        -- The "ONLY" in "functions only in that zone", and the leg that reddens
        -- if the reading is left additive. One difference from the leg above:
        -- the Amalgam starts on the battlefield.
        Spec.it s "CR 113.6m the same ability does not function from the battlefield" $ do
          amalgam <- S.printingOf s registry "Prized Amalgam"
          skeleton <- S.printingOf s registry "Reassembling Skeleton"
          swamp <- S.printingOf s registry "Swamp"
          raiseWith S.addCreature amalgam skeleton swamp $ \raised -> do
            let after = throughEndStep raised
            Spec.assertEqWith s "CR 113.6m nothing triggered off the entry" (length (GameState.stack raised)) 0
            Spec.assertEqWith s "so nothing was armed to fire at the end step" (Seq.length (GameState.delayedTriggers after)) 0
            Spec.assertEqWith s "and the one Amalgam is the untapped one that was already there" (amalgamTapStates after) [Just TapState.Untapped]
            Spec.assertEqWith s "off the same entry the leg above triggers on" (length (onBattlefieldNamed skeletonName raised)) 1

-- CR 303.4b's ENCHANTED CREATURE under CR 400.7f's condition: the other
-- permanent an "enchanted creature dies" event names, which Screams from Within
-- above never reads. Banewasp Affliction {1}{B} Enchantment -- Aura: "Enchant
-- creature / When enchanted creature dies, that creature's controller loses life
-- equal to its toughness." (Name, cost, type line and oracle text checked against
-- Scryfall.) Both halves of that sentence read the host as CR 608.2h's last known
-- information, not the graveyard card it became: CR 108.4 gives a card in a
-- graveyard no controller and CR 400.7 left it with no battlefield toughness. The
-- controller HALF cannot tell the two apart on this board -- pawl answers the same
-- seat for either id -- so the toughness is what the counter below discriminates
-- on. Event.eventBindings stamps
-- the id under Binding.departedPermanent, which is the one slot besides
-- Binding.sacrificedPermanent that Resolve.effectViewOf answers off CR 608.2h
-- last known information.
--
-- THE HOST IS BOB'S AND THE AURA IS ALICE'S, which is what separates CR 110.2's
-- "that creature's controller" from CR 109.5's "you": an arm reading the Aura's
-- own controller instead lands the loss on alice, and the pair of legs below
-- differ in exactly that one thing.
--
-- Giant Spider (2/4) is the host because power and toughness differ: an arm
-- reading Quantity.Power off the same slot answers the same number as the
-- toughness on a 3/3, and nothing else on the board would catch it.
--
-- AND IT CARRIES A +1/+1 COUNTER, which is what tells the DEPARTED permanent from
-- CR 400.7's graveyard incarnation the same move minted. CR 122.2 leaves the new
-- object with no counters, so the graveyard card reads the printed 4 while CR
-- 608.2h's last known information reads 5; without the counter both readings
-- answer 4 and the board cannot tell ZoneChange.departed from ZoneChange.object.
-- Three distinct numbers on the board -- power 3, printed toughness 4, last known
-- toughness 5 -- and only the last is right.
banewaspAfflictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
banewaspAfflictionSpec s registry =
  let board host = do
        affliction <- S.printingOf s registry "Banewasp Affliction"
        spider <- S.printingOf s registry "Giant Spider"
        let (enchanted, g1) = S.addCreature spider host (Setup.emptyGame S.bothPlayers)
            (aura, g2) = S.addCreature affliction S.alice (S.addCounter CounterKind.PlusOnePlusOne 1 enchanted g1)
        pure (enchanted, S.attach aura enchanted g2)
      -- Kill the host, settle CR 117.5 so the SBA pass buries it and the
      -- now-unattached Aura and the trigger is placed, then resolve it.
      run enchanted gs =
        S.runPure
          S.identityAnswer
          gs
          (Event.destroy Regenerability.Regenerable [enchanted] >> Engine.settleForPriority >> Stack.resolveTop)
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs)
   in Spec.describe s "BanewaspAffliction" $ do
        Spec.it s "CR 608.2h the host's controller loses life equal to the dead host's toughness" $ do
          (enchanted, gs) <- board S.bob
          let after = run enchanted gs
          Spec.assertEqWith s "CR 110.2 bob controlled the Spider, so bob paid its last known 5 toughness and alice paid nothing" (lives after) (Just 20, Just 15)
          -- The preconditions the assertion rests on, AFTER it so neither can
          -- absorb a mutation aimed at the binding.
          Spec.assertEqWith s "the enchanted Giant Spider really died" (Game.lookupObject enchanted after) Nothing
          Spec.assertEqWith s "and the trigger really resolved" (length (GameState.stack after)) 0
        -- The same board with the host moved one seat, which is the only
        -- difference: CR 109.5's "you" is alice in both legs, so a reading that
        -- had substituted her would answer the same pair twice.
        Spec.it s "CR 110.2 the loss follows the host's controller rather than the Aura's" $ do
          (enchanted, gs) <- board S.alice
          Spec.assertEqWith s "alice controlled the Spider, so alice pays" (lives (run enchanted gs)) (Just 15, Just 20)

-- CR 113.6m's general "unless" clause, over a whole card: "an ability whose...
-- effect specifies that it moves the object it's on out of a particular zone
-- functions only in that zone, UNLESS its trigger condition... specifies that
-- the object is put into that zone". Combined here with CR 113.6m's final
-- sentence (#2500): the graveyard-naming move sits inside a DELAYED ability the
-- dies trigger arms, the shape `Pawl.Engine.Event.leftBattlefield` -- CR
-- 603.10a's look-back at the very event that removes a bearer -- exists to
-- read correctly.
--
-- Ivory Gargoyle {4}{W} Creature -- Gargoyle 2/2, "Flying. When this creature
-- dies, return it to the battlefield under its owner's control at the beginning
-- of the next end step and you skip your next draw step. {4}{W}: Exile this
-- creature." (data/cards/ivory-gargoyle.json, checked against Scryfall) is the
-- printing: a dies trigger arming a delayed return whose payload names the
-- graveyard as the card's origin. The Scarab God is the same shape returning to
-- hand.
--
-- Endless Cockroaches cannot prove this: its own effect moves Binding.became
-- rather than the reserved source slot, so Pawl.Engine.EffectZone.zoneFunctionedFrom
-- already answers Nothing for it and the exception is never reached
-- (`becameSlotSpec` above proves the trigger fires either way). Here the
-- delayed ability's own effect names the reserved slot, so the fold DOES pin
-- the graveyard, and only `Event.conditionPutsSelfInto` reading SelfDies against
-- that zone exempts the ability back to the battlefield default -- where its
-- SelfDies condition needs to function to ever be checked at all.
--
-- The proving quantity is whether the trigger reaches the stack: without the
-- exception, `functionsIn Zone.Battlefield` is False for this ability,
-- `leftBattlefield`'s filter excludes it from the death event's own candidates,
-- and SelfDies is never checked against the one event that could satisfy it.
--
-- Not carried to the delayed ability's OWN resolution: CR 603.7e gives a
-- delayed ability the ARMING ability's source, which here is the dead
-- battlefield id CR 608.2h's last known information supplies for a SelfDies
-- trigger -- not Binding.became, which only `eventBindings` stamps onto the
-- ARMING ability's own bindings, never onto a delayed ability's. So the
-- payload's `InSlot self` finds nothing to move once the end step arrives; a
-- correct printing would need the delayed ability's own source rebound to the
-- graveyard incarnation, which is a second gap this unit does not fix (#3173).
-- The two legs below stop where that gap starts.
ivoryGargoyleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ivoryGargoyleSpec s registry =
  let settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority
      resolveTop gs = S.runPure S.identityAnswer gs Stack.resolveTop
      -- Kill the creature, settle CR 117.5 so the SBA pass places the dies
      -- trigger.
      kill victim gs = settle (S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [victim]))
   in Spec.describe s "IvoryGargoyle" $ do
        Spec.it s "CR 113.6m a dies-armed delayed graveyard return still fires from the battlefield" $ do
          gargoyle <- S.printingOf s registry "Ivory Gargoyle"
          let (victim, g1) = S.addCreature gargoyle S.alice (Setup.emptyGame S.bothPlayers)
              placed = kill victim g1
          Spec.assertEqWith s "CR 113.6m the dies trigger reached the stack" (length (GameState.stack placed)) 1
          -- The precondition the assertion above rests on, AFTER it so it
          -- cannot absorb a mutation aimed at the exception.
          Spec.assertEqWith s "the creature really died first" (Game.lookupObject victim placed) Nothing
          let armed = resolveTop placed
          Spec.assertEqWith s "resolving it armed exactly one delayed ability" (Seq.length (GameState.delayedTriggers armed)) 1
        -- The classification itself, isolated from the board: without the
        -- exception the delayed effect's stated graveyard origin would pin the
        -- ability there; with it, SelfDies exempts the pin away.
        Spec.it s "CR 113.6m the ability's own effect names the graveyard, and SelfDies exempts it" $ do
          gargoyle <- S.printingOf s registry "Ivory Gargoyle"
          let face = S.combinedFace gargoyle
          Spec.assertEqWith
            s
            "no zone is pinned once the exception is read"
            (fmap (Event.zoneFunctionedFrom (TypeLine.subtypes (Face.typeLine face)) (Face.delayedAbilities face)) (Face.triggeredAbilities face))
            [Nothing]

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Trigger" $ do
  permanentLeavesTheBattlefieldSpec s registry
  permanentReturnedToHandSpec s registry
  permanentsReturnedToHandSpec s registry
  kishlaSkimmerSpec s registry
  warpedDevotionSpec s registry
  becameSlotSpec s registry
  promiseOfTomorrowSpec s registry
  cleopatraSpec s registry
  promiseOfTomorrowReturnSpec s registry
  lookBackInterveningSpec s registry
  counterLookBackSpec s registry
  undyingSpec s registry
  afterlifeSpec s registry
  fabricateSpec s registry
  wardSpec s registry
  cutpurseSpec s registry
  gomazoaSpec s registry
  amuletSpec s registry
  soulshiftSpec s registry
  hauntSpec s registry
  screamsFromWithinSpec s registry
  widowedBladeSpec s registry
  skullclampSpec s registry
  prizedAmalgamSpec s registry
  ivoryGargoyleSpec s registry
  banewaspAfflictionSpec s registry
  strippedTriggerSpec s registry
  bystanderSpec s registry
  bystanderZoneSpec s registry
  aetherFlashSpec s registry
  kindredSpec s registry
