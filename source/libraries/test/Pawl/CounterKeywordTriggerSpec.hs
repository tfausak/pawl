{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Trigger over keyword triggers that move counters or count damage
-- (CR 702): renown, vanishing, fading, cumulative upkeep, modular, the combat-
-- damage bracket, and flanking through rampage. Split out of
-- Pawl.KeywordTriggerSpec, which keeps the machinery.
module Pawl.CounterKeywordTriggerSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import Pawl.CrewSpec (crewWith)
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Event.Binding as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AbilityTriggered as AbilityTriggered
import qualified Pawl.Types.AttackerBlocked as AttackerBlocked
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.SelfCountersRemoved as SelfCountersRemoved
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.StepBegins as StepBegins
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.WithCounters as WithCounters
import qualified Pawl.Types.Zone as Zone

renownSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
renownSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- CR 122.1: what is actually on the permanent, which a +2/+2 EFFECT would
      -- leave empty while reading the same 6/6.
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
      -- CR 702.112b's designation itself, which no characteristic reports.
      renownedness oid gs = fmap (Set.member Designation.Renowned . Object.designations) (Game.lookupObject oid gs)
   in Spec.describe s "Renown" $ do
        -- What separates renown from a damage rider: it is a TRIGGERED ability, so
        -- the counters arrive when it resolves, not as the damage is dealt.
        -- S.fightWith deals combat damage without reaching a priority boundary,
        -- so nothing has been gathered yet -- poisonous' case, read on an object.
        Spec.it s "CR 702.112a the counters ride the stack, not the damage" $ do
          (gs, mine, _) <- board ["Rhox Maulers"] []
          case mine of
            [maulers] -> do
              let fought = S.fightWith S.aggressiveAnswer gs
              Spec.assertEqWith s "damage is dealt" (S.lifeOf S.bob fought) (Just 16)
              Spec.assertEqWith s "but no counters until the trigger resolves" (countersOn maulers fought) Map.empty
              Spec.assertEqWith s "and no designation either" (renownedness maulers fought) (Just False)
            _ -> Spec.assertFailure s "fixture should give alice a Rhox Maulers"
        -- CR 702.112b's "it stays renowned UNTIL IT LEAVES THE BATTLEFIELD": the
        -- designation is per-incarnation state, so CR 400.7's forgetting is what
        -- ends it and a Maulers that dies and returns must connect again.
        --
        -- Asserted on Object.newIncarnation directly, as Pawl.RoomSpec's unlocked
        -- designations are: nothing writes this field on an entry, so a bounce
        -- would read the same forgetting through more machinery. Pawl.SetupSpec's
        -- CR 400.7 case does NOT cover it -- `forgotten` asks whether the
        -- forgetting is idempotent, which is blind to a field it never touches.
        Spec.it s "CR 702.112b the designation does not survive CR 400.7" $ do
          (gs, mine, _) <- board ["Rhox Maulers"] []
          case mine of
            [maulers] -> case Game.lookupObject maulers (S.runCombat S.aggressiveAnswer gs) of
              Nothing -> Spec.assertFailure s "expected to find the Maulers"
              Just obj -> do
                Spec.assertEqWith s "the control: this incarnation is renowned" (Set.member Designation.Renowned (Object.designations obj)) True
                Spec.assertEqWith s "the next one is not" (Set.member Designation.Renowned (Object.designations (Object.newIncarnation obj))) False
            _ -> Spec.assertFailure s "fixture should give alice a Rhox Maulers"
        -- CR 702.112c: "if a creature has multiple instances of renown, each
        -- triggers separately". Asserted of the MINT, as poisonous' multiplicity
        -- is, no card in the pool printing renown twice. What rule 702.112c says
        -- happens NEXT -- the second resolving to nothing -- is the intervening
        -- "if" the gameplay cases above read.
        Spec.it s "CR 702.112c each instance of renown is its own ability" $ do
          Spec.assertEqWith s "renown 2 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Renown 2) 2)) [Keyword.renown 2, Keyword.renown 2]
          Spec.assertEqWith s "and renown 6 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Renown 6) 1)) [Keyword.renown 6]

-- CR 701.37b's designation watched from outside the monstrosity action that sets
-- it: "monstrous is a designation ... that the monstrosity action and OTHER SPELLS
-- AND ABILITIES can identify", read through
-- TriggerCondition.PermanentBecomesDesignated -- the same condition Valeron
-- Wardens uses for renowned, with the other designation as its payload.
--
-- Arbor Colossus {2}{G}{G}{G} Creature -- Giant 6/6, "Reach. {3}{G}{G}{G}:
-- Monstrosity 3. When this creature becomes monstrous, destroy target creature
-- with flying an opponent controls."
--
-- bob holds Bird Maiden 1/2 flying and Goblin Piker 2/1: the Piker is the
-- falsifier for a target slot that dropped "with flying", and both are his, so no
-- assertion here turns on the seat.
--
-- TWELVE Forests, not six: the second-monstrosity case has to be able to PAY for
-- its activation, or it would prove nothing but an unpayable cost.
--
-- The DESIGNATION is what the last case turns on. Valeron Wardens watches the same
-- condition with Renowned, so a matcher that compared only the event's SHAPE would
-- draw alice a card when her Colossus became monstrous.
arborColossusSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
arborColossusSpec s registry =
  let monstrousness oid gs = fmap (Set.member Designation.Monstrous . Object.designations) (Game.lookupObject oid gs)
      plusOnes = S.counterOf CounterKind.PlusOnePlusOne
      -- The trigger TARGETS (CR 603.3d), so the answerer has to aim it; `victim`
      -- pins the choice rather than searching for a legal one, which is what lets
      -- the Piker case below fail rather than repair itself.
      aimed :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      aimed victim p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> Set.filter ((== Just victim) . Recipient.objectOf) legal) sets
        _ -> S.identityAnswer p
      -- One activation of the one monstrosity ability, its trigger settled onto
      -- the stack and resolved, with `victim` aimed at.
      monstrosity colossus victim gs = case Activatable.abilitiesFor colossus gs of
        [ability]
          -- CR 701.37a's condition is the CLAUSE's, not an activation
          -- restriction, so a monstrous permanent's ability stays activatable --
          -- which is what makes the second case below a real activation rather
          -- than an unpaid one.
          | Activatable.activatable S.alice colossus ability gs ->
              Right . snd . Engine.runGamePure (aimed victim) gs $ do
                Activate.activateAbility S.alice colossus ability
                Stack.resolveTop
                Engine.settleForPriority
                Engine.priorityLoop
        [_] -> Left 0
        other -> Left (length other)
      board extra = do
        colossusPrinting <- S.printingOf s registry "Arbor Colossus"
        forest <- S.printingOf s registry "Forest"
        maidenPrinting <- S.printingOf s registry "Bird Maiden"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        base <- extra (S.landsInPlay forest 12)
        let (colossus, g1) = S.addPermanent colossusPrinting S.alice base
            (maiden, g2) = S.addPermanent maidenPrinting S.bob g1
            (piker, g3) = S.addPermanent pikerPrinting S.bob g2
        pure (colossus, maiden, piker, g3)
   in Spec.describe s "Arbor Colossus" $ do
        -- The proving test. CR 701.37a's counters and designation, and then rule
        -- 701.37b's marker read by an ability of the same permanent: the flier dies.
        Spec.it s "CR 701.37b whole card: monstrosity 3 marks the Colossus and its trigger destroys the flier" $ do
          (colossus, maiden, piker, gs) <- board pure
          case monstrosity colossus maiden gs of
            Left n -> Spec.assertFailure s ("expected one activatable monstrosity ability, got " <> show n)
            Right after -> do
              Spec.assertEqWith s "not monstrous to begin with" (monstrousness colossus gs) (Just False)
              Spec.assertEqWith s "three counters, not one" (plusOnes colossus after) 3
              Spec.assertEqWith s "so it is a 9/9" (S.powerToughnessOf colossus after) (Just (9, 9))
              Spec.assertEqWith s "and it is monstrous" (monstrousness colossus after) (Just True)
              Spec.assertBool s (not (S.onBattlefield maiden after)) "the targeted flier was destroyed"
              Spec.assertBool s (S.onBattlefield piker after) "and the ground creature was not"
        -- CR 701.37a's "if this permanent isn't monstrous": the SECOND activation
        -- on the same board does nothing, so the trigger never fires and bob's
        -- second flier lives. The same mana, the same seats, the same creatures --
        -- the one difference is that the Colossus is already monstrous.
        Spec.it s "CR 701.37a a second monstrosity marks nothing, so nothing triggers" $ do
          (colossus, maiden, _, gs) <- board pure
          maidenPrinting <- S.printingOf s registry "Bird Maiden"
          case monstrosity colossus maiden gs of
            Left n -> Spec.assertFailure s ("expected one activatable monstrosity ability, got " <> show n)
            Right once -> do
              let (second, withSecond) = S.addPermanent maidenPrinting S.bob once
              case monstrosity colossus second withSecond of
                Left n -> Spec.assertFailure s ("expected the monstrous Colossus to stay activatable, got " <> show n)
                Right twice -> do
                  Spec.assertEqWith s "still three counters, not six" (plusOnes colossus twice) 3
                  Spec.assertBool s (S.onBattlefield second twice) "the second flier survived, nothing having become monstrous"
        -- The designation is LOAD-BEARING in the match, not just the event's shape.
        -- Valeron Wardens {2}{G} 1/3 watches "whenever a creature you control
        -- becomes renowned" -- the same TriggerCondition constructor with Renowned
        -- in it -- and the Colossus becoming monstrous is not that. alice's library
        -- is stocked, so a spurious draw is visible rather than fatal (CR 104.3c).
        Spec.it s "CR 701.37b a creature becoming monstrous is not a creature becoming renowned" $ do
          wardensPrinting <- S.printingOf s registry "Valeron Wardens"
          piker <- S.printingOf s registry "Goblin Piker"
          (colossus, maiden, _, gs) <- board (pure . snd . S.addLibraryCard piker S.alice . snd . S.addPermanent wardensPrinting S.alice)
          case monstrosity colossus maiden gs of
            Left n -> Spec.assertFailure s ("expected one activatable monstrosity ability, got " <> show n)
            Right after -> do
              Spec.assertEqWith s "the Colossus is monstrous" (monstrousness colossus after) (Just True)
              Spec.assertBool s (not (S.onBattlefield maiden after)) "so its own trigger did fire"
              Spec.assertEqWith s "and the Wardens drew nothing" (length (Game.zoneMembers Zone.Hand S.alice after)) 0

-- CR 701.37c: "other abilities of that permanent may also refer to X", and the
-- value of X in them "is equal to the value of X as that permanent became
-- monstrous". Arbor Colossus above is the same rule with a printed N; this is the
-- rule with an X the activation announced (CR 601.2b).
--
-- Hydra Broodmaster {4}{G}{G} 7/7 -- "{X}{X}{G}: Monstrosity X" and "When this
-- creature becomes monstrous, create X X/X green Hydra creature tokens" -- reads
-- the X back TWICE, for the token count and for the tokens' own box. Both are
-- asserted, since a reading that re-announced X or counted the +1/+1 counters
-- would get one of them right on its own.
hydraBroodmasterSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
hydraBroodmasterSpec s registry =
  let hydraToken = CardName.MkCardName (Text.pack "Hydra Token")
      monstrousness oid gs = fmap (Set.member Designation.Monstrous . Object.designations) (Game.lookupObject oid gs)
      -- CR 701.37c's record itself, read off the permanent rather than inferred
      -- from what the trigger made.
      recordedX oid gs = Game.lookupObject oid gs >>= Map.lookup Designation.Monstrous . Object.designationValues
      plusOnes = S.counterOf CounterKind.PlusOnePlusOne
      tokenSizes gs =
        List.sort
          ( fmap
              (\oid -> S.powerToughnessOf oid gs)
              (filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just hydraToken) (Game.zoneMembers Zone.Battlefield S.alice gs))
          )
      -- CR 601.2b's announcement is the one question the activation asks.
      announcing :: Natural -> Prompt.Prompt r -> r
      announcing x p = case p of
        Prompt.ChooseX {} -> x
        _ -> S.identityAnswer p
      -- One activation announcing `x`, its trigger settled onto the stack and
      -- resolved -- Arbor Colossus' `monstrosity` above with a value to announce.
      monstrosity x broodmaster gs = case Activatable.abilitiesFor broodmaster gs of
        [ability]
          | Activatable.activatable S.alice broodmaster ability gs ->
              Right . snd . Engine.runGamePure (announcing x) gs $ do
                Activate.activateAbility S.alice broodmaster ability
                Stack.resolveTop
                Engine.settleForPriority
                Engine.priorityLoop
        [_] -> Left 0
        other -> Left (length other)
      -- Twelve Forests: {2}{2}{G} for the X=2 activation and {3}{3}{G} for the
      -- second one below, so the negative differs from the positive in the
      -- designation alone and not in what alice can pay.
      board = do
        broodmasterPrinting <- S.printingOf s registry "Hydra Broodmaster"
        forest <- S.printingOf s registry "Forest"
        pure (S.addPermanent broodmasterPrinting S.alice (S.landsInPlay forest 12))
   in Spec.describe s "Hydra Broodmaster" $ do
        -- The proving test.
        Spec.it s "CR 701.37c monstrosity 2 makes TWO 2/2 Hydras" $ do
          (broodmaster, gs) <- board
          Spec.assertEqWith s "no Hydra token to begin with" (tokenSizes gs) []
          case monstrosity 2 broodmaster gs of
            Left n -> Spec.assertFailure s ("expected one activatable monstrosity ability, got " <> show n)
            Right after -> do
              Spec.assertEqWith s "two tokens, each a 2/2: the announced X is both the count and the box" (tokenSizes after) [Just (2, 2), Just (2, 2)]
              Spec.assertEqWith s "two +1/+1 counters, so the counters read the same X" (plusOnes broodmaster after) 2
              Spec.assertEqWith s "which makes the Broodmaster a 9/9" (S.powerToughnessOf broodmaster after) (Just (9, 9))
              Spec.assertEqWith s "and it is monstrous" (monstrousness broodmaster after) (Just True)
              Spec.assertEqWith s "with 2 recorded as the X it became monstrous with" (recordedX broodmaster after) (Just 2)
        -- CR 701.37a's "if this permanent isn't monstrous" and CR 701.37b's "it
        -- stays monstrous": the second activation announces a DIFFERENT X, pays
        -- for it, and does nothing -- so no trigger, no tokens, and the recorded
        -- value stays the first one rather than being overwritten.
        Spec.it s "CR 701.37b a second monstrosity announcing 3 makes nothing and leaves the recorded X at 2" $ do
          (broodmaster, gs) <- board
          case monstrosity 2 broodmaster gs of
            Left n -> Spec.assertFailure s ("expected one activatable monstrosity ability, got " <> show n)
            Right once -> case monstrosity 3 broodmaster once of
              Left n -> Spec.assertFailure s ("expected the monstrous Broodmaster to stay activatable, got " <> show n)
              Right twice -> do
                Spec.assertEqWith s "still the two 2/2 Hydras, no 3/3s" (tokenSizes twice) [Just (2, 2), Just (2, 2)]
                Spec.assertEqWith s "still two +1/+1 counters, not five" (plusOnes broodmaster twice) 2
                Spec.assertEqWith s "and the recorded X is unchanged" (recordedX broodmaster twice) (Just 2)

-- CR 702.63 vanishing, which rule 702 states as triggered
-- abilities -- and the first whose rule text spans BOTH mints, since rule
-- 702.63a's three abilities are one CR 614.1c entry replacement
-- (Keyword.mintedReplacementsFor, riot's position) and two triggers.
--
-- Waning Wurm {3}{B} Creature -- Zombie Wurm 7/6 is the card, and it is nothing
-- but the keyword: no second ability can put a counter on it, take one off, or
-- keep it alive, so every number below is vanishing's own.
--
-- Vanishing 2 rather than a larger printing (Calciderm's 4) because two is the
-- smallest N that tells the two triggers apart: the first upkeep must remove one
-- and NOT sacrifice, the second must do both.
vanishingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
vanishingSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      -- One upkeep for `pid`, run to the end of the priority loop, so the
      -- trigger is gathered (CR 603.3) and resolved.
      upkeepOf pid gs =
        let began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep pid)) (gs {GameState.phase = upkeep, GameState.activePlayer = pid})
            settled = snd (Engine.runGamePure S.identityAnswer began Engine.settleForPriority)
         in (settled, snd (Engine.runGamePure S.identityAnswer settled Engine.priorityLoop))
      times = S.counterOf CounterKind.Time
   in Spec.describe s "Vanishing" $ do
        -- CR 603.4's intervening "if": rule 702.63a's second ability does not
        -- trigger AT ALL on an upkeep where the permanent has no time counter, so
        -- nothing reaches the stack. S.addPermanent is what reaches this board --
        -- it places the wurm without running rule 702.63a's entry replacement, the
        -- position a card that lost its counters some other way would be in.
        --
        -- It also pins rule 702.63a's THIRD ability to the REMOVAL rather than to
        -- the count: a wurm sitting at zero is not sacrificed, because no last
        -- counter came off.
        Spec.it s "CR 603.4 a wurm with no time counters neither triggers nor is sacrificed" $ do
          wurm <- S.printingOf s registry "Waning Wurm"
          let (oid, gs) = S.addPermanent wurm S.alice (Setup.emptyGame S.bothPlayers)
              (settled, resolved) = upkeepOf S.alice gs
          Spec.assertEqWith s "it really has none" (times oid gs) 0
          Spec.assertEqWith s "nothing on the stack" (GameState.stack settled) []
          Spec.assertBool s (S.onBattlefield oid resolved) "and it survives its own upkeep"
        -- CR 702.63c: "if a permanent has multiple instances of vanishing, each
        -- works separately". Asserted of BOTH mints, as renown's multiplicity is
        -- asserted of one, no card in the pool printing vanishing twice.
        --
        -- Spelled out rather than compared against Keyword.vanishing itself: an
        -- assertion written that way says only that two copies are two copies,
        -- and a mint that dropped one of the pair would repair it silently.
        Spec.it s "CR 702.63c each instance is its own three abilities" $ do
          let counted = TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) Nothing TurnScope.ControllersTurn)
              emptied = TriggerCondition.SelfLastCounterRemoved (SelfCountersRemoved.MkSelfCountersRemoved CounterKind.Time Zone.Battlefield)
          Spec.assertEqWith
            s
            "vanishing 2 held twice mints four triggers, two of each kind"
            (fmap TriggeredAbility.condition (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Vanishing (Just 2)) 2)))
            [counted, emptied, counted, emptied]
          Spec.assertEqWith
            s
            "and two entry rewrites of two time counters each, which is what makes them add up"
            (Keyword.mintedReplacementsFor (Keyword.Type.Vanishing (Just 2)) 2)
            (replicate 2 (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource (EntryRewrite.WithCounters (WithCounters.one CounterKind.Time (Quantity.Type.Literal 2))))))

-- CR 702.63b, vanishing printed with NO number: the two triggers and no entry
-- replacement at all. Tidewalker {2}{U} Creature -- Elemental */* is the card --
-- "this creature enters with a time counter on it for each Island you control",
-- numberless vanishing, and a CR 208.2a characteristic-defining power and
-- toughness equal to the time counters on it. data/scenarios/counter-keyword-trigger
-- drives it whole.
numberlessVanishingSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
numberlessVanishingSpec s _ =
  Spec.describe s "Vanishing" $ do
    -- The mint itself, spelled out for vanishingSpec's reason. Rule 702.63b
    -- states the SAME two triggers as rule 702.63a and no entry ability, so
    -- the absent number changes exactly one of the two lists.
    Spec.it s "CR 702.63b keeps both triggers and mints no entry rewrite" $ do
      let counted = TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) Nothing TurnScope.ControllersTurn)
          emptied = TriggerCondition.SelfLastCounterRemoved (SelfCountersRemoved.MkSelfCountersRemoved CounterKind.Time Zone.Battlefield)
      Spec.assertEqWith
        s
        "both of rule 702.63a's triggers, numberless"
        (fmap TriggeredAbility.condition (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Vanishing Nothing) 1)))
        [counted, emptied]
      Spec.assertEqWith
        s
        "and no entry rewrite, however many instances"
        (Keyword.mintedReplacementsFor (Keyword.Type.Vanishing Nothing) 2)
        []

-- CR 702.32 fading, vanishing's neighbour and the reason the two are separate
-- keywords rather than one with a counter kind on it. Rule 702.32a states TWO
-- abilities where rule 702.63a states three, and it hangs the sacrifice on an
-- upkeep where no counter can come off rather than on the removal of the last
-- one -- so a fading N permanent sees N+1 of its controller's upkeeps and a
-- vanishing N permanent sees N.
--
-- That off-by-one is what the board below is built to read, and it is the whole
-- reason the second upkeep gets an assertion of its own: a fading 2 creature that
-- reached zero counters is still on the battlefield, which is exactly where a
-- vanishing 2 creature is not.
--
-- Skyshroud Ridgeback {G} Creature -- Beast 2/3 is the card, and it is nothing
-- but the keyword: no second ability can put a fade counter on it, take one off
-- or keep it alive, so every number below is fading's own. Fading 2 for
-- vanishing's reason -- two is the smallest N that puts a counted-down upkeep
-- between the entry and the sacrifice.
fadingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fadingSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      -- vanishingSpec's, and the same reasons: one upkeep for `pid`, run to the
      -- end of the priority loop so the trigger is gathered (CR 603.3) and
      -- resolved.
      upkeepOf pid gs =
        let began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep pid)) (gs {GameState.phase = upkeep, GameState.activePlayer = pid})
            settled = snd (Engine.runGamePure S.identityAnswer began Engine.settleForPriority)
         in (settled, snd (Engine.runGamePure S.identityAnswer settled Engine.priorityLoop))
      fades = S.counterOf CounterKind.Fade
   in Spec.describe s "Fading" $ do
        -- Rule 702.32a states NO intervening "if", which is the other half of the
        -- difference from rule 702.63a: the ability triggers on an upkeep where
        -- the pile is already empty, and that firing IS the sacrifice.
        -- S.addPermanent is what reaches this board -- it places the Ridgeback
        -- without running the entry replacement, the position a card that lost its
        -- counters some other way would be in.
        Spec.it s "CR 702.32a a Ridgeback with no fade counters triggers and is sacrificed at once" $ do
          ridgeback <- S.printingOf s registry "Skyshroud Ridgeback"
          let (oid, gs) = S.addPermanent ridgeback S.alice (Setup.emptyGame S.bothPlayers)
              (settled, resolved) = upkeepOf S.alice gs
          Spec.assertEqWith s "it really has none" (fades oid gs) 0
          Spec.assertEqWith s "and the ability still reached the stack" (length (GameState.stack settled)) 1
          Spec.assertBool s (not (S.onBattlefield oid resolved)) "so its own first upkeep took it"
        -- The mint, spelled out for vanishingSpec's reason: an assertion written
        -- against Keyword.fading itself would say only that one copy is one copy.
        -- Rule 702.32 states no multiplicity clause, so each instance is its own
        -- pair.
        Spec.it s "CR 702.32a each instance is its own two abilities" $ do
          Spec.assertEqWith
            s
            "fading 2 held twice mints two upkeep triggers"
            (fmap TriggeredAbility.condition (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Fading 2) 2)))
            (replicate 2 (TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) Nothing TurnScope.ControllersTurn)))
          Spec.assertEqWith
            s
            "and two entry rewrites of two FADE counters each, never rule 702.63a's time counters"
            (Keyword.mintedReplacementsFor (Keyword.Type.Fading 2) 2)
            (replicate 2 (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource (EntryRewrite.WithCounters (WithCounters.one CounterKind.Fade (Quantity.Type.Literal 2))))))

-- Rule 702.24a's "you MAY pay" answered yes for one seat -- ZoneTriggerSpec's
-- helper of the same name, duplicated rather than hoisted. S.identityAnswer
-- declines every CR 118.12 offer, and everything else falls through to it,
-- including the mana window the payment opens.
paysFor :: PlayerId.PlayerId -> Prompt.Prompt r -> r
paysFor who p = case p of
  Prompt.ChooseToPay (Decider.MkDecider d) player _ _ _ _
    | d == who && player == who ->
        PaymentDecision.Pays
  _ -> S.identityAnswer p

-- The pay-or-not answers in a transcript, in order -- ZoneTriggerSpec's helper
-- of the same name, duplicated for `paysFor`'s reason.
payResponses :: [Response.Response] -> [Response.Response]
payResponses = filter isPayResponse

isPayResponse :: Response.Response -> Bool
isPayResponse response = case response of
  Response.ChoseToPay _ -> True
  _ -> False

-- CR 702.24 cumulative upkeep, the first keyword whose CR 118.12 payment is not
-- the cost the card prints: rule 702.24a's "you may pay [cost] for each age
-- counter on it" multiplies the printed cost by a pile that grows an upkeep at a
-- time, which is what Pawl.Types.PayGate.perEach carries.
--
-- Revered Unicorn {1}{W} Creature -- Unicorn 2/3, "Cumulative upkeep {1}" and
-- "When this creature leaves the battlefield, you gain life equal to the number
-- of age counters on it". The second ability is why this printing and not a
-- keyword-only one: it reads the pile from OUTSIDE the keyword, so a life total
-- witnesses the count without asking Object.counters, and CR 608.2h answers it --
-- the Unicorn is in a graveyard by the time that trigger resolves.
--
-- SEEDED with two age counters against a board that has had none, modularSpec's
-- device: with an unseeded pile every number below would equal the number of
-- upkeeps run, and an engine reading the wrong one would still be green.
cumulativeUpkeepSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cumulativeUpkeepSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      -- vanishingSpec's, and the same reasons: one upkeep for `pid`, run to the
      -- end of the priority loop so the trigger is gathered (CR 603.3) and
      -- resolved. NO UNTAP STEP, deliberately: rule 702.24a's costs are meant to
      -- outrun the board, and a fixture that untapped between upkeeps would give
      -- alice the same five lands twice.
      steppedTo pid gs = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep pid)) (gs {GameState.phase = upkeep, GameState.activePlayer = pid})
      upkeepOf pid gs =
        let settled = snd (Engine.runGamePure (paysFor S.alice) (steppedTo pid gs) Engine.settleForPriority)
         in (settled, snd (Engine.runGamePure (paysFor S.alice) settled Engine.priorityLoop))
      after pid gs = snd (upkeepOf pid gs)
      -- The same upkeep with rule 702.24a's "may" answered no, S.identityAnswer
      -- declining every CR 118.12 offer. Two helpers rather than one taking the
      -- answerer, since Pawl.Engine.Engine.runGamePure wants a polymorphic one.
      afterDeclining pid gs =
        let settled = snd (Engine.runGamePure S.identityAnswer (steppedTo pid gs) Engine.settleForPriority)
         in snd (Engine.runGamePure S.identityAnswer settled Engine.priorityLoop)
      ages = S.counterOf CounterKind.Age
      -- Five Forests and a two-counter head start, which is the smallest board
      -- that tells rule 702.24a's multiplied cost from the printed {1}: the first
      -- upkeep asks {3} and the second {4}, and five lands cannot cover both.
      -- An engine offering {1} each time would pay every upkeep out of one land.
      boardOf = do
        forest <- S.printingOf s registry "Forest"
        unicorn <- S.printingOf s registry "Revered Unicorn"
        let (oid, placed) = S.addPermanent unicorn S.alice (S.landsFor forest S.alice 5 (Setup.emptyGame S.bothPlayers))
        pure (oid, S.addCounter CounterKind.Age 2 oid placed)
   in Spec.describe s "Cumulative upkeep" $ do
        -- The proving test. Every assertion is board state a player could see --
        -- lands spent, the permanent's zone, a life total -- and none of them
        -- reads the keyword back.
        Spec.it s "CR 702.24a the cost grows with the pile until the board can't cover it" $ do
          (oid, gs) <- boardOf
          Spec.assertEqWith s "the head start is really on it" (ages oid gs) 2
          Spec.assertEqWith s "and nothing is tapped yet" (S.tappedCount S.alice gs) 0
          let first = after S.alice gs
          -- The behaviour, ahead of every proxy: three Forests went for one
          -- upkeep of a permanent whose printed cost is {1}.
          Spec.assertEqWith s "CR 702.24a the first upkeep cost THREE mana, not one" (S.tappedCount S.alice first) 3
          Spec.assertBool s (S.onBattlefield oid first) "so the Unicorn survived it"
          Spec.assertEqWith s "on a third age counter" (ages oid first) 3
          Spec.assertEqWith s "and alice has gained nothing" (S.lifeOf S.alice first) (Just 20)
          let second = after S.alice first
          -- CR 118.3: two untapped Forests cannot pay {4}, so the offer is never
          -- made and rule 702.24a's "if you don't" runs.
          Spec.assertBool s (not (S.onBattlefield oid second)) "CR 702.24a the second upkeep asked {4} of two lands, so it was sacrificed"
          -- CR 701.21a: a sacrifice is a move to the OWNER's graveyard.
          Spec.assertEqWith s "into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice second)) 1
          -- The pile read from outside the keyword, through CR 608.2h.
          Spec.assertEqWith s "and its leaves-play trigger gained FOUR life, the pile it died on" (S.lifeOf S.alice second) (Just 24)
        -- The same board and the same upkeep, differing in NOTHING but the answer
        -- to rule 702.24a's "may". The five Forests can cover {3}, so this leg
        -- separates declining from being unable to pay.
        Spec.it s "CR 702.24a declining the payment sacrifices it with the mana still up" $ do
          (oid, gs) <- boardOf
          let first = afterDeclining S.alice gs
          Spec.assertEqWith s "CR 702.24a no Forest was spent" (S.tappedCount S.alice first) 0
          Spec.assertBool s (not (S.onBattlefield oid first)) "and the Unicorn went anyway"
          Spec.assertEqWith s "gaining three life, the pile after this upkeep's counter" (S.lifeOf S.alice first) (Just 23)
        -- CR 603.4's re-check, which is rule 702.24a's own "if this permanent is
        -- on the battlefield": bob murders the Unicorn with the upkeep trigger
        -- already on the stack, so the ability resolves with its permanent gone
        -- and does nothing at all.
        --
        -- The mana is what witnesses it. CR 113.7a sends the departed source
        -- through last known information (Pawl.Engine.Projection.viewWithLastKnown),
        -- so an engine that skipped rule 702.24a's "if" would still find the two
        -- age counters the Unicorn died on, offer {2} against five untapped
        -- Forests, and take them -- for an ability rule 603.4 says does nothing.
        Spec.it s "CR 603.4 a Unicorn murdered in response is never offered the cost" $ do
          (_, gs0) <- boardOf
          swamp <- S.printingOf s registry "Swamp"
          murder <- S.printingOf s registry "Murder"
          let (held, gs) = S.addHandCard murder S.bob (S.landsFor swamp S.bob 3 gs0)
              settled = fst (upkeepOf S.alice gs)
              killed = S.runPure (paysFor S.alice) settled (S.cast S.bob held >> Stack.resolveTop)
              ((_, done), transcript) = Replay.record (paysFor S.alice) killed (Engine.settleForPriority >> Engine.priorityLoop)
          Spec.assertEqWith s "the trigger really was on the stack when bob cast" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "CR 603.4 not one Forest went to an ability whose permanent had gone" (S.tappedCount S.alice done) 0
          Spec.assertEqWith s "because alice was never offered rule 702.24a's cost at all" (payResponses transcript) []
          -- The leaves-play trigger read the pile bob's Murder left it on, which
          -- is the head start and not a third counter.
          Spec.assertEqWith s "and alice gained TWO life, the pile the Unicorn died on" (S.lifeOf S.alice done) (Just 22)
        -- The mint, spelled out for vanishingSpec's reason. Rule 702.24b states
        -- the multiplicity clause explicitly -- "if a permanent has multiple
        -- instances of cumulative upkeep, each triggers separately" -- and rule
        -- 702.24b's second sentence is why both instances still read one pile:
        -- the counters belong to the permanent, not to an ability.
        Spec.it s "CR 702.24b each instance is its own ability over one pile" $ do
          let cost n = Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic n])) []
          Spec.assertEqWith
            s
            "cumulative upkeep {1} held twice mints two upkeep triggers"
            (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.CumulativeUpkeep (cost 1)) 2))
            [Keyword.cumulativeUpkeep (cost 1), Keyword.cumulativeUpkeep (cost 1)]
          Spec.assertBool s (Keyword.cumulativeUpkeep (cost 1) /= Keyword.cumulativeUpkeep (cost 2)) "and the cost reaches the minted ability"

-- CR 702.43 modular, whose rule text also spans BOTH of
-- Pawl.Engine.Keyword's mints -- one CR 614.1c entry replacement and one death
-- trigger. What is new is the trigger's PAYLOAD: rule 702.43a
-- counts "each +1/+1 counter on this permanent" at a moment when the permanent
-- is in a graveyard, so the number comes from CR 608.2h last known information.
-- Pawl.ZoneTriggerSpec's counterLookBackSpec proves the same record answering
-- an intervening "if"; this is the first read of it at RESOLUTION.
--
-- Printings at distinct N, so no number below can be read two ways:
--
--   * Arcbound Hybrid {4} Artifact Creature -- Beast 0/0, haste and modular 2.
--   * Arcbound Worker {1} Artifact Creature -- Construct 0/0, modular 1.
--   * Arcbound Overseer {8} Artifact Creature -- Golem 0/0, modular 6, for the
--     family case at the end.
--
-- The dying Hybrid is SEEDED to three counters against its printed modular 2,
-- which is the discriminator that matters: an implementation reading the
-- keyword's N instead of the counters on the permanent moves 2, and one reading a
-- literal moves 1. Only counting the pile moves 3.
--
-- Murder does the killing, counterLookBackSpec's reason: a 0/0 body plus counters
-- makes lethal damage a different number per leg, and CR 701.8a's destroy does
-- not care.
modularSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
modularSpec s registry =
  let plusOnes = S.counterOf CounterKind.PlusOnePlusOne
      -- Rule 702.43a's "you may", exercised. S.identityAnswer declines it, which
      -- is what the declining leg below rides.
      exercising :: Prompt.Prompt r -> r
      exercising p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      -- A Hybrid seeded with three +1/+1 counters and one companion creature,
      -- with a Murder in hand. The Hybrid is added FIRST so it holds the lesser
      -- ObjectId: Murder's pool is Pool.Creatures and identityAnswer takes the
      -- least recipient, so this is what aims the removal at it rather than at
      -- the companion.
      board companion = do
        swamp <- S.printingOf s registry "Swamp"
        murder <- S.printingOf s registry "Murder"
        hybrid <- S.printingOf s registry "Arcbound Hybrid"
        other <- S.printingOf s registry companion
        let lands = S.landsInPlay swamp 3
            (hybridId, g1) = S.addPermanent hybrid S.alice lands
            g2 = S.addCounter CounterKind.PlusOnePlusOne 3 hybridId g1
            (otherId, g3) = S.addPermanent other S.alice g2
            -- The companion carries a counter of its own, so a payload that
            -- overwrote rather than added would be visible, and so that a 0/0
            -- Worker survives CR 704.5f.
            g4 = S.addCounter CounterKind.PlusOnePlusOne 1 otherId g3
            -- CR 104.3c: nothing here draws, but a stocked library keeps a leg
            -- from ending on an empty one.
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard swamp S.alice g)) g4 [1 .. 5 :: Int]
        pure (hybridId, otherId, S.handOne murder stocked)
      -- Cast the Murder, resolve it (the Hybrid dies), settle so the death
      -- trigger is gathered (CR 603.3), then resolve the trigger.
      murderIt :: (forall r. Prompt.Prompt r -> r) -> (GameState.GameState, ObjectId.ObjectId) -> (GameState.GameState, GameState.GameState)
      murderIt answer (gs, spellId) =
        let cast = S.runPure answer gs (S.cast S.alice spellId)
            destroyed = S.runPure answer cast Stack.resolveTop
            settled = S.runPure answer destroyed Engine.settleForPriority
         in (settled, S.runPure answer settled Stack.resolveTop)
      -- A printing CAST rather than placed, because rule 702.43a's first ability
      -- is a replacement on the ENTRY -- S.addPermanent reaches no CR 616.1 loop.
      castOne name lands = do
        swamp <- S.printingOf s registry "Swamp"
        printing <- S.printingOf s registry name
        let (held, gs0) = S.addHandCard printing S.alice (S.landsInPlay swamp lands)
            gs =
              gs0
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            entered = S.runPure S.identityAnswer gs (S.cast S.alice held >> Stack.resolveTop)
            named oid = fmap Face.name (Game.faceOf oid entered) == Just (CardName.MkCardName (Text.pack name))
        pure (List.find named (Set.toList (GameState.battlefield entered)), entered)
      -- Arcbound Wanderer {6} cast off exactly six lands, so every one of them
      -- pays and the colours spent are the lands' colours.
      castWanderer lands = do
        wanderer <- S.printingOf s registry "Arcbound Wanderer"
        printings <- traverse (\(name, n) -> fmap (\p -> (p, n)) (S.printingOf s registry name)) lands
        let base = List.foldl' (\acc (land, n) -> S.landsFor land S.alice n acc) (Setup.emptyGame S.bothPlayers) printings
            (held, gs0) = S.addHandCard wanderer S.alice base
            gs =
              gs0
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            entered = S.runPure S.identityAnswer gs (S.cast S.alice held >> Stack.resolveTop)
            named oid = fmap Face.name (Game.faceOf oid entered) == Just (CardName.MkCardName (Text.pack "Arcbound Wanderer"))
        pure (List.find named (Set.toList (GameState.battlefield entered)), entered)
      -- One upkeep for alice, run to the end of the priority loop, so the
      -- trigger is gathered (CR 603.3) and resolved. vanishingSpec's helper.
      upkeepOf gs =
        let began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Beginning BeginningStep.Upkeep) S.alice)) (gs {GameState.phase = Phase.Beginning BeginningStep.Upkeep, GameState.activePlayer = S.alice})
            settled = snd (Engine.runGamePure S.identityAnswer began Engine.settleForPriority)
         in snd (Engine.runGamePure S.identityAnswer settled Engine.priorityLoop)
      -- The board Arcbound Overseer's "each creature you control with modular"
      -- reads: four creatures over that filter's three conjuncts. The Overseer
      -- itself (modular 6); alice's Worker (modular 1, the discriminator); an
      -- Icehide Golem alice controls, an artifact creature of the Overseer's own
      -- subtype differing from it in nothing the filter reads but the modular;
      -- and a Worker BOB controls. PLACED rather than cast, so rule 702.43a's
      -- entry replacement never runs and every pile below is this fixture's;
      -- each carries a distinct seeded pile, so a printed 0/0 survives CR 704.5f
      -- and no two numbers can be read for one another.
      familyBoard = do
        swamp <- S.printingOf s registry "Swamp"
        overseer <- S.printingOf s registry "Arcbound Overseer"
        worker <- S.printingOf s registry "Arcbound Worker"
        golem <- S.printingOf s registry "Icehide Golem"
        let (overseerId, g1) = S.addPermanent overseer S.alice (S.landsInPlay swamp 3)
            g2 = S.addCounter CounterKind.PlusOnePlusOne 3 overseerId g1
            (workerId, g3) = S.addPermanent worker S.alice g2
            g4 = S.addCounter CounterKind.PlusOnePlusOne 1 workerId g3
            (golemId, g5) = S.addPermanent golem S.alice g4
            g6 = S.addCounter CounterKind.PlusOnePlusOne 7 golemId g5
            (theirsId, g7) = S.addPermanent worker S.bob g6
            g8 = S.addCounter CounterKind.PlusOnePlusOne 5 theirsId g7
            -- CR 104.3c: the loop below runs past the upkeep, so both seats need
            -- more library than they draw.
            stock pid g = List.foldl' (\h _ -> snd (S.addLibraryCard swamp pid h)) g [1 .. 5 :: Int]
        pure (overseerId, workerId, golemId, theirsId, stock S.bob (stock S.alice g8))
   in Spec.describe s "Modular" $ do
        -- Rule 702.43a's FIRST ability, at both printed values: the N is the
        -- card's and not the rule's, so one leg alone could not tell a mint that
        -- always placed one counter from a correct one.
        Spec.it s "CR 702.43a the entry places the printed N of +1/+1 counters" $ do
          (foundWorker, workerBoard) <- castOne "Arcbound Worker" 1
          case foundWorker of
            Nothing -> Spec.assertFailure s "Arcbound Worker did not reach the battlefield"
            Just worker -> do
              Spec.assertEqWith s "modular 1 enters with one counter" (plusOnes worker workerBoard) 1
              -- CR 122.1a at layer 7c, which is also why a printed 0/0 survives
              -- CR 704.5f at all.
              Spec.assertEqWith s "so the printed 0/0 is a 1/1" (S.powerToughnessOf worker workerBoard) (Just (1, 1))
          (foundHybrid, hybridBoard) <- castOne "Arcbound Hybrid" 4
          case foundHybrid of
            Nothing -> Spec.assertFailure s "Arcbound Hybrid did not reach the battlefield"
            Just hybrid -> do
              Spec.assertEqWith s "modular 2 enters with two" (plusOnes hybrid hybridBoard) 2
              Spec.assertEqWith s "a 2/2" (S.powerToughnessOf hybrid hybridBoard) (Just (2, 2))
        -- CR 608.2h in isolation, counterLookBackSpec's third case in the payload
        -- rather than in an intervening "if": the record is emptied while the
        -- trigger sits on the stack, which no rule can do to last known
        -- information -- so only a payload that really reads it notices.
        Spec.it s "CR 608.2h the count comes from the last known record, not from the board" $ do
          (hybridId, workerId, gs) <- board "Arcbound Worker"
          let (settled, _) = murderIt exercising gs
              forgotten =
                settled
                  { GameState.lastKnown =
                      Map.adjust (\lk -> lk {LastKnown.counters = Map.empty}) hybridId (GameState.lastKnown settled)
                  }
              after = S.runPure exercising forgotten Stack.resolveTop
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "and resolved off it" (GameState.stack after) []
          Spec.assertEqWith s "and moved nothing, the record being empty" (plusOnes workerId after) 1
        -- The FAMILY, rule 702.43a's N dropped: Arcbound Overseer {8} Artifact
        -- Creature -- Golem 0/0, "At the beginning of your upkeep, put a +1/+1
        -- counter on each creature you control with modular. / Modular 6"
        -- (Oracle verified 2026-09-18), whose filter is Filter.HasKeywordFamily
        -- rather than Filter.HasKeyword. The Worker's modular 1 is the
        -- discriminator: an implementation matching the WRITTEN keyword reaches
        -- only the Overseer's own modular 6.
        Spec.it s "CR 702.43a whole card: Arcbound Overseer's upkeep trigger reads modular as a family, not as a number" $ do
          (overseerId, workerId, golemId, theirsId, gs) <- familyBoard
          let after = upkeepOf gs
          Spec.assertEqWith s "the Worker's modular 1 is modular all the same, so it is up to two" (plusOnes workerId after) 2
          Spec.assertEqWith s "and a 2/2" (S.powerToughnessOf workerId after) (Just (2, 2))
          Spec.assertEqWith s "the Overseer's own modular 6 matches too, four from three" (plusOnes overseerId after) 4
          Spec.assertEqWith s "the Icehide Golem has no modular, so it keeps its seven" (plusOnes golemId after) 7
          Spec.assertEqWith s "and bob's Worker is no creature alice controls" (plusOnes theirsId after) 5
        -- CR 702.44c: Arcbound Wanderer {6} Artifact Creature -- Golem 0/0,
        -- "Modular--Sunburst" (Oracle verified 2026-09-30). Six mana of FOUR
        -- colours, so the count is neither the mana spent (6) nor a printed N;
        -- then the death trigger moves those four onto a Worker holding one.
        Spec.it s "CR 702.44c whole card: Arcbound Wanderer enters with a counter per colour and moves them on death" $ do
          (found, entered) <- castWanderer [("Plains", 1), ("Island", 1), ("Mountain", 1), ("Swamp", 3)]
          swamp <- S.printingOf s registry "Swamp"
          murder <- S.printingOf s registry "Murder"
          worker <- S.printingOf s registry "Arcbound Worker"
          case found of
            Nothing -> Spec.assertFailure s "Arcbound Wanderer did not reach the battlefield"
            Just wandererId -> do
              Spec.assertEqWith s "four colours spent, four +1/+1 counters" (plusOnes wandererId entered) 4
              Spec.assertEqWith s "so the printed 0/0 is a 4/4" (S.powerToughnessOf wandererId entered) (Just (4, 4))
              -- Added AFTER the Wanderer entered, so it holds the greater
              -- ObjectId and Murder's least recipient is the Wanderer.
              let (workerId, g1) = S.addPermanent worker S.alice (S.landsFor swamp S.alice 3 entered)
                  g2 = S.addCounter CounterKind.PlusOnePlusOne 1 workerId g1
                  stocked = List.foldl' (\g _ -> snd (S.addLibraryCard swamp S.alice g)) g2 [1 .. 5 :: Int]
                  (settled, after) = murderIt exercising (S.handOne murder stocked)
              Spec.assertBool s (not (S.onBattlefield wandererId after)) "the Wanderer died"
              Spec.assertEqWith s "the Worker is up to five, one plus the Wanderer's four" (plusOnes workerId after) 5
              Spec.assertEqWith s "so it is a 5/5" (S.powerToughnessOf workerId after) (Just (5, 5))
              Spec.assertEqWith s "the death trigger reached the stack" (length (GameState.stack settled)) 1
        -- The pair for the case above: six mana of ONE colour is one counter,
        -- CR 702.44a's colours rather than mana.
        Spec.it s "CR 702.44c six Swamps are one colour, so one counter" $ do
          (found, entered) <- castWanderer [("Swamp", 6)]
          case found of
            Nothing -> Spec.assertFailure s "Arcbound Wanderer did not reach the battlefield"
            Just wandererId -> do
              Spec.assertEqWith s "one +1/+1 counter" (plusOnes wandererId entered) 1
              Spec.assertEqWith s "a 1/1" (S.powerToughnessOf wandererId entered) (Just (1, 1))
        -- CR 702.43b: each instance works separately. Asserted of BOTH mints,
        -- vanishing's position, no printing in the pool carrying modular twice.
        -- Spelled out rather than compared against Keyword.modular itself, for
        -- vanishingSpec's reason.
        Spec.it s "CR 702.43b each instance is its own two abilities" $ do
          Spec.assertEqWith
            s
            "modular 2 held twice mints two death triggers"
            (fmap TriggeredAbility.condition (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Modular (Just 2)) 2)))
            [TriggerCondition.SelfDies, TriggerCondition.SelfDies]
          Spec.assertEqWith
            s
            "and two entry rewrites of two counters each, which is what makes them add up"
            (Keyword.mintedReplacementsFor (Keyword.Type.Modular (Just 2)) 2)
            (replicate 2 (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource (EntryRewrite.WithCounters (WithCounters.one CounterKind.PlusOnePlusOne (Quantity.Type.Literal 2))))))

-- CR 603.2c's FIRST sentence on the same event: "whenever ONE OR MORE artifact
-- creatures you control deal combat damage to a player" fires once for the CR
-- 510.2 step however many connected --
-- TriggerCondition.PermanentsDealCombatDamageToPlayer, the batch twin of the
-- condition data/scenarios/counter-keyword-trigger proves.
--
-- Pia Nalaar, Chief Mechanic {G}{U}{R} Legendary Creature -- Human Artificer 2/4
-- is the card (data/cards/pia-nalaar-chief-mechanic.json): "Whenever one or more
-- artifact creatures you control deal combat damage to a player, you get {E}{E}."
-- Name, cost, type line and Oracle text checked against Scryfall 2026-09-06.
-- Her second ability -- "at the beginning of your end step, you may pay one or
-- more {E}. If you do, create an X/X colorless Vehicle artifact token named
-- Nalaar Aetherjet with flying and crew 2, where X is the amount of {E} paid
-- this way" -- is the last group below, and is a token whose printed box reads
-- an amount an earlier effect of the same resolution bound (CR 111.3).
--
-- The discrimination is the energy count with two artifact creatures connecting:
-- {E}{E} is the batch reading, {E}{E}{E}{E} is the per-damager one Tovolar's
-- condition would give, and none is silence. Pia attacks too and is no artifact,
-- which is the Filter doing its work inside the same batch. The pool's artifact
-- creatures are Palladium Myr (2/2) and Spined Thopter (2/1 flying).
--
-- What makes the two connections ONE trigger event is Pawl.Engine.Damage.dealWave
-- bracketing the step as one Pawl.Types.EventGroup (CR 510.2), which the group
-- count pins as the precondition: were each DamageDealt its own group, "once per
-- group" and "once per damager" would coincide and the {E}{E} would prove nothing.
piaNalaarSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
piaNalaarSpec s registry =
  let board mine = do
        ours <- mapM (S.printingOf s registry) mine
        let (gs, _, _) = S.combatBoardOf ours []
        pure (S.runCombat S.aggressiveAnswer gs)
      energy = S.playerCounterOf PlayerCounterKind.Energy S.alice
      -- The distinct EventGroups the log's combat damage carries.
      combatDamageGroups gs =
        Set.fromList
          ( Maybe.mapMaybe
              ( \logged -> case LoggedEvent.event logged of
                  GameEvent.DamageDealt ev | DamageEvent.kind ev == DamageKind.Combat -> Just (LoggedEvent.group logged)
                  _ -> Nothing
              )
              (Foldable.toList (GameState.events gs))
          )
   in Spec.describe s "Batched combat damage" $ do
        -- The proving test: two artifact creatures and Pia connect in one step.
        Spec.it s "CR 603.2c two artifact creatures connecting in one step give {E}{E}, not {E}{E}{E}{E}" $ do
          after <- board ["Pia Nalaar, Chief Mechanic", "Palladium Myr", "Spined Thopter"]
          Spec.assertEqWith s "alice got {E}{E} for the step, not {E}{E}{E}{E}" (energy after) 2
          Spec.assertEqWith s "all three connected, for six" (S.lifeOf S.bob after) (Just 14)
          Spec.assertEqWith s "CR 510.2: the three damage events were one event group" (Set.size (combatDamageGroups after)) 1
        -- Her SECOND ability, which is CR 111.3's defined characteristic value
        -- read off a binding: the token's printed box is a Quantity.InSlot
        -- naming the amount Effect.PayAnyEnergy bound one clause earlier.
        --
        -- The board separates every other reading of "the amount of {E} paid
        -- this way": alice banks five (two off the combat trigger and three
        -- placed) and pays THREE, so 5 is what she had, 2 is what she has left,
        -- 2/4 is Pia's own box, and only 3 is what she paid.
        --
        -- CREWED before the box is read, because CR 208.3 gives a noncreature
        -- permanent no power or toughness however its printed box reads -- so an
        -- uncrewed Vehicle answers Nothing under every value of X and could not
        -- tell them apart. The Hill Giant is the crewer, placed untapped after
        -- the combat that tapped everything else alice controls.
        Spec.it s "CR 111.3 the Aetherjet's X/X is the energy PAID, not the energy held" $ do
          after <- board ["Pia Nalaar, Chief Mechanic", "Palladium Myr", "Spined Thopter"]
          giant <- S.printingOf s registry "Hill Giant"
          let resolved = payAtEndStep 3 (S.addPlayerCounter PlayerCounterKind.Energy 3 S.alice after)
              (_, withCrewer) = S.addPermanent giant S.alice resolved
          case aetherjetIds resolved of
            [jet] -> do
              let crewed = crewWith S.identityAnswer jet (withCrewer {GameState.priority = Just S.alice})
              Spec.assertEqWith s "crewed, a 3/3: three {E} were paid of the five she held" (S.powerToughnessOf jet crewed) (Just (3, 3))
              Spec.assertEqWith s "CR 208.3 and uncrewed it had none, whatever its box said" (S.powerToughnessOf jet resolved) Nothing
            other -> Spec.assertFailure s ("expected exactly one Aetherjet, got " <> show (length other))
          Spec.assertEqWith s "and the three really left her pool" (energy resolved) 2
        -- The twin, differing in the two {E} alone: the same trigger on the same
        -- board is a real choice, so it IS raised, and the amount it binds is the
        -- token's box. Whether the box reads the energy paid or the energy held
        -- is the neighbouring case's discrimination, not this one's.
        Spec.it s "CR 107.14 two {E} makes it a real choice, and the Aetherjet is a 2/2" $ do
          after <- board ["Pia Nalaar, Chief Mechanic"]
          giant <- S.printingOf s registry "Hill Giant"
          let (resolved, asked) = payAtEndStepCounting 2 (S.addPlayerCounter PlayerCounterKind.Energy 2 S.alice after)
              (_, withCrewer) = S.addPermanent giant S.alice resolved
          case aetherjetIds resolved of
            [jet] -> do
              let crewed = crewWith S.identityAnswer jet (withCrewer {GameState.priority = Just S.alice})
              Spec.assertEqWith s "crewed, a 2/2: both {E} were paid" (S.powerToughnessOf jet crewed) (Just (2, 2))
            other -> Spec.assertFailure s ("expected exactly one Aetherjet, got " <> show (length other))
          Spec.assertEqWith s "and the two really left her pool" (energy resolved) 0
          Spec.assertEqWith s "she was asked exactly once" asked 1

-- Pia's end-step trigger, driven to its answer: alice's end step begins, the
-- trigger is put on the stack, and it resolves with `n` paid to
-- Prompt.ChoosePaidEnergy. The amount is PINNED rather than left to
-- S.identityAnswer, which answers the same way on both boards below and so
-- could not tell paying three from declining.
payAtEndStep :: Natural -> GameState.GameState -> GameState.GameState
payAtEndStep n = fst . payAtEndStepCounting n

-- The same drive, also reporting how many times Prompt.ChoosePaidEnergy was
-- raised. A pure answerer cannot say whether it was called, and two boards that
-- differ only in whether the question was worth asking look alike on the board
-- itself, so the count is threaded through State the way Pawl.ManaSpec's
-- countingAnswer does.
payAtEndStepCounting :: Natural -> GameState.GameState -> (GameState.GameState, Int)
payAtEndStepCounting n gs =
  let endStep = Phase.Ending EndingStep.EndStep
      begun = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)) (gs {GameState.phase = endStep})
      counting :: Prompt.Prompt r -> State.State Int r
      counting p = case p of
        Prompt.ChoosePaidEnergy {} -> do
          State.modify' (+ 1)
          pure n
        _ -> pure (S.identityAnswer p)
      driven = do
        (_, settled) <- Engine.runGame counting begun Engine.settleForPriority
        (_, resolved) <- Engine.runGame counting settled Stack.resolveTop
        pure (S.settleSba resolved)
   in State.runState driven 0

-- The tokens named for Pia's Vehicle. By NAME rather than by S.tokensOf alone,
-- so a token minted by anything else could not be mistaken for hers.
aetherjetIds :: GameState.GameState -> [ObjectId.ObjectId]
aetherjetIds gs =
  filter
    (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Nalaar Aetherjet")))
    (S.tokensOf gs)

-- What the filtered condition is FOR: a payload that aims at the creature that
-- dealt the damage (Pawl.Engine.Binding.combatDamager) rather than at the bearer
-- -- Aragorn, Hornburg Hero {1}{R}{G}{W} Legendary Creature -- Human Soldier 4/4,
-- "attacking creatures you control have first strike and renown 1" and "whenever
-- a renowned creature you control deals combat damage to a player, double the
-- number of +1/+1 counters on it".
--
-- Three capabilities meet here, and each has a way to fail that the counts below
-- tell apart: the slot naming the damager (aim it at the source and Aragorn takes
-- the counters), Quantity.AgainstSlot reading the damager's counters (read the
-- source's and the number is 0), and Filter.HasDesignation rejecting a candidate that
-- is not renowned yet (drop it and the Piker doubles too).
--
-- Aurelia, the Warleader supplies the second combat phase, as she does in
-- renownSpec: the doubling needs a creature that was ALREADY renowned when it
-- connected, and CR 603.2 checks this condition against the damage event itself,
-- where renown's own counters arrive only as ITS trigger resolves -- so one
-- connection can never both renown a creature and double it.
--
-- The doubling itself is CR 701.10e, and this group is what proves it: "as many
-- of those counters as that player or permanent already has" is the 2 -> 4 below.
-- The rule's sentence is about A kind, the one PutCounters carries. A card's
-- "each kind of counter" quantifies over kinds instead, which Pawl.PutCounterSpec
-- proves with Vorel of the Hull Clade.
--
-- Aragorn arrives BETWEEN the two combats so the Maulers takes its two counters
-- from printed renown 2 alone: with him out on the first swing the Maulers would
-- hold renown 2 and a granted renown 1 at once, and CR 702.112c leaves which
-- resolves first to its controller.
aragornSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
aragornSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      plan :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      plan attackers p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        _ -> S.aggressiveAnswer p
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
      renownedness oid gs = fmap (Set.member Designation.Renowned . Object.designations) (Game.lookupObject oid gs)
   in Spec.describe s "Doubling a damager's counters" $ do
        -- The proving test. 2 -> 4 rather than 2 -> 3, which is what separates
        -- "double" from "add one", and 4 rather than 2, which is what separates
        -- reading the damager's counters from reading the bearer's.
        Spec.it s "CR 702.112b whole card: a renowned creature's counters double when it connects" $ do
          (gs, mine, _) <- board ["Rhox Maulers", "Aurelia, the Warleader", "Goblin Piker"] []
          aragorn <- S.printingOf s registry "Aragorn, Hornburg Hero"
          case mine of
            [maulers, aurelia, piker] -> do
              let first = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (plan [maulers, aurelia]) gs
                  (hero, staged) = S.addPermanent aragorn S.alice first
                  loaded = S.addCounter CounterKind.PlusOnePlusOne 3 piker staged
                  second = S.runToStep (Phase.Combat CombatStep.DeclareAttackers) (plan [maulers, piker]) loaded
                  after = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (plan [maulers, piker]) second
              Spec.assertEqWith s "the first combat connected for seven" (S.lifeOf S.bob first) (Just 13)
              Spec.assertEqWith s "renown 2 alone renowned the Maulers" (countersOn maulers first) (Map.singleton CounterKind.PlusOnePlusOne 2)
              Spec.assertEqWith s "the second phase really ran a declaration" (GameState.phase second) (Phase.Combat CombatStep.DeclareAttackers)
              Spec.assertEqWith s "and the second combat connected for eleven" (S.lifeOf S.bob after) (Just 2)
              Spec.assertEqWith s "so the Maulers doubled to four, not three" (countersOn maulers after) (Map.singleton CounterKind.PlusOnePlusOne 4)
              Spec.assertEqWith s "making it an 8/8" (S.powerToughnessOf maulers after) (Just (8, 8))
              -- CR 702.112b's designation read of a CANDIDATE: the Piker carries
              -- three counters from outside renown and was NOT renowned when it
              -- dealt its damage, so Aragorn's ability never triggered for it. The
              -- fourth counter is the renown 1 he granted it, which becomes
              -- renowned only as that trigger resolves -- 4 rather than the 7 a
              -- filter without the designation conjunct would leave.
              Spec.assertEqWith s "the Piker was renowned by that same damage" (renownedness piker after) (Just True)
              Spec.assertEqWith s "but took one counter rather than doubling" (countersOn piker after) (Map.singleton CounterKind.PlusOnePlusOne 4)
              -- The slot: had the payload aimed at CR 113.7a's source, these
              -- counters would be here instead.
              Spec.assertEqWith s "and Aragorn himself took none" (countersOn hero after) Map.empty
            _ -> Spec.assertFailure s "fixture should give alice a Maulers, an Aurelia and a Piker"

-- CR 702.25a's flanking, which rule 702 states as a triggered
-- ability, and with it CR 509.3d -- "becomes blocked by a creature", the one
-- block-trigger form that fires once per BLOCKER and names it.
--
-- Benalish Cavalry {1}{W} Creature -- Human Knight 2/2 is the card: flanking and
-- nothing else, so every number below is the keyword's. Its blockers are drawn
-- from the pool's vanilla creatures for the same reason.
--
-- Every reading is taken at the COMBAT DAMAGE step, before damage is dealt (CR
-- 509.2a puts these triggers on the stack in the declare blockers step), so the
-- -1/-1 is read directly rather than through what survives combat.
flankingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
flankingSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) S.aggressiveAnswer
   in Spec.describe s "Flanking" $ do
        -- The proving test, and its control: the same 2/2 attacker WITHOUT
        -- flanking (Icehide Golem) against the same 2/1 blocker. The flanker's
        -- Piker is 1/0 and already dead when damage would be dealt, so the
        -- flanker survives; the Golem trades with it.
        Spec.it s "CR 702.25a whole card: the blocking Piker is -1/-1 and dies before damage" $ do
          (gs, mine, theirs) <- board ["Benalish Cavalry"] ["Goblin Piker"]
          (control, controlMine, controlTheirs) <- board ["Icehide Golem"] ["Goblin Piker"]
          case (mine, theirs, controlMine, controlTheirs) of
            ([cavalry], [piker], [golem], [otherPiker]) -> do
              let struck = atDamage gs
                  traded = S.runCombat S.aggressiveAnswer gs
                  controlStruck = atDamage control
                  controlTraded = S.runCombat S.aggressiveAnswer control
              Spec.assertBool s (not (S.onBattlefield piker struck)) "the 2/1 Piker went to 1/0 and CR 704.5f buried it"
              Spec.assertEqWith s "the Cavalry itself is untouched" (S.powerToughnessOf cavalry struck) (Just (2, 2))
              Spec.assertBool s (S.onBattlefield cavalry traded) "so nothing was left to deal it damage"
              Spec.assertEqWith s "control leg: a 2/2 without flanking leaves the Piker at 2/1" (S.powerToughnessOf otherPiker controlStruck) (Just (2, 1))
              Spec.assertBool s (not (S.onBattlefield golem controlTraded)) "and the Piker's 2 kills it"
              Spec.assertBool s (not (S.onBattlefield otherPiker controlTraded)) "both die, where the flanker died alone"
            _ -> Spec.assertFailure s "fixture should give each seat one creature"
        -- CR 702.25a's "without flanking", read as CR 509.3f asks -- the blocker's
        -- characteristics as it becomes a blocking creature. A second Benalish
        -- Cavalry blocking is 2/2 still; the Icehide Golem, the same 2/2 without
        -- the keyword, is 1/1. The two boards differ in nothing else.
        Spec.it s "CR 702.25a a blocker WITH flanking is spared and one without is not" $ do
          (withIt, _, theirs) <- board ["Benalish Cavalry"] ["Benalish Cavalry"]
          (without, _, others) <- board ["Benalish Cavalry"] ["Icehide Golem"]
          case (theirs, others) of
            ([blockingCavalry], [golem]) -> do
              Spec.assertEqWith s "the flanking blocker is untouched" (S.powerToughnessOf blockingCavalry (atDamage withIt)) (Just (2, 2))
              Spec.assertEqWith s "the one without takes -1/-1" (S.powerToughnessOf golem (atDamage without)) (Just (1, 1))
            _ -> Spec.assertFailure s "fixture should give bob one blocker on each board"
        -- CR 702.25b: each instance triggers separately, which is abilitiesFor's
        -- replicate. No card in the pool prints flanking twice, so this is
        -- asserted of the MINT rather than of a board -- as bushido's, prowess'
        -- and battle cry's are.
        Spec.it s "CR 702.25b two instances mint two abilities, both CR 509.3d" $ do
          let abilities = Keyword.abilitiesFor Keyword.Type.Flanking 2
              expected =
                TriggerCondition.SelfBecomesBlockedBy
                  (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not (Filter.Type.HasKeyword Keyword.Type.Flanking)])
          Spec.assertEqWith s "two abilities" (length abilities) 2
          Spec.assertEqWith s "each watching CR 509.3d, filtered on the blocker's own flanking" (fmap TriggeredAbility.condition abilities) [expected, expected]

-- CR 702.45a: "'Bushido N' means 'Whenever this creature blocks or becomes
-- blocked, it gets +N/+N until end of turn.'" Rule 702 states it as a triggered
-- ability, as it does CR 702.70a, CR 702.86a, CR
-- 702.91a and CR 702.108a, and it is the only one that names TWO events: "blocks" is
-- CR 509.3a and "becomes blocked" is CR 509.3c, so Pawl.Engine.Keyword.bushido
-- mints two abilities and the two cases below fire one each.
--
-- Inner-Chamber Guard, {1}{W} Creature -- Human Samurai 0/2 with bushido 2 and
-- nothing else. Chosen for its numbers: 0/2 becoming 2/4 is unmistakable, an
-- asymmetric base means no reading of the rule lands on the same pair, and
-- bushido 2 rather than 1 keeps +N/+N apart from a hardcoded +1/+1. Goblin Piker
-- 2/1 is the other side, and the two flip TOGETHER on the pump: at 2/4 the Guard
-- kills the Piker and lives, at 0/2 it kills nothing and dies. Those two survival
-- assertions are regression fences rather than proofs -- every mutation tried
-- against this group tripped the power/toughness assertion above them first --
-- and what they fence is the TIMING: a pump that landed after CR 510's damage
-- would leave both creatures where an unpumped Guard does.
bushidoSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bushidoSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- The board as combat damage is about to be dealt, so the pump is readable
      -- before it decides anything.
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) S.aggressiveAnswer
   in Spec.describe s "Bushido" $ do
        -- CR 509.3a's half, whole card: bob's Guard blocks alice's Piker.
        Spec.it s "CR 702.45a whole card: blocking makes Inner-Chamber Guard 2/4" $ do
          (gs, attackers, blockers) <- board ["Goblin Piker"] ["Inner-Chamber Guard"]
          case (attackers, blockers) of
            ([piker], [guard]) -> do
              let pumped = atDamage gs
                  fought = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "0/2 before blockers are declared" (S.powerToughnessOf guard gs) (Just (0, 2))
              Spec.assertEqWith s "and 2/4 once the trigger has resolved" (S.powerToughnessOf guard pumped) (Just (2, 4))
              Spec.assertBool s (not (S.onBattlefield piker fought)) "the pumped Guard's 2 killed the 2/1 Piker"
              Spec.assertBool s (S.onBattlefield guard fought) "and its 4 toughness survived the Piker's 2"
            _ -> Spec.assertFailure s "fixture should give alice a Piker and bob a Guard"
        -- CR 509.3c's half, the other arm of the same printed sentence: now the
        -- Guard is alice's and attacks, and bob's Piker blocks it. An
        -- implementation with only the CR 509.3a arm passes every case above and
        -- fails this one.
        Spec.it s "CR 702.45a whole card: becoming blocked makes Inner-Chamber Guard 2/4" $ do
          (gs, attackers, blockers) <- board ["Inner-Chamber Guard"] ["Goblin Piker"]
          case (attackers, blockers) of
            ([guard], [piker]) -> do
              let pumped = atDamage gs
                  fought = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "2/4 once the trigger has resolved" (S.powerToughnessOf guard pumped) (Just (2, 4))
              Spec.assertBool s (not (S.onBattlefield piker fought)) "the pumped Guard's 2 killed the 2/1 Piker"
              Spec.assertBool s (S.onBattlefield guard fought) "and its 4 toughness survived the Piker's 2"
            _ -> Spec.assertFailure s "fixture should give alice a Guard and bob a Piker"
        -- CR 702.45b: "If a creature has multiple instances of bushido, each
        -- triggers separately." Asked of the mint rather than of a board, as
        -- prowess' and battle cry's are: no card in this pool prints bushido twice
        -- and nothing here grants it. The count is FOUR rather than two because
        -- one instance is already two abilities -- rule 702.45a's one sentence,
        -- CR 509.3a's event and CR 509.3c's.
        Spec.it s "CR 702.45b each instance of bushido is its own ability" $ do
          Spec.assertEqWith s "bushido 2 held once is its two halves" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Bushido 2) 1)) (Keyword.bushido 2)
          Spec.assertEqWith s "and held twice is four abilities" (length (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Bushido 2) 2))) 4

-- CR 702.130a: "'Afflict N' means 'Whenever this creature becomes blocked,
-- defending player loses N life.'" Rule 702 states it as a triggered ability,
-- and it is the first to put CR 509.3c's event and CR
-- 508.5's defending player in one sentence.
--
-- Khenra Eternal {1}{B} Creature -- Zombie Jackal Warrior 2/2 with afflict 1 and
-- nothing else printed on it, so every number below is the keyword's.
--
-- THREE SEATS, for annihilatorSpec's reason: at two players "the defending
-- player" and "the attacker's one opponent" are the same seat.
--
-- Afflict 1 is the only N a card in this pool puts on a board, so no case below
-- can tell the keyword's N from a hardcoded 1. The mint inequality in the last
-- case is what does, and it is there for that and no other reason.
afflictSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
afflictSpec s _ =
  Spec.describe s "Afflict" $ do
    -- CR 603.2 through CR 508.5: the becomes-blocked event carries the
    -- defending player, and the scan stamps them under the reserved slot rule
    -- 702.130a's "defending player" reads. The falsifier is an arm that binds
    -- the attacking side, or none at all.
    Spec.it s "CR 603.2 the defending player rides the becomes-blocked event in the reserved slot" $ do
      let bindings = Event.eventBindings (Setup.emptyGame S.bothPlayers) Nothing Map.empty (ObjectId.MkObjectId 0) S.alice TriggerCondition.SelfBecomesBlocked (GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked (ObjectId.MkObjectId 9) S.carol 1))
      Spec.assertEqWith s "carol is bound under thatPlayer" (Binding.targetsOf bindings) (Map.singleton Binding.triggerPlayer (Set.singleton (Recipient.ToPlayer S.carol)))
    -- CR 702.130b: "If a creature has multiple instances of afflict, each
    -- triggers separately." Asked of the mint rather than of a board, as
    -- bushido's and prowess' are: no card in this pool prints afflict twice
    -- and nothing here grants it.
    --
    -- The inequality is the second half of the case and a separate claim: N
    -- reaches the minted ability at all. Afflict 1 is the only N a board in
    -- this pool can show, so nothing above would go red if the mint hardcoded
    -- its 1.
    Spec.it s "CR 702.130b each instance of afflict is its own ability, and N reaches it" $ do
      Spec.assertEqWith s "afflict 1 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Afflict 1) 2)) [Keyword.afflict 1, Keyword.afflict 1]
      Spec.assertEqWith s "and afflict 3 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Afflict 3) 1)) [Keyword.afflict 3]
      Spec.assertBool s (Keyword.afflict 1 /= Keyword.afflict 3) "and the two differ, so N is in the ability"

-- CR 702.121 melee, whose rule text is a triggered ability,
-- and the first whose payload is a number read off game state rather than a
-- literal, with Wings of the Guard ({1}{W} Creature -- Bird 1/1, flying and
-- melee, and nothing else).
--
-- THREE SEATS throughout, and here that is load-bearing rather than tidy: at two
-- players "each opponent you attacked" and "each opponent" are the same number,
-- so a bonus that ignored the combat record entirely would pass every case.
--
-- CR 802.2 is the default option (Setup.emptyGame), so both opponents defend; it
-- is the ANSWERERS here that aim every attack at one of them, which holds the
-- bonus to 0 or 1. What separates the two is CR 506.3's other attackable
-- permanents: a creature that attacked only a planeswalker attacked no opponent.
-- The split declaration, where the bonus is 2, is
-- data/scenarios/counter-keyword-trigger/cr-702-121a-attacking-both-opponents-makes-wings-of-the-guard-3-3.json.
meleeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
meleeSpec s registry =
  let -- Attacks `who` with everything, aiming every attack at the player.
      attacking :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      attacking who p = case p of
        Prompt.ChooseDefender {} -> who
        Prompt.ChooseAttackTarget {} -> S.attackTo who p
        _ -> S.aggressiveAnswer p
      -- alice fields the Bird (plus whatever else `mine` names); bob and carol
      -- each field a Goblin Piker, so either is a legal defending player.
      board mine = do
        wings <- S.printingOf s registry "Wings of the Guard"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (S.threePlayerCombat (wings : mine) [piker] [piker])
   in Spec.describe s "Melee" $ do
        -- CR 611.2d: the bonus is fixed as the ability resolves, and CR 511.3
        -- clears the combat record at end of combat -- so a pump that re-read the
        -- record live would shrink back to +0/+0 the moment combat ended, while
        -- the printed duration runs to end of turn.
        Spec.it s "CR 611.2d the +1/+1 outlives the combat record it was computed from" $ do
          (gs, ours, _, _) <- board []
          case ours of
            [wings] -> do
              let after = S.runToStep Phase.PostcombatMain (attacking S.bob) gs
              Spec.assertEqWith s "CR 511.3 the record is cleared" (Combat.Type.declaredAttacked (GameState.combat after)) Set.empty
              Spec.assertEqWith s "and the Bird is still a 2/2" (S.powerToughnessOf wings after) (Just (2, 2))
            _ -> Spec.assertFailure s "fixture should give alice one Bird"
        -- CR 702.121b: two instances are two abilities, so two triggers and two
        -- bonuses. Asserted at the mint, no card in the pool having melee twice.
        Spec.it s "CR 702.121b each instance triggers separately" $ do
          Spec.assertEqWith s "melee held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Melee 2)) [Keyword.melee, Keyword.melee]
          Spec.assertEqWith s "and once is one" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Melee 1)) [Keyword.melee]

-- CR 702.105 dethrone, whose CONDITION is the whole keyword -- the first minted
-- trigger narrowed by a fact about the game rather than about the declaration --
-- with Enraged Revolutionary ({2}{R} Creature -- Human Warrior 2/1, dethrone and
-- nothing else). The counter is read as power and toughness, so 2/1 is "did not
-- trigger" and 3/2 is "did".
--
-- THREE SEATS throughout, and load-bearing twice over: at two players "the player
-- with the most life" and "the defending player" coincide whenever the attacker's
-- controller is behind, and there is no second opponent to be the wrong one.
--
-- Life totals are all distinct except where a tie is the point, so no reading of
-- the rule produces the same board twice.
dethroneSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
dethroneSpec s _ =
  Spec.describe s "Dethrone" $ do
    -- CR 702.105b: two instances are two abilities, so two triggers and two
    -- counters. Asserted at the mint, no card in the pool having dethrone twice.
    Spec.it s "CR 702.105b each instance triggers separately" $ do
      Spec.assertEqWith s "dethrone held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Dethrone 2)) [Keyword.dethrone, Keyword.dethrone]
      Spec.assertEqWith s "and once is one" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Dethrone 1)) [Keyword.dethrone]

-- CR 702.23 rampage, whose rule text is a triggered ability,
-- and the first whose bonus multiplies a printed N by a number read off the
-- board.
--
-- Wolverine Pack {2}{G}{G} Creature -- Wolverine 2/4 is the card: rampage 2 and
-- nothing else. Its numbers are chosen so no two readings of rule 702.23a agree
-- -- an asymmetric 2/4 base, and N = 2 rather than 1, so "+N per blocker" (6/8 at
-- two blockers), "+1 per blocker beyond the first" (3/5) and the rule's own
-- reading (4/6) are three different pairs.
--
-- Horrible Hordes {3} Artifact Creature -- Spirit 2/2, rampage 1, is the second
-- producer and is what pins N: the same three blockers give it +2/+2 where the
-- Pack gets +4/+4.
--
-- Every reading but the last is taken at the COMBAT DAMAGE step, before damage is
-- dealt -- CR 509.2a puts the trigger on the stack in the declare blockers step,
-- so the bonus is already applied there.
rampageSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
rampageSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      noBlocks :: Prompt.Prompt r -> r
      noBlocks p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) S.aggressiveAnswer
   in Spec.describe s "Rampage" $ do
        -- CR 509.3c is the event, so an UNBLOCKED attacker never triggers at all.
        -- Asserted on the LOG and not on power and toughness, which cannot tell
        -- the two apart: a trigger that fired with no blockers would be +0/+0 and
        -- leave the same 2/4. The blocked leg is the same board with the block
        -- taken, so the pair differs in nothing but CR 509.1's declaration.
        Spec.it s "CR 509.3c an unblocked Pack never triggers" $ do
          (gs, mine, _) <- board ["Wolverine Pack"] ["Goblin Piker"]
          case mine of
            [pack] -> do
              let fired after =
                    not
                      ( null
                          ( Maybe.mapMaybe
                              ( \event -> case event of
                                  GameEvent.AbilityTriggered record
                                    | AbilityTriggered.source record == TriggerSource.OfObject pack,
                                      AbilityTriggered.controller record == S.alice,
                                      TriggeredAbility.condition (AbilityTriggered.ability record) == TriggerCondition.SelfBecomesBlocked ->
                                        Just ()
                                  _ -> Nothing
                              )
                              (S.eventsOf after)
                          )
                      )
              Spec.assertBool s (not (fired (S.runToStep (Phase.Combat CombatStep.CombatDamage) noBlocks gs))) "nothing blocked, so nothing triggered"
              Spec.assertBool s (fired (atDamage gs)) "and the same board with the block taken does trigger"
            _ -> Spec.assertFailure s "fixture should give alice one Pack"
        -- CR 702.23c: each instance triggers separately. Asserted at the MINT, no
        -- printing in the pool carrying rampage twice, and the second assertion is
        -- what puts N inside the ability rather than beside it.
        Spec.it s "CR 702.23c each instance triggers separately" $ do
          Spec.assertEqWith s "rampage 2 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Rampage 2) 2)) [Keyword.rampage 2, Keyword.rampage 2]
          Spec.assertEqWith s "and once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Rampage 2) 1)) [Keyword.rampage 2]
          Spec.assertBool s (Keyword.rampage 1 /= Keyword.rampage 2) "and the two differ, so N is in the ability"

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Trigger" $ do
  bushidoSpec s registry
  flankingSpec s registry
  renownSpec s registry
  arborColossusSpec s registry
  hydraBroodmasterSpec s registry
  vanishingSpec s registry
  numberlessVanishingSpec s registry
  fadingSpec s registry
  cumulativeUpkeepSpec s registry
  modularSpec s registry
  piaNalaarSpec s registry
  aragornSpec s registry
  afflictSpec s registry
  meleeSpec s registry
  dethroneSpec s registry
  rampageSpec s registry
