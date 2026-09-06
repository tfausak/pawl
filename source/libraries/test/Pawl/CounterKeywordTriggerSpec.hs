{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Trigger over keyword triggers that move counters or count damage
-- (CR 702): renown, vanishing, fading, cumulative upkeep, modular, the combat-
-- damage bracket, and flanking through rampage. Split out of
-- Pawl.KeywordTriggerSpec, which keeps the machinery.
module Pawl.CounterKeywordTriggerSpec where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import Pawl.CrewSpec (crewWith)
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
import qualified Pawl.Types.AttackTarget as AttackTarget
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
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Response as Response
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
      -- CR 508.1's declaration narrowed to the named creatures, trainingSpec's
      -- plan: S.aggressiveAnswer attacks with everything, so a case about who
      -- attacks in which phase has to say so.
      plan :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      plan attackers p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        _ -> S.aggressiveAnswer p
      -- CR 122.1: what is actually on the permanent, which a +2/+2 EFFECT would
      -- leave empty while reading the same 6/6.
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
      -- CR 702.112b's designation itself, which no characteristic reports.
      renownedness oid gs = fmap (Set.member Designation.Renowned . Object.designations) (Game.lookupObject oid gs)
      -- CR 104.3c: a draw case needs a library to draw from, and more of one than
      -- it draws, so an extra draw is visible rather than fatal.
      stock printing n pid gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 .. (n :: Int)]
      -- CR 509.1: no blocks. S.aggressiveAnswer blocks with everything, which
      -- would put the defender's own watcher in front of an attacker.
      noBlocks :: Prompt.Prompt r -> r
      noBlocks p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
   in Spec.describe s "Renown" $ do
        -- The proving test. CR 702.112a: two counters on the BEARER, and the
        -- designation with them. The counter assertion is what separates rule
        -- 702.112a's placement from a pump, and the 2 what separates N from 1.
        Spec.it s "CR 702.112a whole card: Rhox Maulers connects and takes two +1/+1 counters" $ do
          (gs, mine, _) <- board ["Rhox Maulers"] []
          case mine of
            [maulers] -> do
              let after = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "bob took the printed four" (S.lifeOf S.bob after) (Just 16)
              Spec.assertEqWith s "two counters, not one" (countersOn maulers after) (Map.singleton CounterKind.PlusOnePlusOne 2)
              Spec.assertEqWith s "so it is a 6/6" (S.powerToughnessOf maulers after) (Just (6, 6))
              Spec.assertEqWith s "and it is renowned" (renownedness maulers after) (Just True)
            _ -> Spec.assertFailure s "fixture should give alice a Rhox Maulers"
        -- CR 702.112a is scoped to combat damage dealt TO A PLAYER. The 1/4
        -- absorbs all four (CR 702.19b leaves nothing to trample over), so the
        -- event never happens and neither half of the ability runs.
        Spec.it s "CR 702.112a a fully blocked Maulers is renowned by nobody" $ do
          (gs, mine, _) <- board ["Rhox Maulers"] ["Apprentice Sharpshooter"]
          case mine of
            [maulers] -> do
              let after = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "bob lost no life" (S.lifeOf S.bob after) (Just 20)
              Spec.assertEqWith s "no counters" (countersOn maulers after) Map.empty
              Spec.assertEqWith s "and no designation" (renownedness maulers after) (Just False)
            _ -> Spec.assertFailure s "fixture should give alice a Rhox Maulers"
        -- CR 603.4's intervening "if", at the board level: a second connection in
        -- the same turn finds the creature already renowned, so nothing is added.
        -- Aurelia, the Warleader is the pool's extra combat phase, and she untaps
        -- the Maulers to attack again. The life drop is the discriminator -- it
        -- proves the second combat really connected, so a green assertion cannot
        -- mean the phase never ran.
        Spec.it s "CR 702.112a a second connection adds nothing, the creature being renowned" $ do
          (gs, mine, _) <- board ["Rhox Maulers", "Aurelia, the Warleader"] []
          case mine of
            [maulers, aurelia] -> do
              let first = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (plan [maulers, aurelia]) gs
                  second = S.runToStep (Phase.Combat CombatStep.DeclareAttackers) (plan [maulers]) first
                  after = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (plan [maulers]) second
              Spec.assertEqWith s "the first combat renowned it" (countersOn maulers first) (Map.singleton CounterKind.PlusOnePlusOne 2)
              Spec.assertEqWith s "the second phase really ran a declaration" (GameState.phase second) (Phase.Combat CombatStep.DeclareAttackers)
              Spec.assertEqWith s "and the second really connected, for six" (S.lifeOf S.bob after) (Just 7)
              Spec.assertEqWith s "but added no third counter" (countersOn maulers after) (Map.singleton CounterKind.PlusOnePlusOne 2)
            _ -> Spec.assertFailure s "fixture should give alice a Maulers and Aurelia"
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
        -- CR 702.112b's designation read by a WATCHER, which is what the rule
        -- calls it a marker FOR: Valeron Wardens {2}{G} Creature -- Human Monk
        -- 1/3, renown 2 and "whenever a creature you control becomes renowned,
        -- draw a card". Both attackers connect, so the Wardens' trigger fires
        -- TWICE -- once for the Maulers and once for itself, which is what "a
        -- creature you control" says and a self-scoped reading would not.
        --
        -- The library is stocked past the two draws, so a third draw would show as
        -- an extra card rather than as CR 104.3c losing alice the game before the
        -- assertions run.
        Spec.it s "CR 702.112b a watcher draws once per creature that becomes renowned" $ do
          (gs, mine, _) <- board ["Valeron Wardens", "Rhox Maulers"] []
          piker <- S.printingOf s registry "Goblin Piker"
          case mine of
            [wardens, maulers] -> do
              let after = S.runCombat S.aggressiveAnswer (stock piker 3 S.alice gs)
              Spec.assertEqWith s "both connected, for five" (S.lifeOf S.bob after) (Just 15)
              Spec.assertEqWith s "the Wardens is renowned" (renownedness wardens after) (Just True)
              Spec.assertEqWith s "and so is the Maulers" (renownedness maulers after) (Just True)
              Spec.assertEqWith s "so two cards were drawn, not one" (length (Game.zoneMembers Zone.Hand S.alice after)) 2
              Spec.assertEqWith s "leaving one in the library" (length (Game.zoneMembers Zone.Library S.alice after)) 1
            _ -> Spec.assertFailure s "fixture should give alice a Wardens and a Maulers"
        -- What the condition is NOT: combat damage. Goblin Piker connects for two
        -- and has no renown, so it never becomes renowned and contributes no draw
        -- -- the one card is the Wardens' own designation.
        Spec.it s "CR 702.112b a creature that connects without renown draws nothing" $ do
          (gs, mine, _) <- board ["Valeron Wardens", "Goblin Piker"] []
          piker <- S.printingOf s registry "Goblin Piker"
          case mine of
            [wardens, goblin] -> do
              let after = S.runCombat S.aggressiveAnswer (stock piker 3 S.alice gs)
              Spec.assertEqWith s "both connected, for three" (S.lifeOf S.bob after) (Just 17)
              Spec.assertEqWith s "the Wardens is renowned" (renownedness wardens after) (Just True)
              Spec.assertEqWith s "the Piker is not" (renownedness goblin after) (Just False)
              Spec.assertEqWith s "so exactly one card was drawn" (length (Game.zoneMembers Zone.Hand S.alice after)) 1
            _ -> Spec.assertFailure s "fixture should give alice a Wardens and a Piker"
        -- CR 109.5's "you control", and with it WHICH permanent the Filter reads:
        -- bob has a Valeron Wardens of his own, watching from the defending side.
        -- Nothing he controls becomes renowned, so he draws nothing -- an arm that
        -- read the BEARER instead of the event's subject would have his Wardens
        -- match itself and draw twice.
        Spec.it s "CR 702.112b the defender's own Wardens sees no creature of his become renowned" $ do
          (gs, mine, theirs) <- board ["Valeron Wardens", "Rhox Maulers"] ["Valeron Wardens"]
          piker <- S.printingOf s registry "Goblin Piker"
          case (mine, theirs) of
            ([wardens, maulers], [hisWardens]) -> do
              let after = S.runCombat noBlocks (stock piker 3 S.bob (stock piker 3 S.alice gs))
              Spec.assertEqWith s "both of alice's connected, for five" (S.lifeOf S.bob after) (Just 15)
              Spec.assertEqWith s "hers are renowned" (fmap (`renownedness` after) [wardens, maulers]) [Just True, Just True]
              Spec.assertEqWith s "his is not" (renownedness hisWardens after) (Just False)
              Spec.assertEqWith s "she drew two" (length (Game.zoneMembers Zone.Hand S.alice after)) 2
              Spec.assertEqWith s "and he drew none" (length (Game.zoneMembers Zone.Hand S.bob after)) 0
            _ -> Spec.assertFailure s "fixture should give alice a Wardens and a Maulers, bob a Wardens"
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
      monstrosity colossus victim gs = case Activate.abilitiesFor colossus gs of
        [ability]
          -- CR 701.37a's condition is the CLAUSE's, not an activation
          -- restriction, so a monstrous permanent's ability stays activatable --
          -- which is what makes the second case below a real activation rather
          -- than an unpaid one.
          | Activate.activatable S.alice colossus ability gs ->
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
        -- The designation is LOAD-BEARING in the CLAUSE CONDITION too, and one board
        -- can carry two designations at once: Rune-Brand Juggler {2}{B}{R} 3/3,
        -- "When this creature enters, suspect up to one target creature you control",
        -- aimed at the Colossus. CR 701.60b's mark is not CR 701.37b's, so CR
        -- 701.37a's "if this permanent isn't monstrous" still holds and monstrosity
        -- still does its whole job. A Quantity arm that read "has SOME designation"
        -- would fail the condition and put nothing on the Colossus at all.
        Spec.it s "CR 701.37a a suspected Colossus is still not monstrous" $ do
          jugglerPrinting <- S.printingOf s registry "Rune-Brand Juggler"
          (colossus, maiden, _, gs) <- board pure
          let (_, entering) = S.entersWithTrigger jugglerPrinting S.alice gs
              suspected = snd (Engine.runGamePure (aimed colossus) entering Engine.priorityLoop)
          Spec.assertBool s (Set.member Designation.Suspected (maybe Set.empty Object.designations (Game.lookupObject colossus suspected))) "the Juggler suspected the Colossus"
          Spec.assertEqWith s "which leaves it not monstrous" (monstrousness colossus suspected) (Just False)
          case monstrosity colossus maiden suspected of
            Left n -> Spec.assertFailure s ("expected one activatable monstrosity ability, got " <> show n)
            Right after -> do
              Spec.assertEqWith s "so monstrosity still places its three counters" (plusOnes colossus after) 3
              Spec.assertEqWith s "and still marks it monstrous" (monstrousness colossus after) (Just True)
              Spec.assertBool s (not (S.onBattlefield maiden after)) "and its trigger still fired"
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
      after pid gs = snd (upkeepOf pid gs)
      times = S.counterOf CounterKind.Time
      -- The wurm CAST rather than placed, because rule 702.63a's first ability is
      -- a replacement on the entry -- S.addPermanent builds the object directly and
      -- so reaches no CR 616.1 loop, which is what the counterless case below
      -- turns on.
      castWurm = do
        swamp <- S.printingOf s registry "Swamp"
        wurm <- S.printingOf s registry "Waning Wurm"
        let base = S.landsInPlay swamp 4
            (held, gs0) = S.addHandCard wurm S.alice base
            gs =
              gs0
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            entered = S.runPure S.identityAnswer gs (S.cast S.alice held >> Stack.resolveTop)
        pure (wurmOn entered, entered)
      wurmOn gs =
        let named oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Waning Wurm"))
         in List.find named (Set.toList (GameState.battlefield gs))
   in Spec.describe s "Vanishing" $ do
        -- The proving test, and all three of rule 702.63a's abilities in one
        -- board: two counters on the entry, one removed at each of alice's
        -- upkeeps, and the sacrifice when the last one goes.
        Spec.it s "CR 702.63a whole card: the Wurm enters with two time counters and counts them down" $ do
          (found, entered) <- castWurm
          case found of
            Nothing -> Spec.assertFailure s "Waning Wurm did not reach the battlefield"
            Just wurm -> do
              Spec.assertEqWith s "two time counters on the entry" (times wurm entered) 2
              let first = after S.alice entered
              Spec.assertEqWith s "one after the first upkeep" (times wurm first) 1
              Spec.assertBool s (S.onBattlefield wurm first) "and it is still on the battlefield"
              let second = after S.alice first
              Spec.assertEqWith s "none after the second" (times wurm second) 0
              Spec.assertBool s (not (S.onBattlefield wurm second)) "so the last removal sacrificed it"
              -- CR 701.21a: a sacrifice is a move to the OWNER's graveyard, and
              -- not a destruction -- so this is the zone the wurm is in.
              Spec.assertEqWith s "in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice second)) 1
        -- Rule 702.63a says "YOUR upkeep", which is TurnScope.ControllersTurn: an
        -- opponent's upkeep is not this trigger, and an arm reading EachTurn would
        -- count the wurm down twice as fast.
        Spec.it s "CR 702.63a bob's upkeep removes nothing" $ do
          (found, entered) <- castWurm
          case found of
            Nothing -> Spec.assertFailure s "Waning Wurm did not reach the battlefield"
            Just wurm -> do
              let (settled, resolved) = upkeepOf S.bob entered
              Spec.assertEqWith s "nothing was even put on the stack" (GameState.stack settled) []
              Spec.assertEqWith s "so both counters are still there" (times wurm resolved) 2
              Spec.assertBool s (S.onBattlefield wurm resolved) "and the wurm is untouched"
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
          let counted = TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn)
              emptied = TriggerCondition.SelfLastCounterRemoved CounterKind.Time
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
-- toughness equal to the time counters on it.
--
-- ALICE HOLDS THREE ISLANDS AND BOB HOLDS TWO, which is what makes the numbers
-- readable rather than coincidental: three is not one (a mint that defaulted the
-- absent N), not zero (an entry rider whose "you" was empty, which CR 704.5f
-- would then bury as a 0/0 before any upkeep), not five (a filter that dropped
-- "you control") and not the mana value. The one-Island board every number on
-- would be 1 cannot tell any of those apart.
--
-- The CDA is the reason a second reading is taken AFTER an upkeep: a power set
-- once on entry and a CR 613.4a ability that re-reads the counters agree at 3/3
-- and disagree at 2/2.
numberlessVanishingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
numberlessVanishingSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      -- vanishingSpec's helper, and for its reasons: one upkeep for `pid`, run
      -- out to the end of the priority loop so the trigger is gathered (CR 603.3)
      -- and resolved.
      after pid gs =
        let began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep pid)) (gs {GameState.phase = upkeep, GameState.activePlayer = pid})
            settled = snd (Engine.runGamePure S.identityAnswer began Engine.settleForPriority)
         in snd (Engine.runGamePure S.identityAnswer settled Engine.priorityLoop)
      times = S.counterOf CounterKind.Time
      -- CAST rather than placed, for vanishingSpec's reason: the card's own entry
      -- replacement is what puts the counters on, and S.addPermanent reaches no CR
      -- 616.1 loop. `swamps` pays the generic half of {2}{U} on the control board,
      -- where two Islands are one mana short.
      castTidewalker islands swamps = do
        island <- S.printingOf s registry "Island"
        swamp <- S.printingOf s registry "Swamp"
        tidewalker <- S.printingOf s registry "Tidewalker"
        let base = S.landsFor swamp S.alice swamps (S.landsFor island S.bob 2 (S.landsInPlay island islands))
            (held, gs0) = S.addHandCard tidewalker S.alice base
            gs =
              gs0
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            entered = S.runPure S.identityAnswer gs (S.cast S.alice held >> Stack.resolveTop)
        pure (walkerOn entered, entered)
      walkerOn gs =
        let named oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Tidewalker"))
         in List.find named (Set.toList (GameState.battlefield gs))
   in Spec.describe s "Vanishing" $ do
        Spec.it s "CR 702.63b whole card: Tidewalker counts down the counters its own text put on" $ do
          (found, entered) <- castTidewalker 3 0
          case found of
            Nothing -> Spec.assertFailure s "Tidewalker did not reach the battlefield"
            Just walker -> do
              Spec.assertEqWith s "a 3/3 on the entry, alice's three Islands and not bob's two" (S.powerToughnessOf walker entered) (Just (3, 3))
              Spec.assertEqWith s "three time counters behind it" (times walker entered) 3
              let first = after S.alice entered
              Spec.assertEqWith s "a 2/2 after the first upkeep, so CR 613.4a re-read the counters" (S.powerToughnessOf walker first) (Just (2, 2))
              Spec.assertEqWith s "two counters left" (times walker first) 2
              let second = after S.alice first
              Spec.assertEqWith s "a 1/1 after the second" (S.powerToughnessOf walker second) (Just (1, 1))
              Spec.assertBool s (S.onBattlefield walker second) "and still on the battlefield, which a 0/0 would not be"
              let third = after S.alice second
              Spec.assertBool s (not (S.onBattlefield walker third)) "the third upkeep takes the last counter"
              -- Either road ends here: CR 702.63b's second ability sacrifices it,
              -- and a 0/0 it briefly is goes the same way under CR 704.5f. CR
              -- 701.21a makes a sacrifice a move to the OWNER's graveyard.
              Spec.assertEqWith s "and it is in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice third)) 1
        -- The pair board, differing in ONE thing: alice holds two Islands instead
        -- of three (the Swamp only pays the generic half of {2}{U}). Both numbers
        -- move, which is what says they are read rather than constant.
        Spec.it s "CR 702.63b one Island fewer is a 2/2 that goes an upkeep sooner" $ do
          (found, entered) <- castTidewalker 2 1
          case found of
            Nothing -> Spec.assertFailure s "Tidewalker did not reach the battlefield"
            Just walker -> do
              Spec.assertEqWith s "a 2/2 on the entry" (S.powerToughnessOf walker entered) (Just (2, 2))
              Spec.assertEqWith s "two time counters" (times walker entered) 2
              let second = after S.alice (after S.alice entered)
              Spec.assertBool s (not (S.onBattlefield walker second)) "gone after two upkeeps, where three Islands survived two"
        -- The mint itself, spelled out for vanishingSpec's reason. Rule 702.63b
        -- states the SAME two triggers as rule 702.63a and no entry ability, so
        -- the absent number changes exactly one of the two lists.
        Spec.it s "CR 702.63b keeps both triggers and mints no entry rewrite" $ do
          let counted = TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn)
              emptied = TriggerCondition.SelfLastCounterRemoved CounterKind.Time
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
      after pid gs = snd (upkeepOf pid gs)
      fades = S.counterOf CounterKind.Fade
      -- CAST rather than placed, for vanishingSpec's reason: rule 702.32a's first
      -- ability is a replacement on the entry, and S.addPermanent reaches no CR
      -- 616.1 loop.
      castRidgeback = do
        forest <- S.printingOf s registry "Forest"
        ridgeback <- S.printingOf s registry "Skyshroud Ridgeback"
        let base = S.landsInPlay forest 4
            (held, gs0) = S.addHandCard ridgeback S.alice base
            gs =
              gs0
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            entered = S.runPure S.identityAnswer gs (S.cast S.alice held >> Stack.resolveTop)
        pure (ridgebackOn entered, entered)
      ridgebackOn gs =
        let named oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Skyshroud Ridgeback"))
         in List.find named (Set.toList (GameState.battlefield gs))
   in Spec.describe s "Fading" $ do
        -- The proving test, and both of rule 702.32a's abilities in one board.
        Spec.it s "CR 702.32a whole card: the Ridgeback enters with two fade counters and outlives them by an upkeep" $ do
          (found, entered) <- castRidgeback
          case found of
            Nothing -> Spec.assertFailure s "Skyshroud Ridgeback did not reach the battlefield"
            Just ridgeback -> do
              Spec.assertEqWith s "two fade counters on the entry" (fades ridgeback entered) 2
              let first = after S.alice entered
              Spec.assertEqWith s "one after the first upkeep" (fades ridgeback first) 1
              Spec.assertBool s (S.onBattlefield ridgeback first) "and it is still on the battlefield"
              let second = after S.alice first
              Spec.assertEqWith s "none after the second" (fades ridgeback second) 0
              -- Rule 702.32a rather than rule 702.63a: the removal that empties
              -- the pile sacrifices nothing, because the rule's "if you can't" is
              -- about a removal that did not happen.
              Spec.assertBool s (S.onBattlefield ridgeback second) "and STILL on it, which a vanishing 2 creature would not be"
              let third = after S.alice second
              Spec.assertBool s (not (S.onBattlefield ridgeback third)) "the third upkeep could remove none, so it was sacrificed"
              -- CR 701.21a: a sacrifice is a move to the OWNER's graveyard and not
              -- a destruction.
              Spec.assertEqWith s "in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice third)) 1
              Spec.assertEqWith s "and the pile it was counting is still empty" (fades ridgeback third) 0
        -- Rule 702.32a says "YOUR upkeep", which is TurnScope.ControllersTurn: an
        -- arm reading EachTurn would count the Ridgeback down twice as fast.
        Spec.it s "CR 702.32a bob's upkeep removes nothing" $ do
          (found, entered) <- castRidgeback
          case found of
            Nothing -> Spec.assertFailure s "Skyshroud Ridgeback did not reach the battlefield"
            Just ridgeback -> do
              let (settled, resolved) = upkeepOf S.bob entered
              Spec.assertEqWith s "nothing was even put on the stack" (GameState.stack settled) []
              Spec.assertEqWith s "so both counters are still there" (fades ridgeback resolved) 2
              Spec.assertBool s (S.onBattlefield ridgeback resolved) "and the Ridgeback is untouched"
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
            (replicate 2 (TriggerCondition.StepBegins (StepBegins.MkStepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn)))
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
        -- Rule 702.24a says "YOUR upkeep", which is TurnScope.ControllersTurn: an
        -- arm reading EachTurn would age the Unicorn on bob's upkeep too.
        Spec.it s "CR 702.24a bob's upkeep ages nothing" $ do
          (oid, gs) <- boardOf
          let (settled, resolved) = upkeepOf S.bob gs
          Spec.assertEqWith s "CR 702.24a the pile is untouched by bob's upkeep" (ages oid resolved) 2
          Spec.assertEqWith s "no mana was spent" (S.tappedCount S.alice resolved) 0
          Spec.assertBool s (S.onBattlefield oid resolved) "and the Unicorn is untouched"
          -- The proxy LAST, so a mutation has to move the board before it moves
          -- the stack. What keeps the ability off it is the trigger condition
          -- never matching, CR 603.3a making bob's upkeep somebody else's.
          Spec.assertEqWith s "nothing was even put on the stack" (GameState.stack settled) []
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
-- Two printings, so no number below can be read two ways:
--
--   * Arcbound Hybrid {4} Artifact Creature -- Beast 0/0, haste and modular 2.
--   * Arcbound Worker {1} Artifact Creature -- Construct 0/0, modular 1.
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
        -- The proving test. Rule 702.43a's SECOND ability, counting the pile the
        -- dead permanent had rather than its printed N.
        Spec.it s "CR 702.43a whole card: the dead Hybrid moves all three of its counters" $ do
          (hybridId, workerId, gs) <- board "Arcbound Worker"
          let (settled, after) = murderIt exercising gs
          Spec.assertEqWith s "the Hybrid held three, not its printed two" (plusOnes hybridId (fst gs)) 3
          Spec.assertEqWith s "the Worker held one" (plusOnes workerId (fst gs)) 1
          Spec.assertEqWith s "the death trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertBool s (not (S.onBattlefield hybridId after)) "and the Hybrid is gone"
          -- CR 608.2h: four is one plus THREE, so the count came from the last
          -- known record. Two would be the printed N and one a literal.
          Spec.assertEqWith s "the Worker is up to four" (plusOnes workerId after) 4
          Spec.assertEqWith s "so it is a 4/4" (S.powerToughnessOf workerId after) (Just (4, 4))
        -- CR 603.5's "may" is a real fork, and the control for the case above --
        -- same board, same Murder, and the trigger still reaches the stack.
        Spec.it s "CR 603.5 declining the may leaves the counters nowhere" $ do
          (_, workerId, gs) <- board "Arcbound Worker"
          let (settled, after) = murderIt S.identityAnswer gs
          Spec.assertEqWith s "the trigger reached the stack all the same" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "the Worker is still on one" (plusOnes workerId after) 1
          Spec.assertEqWith s "a 1/1" (S.powerToughnessOf workerId after) (Just (1, 1))
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
        -- "Target ARTIFACT creature": Goblin Piker 2/1 is a creature and not an
        -- artifact, so CR 603.3d finds no legal target and the ability never
        -- reaches the stack. The case above is the control -- the only difference
        -- between the two boards is which creature stands beside the Hybrid.
        Spec.it s "CR 702.43a a nonartifact creature is no target at all" $ do
          (_, pikerId, gs) <- board "Goblin Piker"
          let (settled, after) = murderIt exercising gs
          Spec.assertEqWith s "nothing reached the stack" (GameState.stack settled) []
          Spec.assertEqWith s "and the Piker is still on the one it started with" (plusOnes pikerId after) 1
        -- CR 702.43b: each instance works separately. Asserted of BOTH mints,
        -- vanishing's position, no printing in the pool carrying modular twice.
        -- Spelled out rather than compared against Keyword.modular itself, for
        -- vanishingSpec's reason.
        Spec.it s "CR 702.43b each instance is its own two abilities" $ do
          Spec.assertEqWith
            s
            "modular 2 held twice mints two death triggers"
            (fmap TriggeredAbility.condition (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Modular 2) 2)))
            [TriggerCondition.SelfDies, TriggerCondition.SelfDies]
          Spec.assertEqWith
            s
            "and two entry rewrites of two counters each, which is what makes them add up"
            (Keyword.mintedReplacementsFor (Keyword.Type.Modular 2) 2)
            (replicate 2 (ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource (EntryRewrite.WithCounters (WithCounters.one CounterKind.PlusOnePlusOne (Quantity.Type.Literal 2))))))

-- CR 510.1b / 510.2's combat damage watched by a BYSTANDER rather than by the
-- creature that dealt it -- TriggerCondition.PermanentDealsCombatDamageToPlayer,
-- the filtered twin of poisonousSpec's SelfDealsCombatDamageToPlayer.
--
-- Tovolar, Dire Overlord {1}{R}{G} Legendary Creature -- Human Werewolf 3/3 is
-- the card: "whenever a Wolf or Werewolf you control deals combat damage to a
-- player, draw a card". Both faces print it; the back face's copy goes through
-- Pawl.CardSpec's corpus lints, but no case here reaches it -- that needs the CR
-- 731 transform Pawl.DaytimeSpec drives.
--
-- Tovolar is himself a Werewolf, so the filter admits the watcher: a self-scoped
-- reading would draw one card where these cases draw two. Russet Wolves (Wolf
-- 3/3) is the other subtype of the printed "or", and Goblin Piker (Goblin Warrior
-- 2/1) is the creature the filter must reject.
--
-- Every library is stocked past the draws, so an extra draw shows as an extra
-- card rather than as CR 104.3c ending the game before the assertions run.
tovolarSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tovolarSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      stock printing n pid gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 .. (n :: Int)]
      handSize pid gs = length (Game.zoneMembers Zone.Hand pid gs)
      -- CR 508.1's declaration narrowed to the named creatures, and CR 509.1's
      -- left empty: S.aggressiveAnswer attacks and blocks with everything, which
      -- a case about one attacker connecting cannot allow.
      plan :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      plan attackers p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      -- CR 508.1 / 509.1: one named attacker, met by one named blocker.
      oneOnOne :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      oneOnOne attacker blocker p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (== attacker) ids
        Prompt.DeclareBlockers _ _ _ attackers -> Map.singleton blocker (Set.fromList attackers)
        _ -> S.aggressiveAnswer p
   in Spec.describe s "Filtered combat damage" $ do
        -- The proving test. Three unblocked attackers, two of which the filter
        -- admits: Tovolar for "Werewolf" and the Wolves for "Wolf". The Piker
        -- connects too and draws nothing, which is the filter doing its work
        -- inside the same event.
        Spec.it s "CR 510.2 a bystander draws once per Wolf or Werewolf that connects" $ do
          (gs, _, _) <- board ["Tovolar, Dire Overlord", "Russet Wolves", "Goblin Piker"] []
          piker <- S.printingOf s registry "Goblin Piker"
          let after = S.runCombat S.aggressiveAnswer (stock piker 4 S.alice gs)
          Spec.assertEqWith s "all three connected, for eight" (S.lifeOf S.bob after) (Just 12)
          Spec.assertEqWith s "so two cards were drawn, not three and not one" (handSize S.alice after) 2
          Spec.assertEqWith s "leaving two in the library" (length (Game.zoneMembers Zone.Library S.alice after)) 2
        -- CR 510.1c: a BLOCKED creature assigns its combat damage to the blocker,
        -- so the Wolves deals its three to bob's Piker and the condition's
        -- player-recipient half rejects the event. The watcher is on the board and
        -- the damager is a Wolf she controls; only the recipient differs.
        Spec.it s "CR 510.1c combat damage dealt to a creature draws nothing" $ do
          (gs, mine, theirs) <- board ["Tovolar, Dire Overlord", "Russet Wolves"] ["Goblin Piker"]
          piker <- S.printingOf s registry "Goblin Piker"
          case (mine, theirs) of
            ([_, wolves], [blocker]) -> do
              let after = S.runCombat (oneOnOne wolves blocker) (stock piker 4 S.alice gs)
              Spec.assertEqWith s "bob took none of it" (S.lifeOf S.bob after) (Just 20)
              Spec.assertEqWith s "and his Piker died for it" (S.creaturesInPlay S.bob after) 0
              Spec.assertEqWith s "so no card was drawn" (handSize S.alice after) 0
            _ -> Spec.assertFailure s "fixture should give alice a Tovolar and a Wolves, bob a Piker"
        -- CR 109.5's "you control", which is what makes this a bystander's
        -- condition rather than the board's: bob has a Tovolar of his own,
        -- watching alice's two connect. He controls neither, so he draws nothing
        -- -- an arm that read the event's damager without the Filter's
        -- ControlledBy would have him draw twice.
        Spec.it s "CR 109.5 the defender's own Tovolar sees no Wolf of his connect" $ do
          (gs, mine, theirs) <- board ["Tovolar, Dire Overlord", "Russet Wolves"] ["Tovolar, Dire Overlord"]
          piker <- S.printingOf s registry "Goblin Piker"
          case (mine, theirs) of
            ([tovolar, wolves], [_]) -> do
              let after = S.runCombat (plan [tovolar, wolves]) (stock piker 4 S.bob (stock piker 4 S.alice gs))
              Spec.assertEqWith s "both of alice's connected, for six" (S.lifeOf S.bob after) (Just 14)
              Spec.assertEqWith s "she drew two" (handSize S.alice after) 2
              Spec.assertEqWith s "and he drew none" (handSize S.bob after) 0
            _ -> Spec.assertFailure s "fixture should give alice a Tovolar and a Wolves, bob a Tovolar"

-- CR 603.2c's FIRST sentence on the same event: "whenever ONE OR MORE artifact
-- creatures you control deal combat damage to a player" fires once for the CR
-- 510.2 step however many connected --
-- TriggerCondition.PermanentsDealCombatDamageToPlayer, the batch twin of the
-- condition tovolarSpec above proves.
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
        List.nub
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
          Spec.assertEqWith s "CR 510.2: the three damage events were one event group" (length (combatDamageGroups after)) 1
        -- The board that differs in one artifact creature: one connecting is one
        -- occurrence under either reading, so this is the negative half's floor
        -- rather than a discrimination.
        Spec.it s "CR 603.2c one artifact creature connecting gives {E}{E} too" $ do
          after <- board ["Pia Nalaar, Chief Mechanic", "Palladium Myr"]
          Spec.assertEqWith s "alice got {E}{E} for the one" (energy after) 2
          Spec.assertEqWith s "both connected, for four" (S.lifeOf S.bob after) (Just 16)
        -- The Filter: Pia alone connects, and she is no artifact.
        Spec.it s "CR 603.2c a non-artifact connecting alone gives nothing" $ do
          after <- board ["Pia Nalaar, Chief Mechanic"]
          Spec.assertEqWith s "alice got no energy" (energy after) 0
          Spec.assertEqWith s "though Pia connected, for two" (S.lifeOf S.bob after) (Just 18)
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
        -- The discriminating twin, differing in the ANSWER alone: paying nothing
        -- is how the printed "may" is declined (CR 107.14), so CR 118.12's "if
        -- you do" gate fails and no token is created at all.
        Spec.it s "CR 107.14 paying no energy declines the ability, so no Aetherjet is created" $ do
          after <- board ["Pia Nalaar, Chief Mechanic", "Palladium Myr", "Spined Thopter"]
          let resolved = payAtEndStep 0 (S.addPlayerCounter PlayerCounterKind.Energy 3 S.alice after)
          Spec.assertEqWith s "no Aetherjet" (aetherjetIds resolved) []
          Spec.assertEqWith s "and every {E} she held is still hers" (energy resolved) 5

-- Pia's end-step trigger, driven to its answer: alice's end step begins, the
-- trigger is put on the stack, and it resolves with `n` paid to
-- Prompt.ChoosePaidEnergy. The amount is PINNED rather than left to
-- S.identityAnswer, which answers the same way on both boards below and so
-- could not tell paying three from declining.
payAtEndStep :: Natural -> GameState.GameState -> GameState.GameState
payAtEndStep n gs =
  let endStep = Phase.Ending EndingStep.EndStep
      begun = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)) (gs {GameState.phase = endStep})
      settled = snd (Engine.runGamePure (paying n) begun Engine.settleForPriority)
   in S.settleSba (snd (Engine.runGamePure (paying n) settled Stack.resolveTop))

paying :: Natural -> Prompt.Prompt r -> r
paying n p = case p of
  Prompt.ChoosePaidEnergy {} -> n
  _ -> S.identityAnswer p

-- The tokens named for Pia's Vehicle. By NAME rather than by S.tokensOf alone,
-- so a token minted by anything else could not be mistaken for hers.
aetherjetIds :: GameState.GameState -> [ObjectId.ObjectId]
aetherjetIds gs =
  [ oid
  | oid <- S.tokensOf gs,
    fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Nalaar Aetherjet"))
  ]

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
-- Aragorn arrives BETWEEN the two combats so the Maulers takes its two counters
-- from printed renown 2 alone: with him out on the first swing the Maulers would
-- hold renown 2 and a granted renown 1 at once, and CR 702.112c leaves which
-- resolves first to its controller.
aragornSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
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
        -- The other half of the same static ability, which the case above only
        -- passes through: CR 702.7b's first strike, granted to an ATTACKING
        -- creature. Two identical 2/1s meet, and only the attacker's controller
        -- has an Aragorn -- so the blocker is dead before it assigns (CR 510.4),
        -- where without the grant both would die.
        Spec.it s "CR 702.7b the same static grants first strike to attackers" $ do
          (gs, mine, theirs) <- board ["Aragorn, Hornburg Hero", "Goblin Piker"] ["Goblin Piker"]
          case (mine, theirs) of
            ([_, piker], [blocker]) -> do
              let after = S.runCombat (plan [piker]) gs
              Spec.assertEqWith s "the blocker is dead" (Game.lookupObject blocker after) Nothing
              Spec.assertEqWith s "the attacker survived, unrenowned" (renownedness piker after) (Just False)
              Spec.assertEqWith s "alice keeps both creatures" (S.creaturesInPlay S.alice after) 2
              Spec.assertEqWith s "bob none" (S.creaturesInPlay S.bob after) 0
              Spec.assertEqWith s "and nothing reached bob" (S.lifeOf S.bob after) (Just 20)
            _ -> Spec.assertFailure s "fixture should give alice an Aragorn and a Piker, bob a Piker"

-- The same filtered condition's OTHER half of the event: HOW MUCH it carried,
-- read back through Pawl.Engine.Binding.eventAmount -- Shroofus Sproutsire
-- {2}{G} Legendary Creature -- Saproling 1/1, "trample" and "whenever a Saproling
-- you control deals combat damage to a player, create that many 1/1 green
-- Saproling creature tokens".
--
-- Trample is what makes "that many" a different number from anything else on the
-- board: CR 702.19b lets the attacker's controller hold damage back on the
-- blocker, so a 5/5 trampler can deal 4 to the player. Four tokens is therefore
-- not the damager's power (5), not a fixed count (1), and not one per damager (2
-- creatures connect). The power reading is the one that needs the trample: an
-- unblocked trampler deals its whole power, and no board without a blocker tells
-- the two apart.
--
-- Shroofus is himself a Saproling, so the filter admits the watcher, and the
-- Goblin Piker (Goblin Warrior 2/1) beside him is the creature it must reject.
--
-- No library stocking: nothing here draws, and runCombat stops at end of combat,
-- so CR 104.3c never comes up.
shroofusSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
shroofusSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- CR 508.1 / 509.1 / 510.1c: the named attackers, the named blocks, and the
      -- trampler's division pinned rather than left to the fixture -- the division
      -- is the one thing the two cases below differ in.
      swing ::
        [ObjectId.ObjectId] ->
        Map.Map ObjectId.ObjectId (Set.Set ObjectId.ObjectId) ->
        [(ObjectId.ObjectId, Map.Map Recipient.Recipient Natural)] ->
        Prompt.Prompt r ->
        r
      swing attackers blocks divisions p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        Prompt.DeclareBlockers {} -> blocks
        Prompt.AssignCombatDamage _ _ damager _ _ -> Maybe.fromMaybe Map.empty (List.lookup damager divisions)
        _ -> S.aggressiveAnswer p
      -- CR 508.1b's choice, which only the three-seat board raises: bob is
      -- attacked, carol is the opponent who is neither the damaged player nor the
      -- damager's controller.
      atBob :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      atBob attackers p = case p of
        Prompt.ChooseAttackTarget _ _ _ options -> Maybe.fromMaybe (NonEmpty.head options) (List.find (== AttackTarget.OfPlayer S.bob) (NonEmpty.toList options))
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
   in Spec.describe s "Tokens counted off a combat damage event" $ do
        -- The proving case. A 5/5 trampler holds 1 back on the blocker and spills
        -- 4, while a Goblin Piker connects unblocked for 2 alongside it.
        Spec.it s "CR 510.2 whole card: that many is the damage dealt, not the damager's power" $ do
          (gs, mine, theirs) <- board ["Shroofus Sproutsire", "Goblin Piker"] ["Goblin Piker"]
          case (mine, theirs) of
            ([shroofus, piker], [blocker]) -> do
              let loaded = S.addCounter CounterKind.PlusOnePlusOne 4 shroofus gs
                  blocks = Map.singleton blocker (Set.singleton shroofus)
                  spilling = [(shroofus, Map.fromList [(Recipient.ToCreature blocker, 1), (Recipient.ToPlayer S.bob, 4)])]
                  after = S.runCombat (swing [shroofus, piker] blocks spilling) loaded
              Spec.assertEqWith s "the counters make Shroofus a 5/5" (S.powerToughnessOf shroofus loaded) (Just (5, 5))
              Spec.assertEqWith s "4 spilled past the blocker, plus the Piker's 2" (S.lifeOf S.bob after) (Just 14)
              Spec.assertBool s (not (Set.member blocker (GameState.battlefield after))) "CR 704.5g: the blocker took its 1"
              -- 6 = Shroofus, the Goblin Piker, and four tokens. A count read off
              -- his POWER would be 7, a fixed count of one 3, and a filter that
              -- admitted the Goblin Piker's 2 as well 8.
              Spec.assertEqWith s "so four tokens, not five and not one and not six" (S.creaturesInPlay S.alice after) 6
              Spec.assertEqWith s "and bob keeps nothing" (S.creaturesInPlay S.bob after) 0
            _ -> Spec.assertFailure s "fixture should give alice a Shroofus and a Piker, bob a Piker"
        -- The negative, on the SAME board with the same seats, the same creatures
        -- and the same declaration -- only the division differs. CR 702.19b lets
        -- the whole 5 stay on the blocker, so the Saproling deals its
        -- combat damage to a creature rather than to a player and the condition's
        -- recipient half rejects the event. The Goblin Piker still connects for 2,
        -- which the filter rejects -- so both halves answer at once, and neither
        -- makes a token.
        Spec.it s "CR 510.1c the same trampler soaking its blocker makes nothing" $ do
          (gs, mine, theirs) <- board ["Shroofus Sproutsire", "Goblin Piker"] ["Goblin Piker"]
          case (mine, theirs) of
            ([shroofus, piker], [blocker]) -> do
              let loaded = S.addCounter CounterKind.PlusOnePlusOne 4 shroofus gs
                  blocks = Map.singleton blocker (Set.singleton shroofus)
                  soaking = [(shroofus, Map.fromList [(Recipient.ToCreature blocker, 5), (Recipient.ToPlayer S.bob, 0)])]
                  after = S.runCombat (swing [shroofus, piker] blocks soaking) loaded
              Spec.assertEqWith s "only the Goblin Piker's 2 reached bob" (S.lifeOf S.bob after) (Just 18)
              Spec.assertBool s (not (Set.member blocker (GameState.battlefield after))) "CR 704.5g: the blocker took all 5"
              Spec.assertEqWith s "and alice keeps her two creatures, with no token beside them" (S.creaturesInPlay S.alice after) 2
            _ -> Spec.assertFailure s "fixture should give alice a Shroofus and a Piker, bob a Piker"
        -- CR 109.5's "you control", on three seats so that the damager's
        -- controller, the damaged player and a bystanding opponent are three
        -- different people. alice's Shroofus connects unblocked for 5; bob's and
        -- carol's watch it and make nothing.
        Spec.it s "CR 109.5 an opponent's Shroofus counts no Saproling of hers" $ do
          shroofus <- S.printingOf s registry "Shroofus Sproutsire"
          let (gs, mine, theirs, hers) = S.threePlayerCombat [shroofus] [shroofus] [shroofus]
          case (mine, theirs, hers) of
            ([attacker], [_], [_]) -> do
              let loaded = S.addCounter CounterKind.PlusOnePlusOne 4 attacker gs
                  after = S.runCombat (atBob [attacker]) loaded
              Spec.assertEqWith s "unblocked, all 5 reached bob" (S.lifeOf S.bob after) (Just 15)
              Spec.assertEqWith s "carol was not attacked" (S.lifeOf S.carol after) (Just 20)
              Spec.assertEqWith s "alice took five tokens" (S.creaturesInPlay S.alice after) 6
              Spec.assertEqWith s "bob none" (S.creaturesInPlay S.bob after) 1
              Spec.assertEqWith s "and carol none" (S.creaturesInPlay S.carol after) 1
            _ -> Spec.assertFailure s "fixture should give each seat a Shroofus"

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
        -- CR 509.3d's arity, and the whole difference from CR 509.3c: "triggers
        -- once for each creature that blocks the specified creature". Two
        -- blockers, two triggers, and each -1/-1 lands on its OWN blocker.
        --
        -- The Hill Giant is the load-bearing reading: a condition matched against
        -- the GROUPED GameEvent.AttackerBlocked fires once and leaves it 3/3,
        -- and a binding that named the bearer instead moves the Cavalry's own
        -- 2/2.
        Spec.it s "CR 509.3d two blockers are two triggers, each on its own blocker" $ do
          (gs, mine, theirs) <- board ["Benalish Cavalry"] ["Goblin Piker", "Hill Giant"]
          case (mine, theirs) of
            ([cavalry], [piker, giant]) -> do
              let struck = atDamage gs
              -- One assertion over all three readings, so a mutation cannot hide
              -- behind whichever of them is checked first.
              Spec.assertEqWith
                s
                "the 3/3 Giant is 2/2, the 2/1 Piker is gone, and the Cavalry took neither -1/-1"
                (S.powerToughnessOf giant struck, S.onBattlefield piker struck, S.powerToughnessOf cavalry struck)
                (Just (2, 2), False, Just (2, 2))
            _ -> Spec.assertFailure s "fixture should give bob two blockers"
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
  let noBlocks :: Prompt.Prompt r -> r
      noBlocks p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      board mine theirs = do
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
        -- The control leg for the case above, on the same board: nothing blocks,
        -- so CR 509.3a's event never happens and the Guard stays 0/2. Without it
        -- an ability that pumped on any combat event at all would pass.
        Spec.it s "CR 509.3a a Guard that does not block is not pumped" $ do
          (gs, _, blockers) <- board ["Goblin Piker"] ["Inner-Chamber Guard"]
          case blockers of
            [guard] -> do
              let after = S.runCombat noBlocks gs
              Spec.assertEqWith s "still 0/2" (S.powerToughnessOf guard after) (Just (0, 2))
              Spec.assertEqWith s "and the unblocked Piker's 2 reached bob" (S.lifeOf S.bob after) (Just 18)
            _ -> Spec.assertFailure s "fixture should give bob a Guard"
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
        -- The control leg for CR 509.3c, on the same board as the case above:
        -- attacking is not becoming blocked, so an unblocked Guard stays 0/2 and
        -- takes nothing from bob.
        Spec.it s "CR 509.3c a Guard that goes unblocked is not pumped" $ do
          (gs, attackers, _) <- board ["Inner-Chamber Guard"] ["Goblin Piker"]
          case attackers of
            [guard] -> do
              let after = S.runCombat noBlocks gs
              Spec.assertEqWith s "still 0/2" (S.powerToughnessOf guard after) (Just (0, 2))
              Spec.assertEqWith s "and 0 power took nothing from bob" (S.lifeOf S.bob after) (Just 20)
            _ -> Spec.assertFailure s "fixture should give alice a Guard"
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
afflictSpec s registry =
  let -- Attacks `who` with everything and lets them block with everything.
      attacking :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      attacking who p = case p of
        Prompt.ChooseDefender {} -> who
        Prompt.ChooseAttackTarget {} -> S.attackTo who p
        _ -> S.aggressiveAnswer p
      -- The same, with CR 509.1's declaration switched off -- the control leg, and
      -- the only difference between the two answerers.
      unblocked :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      unblocked who p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> attacking who p
      -- alice fields the Khenra; bob and carol each field a Goblin Piker, so
      -- either can block and neither is the only possible defender.
      board = do
        khenra <- S.printingOf s registry "Khenra Eternal"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (S.threePlayerCombat [khenra] [piker] [piker])
      -- All three life totals as one reading, so no mutation can hide behind the
      -- order the assertions happen to be written in.
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
   in Spec.describe s "Afflict" $ do
        -- The proving test. alice attacks bob, bob blocks, and CR 508.5 makes bob
        -- the defending player, so bob alone loses 1. No combat damage reaches a
        -- player: the Khenra is blocked, and its 2 and the Piker's 2 trade.
        Spec.it s "CR 702.130a whole card: a blocked Khenra Eternal costs the defending player 1 life" $ do
          (gs, ours, yours, _) <- board
          case (ours, yours) of
            ([khenra], [piker]) -> do
              let after = S.runCombat (attacking S.bob) gs
              Spec.assertEqWith s "bob, and nobody else, is down 1" (lives after) (Just 20, Just 19, Just 20)
              Spec.assertBool s (not (S.onBattlefield khenra after)) "the 2/2 Khenra died to the Piker's 2"
              Spec.assertBool s (not (S.onBattlefield piker after)) "and the 2/1 Piker to the Khenra's"
            _ -> Spec.assertFailure s "fixture should give alice a Khenra and bob a Piker"
        -- CR 508.5a: the defending player is one SPECIFIC player, determined per
        -- attacking creature. The only difference from the case above is the
        -- answer to Prompt.ChooseDefender, so an implementation that bound the
        -- attacker's controller, or "an opponent", or a fixed seat cannot pass
        -- both.
        Spec.it s "CR 508.5 the life follows whichever opponent was attacked" $ do
          (gs, _, _, _) <- board
          Spec.assertEqWith s "carol, attacked this time, is the one down 1" (lives (S.runCombat (attacking S.carol) gs)) (Just 20, Just 20, Just 19)
        -- The control leg, on the same board: no block, so CR 509.3c's event never
        -- happens and no life is lost to afflict. bob is down TWO instead of one,
        -- the Khenra's combat damage -- distinct from 1, so the two legs cannot be
        -- read as each other.
        Spec.it s "CR 509.3c an unblocked Khenra Eternal afflicts nobody" $ do
          (gs, _, _, _) <- board
          Spec.assertEqWith s "bob took 2 combat damage and no afflict" (lives (S.runCombat (unblocked S.bob) gs)) (Just 20, Just 18, Just 20)
        -- CR 603.2 through CR 508.5: the becomes-blocked event carries the
        -- defending player, and the scan stamps them under the reserved slot rule
        -- 702.130a's "defending player" reads. The falsifier is an arm that binds
        -- the attacking side, or none at all.
        Spec.it s "CR 603.2 the defending player rides the becomes-blocked event in the reserved slot" $ do
          let bindings = Event.eventBindings (Setup.emptyGame S.bothPlayers) Nothing S.alice TriggerCondition.SelfBecomesBlocked (GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked (ObjectId.MkObjectId 9) S.carol 1))
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
--
-- Not implemented: a case splitting the declaration across both opponents, where
-- CR 702.121a's bonus is 2 (#3039).
meleeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
meleeSpec s registry =
  let -- Attacks `who` with everything, aiming every attack at the player.
      attacking :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      attacking who p = case p of
        Prompt.ChooseDefender {} -> who
        Prompt.ChooseAttackTarget {} -> S.attackTo who p
        _ -> S.aggressiveAnswer p
      isPlaneswalker target = case target of
        AttackTarget.OfPlaneswalker _ -> True
        AttackTarget.OfPlayer _ -> False
        AttackTarget.OfBattle _ -> False
      -- The same, with `these` creatures aimed at a planeswalker instead (CR
      -- 508.1b). Falls back to the head, so a board with no planeswalker offered
      -- runs exactly as the answerer above.
      aimingAtJace :: [ObjectId.ObjectId] -> PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimingAtJace these who p = case p of
        Prompt.ChooseAttackTarget _ _ oid options
          | elem oid these -> case filter isPlaneswalker (NonEmpty.toList options) of
              target : _ -> target
              [] -> NonEmpty.head options
        _ -> attacking who p
      -- alice fields the Bird (plus whatever else `mine` names); bob and carol
      -- each field a Goblin Piker, so either is a legal defending player.
      board mine = do
        wings <- S.printingOf s registry "Wings of the Guard"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (S.threePlayerCombat (wings : mine) [piker] [piker])
      -- The same, with bob fielding Jace Beleren at loyalty 3 as well, which is
      -- what lets an attack be declared at something that is not an opponent.
      jaceBoard mine = do
        wings <- S.printingOf s registry "Wings of the Guard"
        piker <- S.printingOf s registry "Goblin Piker"
        jace <- S.printingOf s registry "Jace Beleren"
        pure $ case S.threePlayerCombat (wings : mine) [piker, jace] [piker] of
          (gs, ours, theirs@(_ : jaceId : _), others) -> (S.addCounter CounterKind.Loyalty 3 jaceId gs, ours, theirs, others)
          done -> done
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
   in Spec.describe s "Melee" $ do
        -- The proving test. One opponent attacked, so +1/+1 on a 1/1. The
        -- falsifier three seats buy: a bonus counting alice's OPPONENTS rather
        -- than the ones she attacked reads 2 here and cannot pass.
        Spec.it s "CR 702.121a whole card: Wings of the Guard attacking one of two opponents is 2/2" $ do
          (gs, ours, _, _) <- board []
          case ours of
            [wings] -> Spec.assertEqWith s "1/1 plus one opponent attacked" (S.powerToughnessOf wings (atBlockers (attacking S.bob) gs)) (Just (2, 2))
            _ -> Spec.assertFailure s "fixture should give alice one Bird"
        -- Rule 702.121a counts OPPONENTS, not creatures: a second attacker at the
        -- same seat adds nothing. The falsifier is a bonus read off the size of
        -- the declaration, which reads 2 here and 1 above.
        Spec.it s "CR 702.121a a second attacker at the same opponent does not raise the bonus" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          (gs, ours, _, _) <- board [piker]
          case ours of
            [wings, _] -> Spec.assertEqWith s "still one opponent attacked" (S.powerToughnessOf wings (atBlockers (attacking S.bob) gs)) (Just (2, 2))
            _ -> Spec.assertFailure s "fixture should give alice a Bird and a Piker"
        -- CR 508.4's sibling reading, from the other side: attacking an
        -- opponent's PLANESWALKER is not attacking that opponent, so the bonus is
        -- 0 and the Bird stays a 1/1. The attack record is asserted first, so a
        -- run where the Bird failed to attack at all fails there rather than
        -- passing this vacuously.
        Spec.it s "CR 506.3 a creature that attacked only a planeswalker gets +0/+0" $ do
          (gs, ours, theirs, _) <- jaceBoard []
          case (ours, theirs) of
            ([wings], [_, jaceId]) -> do
              let after = atBlockers (aimingAtJace [wings] S.bob) gs
              Spec.assertEqWith s "CR 508.1b the Bird really did attack Jace" (Map.lookup wings (Combat.Type.attackers (GameState.combat after))) (Just (AttackTarget.OfPlaneswalker jaceId))
              Spec.assertEqWith s "and no opponent was attacked, so it is still a 1/1" (S.powerToughnessOf wings after) (Just (1, 1))
            _ -> Spec.assertFailure s "fixture should give alice a Bird and bob a Jace"
        -- Melee still TRIGGERS when its bearer attacks a planeswalker -- what the
        -- planeswalker changes is the bonus. Same board as above plus a Piker
        -- sent at bob, so the record holds one opponent and the Bird is pumped
        -- although it attacked nobody.
        Spec.it s "CR 702.121a the bearer's own attack need not be the one that counts" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          (gs, ours, _, _) <- jaceBoard [piker]
          case ours of
            [wings, _] -> Spec.assertEqWith s "the Piker's attack on bob is the +1/+1" (S.powerToughnessOf wings (atBlockers (aimingAtJace [wings] S.bob) gs)) (Just (2, 2))
            _ -> Spec.assertFailure s "fixture should give alice a Bird and a Piker"
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
dethroneSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
dethroneSpec s registry =
  let at pid n gs = gs {GameState.players = Map.adjust (\pl -> pl {Player.life = n}) pid (GameState.players gs)}
      lives a b c gs = at S.alice a (at S.bob b (at S.carol c gs))
      -- alice fields the Revolutionary; bob and carol each field a Piker, so
      -- either is a legal defending player.
      board = do
        rev <- S.printingOf s registry "Enraged Revolutionary"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (S.threePlayerCombat [rev] [piker] [piker])
      -- The same with bob fielding Jace Beleren at loyalty 3, so that the only
      -- attackable permanent on his side is not a player.
      jaceBoard = do
        rev <- S.printingOf s registry "Enraged Revolutionary"
        piker <- S.printingOf s registry "Goblin Piker"
        jace <- S.printingOf s registry "Jace Beleren"
        pure $ case S.threePlayerCombat [rev] [piker, jace] [piker] of
          (gs, ours, theirs@(_ : jaceId : _), others) -> (S.addCounter CounterKind.Loyalty 3 jaceId gs, ours, theirs, others)
          done -> done
      isPlaneswalker target = case target of
        AttackTarget.OfPlaneswalker _ -> True
        AttackTarget.OfPlayer _ -> False
        AttackTarget.OfBattle _ -> False
      aimingAtJace :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimingAtJace who p = case p of
        Prompt.ChooseAttackTarget _ _ _ options -> case filter isPlaneswalker (NonEmpty.toList options) of
          target : _ -> target
          [] -> NonEmpty.head options
        _ -> attacking who p
      attacking :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      attacking who p = case p of
        Prompt.ChooseDefender {} -> who
        Prompt.ChooseAttackTarget {} -> S.attackTo who p
        _ -> S.aggressiveAnswer p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
   in Spec.describe s "Dethrone" $ do
        -- The proving test. bob is on 20 against alice's 15 and carol's 10, so the
        -- creature grows.
        Spec.it s "CR 702.105a whole card: attacking the player with the most life is a +1/+1 counter" $ do
          (gs, ours, _, _) <- board
          case ours of
            [rev] -> Spec.assertEqWith s "2/1 plus one counter" (S.powerToughnessOf rev (atBlockers (attacking S.bob) (lives 15 20 10 gs))) (Just (3, 2))
            _ -> Spec.assertFailure s "fixture should give alice one Revolutionary"
        -- The same board attacked the other way. carol is the LOWEST, so nothing
        -- triggers -- the falsifier for a condition that fired on any attack.
        Spec.it s "CR 702.105a attacking a player who is not on the most life does nothing" $ do
          (gs, ours, _, _) <- board
          case ours of
            [rev] -> do
              let after = atBlockers (attacking S.carol) (lives 15 20 10 gs)
              Spec.assertEqWith s "CR 508.1b the Revolutionary really did attack carol" (Map.lookup rev (Combat.Type.attackers (GameState.combat after))) (Just (AttackTarget.OfPlayer S.carol))
              Spec.assertEqWith s "and it is still a 2/1" (S.powerToughnessOf rev after) (Just (2, 1))
            _ -> Spec.assertFailure s "fixture should give alice one Revolutionary"
        -- Rule 702.105a says "the player with the most life", not "the opponent
        -- with the most life", so the attacker's OWN controller is compared too:
        -- alice on 25 makes bob's 20 not the most, and the same attack that grew
        -- the creature above now does nothing.
        Spec.it s "CR 702.105a the attacking creature's controller counts as a player" $ do
          (gs, ours, _, _) <- board
          case ours of
            [rev] -> do
              let after = atBlockers (attacking S.bob) (lives 25 20 10 gs)
              Spec.assertEqWith s "CR 508.1b the Revolutionary really did attack bob" (Map.lookup rev (Combat.Type.attackers (GameState.combat after))) (Just (AttackTarget.OfPlayer S.bob))
              Spec.assertEqWith s "and alice is ahead, so it is still a 2/1" (S.powerToughnessOf rev after) (Just (2, 1))
            _ -> Spec.assertFailure s "fixture should give alice one Revolutionary"
        -- "Or tied for most life": bob and carol both on 20, and attacking either
        -- triggers. The falsifier is a strict comparison, which fires on neither.
        Spec.it s "CR 702.105a a tie for most life still triggers" $ do
          (gs, ours, _, _) <- board
          case ours of
            [rev] -> Spec.assertEqWith s "tied at 20 against alice's 15" (S.powerToughnessOf rev (atBlockers (attacking S.carol) (lives 15 20 20 gs))) (Just (3, 2))
            _ -> Spec.assertFailure s "fixture should give alice one Revolutionary"
        -- CR 702.105a names THE PLAYER, and CR 508.1b lets a creature attack a
        -- planeswalker instead. The defending player is bob either way, so this is
        -- the case that separates reading Combat.attackers from reading the
        -- declaration event's CR 508.5 field.
        Spec.it s "CR 508.1b attacking the leader's planeswalker is not attacking the leader" $ do
          (gs, ours, theirs, _) <- jaceBoard
          case (ours, theirs) of
            ([rev], [_, jaceId]) -> do
              let after = atBlockers (aimingAtJace S.bob) (lives 15 20 10 gs)
              Spec.assertEqWith s "CR 508.1b the Revolutionary really did attack Jace" (Map.lookup rev (Combat.Type.attackers (GameState.combat after))) (Just (AttackTarget.OfPlaneswalker jaceId))
              Spec.assertEqWith s "and no player was attacked, so it is still a 2/1" (S.powerToughnessOf rev after) (Just (2, 1))
            _ -> Spec.assertFailure s "fixture should give alice a Revolutionary and bob a Jace"
        -- CR 702.105b: two instances are two abilities, so two triggers and two
        -- counters. Asserted at the mint, no card in the pool having dethrone twice.
        Spec.it s "CR 702.105b each instance triggers separately" $ do
          Spec.assertEqWith s "dethrone held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Dethrone 2)) [Keyword.dethrone, Keyword.dethrone]
          Spec.assertEqWith s "and once is one" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Dethrone 1)) [Keyword.dethrone]

-- CR 508.3a's second sentence, the first condition to read WHOM a declaration
-- attacked from a bystander's seat rather than from the attacker's.
--
-- Marchesa's Decree {3}{B} Enchantment is the card: "whenever a creature attacks
-- you or a planeswalker you control, that creature's controller loses 1 life".
-- CR 508.5/508.5a make that one test on GameEvent.AttackerDeclared's defending
-- player, so the condition never reads the board.
--
-- THREE SEATS, and load-bearing: at two players the defending player, "an
-- opponent" and the attacking creature's controller are all one seat, so a
-- condition that fired on every declaration and a payload that hit the wrong
-- player would both pass. alice is active and attacks, bob holds the Decree, and
-- carol is the second opponent whose leg separates the two readings.
--
-- Every leg reads all three life totals, since "that creature's controller"
-- (alice) and CR 109.5's "you" (bob) are different seats on this board.
--
-- TWO attackers, because CR 508.3a's arity is per declared attacker: one life per
-- creature, not one per declaration -- which is CR 508.3b's arity, and
-- Pawl.EventTriggerSpec's Curse of Vitality group is the same board proving it.
marchesasDecreeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
marchesasDecreeSpec s registry =
  let board = do
        decree <- S.printingOf s registry "Marchesa's Decree"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (S.threePlayerCombat [piker, piker] [decree] [])
      attacking :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      attacking who p = case p of
        Prompt.ChooseDefender {} -> who
        Prompt.ChooseAttackTarget {} -> S.attackTo who p
        _ -> S.aggressiveAnswer p
      -- The same board with the declaration itself declined, which is the leg
      -- that separates "a creature attacked you" from "the step began".
      standingStill :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      standingStill who p = case p of
        Prompt.DeclareAttackers {} -> []
        _ -> attacking who p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
   in Spec.describe s "Marchesa's Decree" $ do
        -- The proving test. Two of alice's Pikers attack bob, so the Decree fires
        -- twice and ALICE is down 2 -- bob, whose enchantment it is, loses nothing.
        Spec.it s "CR 508.3a/508.5 whole card: each creature attacking you costs its controller 1 life" $ do
          (gs, _, _, _) <- board
          Spec.assertEqWith s "alice lost 1 per attacker, bob and carol nothing" (lives (atBlockers (attacking S.bob) gs)) (Just 18, Just 20, Just 20)
        -- The same declaration aimed at the other opponent. CR 508.5 makes carol
        -- the defending player, so the Decree is silent -- the falsifier for a
        -- condition that fired on any declaration, which a two-seat board cannot
        -- see.
        Spec.it s "CR 508.5 a creature attacking the other opponent does not trigger it" $ do
          (gs, ours, _, _) <- board
          let after = atBlockers (attacking S.carol) gs
          case ours of
            piker : _ -> Spec.assertEqWith s "CR 508.1b the Piker really did attack carol" (Map.lookup piker (Combat.Type.attackers (GameState.combat after))) (Just (AttackTarget.OfPlayer S.carol))
            _ -> Spec.assertFailure s "fixture should give alice two Goblin Pikers"
          Spec.assertEqWith s "and nobody lost life" (lives after) (Just 20, Just 20, Just 20)
        -- No declaration at all, on the same board and against the same defending
        -- player: the falsifier for a condition that fired on the step rather than
        -- on CR 508.1a's declaration.
        Spec.it s "CR 508.3a a declare attackers step with no attackers triggers nothing" $ do
          (gs, _, _, _) <- board
          let after = atBlockers (standingStill S.bob) gs
          Spec.assertEqWith s "nothing was declared" (Combat.Type.attackers (GameState.combat after)) Map.empty
          Spec.assertEqWith s "and nobody lost life" (lives after) (Just 20, Just 20, Just 20)

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
        -- The proving test: two blockers is one beyond the first, so rampage 2 is
        -- +2/+2 and the 2/4 Pack is a 4/6.
        Spec.it s "CR 702.23a whole card: Wolverine Pack blocked by two creatures is 4/6" $ do
          (gs, mine, _) <- board ["Wolverine Pack"] ["Goblin Piker", "Hill Giant"]
          case mine of
            [pack] -> Spec.assertEqWith s "2/4 plus one blocker beyond the first, twice" (S.powerToughnessOf pack (atDamage gs)) (Just (4, 6))
            _ -> Spec.assertFailure s "fixture should give alice one Pack"
        -- A THIRD blocker is a second creature beyond the first, so the bonus
        -- doubles rather than growing by one. The falsifier is a bonus that adds 1
        -- per creature beyond the first instead of N, which reads 4/6 here.
        Spec.it s "CR 702.23a a third blocker is a second +2/+2" $ do
          (gs, mine, _) <- board ["Wolverine Pack"] ["Goblin Piker", "Hill Giant", "Icehide Golem"]
          case mine of
            [pack] -> Spec.assertEqWith s "2/4 plus two beyond the first, twice" (S.powerToughnessOf pack (atDamage gs)) (Just (6, 8))
            _ -> Spec.assertFailure s "fixture should give alice one Pack"
        -- "BEYOND THE FIRST": one blocker is a trigger with a bonus of 0, not a
        -- bonus of N. The falsifier is a bonus counting blockers outright, which
        -- reads 4/6 here.
        Spec.it s "CR 702.23a one blocker leaves the Pack a 2/4" $ do
          (gs, mine, _) <- board ["Wolverine Pack"] ["Goblin Piker"]
          case mine of
            [pack] -> Spec.assertEqWith s "the first blocker is not beyond the first" (S.powerToughnessOf pack (atDamage gs)) (Just (2, 4))
            _ -> Spec.assertFailure s "fixture should give alice one Pack"
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
                          [ ()
                          | GameEvent.AbilityTriggered record <- S.eventsOf after,
                            AbilityTriggered.source record == TriggerSource.OfObject pack,
                            AbilityTriggered.controller record == S.alice,
                            TriggeredAbility.condition (AbilityTriggered.ability record) == TriggerCondition.SelfBecomesBlocked
                          ]
                      )
              Spec.assertBool s (not (fired (S.runToStep (Phase.Combat CombatStep.CombatDamage) noBlocks gs))) "nothing blocked, so nothing triggered"
              Spec.assertBool s (fired (atDamage gs)) "and the same board with the block taken does trigger"
            _ -> Spec.assertFailure s "fixture should give alice one Pack"
        -- N is the card's, not the engine's: rampage 1 against the same three
        -- blockers is +2/+2 where rampage 2 was +4/+4.
        Spec.it s "CR 702.23a rampage 1 on the same board is half the bonus" $ do
          (gs, mine, _) <- board ["Horrible Hordes"] ["Goblin Piker", "Hill Giant", "Icehide Golem"]
          case mine of
            [hordes] -> Spec.assertEqWith s "2/2 plus two beyond the first, once" (S.powerToughnessOf hordes (atDamage gs)) (Just (4, 4))
            _ -> Spec.assertFailure s "fixture should give alice one Hordes"
        -- CR 702.23b: the bonus is calculated as the ability RESOLVES and does not
        -- move afterwards. CR 511.3 clears Combat.blockers at end of combat, so a
        -- bonus that re-read the declaration live would fall back to +0/+0 the
        -- moment combat ended, while the printed duration runs to end of turn. The
        -- Pack is a 6/8 taking 7, so it survives to be read.
        Spec.it s "CR 702.23b the bonus outlives the blockers it was counted from" $ do
          (gs, mine, _) <- board ["Wolverine Pack"] ["Goblin Piker", "Hill Giant", "Icehide Golem"]
          case mine of
            [pack] -> do
              let after = S.runToStep Phase.PostcombatMain S.aggressiveAnswer gs
              Spec.assertEqWith s "CR 511.3 the declaration is cleared" (Combat.Type.blockers (GameState.combat after)) Map.empty
              Spec.assertEqWith s "and the Pack is still a 6/8" (S.powerToughnessOf pack after) (Just (6, 8))
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
  vanishingSpec s registry
  numberlessVanishingSpec s registry
  fadingSpec s registry
  cumulativeUpkeepSpec s registry
  modularSpec s registry
  tovolarSpec s registry
  piaNalaarSpec s registry
  aragornSpec s registry
  shroofusSpec s registry
  afflictSpec s registry
  meleeSpec s registry
  dethroneSpec s registry
  marchesasDecreeSpec s registry
  rampageSpec s registry
