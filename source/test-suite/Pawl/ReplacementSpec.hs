{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Replacement (the CR 616.1 loop, its buckets and its prompt) and
-- the funnels that raise proposed events through it. Mostly gameplay-level --
-- put a board together, cast or resolve, assert on game state -- but a case
-- reaches for a more direct construction whenever gameplay cannot produce the
-- exact shape the property under test needs: driving the loop on hand-built
-- ProposedEvent values (CR 614.5, where the card pool has only one
-- replacement-bearing printing and no gameplay board can produce two
-- independently-sourced, mutually-applicable redirects), hand-building a
-- DamageEvent batch after a real cast (CR 615.10 Fog, where reaching one
-- through combat would mean driving an entire combat phase the property does
-- not need), or seeding GameState.combat's attacker map directly (CR 701.19a,
-- the same shortcut Support.addRegenShield takes for the shield itself,
-- because declaring a legal attacker needs a full combat phase too). Each such
-- case says so at the point it happens, rather than here.
module Pawl.ReplacementSpec where

import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Activate as Activate
import qualified Pawl.Cards as Cards
import qualified Pawl.Cast as Cast
import qualified Pawl.Damage as Damage
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Replacement as Replacement
import qualified Pawl.Replay as Replay
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.AbilityCost as AbilityCost
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.AttackTarget as AttackTarget
import qualified Pawl.Type.CandidateId as CandidateId
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.Combat as Combat
import qualified Pawl.Type.ControllerRelation as ControllerRelation
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.DamageKind as DamageKind
import qualified Pawl.Type.Game as Game.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.ProposedEvent as ProposedEvent
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.ReplacementCandidate as ReplacementCandidate
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.Response as Response
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Pawl.Type.ZoneChangePattern as ZoneChangePattern
import qualified System.Timeout as Timeout
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Every answer the engine asked for, in order -- so a test can assert that a
-- prompt WAS raised (the engine did not decide) or was NOT (the choice was
-- indistinguishable and correctly elided).
answersFor :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> Game.Type.Game a -> [Response.Response]
answersFor answer gs game = snd (Replay.record answer gs game)

-- The single activated ability of a printing (Drudge Skeletons has exactly
-- one). Total: the empty-ability fallback is unreachable in this fixture.
-- Same shape as ActivateSpec.theAbility -- duplicated per this test suite's
-- existing convention of group-local helpers (ActivateSpec and ManaSpec
-- already duplicate singleModeAbility the same way) rather than centralizing
-- a helper this small in Support.
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Card
theAbility p = case Card.activatedAbilities (Printing.card p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (AbilityCost.MkAbilityCost Nothing []) (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1))

wasAskedToReplace :: [Response.Response] -> Bool
wasAskedToReplace responses =
  let isReplacement r = case r of
        Response.ChoseReplacement _ -> True
        _ -> False
   in any isReplacement responses

-- alice controls one Forest plus `mine`; bob controls `theirs`; alice holds one
-- Battlegrowth ({G} instant: put a +1/+1 counter on target creature). Returns the
-- state, Battlegrowth's hand id, and the two id lists in the order given.
counterBoard :: Cards.Cards -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId], [ObjectId.ObjectId])
counterBoard cards mine theirs =
  let addAll pid ps gs =
        List.foldl'
          (\(ids, g) p -> let (oid, g1) = S.addCreature p pid g in (ids ++ [oid], g1))
          ([], gs)
          ps
      (ours, gs1) = addAll S.alice mine (S.landsInPlay (Cards.forestPrinting cards) 1)
      (yours, gs2) = addAll S.bob theirs gs1
      (gs3, spellId) = S.handOne (Cards.battlegrowthPrinting cards) gs2
   in (gs3, spellId, ours, yours)

-- Aim every target slot at `victim`, and answer a CR 616.1 race by picking the
-- candidate whose SOURCE is `preferred` -- by id, so the assertion does not
-- depend on the engine's canonical candidate order.
raceAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
raceAnswer preferred victim p = case p of
  Prompt.ChooseReplacement _ _ sources -> maybe 0 fromIntegral (List.elemIndex preferred sources)
  Prompt.ChooseTargets _ _ _ sets -> Map.map (const (Recipient.ToCreature victim)) sets
  _ -> S.identityAnswer p

countersOn :: CounterKind.CounterKind -> ObjectId.ObjectId -> GameState.GameState -> Natural.Natural
countersOn kind oid gs =
  maybe 0 (Map.findWithDefault 0 kind . Object.counters) (Game.lookupObject oid gs)

castAndResolve :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
castAndResolve answer gs spellId =
  S.runPure answer gs (Cast.castSpell S.alice spellId >> Stack.resolveTop)

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Pawl.Replacement"
    [ -- NOT a CR 614.5 test: this does not exercise the applied set at all. After
      -- the first Rest in Peace redirects the event to Exile, the SECOND Rest in
      -- Peace's pattern (whenDestination = Graveyard) no longer matches the
      -- rewritten event, so `applies` alone -- not CR 614.5's applied-set --
      -- is what stops the second application. Deleting the applied-set logic
      -- from `loop` entirely leaves this test passing. What it actually proves:
      -- a redirect whose output no longer matches its own `whenDestination`
      -- cannot re-fire. See "CR 614.5 the applied set ..." below for the real
      -- 614.5 coverage.
      HU.testCase "CR 614.1a a redirect that no longer matches its own pattern cannot re-fire" $
        let (_, g0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (_, g1) = S.addCreature (Cards.restInPeacePrinting cards) S.alice g0
            (piker, g2) = S.addPiker cards S.bob g1
            after = S.runPure S.identityAnswer g2 (Event.changeZone piker Zone.Graveyard)
         in do
              HU.assertEqual "not in a graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after))
              HU.assertEqual "exactly one object in exile" 1 (Set.size (GameState.exile after)),
      HU.testCase "CR 616.1 value-equal candidates elide the prompt (nothing to choose)" $
        let (_, g0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (_, g1) = S.addCreature (Cards.restInPeacePrinting cards) S.alice g0
            (piker, g2) = S.addPiker cards S.bob g1
            asked = answersFor S.identityAnswer g2 (Event.changeZone piker Zone.Graveyard)
         in HU.assertBool "no ChooseReplacement was raised" (not (wasAskedToReplace asked)),
      HU.testCase "CR 614.1a a move whose destination the pattern misses is untouched" $
        let (_, g0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (piker, g1) = S.addPiker cards S.bob g0
            -- Rest in Peace watches graveyard-bound moves only; a bounce to hand
            -- is not one, so the loop finds no candidate and the move stands.
            after = S.runPure S.identityAnswer g1 (Event.changeZone piker Zone.Hand)
         in do
              HU.assertEqual "in bob's hand" 1 (length (Game.zoneMembers Zone.Hand S.bob after))
              HU.assertEqual "nothing was exiled" 0 (Set.size (GameState.exile after)),
      -- CR 614.5's actual mechanism, driven directly against Pawl.Replacement --
      -- the card pool has exactly one replacement-bearing printing (Rest in
      -- Peace), never two independently-sourced redirects that stay applicable
      -- to their own output, so gameplay-level coverage cannot reach this. Two
      -- candidates EQUAL AS VALUES (same ZoneChangeR effect) but with different
      -- SOURCES, on a SYNTHETIC PRINTING -- the Card value carries Rest in
      -- Peace's real name and trigger, but a fabricated Graveyard -> Graveyard
      -- replacement effect no real card in the pool prints -- redirecting to
      -- the very zone their own pattern watches, so,
      -- unlike the test above, applying one does NOT make the event stop
      -- matching the other's pattern. Both stay applicable to the rewritten
      -- event FOREVER unless CR 614.5's applied set excludes each one after its
      -- own turn. Verified by hand (temporarily replacing loop's `unused`
      -- filter with `const True`) that this hangs `loop` -- confirming the
      -- timeout below is not vacuous. Retire this synthetic printing once the
      -- Hardened Scales case lands, two tasks from now: two Hardened Scales and
      -- one +1/+1 counter must yield 3 -- not 2, and not a hang -- which covers
      -- BOTH halves of CR 614.5 (termination and one-opportunity-each) at
      -- gameplay level with a real card, while this synthetic printing can only
      -- cover the termination half (#N).
      HU.testCase "CR 614.5 the applied set gives two value-equal candidates one turn each, and only one" $
        let selfRedirect =
              ReplacementEffect.ZoneChangeR
                (ZoneChangePattern.MkZoneChangePattern Zone.Graveyard ControllerRelation.Anyones)
                Zone.Graveyard
            baseCard = Printing.card (Cards.restInPeacePrinting cards)
            printing = Printing.MkPrinting baseCard {Card.replacementEffects = [selfRedirect]}
            (src1, g0) = S.addCreature printing S.alice (Setup.emptyGame S.bothPlayers)
            (src2, g1) = S.addCreature printing S.alice g0
            (piker, g2) = S.addPiker cards S.bob g1
            zc = ZoneChange.MkZoneChange piker Zone.Battlefield Zone.Graveyard
            event = ProposedEvent.WouldChangeZone zc
            id1 = CandidateId.OfPermanent src1 selfRedirect
            id2 = CandidateId.OfPermanent src2 selfRedirect
            before = Replacement.applicable g2 event
            -- collect (and so applicable) lists battlefield permanents ascending
            -- by id (its own comment); src1 was placed before src2, so it sorts
            -- first.
            run = fst (Engine.runGamePure S.identityAnswer g2 (Replacement.loop Set.empty event))
         in do
              HU.assertEqual
                "both candidates apply before either fires -- this is not the pattern-mismatch escape above"
                [id1, id2]
                (map ReplacementCandidate.identity before)
              settled <- Timeout.timeout 2000000 (Exception.evaluate run)
              case settled of
                Nothing -> HU.assertFailure "loop did not terminate -- CR 614.5's applied set is not stopping re-application"
                Just outcome -> HU.assertEqual "the event survives, settled back at the zone both candidates watch" (Just event) outcome,
      HU.testCase "CR 615.10 Fog prevents both attackers' damage in one batch" $
        let base = S.landsInPlay (Cards.forestPrinting cards) 1
            (victimA, g1) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            (victimB, g2) = S.addCreature (Cards.pikerPrinting cards) S.bob g1
            (g3, fogId) = S.handOne (Cards.fogPrinting cards) g2
            resolved = S.runPure S.identityAnswer g3 (Cast.castSpell S.alice fogId >> Stack.resolveTop)
            batch =
              [ DamageEvent.MkDamageEvent victimA (Recipient.ToCreature victimA) 2 False DamageKind.Combat,
                DamageEvent.MkDamageEvent victimB (Recipient.ToCreature victimB) 2 False DamageKind.Combat
              ]
            after = S.runPure S.identityAnswer resolved (Damage.applyDamage batch)
         in do
              HU.assertEqual "the first attacker's damage was prevented" (Just 0) (S.damageOf victimA after)
              HU.assertEqual "and so was the second's, independently" (Just 0) (S.damageOf victimB after)
              HU.assertEqual "no damage event was recorded at all" [] (S.damageEventsOf after),
      HU.testCase "CR 701.19a Uses=Once: the first destruction is replaced, the second is not" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 1
            (skel, g1) = S.addCreature (Cards.drudgeSkeletonsPrinting cards) S.alice base
            -- Activate {B}: regenerate this creature, and resolve it.
            armed = S.runPure S.identityAnswer g1 (Activate.activateAbility S.alice skel (theAbility (Cards.drudgeSkeletonsPrinting cards)) >> Stack.resolveTop)
            -- CR 701.19a's "remove it from combat" half needs the creature
            -- actually attacking. Driving a full combat phase to reach a legal
            -- attack is disproportionate to what this asserts, so seed
            -- GameState.combat's attacker map directly -- the same shortcut
            -- Support.addRegenShield takes for the shield itself.
            attacking = armed {GameState.combat = (GameState.combat armed) {Combat.attackers = Map.singleton skel (AttackTarget.OfPlayer S.bob)}}
            once = S.runPure S.identityAnswer attacking (Event.destroy skel)
            twice = S.runPure S.identityAnswer once (Event.destroy skel)
         in do
              HU.assertBool "combat started with no attackers" (Map.null (Combat.attackers (GameState.combat armed)))
              HU.assertBool "survived the first destruction" (Set.member skel (GameState.battlefield once))
              HU.assertEqual "the shield was spent" [] (GameState.replacements once)
              HU.assertBool "removed from combat by the regeneration (CR 701.19a)" (not (Map.member skel (Combat.attackers (GameState.combat once))))
              HU.assertBool "the second destruction kills it" (not (Set.member skel (GameState.battlefield twice))),
      HU.testCase "CR 614.8 regeneration replaces the destruction, so Rest in Peace never sees it" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 1
            (_, g1) = S.addCreature (Cards.restInPeacePrinting cards) S.bob base
            (skel, g2) = S.addCreature (Cards.drudgeSkeletonsPrinting cards) S.alice g1
            shielded = S.addRegenShield skel g2
            after = S.runPure S.identityAnswer shielded (Event.destroy skel)
         in do
              HU.assertBool "still on the battlefield" (Set.member skel (GameState.battlefield after))
              HU.assertEqual "nothing was exiled -- the put-into-graveyard never happened" 0 (Set.size (GameState.exile after))
              HU.assertEqual "and nothing reached a graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      HU.testCase "CR 614.7 an event that never happens does not consume a shield" $
        let base = Setup.emptyGame S.bothPlayers
            (myr, g1) = S.addCreature (Cards.darksteelMyrPrinting cards) S.alice base
            shielded = S.addRegenShield myr g1
            after = S.runPure S.identityAnswer shielded (Event.destroy myr)
         in do
              HU.assertBool "the indestructible creature survives" (Set.member myr (GameState.battlefield after))
              HU.assertEqual "the shield is intact" 1 (length (GameState.replacements after)),
      HU.testCase "CR 616.1 Scales first, then Corpsejack: 1 -> 2 -> 4" $
        let (gs, spellId, mine, _) = counterBoard cards [Cards.hardenedScalesPrinting cards, Cards.corpsejackMenacePrinting cards, Cards.pikerPrinting cards] []
         in case mine of
              scales : _ : piker : _ ->
                let after = castAndResolve (raceAnswer scales piker) gs spellId
                 in HU.assertEqual "(1 + 1) * 2" 4 (countersOn CounterKind.PlusOnePlusOne piker after)
              _ -> HU.assertFailure "fixture did not build three permanents",
      HU.testCase "CR 616.1 Corpsejack first, then Scales: 1 -> 2 -> 3 (same input, different board)" $
        let (gs, spellId, mine, _) = counterBoard cards [Cards.hardenedScalesPrinting cards, Cards.corpsejackMenacePrinting cards, Cards.pikerPrinting cards] []
         in case mine of
              _ : corpsejack : piker : _ ->
                let after = castAndResolve (raceAnswer corpsejack piker) gs spellId
                 in HU.assertEqual "(1 * 2) + 1" 3 (countersOn CounterKind.PlusOnePlusOne piker after)
              _ -> HU.assertFailure "fixture did not build three permanents",
      HU.testCase "CR 616.1 the engine ASKS -- it does not proceed on list order" $
        let (gs, spellId, mine, _) = counterBoard cards [Cards.hardenedScalesPrinting cards, Cards.corpsejackMenacePrinting cards, Cards.pikerPrinting cards] []
         in case mine of
              scales : _ : piker : _ ->
                let asked = answersFor (raceAnswer scales piker) gs (Cast.castSpell S.alice spellId >> Stack.resolveTop)
                 in HU.assertBool "a ChooseReplacement was raised" (wasAskedToReplace asked)
              _ -> HU.assertFailure "fixture did not build three permanents",
      HU.testCase "CR 616.1 one Hardened Scales alone is not asked about (nothing to choose)" $
        let (gs, spellId, mine, _) = counterBoard cards [Cards.hardenedScalesPrinting cards, Cards.pikerPrinting cards] []
         in case mine of
              scales : piker : _ ->
                let after = castAndResolve (raceAnswer scales piker) gs spellId
                    asked = answersFor (raceAnswer scales piker) gs (Cast.castSpell S.alice spellId >> Stack.resolveTop)
                 in do
                      HU.assertEqual "1 + 1" 2 (countersOn CounterKind.PlusOnePlusOne piker after)
                      HU.assertBool "no ChooseReplacement was raised" (not (wasAskedToReplace asked))
              _ -> HU.assertFailure "fixture did not build two permanents",
      HU.testCase "CR 614.5 two Hardened Scales are two instances: 1 -> 2 -> 3, unprompted" $
        let (gs, spellId, mine, _) = counterBoard cards [Cards.hardenedScalesPrinting cards, Cards.hardenedScalesPrinting cards, Cards.pikerPrinting cards] []
         in case mine of
              scales : _ : piker : _ ->
                let after = castAndResolve (raceAnswer scales piker) gs spellId
                    asked = answersFor (raceAnswer scales piker) gs (Cast.castSpell S.alice spellId >> Stack.resolveTop)
                 in do
                      HU.assertEqual "each gets its own opportunity" 3 (countersOn CounterKind.PlusOnePlusOne piker after)
                      HU.assertBool "value-equal candidates elide the prompt" (not (wasAskedToReplace asked))
              _ -> HU.assertFailure "fixture did not build three permanents",
      HU.testCase "CR 614.1 Hardened Scales ignores a -1/-1 counter (whichKind)" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 4
            (scales, g1) = S.addCreature (Cards.hardenedScalesPrinting cards) S.alice base
            (piker, g2) = S.addCreature (Cards.pikerPrinting cards) S.alice g1
            (g3, spellId) = S.handOne (Cards.instillInfectionPrinting cards) g2
            after = castAndResolve (raceAnswer scales piker) g3 spellId
         in HU.assertEqual "one -1/-1 counter, unscaled" 1 (countersOn CounterKind.MinusOneMinusOne piker after),
      HU.testCase "CR 109.5 Corpsejack Menace does not double an opponent's counters" $
        let (gs, spellId, mine, theirs) = counterBoard cards [Cards.corpsejackMenacePrinting cards] [Cards.pikerPrinting cards]
         in case (mine, theirs) of
              (corpsejack : _, piker : _) ->
                let after = castAndResolve (raceAnswer corpsejack piker) gs spellId
                 in HU.assertEqual "not doubled -- ControllerRelation is Yours" 1 (countersOn CounterKind.PlusOnePlusOne piker after)
              _ -> HU.assertFailure "fixture did not build both sides"
    ]
