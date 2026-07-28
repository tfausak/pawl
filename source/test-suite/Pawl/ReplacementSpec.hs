{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Replacement (the CR 616.1 loop, its buckets and its prompt) and
-- the funnels that raise proposed events through it. Mostly gameplay-level --
-- put a board together, cast or resolve, assert on game state -- but a case
-- reaches for a more direct construction whenever gameplay cannot produce the
-- exact shape the property under test needs. Where a case departs from
-- gameplay-level testing, it justifies itself at the point it happens, rather
-- than here.
module Pawl.ReplacementSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Activate as Activate
import qualified Pawl.Cast as Cast
import qualified Pawl.Damage as Damage
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Extra.Int as Int
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Replacement as Replacement
import qualified Pawl.Replay as Replay
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.ActivationTiming as ActivationTiming
import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.AttackTarget as AttackTarget
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Combat as Combat
import qualified Pawl.Type.ControllerRelation as ControllerRelation
import qualified Pawl.Type.Cost as Cost.Type
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.DamageKind as DamageKind
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.EntryOption as EntryOption
import qualified Pawl.Type.EntryRewrite as EntryRewrite
import qualified Pawl.Type.Expiry as Expiry
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Filter may later be imported and must not collide.
import qualified Pawl.Type.Filter as Filter.Type
import qualified Pawl.Type.Game as Game.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Optionality as Optionality
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Regenerability as Regenerability
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.Response as Response
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Timestamp as Timestamp
import qualified Pawl.Type.Uses as Uses
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Type.ZoneChangeSubject as ZoneChangeSubject
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
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1)) ActivationTiming.AnyTime

wasAskedToReplace :: [Response.Response] -> Bool
wasAskedToReplace responses =
  let isReplacement r = case r of
        Response.ChoseReplacement _ -> True
        _ -> False
   in any isReplacement responses

wasAskedForEntryOption :: [Response.Response] -> Bool
wasAskedForEntryOption responses =
  let isEntryOption r = case r of
        Response.ChoseEntryOption _ -> True
        _ -> False
   in any isEntryOption responses

-- alice controls one Forest plus `mine`; bob controls `theirs`; alice holds one
-- Battlegrowth ({G} instant: put a +1/+1 counter on target creature). Returns the
-- state, Battlegrowth's hand id, and the two id lists in the order given.
counterBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId], [ObjectId.ObjectId])
counterBoard forest battlegrowth mine theirs =
  let addAll pid ps gs =
        List.foldl'
          (\(ids, g) p -> let (oid, g1) = S.addCreature p pid g in (ids <> [oid], g1))
          ([], gs)
          ps
      (ours, gs1) = addAll S.alice mine (S.landsInPlay forest 1)
      (yours, gs2) = addAll S.bob theirs gs1
      (gs3, spellId) = S.handOne battlegrowth gs2
   in (gs3, spellId, ours, yours)

-- Aim every target slot at `victim`, and answer a CR 616.1 race by picking the
-- candidate whose SOURCE is `preferred` -- by id, so the assertion does not
-- depend on the engine's canonical candidate order.
raceAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
raceAnswer preferred victim p = case p of
  Prompt.ChooseReplacement _ _ sources -> maybe 0 Int.toNaturalSaturating (List.elemIndex preferred sources)
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToCreature victim)) sets
  _ -> S.identityAnswer p

countersOn :: CounterKind.CounterKind -> ObjectId.ObjectId -> GameState.GameState -> Natural.Natural
countersOn kind oid gs =
  maybe 0 (Map.findWithDefault 0 kind . Object.counters) (Game.lookupObject oid gs)

castAndResolve :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
castAndResolve answer gs spellId =
  S.runPure answer gs (Cast.castSpell S.alice spellId >> Stack.resolveTop)

-- Copy `wanted` when it is offered, decline otherwise.
copyOf :: ObjectId.ObjectId -> Prompt.Prompt r -> r
copyOf wanted p = case p of
  Prompt.ChooseCopyTarget _ _ _ legal -> if List.elem wanted legal then Just wanted else Nothing
  _ -> S.identityAnswer p

-- alice controls `n` untapped Islands in a main phase with priority, holding one
-- card of each printing in `hand`. Returns the state and the hand ids in order --
-- unlike S.handOne, which replaces the whole hand.
blueBoard :: Printing.Printing -> Int -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId])
blueBoard island n hand =
  let base = S.landsInPlay island n
      addOne (ids, g) p = let (oid, g1) = S.addHandCard p S.alice g in (ids <> [oid], g1)
      (held, gs) = List.foldl' addOne ([], base) hand
   in ( gs
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        held
      )

-- Pick entry option `which`, and copy the highest-id legal creature when offered.
enteringAs :: Natural.Natural -> Prompt.Prompt r -> r
enteringAs which p = case p of
  Prompt.ChooseEntryOption {} -> which
  Prompt.ChooseCopyTarget _ _ _ legal -> Maybe.listToMaybe (List.sortOn Ord.Down legal)
  _ -> S.identityAnswer p

-- The newest battlefield object whose printed card has this name.
newestNamed :: Text.Text -> GameState.GameState -> Maybe ObjectId.ObjectId
newestNamed wanted gs =
  let named oid = fmap Card.name (Game.cardOf oid gs) == Just wanted
   in Maybe.listToMaybe (List.sortOn Ord.Down (filter named (Set.toList (GameState.battlefield gs))))

-- Leyline of the Void's redirect, as a floating replacement: any card headed for
-- an OPPONENT's graveyard is exiled instead. CR 400.3 makes that graveyard the
-- card's OWNER's, which is what Replacement.matchesZoneOwner tests.
leylineShape :: ObjectId.ObjectId -> Timestamp.Timestamp -> ActiveReplacement.ActiveReplacement
leylineShape src ts =
  ActiveReplacement.MkActiveReplacement
    { ActiveReplacement.effect =
        ReplacementEffect.ZoneChangeR
          (ZoneChangePattern.MkZoneChangePattern Zone.Graveyard ControllerRelation.Opponents ZoneChangeSubject.AnyObject)
          Zone.Exile,
      ActiveReplacement.source = src,
      ActiveReplacement.timestamp = ts,
      ActiveReplacement.expiry = Expiry.Never,
      ActiveReplacement.uses = Uses.Unlimited
    }

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  -- CR 614.5's applied set is what makes the CR 616.1 loop TERMINATE, not merely
  -- correct: a regression there (an effect invoking itself repeatedly, e.g. two
  -- Hardened Scales re-triggering each other forever) manifests as this group
  -- hanging, not failing. "CR 614.5 two Hardened Scales are two instances" below
  -- is the case that asserts the CORRECTNESS half (each gets exactly one
  -- opportunity); this timeout is the safety net for the TERMINATION half -- it
  -- asserts nothing on a green run. Five seconds, not two: this guards against a
  -- hang, not a slowdown, and the group runs in ~0.01s today, so a tight bound
  -- would only risk becoming a CI flake.
  Tasty.localOption (Tasty.mkTimeout 5000000) $
    Tasty.testGroup
      "Pawl.Replacement"
      [ -- P9: a pattern's permanent match runs through the lower Pawl.Filter over
        -- the PROJECTED view, the same evaluator Pawl.Cost narrows sacrifices with
        -- (#111 retired). CR 205.2b/300.2/613.1d: creature-ness is projected; the
        -- trivial filter And [] matches every permanent (what AnyPermanent was).
        HU.testCase "CR 614.1 matchesPermanent narrows a permanent through Filter.matches" $ do
          swamp <- Registry.printing registry "Swamp"
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          let base = S.landsInPlay swamp 1
              (piker, g1) = S.addCreature pikerPrinting S.alice base
              land = case Set.toList (GameState.battlefield base) of
                oid : _ -> Just oid
                [] -> Nothing
          case land of
            Nothing -> HU.assertFailure "fixture did not build a land"
            Just landId -> do
              HU.assertBool "the creature matches HasCardType Creature" (Replacement.matchesPermanent g1 (Filter.Type.HasCardType CardType.Creature) piker)
              HU.assertBool "the land does not match HasCardType Creature" (not (Replacement.matchesPermanent g1 (Filter.Type.HasCardType CardType.Creature) landId))
              HU.assertBool "the trivial filter matches the land too" (Replacement.matchesPermanent g1 (Filter.Type.And []) landId),
        -- NOT a CR 614.5 test: this does not exercise the applied set at all. After
        -- the first Rest in Peace redirects the event to Exile, the SECOND Rest in
        -- Peace's pattern (whenDestination = Graveyard) no longer matches the
        -- rewritten event, so `applies` alone -- not CR 614.5's applied-set --
        -- is what stops the second application. Deleting the applied-set logic
        -- from `loop` entirely leaves this test passing. What it actually proves:
        -- a redirect whose output no longer matches its own `whenDestination`
        -- cannot re-fire. See "CR 614.5 the applied set ..." below for the real
        -- 614.5 coverage.
        HU.testCase "CR 614.1a a redirect that no longer matches its own pattern cannot re-fire" $ do
          restInPeace <- Registry.printing registry "Rest in Peace"
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          let (_, g0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
              (_, g1) = S.addCreature restInPeace S.alice g0
              (piker, g2) = S.addCreature pikerPrinting S.bob g1
              after = S.runPure S.identityAnswer g2 (Event.changeZone piker Zone.Graveyard)
          HU.assertEqual "not in a graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after))
          HU.assertEqual "exactly one object in exile" 1 (Set.size (GameState.exile after)),
        HU.testCase "CR 616.1 value-equal candidates elide the prompt (nothing to choose)" $ do
          restInPeace <- Registry.printing registry "Rest in Peace"
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          let (_, g0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
              (_, g1) = S.addCreature restInPeace S.alice g0
              (piker, g2) = S.addCreature pikerPrinting S.bob g1
              asked = answersFor S.identityAnswer g2 (Event.changeZone piker Zone.Graveyard)
          HU.assertBool "no ChooseReplacement was raised" (not (wasAskedToReplace asked)),
        HU.testCase "CR 614.1a a move whose destination the pattern misses is untouched" $ do
          restInPeace <- Registry.printing registry "Rest in Peace"
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          let (_, g0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
              (piker, g1) = S.addCreature pikerPrinting S.bob g0
              -- Rest in Peace watches graveyard-bound moves only; a bounce to hand
              -- is not one, so the loop finds no candidate and the move stands.
              after = S.runPure S.identityAnswer g1 (Event.changeZone piker Zone.Hand)
          HU.assertEqual "in bob's hand" 1 (length (Game.zoneMembers Zone.Hand S.bob after))
          HU.assertEqual "nothing was exiled" 0 (Set.size (GameState.exile after)),
        HU.testCase "CR 615.10 Fog prevents both attackers' damage in one batch" $ do
          forest <- Registry.printing registry "Forest"
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          fog <- Registry.printing registry "Fog"
          let base = S.landsInPlay forest 1
              (victimA, g1) = S.addCreature pikerPrinting S.bob base
              (victimB, g2) = S.addCreature pikerPrinting S.bob g1
              (g3, fogId) = S.handOne fog g2
              resolved = S.runPure S.identityAnswer g3 (Cast.castSpell S.alice fogId >> Stack.resolveTop)
              -- Hand-built rather than driven through real combat: reaching a real
              -- combat-damage batch would mean driving an entire combat phase, which
              -- this assertion (Fog prevents a whole batch, not just one event) does
              -- not need.
              batch =
                [ DamageEvent.MkDamageEvent victimA (Recipient.ToCreature victimA) 2 False False 0 DamageKind.Combat,
                  DamageEvent.MkDamageEvent victimB (Recipient.ToCreature victimB) 2 False False 0 DamageKind.Combat
                ]
              after = S.runPure S.identityAnswer resolved (Damage.applyDamage batch)
          HU.assertEqual "the first attacker's damage was prevented" (Just 0) (S.damageOf victimA after)
          HU.assertEqual "and so was the second's, independently" (Just 0) (S.damageOf victimB after)
          HU.assertEqual "no damage event was recorded at all" [] (S.damageEventsOf after),
        HU.testCase "CR 701.19a Uses=Once: the first destruction is replaced, the second is not" $ do
          swamp <- Registry.printing registry "Swamp"
          drudgeSkeletons <- Registry.printing registry "Drudge Skeletons"
          let base = S.landsInPlay swamp 1
              (skel, g1) = S.addCreature drudgeSkeletons S.alice base
              -- Activate {B}: regenerate this creature, and resolve it.
              armed = S.runPure S.identityAnswer g1 (Activate.activateAbility S.alice skel (theAbility drudgeSkeletons) >> Stack.resolveTop)
              -- CR 701.19a's "remove it from combat" half needs the creature
              -- actually attacking. Driving a full combat phase to reach a legal
              -- attack is disproportionate to what this asserts, so seed
              -- GameState.combat's attacker map directly -- the same shortcut
              -- Support.addRegenShield takes for the shield itself.
              attacking = armed {GameState.combat = (GameState.combat armed) {Combat.attackers = Map.singleton skel (AttackTarget.OfPlayer S.bob)}}
              once = S.runPure S.identityAnswer attacking (Event.destroy Regenerability.Regenerable skel)
              twice = S.runPure S.identityAnswer once (Event.destroy Regenerability.Regenerable skel)
          HU.assertBool "combat started with no attackers" (Map.null (Combat.attackers (GameState.combat armed)))
          HU.assertBool "survived the first destruction" (Set.member skel (GameState.battlefield once))
          HU.assertEqual "the shield was spent" [] (GameState.replacements once)
          HU.assertBool "removed from combat by the regeneration (CR 701.19a)" (not (Map.member skel (Combat.attackers (GameState.combat once))))
          HU.assertBool "the second destruction kills it" (not (Set.member skel (GameState.battlefield twice))),
        -- CR 701.19c: "Effects that say that a permanent can't be regenerated
        -- don't preclude such abilities from being activated or such spells from
        -- being cast; rather, they cause regeneration shields to not be applied."
        -- So the shield still exists -- it simply does not fire.
        HU.testCase "CR 701.19c a shield does not save a creature from a destruction that forbids regeneration" $ do
          swamp <- Registry.printing registry "Swamp"
          drudgeSkeletons <- Registry.printing registry "Drudge Skeletons"
          let (skel, g1) = S.addCreature drudgeSkeletons S.alice (S.landsInPlay swamp 1)
              shielded = S.addRegenShield skel g1
              after = S.runPure S.identityAnswer shielded (Event.destroy Regenerability.CantBeRegenerated skel)
          HU.assertBool "it died anyway" (not (Set.member skel (GameState.battlefield after)))
          -- CR 701.19c again, and the sharp half: an unapplied shield is not a
          -- spent one. Nothing consumed it, because it was never chosen.
          HU.assertEqual "and the shield was not consumed" (length (GameState.replacements shielded)) (length (GameState.replacements after)),
        -- The discriminating twin: identical creature, identical shield, and the
        -- only difference is whether the destruction forbids regeneration. This
        -- fails if the gate is ignored, and equally if it is applied to every
        -- destruction.
        HU.testCase "CR 701.19a the same shield DOES save it from an ordinary destruction" $ do
          swamp <- Registry.printing registry "Swamp"
          drudgeSkeletons <- Registry.printing registry "Drudge Skeletons"
          let (skel, g1) = S.addCreature drudgeSkeletons S.alice (S.landsInPlay swamp 1)
              shielded = S.addRegenShield skel g1
              after = S.runPure S.identityAnswer shielded (Event.destroy Regenerability.Regenerable skel)
          HU.assertBool "it survived" (Set.member skel (GameState.battlefield after))
          HU.assertEqual "and this time the shield was spent" [] (GameState.replacements after),
        -- The gameplay-level proof (design.md section 4): real cards, cast and
        -- resolved. Uthden Troll rather than Drudge Skeletons because Terror
        -- cannot target a black creature -- the Troll is red.
        HU.testCase "CR 701.19c whole cards: Terror kills an Uthden Troll that just regenerated" $ do
          mountain <- Registry.printing registry "Mountain"
          swamp <- Registry.printing registry "Swamp"
          uthdenTroll <- Registry.printing registry "Uthden Troll"
          terror <- Registry.printing registry "Terror"
          let base = foldl (\gs p -> snd (S.addCreature p S.alice gs)) (Setup.emptyGame S.bothPlayers) [mountain, swamp, swamp]
              (troll, g1) = S.addCreature uthdenTroll S.alice base
              -- {R}: Regenerate this creature -- the shield is really activated.
              armed = S.runPure S.identityAnswer g1 (Activate.activateAbility S.alice troll (theAbility uthdenTroll) >> Stack.resolveTop)
              (withTerror, spell) = S.handOne terror armed
              afterCast = S.runPure S.identityAnswer withTerror (Cast.castSpell S.alice spell)
              resolved = S.runPure S.identityAnswer afterCast Stack.resolveTop
          HU.assertBool "the shield really was created" (not (null (GameState.replacements armed)))
          HU.assertBool "and Terror killed the Troll through it" (not (Set.member troll (GameState.battlefield resolved))),
        -- The twin of the whole-card test: the SAME creature and the SAME shield,
        -- destroyed by the CR 704.5g state-based action instead, which carries no
        -- such clause. Regeneration is exactly what it is for.
        HU.testCase "CR 701.19a an Uthden Troll's shield still saves it from lethal damage" $ do
          mountain <- Registry.printing registry "Mountain"
          uthdenTroll <- Registry.printing registry "Uthden Troll"
          let base = S.landsInPlay mountain 1
              (troll, g1) = S.addCreature uthdenTroll S.alice base
              armed = S.runPure S.identityAnswer g1 (Activate.activateAbility S.alice troll (theAbility uthdenTroll) >> Stack.resolveTop)
              -- 2 damage is lethal to a 2/2.
              hurt = S.runPure S.identityAnswer armed (Damage.applyDamage [DamageEvent.MkDamageEvent troll (Recipient.ToCreature troll) 2 False False 0 DamageKind.Combat])
              settled = S.settleSba hurt
          HU.assertBool "the shield saved it" (Set.member troll (GameState.battlefield settled)),
        HU.testCase "CR 614.8 regeneration replaces the destruction, so Rest in Peace never sees it" $ do
          swamp <- Registry.printing registry "Swamp"
          restInPeace <- Registry.printing registry "Rest in Peace"
          drudgeSkeletons <- Registry.printing registry "Drudge Skeletons"
          let base = S.landsInPlay swamp 1
              (_, g1) = S.addCreature restInPeace S.bob base
              (skel, g2) = S.addCreature drudgeSkeletons S.alice g1
              shielded = S.addRegenShield skel g2
              after = S.runPure S.identityAnswer shielded (Event.destroy Regenerability.Regenerable skel)
          HU.assertBool "still on the battlefield" (Set.member skel (GameState.battlefield after))
          HU.assertEqual "nothing was exiled -- the put-into-graveyard never happened" 0 (Set.size (GameState.exile after))
          HU.assertEqual "and nothing reached a graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
        HU.testCase "CR 614.7 an event that never happens does not consume a shield" $ do
          darksteelMyr <- Registry.printing registry "Darksteel Myr"
          let base = Setup.emptyGame S.bothPlayers
              (myr, g1) = S.addCreature darksteelMyr S.alice base
              shielded = S.addRegenShield myr g1
              after = S.runPure S.identityAnswer shielded (Event.destroy Regenerability.Regenerable myr)
          HU.assertBool "the indestructible creature survives" (Set.member myr (GameState.battlefield after))
          HU.assertEqual "the shield is intact" 1 (length (GameState.replacements after)),
        HU.testCase "CR 616.1 Scales first, then Corpsejack: 1 -> 2 -> 4" $ do
          forest <- Registry.printing registry "Forest"
          battlegrowth <- Registry.printing registry "Battlegrowth"
          hardenedScales <- Registry.printing registry "Hardened Scales"
          corpsejackMenace <- Registry.printing registry "Corpsejack Menace"
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          let (gs, spellId, mine, _) = counterBoard forest battlegrowth [hardenedScales, corpsejackMenace, pikerPrinting] []
          case mine of
            scales : _ : piker : _ ->
              let after = castAndResolve (raceAnswer scales piker) gs spellId
               in HU.assertEqual "(1 + 1) * 2" 4 (countersOn CounterKind.PlusOnePlusOne piker after)
            _ -> HU.assertFailure "fixture did not build three permanents",
        HU.testCase "CR 616.1 Corpsejack first, then Scales: 1 -> 2 -> 3 (same input, different board)" $ do
          forest <- Registry.printing registry "Forest"
          battlegrowth <- Registry.printing registry "Battlegrowth"
          hardenedScales <- Registry.printing registry "Hardened Scales"
          corpsejackMenace <- Registry.printing registry "Corpsejack Menace"
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          let (gs, spellId, mine, _) = counterBoard forest battlegrowth [hardenedScales, corpsejackMenace, pikerPrinting] []
          case mine of
            _ : corpsejack : piker : _ ->
              let after = castAndResolve (raceAnswer corpsejack piker) gs spellId
               in HU.assertEqual "(1 * 2) + 1" 3 (countersOn CounterKind.PlusOnePlusOne piker after)
            _ -> HU.assertFailure "fixture did not build three permanents",
        HU.testCase "CR 616.1 the engine ASKS -- it does not proceed on list order" $ do
          forest <- Registry.printing registry "Forest"
          battlegrowth <- Registry.printing registry "Battlegrowth"
          hardenedScales <- Registry.printing registry "Hardened Scales"
          corpsejackMenace <- Registry.printing registry "Corpsejack Menace"
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          let (gs, spellId, mine, _) = counterBoard forest battlegrowth [hardenedScales, corpsejackMenace, pikerPrinting] []
          case mine of
            scales : _ : piker : _ ->
              let asked = answersFor (raceAnswer scales piker) gs (Cast.castSpell S.alice spellId >> Stack.resolveTop)
               in HU.assertBool "a ChooseReplacement was raised" (wasAskedToReplace asked)
            _ -> HU.assertFailure "fixture did not build three permanents",
        HU.testCase "CR 616.1 one Hardened Scales alone is not asked about (nothing to choose)" $ do
          forest <- Registry.printing registry "Forest"
          battlegrowth <- Registry.printing registry "Battlegrowth"
          hardenedScales <- Registry.printing registry "Hardened Scales"
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          let (gs, spellId, mine, _) = counterBoard forest battlegrowth [hardenedScales, pikerPrinting] []
          case mine of
            scales : piker : _ ->
              let after = castAndResolve (raceAnswer scales piker) gs spellId
                  asked = answersFor (raceAnswer scales piker) gs (Cast.castSpell S.alice spellId >> Stack.resolveTop)
               in do
                    HU.assertEqual "1 + 1" 2 (countersOn CounterKind.PlusOnePlusOne piker after)
                    HU.assertBool "no ChooseReplacement was raised" (not (wasAskedToReplace asked))
            _ -> HU.assertFailure "fixture did not build two permanents",
        HU.testCase "CR 614.5 two Hardened Scales are two instances: 1 -> 2 -> 3, unprompted" $ do
          forest <- Registry.printing registry "Forest"
          battlegrowth <- Registry.printing registry "Battlegrowth"
          hardenedScales <- Registry.printing registry "Hardened Scales"
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          let (gs, spellId, mine, _) = counterBoard forest battlegrowth [hardenedScales, hardenedScales, pikerPrinting] []
          case mine of
            scales : _ : piker : _ ->
              let after = castAndResolve (raceAnswer scales piker) gs spellId
                  asked = answersFor (raceAnswer scales piker) gs (Cast.castSpell S.alice spellId >> Stack.resolveTop)
               in do
                    HU.assertEqual "each gets its own opportunity" 3 (countersOn CounterKind.PlusOnePlusOne piker after)
                    HU.assertBool "value-equal candidates elide the prompt" (not (wasAskedToReplace asked))
            _ -> HU.assertFailure "fixture did not build three permanents",
        HU.testCase "CR 614.1 Hardened Scales ignores a -1/-1 counter (whichKind)" $ do
          swamp <- Registry.printing registry "Swamp"
          hardenedScales <- Registry.printing registry "Hardened Scales"
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          instillInfection <- Registry.printing registry "Instill Infection"
          let base = S.landsInPlay swamp 4
              (scales, g1) = S.addCreature hardenedScales S.alice base
              (piker, g2) = S.addCreature pikerPrinting S.alice g1
              (g3, spellId) = S.handOne instillInfection g2
              after = castAndResolve (raceAnswer scales piker) g3 spellId
          HU.assertEqual "one -1/-1 counter, unscaled" 1 (countersOn CounterKind.MinusOneMinusOne piker after),
        HU.testCase "CR 109.5 Corpsejack Menace does not double an opponent's counters" $ do
          forest <- Registry.printing registry "Forest"
          battlegrowth <- Registry.printing registry "Battlegrowth"
          corpsejackMenace <- Registry.printing registry "Corpsejack Menace"
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          let (gs, spellId, mine, theirs) = counterBoard forest battlegrowth [corpsejackMenace] [pikerPrinting]
          case (mine, theirs) of
            (corpsejack : _, piker : _) ->
              let after = castAndResolve (raceAnswer corpsejack piker) gs spellId
               in HU.assertEqual "not doubled -- ControllerRelation is Yours" 1 (countersOn CounterKind.PlusOnePlusOne piker after)
            _ -> HU.assertFailure "fixture did not build both sides",
        HU.testCase "CR 707.5 declining the copy leaves a 0/0 that dies (CR 704.5f)" $ do
          island <- Registry.printing registry "Island"
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          clone <- Registry.printing registry "Clone"
          let base = S.landsInPlay island 4
              (_, withPiker) = S.addCreature pikerPrinting S.alice base
              (gs, cloneId) = S.handOne clone withPiker
              -- S.identityAnswer declines ChooseCopyTarget (Clone's own "may").
              resolved = S.runPure S.identityAnswer gs (Cast.castSpell S.alice cloneId >> Stack.resolveTop >> Engine.settleForPriority)
              named = filter (\oid -> fmap Card.name (Game.cardOf oid resolved) == Just (Text.pack "Clone")) (Set.toList (GameState.battlefield resolved))
          HU.assertEqual "the 0/0 Clone is gone" [] named,
        HU.testCase "CR 614.12a the copy choice is locked in BEFORE the enters event exists" $ do
          island <- Registry.printing registry "Island"
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          clonePrinting <- Registry.printing registry "Clone"
          let base = S.landsInPlay island 4
              (piker, withPiker) = S.addCreature pikerPrinting S.alice base
              (gs, cloneId) = S.handOne clonePrinting withPiker
              -- No settle: the choice must already be made when resolveTop returns.
              resolved = S.runPure (copyOf piker) gs (Cast.castSpell S.alice cloneId >> Stack.resolveTop)
              named = filter (\oid -> fmap Card.name (Game.cardOf oid resolved) == Just (Text.pack "Clone")) (Set.toList (GameState.battlefield resolved))
          case named of
            [] -> HU.assertFailure "Clone did not reach the battlefield"
            clone : _ -> HU.assertEqual "already a 2/1, with no settle run" (Just 2) (Projection.powerOf clone resolved),
        HU.testCase "CR 208.2b Primal Plasma enters as the 2/2 with flying its controller picked" $ do
          island <- Registry.printing registry "Island"
          primalPlasma <- Registry.printing registry "Primal Plasma"
          let (gs, held) = blueBoard island 4 [primalPlasma]
          case held of
            plasmaCard : _ ->
              let after = S.runPure (enteringAs 1) gs (Cast.castSpell S.alice plasmaCard >> Stack.resolveTop)
               in case newestNamed (Text.pack "Primal Plasma") after of
                    Nothing -> HU.assertFailure "Primal Plasma did not reach the battlefield"
                    Just plasma -> do
                      HU.assertEqual "power" (Just 2) (Projection.powerOf plasma after)
                      HU.assertEqual "toughness" (Just 2) (Projection.toughnessOf plasma after)
                      HU.assertBool "flying" (Projection.hasKeyword Keyword.Flying plasma after)
            _ -> HU.assertFailure "fixture did not deal a card",
        HU.testCase "CR 616.2 a Clone of a 2/2-flying Plasma that picks 1/6 is 1/6 with flying AND defender" $ do
          -- THE CENTERPIECE, and the Gatherer ruling verbatim: "it copies the values
          -- determined by its enters-the-battlefield replacement effect, but its
          -- power and toughness are determined by the copy's own
          -- enters-the-battlefield replacement effect."
          island <- Registry.printing registry "Island"
          primalPlasma <- Registry.printing registry "Primal Plasma"
          clonePrinting <- Registry.printing registry "Clone"
          let (gs, held) = blueBoard island 8 [primalPlasma, clonePrinting]
          case held of
            plasmaCard : cloneCard : _ ->
              let withPlasma = S.runPure (enteringAs 1) gs (Cast.castSpell S.alice plasmaCard >> Stack.resolveTop)
                  after = S.runPure (enteringAs 2) withPlasma (Cast.castSpell S.alice cloneCard >> Stack.resolveTop)
               in case newestNamed (Text.pack "Clone") after of
                    Nothing -> HU.assertFailure "Clone did not reach the battlefield"
                    Just clone -> do
                      HU.assertEqual "power is the CLONE's own choice" (Just 1) (Projection.powerOf clone after)
                      HU.assertEqual "toughness is the CLONE's own choice" (Just 6) (Projection.toughnessOf clone after)
                      HU.assertBool "flying came from the COPY" (Projection.hasKeyword Keyword.Flying clone after)
                      HU.assertBool "defender came from the CHOICE" (Projection.hasKeyword Keyword.Defender clone after)
            _ -> HU.assertFailure "fixture did not deal two cards",
        HU.testCase "CR 616.2 the same Clone picking 3/3 is a 3/3 with flying" $ do
          island <- Registry.printing registry "Island"
          primalPlasma <- Registry.printing registry "Primal Plasma"
          clonePrinting <- Registry.printing registry "Clone"
          let (gs, held) = blueBoard island 8 [primalPlasma, clonePrinting]
          case held of
            plasmaCard : cloneCard : _ ->
              let withPlasma = S.runPure (enteringAs 1) gs (Cast.castSpell S.alice plasmaCard >> Stack.resolveTop)
                  after = S.runPure (enteringAs 0) withPlasma (Cast.castSpell S.alice cloneCard >> Stack.resolveTop)
               in case newestNamed (Text.pack "Clone") after of
                    Nothing -> HU.assertFailure "Clone did not reach the battlefield"
                    Just clone -> do
                      HU.assertEqual "3/3" (Just 3) (Projection.powerOf clone after)
                      HU.assertEqual "3/3" (Just 3) (Projection.toughnessOf clone after)
                      HU.assertBool "still flying (keywords UNION, never assign)" (Projection.hasKeyword Keyword.Flying clone after)
                      HU.assertBool "no defender" (not (Projection.hasKeyword Keyword.Defender clone after))
            _ -> HU.assertFailure "fixture did not deal two cards",
        HU.testCase "CR 707.2 a Clone of that Clone copies 1/6-flying-defender and then chooses again" $ do
          island <- Registry.printing registry "Island"
          primalPlasma <- Registry.printing registry "Primal Plasma"
          clonePrinting <- Registry.printing registry "Clone"
          let (gs, held) = blueBoard island 12 [primalPlasma, clonePrinting, clonePrinting]
          case held of
            plasmaCard : cloneA : cloneB : _ ->
              let s1 = S.runPure (enteringAs 1) gs (Cast.castSpell S.alice plasmaCard >> Stack.resolveTop)
                  s2 = S.runPure (enteringAs 2) s1 (Cast.castSpell S.alice cloneA >> Stack.resolveTop)
                  s3 = S.runPure (enteringAs 0) s2 (Cast.castSpell S.alice cloneB >> Stack.resolveTop)
               in case newestNamed (Text.pack "Clone") s3 of
                    Nothing -> HU.assertFailure "the second Clone did not reach the battlefield"
                    Just clone -> do
                      HU.assertEqual "its OWN choice wins on P/T" (Just 3) (Projection.powerOf clone s3)
                      HU.assertEqual "its OWN choice wins on P/T" (Just 3) (Projection.toughnessOf clone s3)
                      HU.assertBool "flying and defender rode the copy chain" (Projection.hasKeyword Keyword.Flying clone s3 && Projection.hasKeyword Keyword.Defender clone s3)
            _ -> HU.assertFailure "fixture did not deal three cards",
        -- CR 208.2b's own elision, at the ChoiceOf boundary: such an ability
        -- "lists two or more specific power and toughness values", so a
        -- single-option as-enters choice is not a 208.2b choice at all and
        -- must apply with no ChooseEntryOption prompt. NOT the same shape as
        -- the CR 616.1 "one Hardened Scales alone is not asked about" case
        -- above -- that is one CANDIDATE in a race between several sources;
        -- this is one OPTION inside a single candidate's own payload, which
        -- 616.1 (choosing which replacement effect applies) never reaches.
        -- Built as rules-level data (a floating EntryR ChoiceOf with one
        -- option, seeded via S.addReplacement) rather than a synthetic card
        -- file: no printed card in the pool has a single-option choice.
        HU.testCase "CR 400.3 an Opponents zone-change redirect exiles an opponent's card, not your own" $ do
          -- Leyline of the Void's shape without the Leyline: a floating redirect
          -- whose source alice controls. Bob's card is exiled on the way to his
          -- graveyard; alice's own reaches hers untouched.
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          let (src, g1) = S.addCreature pikerPrinting S.alice (Setup.emptyGame S.bothPlayers)
              (mine, g2) = S.addCreature pikerPrinting S.alice g1
              (theirs, g3) = S.addCreature pikerPrinting S.bob g2
              g4 = S.addReplacement (leylineShape src (fst (Game.freshTimestamp g3))) g3
              after = S.runPure S.identityAnswer g4 (Event.changeZone mine Zone.Graveyard >> Event.changeZone theirs Zone.Graveyard)
          HU.assertEqual "alice's own card reaches her graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after))
          HU.assertEqual "bob's does not" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after))
          HU.assertEqual "it was exiled instead" 1 (length (Game.zoneMembers Zone.Exile S.bob after)),
        HU.testCase "CR 400.3 the zone-change subject is the card's OWNER, not its controller" $ do
          -- A card alice OWNS but bob CONTROLS still dies to alice's graveyard
          -- (CR 400.3), so alice's own redirect must not exile it. A
          -- controller-based test would, which is the case this pins.
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          let slot = SlotName.MkSlotName (Text.pack "target")
              (src, g1) = S.addCreature pikerPrinting S.alice (Setup.emptyGame S.bothPlayers)
              (oid, g2) = S.addCreature pikerPrinting S.alice g1
              g3 = S.addReplacement (leylineShape src (fst (Game.freshTimestamp g2))) g2
              stolen =
                S.runPure S.identityAnswer g3 $
                  Resolve.applyEffect S.noSource S.bob Map.empty (Map.singleton slot True) (Map.singleton slot (Recipient.ToObject oid)) (Effect.GainControl Duration.Indefinite slot)
              after = S.runPure S.identityAnswer stolen (Event.changeZone oid Zone.Graveyard)
          HU.assertEqual "bob really did take control of it" (Just S.bob) (Projection.controllerOf oid stolen)
          HU.assertEqual "it reaches its OWNER's graveyard, unexiled" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after))
          HU.assertEqual "and nothing was exiled" 0 (length (Game.zoneMembers Zone.Exile S.alice after)),
        HU.testCase "CR 208.2b a single-option ChoiceOf is not a choice and must not prompt" $ do
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          let (piker, g1) = S.addCreature pikerPrinting S.alice (Setup.emptyGame S.bothPlayers)
              (ts, g2) = Game.freshTimestamp g1
              onlyOption = EntryOption.MkEntryOption {EntryOption.power = 3, EntryOption.toughness = 3, EntryOption.keywords = Set.empty}
              active =
                ActiveReplacement.MkActiveReplacement
                  { ActiveReplacement.effect = ReplacementEffect.EntryR (EntryRewrite.ChoiceOf [onlyOption]),
                    ActiveReplacement.source = piker,
                    ActiveReplacement.timestamp = ts,
                    ActiveReplacement.expiry = Expiry.AtCleanup,
                    ActiveReplacement.uses = Uses.Once
                  }
              g3 = S.addReplacement active g2
              asked = answersFor S.identityAnswer g3 (Replacement.runEntry Set.empty piker)
              after = S.runPure S.identityAnswer g3 (Replacement.runEntry Set.empty piker)
          HU.assertBool "no ChooseEntryOption was raised" (not (wasAskedForEntryOption asked))
          HU.assertEqual "the sole option applied anyway" (Just 3) (Projection.powerOf piker after),
        HU.testCase "CR 614.16 Doubling Season turns Dragon Fodder's two Goblins into four" $ do
          mountain <- Registry.printing registry "Mountain"
          doublingSeason <- Registry.printing registry "Doubling Season"
          dragonFodder <- Registry.printing registry "Dragon Fodder"
          let base = S.landsInPlay mountain 2
              (_, g1) = S.addCreature doublingSeason S.alice base
              (g2, spellId) = S.handOne dragonFodder g1
              after = castAndResolve S.identityAnswer g2 spellId
          HU.assertEqual "twice that many" 4 (S.countOnBattlefieldByName (Text.pack "Goblin") S.alice after),
        HU.testCase "CR 614.5 two Doubling Seasons are two instances: eight Goblins" $ do
          mountain <- Registry.printing registry "Mountain"
          doublingSeason <- Registry.printing registry "Doubling Season"
          dragonFodder <- Registry.printing registry "Dragon Fodder"
          let base = S.landsInPlay mountain 2
              (_, g1) = S.addCreature doublingSeason S.alice base
              (_, g2) = S.addCreature doublingSeason S.alice g1
              (g3, spellId) = S.handOne dragonFodder g2
              after = castAndResolve S.identityAnswer g3 spellId
          HU.assertEqual "2 -> 4 -> 8" 8 (S.countOnBattlefieldByName (Text.pack "Goblin") S.alice after),
        HU.testCase "CR 614.1 Doubling Season's OTHER clause doubles counters, not tokens" $ do
          forest <- Registry.printing registry "Forest"
          battlegrowth <- Registry.printing registry "Battlegrowth"
          doublingSeason <- Registry.printing registry "Doubling Season"
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          let (gs, spellId, mine, _) = counterBoard forest battlegrowth [doublingSeason, pikerPrinting] []
          case mine of
            season : piker : _ ->
              let after = castAndResolve (raceAnswer season piker) gs spellId
               in HU.assertEqual "1 * 2" 2 (countersOn CounterKind.PlusOnePlusOne piker after)
            _ -> HU.assertFailure "fixture did not build two permanents",
        HU.testCase "CR 616.1 Doubling Season racing Hardened Scales: 4 or 3, by the prompt" $ do
          forest <- Registry.printing registry "Forest"
          battlegrowth <- Registry.printing registry "Battlegrowth"
          doublingSeason <- Registry.printing registry "Doubling Season"
          hardenedScales <- Registry.printing registry "Hardened Scales"
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          let (gs, spellId, mine, _) = counterBoard forest battlegrowth [doublingSeason, hardenedScales, pikerPrinting] []
          case mine of
            season : scales : piker : _ ->
              let seasonFirst = castAndResolve (raceAnswer season piker) gs spellId
                  scalesFirst = castAndResolve (raceAnswer scales piker) gs spellId
               in do
                    HU.assertEqual "(1 * 2) + 1" 3 (countersOn CounterKind.PlusOnePlusOne piker seasonFirst)
                    HU.assertEqual "(1 + 1) * 2" 4 (countersOn CounterKind.PlusOnePlusOne piker scalesFirst)
            _ -> HU.assertFailure "fixture did not build three permanents",
        -- #79: resolveDestruction answers with the SETTLED object, not a Bool. The
        -- identity of what the CR 616.1 loop hands back is what Event.destroy must
        -- put into the graveyard; collapsing it to a predicate is what made a
        -- redirecting DestructionRewrite silently unimplementable.
        HU.testCase "CR 701.8 an unreplaced destruction settles on the object itself" $ do
          swamp <- Registry.printing registry "Swamp"
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          let base = S.landsInPlay swamp 1
              (piker, g1) = S.addCreature pikerPrinting S.alice base
              (settled, _) = S.runPureWith S.identityAnswer g1 (Replacement.resolveDestruction Regenerability.Regenerable piker)
          HU.assertEqual "the object it was asked about" (Just piker) settled,
        HU.testCase "CR 701.19a a regenerated destruction settles on nothing" $ do
          swamp <- Registry.printing registry "Swamp"
          pikerPrinting <- Registry.printing registry "Goblin Piker"
          let base = S.landsInPlay swamp 1
              (piker, g1) = S.addCreature pikerPrinting S.alice base
              (settled, _) = S.runPureWith S.identityAnswer (S.addRegenShield piker g1) (Replacement.resolveDestruction Regenerability.Regenerable piker)
          HU.assertEqual "consumed by the shield" Nothing settled
      ]
