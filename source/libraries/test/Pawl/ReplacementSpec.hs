{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.Replacement (the CR 616.1 loop, its buckets and its prompt) and
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
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Engine.Filter may later be imported and must not collide.

import Pawl.DamageReplacementSpec (graveyardNames)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Engine.Resolve.Effect as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Extra.Int as Int
import Pawl.PreventionSpec (aimObject, answersFor, blightAnswer, blueBoard, castAndResolve, castOrPassAnswer, clachanBoard, copyOf, counterBoard, countersOn, enteringAs, isFlip, leylineShape, lostLife, namedOut, newestNamed, payLifeOnEntryAnswer, raceAnswer, razorgrassBoard, revealAsks, revealOnEntryAnswer, runSentry, seaGateBoard, sentryAnswer, sentryBoard, theAbility, wardenOut, warriorOut, wasAskedForEntryOption, wasAskedToReplace, wasCall)
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CoinFace as CoinFace
import qualified Pawl.Types.CoinFlipped as CoinFlipped
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.Daytime as Daytime
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.DestructionCause as DestructionCause
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KickerDecision as KickerDecision
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementEntry as ReplacementEntry
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone

-- CR 614.5's applied set is what makes the CR 616.1 loop TERMINATE, not merely
-- correct: a regression there (an effect invoking itself repeatedly, e.g. two
-- Hardened Scales re-triggering each other forever) manifests as this group
-- hanging, not failing. "CR 614.5 two Hardened Scales are two instances" below
-- is the case that asserts the CORRECTNESS half (each gets exactly one
-- opportunity); the suite's timeout is the safety net for the TERMINATION half
-- -- it asserts nothing on a green run, and guards a hang rather than a
-- slowdown. This group used to carry a five-second budget of its own, dropped
-- with every other per-group budget in #3284: a hang fails at any figure, and
-- the slowest case here runs 0.02s.
spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Replacement" $ do
  -- P9: a pattern's permanent match runs through the lower Pawl.Engine.Filter over
  -- the PROJECTED view, the same evaluator Pawl.Engine.Cost narrows sacrifices with
  -- (#111 retired). CR 205.2b/300.2/613.1d: creature-ness is projected; the
  -- trivial filter And [] matches every permanent (what AnyPermanent was).
  Spec.it s "CR 614.1 matchesPermanent narrows a permanent through Filter.matches" $ do
    swamp <- S.printingOf s registry "Swamp"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay swamp 1
        (piker, g1) = S.addPermanent pikerPrinting S.alice base
        land = case Set.toList (GameState.battlefield base) of
          oid : _ -> Just oid
          [] -> Nothing
    case land of
      Nothing -> Spec.assertFailure s "fixture did not build a land"
      Just landId -> do
        Spec.assertBool s (Replacement.matchesPermanent g1 Map.empty Nothing (Filter.Type.HasCardType CardType.Creature) piker) "the creature matches HasCardType Creature"
        Spec.assertBool s (not (Replacement.matchesPermanent g1 Map.empty Nothing (Filter.Type.HasCardType CardType.Creature) landId)) "the land does not match HasCardType Creature"
        Spec.assertBool s (Replacement.matchesPermanent g1 Map.empty Nothing (Filter.Type.And []) landId) "the trivial filter matches the land too"
  -- NOT a CR 614.5 test: this does not exercise the applied set at all. After
  -- the first Rest in Peace redirects the event to Exile, the SECOND Rest in
  -- Peace's pattern (whenDestination = Graveyard) no longer matches the
  -- rewritten event, so `applies` alone -- not CR 614.5's applied-set --
  -- is what stops the second application. Deleting the applied-set logic
  -- from `loop` entirely leaves this test passing. What it actually proves:
  -- a redirect whose output no longer matches its own `whenDestination`
  -- cannot re-fire. See "CR 614.5 the applied set ..." below for the real
  -- 614.5 coverage.
  Spec.it s "CR 614.1a a redirect that no longer matches its own pattern cannot re-fire" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addPermanent restInPeace S.alice (Setup.emptyGame S.bothPlayers)
        (_, g1) = S.addPermanent restInPeace S.alice g0
        (piker, g2) = S.addPermanent pikerPrinting S.bob g1
        after = S.runPure S.identityAnswer g2 (Event.changeZone piker Zone.Graveyard)
    Spec.assertEqWith s "not in a graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
    Spec.assertEqWith s "exactly one object in exile" (Set.size (GameState.exile after)) 1
  Spec.it s "CR 616.1 value-equal candidates elide the prompt (nothing to choose)" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addPermanent restInPeace S.alice (Setup.emptyGame S.bothPlayers)
        (_, g1) = S.addPermanent restInPeace S.alice g0
        (piker, g2) = S.addPermanent pikerPrinting S.bob g1
        asked = answersFor S.identityAnswer g2 (Event.changeZone piker Zone.Graveyard)
    Spec.assertBool s (not (wasAskedToReplace asked)) "no ChooseReplacement was raised"
  -- The other side of Replacement.readsApplier, and the reason it exists rather
  -- than a blanket "compare the controller too". Rest in Peace's pattern is
  -- the trivial Filter under ControllerRelation.Anyones, so alice's copy
  -- and bob's are both applicable to bob's dying Piker at once, equal in `effect`
  -- and differing only in who controls the row. Applying either exiles the same
  -- card, so there is nothing to decide and nothing to ask -- where comparing
  -- `(effect, controller)` unconditionally would have started prompting.
  Spec.it s "CR 616.1 value-equal candidates under DIFFERENT controllers still elide" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addPermanent restInPeace S.alice (Setup.emptyGame S.bothPlayers)
        (_, g1) = S.addPermanent restInPeace S.bob g0
        (piker, g2) = S.addPermanent pikerPrinting S.bob g1
        after = S.runPure S.identityAnswer g2 (Event.changeZone piker Zone.Graveyard)
        asked = answersFor S.identityAnswer g2 (Event.changeZone piker Zone.Graveyard)
    Spec.assertEqWith s "the Piker was exiled, not buried" (Set.size (GameState.exile after)) 1
    Spec.assertBool s (not (wasAskedToReplace asked)) "no ChooseReplacement was raised"
  -- CR 704.3: "the game checks for any of the listed conditions for
  -- state-based actions, then performs all applicable state-based actions
  -- simultaneously as a single event." So the put-into-graveyard batch one
  -- pass performs is ONE event, and the replacement effects in force for it
  -- are the ones on the battlefield when the pass began -- including one
  -- belonging to a permanent the pass is itself burying.
  --
  -- Opalescence makes Rest in Peace a 2/2 (its mana value); two -1/-1
  -- counters take it to 0/0 and one takes the 2/1 Piker to 1/0, so CR
  -- 704.5f names both in the same pass. Rest in Peace is added FIRST on
  -- purpose: Sba walks the battlefield in ascending ObjectId order, so it
  -- is buried first and an implementation that re-collected the Piker's
  -- candidates from the live board would find it gone.
  Spec.it s "CR 704.3 a Rest in Peace buried by an SBA pass still exiles that pass's other victim" $ do
    opalescence <- S.printingOf s registry "Opalescence"
    restInPeace <- S.printingOf s registry "Rest in Peace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addPermanent opalescence S.alice (Setup.emptyGame S.bothPlayers)
        (rip, g1) = S.addPermanent restInPeace S.alice g0
        (piker, g2) = S.addPermanent pikerPrinting S.bob g1
        board = S.addCounter CounterKind.MinusOneMinusOne 1 piker (S.addCounter CounterKind.MinusOneMinusOne 2 rip g2)
        after = S.settleSba board
    Spec.assertBool s (rip < piker) "setup: Rest in Peace is buried before the Piker"
    Spec.assertEqWith s "setup: Opalescence's 2/2 is a 0/0" (S.powerToughnessOf rip board) (Just (0, 0))
    Spec.assertEqWith s "setup: the Piker is a 1/0" (S.powerToughnessOf piker board) (Just (1, 0))
    Spec.assertEqWith s "the Piker was exiled, not buried" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
    Spec.assertEqWith s "the Piker's card is in exile" (length (Game.zoneMembers Zone.Exile S.bob after)) 1
    Spec.assertEqWith s "and Rest in Peace exiled its own card too" (length (Game.zoneMembers Zone.Exile S.alice after)) 1
  -- The same CR 704.3 event, across the pass's OTHER seam. The case above
  -- keeps both victims inside the pass's put-into-graveyard batch; this one
  -- puts the second victim in the DESTRUCTION batch (CR 704.5g's lethal
  -- marked damage), which Pawl.Engine.Sba performs after the buries. CR 704.3 makes
  -- the two one event, so the destruction's graveyard move must see the same
  -- board the buries did -- with Rest in Peace still on it.
  --
  -- Rest in Peace is again a 2/2 by Opalescence taken to 0/0 by two -1/-1
  -- counters (CR 704.5f). The Piker keeps its printed 2/1 and takes 1
  -- marked damage instead, so it is lethally damaged rather than
  -- zero-toughness and CR 704.5g claims it.
  Spec.it s "CR 704.3 a Rest in Peace the pass buries still exiles what the pass DESTROYS" $ do
    opalescence <- S.printingOf s registry "Opalescence"
    restInPeace <- S.printingOf s registry "Rest in Peace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addPermanent opalescence S.alice (Setup.emptyGame S.bothPlayers)
        (rip, g1) = S.addPermanent restInPeace S.alice g0
        (piker, g2) = S.addPermanent pikerPrinting S.bob g1
        board = S.markDamage piker 1 (S.addCounter CounterKind.MinusOneMinusOne 2 rip g2)
        after = S.settleSba board
    Spec.assertEqWith s "setup: Opalescence's 2/2 is a 0/0, so CR 704.5f buries it" (S.powerToughnessOf rip board) (Just (0, 0))
    Spec.assertEqWith s "setup: the Piker is still a 2/1" (S.powerToughnessOf piker board) (Just (2, 1))
    Spec.assertEqWith s "setup: with lethal damage marked, so CR 704.5g destroys it" (S.damageOf piker board) (Just 1)
    Spec.assertEqWith s "the Piker was exiled, not buried" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
    Spec.assertEqWith s "the Piker's card is in exile" (length (Game.zoneMembers Zone.Exile S.bob after)) 1
    Spec.assertEqWith s "and Rest in Peace exiled its own card too" (length (Game.zoneMembers Zone.Exile S.alice after)) 1
  -- The other side of the coin above. Sharing the pass's board is what CR
  -- 704.3 asks of the two halves' REPLACEMENT collection; it is not what it
  -- asks of the destroy funnel's existence filter, which stays live. CR
  -- 614.7 is why: "If a replacement effect would replace an event, but that
  -- event never happens, the replacement effect simply doesn't do anything."
  -- A permanent the pass's put-into-graveyard half has already moved is not
  -- on the battlefield to be destroyed, so the destruction never happens and
  -- a regeneration shield on it must be neither applied nor spent.
  --
  -- The one shape in the pool that reaches it: a permanent named by both
  -- halves of one pass. CR 704.5f's victims can never also be CR 704.5g's
  -- (Pawl.Engine.Sba's classify gives 704.5f priority) and Pawl.Engine.Sba already
  -- excludes CR 704.5j's and CR 704.5k's by name, so an Aura -- named by CR
  -- 704.5m in the first half and CR 704.5g in the second -- is all that is
  -- left. Getting one takes Liquimetal Coating plus Skilled Animator, since
  -- every printed enchantment animator excludes Auras: the Aura is made an
  -- artifact first, then animated as one. See Pawl.AuraSpec's CR 303.4d case
  -- for the same fixture proving the detach-then-bury order this builds on.
  --
  -- The shield is seeded rather than activated because CR 701.19a's shield
  -- "protects the permanent" its effect names, and the only two producers in
  -- the pool -- Drudge Skeletons and Uthden Troll -- name themselves. No Aura
  -- prints one, so there is no gameplay route to a shield on this Aura.
  Spec.it s "CR 614.7 an Aura the same pass buries is never offered to a regeneration shield" $ do
    island <- S.printingOf s registry "Island"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    coating <- S.printingOf s registry "Liquimetal Coating"
    animator <- S.printingOf s registry "Skilled Animator"
    let base = S.landsInPlay island 3 -- {2}{U} for the Animator
        (creature, g1) = S.addPermanent pikerPrinting S.alice base
        (aura, g2) = S.addPermanent unholyStrength S.alice g1
        (coatingId, g3) = S.addPermanent coating S.alice (S.attach aura creature g2)
        ready = g3 {GameState.priority = Just S.alice}
        activated = S.runPure (aimObject aura) ready (Activate.activateAbility S.alice coatingId (theAbility coating))
        coated = S.runPure (aimObject aura) activated Stack.resolveTop
        (withSpell, spellId) = S.handOne animator coated
        entered = S.runPure (aimObject aura) withSpell (S.cast S.alice spellId >> Stack.resolveTop)
        triggered = S.runPure (aimObject aura) entered Engine.settleForPriority
        animated = S.runPure (aimObject aura) triggered Stack.resolveTop
        -- One pass, so the two state-based actions stay separately
        -- observable: CR 704.5p unattaches the animated Aura here and CR
        -- 704.5m buries it on the pass below.
        unattachedNow = S.settleSba animated
        -- Lethal damage on the 5/5 makes CR 704.5g name it too, so the next
        -- pass names it in BOTH halves.
        armed = S.addRegenShield aura (S.markDamage aura 5 unattachedNow)
        after = S.settleSba armed
    Spec.assertEqWith s "setup: the Aura is an unattached 5/5" (S.powerToughnessOf aura armed) (Just (5, 5))
    Spec.assertEqWith s "setup: attached to nothing, so CR 704.5m names it" (fmap Object.attachedTo (Game.lookupObject aura armed)) (Just Nothing)
    Spec.assertEqWith s "setup: with lethal damage, so CR 704.5g names it as well" (S.damageOf aura armed) (Just 5)
    Spec.assertEqWith s "setup: exactly one floating replacement, the shield" (length (GameState.replacements armed)) 1
    Spec.assertBool s (not (Set.member aura (GameState.battlefield after))) "CR 704.5m buried it"
    Spec.assertEqWith s "in its owner's graveyard, not regenerated" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "and the shield was never spent on a destruction that did not happen" (length (GameState.replacements after)) 1
  Spec.it s "CR 614.1a a move whose destination the pattern misses is untouched" $ do
    restInPeace <- S.printingOf s registry "Rest in Peace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (_, g0) = S.addPermanent restInPeace S.alice (Setup.emptyGame S.bothPlayers)
        (piker, g1) = S.addPermanent pikerPrinting S.bob g0
        -- Rest in Peace watches graveyard-bound moves only; a bounce to hand
        -- is not one, so the loop finds no candidate and the move stands.
        after = S.runPure S.identityAnswer g1 (Event.changeZone piker Zone.Hand)
    Spec.assertEqWith s "in bob's hand" (length (Game.zoneMembers Zone.Hand S.bob after)) 1
    Spec.assertEqWith s "nothing was exiled" (Set.size (GameState.exile after)) 0
  Spec.it s "CR 615.10 Fog prevents both attackers' damage in one batch" $ do
    forest <- S.printingOf s registry "Forest"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    fog <- S.printingOf s registry "Fog"
    let base = S.landsInPlay forest 1
        (victimA, g1) = S.addPermanent pikerPrinting S.bob base
        (victimB, g2) = S.addPermanent pikerPrinting S.bob g1
        (g3, fogId) = S.handOne fog g2
        resolved = S.runPure S.identityAnswer g3 (S.cast S.alice fogId >> Stack.resolveTop)
        -- Hand-built rather than driven through real combat: reaching a real
        -- combat-damage batch would mean driving an entire combat phase, which
        -- this assertion (Fog prevents a whole batch, not just one event) does
        -- not need.
        batch =
          [ DamageEvent.MkDamageEvent victimA (Recipient.ToCreature victimA) 2 False False False 0 Nothing DamageKind.Combat,
            DamageEvent.MkDamageEvent victimB (Recipient.ToCreature victimB) 2 False False False 0 Nothing DamageKind.Combat
          ]
        after = S.runPure S.identityAnswer resolved (Damage.applyDamage batch)
    Spec.assertEqWith s "the first attacker's damage was prevented" (S.damageOf victimA after) (Just 0)
    Spec.assertEqWith s "and so was the second's, independently" (S.damageOf victimB after) (Just 0)
    Spec.assertEqWith s "no damage event was recorded at all" (S.damageEventsOf after) []
  Spec.it s "CR 701.19a Uses=Once: the first destruction is replaced, the second is not" $ do
    swamp <- S.printingOf s registry "Swamp"
    drudgeSkeletons <- S.printingOf s registry "Drudge Skeletons"
    let base = S.landsInPlay swamp 1
        (skel, g1) = S.addPermanent drudgeSkeletons S.alice base
        -- Activate {B}: regenerate this creature, and resolve it.
        armed = S.runPure S.identityAnswer g1 (Activate.activateAbility S.alice skel (theAbility drudgeSkeletons) >> Stack.resolveTop)
        -- CR 701.19a's "remove it from combat" half needs the creature
        -- actually attacking. Driving a full combat phase to reach a legal
        -- attack is disproportionate to what this asserts, so seed
        -- GameState.combat's attacker map directly -- the same shortcut
        -- Support.addRegenShield takes for the shield itself.
        attacking = armed {GameState.combat = (GameState.combat armed) {Combat.Type.attackers = Map.singleton skel (AttackTarget.OfPlayer S.bob)}}
        once = S.runPure S.identityAnswer attacking (Event.destroy Regenerability.Regenerable [skel])
        twice = S.runPure S.identityAnswer once (Event.destroy Regenerability.Regenerable [skel])
    Spec.assertBool s (Map.null (Combat.Type.attackers (GameState.combat armed))) "combat started with no attackers"
    Spec.assertBool s (Set.member skel (GameState.battlefield once)) "survived the first destruction"
    Spec.assertEqWith s "the shield was spent" (GameState.replacements once) []
    Spec.assertBool s (not (Map.member skel (Combat.Type.attackers (GameState.combat once)))) "removed from combat by the regeneration (CR 701.19a)"
    Spec.assertBool s (not (Set.member skel (GameState.battlefield twice))) "the second destruction kills it"
  -- CR 701.19c: "Effects that say that a permanent can't be regenerated
  -- don't preclude such abilities from being activated or such spells from
  -- being cast; rather, they cause regeneration shields to not be applied."
  -- So the shield still exists -- it simply does not fire.
  Spec.it s "CR 701.19c a shield does not save a creature from a destruction that forbids regeneration" $ do
    swamp <- S.printingOf s registry "Swamp"
    drudgeSkeletons <- S.printingOf s registry "Drudge Skeletons"
    let (skel, g1) = S.addPermanent drudgeSkeletons S.alice (S.landsInPlay swamp 1)
        shielded = S.addRegenShield skel g1
        after = S.runPure S.identityAnswer shielded (Event.destroy Regenerability.CantBeRegenerated [skel])
    Spec.assertBool s (not (Set.member skel (GameState.battlefield after))) "it died anyway"
    -- CR 701.19c again, and the sharp half: an unapplied shield is not a
    -- spent one. Nothing consumed it, because it was never chosen.
    Spec.assertEqWith s "and the shield was not consumed" (length (GameState.replacements after)) (length (GameState.replacements shielded))
  -- The discriminating twin: identical creature, identical shield, and the
  -- only difference is whether the destruction forbids regeneration. This
  -- fails if the gate is ignored, and equally if it is applied to every
  -- destruction.
  Spec.it s "CR 701.19a the same shield DOES save it from an ordinary destruction" $ do
    swamp <- S.printingOf s registry "Swamp"
    drudgeSkeletons <- S.printingOf s registry "Drudge Skeletons"
    let (skel, g1) = S.addPermanent drudgeSkeletons S.alice (S.landsInPlay swamp 1)
        shielded = S.addRegenShield skel g1
        after = S.runPure S.identityAnswer shielded (Event.destroy Regenerability.Regenerable [skel])
    Spec.assertBool s (Set.member skel (GameState.battlefield after)) "it survived"
    Spec.assertEqWith s "and this time the shield was spent" (GameState.replacements after) []
  -- The gameplay-level proof (design.md section 4): real cards, cast and
  -- resolved. Uthden Troll rather than Drudge Skeletons because Terror
  -- cannot target a black creature -- the Troll is red.
  Spec.it s "CR 701.19c whole cards: Terror kills an Uthden Troll that just regenerated" $ do
    mountain <- S.printingOf s registry "Mountain"
    swamp <- S.printingOf s registry "Swamp"
    uthdenTroll <- S.printingOf s registry "Uthden Troll"
    terror <- S.printingOf s registry "Terror"
    let base = foldl (\gs p -> snd (S.addPermanent p S.alice gs)) (Setup.emptyGame S.bothPlayers) [mountain, swamp, swamp]
        (troll, g1) = S.addPermanent uthdenTroll S.alice base
        -- {R}: Regenerate this creature -- the shield is really activated.
        armed = S.runPure S.identityAnswer g1 (Activate.activateAbility S.alice troll (theAbility uthdenTroll) >> Stack.resolveTop)
        (withTerror, spell) = S.handOne terror armed
        afterCast = S.runPure S.identityAnswer withTerror (S.cast S.alice spell)
        resolved = S.runPure S.identityAnswer afterCast Stack.resolveTop
    Spec.assertBool s (not (null (GameState.replacements armed))) "the shield really was created"
    Spec.assertBool s (not (Set.member troll (GameState.battlefield resolved))) "and Terror killed the Troll through it"
  -- The twin of the whole-card test: the SAME creature and the SAME shield,
  -- destroyed by the CR 704.5g state-based action instead, which carries no
  -- such clause. Regeneration is exactly what it is for.
  Spec.it s "CR 701.19a an Uthden Troll's shield still saves it from lethal damage" $ do
    mountain <- S.printingOf s registry "Mountain"
    uthdenTroll <- S.printingOf s registry "Uthden Troll"
    let base = S.landsInPlay mountain 1
        (troll, g1) = S.addPermanent uthdenTroll S.alice base
        armed = S.runPure S.identityAnswer g1 (Activate.activateAbility S.alice troll (theAbility uthdenTroll) >> Stack.resolveTop)
        -- 2 damage is lethal to a 2/2.
        hurt = S.runPure S.identityAnswer armed (Damage.applyDamage [DamageEvent.MkDamageEvent troll (Recipient.ToCreature troll) 2 False False False 0 Nothing DamageKind.Combat])
        settled = S.settleSba hurt
    Spec.assertBool s (Set.member troll (GameState.battlefield settled)) "the shield saved it"
  Spec.it s "CR 614.8 regeneration replaces the destruction, so Rest in Peace never sees it" $ do
    swamp <- S.printingOf s registry "Swamp"
    restInPeace <- S.printingOf s registry "Rest in Peace"
    drudgeSkeletons <- S.printingOf s registry "Drudge Skeletons"
    let base = S.landsInPlay swamp 1
        (_, g1) = S.addPermanent restInPeace S.bob base
        (skel, g2) = S.addPermanent drudgeSkeletons S.alice g1
        shielded = S.addRegenShield skel g2
        after = S.runPure S.identityAnswer shielded (Event.destroy Regenerability.Regenerable [skel])
    Spec.assertBool s (Set.member skel (GameState.battlefield after)) "still on the battlefield"
    Spec.assertEqWith s "nothing was exiled -- the put-into-graveyard never happened" (Set.size (GameState.exile after)) 0
    Spec.assertEqWith s "and nothing reached a graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
  Spec.it s "CR 614.7 an event that never happens does not consume a shield" $ do
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let base = Setup.emptyGame S.bothPlayers
        (myr, g1) = S.addPermanent darksteelMyr S.alice base
        shielded = S.addRegenShield myr g1
        after = S.runPure S.identityAnswer shielded (Event.destroy Regenerability.Regenerable [myr])
    Spec.assertBool s (Set.member myr (GameState.battlefield after)) "the indestructible creature survives"
    Spec.assertEqWith s "the shield is intact" (length (GameState.replacements after)) 1
  Spec.it s "CR 616.1 Scales first, then Corpsejack: 1 -> 2 -> 4" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    hardenedScales <- S.printingOf s registry "Hardened Scales"
    corpsejackMenace <- S.printingOf s registry "Corpsejack Menace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, _) = counterBoard forest battlegrowth [hardenedScales, corpsejackMenace, pikerPrinting] []
    case mine of
      scales : _ : piker : _ ->
        let after = castAndResolve (raceAnswer scales piker) gs spellId
         in Spec.assertEqWith s "(1 + 1) * 2" (countersOn CounterKind.PlusOnePlusOne piker after) 4
      _ -> Spec.assertFailure s "fixture did not build three permanents"
  Spec.it s "CR 616.1 Corpsejack first, then Scales: 1 -> 2 -> 3 (same input, different board)" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    hardenedScales <- S.printingOf s registry "Hardened Scales"
    corpsejackMenace <- S.printingOf s registry "Corpsejack Menace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, _) = counterBoard forest battlegrowth [hardenedScales, corpsejackMenace, pikerPrinting] []
    case mine of
      _ : corpsejack : piker : _ ->
        let after = castAndResolve (raceAnswer corpsejack piker) gs spellId
         in Spec.assertEqWith s "(1 * 2) + 1" (countersOn CounterKind.PlusOnePlusOne piker after) 3
      _ -> Spec.assertFailure s "fixture did not build three permanents"
  -- CR 305.7: a land whose subtype is set to a basic type "loses all
  -- abilities generated from its rules text", and a replacement effect is
  -- one of them. Ashaya makes the Menace a Forest land, Blood Moon sets
  -- that to Mountain, and the doubling goes with the rest of its text --
  -- so Battlegrowth's one counter stays one. The Piker is animated too and
  -- is still a creature (CR 305.7 removes no card types), so it is still a
  -- legal target for "target creature".
  Spec.it s "CR 305.7 an Ashaya-animated, Blood Moon'd Corpsejack Menace doubles nothing" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    corpsejackMenace <- S.printingOf s registry "Corpsejack Menace"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, _) = counterBoard forest battlegrowth [corpsejackMenace, ashaya, bloodMoon, pikerPrinting] []
    case mine of
      corpsejack : _ : _ : piker : _ ->
        let after = castAndResolve (raceAnswer corpsejack piker) gs spellId
         in Spec.assertEqWith s "one counter, not two" (countersOn CounterKind.PlusOnePlusOne piker after) 1
      _ -> Spec.assertFailure s "fixture did not build four permanents"
  Spec.it s "CR 616.1 the engine ASKS -- it does not proceed on list order" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    hardenedScales <- S.printingOf s registry "Hardened Scales"
    corpsejackMenace <- S.printingOf s registry "Corpsejack Menace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, _) = counterBoard forest battlegrowth [hardenedScales, corpsejackMenace, pikerPrinting] []
    case mine of
      scales : _ : piker : _ ->
        let asked = answersFor (raceAnswer scales piker) gs (S.cast S.alice spellId >> Stack.resolveTop)
         in Spec.assertBool s (wasAskedToReplace asked) "a ChooseReplacement was raised"
      _ -> Spec.assertFailure s "fixture did not build three permanents"
  Spec.it s "CR 616.1 one Hardened Scales alone is not asked about (nothing to choose)" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    hardenedScales <- S.printingOf s registry "Hardened Scales"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, _) = counterBoard forest battlegrowth [hardenedScales, pikerPrinting] []
    case mine of
      scales : piker : _ ->
        let after = castAndResolve (raceAnswer scales piker) gs spellId
            asked = answersFor (raceAnswer scales piker) gs (S.cast S.alice spellId >> Stack.resolveTop)
         in do
              Spec.assertEqWith s "1 + 1" (countersOn CounterKind.PlusOnePlusOne piker after) 2
              Spec.assertBool s (not (wasAskedToReplace asked)) "no ChooseReplacement was raised"
      _ -> Spec.assertFailure s "fixture did not build two permanents"
  Spec.it s "CR 614.5 two Hardened Scales are two instances: 1 -> 2 -> 3, unprompted" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    hardenedScales <- S.printingOf s registry "Hardened Scales"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, _) = counterBoard forest battlegrowth [hardenedScales, hardenedScales, pikerPrinting] []
    case mine of
      scales : _ : piker : _ ->
        let after = castAndResolve (raceAnswer scales piker) gs spellId
            asked = answersFor (raceAnswer scales piker) gs (S.cast S.alice spellId >> Stack.resolveTop)
         in do
              Spec.assertEqWith s "each gets its own opportunity" (countersOn CounterKind.PlusOnePlusOne piker after) 3
              Spec.assertBool s (not (wasAskedToReplace asked)) "value-equal candidates elide the prompt"
      _ -> Spec.assertFailure s "fixture did not build three permanents"
  Spec.it s "CR 614.1 Hardened Scales ignores a -1/-1 counter (whichKind)" $ do
    swamp <- S.printingOf s registry "Swamp"
    hardenedScales <- S.printingOf s registry "Hardened Scales"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    instillInfection <- S.printingOf s registry "Instill Infection"
    let base = S.landsInPlay swamp 4
        (scales, g1) = S.addPermanent hardenedScales S.alice base
        (piker, g2) = S.addPermanent pikerPrinting S.alice g1
        (g3, spellId) = S.handOne instillInfection g2
        after = castAndResolve (raceAnswer scales piker) g3 spellId
    Spec.assertEqWith s "one -1/-1 counter, unscaled" (countersOn CounterKind.MinusOneMinusOne piker after) 1
  Spec.it s "CR 109.5 Corpsejack Menace does not double an opponent's counters" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    corpsejackMenace <- S.printingOf s registry "Corpsejack Menace"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, theirs) = counterBoard forest battlegrowth [corpsejackMenace] [pikerPrinting]
    case (mine, theirs) of
      (corpsejack : _, piker : _) ->
        let after = castAndResolve (raceAnswer corpsejack piker) gs spellId
         in Spec.assertEqWith s "not doubled -- ControllerRelation is Yours" (countersOn CounterKind.PlusOnePlusOne piker after) 1
      _ -> Spec.assertFailure s "fixture did not build both sides"
  -- THE PROVING TEST for #78's candidate-collection channel. CR 614.12 settles
  -- which replacement effects modify how a permanent enters by taking into
  -- account "continuous effects that already exist and would apply to the
  -- permanent" -- and a permanent arriving in the SAME batch has none yet, since
  -- its static abilities begin to apply only once it is on the battlefield, which
  -- is the moment this one arrives too. Corpsejack Menace's own ruling states the
  -- effect this rule denies it here: "if a creature you control would enter the
  -- battlefield with a number of +1/+1 counters on it, it enters with twice that
  -- many instead."
  --
  -- Rise of the Dark Realms returns every creature card from every graveyard as
  -- ONE CR 608.2f event, so the Menace and the Worker enter simultaneously.
  -- Arcbound Worker is a printed 0/0 with modular 1 (CR 702.43a), which
  -- Pawl.Engine.Keyword mints as the CR 614.1c entry replacement "enters with one
  -- +1/+1 counter" -- so the counter is placed inside the Worker's entry loop,
  -- exactly where the rule is asked.
  --
  -- The Menace is buried FIRST so it takes the lower ObjectId and moves first
  -- (Resolve.graveyardCardsOf sorts ascending, S.addGraveyardCard mints in call
  -- order), which is the only order in which a live-board reading has anything to
  -- double; the mirrored leg pins that the answer does not depend on it, which is
  -- CR 608.2f's point -- the batch is one event and nobody gets to order it.
  --
  -- Power and toughness ride along with the counter count because they are what a
  -- player sees: 1 counter is a 1/1 Worker, 2 is a 2/2, and 0 would be a 0/0 that
  -- CR 704.5f buries -- three boards no pair of readings can confuse.
  Spec.it s "CR 614.12 a Corpsejack Menace reanimated beside a modular creature doubles nothing (#78)" $ do
    swamp <- S.printingOf s registry "Swamp"
    rise <- S.printingOf s registry "Rise of the Dark Realms"
    corpsejackMenace <- S.printingOf s registry "Corpsejack Menace"
    arcboundWorker <- S.printingOf s registry "Arcbound Worker"
    let outcome buried =
          let graves = List.foldl' (\g printing -> snd (S.addGraveyardCard printing S.alice g)) (S.landsInPlay swamp 9) buried
              (gs, spellId) = S.handOne rise graves
              after = castAndResolve S.identityAnswer gs spellId
              workers =
                [ oid
                | oid <- Set.toList (GameState.battlefield after),
                  Projection.namesOf oid after == Set.singleton (CardName.MkCardName (Text.pack "Arcbound Worker"))
                ]
           in [(countersOn CounterKind.PlusOnePlusOne oid after, S.powerToughnessOf oid after) | oid <- workers]
        menaceFirst = outcome [corpsejackMenace, arcboundWorker]
        workerFirst = outcome [arcboundWorker, corpsejackMenace]
    Spec.assertEqWith s "modular 1's one counter, undoubled -- a 1/1 Worker" menaceFirst [(1, Just (1, 1))]
    Spec.assertEqWith s "and the batch's processing order changes nothing (CR 608.2f)" workerFirst menaceFirst
  -- THE PROVING TEST for #78's PROJECTION channel, the other half of the same
  -- rule. A permanent's static abilities function only while it is on the
  -- battlefield (CR 113.6), and CR 614.12a puts an as-enters choice BEFORE the
  -- permanent enters -- so at the moment a batch member makes its choice, a
  -- sibling arriving in the same batch has no continuous effect yet, which is the
  -- same thing CR 614.12 says by admitting only "continuous effects that already
  -- exist". This engine materializes every batch member up front, so the sibling
  -- IS sitting on the battlefield when the projection reads run, and its static
  -- has to be suppressed rather than merely not looked at.
  --
  -- Ashaya, Soul of the Wild ("nontoken creatures you control are Forest lands in
  -- addition to their other types") and Wood Elemental ("as this creature enters,
  -- sacrifice any number of untapped Forests") come back from one graveyard as ONE
  -- CR 608.2f event. The victim is a THIRD permanent -- a Goblin Piker already on
  -- the battlefield -- because CR 614.13a already bars the batch's own members from
  -- the offer, so a sibling of the batch could not tell the two rules apart.
  --
  -- The answer is greedy (sacrificesAll), so the offer is not merely observed but
  -- SPENT: with Ashaya's static visible the Piker projects as an untapped Forest,
  -- is offered, and dies.
  --
  -- Ashaya is buried FIRST so it takes the lower ObjectId and arrives first
  -- (Resolve.graveyardCardsOf sorts ascending, S.addGraveyardCard mints in call
  -- order) -- the only order in which a live-board reading has a Forest to offer.
  -- The mirrored leg pins that the answer does not depend on it, which is CR
  -- 608.2f's point. The nine lands are Swamps, not Forests, so the only Forest
  -- anywhere on the board is one Ashaya would have made.
  --
  -- Wood Elemental's power and toughness ride along, read after one SBA sweep:
  -- its CDA reads the count it sacrificed (CR 208.2a), so 0 is a 0/0 that CR
  -- 704.5f buries and 1 is a 1/1 still standing -- a second reading of the same
  -- divergence, on the same board, that the Piker count cannot be confused with.
  Spec.it s "CR 614.12 a Wood Elemental reanimated beside Ashaya sacrifices nothing (#78)" $ do
    swamp <- S.printingOf s registry "Swamp"
    rise <- S.printingOf s registry "Rise of the Dark Realms"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    woodElemental <- S.printingOf s registry "Wood Elemental"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let pikerName = CardName.MkCardName (Text.pack "Goblin Piker")
        outcome buried =
          let (_, withPiker) = S.addPermanent pikerPrinting S.alice (S.landsInPlay swamp 9)
              graves = List.foldl' (\g printing -> snd (S.addGraveyardCard printing S.alice g)) withPiker buried
              (gs, spellId) = S.handOne rise graves
              after = S.settleSba (castAndResolve sacrificesAll gs spellId)
              pikers = length [oid | oid <- Set.toList (GameState.battlefield after), Projection.hasName pikerName oid after]
           in (pikers, fmap (`S.powerToughnessOf` after) (newestNamed (CardName.MkCardName (Text.pack "Wood Elemental")) after))
        ashayaFirst = outcome [ashaya, woodElemental]
        elementalFirst = outcome [woodElemental, ashaya]
    Spec.assertEqWith s "the Piker was never a Forest, so it was not offered and it lives -- and the Wood Elemental that sacrificed nothing is a 0/0 CR 704.5f buried" ashayaFirst (1, Nothing)
    Spec.assertEqWith s "and the batch's processing order changes nothing (CR 608.2f)" elementalFirst ashayaFirst
  -- CR 614.1d: "Continuous effects that read '[This permanent] enters . . .' or
  -- '[Objects] enter [the battlefield] . . .' are replacement effects." Zof
  -- Bloodbog prints one sentence of exactly that shape -- "This land enters
  -- tapped" -- and no effect is involved anywhere on the path: CR 305.1's special
  -- action simply puts the land onto the battlefield, so the rewrite has to be
  -- read off the permanent's own text through the CR 616.1 loop.
  --
  -- Played through the real priority loop rather than through the entry funnel,
  -- so what is asserted is the whole path a player actually takes to CR 305.1.
  --
  -- The tap state on its own is a field this same code just wrote, so what the
  -- case measures is what a PLAYER loses by it: Typhoid Rats, a {B} creature, is
  -- in the hand beside the land, and the tapped land's "{T}: Add {B}" is the only
  -- mana in the game. So the Rats stays uncast here and enters on the SAME board
  -- with that one land forced untapped -- the only difference entering tapped
  -- makes. Without the second half the first would pass on a board where the Rats
  -- was uncastable for some other reason.
  --
  -- CR 302.6 has nothing to say either way: Zof Bloodbog is a land, so summoning
  -- sickness is not what is being measured.
  Spec.it s "CR 614.1d Zof Bloodbog's own text makes it enter TAPPED" $ do
    zof <- S.printingOf s registry "Zof Consumption"
    rats <- S.printingOf s registry "Typhoid Rats"
    let (_, withZof) = S.addHandCard zof S.alice (Setup.emptyGame S.bothPlayers)
        (_, filled) = S.addHandCard rats S.alice withZof
        board =
          filled
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
        played = S.runPure S.playLandAnswer board Engine.priorityLoop
        untap oid gs = gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Untapped}) oid (GameState.objects gs)}
        ratsName = CardName.MkCardName (Text.pack "Typhoid Rats")
        ratsOut gs = length [o | o <- Set.toList (GameState.battlefield gs), Projection.hasName ratsName o gs]
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "the land entered tapped" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Tapped)
        Spec.assertEqWith
          s
          "so the {B} creature in hand stays there -- no mana to cast it with"
          (ratsOut (S.runPure castOrPassAnswer played Engine.priorityLoop))
          0
        Spec.assertEqWith
          s
          "and the same land untapped pays for it"
          (ratsOut (S.runPure castOrPassAnswer (untap permId played) Engine.priorityLoop))
          1
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  -- CR 614.1c: "Effects that read ... 'As [this permanent] enters . . .' ... are
  -- replacement effects." Razorgrass Field -- the land face of the modal
  -- double-faced Razorgrass Ambush // Razorgrass Field -- prints one of exactly
  -- that shape with a PRICE in it: "As this land enters, you may pay 3 life. If
  -- you don't, it enters tapped."
  --
  -- The pair of cases below is one fixture answered two ways, so nothing but the
  -- answer differs between them. Played through the real priority loop, the whole
  -- path a player takes to CR 305.1, exactly as the Zof Bloodbog case above is.
  --
  -- What each case measures is not the tap-state field this same code just wrote
  -- but what a PLAYER gets for the 3 life: Soul Warden, a {W} creature, sits in
  -- the hand beside the land, and the land's "{T}: Add {W}" is the only mana in
  -- the game. So declining leaves the Warden uncast and paying gets it onto the
  -- battlefield -- Activate.activatable is deliberately NOT asked, since CR
  -- 605.3b keeps a mana ability off the stack and it answers False for one on
  -- every board.
  --
  -- The life is asserted twice over: the total, and CR 119.4's "in other words,
  -- the player loses that much life" as a recorded GameEvent.LifeLost. The second
  -- is the channel every life-loss trigger in the pool reads -- Mindcrank's and
  -- Exquisite Blood's "whenever an opponent loses life" watch this same
  -- GameEvent -- so a payment that quietly subtracted from the total instead of
  -- going through the CR 119.4 door would pass the first assertion and fail this
  -- one.
  --
  -- Soul Warden's own "whenever ANOTHER creature enters" cannot move the total:
  -- it is the only creature in the fixture, and both life assertions are read off
  -- the board the land play left, before it is ever cast.
  Spec.it s "CR 614.1c Razorgrass Field DECLINED enters tapped and costs no life" $ do
    razorgrass <- S.printingOf s registry "Razorgrass Ambush"
    warden <- S.printingOf s registry "Soul Warden"
    let played = S.runPure (payLifeOnEntryAnswer OptionalDecision.Declines) (razorgrassBoard razorgrass warden) Engine.priorityLoop
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "the land entered tapped" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Tapped)
        Spec.assertEqWith s "and cost nothing" (S.lifeOf S.alice played) (Just 20)
        Spec.assertBool s (not (lostLife S.alice 3 played)) "no life loss was recorded"
        Spec.assertEqWith
          s
          "so the {W} creature in hand stays there -- no mana to cast it with"
          (wardenOut (S.runPure castOrPassAnswer played Engine.priorityLoop))
          0
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  Spec.it s "CR 614.1c Razorgrass Field PAID FOR enters untapped, for exactly 3 life" $ do
    razorgrass <- S.printingOf s registry "Razorgrass Ambush"
    warden <- S.printingOf s registry "Soul Warden"
    let played = S.runPure (payLifeOnEntryAnswer OptionalDecision.Exercises) (razorgrassBoard razorgrass warden) Engine.priorityLoop
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "the land entered untapped" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Untapped)
        Spec.assertEqWith s "20 - 3" (S.lifeOf S.alice played) (Just 17)
        Spec.assertBool s (lostLife S.alice 3 played) "the payment was recorded as a life loss (CR 119.4)"
        Spec.assertEqWith
          s
          "and the untapped land pays for the {W} creature"
          (wardenOut (S.runPure castOrPassAnswer played Engine.priorityLoop))
          1
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  -- The pool's other printing of the same CR 614.1c sentence: Sea Gate, Reborn,
  -- the land face of Sea Gate Restoration // Sea Gate, Reborn ("As this land
  -- enters, you may pay 3 life. If you don't, it enters tapped." -- oracle checked
  -- on Scryfall). Razorgrass Field's pair above proves the rewrite; this pair
  -- proves the CARD reaches it, which it did not while the face was transcribed as
  -- a bare EntryRewrite.Tapped.
  --
  -- The PAID case is the one that carries that, and it is the only one that can:
  -- a bare Tapped leaves exactly the board declining leaves, so the DECLINED case
  -- below passes either way and is a regression fence rather than a proof.
  --
  -- Tidal Warrior, a {U} creature with no enters trigger, plays Soul Warden's part
  -- above: the land's "{T}: Add {U}" is the only mana in the game, so what the
  -- 3 life buys is read off whether the Warrior gets cast.
  Spec.it s "CR 614.1c Sea Gate, Reborn DECLINED enters tapped and costs no life" $ do
    seaGate <- S.printingOf s registry "Sea Gate Restoration"
    warrior <- S.printingOf s registry "Tidal Warrior"
    let played = S.runPure (payLifeOnEntryAnswer OptionalDecision.Declines) (seaGateBoard seaGate warrior 20) Engine.priorityLoop
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "the land entered tapped" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Tapped)
        Spec.assertEqWith s "and cost nothing" (S.lifeOf S.alice played) (Just 20)
        Spec.assertBool s (not (lostLife S.alice 3 played)) "no life loss was recorded"
        Spec.assertEqWith
          s
          "so the {U} creature in hand stays there -- no mana to cast it with"
          (warriorOut (S.runPure castOrPassAnswer played Engine.priorityLoop))
          0
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  Spec.it s "CR 614.1c Sea Gate, Reborn PAID FOR enters untapped, for exactly 3 life" $ do
    seaGate <- S.printingOf s registry "Sea Gate Restoration"
    warrior <- S.printingOf s registry "Tidal Warrior"
    let played = S.runPure (payLifeOnEntryAnswer OptionalDecision.Exercises) (seaGateBoard seaGate warrior 20) Engine.priorityLoop
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "the land entered untapped" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Untapped)
        Spec.assertEqWith s "20 - 3" (S.lifeOf S.alice played) (Just 17)
        Spec.assertBool s (lostLife S.alice 3 played) "the payment was recorded as a life loss (CR 119.4)"
        Spec.assertEqWith
          s
          "and the untapped land pays for the {U} creature"
          (warriorOut (S.runPure castOrPassAnswer played Engine.priorityLoop))
          1
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  -- CR 119.4: a player may pay an amount of life greater than 0 only if their
  -- life total is at least that amount. So a tapped Sea Gate, Reborn has TWO
  -- causes -- the controller declined, or the controller could not pay -- and the
  -- pair below is what tells them apart.
  --
  -- The answerer is pinned to Exercises in BOTH cases, so the ANSWER is held
  -- fixed and the only difference between the two boards is alice's life total:
  -- 4, where paying 3 is legal, against 2, where it is not. The engine's own
  -- CR 119.4 gate is therefore the only thing that can move the outcome. Were
  -- the gate dropped, the 2-life board would pay anyway and enter untapped; were
  -- the prompt never raised at all, the 4-life board would enter tapped.
  --
  -- 4 and 2 rather than 3 and 2, because paying 3 at 3 life leaves 0 and CR
  -- 704.5a ends the game before the assertions run.
  Spec.it s "CR 119.4 at 4 life the payment is legal, so Sea Gate, Reborn enters untapped" $ do
    seaGate <- S.printingOf s registry "Sea Gate Restoration"
    warrior <- S.printingOf s registry "Tidal Warrior"
    let played = S.runPure (payLifeOnEntryAnswer OptionalDecision.Exercises) (seaGateBoard seaGate warrior 4) Engine.priorityLoop
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "untapped" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Untapped)
        Spec.assertEqWith s "4 - 3" (S.lifeOf S.alice played) (Just 1)
        Spec.assertEqWith
          s
          "and the untapped land pays for the {U} creature"
          (warriorOut (S.runPure castOrPassAnswer played Engine.priorityLoop))
          1
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  Spec.it s "CR 119.4 at 2 life the payment is ILLEGAL, so it enters tapped with no life paid" $ do
    seaGate <- S.printingOf s registry "Sea Gate Restoration"
    warrior <- S.printingOf s registry "Tidal Warrior"
    let played = S.runPure (payLifeOnEntryAnswer OptionalDecision.Exercises) (seaGateBoard seaGate warrior 2) Engine.priorityLoop
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "tapped, though the answerer said pay" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Tapped)
        Spec.assertEqWith s "and the 2 life is untouched" (S.lifeOf S.alice played) (Just 2)
        Spec.assertBool s (not (lostLife S.alice 3 played)) "no life loss was recorded"
        Spec.assertEqWith
          s
          "so the {U} creature in hand stays there"
          (warriorOut (S.runPure castOrPassAnswer played Engine.priorityLoop))
          0
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  -- CR 614.1c's other price: Rustic Clachan, "As this land enters, you may reveal
  -- a Kithkin card from your hand. If you don't, this land enters tapped" (oracle
  -- checked on Scryfall). The three cases below are one fixture in three states,
  -- and they differ pairwise in exactly one thing each: the first two share a
  -- board and differ only in the ANSWER, the first and third share an answer that
  -- names the held creature and differ only in whether that creature is a Kithkin.
  --
  -- The land enters in all three, so "entered untapped" is told apart from
  -- "entered" -- each case reads the tap state of the one permanent on the board.
  --
  -- Mosquito Guard ({W} 1/1 Kithkin Soldier) and Benalish Hero ({W} 1/1 Human
  -- Soldier) are the pair. Neither has an enters trigger, and the Clachan's
  -- "{T}: Add {W}" is the only mana in the game, so what the reveal buys is read
  -- off whether the creature gets cast -- Activate.activatable is deliberately not
  -- asked, since CR 605.3b keeps a mana ability off the stack.
  --
  -- The reveal is asserted through S.revealsOf, CR 701.20a's public log, and not
  -- through the tap state alone: showing a card is the whole of what the player
  -- did, and a rewrite that left the land untapped without revealing anything
  -- would pass the tap assertion.
  Spec.it s "CR 614.1c Rustic Clachan REVEALING a Kithkin card enters untapped" $ do
    clachan <- S.printingOf s registry "Rustic Clachan"
    guard_ <- S.printingOf s registry "Mosquito Guard"
    let (guardId, board) = clachanBoard clachan guard_
        played = S.runPure (revealOnEntryAnswer (Just guardId)) board Engine.priorityLoop
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "the land entered untapped" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Untapped)
        Spec.assertEqWith
          s
          "and alice showed the Kithkin card to do it (CR 701.20a)"
          (S.revealsOf played)
          [(S.alice, Set.singleton (CardName.MkCardName (Text.pack "Mosquito Guard")))]
        Spec.assertEqWith s "one ChooseRevealOnEntry was raised" (revealAsks (answersFor (revealOnEntryAnswer (Just guardId)) board Engine.priorityLoop)) 1
        Spec.assertEqWith
          s
          "and the untapped land pays for the {W} creature"
          (namedOut "Mosquito Guard" (S.runPure castOrPassAnswer played Engine.priorityLoop))
          1
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  -- The "may" half. CR 614.1c states the reveal as optional, so holding the
  -- Kithkin card is not being made to show it -- and this is the case that proves
  -- the answer is the player's rather than the engine's, since the board is the
  -- one above's exactly.
  Spec.it s "CR 614.1c DECLINING with a Kithkin card in hand still enters tapped" $ do
    clachan <- S.printingOf s registry "Rustic Clachan"
    guard_ <- S.printingOf s registry "Mosquito Guard"
    let (_, board) = clachanBoard clachan guard_
        played = S.runPure (revealOnEntryAnswer Nothing) board Engine.priorityLoop
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "the land entered tapped" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Tapped)
        Spec.assertEqWith s "and nothing was shown" (S.revealsOf played) []
        Spec.assertEqWith
          s
          "so the {W} creature in hand stays there -- no mana to cast it with"
          (namedOut "Mosquito Guard" (S.runPure castOrPassAnswer played Engine.priorityLoop))
          0
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  -- The negative, and the case the printed filter is for. The answerer is pinned
  -- to the held creature in BOTH this case and the first, so the only difference
  -- between the two boards is whether that creature is a Kithkin: the engine's own
  -- filter is the only thing that can move the outcome. Were the offer unfiltered,
  -- the Hero would be shown and the land would enter untapped.
  Spec.it s "CR 614.1c with NO Kithkin card in hand it enters tapped, unasked" $ do
    clachan <- S.printingOf s registry "Rustic Clachan"
    hero <- S.printingOf s registry "Benalish Hero"
    let (heroId, board) = clachanBoard clachan hero
        played = S.runPure (revealOnEntryAnswer (Just heroId)) board Engine.priorityLoop
    case Set.toList (GameState.battlefield played) of
      [permId] -> do
        Spec.assertEqWith s "the land entered tapped" (fmap Object.tapped (Game.lookupObject permId played)) (Just TapState.Tapped)
        Spec.assertEqWith s "and the non-Kithkin card was not shown" (S.revealsOf played) []
        Spec.assertEqWith s "and no ChooseRevealOnEntry was raised -- nothing in hand to show" (revealAsks (answersFor (revealOnEntryAnswer (Just heroId)) board Engine.priorityLoop)) 0
        Spec.assertEqWith
          s
          "so the {W} creature in hand stays there"
          (namedOut "Benalish Hero" (S.runPure castOrPassAnswer played Engine.priorityLoop))
          0
      other -> Spec.assertFailure s ("expected one permanent, got " <> show (length other))
  Spec.it s "CR 707.5 declining the copy leaves a 0/0 that dies (CR 704.5f)" $ do
    island <- S.printingOf s registry "Island"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let base = S.landsInPlay island 4
        (_, withPiker) = S.addPermanent pikerPrinting S.alice base
        (gs, cloneId) = S.handOne clone withPiker
        -- S.identityAnswer declines ChooseCopyTarget (Clone's own "may").
        resolved = S.runPure S.identityAnswer gs (S.cast S.alice cloneId >> Stack.resolveTop >> Engine.settleForPriority)
        named = filter (\oid -> fmap Face.name (Game.faceOf oid resolved) == Just (CardName.MkCardName $ Text.pack "Clone")) (Set.toList (GameState.battlefield resolved))
    Spec.assertEqWith s "the 0/0 Clone is gone" named []
  Spec.it s "CR 614.12a the copy choice is locked in BEFORE the enters event exists" $ do
    island <- S.printingOf s registry "Island"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    clonePrinting <- S.printingOf s registry "Clone"
    let base = S.landsInPlay island 4
        (piker, withPiker) = S.addPermanent pikerPrinting S.alice base
        (gs, cloneId) = S.handOne clonePrinting withPiker
        -- No settle: the choice must already be made when resolveTop returns.
        resolved = S.runPure (copyOf piker) gs (S.cast S.alice cloneId >> Stack.resolveTop)
        named = filter (\oid -> fmap Face.name (Game.faceOf oid resolved) == Just (CardName.MkCardName $ Text.pack "Clone")) (Set.toList (GameState.battlefield resolved))
    case named of
      [] -> Spec.assertFailure s "Clone did not reach the battlefield"
      clone : _ -> Spec.assertEqWith s "already a 2/1, with no settle run" (Projection.powerOf clone resolved) (Just 2)
  Spec.it s "CR 208.2b Primal Plasma enters as the 2/2 with flying its controller picked" $ do
    island <- S.printingOf s registry "Island"
    primalPlasma <- S.printingOf s registry "Primal Plasma"
    let (gs, held) = blueBoard island 4 [primalPlasma]
    case held of
      plasmaCard : _ ->
        let after = S.runPure (enteringAs 1) gs (S.cast S.alice plasmaCard >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Primal Plasma") after of
              Nothing -> Spec.assertFailure s "Primal Plasma did not reach the battlefield"
              Just plasma -> do
                Spec.assertEqWith s "power" (Projection.powerOf plasma after) (Just 2)
                Spec.assertEqWith s "toughness" (Projection.toughnessOf plasma after) (Just 2)
                Spec.assertBool s (Projection.hasKeyword Keyword.Flying plasma after) "flying"
      _ -> Spec.assertFailure s "fixture did not deal a card"
  Spec.it s "CR 616.2 a Clone of a 2/2-flying Plasma that picks 1/6 is 1/6 with flying AND defender" $ do
    -- THE CENTERPIECE, and the Gatherer ruling verbatim: "it copies the values
    -- determined by its enters-the-battlefield replacement effect, but its
    -- power and toughness are determined by the copy's own
    -- enters-the-battlefield replacement effect."
    island <- S.printingOf s registry "Island"
    primalPlasma <- S.printingOf s registry "Primal Plasma"
    clonePrinting <- S.printingOf s registry "Clone"
    let (gs, held) = blueBoard island 8 [primalPlasma, clonePrinting]
    case held of
      plasmaCard : cloneCard : _ ->
        let withPlasma = S.runPure (enteringAs 1) gs (S.cast S.alice plasmaCard >> Stack.resolveTop)
            after = S.runPure (enteringAs 2) withPlasma (S.cast S.alice cloneCard >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Clone") after of
              Nothing -> Spec.assertFailure s "Clone did not reach the battlefield"
              Just clone -> do
                Spec.assertEqWith s "power is the CLONE's own choice" (Projection.powerOf clone after) (Just 1)
                Spec.assertEqWith s "toughness is the CLONE's own choice" (Projection.toughnessOf clone after) (Just 6)
                Spec.assertBool s (Projection.hasKeyword Keyword.Flying clone after) "flying came from the COPY"
                Spec.assertBool s (Projection.hasKeyword Keyword.Defender clone after) "defender came from the CHOICE"
      _ -> Spec.assertFailure s "fixture did not deal two cards"
  Spec.it s "CR 616.2 the same Clone picking 3/3 is a 3/3 with flying" $ do
    island <- S.printingOf s registry "Island"
    primalPlasma <- S.printingOf s registry "Primal Plasma"
    clonePrinting <- S.printingOf s registry "Clone"
    let (gs, held) = blueBoard island 8 [primalPlasma, clonePrinting]
    case held of
      plasmaCard : cloneCard : _ ->
        let withPlasma = S.runPure (enteringAs 1) gs (S.cast S.alice plasmaCard >> Stack.resolveTop)
            after = S.runPure (enteringAs 0) withPlasma (S.cast S.alice cloneCard >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Clone") after of
              Nothing -> Spec.assertFailure s "Clone did not reach the battlefield"
              Just clone -> do
                Spec.assertEqWith s "3/3" (Projection.powerOf clone after) (Just 3)
                Spec.assertEqWith s "3/3" (Projection.toughnessOf clone after) (Just 3)
                Spec.assertBool s (Projection.hasKeyword Keyword.Flying clone after) "still flying (keywords UNION, never assign)"
                Spec.assertBool s (not (Projection.hasKeyword Keyword.Defender clone after)) "no defender"
      _ -> Spec.assertFailure s "fixture did not deal two cards"
  Spec.it s "CR 707.2 a Clone of that Clone copies 1/6-flying-defender and then chooses again" $ do
    island <- S.printingOf s registry "Island"
    primalPlasma <- S.printingOf s registry "Primal Plasma"
    clonePrinting <- S.printingOf s registry "Clone"
    let (gs, held) = blueBoard island 12 [primalPlasma, clonePrinting, clonePrinting]
    case held of
      plasmaCard : cloneA : cloneB : _ ->
        let s1 = S.runPure (enteringAs 1) gs (S.cast S.alice plasmaCard >> Stack.resolveTop)
            s2 = S.runPure (enteringAs 2) s1 (S.cast S.alice cloneA >> Stack.resolveTop)
            s3 = S.runPure (enteringAs 0) s2 (S.cast S.alice cloneB >> Stack.resolveTop)
         in case newestNamed (CardName.MkCardName $ Text.pack "Clone") s3 of
              Nothing -> Spec.assertFailure s "the second Clone did not reach the battlefield"
              Just clone -> do
                Spec.assertEqWith s "its OWN choice wins on P/T" (Projection.powerOf clone s3) (Just 3)
                Spec.assertEqWith s "its OWN choice wins on P/T" (Projection.toughnessOf clone s3) (Just 3)
                Spec.assertBool s (Projection.hasKeyword Keyword.Flying clone s3 && Projection.hasKeyword Keyword.Defender clone s3) "flying and defender rode the copy chain"
      _ -> Spec.assertFailure s "fixture did not deal three cards"
  -- CR 705.2's FIRST sentence, the flip nobody wins: Molten Sentry {3}{R}
  -- Creature -- Elemental */*, "As this creature enters, flip a coin. If the
  -- coin comes up heads, this creature enters as a 5/2 creature with haste. If
  -- it comes up tails, this creature enters as a 2/5 creature with defender."
  --
  -- THE BOARD carries a Tavern Scoundrel ("Whenever you win a coin flip, create
  -- two Treasure tokens") under the same seat, and it is the discrimination.
  -- The shortest wrong implementation of this rewrite is the road already built
  -- -- Pawl.Engine.Resolve's Effect.FlipCoin arm under CoinReading.Wins, which
  -- calls the coin first and records the flip as WON when the call matches. The answerer below pins
  -- both questions to the same face, so that road wins its flip and the
  -- Scoundrel mints two Treasures; the correct road asks no call, records no
  -- outcome -- nothing on this board states one under CR 705.3, which
  -- Pawl.CoinSpec's Edgar case is the other side of -- and mints none. 0 against
  -- 2, asserted FIRST so no proxy absorbs it.
  --
  -- CR 603.3 IS THE SEQUENCING, as in Pawl.EventTriggerSpec's Scoundrel case:
  -- the flip happens inside the entry replacement, so a trigger off it would
  -- reach the stack only at the next priority. `runSentry` runs the
  -- place/resolve cycle twice, which is what gives a wrongly-won flip room to
  -- pay out.
  --
  -- THE P/T assertions run on both faces, one board apiece, so the face-to-option
  -- mapping is pinned in both directions: a swapped mapping cannot pass both.
  Spec.it s "CR 705.2 nobody wins Molten Sentry's flip, so its heads face mints no Treasure" $ do
    mountain <- S.printingOf s registry "Mountain"
    scoundrel <- S.printingOf s registry "Tavern Scoundrel"
    sentry <- S.printingOf s registry "Molten Sentry"
    let (board, spellId) = sentryBoard mountain scoundrel sentry
        after = runSentry CoinFace.Heads board spellId
    Spec.assertEqWith
      s
      "CR 705.2: no player won the flip, so the Scoundrel minted nothing"
      (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Treasure Token") S.alice after)
      0
    -- The flip HAPPENED and was recorded with no outcome, which keeps the zero
    -- above from passing for a Sentry that never flipped at all. Asserted as the
    -- WHOLE list of flips rather than as membership: CR 705.1's sentence
    -- instructs one flip, so a rewrite applied twice is as wrong as one applied
    -- never.
    Spec.assertEqWith
      s
      "CR 705.1: one flip, and CR 705.2 left it with no winner"
      (filter isFlip (S.eventsOf after))
      [GameEvent.CoinFlipped CoinFlipped.MkCoinFlipped {CoinFlipped.flipper = S.alice, CoinFlipped.won = Nothing}]
    case newestNamed (CardName.MkCardName $ Text.pack "Molten Sentry") after of
      Nothing -> Spec.assertFailure s "Molten Sentry did not reach the battlefield"
      Just sentryId -> do
        Spec.assertEqWith s "CR 208.2b: heads is the 5/2" (S.powerToughnessOf sentryId after) (Just (5, 2))
        Spec.assertBool s (Projection.hasKeyword Keyword.Haste sentryId after) "with haste"
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Defender sentryId after)) "and not defender"
    -- No call was ever made, which is the other half of rule 705.2's first
    -- sentence: the flip was asked (Response.FlippedCoin) and nobody was asked
    -- to call it.
    let asked = answersFor (sentryAnswer CoinFace.Heads) board (S.cast S.alice spellId >> Stack.resolveTop)
    Spec.assertBool s (elem (Response.FlippedCoin CoinFace.Heads) asked) "the coin was flipped"
    Spec.assertBool s (not (any wasCall asked)) "and no CallCoin was raised"
  Spec.it s "CR 705.2 the same flip coming up tails is the 2/5 with defender" $ do
    mountain <- S.printingOf s registry "Mountain"
    scoundrel <- S.printingOf s registry "Tavern Scoundrel"
    sentry <- S.printingOf s registry "Molten Sentry"
    let (board, spellId) = sentryBoard mountain scoundrel sentry
        after = runSentry CoinFace.Tails board spellId
    Spec.assertEqWith
      s
      "CR 705.2: a tails flip has no winner either"
      (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Treasure Token") S.alice after)
      0
    case newestNamed (CardName.MkCardName $ Text.pack "Molten Sentry") after of
      Nothing -> Spec.assertFailure s "Molten Sentry did not reach the battlefield"
      Just sentryId -> do
        Spec.assertEqWith s "CR 208.2b: tails is the 2/5" (S.powerToughnessOf sentryId after) (Just (2, 5))
        Spec.assertBool s (Projection.hasKeyword Keyword.Defender sentryId after) "with defender"
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Haste sentryId after)) "and not haste"
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
  Spec.it s "CR 400.3 an Opponents zone-change redirect exiles an opponent's card, not your own" $ do
    -- Leyline of the Void's shape without the Leyline: a floating redirect
    -- whose source alice controls. Bob's card is exiled on the way to his
    -- graveyard; alice's own reaches hers untouched.
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (src, g1) = S.addPermanent pikerPrinting S.alice (Setup.emptyGame S.bothPlayers)
        (mine, g2) = S.addPermanent pikerPrinting S.alice g1
        (theirs, g3) = S.addPermanent pikerPrinting S.bob g2
        g4 = S.addReplacement (leylineShape src (fst (Game.freshTimestamp g3))) g3
        after = S.runPure S.identityAnswer g4 (Event.changeZone mine Zone.Graveyard >> Event.changeZone theirs Zone.Graveyard)
    Spec.assertEqWith s "alice's own card reaches her graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "bob's does not" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 0
    Spec.assertEqWith s "it was exiled instead" (length (Game.zoneMembers Zone.Exile S.bob after)) 1
  Spec.it s "CR 400.3 the zone-change subject is the card's OWNER, not its controller" $ do
    -- A card alice OWNS but bob CONTROLS still dies to alice's graveyard
    -- (CR 400.3), so alice's own redirect must not exile it. A
    -- controller-based test would, which is the case this pins.
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let slot = SlotName.MkSlotName (Text.pack "target")
        (src, g1) = S.addPermanent pikerPrinting S.alice (Setup.emptyGame S.bothPlayers)
        (oid, g2) = S.addPermanent pikerPrinting S.alice g1
        g3 = S.addReplacement (leylineShape src (fst (Game.freshTimestamp g2))) g2
        stolen =
          S.runPure S.identityAnswer g3 $
            Resolve.applyEffect S.noSource S.noSource S.bob (Map.singleton slot (Set.singleton (Recipient.ToObject oid))) (Map.singleton slot (Set.singleton (Recipient.ToObject oid))) (Effect.GainControl (DurationRef.MkDurationRef Duration.Indefinite (ObjectRef.InSlot slot)))
        after = S.runPure S.identityAnswer stolen (Event.changeZone oid Zone.Graveyard)
    Spec.assertEqWith s "bob really did take control of it" (Projection.controllerOf oid stolen) (Just S.bob)
    Spec.assertEqWith s "it reaches its OWNER's graveyard, unexiled" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    Spec.assertEqWith s "and nothing was exiled" (length (Game.zoneMembers Zone.Exile S.alice after)) 0
  Spec.it s "CR 208.2b a single-option ChoiceOf is not a choice and must not prompt" $ do
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (piker, g1) = S.addPermanent pikerPrinting S.alice (Setup.emptyGame S.bothPlayers)
        (ts, g2) = Game.freshTimestamp g1
        onlyOption = EntryOption.MkEntryOption {EntryOption.power = 3, EntryOption.toughness = 3, EntryOption.keywords = Set.empty}
        active =
          ActiveReplacement.MkActiveReplacement
            { ActiveReplacement.effect = ReplacementEffect.EntryR (EntryR.MkEntryR Filter.Type.IsSource (EntryRewrite.ChoiceOf [onlyOption])),
              ActiveReplacement.source = piker,
              ActiveReplacement.controller = S.alice,
              ActiveReplacement.timestamp = ts,
              ActiveReplacement.expiry = Expiry.AtCleanup,
              ActiveReplacement.uses = Uses.Once,
              ActiveReplacement.origin = ReplacementOrigin.Other,
              ActiveReplacement.condition = Nothing,
              ActiveReplacement.rider = Nothing,
              ActiveReplacement.slots = Map.empty
            }
        g3 = S.addReplacement active g2
        asked = answersFor S.identityAnswer g3 (Event.runEntry Set.empty piker)
        after = S.runPure S.identityAnswer g3 (Event.runEntry Set.empty piker)
    Spec.assertBool s (not (wasAskedForEntryOption asked)) "no ChooseEntryOption was raised"
    Spec.assertEqWith s "the sole option applied anyway" (Projection.powerOf piker after) (Just 3)
  Spec.it s "CR 614.16 Doubling Season turns Dragon Fodder's two Goblins into four" $ do
    mountain <- S.printingOf s registry "Mountain"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    dragonFodder <- S.printingOf s registry "Dragon Fodder"
    let base = S.landsInPlay mountain 2
        (_, g1) = S.addPermanent doublingSeason S.alice base
        (g2, spellId) = S.handOne dragonFodder g1
        after = castAndResolve S.identityAnswer g2 spellId
    Spec.assertEqWith s "twice that many" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Token") S.alice after) 4
  Spec.it s "CR 614.5 two Doubling Seasons are two instances: eight Goblins" $ do
    mountain <- S.printingOf s registry "Mountain"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    dragonFodder <- S.printingOf s registry "Dragon Fodder"
    let base = S.landsInPlay mountain 2
        (_, g1) = S.addPermanent doublingSeason S.alice base
        (_, g2) = S.addPermanent doublingSeason S.alice g1
        (g3, spellId) = S.handOne dragonFodder g2
        after = castAndResolve S.identityAnswer g3 spellId
    Spec.assertEqWith s "2 -> 4 -> 8" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Token") S.alice after) 8
  Spec.it s "CR 614.1 Doubling Season's OTHER clause doubles counters, not tokens" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, _) = counterBoard forest battlegrowth [doublingSeason, pikerPrinting] []
    case mine of
      season : piker : _ ->
        let after = castAndResolve (raceAnswer season piker) gs spellId
         in Spec.assertEqWith s "1 * 2" (countersOn CounterKind.PlusOnePlusOne piker after) 2
      _ -> Spec.assertFailure s "fixture did not build two permanents"
  Spec.it s "CR 616.1 Doubling Season racing Hardened Scales: 4 or 3, by the prompt" $ do
    forest <- S.printingOf s registry "Forest"
    battlegrowth <- S.printingOf s registry "Battlegrowth"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    hardenedScales <- S.printingOf s registry "Hardened Scales"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let (gs, spellId, mine, _) = counterBoard forest battlegrowth [doublingSeason, hardenedScales, pikerPrinting] []
    case mine of
      season : scales : piker : _ ->
        let seasonFirst = castAndResolve (raceAnswer season piker) gs spellId
            scalesFirst = castAndResolve (raceAnswer scales piker) gs spellId
         in do
              Spec.assertEqWith s "(1 * 2) + 1" (countersOn CounterKind.PlusOnePlusOne piker seasonFirst) 3
              Spec.assertEqWith s "(1 + 1) * 2" (countersOn CounterKind.PlusOnePlusOne piker scalesFirst) 4
      _ -> Spec.assertFailure s "fixture did not build three permanents"
  -- CR 614.16 read against CR 601.2h, and the pair that says which of the two
  -- subjects an answer is about. Soul Immolation's "as an additional cost to cast
  -- this spell, blight X" is paid while the spell is being CAST, so CR 609.1
  -- gives the placement no resolving spell or ability to be the effect of and
  -- rule 614.16's row does not apply -- Pawl.Types.CounterCause.ByPayment, which
  -- Pawl.Engine.Cost.counterCause chooses off the payment's moment; see #1647.
  --
  -- Wall of Stone is 0/8 so BOTH readings leave it alive: three counters and six
  -- are each an assertable count, where a creature that died under the doubled
  -- reading would report zero and confuse "not doubled" with "gone".
  Spec.it s "CR 614.16 Doubling Season does NOT double a blight paid to cast the spell" $ do
    mountain <- S.printingOf s registry "Mountain"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    immolation <- S.printingOf s registry "Soul Immolation"
    let (_, g1) = S.addPermanent doublingSeason S.alice (S.landsInPlay mountain 5)
        (wall, g2) = S.addPermanent wallOfStone S.alice g1
        (g3, spellId) = S.handOne immolation g2
        after = castAndResolve (blightAnswer wall) g3 spellId
    Spec.assertEqWith s "X counters, not 2X" (countersOn CounterKind.MinusOneMinusOne wall after) 3
    -- The spell really resolved, so the count above is the paid cost's and not a
    -- reversed announcement's: CR 601.2e would have left bob on 20.
    Spec.assertEqWith s "and the spell dealt its three" (S.lifeOf S.bob after) (Just 17)
  -- The CONTROL, one thing changed: Vorinclex, Monstrous Raider's clause names a
  -- PLAYER ("if you would put") rather than an effect, and the payer is a player
  -- whatever moment they pay at -- so this one DOES double the same blight. A fix
  -- that made a cost-paid placement invisible to CR 614.1 altogether, rather than
  -- to rule 614.16's subject alone, fails here.
  Spec.it s "CR 614.1 Vorinclex DOES double the same blight" $ do
    mountain <- S.printingOf s registry "Mountain"
    vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    immolation <- S.printingOf s registry "Soul Immolation"
    let (_, g1) = S.addPermanent vorinclex S.alice (S.landsInPlay mountain 5)
        (wall, g2) = S.addPermanent wallOfStone S.alice g1
        (g3, spellId) = S.handOne immolation g2
        after = castAndResolve (blightAnswer wall) g3 spellId
    Spec.assertEqWith s "twice that many" (countersOn CounterKind.MinusOneMinusOne wall after) 6
    Spec.assertEqWith s "and the spell dealt its three" (S.lifeOf S.bob after) (Just 17)
  -- The THIRD board, and what says the split is on the MOMENT rather than on the
  -- keyword action: Boggart Mischief's "you may blight 1" is CR 118.12's cost,
  -- paid as the trigger RESOLVES, so CR 609.1 does give it an effect and rule
  -- 614.16's row applies. Same card data, same Pawl.Types.CostComponent, opposite
  -- answer from the case above. A fix reading "a blight cost is never doubled"
  -- fails here.
  Spec.it s "CR 118.12 Doubling Season DOES double a blight paid as the trigger resolves" $ do
    swamp <- S.printingOf s registry "Swamp"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    mischief <- S.printingOf s registry "Boggart Mischief"
    let (_, g1) = S.addPermanent doublingSeason S.alice (S.landsInPlay swamp 3)
        (wall, g2) = S.addPermanent wallOfStone S.alice g1
        (g3, spellId) = S.handOne mischief g2
        -- CR 603.3: the enters trigger is put on the stack by the next CR 117.5
        -- scan, not by the resolution that fired it, so the spell's resolution is
        -- settled for priority before the trigger's own resolveTop.
        entered = S.runPure (blightAnswer wall) g3 (S.cast S.alice spellId >> Stack.resolveTop >> Engine.settleForPriority)
        after = S.runPure (blightAnswer wall) entered Stack.resolveTop
    Spec.assertEqWith s "one counter became two" (countersOn CounterKind.MinusOneMinusOne wall after) 2
    -- The rider ran, so the payment was made rather than refused, and Doubling
    -- Season's OTHER clause doubled its two Goblins on the way.
    Spec.assertEqWith s "and four Goblins, not two" (length (S.tokensOf after)) 4
  -- #79: resolveDestruction answers with the SETTLED object, not a Bool. The
  -- identity of what the CR 616.1 loop hands back is what Event.destroy must
  -- put into the graveyard; collapsing it to a predicate is what made a
  -- redirecting DestructionRewrite silently unimplementable.
  Spec.it s "CR 701.8 an unreplaced destruction settles on the object itself" $ do
    swamp <- S.printingOf s registry "Swamp"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay swamp 1
        (piker, g1) = S.addPermanent pikerPrinting S.alice base
        (settled, _) = S.runPureWith S.identityAnswer g1 (Event.resolveDestruction Nothing DestructionCause.ByEffect Regenerability.Regenerable piker)
    Spec.assertEqWith s "the object it was asked about" settled (Just piker)
  Spec.it s "CR 701.19a a regenerated destruction settles on nothing" $ do
    swamp <- S.printingOf s registry "Swamp"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay swamp 1
        (piker, g1) = S.addPermanent pikerPrinting S.alice base
        (settled, _) = S.runPureWith S.identityAnswer (S.addRegenShield piker g1) (Event.resolveDestruction Nothing DestructionCause.ByEffect Regenerability.Regenerable piker)
    Spec.assertEqWith s "consumed by the shield" settled Nothing
  galvanicBlastSpec s registry
  voltaicSurgeSpec s registry
  gatherSpecimensSpec s registry
  kismetSpec s registry
  shimatsuSpec s registry
  undergrowthScavengerSpec s registry
  entryBudgetSpec s registry
  warLeechSpec s registry
  faerieSquadronSpec s registry

-- Faerie Squadron {U} Creature -- Faerie 1/1, whole text: "Kicker {3}{U} (You may
-- pay an additional {3}{U} as you cast this spell.) / If this creature was
-- kicked, it enters with two +1/+1 counters on it and with flying." (oracle
-- checked on Scryfall)
--
-- The card whose second clause EntryRewrite.WithKeywords exists for (#2323): CR
-- 614.1c's "enters with" naming a keyword rather than a counter. It writes the
-- one sentence as two rows -- the counters and the keyword -- each on CR 604.2's
-- "if this creature was kicked", which is why both reach CR 616.1e together and
-- the entry loop asks for an order. That order is pawl's, not the rules': one
-- printed sentence is one replacement effect, and CR 616.1 asks nothing here
-- (#3288). `kicks` defers it, so these cases read past it either way.
--
-- THE BOARD: nine Islands, the Squadron in hand and a Rite of Replication beside
-- it. Nine is the two casts added up -- {U} plus the kicker {3}{U} is five, and
-- the Rite unkicked is four -- so the kicked and unkicked cases below differ in
-- the kicker answer and in nothing else.
squadronBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
squadronBoard island squadron rite =
  let (gs1, squadronId) = S.handOne squadron (S.landsInPlay island 9)
      (riteId, gs2) = S.addHandCard rite S.alice gs1
   in (gs2, squadronId, riteId)

-- The battlefield's Faerie Squadrons, by name: CR 400.7 gives the permanent a new
-- id, so the one the cast was handed names nothing here.
squadronsOut :: GameState.GameState -> [ObjectId.ObjectId]
squadronsOut gs = [o | o <- Set.toList (GameState.battlefield gs), Projection.hasName (CardName.MkCardName (Text.pack "Faerie Squadron")) o gs]

-- Cast the Squadron with this kicker answer and settle. `kicks` answers CR
-- 702.33a and defers the rest, so CR 616.1's order between the two entry rows is
-- the canonical one -- which the rule makes immaterial, both rows applying either
-- way (CR 616.1f).
castSquadron :: KickerDecision.KickerDecision -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
castSquadron decision gs squadronId =
  let cast = snd (Engine.runGamePure (kicks decision) gs (S.cast S.alice squadronId))
   in snd (Engine.runGamePure (kicks decision) cast (Stack.resolveTop >> Engine.settleForPriority))

-- Rite of Replication unkicked, aimed at `victim` -- PINNED to that id rather
-- than searched for, so a mutation cannot be repaired by an answerer that finds
-- another legal target.
riteAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
riteAt victim p = case p of
  Prompt.ChooseKicker {} -> KickerDecision.MkKickerDecision 0
  Prompt.ChooseTargets _ _ _ sets -> Map.map (const (Set.singleton (Recipient.ToCreature victim))) sets
  _ -> S.identityAnswer p

faerieSquadronSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
faerieSquadronSpec s registry = Spec.describe s "Faerie Squadron" $ do
  -- CR 702.33d's designation survives the resolution (CR 400.7d), CR 604.2's
  -- clause reads it off the entering permanent, and CR 614.1c's two rewrites place
  -- the counters and grant the keyword.
  Spec.it s "CR 614.1c kicked, the Squadron enters with flying and with two +1/+1 counters" $ do
    island <- S.printingOf s registry "Island"
    squadron <- S.printingOf s registry "Faerie Squadron"
    rite <- S.printingOf s registry "Rite of Replication"
    let (board, squadronId, _) = squadronBoard island squadron rite
        settled = castSquadron (KickerDecision.MkKickerDecision 1) board squadronId
    case squadronsOut settled of
      [permId] -> do
        Spec.assertBool s (Projection.hasKeyword Keyword.Flying permId settled) "CR 614.1c it has flying"
        Spec.assertEqWith s "and the counter half of the same sentence placed two +1/+1 counters" (S.powerToughnessOf permId settled) (Just (3, 3))
      other -> Spec.assertFailure s ("expected one Squadron, got " <> show (length other))
  -- The same board and the same answerer but for the one answer. The Squadron
  -- enters HERE TOO, so what the two cases tell apart is whether the rewrite ran
  -- and not whether the permanent arrived.
  Spec.it s "CR 604.2 unkicked, neither rewrite applies: no flying and a 1/1" $ do
    island <- S.printingOf s registry "Island"
    squadron <- S.printingOf s registry "Faerie Squadron"
    rite <- S.printingOf s registry "Rite of Replication"
    let (board, squadronId, _) = squadronBoard island squadron rite
        settled = castSquadron (KickerDecision.MkKickerDecision 0) board squadronId
    case squadronsOut settled of
      [permId] -> do
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying permId settled)) "CR 604.2 the unkicked Squadron does not have flying"
        Spec.assertEqWith s "and it is the printed 1/1" (S.powerToughnessOf permId settled) (Just (1, 1))
      other -> Spec.assertFailure s ("expected one Squadron, got " <> show (length other))
  -- WHERE THE GRANT LIVES, which the two cases above cannot see: CR 707.2 copies
  -- an "as . . . enters" ability's values only where it SETS POWER AND TOUGHNESS,
  -- and "with flying" sets neither, so the keyword is not a copiable value. A
  -- token copy of the kicked Squadron is a printed 1/1 with no flying -- and an
  -- implementation writing the keyword into the copiable snapshot the way
  -- Pawl.Engine.Replacement.applyEntryOption does for CR 208.2b's options would
  -- hand the token flying instead.
  --
  -- The counters are the control beside it: CR 707.2's last sentence keeps them
  -- off the copy too, so a token that arrived at 3/3 would say the whole snapshot
  -- was written rather than only the keyword.
  Spec.it s "CR 707.2 a token copy of the kicked Squadron has neither the flying nor the counters" $ do
    island <- S.printingOf s registry "Island"
    squadron <- S.printingOf s registry "Faerie Squadron"
    rite <- S.printingOf s registry "Rite of Replication"
    let (board, squadronId, riteId) = squadronBoard island squadron rite
        entered = castSquadron (KickerDecision.MkKickerDecision 1) board squadronId
    case squadronsOut entered of
      [origId] -> do
        let cast = snd (Engine.runGamePure (riteAt origId) entered (S.cast S.alice riteId))
            after = snd (Engine.runGamePure (riteAt origId) cast (Stack.resolveTop >> Engine.settleForPriority))
        case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield entered)) of
          [tokenId] -> do
            Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying tokenId after)) "CR 707.2 the token copy does not have flying"
            Spec.assertEqWith s "it is a Faerie Squadron all the same" (Projection.namesOf tokenId after) (Set.singleton (CardName.MkCardName (Text.pack "Faerie Squadron")))
            Spec.assertEqWith s "at its printed 1/1, the counters not being copied either" (S.powerToughnessOf tokenId after) (Just (1, 1))
            Spec.assertBool s (Projection.hasKeyword Keyword.Flying origId after) "and the original still has the flying it entered with"
          tokens -> Spec.assertFailure s ("expected exactly one token, got " <> show (length tokens))
      other -> Spec.assertFailure s ("expected one Squadron, got " <> show (length other))

-- Monstrous War-Leech {3}{B} Creature -- Leech Horror \*/*, whole text: "Kicker
-- {U}. As this creature enters, if it was kicked, mill four cards. Monstrous
-- War-Leech's power and toughness are each equal to the greatest mana value
-- among cards in your graveyard." (oracle checked on Scryfall)
--
-- CR 614.1c's shape that RUNS AN EFFECT, gated on a condition (see #1416) --
-- EntryRewrite.RunEffects, with "if it was kicked" on CR 604.2's clause.
--
-- THE BOARD, one fixture the cases below take in three states: five lands (four
-- Swamps and an Island, so {3}{B} is payable with or without the kicker {U}), the
-- Leech in hand, SIX Lairwatch Giants in the library, and whatever `buried` names
-- already in the graveyard.
--
-- ONE Lightning Bolt is what the first two pass, and it is what makes both halves
-- observable at once. Without it the unkicked Leech is a 0/0 that CR 704.5f
-- buries, and "no mill" would be told from "mill" by a permanent that is not
-- there -- the confusion that issue's own bar rules out. Lightning Bolt's mana value is
-- 1 and Lairwatch Giant's is 6 (CR 202.3), so the Leech ENTERS AND SURVIVES on
-- both boards and the two are told apart by what it is: a 1/1 unkicked, a 6/6
-- kicked. The third case passes NONE, which is how it sees the ordering the other
-- two cannot.
--
-- Every number distinct: one Bolt, four milled, six in the library before and two
-- after, five lands, and the two power/toughness readings 1 and 6.
warLeechBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> [Printing.Printing] -> (GameState.GameState, ObjectId.ObjectId)
warLeechBoard swamp island leech giant buried =
  let lands = S.landsFor island S.alice 1 (S.landsInPlay swamp 4)
      withBuried = List.foldl' (\g p -> snd (S.addGraveyardCard p S.alice g)) lands buried
      stocked = List.foldl' (\g _ -> snd (S.addLibraryCard giant S.alice g)) withBuried [1 :: Int .. 6]
   in S.handOne leech stocked

-- Answers CR 702.33a's kicker question with `decision` and defers everything else,
-- so the two boards below differ in this one answer and nothing else.
kicks :: KickerDecision.KickerDecision -> Prompt.Prompt r -> r
kicks decision p = case p of
  Prompt.ChooseKicker {} -> decision
  _ -> S.identityAnswer p

-- How many cards are in alice's library, and in her graveyard.
zoneSizes :: GameState.GameState -> (Int, Int)
zoneSizes gs =
  ( Seq.length (Map.findWithDefault Seq.empty S.alice (GameState.library gs)),
    Seq.length (Map.findWithDefault Seq.empty S.alice (GameState.graveyard gs))
  )

-- The battlefield's Monstrous War-Leech, by name: CR 400.7 gives the permanent a
-- new id, so the one the cast was handed names nothing here.
leechOut :: GameState.GameState -> [ObjectId.ObjectId]
leechOut gs = [o | o <- Set.toList (GameState.battlefield gs), Projection.hasName (CardName.MkCardName (Text.pack "Monstrous War-Leech")) o gs]

-- Cast the Leech with this kicker answer and settle: the entry rewrite's effects
-- are queued by Pawl.Engine.Event and drained by performSettle, so the mill has
-- happened by the time the state-based action pass reads the Leech's toughness.
castLeech :: KickerDecision.KickerDecision -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
castLeech decision gs leechId =
  let cast = snd (Engine.runGamePure (kicks decision) gs (S.cast S.alice leechId))
   in snd (Engine.runGamePure (kicks decision) cast (Stack.resolveTop >> Engine.settleForPriority))

warLeechSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
warLeechSpec s registry = Spec.describe s "Monstrous War-Leech" $ do
  -- CR 702.33d's designation survives the resolution (CR 400.7d), the CR 604.2
  -- clause reads it off the entering permanent, and CR 614.1c's rewrite runs the
  -- mill.
  Spec.it s "CR 614.1c kicked, the as-enters mill runs: four cards leave the library and the Leech is a 6/6" $ do
    swamp <- S.printingOf s registry "Swamp"
    island <- S.printingOf s registry "Island"
    leech <- S.printingOf s registry "Monstrous War-Leech"
    giant <- S.printingOf s registry "Lairwatch Giant"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (board, leechId) = warLeechBoard swamp island leech giant [bolt]
        settled = castLeech (KickerDecision.MkKickerDecision 1) board leechId
    Spec.assertEqWith s "four cards were milled: six in the library became two, and the graveyard's one Bolt became five cards" (zoneSizes settled) (2, 5)
    case leechOut settled of
      [permId] -> Spec.assertEqWith s "the greatest mana value among them is Lairwatch Giant's 6" (S.powerToughnessOf permId settled) (Just (6, 6))
      other -> Spec.assertFailure s ("expected one Leech, got " <> show (length other))
  -- The same board and the same answerer but for the one answer. The Leech enters
  -- HERE TOO, so what the two cases tell apart is whether the replacement ran and
  -- not whether the permanent arrived.
  Spec.it s "CR 614.1c unkicked, the rewrite does not apply: nothing is milled and the Leech is a 1/1" $ do
    swamp <- S.printingOf s registry "Swamp"
    island <- S.printingOf s registry "Island"
    leech <- S.printingOf s registry "Monstrous War-Leech"
    giant <- S.printingOf s registry "Lairwatch Giant"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (board, leechId) = warLeechBoard swamp island leech giant [bolt]
        settled = castLeech (KickerDecision.MkKickerDecision 0) board leechId
    Spec.assertEqWith s "the library is untouched and the graveyard still holds only the Bolt" (zoneSizes settled) (6, 1)
    case leechOut settled of
      [permId] -> Spec.assertEqWith s "so the greatest mana value is Lightning Bolt's 1" (S.powerToughnessOf permId settled) (Just (1, 1))
      other -> Spec.assertFailure s ("expected one Leech, got " <> show (length other))
  -- WHEN the effects run, which the two cases above cannot see: the Bolt keeps the
  -- Leech alive whatever the mill does. With an EMPTY graveyard the Leech's CDA
  -- determines nothing and CR 208.2a makes it 0, so the mill is the only thing
  -- between it and CR 704.5f -- and it survives, which says the drain happens
  -- before the state-based action pass rather than after it.
  --
  -- Pawl.PowerToughnessSpec's "CR 704.5f the 0/0 Leech dies" is this board's other
  -- half: no blue mana there, so no kicker is offered, nothing is milled and the
  -- Leech is buried.
  Spec.it s "CR 704.5f kicked with an EMPTY graveyard, the mill beats the SBA pass: the Leech lives as a 6/6" $ do
    swamp <- S.printingOf s registry "Swamp"
    island <- S.printingOf s registry "Island"
    leech <- S.printingOf s registry "Monstrous War-Leech"
    giant <- S.printingOf s registry "Lairwatch Giant"
    let (board, leechId) = warLeechBoard swamp island leech giant []
        settled = castLeech (KickerDecision.MkKickerDecision 1) board leechId
    Spec.assertEqWith s "four cards were milled into an empty graveyard" (zoneSizes settled) (2, 4)
    case leechOut settled of
      [permId] -> Spec.assertEqWith s "and the Leech is a 6/6 rather than a buried 0/0" (S.powerToughnessOf permId settled) (Just (6, 6))
      other -> Spec.assertFailure s ("expected one Leech, got " <> show (length other))

-- alice controls one Mountain plus `artifacts` Darksteel Myr, and holds a
-- Galvanic Blast; `others` are her further permanents, added after the Myr.
-- Returns the state and the Blast's hand id.
--
-- Darksteel Myr because it is an artifact with nothing else going on -- no
-- static ability, no mana ability of its own to be tapped for, and CR 702.12b's
-- indestructibility never comes up because nothing here destroys anything. Three
-- copies of one card, since "three or more artifacts" counts artifacts and not
-- distinct names.
metalcraftBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> [Printing.Printing] -> (GameState.GameState, ObjectId.ObjectId)
metalcraftBoard mountain myr galvanicBlast artifacts others =
  let addAll ps gs = List.foldl' (\g p -> snd (S.addPermanent p S.alice g)) gs ps
      gs1 = addAll (replicate artifacts myr <> others) (S.landsInPlay mountain 1)
      (gs2, spellId) = S.handOne galvanicBlast gs1
   in (gs2, spellId)

-- Aim every target slot at bob himself. CR 115.4's "any target" admits a player,
-- and a life total is the cleanest readout of an amount: 2, 4 and 8 are three
-- distinct answers with no toughness or state-based action in the way.
atBob :: Prompt.Prompt r -> r
atBob p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
  _ -> S.identityAnswer p

-- CR 614.15's self-replacement effects and CR 616.1a's bucket, through the one
-- card in the pool that prints one.
--
-- Galvanic Blast is CR 614.15's own description almost word for word -- "the text
-- creating a self-replacement effect is usually part of the ability whose effect
-- is being replaced, but the text can be a separate ability, particularly when
-- preceded by an ability word." Metalcraft is the ability word -- CR 207.2c lists
-- it by name and says ability words "have no special rules meaning" -- the clause
-- replaces the damage the spell's own first line deals, and the whole thing is
-- one instant.
--
-- The card's two lines resolve as two effects in the ISA, and in the opposite
-- order from the printing: the Replace comes first so the replacement exists
-- before the DealDamage proposes the event it replaces (CR 614.4, "replacement
-- effects must exist before the appropriate event occurs"). Nothing observes the
-- gap: CR 117.3b gives the active player priority only AFTER a spell resolves,
-- and CR 608.2g's last sentence forbids casting or activating anything during one.
-- So the printed reading and this one agree on every board.
galvanicBlastSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
galvanicBlastSpec s registry =
  Spec.describe s "Galvanic Blast (CR 614.15)" $ do
    Spec.it s "CR 614.15 with two artifacts metalcraft is off, so the Blast deals its printed 2" $ do
      mountain <- S.printingOf s registry "Mountain"
      myr <- S.printingOf s registry "Darksteel Myr"
      galvanicBlast <- S.printingOf s registry "Galvanic Blast"
      let (gs, spellId) = metalcraftBoard mountain myr galvanicBlast 2 []
          after = castAndResolve atBob gs spellId
      Spec.assertEqWith s "bob takes 2" (S.lifeOf S.bob after) (Just 18)
      -- CR 614.1: the row IS installed and simply does not apply, its clause
      -- being false when the damage would happen (voltaicSurgeSpec below is
      -- where that separation is observable). Unspent, since it never applied.
      Spec.assertEqWith s "the row is installed but unapplied" (length (GameState.replacements after)) 1
    -- The discriminating twin: one more artifact, everything else identical.
    --
    -- The FOUR-artifact leg is what makes this a Comparison.AtLeast test rather
    -- than an Exactly one -- the card says "three or MORE", and at exactly three
    -- the two comparisons agree.
    Spec.it s "CR 614.15 with three or more artifacts the self-replacement applies: 4 instead of 2" $ do
      mountain <- S.printingOf s registry "Mountain"
      myr <- S.printingOf s registry "Darksteel Myr"
      galvanicBlast <- S.printingOf s registry "Galvanic Blast"
      let (three, threeId) = metalcraftBoard mountain myr galvanicBlast 3 []
          (four, fourId) = metalcraftBoard mountain myr galvanicBlast 4 []
          after = castAndResolve atBob three threeId
      Spec.assertEqWith s "at three, bob takes 4" (S.lifeOf S.bob after) (Just 16)
      Spec.assertEqWith s "at four, still 4 -- not back down to the printed 2" (S.lifeOf S.bob (castAndResolve atBob four fourId)) (Just 16)
      -- CR 614.3's "until they're used up": the row applied, so Uses.Once spent
      -- it. Nothing is left to replace a later damage event this turn.
      Spec.assertEqWith s "and the one-shot was consumed" (GameState.replacements after) []
    Spec.it s "CR 614.15 the metalcraft count is artifacts YOU control, not everyone's" $ do
      mountain <- S.printingOf s registry "Mountain"
      myr <- S.printingOf s registry "Darksteel Myr"
      galvanicBlast <- S.printingOf s registry "Galvanic Blast"
      -- alice has two; bob has three. CR 109.5's "you" is the spell's
      -- controller, so hers is the count that matters and metalcraft is off.
      let (gs0, spellId) = metalcraftBoard mountain myr galvanicBlast 2 []
          gs = List.foldl' (\g _ -> snd (S.addPermanent myr S.bob g)) gs0 [1 :: Int, 2, 3]
          after = castAndResolve atBob gs spellId
      Spec.assertEqWith s "bob takes 2, not 4" (S.lifeOf S.bob after) (Just 18)
    -- CR 616.1a, and the reason the SelfReplacement bucket exists: "if any of
    -- the replacement and/or prevention effects are self-replacement effects
    -- (see rule 614.15), one of them must be chosen."
    --
    -- Furnace of Rath is the other applicable damage replacement, and the two
    -- ORDERS DISAGREE, which is what makes this an assertion rather than a
    -- coincidence:
    --
    --   * CR 616.1a's order -- metalcraft first (2 becomes 4), then the Furnace
    --     doubles it: 8.
    --   * the other order -- the Furnace first (2 becomes 4), then metalcraft
    --     sets it to 4: 4.
    --
    -- Furnace of Rath's own ruling states the general rule this is an instance
    -- of: "if multiple effects modify how damage will be dealt, the player who
    -- would be dealt damage ... chooses the order to apply the effects." CR
    -- 616.1a is the exception that takes that choice away here.
    Spec.it s "CR 616.1a the self-replacement is applied BEFORE Furnace of Rath: 2 -> 4 -> 8" $ do
      mountain <- S.printingOf s registry "Mountain"
      myr <- S.printingOf s registry "Darksteel Myr"
      furnaceOfRath <- S.printingOf s registry "Furnace of Rath"
      galvanicBlast <- S.printingOf s registry "Galvanic Blast"
      let (gs, spellId) = metalcraftBoard mountain myr galvanicBlast 3 [furnaceOfRath]
          after = castAndResolve atBob gs spellId
          asked = answersFor atBob gs (S.cast S.alice spellId >> Stack.resolveTop)
      Spec.assertEqWith s "bob takes 8, not the 4 the other order gives" (S.lifeOf S.bob after) (Just 12)
      -- The second half of CR 616.1a: because the self-replacement is alone in
      -- the highest non-empty bucket, there is nothing to choose and the engine
      -- must not ask. The Hardened-Scales-versus-Corpsejack race above, whose
      -- two candidates share CR 616.1e's bucket, DOES ask -- so this is the
      -- bucket ordering being observed, not prompts being suppressed in general.
      Spec.assertBool s (not (wasAskedToReplace asked)) "no ChooseReplacement was raised"
    -- The control leg for the Furnace: with metalcraft OFF there is no
    -- self-replacement at all, so the Furnace doubles the printed 2. Without
    -- this, an engine that ignored the metalcraft clause entirely and simply
    -- doubled twice would also reach 8.
    Spec.it s "CR 614.1a Furnace of Rath alone doubles the printed 2, not 4" $ do
      mountain <- S.printingOf s registry "Mountain"
      myr <- S.printingOf s registry "Darksteel Myr"
      furnaceOfRath <- S.printingOf s registry "Furnace of Rath"
      galvanicBlast <- S.printingOf s registry "Galvanic Blast"
      -- Two Myr, so the Furnace is the ONLY artifact short of metalcraft's three
      -- -- an enchantment, so it cannot make up the count itself.
      let (gs, spellId) = metalcraftBoard mountain myr galvanicBlast 2 [furnaceOfRath]
          after = castAndResolve atBob gs spellId
      Spec.assertEqWith s "bob takes 4" (S.lifeOf S.bob after) (Just 16)
    -- CR 614.15's "this way": the clause replaces the damage ITS OWN SOURCE is
    -- dealing and nothing else.
    --
    -- The row is seeded rather than cast, and its use count is widened to
    -- Unlimited, because Galvanic Blast cannot show this on any board: the
    -- Blast's own damage event is the first one the row is ever offered, and
    -- Uses.Once spends it there (CR 614.3), so a second event would be untouched
    -- whatever the pattern said. Widening the count is what lets both events
    -- reach the same row, which is what isolates the PATTERN. Everything else --
    -- the funnel, the CR 616.1 loop, the rewrite -- is the real machinery, and
    -- the shape seeded is the one data/cards/galvanic-blast.json carries.
    Spec.it s "CR 614.15 a source-scoped rewrite takes its own source's damage and no one else's" $ do
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      let base = Setup.emptyGame S.bothPlayers
          (mine, g1) = S.addPermanent pikerPrinting S.alice base
          (theirs, g2) = S.addPermanent pikerPrinting S.bob g1
          (victim, g3) = S.addPermanent pikerPrinting S.bob g2
          (ts, g4) = Game.freshTimestamp g3
          armed = S.addReplacement (blastShape mine ts) g4
          hit src = S.runPure S.identityAnswer armed (Damage.applyDamage [DamageEvent.MkDamageEvent src (Recipient.ToCreature victim) 2 False False False 0 Nothing DamageKind.Noncombat])
      Spec.assertEqWith s "its own source's 2 becomes 4" (S.damageOf victim (hit mine)) (Just 4)
      Spec.assertEqWith s "another source's 2 stays 2" (S.damageOf victim (hit theirs)) (Just 2)

-- Synthetic Voltaic Surge {1}{R} Instant: "Until end of turn, if a source you
-- control would deal damage to a permanent or player and you control three or
-- more artifacts, it deals double that damage to that permanent or player
-- instead." A floating (CR 614.3) row with a stated duration whose printed "if"
-- is separated from the resolution that installed it, which is what makes CR
-- 614.1's "they aren't locked in ahead of time" observable: the clause is asked
-- as the damage would happen, not when the row was created.
--
-- SYNTHETIC because the shape has no printing. The clause and the rewrite are
-- both taken from cards that do print them -- Anthem of Rakdos ("if a source you
-- control would deal damage to a permanent or player, it deals double that
-- damage . . . instead") and Galvanic Blast's metalcraft count -- and what no
-- card puts together is that pair with a DURATION. Scryfall, 2026-08-21:
-- `(t:instant or t:sorcery) o:"this turn" o:"instead" o:"if you"`,
-- `o:"this turn" o:"would" o:"instead" o:"as long as"`,
-- `o:"this turn" o:"would" o:"instead" (o:"only while" or o:"only as long as" or
-- o:"only if you")` and `o:"the next time" o:"would" o:"instead" o:"if"` return
-- only two families: one-shot spells whose "if" is settled inside their own
-- resolution (Galvanic Blast, Cackling Flames, Twinstrike, Winds of Qal Sisma)
-- and permanents whose "as long as" rides a static ability (Anthem of Rakdos,
-- Aether Revolt, Jared Carthalion). A printing of either family with a stated
-- duration would refute this and replace the synthetic.
--
-- Bonesplitter is the artifact, NOT Darksteel Myr: the case below destroys one to
-- turn the clause off, and rule 702.12b would refuse. Unattached it modifies
-- nothing, so the board reads only its count.
--
-- Firebolt deals 2, so bob's life tells the two readings apart at every step: 20,
-- then 18 (printed) or 16 (doubled), then 14 either way is the coincidence this
-- avoids by asserting the intermediate.
surgeBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId], [ObjectId.ObjectId])
surgeBoard mountain splitter surge firebolt artifacts =
  let base = S.landsFor mountain S.alice 5 (Setup.emptyGame S.bothPlayers)
      addArtifact (ids, g) _ = let (oid, g') = S.addPermanent splitter S.alice g in (ids <> [oid], g')
      (splitters, g1) = List.foldl' addArtifact ([], base) [1 .. artifacts]
      (surgeId, g2) = S.addHandCard surge S.alice g1
      addBolt (ids, g) _ = let (oid, g') = S.addHandCard firebolt S.alice g in (ids <> [oid], g')
      (bolts, g3) = List.foldl' addBolt ([], g2) [1 :: Int, 2]
   in ( g3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        surgeId,
        bolts,
        splitters
      )

voltaicSurgeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
voltaicSurgeSpec s registry =
  Spec.describe s "Synthetic Voltaic Surge (CR 614.1)" $ do
    let board artifacts = do
          mountain <- S.printingOf s registry "Mountain"
          splitter <- S.printingOf s registry "Bonesplitter"
          surge <- S.printingOf s registry "Synthetic Voltaic Surge"
          firebolt <- S.printingOf s registry "Firebolt"
          pure (splitter, surgeBoard mountain splitter surge firebolt artifacts)
    -- THE PROVING TEST. The clause is true when the row is installed and false
    -- when the second Firebolt would be doubled, and NOTHING removed the row: a
    -- gate read at installation doubles both, and a row that was swept would
    -- leave GameState.replacements empty.
    Spec.it s "CR 614.1 the clause is re-asked, so losing an artifact turns the row off without removing it" $ do
      (_, (gs, surgeId, bolts, splitters)) <- board 3
      case (bolts, splitters) of
        ([first_, second], doomed : _) -> do
          let armed = castAndResolve atBob gs surgeId
              doubled = castAndResolve atBob armed first_
              shrunk = S.runPure S.identityAnswer doubled (Event.destroy Regenerability.Regenerable [doomed])
              after = castAndResolve atBob shrunk second
          Spec.assertEqWith s "the first Firebolt is doubled while alice controls three artifacts" (S.lifeOf S.bob doubled) (Just 16)
          Spec.assertEqWith s "the second lands at its printed 2 once one artifact is gone" (S.lifeOf S.bob after) (Just 14)
          -- By NAME rather than by a controls-count: alice owns every artifact
          -- on this board, so S.countOnBattlefieldByName's owner index answers
          -- the control question too (see Pawl.Support).
          Spec.assertEqWith s "setup: alice is down to two artifacts" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Bonesplitter")) S.alice shrunk) 2
          Spec.assertEqWith s "and the row is still installed -- it stopped applying, it was not removed" (length (GameState.replacements after)) 1
        _ -> Spec.assertFailure s "fixture should hold two Firebolts and three artifacts"
    -- The other direction, and the one a gate read at installation cannot reach
    -- at all: the clause is FALSE as the spell resolves, so the old reading
    -- installed nothing and no later board could turn it on.
    Spec.it s "CR 614.1 a row installed while its clause was false applies once the clause turns true" $ do
      (splitter, (gs, surgeId, bolts, _)) <- board 2
      case bolts of
        [first_, second] -> do
          let armed = castAndResolve atBob gs surgeId
              printed = castAndResolve atBob armed first_
              grown = snd (S.addPermanent splitter S.alice printed)
              after = castAndResolve atBob grown second
          Spec.assertEqWith s "the first Firebolt lands at its printed 2" (S.lifeOf S.bob printed) (Just 18)
          Spec.assertEqWith s "the third artifact turns the clause on, so the second is doubled" (S.lifeOf S.bob after) (Just 14)
          Spec.assertEqWith s "setup: the row was installed though its clause was false" (length (GameState.replacements armed)) 1
        _ -> Spec.assertFailure s "fixture should hold two Firebolts"

-- How many battlefield permanents `pid` CONTROLS are printed with this name. NOT
-- S.countOnBattlefieldByName, which counts by OWNER (Game.zoneMembers filters the
-- shared battlefield by Object.owner) -- the whole point of a CR 616.1b rewrite
-- is that the owner and the controller have come apart.
controlledNamed :: CardName.CardName -> PlayerId.PlayerId -> GameState.GameState -> Int
controlledNamed wanted pid gs =
  length (filter (\oid -> fmap Face.name (Game.faceOf oid gs) == Just wanted) (Projection.controls pid gs))

-- alice controls six untapped Islands (Gather Specimens is {3}{U}{U}{U}) and one
-- Goblin Piker for a Clone to copy; bob controls ten, enough for a Gather
-- Specimens of his own plus a Clone at {3}{U}, or for two Clones, with no untap
-- step in between. alice holds one Gather Specimens, bob holds one of each
-- printing in `bobsHand`. It is alice's precombat main phase, and she has
-- priority. Returns the state, alice's spell id, bob's hand ids in order, and
-- the Piker.
specimenBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> [Printing.Printing] -> (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId], ObjectId.ObjectId)
specimenBoard island pikerPrinting gatherSpecimens bobsHand =
  let addLands pid n g = List.foldl' (\acc _ -> snd (S.addPermanent island pid acc)) g [1 .. n :: Int]
      base = addLands S.bob 10 (addLands S.alice 6 (Setup.emptyGame S.bothPlayers))
      (piker, g1) = S.addPermanent pikerPrinting S.alice base
      (gatherId, g2) = S.addHandCard gatherSpecimens S.alice g1
      addOne (ids, g) p = let (oid, g3) = S.addHandCard p S.bob g in (ids <> [oid], g3)
      (bobs, g4) = List.foldl' addOne ([], g2) bobsHand
   in ( g4
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        gatherId,
        bobs,
        piker
      )

-- CR 800.1: specimenBoard's three-seat twin, and the smallest board on which two
-- control-on-entry replacements can race for one creature. alice and bob each
-- control six untapped Islands (Gather Specimens is {3}{U}{U}{U}) and hold one
-- Gather Specimens; carol controls two and holds one card of `creature`. It is
-- alice's precombat main phase with priority. Returns the state, alice's spell
-- id, bob's, and carol's card.
threeSeatSpecimenBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
threeSeatSpecimenBoard island gatherSpecimens creature =
  let addLands pid n g = List.foldl' (\acc _ -> snd (S.addPermanent island pid acc)) g [1 .. n :: Int]
      base = addLands S.carol 2 (addLands S.bob 6 (addLands S.alice 6 (Setup.emptyGame S.threePlayers)))
      (aliceGather, g1) = S.addHandCard gatherSpecimens S.alice base
      (bobGather, g2) = S.addHandCard gatherSpecimens S.bob g1
      (carols, g3) = S.addHandCard creature S.carol g2
   in ( g3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        aliceGather,
        bobGather,
        carols
      )

-- The SOURCE of the floating row `who` installed, so an answer can name a CR
-- 616.1 candidate by whose row it is rather than by list position -- the same
-- reason raceAnswer takes an ObjectId.
rowSourceOf :: PlayerId.PlayerId -> GameState.GameState -> Maybe ObjectId.ObjectId
rowSourceOf who gs =
  Maybe.listToMaybe
    [ ActiveReplacement.source active
    | active <- GameState.replacements gs,
      ActiveReplacement.controller active == who
    ]

-- Name the candidate whose source is `preferred`, but only when CR 616.1's race
-- is put to `who`; every other prompt takes the default. The readout for WHICH
-- player the choice was handed to: an engine that asked anyone else would take
-- the canonical first for both preferences, and the two runs would converge on
-- one board instead of disagreeing.
replaceIfAskedOf :: PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
replaceIfAskedOf who preferred p = case p of
  Prompt.ChooseReplacement _ asked entries
    | asked == who ->
        maybe 0 Int.toNaturalSaturating (List.findIndex ((== preferred) . ReplacementEntry.source) entries)
  _ -> S.identityAnswer p

-- Copy `wanted` if and only if the copy choice is offered to `who`, and decline
-- otherwise. The readout for WHICH player held Clone's own CR 109.5 "you" when
-- the copy choice was made: the copy lands (a 2/1) only when the engine asked
-- the named player, and the Clone stays a 0/0 when it asked anyone else.
copyIfAskedOf :: PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
copyIfAskedOf who wanted p = case p of
  Prompt.ChooseCopyTarget _ asked _ legal ->
    if asked == who && List.elem wanted legal then Just wanted else Nothing
  _ -> S.identityAnswer p

-- CR 616.1b's bucket, through the one card in the pool that produces one.
--
-- Gather Specimens ({3}{U}{U}{U} instant, Shards of Alara): "If a creature would
-- enter the battlefield under an opponent's control this turn, it enters under
-- your control instead." It is CR 614.1d's other-objects form ("[Objects] enter
-- [the battlefield] . . ."), which is why EntryR carries a Filter at all, and its
-- whole content is modifying under whose control an object enters -- CR 616.1b's
-- description word for word.
--
-- Clone is the competing entry replacement. CR 616.1c's copy bucket sits one step
-- BELOW 616.1b's, so on the entering Clone's first iteration the control rewrite
-- is the only candidate in the highest non-empty bucket -- and the copy choice
-- that follows goes to whoever controls the object THEN, which the control
-- rewrite has just changed. CR 109.5 is the rule for that second question:
-- Clone's "YOU may have this enter as a copy" is its own controller's choice,
-- made at CR 614.12a's moment (before the permanent enters). CR 616.1's chooser
-- is a different question -- WHICH replacement to apply -- and this board never
-- raises it, since each bucket holds one candidate.
gatherSpecimensSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gatherSpecimensSpec s registry =
  Spec.describe s "Gather Specimens (CR 616.1b)" $ do
    Spec.it s "CR 616.1b an opponent's entering creature enters under YOUR control instead" $ do
      island <- S.printingOf s registry "Island"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      gatherSpecimens <- S.printingOf s registry "Gather Specimens"
      clonePrinting <- S.printingOf s registry "Clone"
      let (gs, gatherId, bobs, piker) = specimenBoard island pikerPrinting gatherSpecimens [clonePrinting]
      case bobs of
        cloneId : _ ->
          let armed = S.runPure S.identityAnswer gs (S.cast S.alice gatherId >> Stack.resolveTop)
              after = S.runPure (copyIfAskedOf S.alice piker) armed (S.cast S.bob cloneId >> Stack.resolveTop)
              -- The DISCRIMINATING TWIN: the same cast on the same board with no
              -- Gather Specimens resolved first.
              alone = S.runPure (copyIfAskedOf S.alice piker) gs (S.cast S.bob cloneId >> Stack.resolveTop)
           in case (newestNamed (CardName.MkCardName $ Text.pack "Clone") after, newestNamed (CardName.MkCardName $ Text.pack "Clone") alone) of
                (Just taken, Just untaken) -> do
                  Spec.assertEqWith s "bob's Clone entered under alice's control" (Projection.controllerOf taken after) (Just S.alice)
                  Spec.assertEqWith s "without the Gather Specimens it is bob's" (Projection.controllerOf untaken alone) (Just S.bob)
                _ -> Spec.assertFailure s "a Clone did not reach the battlefield"
        _ -> Spec.assertFailure s "fixture did not deal bob a card"
    -- CR 616.1b BEFORE CR 616.1c, and the two orders disagree about WHO IS ASKED
    -- -- which is what makes this an assertion rather than a coincidence:
    --
    --   * CR 616.1b's order -- the control rewrite first, so the object is
    --     alice's by the time Clone's own choice is offered, and ALICE picks
    --     the copy target.
    --   * the other order -- the copy first, while the object is still bob's,
    --     so BOB picks.
    --
    -- CR 109.5 makes Clone's "you" the entering object's CONTROLLER, and CR
    -- 614.12a fixes when that is read (before the permanent enters). For an
    -- opponent's entering creature that controller is not the Gather Specimens
    -- controller until CR 616.1b's rewrite has been applied -- which is the whole
    -- point: the bucket ordering decides who the second question goes to.
    Spec.it s "CR 616.1b before CR 616.1c: the NEW controller chooses the copy" $ do
      island <- S.printingOf s registry "Island"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      gatherSpecimens <- S.printingOf s registry "Gather Specimens"
      clonePrinting <- S.printingOf s registry "Clone"
      let (gs, gatherId, bobs, piker) = specimenBoard island pikerPrinting gatherSpecimens [clonePrinting]
      case bobs of
        cloneId : _ ->
          let armed = S.runPure S.identityAnswer gs (S.cast S.alice gatherId >> Stack.resolveTop)
              askedAlice = S.runPure (copyIfAskedOf S.alice piker) armed (S.cast S.bob cloneId >> Stack.resolveTop)
              askedBob = S.runPure (copyIfAskedOf S.bob piker) armed (S.cast S.bob cloneId >> Stack.resolveTop)
              asked = answersFor (copyIfAskedOf S.alice piker) armed (S.cast S.bob cloneId >> Stack.resolveTop)
           in case (newestNamed (CardName.MkCardName $ Text.pack "Clone") askedAlice, newestNamed (CardName.MkCardName $ Text.pack "Clone") askedBob) of
                (Just toAlice, Just toBob) -> do
                  Spec.assertEqWith s "alice was offered the copy, and took it" (Projection.powerOf toAlice askedAlice) (Just 2)
                  Spec.assertEqWith s "bob was never offered it, so the Clone is still a 0/0" (Projection.powerOf toBob askedBob) (Just 0)
                  -- CR 616.1b's "one of them must be chosen" with one member:
                  -- the control rewrite is alone in the highest non-empty
                  -- bucket, so there is nothing to choose and the engine must
                  -- not ask. Were both candidates in CR 616.1e's bucket, this
                  -- is the race that would be prompted.
                  Spec.assertBool s (not (wasAskedToReplace asked)) "no ChooseReplacement was raised"
                _ -> Spec.assertFailure s "a Clone did not reach the battlefield"
        _ -> Spec.assertFailure s "fixture did not deal bob a card"
    -- CR 614.1d's filter is the card's own "a creature", and this is the leg that
    -- holds it to that word. Its other half -- "under an OPPONENT's control" --
    -- is held by the duelling-Gather-Specimens leg below, which needs a second
    -- copy of the card to see it at all: with one on the board the relation is
    -- invisible, because rewriting alice's own entering creature to alice's
    -- control is a no-op and adding seats adds no discrimination.
    Spec.it s "CR 614.1d an opponent's entering NONcreature is not a specimen" $ do
      island <- S.printingOf s registry "Island"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      gatherSpecimens <- S.printingOf s registry "Gather Specimens"
      coating <- S.printingOf s registry "Liquimetal Coating"
      let (gs, gatherId, bobs, _) = specimenBoard island pikerPrinting gatherSpecimens [coating]
      case bobs of
        coatingId : _ ->
          let armed = S.runPure S.identityAnswer gs (S.cast S.alice gatherId >> Stack.resolveTop)
              after = S.runPure S.identityAnswer armed (S.cast S.bob coatingId >> Stack.resolveTop)
           in case newestNamed (CardName.MkCardName $ Text.pack "Liquimetal Coating") after of
                Nothing -> Spec.assertFailure s "the Coating did not reach the battlefield"
                Just coatingObj -> Spec.assertEqWith s "an artifact is not a creature" (Projection.controllerOf coatingObj after) (Just S.bob)
        _ -> Spec.assertFailure s "fixture did not deal bob a card"
    -- CR 614.3's "until they're used up": Gather Specimens states no count, so
    -- its row is Uses.Unlimited and every creature an opponent plays for the rest
    -- of the turn comes over. A Uses.Once row would take the first and leave the
    -- second, which is the difference this pins.
    Spec.it s "CR 614.3 the effect lasts the turn: bob's SECOND creature comes over too" $ do
      island <- S.printingOf s registry "Island"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      gatherSpecimens <- S.printingOf s registry "Gather Specimens"
      clonePrinting <- S.printingOf s registry "Clone"
      let (gs, gatherId, bobs, piker) = specimenBoard island pikerPrinting gatherSpecimens [clonePrinting, clonePrinting]
      case bobs of
        firstClone : secondClone : _ ->
          let armed = S.runPure S.identityAnswer gs (S.cast S.alice gatherId >> Stack.resolveTop)
              one = S.runPure (copyIfAskedOf S.alice piker) armed (S.cast S.bob firstClone >> Stack.resolveTop)
              two = S.runPure (copyIfAskedOf S.alice piker) one (S.cast S.bob secondClone >> Stack.resolveTop)
           in Spec.assertEqWith s "both of bob's Clones are alice's" (controlledNamed (CardName.MkCardName $ Text.pack "Clone") S.alice two) 2
        _ -> Spec.assertFailure s "fixture did not deal bob two cards"
    -- DUELLING GATHER SPECIMENS, and the only board where the filter's
    -- "under an OPPONENT's control" is observable at all.
    --
    -- alice resolves one, bob resolves one, and then BOB's Clone enters. The
    -- entering side is what makes this discriminate; alice's own creature does
    -- not, for the reason the two orders below converge on it.
    --
    -- With the relation, CR 616.1f drives a forced two-step -- "this process is
    -- repeated (taking into account only replacement or prevention effects that
    -- would now be applicable) until there are no more left to apply":
    --
    --   1. bob's creature would enter under bob's control. alice's row applies
    --      (bob is her opponent); bob's does not (bob is not his own opponent).
    --      One candidate in CR 616.1b's bucket, so nothing is chosen and nothing
    --      is asked. It enters under alice's control.
    --   2. Re-collected, bob's row is NOW applicable -- alice is his opponent --
    --      and alice's is spent by CR 614.5's "only one opportunity". One
    --      candidate again. It enters under BOB's control.
    --   3. Nothing is left in that bucket, so CR 616.1c's copy choice follows,
    --      offered to bob.
    --
    -- Without the relation both rows are applicable at step 1, and CR 616.1b's
    -- "one of them must be chosen" would have something to choose between: they
    -- are equal in `effect` (one card, one filter) but differ in the baked CR
    -- 109.5 controller, which Replacement.readsApplier makes a distinguishing
    -- field for this rewrite. So bob would be ASKED, take the canonical first --
    -- his own, the newest floating row -- as a no-op, and alice's would apply at
    -- step 2, leaving the creature HERS. The assertion is therefore
    -- bob-not-alice, and the prompt assertion below discriminates too: with the
    -- relation each step has one candidate and nothing is asked.
    Spec.it s "CR 614.1d/616.1f duelling Gather Specimens: alice takes it, then bob takes it back" $ do
      island <- S.printingOf s registry "Island"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      gatherSpecimens <- S.printingOf s registry "Gather Specimens"
      clonePrinting <- S.printingOf s registry "Clone"
      let (gs, aliceGather, bobs, piker) = specimenBoard island pikerPrinting gatherSpecimens [gatherSpecimens, clonePrinting]
      case bobs of
        bobGather : cloneId : _ ->
          let armed = S.runPure S.identityAnswer gs (S.cast S.alice aliceGather >> Stack.resolveTop)
              duelling = S.runPure S.identityAnswer armed (S.cast S.bob bobGather >> Stack.resolveTop)
              after = S.runPure (copyIfAskedOf S.bob piker) duelling (S.cast S.bob cloneId >> Stack.resolveTop)
              asked = answersFor (copyIfAskedOf S.bob piker) duelling (S.cast S.bob cloneId >> Stack.resolveTop)
           in case newestNamed (CardName.MkCardName $ Text.pack "Clone") after of
                Nothing -> Spec.assertFailure s "the Clone did not reach the battlefield"
                Just clone -> do
                  -- Both rows really are on the board: without bob's, this is
                  -- the first case in this group and the answer is alice.
                  Spec.assertEqWith s "two floating replacements are live" (length (GameState.replacements duelling)) 2
                  Spec.assertEqWith s "alice took it, and bob took it back" (Projection.controllerOf clone after) (Just S.bob)
                  -- And the copy choice landed on bob, the controller CR 616.1b
                  -- left the object with.
                  Spec.assertEqWith s "bob was offered the copy, and took it" (Projection.powerOf clone after) (Just 2)
                  -- Each step of CR 616.1f had ONE applicable control rewrite,
                  -- so there was never anything for CR 616.1b's "one of them
                  -- must be chosen" to choose between.
                  Spec.assertBool s (not (wasAskedToReplace asked)) "no ChooseReplacement was raised"
        _ -> Spec.assertFailure s "fixture did not deal bob two cards"
    -- THREE SEATS, where CR 616.1b's "one of them must be chosen" finally has
    -- something to choose between -- the case the duelling leg above cannot
    -- reach. alice and bob each resolve a Gather Specimens and CAROL casts a
    -- creature: it would enter under carol's control, carol is an opponent of
    -- both, so BOTH rows are applicable in the SAME iteration of CR 616.1f. Two
    -- seats cannot produce that (a permanent has one controller, so at most one
    -- such row can see it as an opponent's), which is why the leg above sees a
    -- forced order instead.
    --
    -- The two rows are equal in `effect` -- one card, one filter -- and differ
    -- only in the CR 109.5 controller each baked, which rides the CANDIDATE.
    -- That is precisely what Replacement.readsApplier answers True for, so the
    -- pair is not indistinguishable and CR 616.1 owes the question to the
    -- affected object's controller: carol.
    --
    -- Her answer decides the board, and INVERTS it. The row she names applies
    -- now; CR 616.1f then re-collects, where the other row is newly applicable
    -- (from its controller's side the creature is an opponent's again) and the
    -- named one is spent by CR 614.5. So the creature settles with the player
    -- she did NOT name, and the two runs disagree -- which is the whole point:
    -- before this was fixed both rows were elided as value-equal and the
    -- floating store's newest-first order decided the board with nobody asked.
    Spec.it s "CR 616.1b three seats: carol is asked WHICH Gather Specimens takes her creature" $ do
      island <- S.printingOf s registry "Island"
      gatherSpecimens <- S.printingOf s registry "Gather Specimens"
      narcomoeba <- S.printingOf s registry "Narcomoeba"
      let (gs, aliceGather, bobGather, moeba) = threeSeatSpecimenBoard island gatherSpecimens narcomoeba
          resolveFor pid oid g = S.runPure S.identityAnswer g (S.cast pid oid >> Stack.resolveTop)
          armed = resolveFor S.bob bobGather (resolveFor S.alice aliceGather gs)
          entry = S.cast S.carol moeba >> Stack.resolveTop
          asked = answersFor S.identityAnswer armed entry
          moebaName = CardName.MkCardName $ Text.pack "Narcomoeba"
      Spec.assertEqWith s "two floating replacements are live" (length (GameState.replacements armed)) 2
      Spec.assertBool s (wasAskedToReplace asked) "a ChooseReplacement was raised"
      case (rowSourceOf S.alice armed, rowSourceOf S.bob armed) of
        (Just aliceRow, Just bobRow) ->
          let namedAlice = S.runPure (replaceIfAskedOf S.carol aliceRow) armed entry
              namedBob = S.runPure (replaceIfAskedOf S.carol bobRow) armed entry
           in case (newestNamed moebaName namedAlice, newestNamed moebaName namedBob) of
                (Just afterAlice, Just afterBob) -> do
                  Spec.assertEqWith s "carol named alice's row, so bob's applies second and keeps it" (Projection.controllerOf afterAlice namedAlice) (Just S.bob)
                  Spec.assertEqWith s "carol named bob's row, so alice's applies second and keeps it" (Projection.controllerOf afterBob namedBob) (Just S.alice)
                _ -> Spec.assertFailure s "the creature did not reach the battlefield"
        _ -> Spec.assertFailure s "both Gather Specimens rows should be floating"
    -- CR 800.4a's SECOND clause, at three seats: alice resolves a Gather
    -- Specimens and then concedes. A floating control-on-entry row is an effect
    -- whose whole content is giving its controller control of objects, so it is
    -- one of the "effects which give that player control of any objects" that
    -- end when she leaves -- and carol's creature, entering afterwards, stays
    -- carol's.
    --
    -- Three seats are required twice over: Departure.continuesAfterDeparture is
    -- `> 2`, so at two seats CR 104.2a ends the game and none of CR 800.4a runs,
    -- and a creature entering under bob's control is not "an opponent's" from
    -- bob's own side.
    Spec.it s "CR 800.4a a control-on-entry row ends when its controller leaves the game" $ do
      island <- S.printingOf s registry "Island"
      gatherSpecimens <- S.printingOf s registry "Gather Specimens"
      narcomoeba <- S.printingOf s registry "Narcomoeba"
      let (gs, aliceGather, _, moeba) = threeSeatSpecimenBoard island gatherSpecimens narcomoeba
          armed = S.runPure S.identityAnswer gs (S.cast S.alice aliceGather >> Stack.resolveTop)
          -- The one difference between the two runs.
          gone = S.runPure S.identityAnswer armed (Departure.leaveGame Departure.Type.Conceded S.alice)
          entry = S.cast S.carol moeba >> Stack.resolveTop
          after = S.runPure S.identityAnswer gone entry
          stayed = S.runPure S.identityAnswer armed entry
          moebaName = CardName.MkCardName $ Text.pack "Narcomoeba"
      -- Both read boards taken BEFORE the departure filter runs, so neither can
      -- absorb a mutation of it.
      Spec.assertEqWith s "alice's row was floating before she left" (length (GameState.replacements armed)) 1
      Spec.assertBool s (List.notElem S.alice (Game.stillPlaying gone)) "alice really has left"
      case (newestNamed moebaName after, newestNamed moebaName stayed) of
        (Just departed, Just present) -> do
          Spec.assertEqWith s "carol's creature stays carol's (CR 800.4a)" (Projection.controllerOf departed after) (Just S.carol)
          -- The discriminating twin, on the identical board with alice seated:
          -- the fix ended the row, it did not disable the rewrite.
          Spec.assertEqWith s "with alice seated the same creature is hers (CR 616.1b)" (Projection.controllerOf present stayed) (Just S.alice)
          -- CR 110.2a's entry controller, written by the effect that put the
          -- permanent there and left alone by an entry loop with no candidate:
          -- carol, and so not the departed player CR 800.4c draws its line at.
          Spec.assertEqWith s "the recorded entry controller is carol, not alice (CR 110.2a)" (fmap Object.enteredUnder (Game.lookupObject departed after)) (Just (Just S.carol))
          Spec.assertEqWith s "the row itself is gone" (length (GameState.replacements gone)) 0
        _ -> Spec.assertFailure s "the creature did not reach the battlefield"
    -- WHY CR 800.4a ends the row rather than Event's UnderSourceControl arm
    -- refusing to name a departed player. A guard inside that arm would leave
    -- alice's row a CR 616.1 candidate, and Replacement.readsApplier answers True
    -- for that rewrite, so the pair stays distinguishable and carol is asked
    -- which row takes her creature -- a choice one of whose options does nothing.
    -- With the row ended there is one candidate, and CR 616.1b's one-candidate
    -- elision means no prompt at all. This is the board the two fixes disagree on.
    Spec.it s "CR 616.1b a departed player's row is not a candidate, so carol is not asked" $ do
      island <- S.printingOf s registry "Island"
      gatherSpecimens <- S.printingOf s registry "Gather Specimens"
      narcomoeba <- S.printingOf s registry "Narcomoeba"
      let (gs, aliceGather, bobGather, moeba) = threeSeatSpecimenBoard island gatherSpecimens narcomoeba
          resolveFor pid oid g = S.runPure S.identityAnswer g (S.cast pid oid >> Stack.resolveTop)
          armed = resolveFor S.bob bobGather (resolveFor S.alice aliceGather gs)
          gone = S.runPure S.identityAnswer armed (Departure.leaveGame Departure.Type.Conceded S.alice)
          entry = S.cast S.carol moeba >> Stack.resolveTop
          asked = answersFor S.identityAnswer gone entry
          after = S.runPure S.identityAnswer gone entry
          moebaName = CardName.MkCardName $ Text.pack "Narcomoeba"
      Spec.assertEqWith s "both rows were floating before alice left" (length (GameState.replacements armed)) 2
      case newestNamed moebaName after of
        Just moebaObj -> do
          Spec.assertEqWith s "bob's row is the only one left, so the creature is bob's" (Projection.controllerOf moebaObj after) (Just S.bob)
          Spec.assertBool s (not (wasAskedToReplace asked)) "no ChooseReplacement was raised"
          Spec.assertEqWith s "only bob's row survived alice's departure" (length (GameState.replacements gone)) 1
        Nothing -> Spec.assertFailure s "the creature did not reach the battlefield"

-- Kismet ({3}{W} Enchantment, "Artifacts, creatures, and lands your opponents
-- control enter tapped") -- CR 614.1d's other-objects form, bucketing to CR
-- 616.1e. bob controls it, so alice's entering permanent is the opponent's one
-- it rewrites.
--
-- alice controls a Goblin Piker to copy and six of `land` to pay with, and holds
-- one card of `spell`. It is her precombat main phase with priority. Returns the
-- state, the Piker and the held card.
kismetBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
kismetBoard land pikerPrinting kismet spell =
  let addLands pid n g = List.foldl' (\acc _ -> snd (S.addPermanent land pid acc)) g [1 .. n :: Int]
      base = addLands S.alice 6 (Setup.emptyGame S.bothPlayers)
      (pikerId, g1) = S.addPermanent pikerPrinting S.alice base
      (_, g2) = S.addPermanent kismet S.bob g1
      (spellId, g3) = S.addHandCard spell S.alice g2
   in ( g3
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        pikerId,
        spellId
      )

-- CR 616.1c's bucket, through the first pair in the pool that races it against
-- CR 616.1e: an entering Clone (AsCopy, CR 616.1c) under an opponent's Kismet
-- (Tapped, CR 616.1e). Both rows are applicable to the same entering permanent on
-- the same iteration of CR 616.1f's loop, and the copy is alone in the highest
-- non-empty bucket -- so there is nothing to choose and the engine must not ask.
--
-- The two orders CONVERGE on one board: CR 616.1f re-collects, so the Clone ends
-- up both a copy and tapped whichever is applied first (Kismet's row is not on
-- the copied Piker, so unlike CR 616.1f's Essence of the Wild example the copy
-- does not take the tap clause away). The absence of the prompt is therefore the
-- only observable the split has, which is why it is what the first case asserts;
-- the second case is the discriminating twin that shows the recorder can see one.
kismetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
kismetSpec s registry =
  Spec.describe s "Kismet (CR 616.1c/616.1d)" $ do
    -- THE PROVING CASE. Collapse CopyOnEntry into Other and the same board raises
    -- a CR 616.1e race the rules do not have.
    --
    -- The two board assertions beside it are the non-vacuity check, not the
    -- proof: they show Kismet really was a second candidate, so "no prompt" is
    -- not "no second effect".
    Spec.it s "CR 616.1c the copy bucket outranks Kismet's, so no order is asked" $ do
      island <- S.printingOf s registry "Island"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      kismet <- S.printingOf s registry "Kismet"
      clonePrinting <- S.printingOf s registry "Clone"
      let (gs, pikerId, cloneId) = kismetBoard island pikerPrinting kismet clonePrinting
          cast = S.cast S.alice cloneId >> Stack.resolveTop
          after = S.runPure (copyIfAskedOf S.alice pikerId) gs cast
          asked = answersFor (copyIfAskedOf S.alice pikerId) gs cast
      case newestNamed (CardName.MkCardName $ Text.pack "Clone") after of
        Nothing -> Spec.assertFailure s "the Clone did not reach the battlefield"
        Just cloneOid -> do
          Spec.assertEqWith s "CR 616.1c the Clone copied the Piker" (Projection.powerOf cloneOid after) (Just 2)
          Spec.assertBool s (Game.isTapped cloneOid after) "CR 614.1d and Kismet tapped it too"
          Spec.assertBool s (not (wasAskedToReplace asked)) "no ChooseReplacement was raised"
    -- The DISCRIMINATING TWIN: the same fixture and the same recorder, a spell
    -- whose own entry rewrites are both CR 616.1e's. Coldsteel Heart is an
    -- artifact, so Kismet's row joins its two in one bucket and the race really
    -- is raised. Without this, "no prompt" above would pass under a recorder that
    -- never sees a ChooseReplacement on any board.
    Spec.it s "CR 616.1e rewrites sharing one bucket ARE raced" $ do
      island <- S.printingOf s registry "Island"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      kismet <- S.printingOf s registry "Kismet"
      coldsteel <- S.printingOf s registry "Coldsteel Heart"
      let (gs, _, heartId) = kismetBoard island pikerPrinting kismet coldsteel
          asked = answersFor S.identityAnswer gs (S.cast S.alice heartId >> Stack.resolveTop)
      Spec.assertBool s (wasAskedToReplace asked) "a ChooseReplacement was raised"
    -- The sibling bucket, one step DOWN: CR 616.1d's back-face-up rewrite against
    -- the same CR 616.1e row. CR 702.145b's daybound mints EntersTransformed, and
    -- at night it and Kismet's row are both applicable to the entering werewolf in
    -- one iteration -- so CR 616.1d's bucket is alone at the top and, again, there
    -- is nothing to ask. Same convergence as the copy case: the werewolf ends up
    -- transformed AND tapped either way, so the prompt is the observable.
    --
    -- Forests, not Islands: Infestation Expert is {4}{G}, and its faces are 3/4
    -- and 4/5. The power reading 4 says the werewolf is back face up; it does NOT
    -- say the entry rewrite is what put it there, since CR 702.145c would
    -- transform it a moment later anyway. Like the tap, it is a non-vacuity
    -- guard. The prompt is the assertion.
    Spec.it s "CR 616.1d the back-face bucket outranks Kismet's, so no order is asked" $ do
      forest <- S.printingOf s registry "Forest"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      kismet <- S.printingOf s registry "Kismet"
      werewolf <- S.printingOf s registry "Infestation Expert"
      let (day, _, wolfId) = kismetBoard forest pikerPrinting kismet werewolf
          gs = day {GameState.daytime = Just Daytime.Night}
          cast = S.cast S.alice wolfId >> Stack.resolveTop
          after = S.runPure S.identityAnswer gs cast
          asked = answersFor S.identityAnswer gs cast
      -- Named by the BACK face, which newestNamed reads off the current face: an
      -- Infestation Expert that stayed front face up is not found at all.
      case newestNamed (CardName.MkCardName $ Text.pack "Infested Werewolf") after of
        Nothing -> Spec.assertFailure s "the werewolf did not reach the battlefield transformed"
        Just wolfOid -> do
          Spec.assertEqWith s "CR 702.145b it entered on its back face, a 4/5" (Projection.powerOf wolfOid after) (Just 4)
          Spec.assertBool s (Game.isTapped wolfOid after) "CR 614.1d and Kismet tapped it too"
          Spec.assertBool s (not (wasAskedToReplace asked)) "no ChooseReplacement was raised"

-- Galvanic Blast's metalcraft clause as a floating row: the damage THIS source is
-- dealing, whatever its kind, becomes 4 (CR 614.15 / 614.1a). Uses.Unlimited
-- rather than the card's Once, for the reason its one caller gives.
blastShape :: ObjectId.ObjectId -> Timestamp.Timestamp -> ActiveReplacement.ActiveReplacement
blastShape src ts =
  ActiveReplacement.MkActiveReplacement
    { ActiveReplacement.effect =
        ReplacementEffect.DamageR (DamageR.MkDamageR (DamagePattern.MkDamagePattern Nothing Filter.Type.IsSource Nothing Nothing Nothing Nothing) (DamageRewrite.SetAmount 4) Seq.empty),
      ActiveReplacement.source = src,
      ActiveReplacement.controller = S.alice,
      ActiveReplacement.timestamp = ts,
      ActiveReplacement.expiry = Expiry.Never,
      ActiveReplacement.uses = Uses.Unlimited,
      ActiveReplacement.origin = ReplacementOrigin.SelfReplacement,
      ActiveReplacement.condition = Nothing,
      ActiveReplacement.rider = Nothing,
      ActiveReplacement.slots = Map.empty
    }

-- alice controls `n` Mountains and `extra` further permanents, and holds a
-- Shimatsu the Bloodcloaked. Returns the state, the Shimatsu's hand id, and the
-- ids of the extra permanents in the order they were added.
--
-- Goblin Piker for the extras: a vanilla creature, so nothing it carries can
-- reach the entry loop and the only thing that changes when one is sacrificed is
-- the count.
shimatsuBoard :: Printing.Printing -> Int -> Printing.Printing -> Int -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId])
shimatsuBoard mountain n pikerPrinting extra shimatsu =
  let base = S.landsInPlay mountain n
      addOne (ids, g) _ = let (oid, g1) = S.addPermanent pikerPrinting S.alice g in (ids <> [oid], g1)
      (pikers, withPikers) = List.foldl' addOne ([], base) (replicate extra ())
      (gs, held) = S.handOne shimatsu withPikers
   in ( gs
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        held,
        pikers
      )

-- Sacrifice exactly `wanted` when the as-enters choice is offered, and nothing
-- else about the game.
sacrificesExactly :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
sacrificesExactly wanted p = case p of
  Prompt.ChooseAnyNumberToSacrifice _ _ _ candidates -> Set.fromList (filter (`elem` candidates) wanted)
  _ -> S.identityAnswer p

-- Sacrifice EVERYTHING the engine offers. What makes the CR 614.12a exclusion
-- testable: if the entering Shimatsu were among its own candidates, a greedy
-- answer would sacrifice it.
sacrificesAll :: Prompt.Prompt r -> r
sacrificesAll p = case p of
  Prompt.ChooseAnyNumberToSacrifice _ _ _ candidates -> Set.fromList candidates
  _ -> S.identityAnswer p

shimatsuSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
shimatsuSpec s registry =
  Spec.describe s "Shimatsu the Bloodcloaked (CR 614.1c)" $ do
    Spec.it s "CR 614.1c sacrificing two permanents enters a 2/2 with two +1/+1 counters" $ do
      mountain <- S.printingOf s registry "Mountain"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
      let (gs, held, pikers) = shimatsuBoard mountain 4 pikerPrinting 3 shimatsu
      case pikers of
        first : second : _ ->
          let after = S.runPure (sacrificesExactly [first, second]) gs (S.cast S.alice held >> Stack.resolveTop)
           in case newestNamed (CardName.MkCardName $ Text.pack "Shimatsu the Bloodcloaked") after of
                Nothing -> Spec.assertFailure s "Shimatsu did not reach the battlefield"
                Just shimatsuId -> do
                  Spec.assertEqWith s "two +1/+1 counters" (countersOn CounterKind.PlusOnePlusOne shimatsuId after) 2
                  -- Printed 0/0, so the counters are the whole of its body.
                  Spec.assertEqWith s "power" (Projection.powerOf shimatsuId after) (Just 2)
                  Spec.assertEqWith s "toughness" (Projection.toughnessOf shimatsuId after) (Just 2)
                  -- CR 701.21a: to the OWNER's graveyard, and only the two named.
                  Spec.assertEqWith s "the two chosen Pikers left the battlefield" (filter (\oid -> Set.member oid (GameState.battlefield after)) [first, second]) []
        _ -> Spec.assertFailure s "fixture did not build two Pikers"
    Spec.it s "CR 704.5f sacrificing nothing enters a 0/0 that dies" $ do
      mountain <- S.printingOf s registry "Mountain"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
      let (gs, held, _) = shimatsuBoard mountain 4 pikerPrinting 2 shimatsu
          -- S.identityAnswer answers the empty set: sacrifice nothing.
          after = S.runPure S.identityAnswer gs (S.cast S.alice held >> Stack.resolveTop >> Engine.settleForPriority)
      Spec.assertEqWith s "the 0/0 Shimatsu is gone" (newestNamed (CardName.MkCardName $ Text.pack "Shimatsu the Bloodcloaked") after) Nothing
    Spec.it s "CR 614.12a/701.21a Shimatsu is not among the permanents it may sacrifice" $ do
      mountain <- S.printingOf s registry "Mountain"
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
      -- Four Mountains and two Pikers, all of them alice's permanents, so a
      -- greedy answer sacrifices six -- and would sacrifice seven if the
      -- entering Shimatsu were offered to itself.
      let (gs, held, _) = shimatsuBoard mountain 4 pikerPrinting 2 shimatsu
          after = S.runPure sacrificesAll gs (S.cast S.alice held >> Stack.resolveTop)
      case newestNamed (CardName.MkCardName $ Text.pack "Shimatsu the Bloodcloaked") after of
        Nothing -> Spec.assertFailure s "Shimatsu sacrificed itself"
        Just shimatsuId -> do
          Spec.assertEqWith s "six counters, one per OTHER permanent" (countersOn CounterKind.PlusOnePlusOne shimatsuId after) 6
          Spec.assertEqWith s "nothing else of alice's is left" (Set.toList (GameState.battlefield after)) [shimatsuId]

-- alice controls four untapped Forests and holds an Undergrowth Scavenger. Her
-- graveyard holds `aliceCreatures` Goblin Pikers and `aliceLands` Mountains;
-- bob's holds `bobCreatures` Goblin Pikers. Returns the state and the
-- Scavenger's hand id.
--
-- Every element earns its place against a different wrong reading of "the number
-- of creature cards in all graveyards":
--
--   * bob's graveyard is stocked, so Scope.EachPlayer and Scope.Relative You
--     disagree. Without it the two readings produce the same number.
--   * alice's graveyard holds a LAND card too, so HasCardType Creature is not
--     vacuous. Without it, dropping the filter changes nothing.
--   * the two seats hold DIFFERENT counts, so an implementation reading one
--     graveyard twice is caught as well.
--
-- Goblin Piker for the creature cards, Mountain for the noncreature: neither
-- carries anything that reaches the entry loop, so the only thing the board
-- varies is what a graveyard holds.
scavengerBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> Int -> Int -> (GameState.GameState, ObjectId.ObjectId)
scavengerBoard forest piker mountain scavenger aliceCreatures aliceLands bobCreatures =
  let bury printing pid g _ = snd (S.addGraveyardCard printing pid g)
      base = S.landsInPlay forest 4
      buried =
        List.foldl' (bury piker S.bob) (List.foldl' (bury mountain S.alice) (List.foldl' (bury piker S.alice) base (replicate aliceCreatures ())) (replicate aliceLands ())) (replicate bobCreatures ())
      (gs, held) = S.handOne scavenger buried
   in ( gs
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        held
      )

-- CR 614.1c's variable amount: "This creature enters with a number of +1/+1
-- counters on it equal to the number of creature cards in all graveyards."
undergrowthScavengerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
undergrowthScavengerSpec s registry =
  Spec.describe s "Undergrowth Scavenger (CR 614.1c)" $ do
    Spec.it s "CR 614.1c one +1/+1 counter per creature card in EVERY graveyard" $ do
      forest <- S.printingOf s registry "Forest"
      piker <- S.printingOf s registry "Goblin Piker"
      mountain <- S.printingOf s registry "Mountain"
      scavenger <- S.printingOf s registry "Undergrowth Scavenger"
      -- Two creature cards and a land in alice's graveyard, one creature card in
      -- bob's: three, and no other reading of the sentence gives three.
      let (gs, held) = scavengerBoard forest piker mountain scavenger 2 1 1
          after = S.runPure S.identityAnswer gs (S.cast S.alice held >> Stack.resolveTop >> Engine.settleForPriority)
      case newestNamed (CardName.MkCardName $ Text.pack "Undergrowth Scavenger") after of
        Nothing -> Spec.assertFailure s "the Scavenger did not survive the battlefield"
        Just scavengerId -> do
          -- The printed body is 0/0, so power and toughness ARE the count.
          Spec.assertEqWith s "power" (Projection.powerOf scavengerId after) (Just 3)
          Spec.assertEqWith s "toughness" (Projection.toughnessOf scavengerId after) (Just 3)
          Spec.assertEqWith s "three +1/+1 counters" (countersOn CounterKind.PlusOnePlusOne scavengerId after) 3
          -- CR 614.1c fixes the number AS the permanent enters, so a fourth
          -- creature card reaching a graveyard afterwards does not grow it. The
          -- half that separates a stamped count from one re-read live.
          let (_, later) = S.addGraveyardCard piker S.bob after
          Spec.assertEqWith s "still 3/3 after a fourth creature card is buried" (Projection.powerOf scavengerId later) (Just 3)
    Spec.it s "CR 704.5f with every graveyard empty it enters 0/0 and dies" $ do
      forest <- S.printingOf s registry "Forest"
      piker <- S.printingOf s registry "Goblin Piker"
      mountain <- S.printingOf s registry "Mountain"
      scavenger <- S.printingOf s registry "Undergrowth Scavenger"
      -- The same board with nothing buried, which is CR 107.1b's floor rather
      -- than the count: a correct engine and one that clamped a zero amount to
      -- zero agree here. What it rules out is an engine that refuses the entry,
      -- raises, or places a counter it was never told to.
      let (gs, held) = scavengerBoard forest piker mountain scavenger 0 0 0
          after = S.runPure S.identityAnswer gs (S.cast S.alice held >> Stack.resolveTop >> Engine.settleForPriority)
      Spec.assertEqWith s "the 0/0 Scavenger is gone" (newestNamed (CardName.MkCardName $ Text.pack "Undergrowth Scavenger") after) Nothing

-- CR 614.12b's board: alice controls nine untapped Islands, two untapped
-- Forests and an untapped Bayou, a TAPPED Forest, a Mountain and a Wood
-- Elemental, and holds a Rite of Replication. Returns the state, the Rite's
-- hand id, the Wood Elemental, the three sacrificeable lands in the order they
-- were added, the tapped Forest and the Mountain.
--
-- Wood Elemental {3}{G} Creature -- Elemental */*: "As this creature enters,
-- sacrifice any number of untapped Forests. Wood Elemental's power and
-- toughness are each equal to the number of Forests sacrificed as it entered."
-- Kicked, the Rite mints FIVE token copies of it at one moment, and each token
-- carries the copied face's own as-enters sacrifice -- so five entry costs
-- compete for one supply of three Forests.
--
-- The Islands are added FIRST, so they are the lowest-id mana sources and
-- Replay.defaultAnswer's head-of-list ChooseManaSource answer pays the whole
-- kicked cost ({2}{U}{U} plus {5}) with them. A Forest tapped to pay for the
-- spell would leave the assertions measuring the payment rather than the rule.
--
-- Three sacrificeable lands against five entering permanents is the scarcity
-- the rule is about, and 3 and 5 are chosen so no two readings of it agree: a
-- shared supply leaves ONE 3/3 token, a per-permanent supply leaves five, and
-- one-each leaves three 1/1s.
--
-- Bayou (Land -- Swamp Forest) among the two Forests, and a tapped Forest and a
-- Mountain beside them, so WHICH permanents went is observable rather than
-- inferred from a count: the criterion is "untapped Forests", which the Bayou
-- satisfies, and which the tapped Forest and the Mountain each fail on one
-- half.
--
-- The Wood Elemental on the battlefield carries a +1/+1 counter, the trick
-- Pawl.CopySpec's Clone board uses: its P/T is a sacrifice count it never made,
-- so a 0/0 would die to CR 704.5f before the Rite could target it. Counters are
-- not copiable (CR 707.2), so the tokens are minted off the printed */*.
woodElementalBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, [ObjectId.ObjectId], ObjectId.ObjectId, ObjectId.ObjectId)
woodElementalBoard island forest bayou mountain woodElemental rite =
  let base = S.landsInPlay island 9
      (firstForest, board1) = S.addPermanent forest S.alice base
      (secondForest, board2) = S.addPermanent forest S.alice board1
      (bayouId, board3) = S.addPermanent bayou S.alice board2
      (tappedForest, board4) = S.addPermanent forest S.alice board3
      (mountainId, board5) = S.addPermanent mountain S.alice board4
      (elementalId, board6) = S.addPermanent woodElemental S.alice board5
      (held, board7) = S.addHandCard rite S.alice (S.addCounter CounterKind.PlusOnePlusOne 1 elementalId (S.tapObject tappedForest board6))
   in ( board7
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        held,
        elementalId,
        [firstForest, secondForest, bayouId],
        tappedForest,
        mountainId
      )

-- Kick the Rite of Replication and aim it at `victim` -- PINNED to that id
-- rather than searched for, so a mutation cannot be repaired by an answerer
-- that finds another legal target -- answering every as-enters sacrifice with
-- `sacrificeAnswer`.
riteOn :: ObjectId.ObjectId -> (forall r. Prompt.Prompt r -> r) -> Prompt.Prompt a -> a
riteOn victim sacrificeAnswer p = case p of
  Prompt.ChooseKicker {} -> KickerDecision.MkKickerDecision 1
  Prompt.ChooseTargets _ _ _ sets -> Map.map (const (Set.singleton (Recipient.ToCreature victim))) sets
  _ -> sacrificeAnswer p

-- Sacrifice the FIRST permanent of `order` that is still being offered, and
-- nothing else. Pinned by id rather than by position in the offer: an engine
-- that let a later entry choice see what an earlier one already spent would
-- hand every token the same first id, where this answerer walks down the list
-- exactly as the supply is consumed.
sacrificesOneOf :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
sacrificesOneOf order p = case p of
  Prompt.ChooseAnyNumberToSacrifice _ _ _ candidates ->
    Set.fromList (take 1 (filter (`elem` candidates) order))
  _ -> S.identityAnswer p

-- riteOn with sacrificesOneOf, as one rank-1 function: a `let` binding of the
-- composition monomorphizes, and both S.runPure and answersFor want it
-- polymorphic.
riteSplitting :: ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
riteSplitting victim order = riteOn victim (sacrificesOneOf order)

-- How many times a player was asked to make an as-enters sacrifice. One per
-- entering permanent that had something to choose from -- the arm elides the
-- prompt when nothing is offered, so this counts the entry costs that found a
-- budget left rather than the permanents that entered.
sacrificeAsks :: [Response.Response] -> Int
sacrificeAsks responses =
  let isSacrifice r = case r of
        Response.ChoseSacrifices _ -> True
        _ -> False
   in length (filter isSacrifice responses)

-- The tokens on the battlefield, newest first.
tokensOnBattlefield :: GameState.GameState -> [ObjectId.ObjectId]
tokensOnBattlefield gs = List.sortOn Ord.Down (filter (`Game.isToken` gs) (Set.toList (GameState.battlefield gs)))

entryBudgetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
entryBudgetSpec s registry =
  Spec.describe s "One budget for simultaneous entry costs (CR 614.12b)" $ do
    -- THE PROVING BOARD for CR 614.12b and CR 614.13b, with a greedy answerer:
    -- the first token to be asked takes every Forest there is, and the four
    -- entering beside it are left with nothing to sacrifice and no prompt at
    -- all. Five permanents, three Forests, one 3/3.
    --
    -- CR 614.13b ("the same object can't be chosen to change zones more than
    -- once when applying replacement effects that modify how one or more
    -- permanents enter the battlefield") is the sharper half of the citation,
    -- and the P/T is what observes it: the arm counts what was CHOSEN, so an
    -- engine that offered a spent Forest to the next token would stamp the same
    -- three on all five and leave five 3/3s, even though the sacrifice funnel
    -- would move nothing the second time.
    Spec.it s "a greedy first choice leaves the four entering beside it nothing (CR 614.12b, CR 614.13b)" $ do
      island <- S.printingOf s registry "Island"
      forest <- S.printingOf s registry "Forest"
      bayou <- S.printingOf s registry "Bayou"
      mountain <- S.printingOf s registry "Mountain"
      woodElemental <- S.printingOf s registry "Wood Elemental"
      rite <- S.printingOf s registry "Rite of Replication"
      let (gs, held, elementalId, sacrificeable, tappedForest, mountainId) = woodElementalBoard island forest bayou mountain woodElemental rite
          play = S.cast S.alice held >> Stack.resolveTop >> Engine.settleForPriority
          after = S.runPure (riteOn elementalId sacrificesAll) gs play
          asks = sacrificeAsks (answersFor (riteOn elementalId sacrificesAll) gs play)
      Spec.assertEqWith s "one token survived, and it is the 3/3 that got all three" (fmap (\oid -> S.powerToughnessOf oid after) (tokensOnBattlefield after)) [Just (3, 3)]
      Spec.assertEqWith s "only ONE of the five entry costs found anything to spend" asks 1
      Spec.assertEqWith s "the two Forests and the Bayou all left the battlefield" (filter (\oid -> Set.member oid (GameState.battlefield after)) sacrificeable) []
      -- BY NAME, not by id: CR 400.7's new object gets a fresh id on the way to
      -- the graveyard, so the ids the fixture holds name only the battlefield
      -- incarnations. Three cards and no more is the other half of the
      -- assertion above -- nothing was sacrificed twice.
      Spec.assertEqWith s "the two Forests and the Bayou are alice's whole graveyard, beside the spent Rite" (graveyardNames S.alice after) (fmap (CardName.MkCardName . Text.pack) ["Bayou", "Forest", "Forest", "Rite of Replication"])
      Spec.assertBool s (Set.member tappedForest (GameState.battlefield after)) "the TAPPED Forest was never offered"
      Spec.assertBool s (Set.member mountainId (GameState.battlefield after)) "nor was the Mountain"
      Spec.assertEqWith s "the copied Wood Elemental is untouched" (S.powerToughnessOf elementalId after) (Just (1, 1))
    -- The same board, the same five entering permanents and the same supply,
    -- with the one answer changed: each entry cost spends ONE named Forest
    -- rather than all of them. The budget NARROWS the later choices rather than
    -- only emptying them -- three tokens are asked and each gets a different
    -- land, the fourth and fifth are asked nothing.
    --
    -- The paired control for the case above: the two boards differ in exactly
    -- the answer, and they disagree about how many tokens survive and at what
    -- size, so neither can pass for the other's reason.
    Spec.it s "each later choice sees only what the earlier ones left (CR 614.12b)" $ do
      island <- S.printingOf s registry "Island"
      forest <- S.printingOf s registry "Forest"
      bayou <- S.printingOf s registry "Bayou"
      mountain <- S.printingOf s registry "Mountain"
      woodElemental <- S.printingOf s registry "Wood Elemental"
      rite <- S.printingOf s registry "Rite of Replication"
      let (gs, held, elementalId, sacrificeable, _, _) = woodElementalBoard island forest bayou mountain woodElemental rite
          play = S.cast S.alice held >> Stack.resolveTop >> Engine.settleForPriority
          after = S.runPure (riteSplitting elementalId sacrificeable) gs play
          asks = sacrificeAsks (answersFor (riteSplitting elementalId sacrificeable) gs play)
      Spec.assertEqWith s "three tokens survived, each a 1/1 off one Forest" (fmap (\oid -> S.powerToughnessOf oid after) (tokensOnBattlefield after)) [Just (1, 1), Just (1, 1), Just (1, 1)]
      Spec.assertEqWith s "three of the five entry costs found something to spend" asks 3
      Spec.assertEqWith s "all three lands went, one apiece" (filter (\oid -> Set.member oid (GameState.battlefield after)) sacrificeable) []

-- alice controls `mountains` untapped Mountains and `forests` untapped Forests
-- in a precombat main phase with priority, holding one card per printing in
-- `hand`. Returns the state and the hand ids in the order given.
--
-- Two land printings rather than blueBoard's one, because riot's producers are
-- Gruul: Zhur-Taa Goblin is {R}{G}.
riotBoard :: Printing.Printing -> Int -> Printing.Printing -> Int -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId])
riotBoard mountain mountains forest forests hand =
  let base = S.landsInPlay mountain mountains
      -- S.addPermanent puts one permanent of a printing onto the battlefield,
      -- settled; nothing in it is creature-specific, which is what lets a second
      -- land printing join a board S.landsInPlay built from one.
      addLand g _ = snd (S.addPermanent forest S.alice g)
      withForests = List.foldl' addLand base (replicate forests ())
      addOne (ids, g) p = let (oid, g1) = S.addHandCard p S.alice g in (ids <> [oid], g1)
      (held, gs) = List.foldl' addOne ([], withForests) hand
   in ( gs
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          },
        held
      )

-- Answer riot's "may" one way, and everything else the way S.aggressiveAnswer
-- does -- which declares every attacker it is offered, so one answerer carries
-- both halves of a case that casts a creature and then attacks with it.
riotChoosing :: OptionalDecision.OptionalDecision -> Prompt.Prompt r -> r
riotChoosing choice p = case p of
  Prompt.ChooseRiot {} -> choice
  _ -> S.aggressiveAnswer p

wasAskedForRiot :: [Response.Response] -> Bool
wasAskedForRiot responses = riotAsks responses > 0

-- How many times riot's "may" was put to a player. CR 702.136b turns this into
-- an assertion rather than a diagnostic: one instance, one ask.
riotAsks :: [Response.Response] -> Int
riotAsks responses =
  let isRiot r = case r of
        Response.ChoseRiot _ -> True
        _ -> False
   in length (filter isRiot responses)

-- Turn the LAST riot answer in a transcript into a decline, leaving every other
-- answer alone.
--
-- A transcript rewrite because a `Prompt r -> r` answerer cannot do it: CR
-- 702.136b's two prompts name the same decider, the same player and the same
-- permanent, so nothing in the prompt tells them apart, while a positional
-- transcript does. Pawl.Engine.Replay.replay is the same machinery MulliganSpec
-- replays an opening hand with.
declineLastRiot :: [Response.Response] -> [Response.Response]
declineLastRiot responses =
  let flipFirst rs = case rs of
        [] -> []
        Response.ChoseRiot _ : rest -> Response.ChoseRiot OptionalDecision.Declines : rest
        r : rest -> r : flipFirst rest
   in reverse (flipFirst (reverse responses))

-- The board moved to alice's declare-attackers step, with bob defending. Stated
-- rather than played out, exactly as S.combatBoardOf states it: a direct-call
-- test never runs the turn-based action that would settle CR 506.2's defending
-- player.
--
-- Nothing else is touched, so a creature cast in the main phase is still as new
-- to the battlefield as CR 302.6 finds it.
atDeclareAttackers :: GameState.GameState -> GameState.GameState
atDeclareAttackers gs =
  gs
    { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
      GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.bob]}
    }

attackersIn :: GameState.GameState -> [ObjectId.ObjectId]
attackersIn gs = Map.keys (Combat.Type.attackers (GameState.combat gs))
