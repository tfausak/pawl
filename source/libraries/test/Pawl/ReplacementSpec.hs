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
import Pawl.DamageReplacementSpec (graveyardNames)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cast as Cast
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
import Pawl.PreventionSpec (aimObject, answersFor, castAndResolve, castOrPassAnswer, clachanBoard, copyOf, counterBoard, countersOn, leylineShape, lostLife, namedOut, newestNamed, payLifeOnEntryAnswer, raceAnswer, revealAsks, revealOnEntryAnswer, seaGateBoard, theAbility, warriorOut, wasAskedForEntryOption, wasAskedToReplace)
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.BecameAttached as BecameAttached
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost.Type
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
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KickerDecision as KickerDecision
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics
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
        Spec.assertBool s (Replacement.matchesPermanent (Projection.viewsOf g1) g1 Nothing Map.empty Nothing (Filter.Type.HasCardType CardType.Creature) piker) "the creature matches HasCardType Creature"
        Spec.assertBool s (not (Replacement.matchesPermanent (Projection.viewsOf g1) g1 Nothing Map.empty Nothing (Filter.Type.HasCardType CardType.Creature) landId)) "the land does not match HasCardType Creature"
        Spec.assertBool s (Replacement.matchesPermanent (Projection.viewsOf g1) g1 Nothing Map.empty Nothing (Filter.Type.And []) landId) "the trivial filter matches the land too"
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
          [ DamageEvent.MkDamageEvent victimA (Recipient.ToCreature victimA) 2 False False False 0 Nothing Nothing mempty False DamageKind.Combat,
            DamageEvent.MkDamageEvent victimB (Recipient.ToCreature victimB) 2 False False False 0 Nothing Nothing mempty False DamageKind.Combat
          ]
        after = S.runPure S.identityAnswer resolved (Damage.applyDamage batch)
    Spec.assertEqWith s "the first attacker's damage was prevented" (S.damageOf victimA after) (Just 0)
    Spec.assertEqWith s "and so was the second's, independently" (S.damageOf victimB after) (Just 0)
    Spec.assertEqWith s "no damage event was recorded at all" (S.damageEventsOf after) []
  -- CR 701.19b's other form of regeneration: Mossbridge Troll's "If this
  -- creature would be destroyed, regenerate it." is a static ability (CR 604.1),
  -- so CR 604.2 keeps its replacement effect active for as long as the permanent
  -- is on the battlefield and the projection re-derives the row for every
  -- destruction -- there is no shield to use up. The Drudge Skeletons beside it
  -- is the discriminator: one board, one pair of destructions, and only CR
  -- 701.19a's shield is spent by the first.
  Spec.it s "CR 701.19b a static regeneration ability replaces every destruction, not just the next one" $ do
    swamp <- S.printingOf s registry "Swamp"
    mossbridgeTroll <- S.printingOf s registry "Mossbridge Troll"
    drudgeSkeletons <- S.printingOf s registry "Drudge Skeletons"
    let base = S.landsInPlay swamp 1
        (troll, g1) = S.addPermanent mossbridgeTroll S.alice base
        (skel, g2) = S.addPermanent drudgeSkeletons S.alice g1
        -- {B}: Regenerate this creature, activated and resolved, so the
        -- Skeletons carries CR 701.19a's shield and the Troll carries nothing
        -- but its printed text.
        armed = S.runPure S.identityAnswer g2 (Activate.activateAbility S.alice skel (theAbility drudgeSkeletons) >> Stack.resolveTop)
        once = S.runPure S.identityAnswer armed (Event.destroy Regenerability.Regenerable [troll, skel])
        twice = S.runPure S.identityAnswer once (Event.destroy Regenerability.Regenerable [troll, skel])
    Spec.assertBool s (Set.member troll (GameState.battlefield once)) "the Troll survived the first destruction"
    Spec.assertBool s (Set.member troll (GameState.battlefield twice)) "and the second -- CR 701.19b's static ability is not used up"
    Spec.assertBool s (not (Set.member skel (GameState.battlefield twice))) "while CR 701.19a's shield beside it was, so the Skeletons died to the second"
    Spec.assertEqWith s "and the only floating row on the board was the Skeletons'" (length (GameState.replacements armed)) 1
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
        hurt = S.runPure S.identityAnswer armed (Damage.applyDamage [DamageEvent.MkDamageEvent troll (Recipient.ToCreature troll) 2 False False False 0 Nothing Nothing mempty False DamageKind.Combat])
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
  -- off whether the creature gets cast -- Activatable.activatable is deliberately not
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
  undergrowthScavengerSpec s registry
  fixedEntryCostSpec s registry
  degavolverSpec s registry
  grifterBladeSpec s registry
  hyenaUmbraSpec s registry
  darkblastSpec s registry

-- Rite of Replication unkicked, aimed at `victim` -- PINNED to that id rather
-- than searched for, so a mutation cannot be repaired by an answerer that finds
-- another legal target.
riteAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
riteAt victim p = case p of
  Prompt.ChooseKicker {} -> KickerDecision.MkKickerDecision 0
  Prompt.ChooseTargets _ _ _ sets -> Map.map (const (Set.singleton (Recipient.ToCreature victim))) sets
  _ -> S.identityAnswer p

-- Degavolver {1}{W} Creature -- Volver 1/1, whole text: "Kicker {1}{B} and/or {R}
-- ... If this creature was kicked with its {1}{B} kicker, it enters with two
-- +1/+1 counters on it and with 'Pay 3 life: Regenerate this creature.' If this
-- creature was kicked with its {R} kicker, it enters with a +1/+1 counter on it
-- and with first strike." (oracle checked on Scryfall)
--
-- CR 614.1c's enters-with clause granting a QUOTED ability: the same
-- stored layer-6 grant Faerie Squadron's flying takes, carrying a
-- GrantedAbility.Activated.
--
-- THE BOARD: three Plains, a Swamp and a Mountain -- {1}{W} plus both kickers --
-- so the cases differ in the kicker answers and in nothing else.
degavolverBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId)
degavolverBoard plains swamp mountain degavolver =
  S.handOne degavolver (S.landsFor mountain S.alice 1 (S.landsFor swamp S.alice 1 (S.landsInPlay plains 3)))

degavolversOut :: GameState.GameState -> [ObjectId.ObjectId]
degavolversOut gs = filter (\o -> Projection.hasName (CardName.MkCardName (Text.pack "Degavolver")) o gs) (Set.toList (GameState.battlefield gs))

-- CR 702.33f: kicks exactly the one kicker cost `wanted`, told apart by the Cost
-- the prompt carries.
kicksOnly :: Cost.Type.Cost Keyword.Keyword -> Prompt.Prompt r -> r
kicksOnly wanted p = case p of
  Prompt.ChooseKicker _ _ _ keyword _ -> KickerDecision.MkKickerDecision (if keyword == Keyword.Kicker wanted then 1 else 0)
  _ -> S.identityAnswer p

blackKicker :: Cost.Type.Cost Keyword.Keyword
blackKicker = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Black)]), Cost.Type.components = []}

redKicker :: Cost.Type.Cost Keyword.Keyword
redKicker = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.OfType (ManaType.Colored Color.Red)]), Cost.Type.components = []}

castDegavolver :: Cost.Type.Cost Keyword.Keyword -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
castDegavolver wanted gs spellId =
  let cast = snd (Engine.runGamePure (kicksOnly wanted) gs (S.cast S.alice spellId))
   in snd (Engine.runGamePure (kicksOnly wanted) cast (Stack.resolveTop >> Engine.settleForPriority))

-- The permanent's projected activated abilities: what it HAS, printed or granted.
activatedOf :: ObjectId.ObjectId -> GameState.GameState -> [ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card)]
activatedOf oid gs = ProjectedCharacteristics.activatedAbilities (Projection.project oid gs)

degavolverSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
degavolverSpec s registry = Spec.describe s "Degavolver" $ do
  -- Every activated ability the permanent has is activated and resolved, then it
  -- is destroyed. Printed, it has none, so a grant that did not land leaves
  -- nothing to activate and the destruction goes through: the survival read
  -- FIRST is the gameplay half, the life total the cost's.
  Spec.it s "CR 614.1c kicked with {1}{B}, it enters with 'Pay 3 life: Regenerate this creature.' and uses it" $ do
    plains <- S.printingOf s registry "Plains"
    swamp <- S.printingOf s registry "Swamp"
    mountain <- S.printingOf s registry "Mountain"
    degavolver <- S.printingOf s registry "Degavolver"
    let (board, spellId) = degavolverBoard plains swamp mountain degavolver
        entered = castDegavolver blackKicker board spellId
    case degavolversOut entered of
      [permId] -> do
        let shielded = S.runPure S.identityAnswer entered (Foldable.for_ (activatedOf permId entered) $ \ability -> Activate.activateAbility S.alice permId ability >> Stack.resolveTop)
            destroyed = S.settleSba (S.runPure S.identityAnswer shielded (Event.destroy Regenerability.Regenerable [permId]))
        Spec.assertBool s (Set.member permId (GameState.battlefield destroyed)) "CR 701.19a the granted ability's shield regenerated it"
        Spec.assertEqWith s "CR 119.4 paying 3 life for it" (S.lifeOf S.alice destroyed) (Just 17)
        Spec.assertEqWith s "and the counter half placed two +1/+1 counters" (S.powerToughnessOf permId entered) (Just (3, 3))
      other -> Spec.assertFailure s ("expected one Degavolver, got " <> show (length other))
  -- The other kicker alone: its row applies and the {1}{B} row does not, so the
  -- permanent has first strike and no activated ability.
  Spec.it s "CR 702.33f kicked with {R} only, first strike and no quoted ability" $ do
    plains <- S.printingOf s registry "Plains"
    swamp <- S.printingOf s registry "Swamp"
    mountain <- S.printingOf s registry "Mountain"
    degavolver <- S.printingOf s registry "Degavolver"
    let (board, spellId) = degavolverBoard plains swamp mountain degavolver
        entered = castDegavolver redKicker board spellId
    case degavolversOut entered of
      [permId] -> do
        Spec.assertEqWith s "CR 604.2 the {1}{B} row did not apply: no activated ability" (length (activatedOf permId entered)) 0
        Spec.assertBool s (Projection.hasKeyword Keyword.FirstStrike permId entered) "CR 614.1c it has first strike"
        Spec.assertEqWith s "and one +1/+1 counter" (S.powerToughnessOf permId entered) (Just (2, 2))
      other -> Spec.assertFailure s ("expected one Degavolver, got " <> show (length other))
  -- CR 707.2: "with '...'" sets no power and toughness, so the grant is not a
  -- copiable value -- a token copy of the kicked Degavolver has no activated
  -- ability, where the original keeps its one. The Islands for Rite of
  -- Replication are added after the entry, so they cannot pay for it.
  Spec.it s "CR 707.2 a token copy of the kicked Degavolver does not have the quoted ability" $ do
    plains <- S.printingOf s registry "Plains"
    swamp <- S.printingOf s registry "Swamp"
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    degavolver <- S.printingOf s registry "Degavolver"
    rite <- S.printingOf s registry "Rite of Replication"
    let (board, spellId) = degavolverBoard plains swamp mountain degavolver
        entered = castDegavolver blackKicker board spellId
        (riteId, withRite) = S.addHandCard rite S.alice (S.landsFor island S.alice 4 entered)
    case degavolversOut entered of
      [origId] -> do
        let cast = snd (Engine.runGamePure (riteAt origId) withRite (S.cast S.alice riteId))
            after = snd (Engine.runGamePure (riteAt origId) cast (Stack.resolveTop >> Engine.settleForPriority))
        case Set.toList (Set.difference (Set.fromList (degavolversOut after)) (Set.singleton origId)) of
          [tokenId] -> do
            Spec.assertEqWith s "CR 707.2 the token copy has no activated ability" (length (activatedOf tokenId after)) 0
            Spec.assertEqWith s "and the original still has the one it entered with" (length (activatedOf origId after)) 1
          tokens -> Spec.assertFailure s ("expected exactly one token copy, got " <> show (length tokens))
      other -> Spec.assertFailure s ("expected one Degavolver, got " <> show (length other))

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
          hit src = S.runPure S.identityAnswer armed (Damage.applyDamage [DamageEvent.MkDamageEvent src (Recipient.ToCreature victim) 2 False False False 0 Nothing Nothing mempty False DamageKind.Noncombat])
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
      addArtifact (ids, g) _ = let (oid, g4) = S.addPermanent splitter S.alice g in (ids <> [oid], g4)
      (splitters, g1) = List.foldl' addArtifact ([], base) [1 .. artifacts]
      (surgeId, g2) = S.addHandCard surge S.alice g1
      addBolt (ids, g) _ = let (oid, g4) = S.addHandCard firebolt S.alice g in (ids <> [oid], g4)
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

voltaicSurgeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
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
    (fmap ActiveReplacement.source (filter (\active -> ActiveReplacement.controller active == who) (GameState.replacements gs)))

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
    -- THREE SEATS, where CR 616.1b's "one of them must be chosen" finally has
    -- something to choose between -- the case
    -- cr-614-1d-616-1f-duelling-gather-specimens-alice-takes-it.json cannot
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
-- only observable the split has, which is why it is what
-- cr-616-1c-the-copy-bucket-outranks-kismet-s-so-no-order-is.json asserts; the
-- case below is the discriminating twin that shows the recorder can see one.
kismetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
kismetSpec s registry =
  Spec.describe s "Kismet (CR 616.1c/616.1d)" $ do
    -- The DISCRIMINATING TWIN: the same fixture and the same recorder, a spell
    -- whose own entry rewrites are both CR 616.1e's. Coldsteel Heart is an
    -- artifact, so Kismet's row joins its two in one bucket and the race really
    -- is raised. Without this, that scenario's "no prompt" would pass under a
    -- recorder that never sees a ChooseReplacement on any board.
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
        ReplacementEffect.DamageR (DamageR.MkDamageR (DamagePattern.MkDamagePattern Nothing Filter.Type.IsSource Nothing Nothing Nothing Nothing Nothing) (DamageRewrite.SetAmount 4) Seq.empty),
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
undergrowthScavengerSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
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

-- How many of alice's permanents carry this name.
onBattlefieldNamed :: String -> GameState.GameState -> Int
onBattlefieldNamed name = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack name)) S.alice

fixedEntryCostSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fixedEntryCostSpec s registry =
  Spec.describe s "Fixed entry costs across a batch (CR 614.12b)" $ do
    -- CR 614.1a's other branch: played from hand with no Swamp to sacrifice, the
    -- Lake is put into its owner's graveyard instead of entering.
    Spec.it s "with nothing to sacrifice, the land goes to the graveyard instead (CR 614.1a)" $ do
      forest <- S.printingOf s registry "Forest"
      lake <- S.printingOf s registry "Lake of the Dead"
      let (lakeId, gs) = S.addHandCard lake S.alice (S.landsInPlay forest 1)
          after = S.runPure S.identityAnswer gs {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice} (Cast.playLand False S.alice lakeId Nothing)
      Spec.assertEqWith s "no Lake of the Dead entered" (onBattlefieldNamed "Lake of the Dead" after) 0
      Spec.assertEqWith s "it is in alice's graveyard" (graveyardNames S.alice after) [CardName.MkCardName (Text.pack "Lake of the Dead")]
      Spec.assertEqWith s "and the Forest is untouched" (onBattlefieldNamed "Forest" after) 1
    -- Storm the Festival puts Wood Elemental and Heart of Yavimaya onto the
    -- battlefield together, the Elemental's entry running first. Its first
    -- answer takes both Forests, starving the Heart: that answer is refused and
    -- the question asked again, and the second answer keeps one Forest back.
    Spec.it s "an any-number answer starving a later member is asked again (CR 614.12b)" $ do
      after <- festivalAfter s registry ["Wood Elemental", "Heart of Yavimaya"]
      Spec.assertEqWith s "Heart of Yavimaya entered" (onBattlefieldNamed "Heart of Yavimaya" after) 1
      Spec.assertEqWith s "Wood Elemental is a 1/1 off the one Forest its second answer named" (woodElementalSize after) (Just (1, 1))
    -- The control, differing in one thing: Lake of the Dead, owing a Swamp
    -- alice does not have, in the Heart's place. No answer causes the later cost
    -- to be unpayable, so the first answer stands and takes both Forests.
    Spec.it s "with the later cost unpayable anyway, the first answer stands" $ do
      after <- festivalAfter s registry ["Wood Elemental", "Lake of the Dead"]
      Spec.assertEqWith s "Wood Elemental is a 2/2 off both Forests" (woodElementalSize after) (Just (2, 2))

-- Storm the Festival {3}{G}{G}{G} Sorcery, "Look at the top five cards of your
-- library. You may put up to two permanent cards with mana value 5 or less from
-- among them onto the battlefield. Put the rest on the bottom of your library in
-- a random order." (Oracle text checked against api.scryfall.com.) alice
-- casts it off three Llanowar Elves and three Islands; only then are two
-- untapped Forests put onto the battlefield, so no payment can tap one. Her
-- library, top first, is `top` followed by four Islands. Resolves the spell and
-- settles the board, putting `top` onto the battlefield. Wood Elemental's first
-- answer is every Forest offered and each later one only the first Forest,
-- pinned by id.
festivalAfter :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> [String] -> m GameState.GameState
festivalAfter s registry top = do
  storm <- S.printingOf s registry "Storm the Festival"
  forest <- S.printingOf s registry "Forest"
  island <- S.printingOf s registry "Island"
  elves <- S.printingOf s registry "Llanowar Elves"
  tops <- Monad.mapM (S.printingOf s registry) top
  let withElves = List.foldl' (\g p -> snd (S.addPermanent p S.alice g)) (S.landsFor island S.alice 3 (Setup.emptyGame S.bothPlayers)) (replicate 3 elves)
      islands = List.foldl' (\g p -> snd (S.addLibraryCard p S.alice g)) withElves (replicate 4 island)
      -- Bottom first: S.addLibraryCard puts each new card on top.
      addTop (ids, g) p = let (oid, g1) = S.addLibraryCard p S.alice g in (oid : ids, g1)
      (topIds, stocked) = List.foldl' addTop ([], islands) (reverse tops)
      (withSpell, spell) = S.handOne storm stocked
      cast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
      (firstForest, board1) = S.addPermanent forest S.alice cast
      (_, gs) = S.addPermanent forest S.alice board1
      answer :: Prompt.Prompt r -> State.State Int r
      answer p = case p of
        Prompt.ChooseCardsFromAmong _ _ _ offered _ -> pure (Set.fromList (filter (`List.elem` topIds) offered))
        Prompt.ChooseAnyNumberToSacrifice _ _ _ candidates -> do
          asked <- State.get
          State.put (asked + 1)
          pure (Set.fromList (if asked == 0 then candidates else filter (== firstForest) candidates))
        _ -> pure (S.identityAnswer p)
  pure (snd (State.evalState (Engine.runGame answer gs (Stack.resolveTop >> Engine.settleForPriority)) 0))

-- Wood Elemental's power and toughness, read off the one on alice's battlefield.
woodElementalSize :: GameState.GameState -> Maybe (Integer, Integer)
woodElementalSize after = newestNamed (CardName.MkCardName (Text.pack "Wood Elemental")) after >>= \oid -> S.powerToughnessOf oid after

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

-- How many of alice's graveyard cards have this printing's name. A ZONE count,
-- not an id lookup: CR 400.7 mints a new object for the card that arrives, so
-- the destroyed Aura's battlefield id names nothing there.
inAliceGraveyard :: Printing.Printing -> GameState.GameState -> Int
inAliceGraveyard printing gs =
  let wanted oid = fmap S.nameOf (Game.cardOf oid gs) == Just (S.printingName printing)
   in length (filter wanted (Game.zoneMembers Zone.Graveyard S.alice gs))

-- Grifter's Blade {3} Artifact -- Equipment, whole text: "Flash / As this
-- Equipment enters, choose a creature you control it could be attached to. If
-- you do, it enters attached to that creature. / Equipped creature gets +1/+1. /
-- Equip {1}". Oracle text and both rulings verified against Scryfall
-- (2026-09-19); the rulings are the two branches below -- "must enter attached
-- to a creature you control, if possible" and "if you don't control a creature
-- Grifter's Blade could be attached to, it simply enters unattached".
--
-- The pool's only producer for EntryRewrite.EntersAttachedTo, CR 614.1c's
-- as-enters host choice. Flash is carried but unexercised here: the entry
-- replacement runs the same whatever the timing permission was.
grifterBladeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
grifterBladeSpec s registry = Spec.describe s "Grifter's Blade (CR 614.1c)" $ do
  -- Three seats' worth of roles on two: alice's two creatures against bob's, so
  -- "a creature you control" is told apart from a bare creature filter. The
  -- answer is pinned to the SECOND candidate, and bob's creature is added FIRST
  -- so it holds the LOWEST id -- which makes one pair of readings separate three
  -- wrong implementations at once. An elided or defaulted choice lands on the
  -- Piker; an offer that forgot "you control" makes index 1 the Piker too; only
  -- the right offer puts the Blade on the Sorcerer.
  Spec.it s "CR 614.1c the Blade enters attached to the creature its controller chose" $ do
    mountain <- S.printingOf s registry "Mountain"
    pikerPrinting <- S.printingOf s registry "Goblin Piker"
    sorcererPrinting <- S.printingOf s registry "Prodigal Sorcerer"
    blade <- S.printingOf s registry "Grifter's Blade"
    let (_, g1) = S.addPermanent pikerPrinting S.bob (S.landsInPlay mountain 3)
        (piker, g2) = S.addPermanent pikerPrinting S.alice g1
        (sorcerer, g3) = S.addPermanent sorcererPrinting S.alice g2
        (g4, held) = S.handOne blade g3
        after = S.runPure (attachesTo 1) g4 (S.cast S.alice held >> Stack.resolveTop)
    -- FIRST, and the assertion this case exists for: the chosen creature is
    -- wearing the Blade, read off its projected body (CR 613 layer 7c) rather
    -- than off the attachment field, so a Blade that arrived unattached or
    -- attached elsewhere shows here.
    Spec.assertEqWith s "the Sorcerer alice chose is a 2/2" (S.powerToughnessOf sorcerer after) (Just (2, 2))
    Spec.assertEqWith s "and the Piker she passed over is still a printed 2/1" (S.powerToughnessOf piker after) (Just (2, 1))
    -- A permanent that ARRIVES attached became attached, which is the half
    -- Event.attach cannot record -- there is no CR 701.3 move here, so the
    -- record is written at the entry (Pawl.Engine.Event, where the reasoning
    -- sits). Read off the LOG rather than through a
    -- trigger because nothing in data/cards/ prints one that would fire: a grep
    -- for TriggerCondition.SelfBecomesAttachedBy (2026-09-19) finds only Bramble
    -- Elemental, whose filter is HasSubtype Aura, and the attachment-side
    -- SelfBecomesAttachedTo only Enormous Energy Blade, which is not this card.
    -- So the line this guards is a regression fence rather than a gameplay-level
    -- proof.
    Spec.assertBool
      s
      (any (\event -> case event of GameEvent.BecameAttached record -> Recipient.objectOf (BecameAttached.host record) == Just sorcerer; _ -> False) (S.eventsOf after))
      "and the arrival recorded one BecameAttached naming the Sorcerer"

-- Answer every Prompt.ChooseAttachment with the candidate at this index,
-- counting from zero, falling back to the last where the offer is shorter.
--
-- PINNED BY INDEX rather than searched for: an answerer that looked for a legal
-- host would find the right one again after a mutation widened the offer.
attachesTo :: Int -> Prompt.Prompt r -> r
attachesTo i p = case p of
  Prompt.ChooseAttachment _ _ _ candidates -> case drop i (NonEmpty.toList candidates) of
    chosen : _ -> chosen
    [] -> NonEmpty.last candidates
  _ -> S.identityAnswer p

-- CR 702.89a: "If enchanted permanent would be destroyed, instead remove all
-- damage marked on it and destroy this Aura." A destruction replacement whose
-- SOURCE is not the permanent whose destruction it replaces, which is what
-- Pawl.Engine.Replacement.scopes exists for.
hyenaUmbraSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
hyenaUmbraSpec s registry = Spec.describe s "Hyena Umbra (CR 702.89a)" $ do
  -- The pair on ONE board differs in exactly the keyword: two War Mammoths, one
  -- under Hyena Umbra (3/3 +1/+1 = a 4/4) and one under Unholy Strength (3/3
  -- +2/+1 = a 5/4), so four damage is CR 704.5g lethal to both and the same SBA
  -- pass decides both.
  Spec.it s "CR 704.5g lethal damage destroys the Aura instead, and the marked damage comes off" $ do
    mammoth <- S.printingOf s registry "War Mammoth"
    hyenaUmbra <- S.printingOf s registry "Hyena Umbra"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    let base = Setup.emptyGame S.bothPlayers
        (armored, g1) = S.addPermanent mammoth S.alice base
        (bare, g2) = S.addPermanent mammoth S.alice g1
        (umbra, g3) = S.addPermanent hyenaUmbra S.alice g2
        (strength, g4) = S.addPermanent unholyStrength S.alice g3
        board = S.attach strength bare (S.attach umbra armored g4)
        hurt = S.markDamage bare 4 (S.markDamage armored 4 board)
        settled = S.settleSba hurt
    Spec.assertEqWith
      s
      "CR 702.89a the enchanted creature survives, with the marked damage removed"
      (S.onBattlefield armored settled, S.damageOf armored settled)
      (True, Just 0)
    Spec.assertEqWith
      s
      "and the Aura took the destruction instead, so it is in its owner's graveyard"
      (S.onBattlefield umbra settled, inAliceGraveyard hyenaUmbra settled)
      (False, 1)
    Spec.assertBool s (not (S.onBattlefield bare settled)) "control: the Mammoth under an Aura WITHOUT umbra armor died"

-- CR 702.52a / 614.11: Darkblast ({B} Instant, "Target creature gets -1/-1 until
-- end of turn. / Dredge 3" -- name, cost, type line and Oracle text checked
-- against api.scryfall.com 2026-09-18).
--
-- The pool's dredge producer and the only row rule 702 mints into a GRAVEYARD:
-- Pawl.Engine.Keyword.graveyardReplacementsOf builds it off the card's keywords and
-- Pawl.Engine.Projection.replacementsAffecting's graveyard walk gathers it, so
-- nothing has to be cast for the row to stand.
--
-- THE BOARD in every case: the Darkblast in alice's graveyard, her library
-- stocked with basics of four distinct names, and one call to Event.drawCard --
-- the narrowest path that raises CR 121.1's WouldDraw. The three cases differ in
-- exactly one thing each: the answer to the dredge prompt, and the size of the
-- library.
--
-- Every number is distinct -- four library cards, a dredge of three, one card
-- returned -- so no two readings land on the same count.
darkblastSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
darkblastSpec s registry = Spec.describe s "Darkblast (CR 702.52)" $ do
  Spec.it s "CR 702.52a dredging returns the card and mills three instead of drawing" $ do
    board <- dredgeBoard s registry 4
    let after = S.runPure dredging board (Event.drawCard S.alice)
    Spec.assertEqWith s "CR 702.52a the Darkblast came back, and no library card was drawn" (handNames S.alice after) [CardName.MkCardName (Text.pack "Darkblast")]
    Spec.assertEqWith s "CR 701.17a three cards were milled, not one drawn" (length (Game.zoneMembers Zone.Library S.alice after)) 1
    Spec.assertEqWith s "CR 614.6 and the graveyard holds those three alone" (length (graveyardNames S.alice after)) 3
  -- The CONTROL for the case above: the same board and the same answerer, one
  -- card short of rule 702.52a's "at least N cards in your library", which rule
  -- 702.52b is the other half of. Pawl.Engine.Replacement.stocked keeps the row
  -- out of CR 616.1's offer entirely, so the draw happens as proposed.
  Spec.it s "CR 702.52b a library short of three is not offered the dredge at all" $ do
    board <- dredgeBoard s registry 2
    let after = S.runPure dredging board (Event.drawCard S.alice)
    Spec.assertEqWith s "CR 702.52b the Darkblast stayed in the graveyard" (graveyardNames S.alice after) [CardName.MkCardName (Text.pack "Darkblast")]
    Spec.assertEqWith s "CR 121.1 so alice drew her card" (S.handSize S.alice after) 1
    Spec.assertEqWith s "and only that one card left her library" (length (Game.zoneMembers Zone.Library S.alice after)) 1
  -- The other CONTROL: rule 702.52a's "you MAY". Same board as the first case,
  -- and only the answer differs -- S.identityAnswer declines every optional
  -- decision -- so the draw is left standing and nothing is milled.
  Spec.it s "CR 702.52a declining leaves the draw standing" $ do
    board <- dredgeBoard s registry 4
    let after = S.runPure S.identityAnswer board (Event.drawCard S.alice)
    Spec.assertEqWith s "CR 702.52a the Darkblast stayed in the graveyard" (graveyardNames S.alice after) [CardName.MkCardName (Text.pack "Darkblast")]
    Spec.assertEqWith s "CR 121.1 alice drew her card" (S.handSize S.alice after) 1
    Spec.assertEqWith s "and nothing was milled" (length (Game.zoneMembers Zone.Library S.alice after)) 3

-- Alice's graveyard holding one Darkblast, and her library `n` basics of
-- distinct names. Distinct names so a milled card cannot be mistaken for
-- another, and basics so nothing in the library carries a row of its own.
dredgeBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Int -> m GameState.GameState
dredgeBoard s registry n = do
  darkblast <- S.printingOf s registry "Darkblast"
  basics <- Monad.mapM (S.printingOf s registry) (take n ["Island", "Mountain", "Forest", "Plains"])
  let (_, g1) = S.addGraveyardCard darkblast S.alice (Setup.emptyGame S.bothPlayers)
  pure (List.foldl' (\g b -> snd (S.addLibraryCard b S.alice g)) g1 basics)

-- Exercises the dredge and nothing else, pinned rather than searched: every
-- other decision falls through to S.identityAnswer, which declines.
dredging :: Prompt.Prompt r -> r
dredging p = case p of
  Prompt.ChooseDredge {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- graveyardNames one zone over.
handNames :: PlayerId.PlayerId -> GameState.GameState -> [CardName.CardName]
handNames pid gs = List.sort (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers Zone.Hand pid gs))
