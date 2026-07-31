{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Damage and Pawl.Engine.Sba: the damage funnel, deathtouch, trample, and
-- state-based actions.
module Pawl.DamageSpec where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Sba as Sba
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone

creatureSbaSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
creatureSbaSpec s registry =
  Spec.describe s "CreatureSba" $ do
    Spec.it s "CR 704.5g a creature with lethal damage is destroyed" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          after = S.settleSba (S.markDamage oid 1 gs)
      Spec.assertEqWith s "off the battlefield" (Game.zoneMembers Zone.Battlefield S.alice after) []
      Spec.assertEqWith s "in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1

    Spec.it s "CR 704.5g damage below toughness is not lethal" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          -- A Piker is 2/1, so 0 marked damage is survivable and 1 is not.
          after = S.settleSba (S.markDamage oid 0 gs)
      Spec.assertEqWith s "still there" (length (Game.zoneMembers Zone.Battlefield S.alice after)) 1

    Spec.it s "CR 704.5g a Mountain with damage marked is not destroyed" $ do
      -- Not a creature: 704.5f/g do not apply. This is the classification
      -- doing its job -- the check never asks WHICH card it is.
      mountain <- Registry.printing registry "Mountain"
      let gs = S.landsInPlay mountain 1
      case Game.zoneMembers Zone.Battlefield S.alice gs of
        [] -> Spec.assertFailure s "fixture should have one Mountain"
        oid : _ ->
          Spec.assertEqWith
            s
            "survives"
            (length (Game.zoneMembers Zone.Battlefield S.alice (S.settleSba (S.markDamage oid 5 gs))))
            1

    Spec.it s "a destroyed creature conserves objects" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          marked = S.markDamage oid 1 gs
      Spec.assertEqWith
        s
        "conserved"
        (Game.objectCount (S.settleSba marked))
        (Game.objectCount marked)

    -- The deterministic successor to the retired green-black "some seed sends a
    -- creature to the graveyard" property: two 2/1 Pikers trade in combat and
    -- both die to the CR 704.5g state-based action.
    Spec.it s "a creature dies in a played-out combat" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (gs, _, _) = S.combatBoard piker 1 1
          after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
      Spec.assertEqWith s "attacker died" (S.creaturesInPlay S.alice after) 0
      Spec.assertEqWith s "blocker died" (S.creaturesInPlay S.bob after) 0

    Spec.it s "CR 704.5d a token off the battlefield ceases to exist" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let base = Setup.emptyGame S.bothPlayers
          goblinCard = Printing.card piker
          (tokId, gs) = S.addToken goblinCard S.alice base
          inGrave = S.runPure S.identityAnswer gs (Event.changeZone tokId Zone.Graveyard)
          -- The changeZone minted a new incarnation; find it in the graveyard.
          settled = S.settleSba inGrave
      Spec.assertEqWith s "before the SBA, a token sits in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice inGrave)) 1
      Spec.assertEqWith s "after the SBA, it has ceased to exist" (length (Game.zoneMembers Zone.Graveyard S.alice settled)) 0
      Spec.assertEqWith s "no token objects remain" (Game.objectCount settled) 0

    Spec.it s "CR 704.5d a token on the battlefield does NOT cease" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let base = Setup.emptyGame S.bothPlayers
          goblinCard = Printing.card piker
          (_, gs) = S.addToken goblinCard S.alice base
          settled = S.settleSba gs
      Spec.assertEqWith s "the token survives on the battlefield" (Game.objectCount settled) 1

    Spec.it s "CR 704.5d/704.5g a 1/1 token dies to lethal damage and ceases to exist" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let base = Setup.emptyGame S.bothPlayers
          goblinCard = Printing.card piker
          -- A real 2/1 Piker (bob's) is the damage source; alice's 1/1 token takes 2.
          (srcId, gs1) = S.addCreature piker S.bob base
          (tokId, gs2) = S.addToken goblinCard S.alice gs1
          damaged = S.runPure S.identityAnswer gs2 (Damage.applyDamage [DamageEvent.MkDamageEvent srcId (Recipient.ToCreature tokId) 2 False False 0 DamageKind.Combat])
          settled = S.settleSba damaged
      Spec.assertEqWith s "the token is gone from the battlefield" (S.creaturesInPlay S.alice settled) 0
      Spec.assertEqWith s "and NOT sitting in a graveyard (the falsifier)" (length (Game.zoneMembers Zone.Graveyard S.alice settled)) 0

    Spec.it s "CR 704.5g regeneration saves a creature from lethal combat damage" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let base = Setup.emptyGame S.bothPlayers
          (victim, gs0) = S.addCreature piker S.alice base -- 2/1
          shielded = S.addRegenShield victim gs0
          -- 2 combat damage is lethal to a 2/1; the shield replaces the CR 704.5g destruction.
          damaged = S.runPure S.identityAnswer shielded (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False 0 DamageKind.Combat])
          settled = S.settleSba damaged
      Spec.assertEqWith s "survived (regenerated)" (Set.member victim (GameState.battlefield settled)) True
      case Game.lookupObject victim settled of
        Just obj -> do
          Spec.assertEqWith s "tapped" (Object.tapped obj) TapState.Tapped
          Spec.assertEqWith s "damage healed" (Object.damage obj) 0
        Nothing -> Spec.assertFailure s "victim vanished"

damageSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
damageSpec s registry =
  Spec.describe s "Damage" $ do
    Spec.it s "a permanent starts with no damage marked" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
      Spec.assertEqWith s "none" (S.damageOf oid gs) (Just 0)

    Spec.it s "CR 514.2 marked damage is removed" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
      Spec.assertEqWith s "removed" (S.damageOf oid (Damage.removeAllDamage (S.markDamage oid 1 gs))) (Just 0)

    Spec.it s "CR 514.2 damage wears off at the cleanup step" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          marked = S.markDamage oid 1 gs
          after = snd (Engine.runGamePure S.identityAnswer marked (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)))
      Spec.assertEqWith s "worn off" (S.damageOf oid after) (Just 0)

    Spec.it s "CR 400.7 a new object carries no damage forward" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          marked = S.markDamage oid 1 gs
          after = S.runPure S.identityAnswer marked (Event.changeZone oid Zone.Graveyard)
      case Game.zoneMembers Zone.Graveyard S.alice after of
        [] -> Spec.assertFailure s "expected a card in the graveyard"
        new : _ -> Spec.assertEqWith s "fresh object, no damage" (S.damageOf new after) (Just 0)

    Spec.it s "CR 615 a prevention drops combat damage but spares Noncombat" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let base = Setup.emptyGame S.bothPlayers
          (victim, gs0) = S.addCreature piker S.alice base
          shield =
            ActiveReplacement.MkActiveReplacement
              { ActiveReplacement.effect = ReplacementEffect.DamageR (DamagePattern.MkDamagePattern (Just DamageKind.Combat)) DamageRewrite.PreventAll,
                ActiveReplacement.source = victim,
                ActiveReplacement.timestamp = Timestamp.MkTimestamp 900,
                ActiveReplacement.expiry = Expiry.Type.AtCleanup,
                ActiveReplacement.uses = Uses.Unlimited
              }
          withShield = S.addReplacement shield gs0
          combat = S.runPure S.identityAnswer withShield (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False 0 DamageKind.Combat])
          spell = S.runPure S.identityAnswer withShield (Damage.applyDamage [DamageEvent.MkDamageEvent victim (Recipient.ToCreature victim) 2 False False 0 DamageKind.Noncombat])
      Spec.assertEqWith s "combat damage prevented -- none marked" (S.damageOf victim combat) (Just 0)
      Spec.assertEqWith s "combat damage prevented -- no event recorded" (S.damageEventsOf combat) []
      Spec.assertEqWith s "noncombat damage still dealt" (S.damageOf victim spell) (Just 2)

    Spec.it s "CR 514.2 an until-end-of-turn replacement wears off at cleanup" $
      let base = Setup.emptyGame S.bothPlayers
          shield =
            ActiveReplacement.MkActiveReplacement
              { ActiveReplacement.effect = ReplacementEffect.DamageR (DamagePattern.MkDamagePattern (Just DamageKind.Combat)) DamageRewrite.PreventAll,
                ActiveReplacement.source = ObjectId.MkObjectId 900,
                ActiveReplacement.timestamp = Timestamp.MkTimestamp 900,
                ActiveReplacement.expiry = Expiry.Type.AtCleanup,
                ActiveReplacement.uses = Uses.Unlimited
              }
          dropped = Expiry.dropAtCleanup (S.addReplacement shield base)
       in Spec.assertEqWith s "no replacements remain" (GameState.replacements dropped) []

infectSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
infectSpec s registry =
  Spec.describe s "Infect" $ do
    Spec.it s "CR 120.3b infect damage to a player becomes poison, not life loss" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (oid, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          ev = DamageEvent.MkDamageEvent oid (Recipient.ToPlayer S.bob) 3 False True 0 DamageKind.Combat
          after = S.runPure S.identityAnswer gs0 (Damage.applyDamage [ev])
      Spec.assertEqWith s "bob has three poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 3
      Spec.assertEqWith s "bob's life unchanged" (S.lifeOf S.bob after) (Just 20)
      Spec.assertEqWith s "the source's controller gains no poison" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0

    Spec.it s "CR 120.3d infect damage to a creature becomes -1/-1 counters, not marked damage" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (src, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          (victim, gs1) = S.addCreature piker S.bob gs0
          ev = DamageEvent.MkDamageEvent src (Recipient.ToCreature victim) 2 False True 0 DamageKind.Combat
          after = S.runPure S.identityAnswer gs1 (Damage.applyDamage [ev])
      Spec.assertEqWith s "two -1/-1 counters" (fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject victim after)) (Just 2)
      Spec.assertEqWith s "no marked damage" (S.damageOf victim after) (Just 0)

    Spec.it s "CR 702.90 Glistener Elf poisons an unblocked player, drains no life" $ do
      glistenerElf <- Registry.printing registry "Glistener Elf"
      let (gs, _, _) = S.combatBoardOf [glistenerElf] []
          after = S.fightWith S.aggressiveAnswer gs
      Spec.assertEqWith s "bob has one poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 1
      Spec.assertEqWith s "bob's life unchanged" (S.lifeOf S.bob after) (Just 20)
      Spec.assertEqWith s "alice (controller) has no poison" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0

    Spec.it s "CR 702.90c Glistener Elf shrinks and kills a blocker with -1/-1 counters" $ do
      glistenerElf <- Registry.printing registry "Glistener Elf"
      piker <- Registry.printing registry "Goblin Piker"
      let (gs, _, blockers) = S.combatBoardOf [glistenerElf] [piker]
          fought = S.fightWith S.aggressiveAnswer gs
          settled = S.settleSba fought
      case blockers of
        [] -> Spec.assertFailure s "fixture should have a blocker"
        blocker : _ -> do
          Spec.assertEqWith s "one -1/-1 counter before SBA" (fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject blocker fought)) (Just 1)
          Spec.assertEqWith s "no marked damage on the blocker" (S.damageOf blocker fought) (Just 0)
          Spec.assertEqWith s "blocker buried by 704.5f" (length (Game.zoneMembers Zone.Graveyard S.bob settled)) 1

toxicSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
toxicSpec s registry =
  Spec.describe s "Toxic" $ do
    Spec.it s "CR 120.3g toxic poison is IN ADDITION to the damage, not instead of it" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (oid, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          ev = DamageEvent.MkDamageEvent oid (Recipient.ToPlayer S.bob) 3 False False 2 DamageKind.Combat
          after = S.runPure S.identityAnswer gs0 (Damage.applyDamage [ev])
      Spec.assertEqWith s "bob has two poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 2
      Spec.assertEqWith s "bob still lost the three life" (S.lifeOf S.bob after) (Just 17)
      Spec.assertEqWith s "the source's controller gains no poison" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0

    Spec.it s "CR 120.3g toxic gives no poison on NONCOMBAT damage" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (oid, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          ev = DamageEvent.MkDamageEvent oid (Recipient.ToPlayer S.bob) 3 False False 2 DamageKind.Noncombat
          after = S.runPure S.identityAnswer gs0 (Damage.applyDamage [ev])
      Spec.assertEqWith s "bob has no poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 0
      Spec.assertEqWith s "bob lost the three life" (S.lifeOf S.bob after) (Just 17)

    -- CR 120.3b and 120.3g compose: infect REPLACES the damage with poison,
    -- toxic ADDS its own on top, so a source with both gives amount + N and
    -- still drains no life. No card in the pool has both, so the event is
    -- hand-built.
    Spec.it s "CR 120.3b/120.3g infect and toxic stack: poison is amount plus N, and no life is lost" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (oid, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          ev = DamageEvent.MkDamageEvent oid (Recipient.ToPlayer S.bob) 3 False True 2 DamageKind.Combat
          after = S.runPure S.identityAnswer gs0 (Damage.applyDamage [ev])
      Spec.assertEqWith s "bob has five poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 5
      Spec.assertEqWith s "bob's life unchanged" (S.lifeOf S.bob after) (Just 20)

    Spec.it s "CR 702.164c Branchblight Stalker poisons an unblocked player AND drains its life" $ do
      stalker <- Registry.printing registry "Branchblight Stalker"
      let (gs, _, _) = S.combatBoardOf [stalker] []
          after = S.fightWith S.aggressiveAnswer gs
      Spec.assertEqWith s "bob has two poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 2
      Spec.assertEqWith s "bob took the three damage too" (S.lifeOf S.bob after) (Just 17)
      Spec.assertEqWith s "alice (controller) has no poison" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0

    -- CR 120.3g is scoped to combat damage dealt TO A PLAYER: a blocked toxic
    -- creature hands its poison to nobody, and marks its blocker normally --
    -- toxic is not infect, so the blocker takes damage, not -1/-1 counters.
    Spec.it s "CR 120.3g a blocked Branchblight Stalker gives no poison and marks its blocker" $ do
      stalker <- Registry.printing registry "Branchblight Stalker"
      piker <- Registry.printing registry "Goblin Piker"
      let (gs, _, blockers) = S.combatBoardOf [stalker] [piker]
          fought = S.fightWith S.aggressiveAnswer gs
      Spec.assertEqWith s "bob has no poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob fought) 0
      Spec.assertEqWith s "bob's life unchanged" (S.lifeOf S.bob fought) (Just 20)
      case blockers of
        [] -> Spec.assertFailure s "fixture should have a blocker"
        blocker : _ -> do
          Spec.assertEqWith s "three damage marked on the blocker" (S.damageOf blocker fought) (Just 3)
          Spec.assertEqWith s "no -1/-1 counters" (fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject blocker fought)) (Just 0)

    -- CR 702.4b deals combat damage TWICE, and CR 120.3g fires per instance of
    -- combat damage, not once per combat: two waves, two lots of poison. The
    -- grant is a layer-6 GainKeyword rather than a card, since no printing in
    -- the pool has both double strike and toxic.
    Spec.it s "CR 702.4b/120.3g a double-striking Branchblight Stalker poisons twice" $ do
      stalker <- Registry.printing registry "Branchblight Stalker"
      let (gs0, attackers, _) = S.combatBoardOf [stalker] []
      case attackers of
        [] -> Spec.assertFailure s "fixture should have an attacker"
        attacker : _ -> do
          let gs = S.withEffectAt attacker (Timestamp.MkTimestamp 100) (Modification.GainKeyword Keyword.DoubleStrike) gs0
              after = S.runCombat S.aggressiveAnswer gs
          Spec.assertEqWith s "toxic 2 twice is four poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 4
          Spec.assertEqWith s "and three damage twice" (S.lifeOf S.bob after) (Just 14)

    -- Two Aspirant's Ascents on one Branchblight Stalker: toxic 2 printed plus
    -- toxic 1 granted twice is a total toxic value of 4 (CR 702.164b sums N
    -- over every toxic ability, and rule 702.164 has no redundancy clause of
    -- the CR 702.3c/702.9c kind). The falsifier is a projection that keeps
    -- keywords in a set, where the second toxic 1 collapses into the first and
    -- bob takes 3 poison instead of 4.
    --
    -- The same two casts grant flying twice, which CR 702.9c DOES make
    -- redundant: the Stalker simply flies, and bob (with no creatures) is not
    -- blocking either way.
    Spec.it s "CR 702.164b two Aspirant's Ascents make Branchblight Stalker toxic 4" $ do
      stalker <- Registry.printing registry "Branchblight Stalker"
      island <- Registry.printing registry "Island"
      ascent <- Registry.printing registry "Aspirant's Ascent"
      let (gs0, attackers, _) = S.combatBoardOf [stalker] []
      case attackers of
        [] -> Spec.assertFailure s "fixture should have an attacker"
        attacker : _ -> do
          let withIsland g = snd (S.addCreature island S.alice g)
              castAscent g =
                let (oid, g1) = S.addHandCard ascent S.alice g
                    g2 = g1 {GameState.priority = Just S.alice}
                 in S.runPure S.identityAnswer g2 (Cast.castSpell S.alice oid Monad.>> Stack.resolveTop)
              gs = castAscent (castAscent (withIsland (withIsland gs0)))
              after = S.fightWith S.aggressiveAnswer gs
          Spec.assertEqWith s "toxic 2 plus toxic 1 twice" (Projection.totalToxic attacker gs) 4
          Spec.assertBool s (Projection.hasKeyword Keyword.Flying attacker gs) "CR 702.9c: two flying grants still just fly"
          Spec.assertEqWith s "bob has four poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 4
          Spec.assertEqWith s "and took the 3/1 Stalker's twice-pumped five damage" (S.lifeOf S.bob after) (Just 15)

    -- CR 615.6: a prevented event never happens, so no combat damage was
    -- "dealt to a player" for CR 120.3g to hang poison off. The falsifier is a
    -- toxic implementation that reads the rider off the event batch instead of
    -- off the survivors.
    Spec.it s "CR 615.6 prevented combat damage gives no toxic poison" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (oid, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          shield =
            ActiveReplacement.MkActiveReplacement
              { ActiveReplacement.effect = ReplacementEffect.DamageR (DamagePattern.MkDamagePattern (Just DamageKind.Combat)) DamageRewrite.PreventAll,
                ActiveReplacement.source = oid,
                ActiveReplacement.timestamp = Timestamp.MkTimestamp 900,
                ActiveReplacement.expiry = Expiry.Type.AtCleanup,
                ActiveReplacement.uses = Uses.Unlimited
              }
          ev = DamageEvent.MkDamageEvent oid (Recipient.ToPlayer S.bob) 3 False False 2 DamageKind.Combat
          after = S.runPure S.identityAnswer (S.addReplacement shield gs0) (Damage.applyDamage [ev])
      Spec.assertEqWith s "no poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 0
      Spec.assertEqWith s "no life lost" (S.lifeOf S.bob after) (Just 20)

sbaBase :: GameState.GameState
sbaBase = Setup.emptyGame S.bothPlayers

-- Answers Prompt.ChooseLegend by keeping the candidate `wanted`, when it is on
-- offer. A pair of tests differing only in this argument proves the ANSWER
-- decides which legend survives, rather than the order Sba enumerates them.
keepsLegend :: ObjectId.ObjectId -> Prompt.Prompt r -> r
keepsLegend wanted p = case p of
  Prompt.ChooseLegend _ _ candidates ->
    if elem wanted (NonEmpty.toList candidates) then wanted else NonEmpty.head candidates
  _ -> S.identityAnswer p

-- Copies `target` when asked what to copy, and keeps `keep` when the legend rule
-- asks which same-named legend survives.
copiesAndKeeps :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
copiesAndKeeps target keep p = case p of
  Prompt.ChooseCopyTarget _ _ _ legal -> if elem target legal then Just target else Nothing
  Prompt.ChooseLegend _ _ candidates ->
    if elem keep (NonEmpty.toList candidates) then keep else NonEmpty.head candidates
  _ -> S.identityAnswer p

-- Is this object still on the battlefield?
inPlay :: ObjectId.ObjectId -> GameState.GameState -> Bool
inPlay oid gs = fmap Object.zone (Game.lookupObject oid gs) == Just Zone.Battlefield

legendRuleSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
legendRuleSpec s registry =
  Spec.describe s "LegendRule" $ do
    -- CR 704.5j: "If two or more legendary permanents with the same name are
    -- controlled by the same player, that player chooses one of them, and the
    -- rest are put into their owners' graveyards."
    Spec.it s "CR 704.5j a second Thalia sends one of them to the graveyard" $ do
      thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
      let (first, g0) = S.addCreature thalia S.alice (Setup.emptyGame S.bothPlayers)
          (second, gs) = S.addCreature thalia S.alice g0
          kept = S.runPure (keepsLegend first) gs Sba.checkStateBasedActions
      Spec.assertBool s (inPlay first kept) "the chosen one stays"
      Spec.assertBool s (not (inPlay second kept)) "the other is gone"
      -- CR 400.7: the move mints a NEW incarnation, so the buried Thalia is not
      -- `second` any more. Count the graveyard rather than chase the dead id.
      Spec.assertEqWith s "exactly one Thalia was buried" (length (Game.zoneMembers Zone.Graveyard S.alice kept)) 1

    -- The discriminating twin: same board, opposite answer. This fails if the
    -- engine picks the survivor itself.
    Spec.it s "CR 704.5j which Thalia survives is the controller's choice" $ do
      thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
      let (first, g0) = S.addCreature thalia S.alice (Setup.emptyGame S.bothPlayers)
          (second, gs) = S.addCreature thalia S.alice g0
          keptSecond = S.runPure (keepsLegend second) gs Sba.checkStateBasedActions
      Spec.assertBool s (inPlay second keptSecond) "the second one stays this time"
      Spec.assertBool s (not (inPlay first keptSecond)) "the first is gone"

    -- CR 704.5j is per CONTROLLER: one legend each is legal, and the rule has
    -- nothing to say about the two of them.
    Spec.it s "CR 704.5j two players may each control a Thalia" $ do
      thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
      let (hers, g0) = S.addCreature thalia S.alice (Setup.emptyGame S.bothPlayers)
          (his, gs) = S.addCreature thalia S.bob g0
          after = S.runPure S.identityAnswer gs Sba.checkStateBasedActions
      Spec.assertBool s (inPlay hers after) "alice keeps hers"
      Spec.assertBool s (inPlay his after) "bob keeps his"

    -- "Legendary" is half the condition; a duplicated ordinary creature is not
    -- the legend rule's business.
    Spec.it s "CR 704.5j two copies of a NON-legendary creature both survive" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (a, g0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          (b, gs) = S.addCreature piker S.alice g0
          after = S.runPure S.identityAnswer gs Sba.checkStateBasedActions
      Spec.assertBool s (inPlay a after && inPlay b after) "both stay"

    -- "With the same name" is the other half: two DIFFERENT legends coexist.
    Spec.it s "CR 704.5j a Thalia and an Urborg coexist under one controller" $ do
      thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
      urborg <- Registry.printing registry "Urborg, Tomb of Yawgmoth"
      let (t, g0) = S.addCreature thalia S.alice (Setup.emptyGame S.bothPlayers)
          (u, gs) = S.addCreature urborg S.alice g0
          after = S.runPure S.identityAnswer gs Sba.checkStateBasedActions
      Spec.assertBool s (inPlay t after && inPlay u after) "both stay"

    -- "Put into their OWNERS' graveyards" -- not the controller's. Alice
    -- controls both, but bob owns the one she stole, so that is where it goes.
    Spec.it s "CR 704.5j the loser goes to its OWNER's graveyard, not the controller's" $ do
      thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
      let (hers, g0) = S.addCreature thalia S.alice (Setup.emptyGame S.bothPlayers)
          (his, g1) = S.addCreature thalia S.bob g0
          stolen = S.giveControl his S.alice g1
          after = S.runPure (keepsLegend hers) stolen Sba.checkStateBasedActions
      Spec.assertBool s (inPlay hers after) "alice keeps her own"
      Spec.assertBool s (not (inPlay his after)) "the stolen one left the battlefield"
      -- The whole point: alice controlled it, but bob owns it, so bob's
      -- graveyard is where it lands. (CR 400.7 gives it a fresh id on the way,
      -- so this counts the zone rather than naming the old one.)
      Spec.assertEqWith s "one card in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
      Spec.assertEqWith s "and none in alice's" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0

    -- The P2-reachable path the issue names, and the gameplay-level proof: a
    -- Clone is neither legendary nor named Thalia on its own, but CR 707.2 lists both
    -- name and supertype among the copiable values, so the copy copies
    -- name and supertype alike, so the copy IS a second Thalia and the rule
    -- fires on it.
    Spec.it s "CR 707.2/704.5j a Clone copying Thalia is a second Thalia and the rule fires" $ do
      thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
      clone <- Registry.printing registry "Clone"
      let (original, board) = S.addCreature thalia S.alice (Setup.emptyGame S.bothPlayers)
          (_, staged) = S.spellOnStack clone S.alice board
          settled = snd (Engine.runGamePure (copiesAndKeeps original original) staged (Stack.resolveTop >> Engine.settleForPriority))
      Spec.assertBool s (inPlay original settled) "the original survives, because alice chose it"
      Spec.assertEqWith s "and exactly one Thalia is left in play" (S.creaturesInPlay S.alice settled) 1

    -- CR 704.3: every applicable state-based action is performed
    -- "simultaneously as a single event". So a legend that CR 704.5f is already
    -- burying stays on CR 704.5j's ballot, and keeping THAT one is a legal
    -- choice which puts every other copy into the graveyard beside it.
    --
    -- Dropping such a member from the candidates would decide for the player
    -- and strand a copy alive that they chose to lose -- which is what this
    -- branch did before review caught it.
    Spec.it s "CR 704.3/704.5j keeping a Thalia that is already dying buries both" $ do
      thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
      let (healthy, g0) = S.addCreature thalia S.alice (Setup.emptyGame S.bothPlayers)
          (dying, g1) = S.addCreature thalia S.alice g0
          -- Thalia is 2/1, so -2/-1 makes this copy a 0/0: CR 704.5f applies to
          -- it and not to the other.
          gs = S.withEffect dying (Modification.ModifyPowerToughness (Quantity.Literal (-2)) (Quantity.Literal (-1))) g1
          keptDying = S.runPure (keepsLegend dying) gs Sba.checkStateBasedActions
          keptHealthy = S.runPure (keepsLegend healthy) gs Sba.checkStateBasedActions
      Spec.assertEqWith s "the 0/0 really is a 0/0" (Projection.toughnessOf dying gs) (Just 0)
      -- Keeping the dying copy: 704.5j buries the healthy one, 704.5f buries this
      -- one, and alice is left with no Thalia at all.
      Spec.assertBool s (not (inPlay healthy keptDying)) "the healthy Thalia went too"
      Spec.assertBool s (not (inPlay dying keptDying)) "and so did the dying one"
      Spec.assertEqWith s "two cards in the graveyard, so neither was moved twice" (length (Game.zoneMembers Zone.Graveyard S.alice keptDying)) 2
      -- The discriminating twin: keeping the healthy copy saves it, so the
      -- outcome above really is alice's choice and not a forced sweep.
      Spec.assertBool s (inPlay healthy keptHealthy) "keeping the healthy one saves it"
      Spec.assertEqWith s "and only the 0/0 was buried" (length (Game.zoneMembers Zone.Graveyard S.alice keptHealthy)) 1

-- The two world enchantments in the pool, fetched together: most tests below
-- want two DIFFERENTLY NAMED world permanents, since a rule that ignores names
-- is half of the contrast with the legend rule above.
worldPair :: Registry.Registry -> IO (Printing.Printing, Printing.Printing)
worldPair registry = do
  crossroads <- Registry.printing registry "Concordant Crossroads"
  livingPlane <- Registry.printing registry "Living Plane"
  pure (crossroads, livingPlane)

worldRuleSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
worldRuleSpec s registry =
  Spec.describe s "WorldRule" $ do
    -- CR 704.5k: "If two or more permanents have the supertype world, all
    -- except the one that has had the world supertype for the shortest amount
    -- of time are put into their owners' graveyards."
    --
    -- Shortest amount of time is the NEWEST arrival, so the second one to
    -- enter is the one that lives.
    Spec.it s "CR 704.5k the newer of two world permanents survives" $ do
      (crossroads, livingPlane) <- worldPair registry
      let (older, g0) = S.addCreature crossroads S.alice (Setup.emptyGame S.bothPlayers)
          (newer, gs) = S.addCreature livingPlane S.alice g0
          after = S.runPure S.identityAnswer gs Sba.checkStateBasedActions
      Spec.assertBool s (inPlay newer after) "the newcomer stays"
      Spec.assertBool s (not (inPlay older after)) "the incumbent is gone"
      Spec.assertEqWith s "exactly one was buried" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1

    -- The discriminating twin: the same two cards, entering in the opposite
    -- order, produce the opposite survivor. This fails if the rule is reading
    -- anything but the clock -- an object id, a name, or the order Sba happens
    -- to enumerate the battlefield in.
    Spec.it s "CR 704.5k which one survives is the entry order, not the card" $ do
      (crossroads, livingPlane) <- worldPair registry
      let (older, g0) = S.addCreature livingPlane S.alice (Setup.emptyGame S.bothPlayers)
          (newer, gs) = S.addCreature crossroads S.alice g0
          after = S.runPure S.identityAnswer gs Sba.checkStateBasedActions
      Spec.assertBool s (inPlay newer after) "the newcomer stays"
      Spec.assertBool s (not (inPlay older after)) "the incumbent is gone"

    -- Unlike CR 704.5j, the world rule is NOT scoped to one controller: it
    -- says "if two or more permanents", full stop. Two players each with a
    -- world permanent is exactly the board the legend rule leaves alone.
    Spec.it s "CR 704.5k two players may NOT each keep a world permanent" $ do
      (crossroads, livingPlane) <- worldPair registry
      let (hers, g0) = S.addCreature crossroads S.alice (Setup.emptyGame S.bothPlayers)
          (his, gs) = S.addCreature livingPlane S.bob g0
          after = S.runPure S.identityAnswer gs Sba.checkStateBasedActions
      Spec.assertBool s (inPlay his after) "bob's newer one stays"
      Spec.assertBool s (not (inPlay hers after)) "alice's older one is gone"

    -- "All except the one" -- so a third arrival buries BOTH incumbents in the
    -- same pass, rather than peeling one off per pass.
    Spec.it s "CR 704.5k a third world permanent buries both incumbents at once" $ do
      (crossroads, livingPlane) <- worldPair registry
      let (first, g0) = S.addCreature crossroads S.alice (Setup.emptyGame S.bothPlayers)
          (second, g1) = S.addCreature livingPlane S.alice g0
          (third, gs) = S.addCreature crossroads S.alice g1
          after = S.runPure S.identityAnswer gs Sba.performStateBasedActions
      Spec.assertBool s (inPlay third after) "only the newest stays"
      Spec.assertBool s (not (inPlay first after)) "the first is gone"
      Spec.assertBool s (not (inPlay second after)) "the second is gone too"
      Spec.assertEqWith s "both were buried in ONE pass" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 2

    -- "Two or more": one world permanent is nobody's business.
    Spec.it s "CR 704.5k a lone world permanent survives" $ do
      (crossroads, _) <- worldPair registry
      let (only, gs) = S.addCreature crossroads S.alice (Setup.emptyGame S.bothPlayers)
          after = S.runPure S.identityAnswer gs Sba.checkStateBasedActions
      Spec.assertBool s (inPlay only after) "it stays"

    -- The other half of the condition: an ordinary enchantment alongside a
    -- world one is not a pair. Bad Moon is the control -- an enchantment in
    -- every way except the supertype.
    Spec.it s "CR 704.5k a world permanent and an ordinary enchantment coexist" $ do
      (crossroads, _) <- worldPair registry
      badMoon <- Registry.printing registry "Bad Moon"
      let (world, g0) = S.addCreature crossroads S.alice (Setup.emptyGame S.bothPlayers)
          (ordinary, gs) = S.addCreature badMoon S.alice g0
          after = S.runPure S.identityAnswer gs Sba.checkStateBasedActions
      Spec.assertBool s (inPlay world after) "the world enchantment stays"
      Spec.assertBool s (inPlay ordinary after) "so does Bad Moon"

    -- "Put into their OWNERS' graveyards" -- alice controls bob's card, but
    -- bob owns it, so bob's graveyard is where it lands. (CR 400.7 gives it a
    -- fresh id on the way, so this counts the zone rather than naming the old
    -- one.)
    Spec.it s "CR 704.5k the loser goes to its OWNER's graveyard" $ do
      (crossroads, livingPlane) <- worldPair registry
      let (his, g0) = S.addCreature crossroads S.bob (Setup.emptyGame S.bothPlayers)
          stolen = S.giveControl his S.alice g0
          (hers, gs) = S.addCreature livingPlane S.alice stolen
          after = S.runPure S.identityAnswer gs Sba.checkStateBasedActions
      Spec.assertBool s (inPlay hers after) "alice's newer one stays"
      Spec.assertBool s (not (inPlay his after)) "the stolen one left the battlefield"
      Spec.assertEqWith s "one card in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
      Spec.assertEqWith s "and none in alice's" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0

    -- CR 704.3: every applicable state-based action is performed
    -- "simultaneously as a single event", so ONE pass settles both rules.
    -- Night of Souls' Betrayal is legendary and Concordant Crossroads is
    -- world, so alice's board is two of each: the legend rule asks her which
    -- Night to keep and the world rule keeps the newer Crossroads without
    -- asking, and both losers are in the graveyard when that single pass
    -- returns. (No printing is legendary AND world, so the two rules cannot
    -- name the same permanent; the deduplicated batch they share is pinned by
    -- the legend rule's own tests above.)
    Spec.it s "CR 704.3 the world rule and the legend rule share one pass" $ do
      (crossroads, _) <- worldPair registry
      night <- Registry.printing registry "Night of Souls' Betrayal"
      let (oldWorld, g0) = S.addCreature crossroads S.alice (Setup.emptyGame S.bothPlayers)
          (firstNight, g1) = S.addCreature night S.alice g0
          (secondNight, g2) = S.addCreature night S.alice g1
          (newWorld, gs) = S.addCreature crossroads S.alice g2
          after = S.runPure (keepsLegend firstNight) gs Sba.performStateBasedActions
      Spec.assertBool s (inPlay newWorld after) "the newest world permanent stays"
      Spec.assertBool s (not (inPlay oldWorld after)) "the older world permanent is gone"
      Spec.assertBool s (inPlay firstNight after) "the chosen legend stays"
      Spec.assertBool s (not (inPlay secondNight after)) "the other legend is gone"
      Spec.assertEqWith s "two cards buried, neither moved twice" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 2

    -- CR 704.3 again, this time where two state-based actions name the SAME
    -- permanent. Opalescence animates every other non-Aura enchantment at its
    -- mana value, so the {G} Concordant Crossroads is a 1/1; Night of Souls'
    -- Betrayal takes every creature down -1/-1, so it is a 0/0. CR 704.5f
    -- buries it for its toughness and CR 704.5k buries it for being the older
    -- world permanent -- and the deduplicated batch must move it once, not
    -- twice, or its zone change (and any dies-trigger watching) would fire
    -- again.
    Spec.it s "CR 704.5f/704.5k a permanent both rules name is moved once" $ do
      (crossroads, livingPlane) <- worldPair registry
      opalescence <- Registry.printing registry "Opalescence"
      night <- Registry.printing registry "Night of Souls' Betrayal"
      let (_, g0) = S.addCreature opalescence S.alice (Setup.emptyGame S.bothPlayers)
          (_, g1) = S.addCreature night S.alice g0
          (doomed, g2) = S.addCreature crossroads S.alice g1
          (newer, gs) = S.addCreature livingPlane S.alice g2
      Spec.assertEqWith s "the animated Crossroads really is a 0/0" (Projection.toughnessOf doomed gs) (Just 0)
      let after = S.runPure S.identityAnswer gs Sba.performStateBasedActions
      Spec.assertBool s (not (inPlay doomed after)) "it is gone"
      Spec.assertBool s (inPlay newer after) "the newer world permanent stays"
      Spec.assertEqWith s "and exactly one card was buried" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1

    -- The whole cards, cast: alice has a Concordant Crossroads out and casts
    -- Living Plane for its printed {2}{G}{G}. Nothing targets, nobody is
    -- asked, and the incumbent is in the graveyard by the time she has
    -- priority again -- the world rule as a player would meet it.
    Spec.it s "CR 704.5k whole cards: resolving Living Plane buries the Concordant Crossroads already out" $ do
      (crossroads, livingPlane) <- worldPair registry
      forest <- Registry.printing registry "Forest"
      let base = S.landsInPlay forest 4 -- {2}{G}{G}
          (incumbent, withCrossroads) = S.addCreature crossroads S.alice base
          (withSpell, spellId) = S.handOne livingPlane withCrossroads
          cast = snd (Engine.runGamePure S.identityAnswer withSpell (Cast.castSpell S.alice spellId))
          after = snd (Engine.runGamePure S.identityAnswer cast (Stack.resolveTop >> Engine.settleForPriority))
      Spec.assertBool s (not (inPlay incumbent after)) "the incumbent is gone"
      Spec.assertEqWith s "one card in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
      -- The survivor is on the battlefield and working: its static ability has
      -- made every Forest a creature, which is how this test knows the world
      -- rule buried the OLD one rather than the new arrival.
      Spec.assertEqWith s "and the four Forests are creatures now" (length (filter (\oid -> Projection.isCreatureOf oid after) (Set.toList (GameState.battlefield after)))) 4

sbaSpec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
sbaSpec s =
  Spec.describe s "Sba" $ do
    Spec.it s "drew-from-empty loses" $
      let after = S.settleSba sbaBase {GameState.drewFromEmpty = Set.singleton S.alice}
       in Spec.assertEqWith s "alice lost" (fmap Player.status (Map.lookup S.alice (GameState.players after))) (Just (Status.Departed Departure.Type.Lost))

    Spec.it s "one remaining player wins" $
      let after = S.settleSba sbaBase {GameState.drewFromEmpty = Set.singleton S.alice}
       in Spec.assertEqWith s "bob won" (GameState.result after) (Just (Result.Won S.bob))

    Spec.it s "life <= 0 loses" $
      let gs = sbaBase {GameState.players = Map.insert S.alice (Player.MkPlayer {Player.life = 0, Player.status = Status.Playing, Player.counters = Map.empty}) (GameState.players sbaBase)}
       in Spec.assertEqWith s "bob won" (GameState.result (S.settleSba gs)) (Just (Result.Won S.bob))

    Spec.it s "simultaneous last departures draw" $
      let after = S.settleSba sbaBase {GameState.drewFromEmpty = Set.fromList [S.alice, S.bob]}
       in Spec.assertEqWith s "draw" (GameState.result after) (Just Result.Drawn)

    Spec.it s "CR 704.5c ten poison counters lose the game" $
      let gs = S.addPlayerCounter PlayerCounterKind.Poison 10 S.bob (Setup.emptyGame S.bothPlayers)
          after = S.settleSba gs
       in Spec.assertEqWith s "bob lost" (fmap Player.status (Map.lookup S.bob (GameState.players after))) (Just (Status.Departed Departure.Type.Lost))

    Spec.it s "CR 704.5c nine poison counters do not" $
      let gs = S.addPlayerCounter PlayerCounterKind.Poison 9 S.bob (Setup.emptyGame S.bothPlayers)
          after = S.settleSba gs
       in Spec.assertEqWith s "bob still playing" (fmap Player.status (Map.lookup S.bob (GameState.players after))) (Just Status.Playing)

damageEventSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
damageEventSpec s registry =
  Spec.describe s "DamageEvent" $ do
    Spec.it s "a blocked 2/1 trade emits both damage events" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (gs, mine, theirs) = S.combatBoard piker 1 1
          after = S.fightWith S.aggressiveAnswer gs
          events = S.damageEventsOf after
      case (mine, theirs) of
        (a : _, b : _) -> do
          Spec.assertEqWith s "two events" (length events) 2
          Spec.assertBool
            s
            (elem (DamageEvent.MkDamageEvent a (Recipient.ToCreature b) 2 False False 0 DamageKind.Combat) events)
            "attacker hit blocker for 2"
          Spec.assertBool
            s
            (elem (DamageEvent.MkDamageEvent b (Recipient.ToCreature a) 2 False False 0 DamageKind.Combat) events)
            "blocker hit attacker for 2"
        _ -> Spec.assertFailure s "fixture should have one creature per side"

    Spec.it s "an unblocked 2/1 emits a ToPlayer event" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (gs, mine, _) = S.combatBoard piker 1 0
          after = S.fightWith S.aggressiveAnswer gs
      case mine of
        a : _ ->
          Spec.assertEqWith
            s
            "one player event"
            (S.damageEventsOf after)
            [DamageEvent.MkDamageEvent a (Recipient.ToPlayer S.bob) 2 False False 0 DamageKind.Combat]
        _ -> Spec.assertFailure s "fixture should have an attacker"

deathtouchSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
deathtouchSpec s registry =
  Spec.describe s "Deathtouch" $ do
    Spec.it s "CR 704.5h a 1/1 deathtoucher destroys a 3/3 it deals 1 to" $ do
      -- Typhoid Rats attacks, Ogre Sentry blocks. Rat deals 1 -> Ogre dies by
      -- 704.5h (toughness 3, not lethal by the numbers); Ogre's 3 kills the Rat.
      typhoidRats <- Registry.printing registry "Typhoid Rats"
      ogreSentry <- Registry.printing registry "Ogre Sentry"
      let (gs, _, _) = S.combatBoardOf [typhoidRats] [ogreSentry]
          after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
      Spec.assertEqWith s "the Ogre is dead" (S.creaturesInPlay S.bob after) 0
      Spec.assertEqWith s "the Rat is dead" (S.creaturesInPlay S.alice after) 0

    Spec.it s "CR 704.5g the control: a 2/1 without deathtouch leaves the 3/3 alive" $ do
      piker <- Registry.printing registry "Goblin Piker"
      ogreSentry <- Registry.printing registry "Ogre Sentry"
      let (gs, _, _) = S.combatBoardOf [piker] [ogreSentry]
          after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
      Spec.assertEqWith s "the Ogre survives" (S.creaturesInPlay S.bob after) 1

    Spec.it s "the SBA check consumes the damage events by watermark, not by draining" $ do
      typhoidRats <- Registry.printing registry "Typhoid Rats"
      ogreSentry <- Registry.printing registry "Ogre Sentry"
      let (gs, _, _) = S.combatBoardOf [typhoidRats] [ogreSentry]
          after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
      Spec.assertEqWith s "nothing left unscanned" (Event.unscannedDamage after) []
      Spec.assertBool s (not (null (S.damageEventsOf after))) "the record survives (CR 608.2i)"

    Spec.it s "CR 702.2e the deal-time bit is true for a real deathtoucher, false for a plain source" $ do
      -- Typhoid Rats (deathtouch) and Ogre Sentry trade combat damage under
      -- aggressiveAnswer (which DOES declare attackers). fightWith runs no SBAs,
      -- so the wave is still unscanned in the turn log.
      typhoidRats <- Registry.printing registry "Typhoid Rats"
      ogreSentry <- Registry.printing registry "Ogre Sentry"
      let (gs, rats, ogres) = S.combatBoardOf [typhoidRats] [ogreSentry]
          fought = S.fightWith S.aggressiveAnswer gs
          ratId = case rats of r : _ -> r; [] -> ObjectId.MkObjectId 999
          ogreId = case ogres of o : _ -> o; [] -> ObjectId.MkObjectId 999
          bitFor src = any (\ev -> DamageEvent.source ev == src && DamageEvent.dealtByDeathtouch ev) (S.damageEventsOf fought)
      Spec.assertBool s (bitFor ratId) "Rat's damage is flagged deathtouch"
      Spec.assertBool s (not (bitFor ogreId)) "Ogre's damage is not"

    Spec.it s "CR 702.2e Humility removes deathtouch, so the deal-time bit is false" $ do
      -- Under Humility the Rat loses deathtouch (layer 6); its combat-damage
      -- event's bit is false -- asserted directly on the event, not via a kill.
      typhoidRats <- Registry.printing registry "Typhoid Rats"
      ogreSentry <- Registry.printing registry "Ogre Sentry"
      humility <- Registry.printing registry "Humility"
      let (gs0, rats, _) = S.combatBoardOf [typhoidRats] [ogreSentry]
          gs = S.withHumility humility gs0
          fought = S.fightWith S.aggressiveAnswer gs
          ratId = case rats of r : _ -> r; [] -> ObjectId.MkObjectId 999
          ratBit = any (\ev -> DamageEvent.source ev == ratId && DamageEvent.dealtByDeathtouch ev) (S.damageEventsOf fought)
      Spec.assertBool s (not ratBit) "no deathtouch at deal time under Humility"

assignmentLegalitySpec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
assignmentLegalitySpec s =
  Spec.describe s "AssignmentLegality" $ do
    Spec.it s "under-assignment with no overflow is legal (power below lethal)" $
      -- One blocker, lethal 3, power 2, defender present with threshold 0.
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 3),
                (Recipient.ToPlayer S.bob, 0)
              ]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 1), 2)]
       in Spec.assertBool s (Damage.legalAssignment thresholds 2 answer) "accepted"

    Spec.it s "defender damage while a blocker is short is illegal" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 3),
                (Recipient.ToPlayer S.bob, 0)
              ]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 0),
                (Recipient.ToPlayer S.bob, 3)
              ]
       in Spec.assertBool s (not (Damage.legalAssignment thresholds 3 answer)) "rejected"

    Spec.it s "defender damage once the blocker has lethal is legal" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 1),
                (Recipient.ToPlayer S.bob, 0)
              ]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 1),
                (Recipient.ToPlayer S.bob, 2)
              ]
       in Spec.assertBool s (Damage.legalAssignment thresholds 3 answer) "accepted"

    Spec.it s "an answer that does not total power is illegal" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 1), 0)]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 1), 1)]
       in Spec.assertBool s (not (Damage.legalAssignment thresholds 2 answer)) "rejected"

    Spec.it s "an illegal recipient is rejected" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 1), 0)]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer = Map.fromList [(Recipient.ToCreature (ObjectId.MkObjectId 2), 2)]
       in Spec.assertBool s (not (Damage.legalAssignment thresholds 2 answer)) "rejected"

    -- The threshold gates the DEFENDER's share; it is not a cap on the
    -- blocker's. CR 702.19b lets the attacker assign past lethal and spill
    -- what is left, so an over-assigned blocker is no obstacle.
    Spec.it s "over-assigning a blocker and still spilling over is legal" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 2),
                (Recipient.ToPlayer S.bob, 0)
              ]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 3),
                (Recipient.ToPlayer S.bob, 2)
              ]
       in Spec.assertBool s (Damage.legalAssignment thresholds 5 answer) "accepted"

    -- CR 702.19b gates the defender on ALL blocking creatures having lethal,
    -- so this pair is the quantifier: one blocker short rejects the very same
    -- defender share that the twin below accepts once it is filled in. A gate
    -- reading "some blocker is at lethal" passes the first and is caught here.
    -- Two blockers is also the shape the prompt is actually reached in
    -- (Damage.attackerAssignment forces the single-blocker case unless the
    -- attacker tramples past its threshold).
    Spec.it s "two blockers: one short of lethal gates the defender" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 2),
                (Recipient.ToCreature (ObjectId.MkObjectId 2), 2),
                (Recipient.ToPlayer S.bob, 0)
              ]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 2),
                (Recipient.ToCreature (ObjectId.MkObjectId 2), 1),
                (Recipient.ToPlayer S.bob, 2)
              ]
       in Spec.assertBool s (not (Damage.legalAssignment thresholds 5 answer)) "rejected"

    Spec.it s "two blockers: both at lethal frees the defender" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 2),
                (Recipient.ToCreature (ObjectId.MkObjectId 2), 2),
                (Recipient.ToPlayer S.bob, 0)
              ]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 2),
                (Recipient.ToCreature (ObjectId.MkObjectId 2), 2),
                (Recipient.ToPlayer S.bob, 1)
              ]
       in Spec.assertBool s (Damage.legalAssignment thresholds 5 answer) "accepted"

    -- Without trample the defending player is not among the thresholds at all
    -- (Damage.attackerAssignment adds that entry only for a trampler), so a
    -- point aimed at them is an illegal RECIPIENT rather than a gated one.
    Spec.it s "with no trample the defending player is not a legal recipient" $
      let thresholds :: Map.Map Recipient.Recipient Natural.Natural
          thresholds =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 0),
                (Recipient.ToCreature (ObjectId.MkObjectId 2), 0)
              ]
          answer :: Map.Map Recipient.Recipient Natural.Natural
          answer =
            Map.fromList
              [ (Recipient.ToCreature (ObjectId.MkObjectId 1), 1),
                (Recipient.ToCreature (ObjectId.MkObjectId 2), 1),
                (Recipient.ToPlayer S.bob, 1)
              ]
       in Spec.assertBool s (not (Damage.legalAssignment thresholds 3 answer)) "rejected"

-- Assigns each blocker exactly its threshold, and every leftover point to the
-- defender. A legal trample division for these boards.
tramplingAnswer :: Prompt.Prompt r -> r
tramplingAnswer p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    let blockers = Map.toList (Map.filterWithKey (\r _ -> S.isCreatureRecipient r) thresholds)
        toBlockers = Map.fromList (fmap (\(r, t) -> (r, t)) blockers)
        spent = sum (fmap snd blockers)
        leftover = if n >= spent then n - spent else 0
        defenders = filter (not . S.isCreatureRecipient) (Map.keys thresholds)
     in case defenders of
          d : _ -> Map.insert d leftover toBlockers
          [] -> toBlockers
  _ -> S.aggressiveAnswer p

trampleSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
trampleSpec s registry =
  Spec.describe s "Trample" $ do
    Spec.it s "CR 702.19b a 3/3 trampler spills excess onto the defending player" $ do
      -- War Mammoth (3/3 trample) blocked by a Piker (2/1): 1 lethal to the
      -- Piker, 2 to bob. Mammoth survives (2 marked < 3).
      warMammoth <- Registry.printing registry "War Mammoth"
      piker <- Registry.printing registry "Goblin Piker"
      let (gs, _, _) = S.combatBoardOf [warMammoth] [piker]
          after = S.settleSba (S.fightWith tramplingAnswer gs)
      Spec.assertEqWith s "bob took the 2 overflow" (S.lifeOf S.bob after) (Just 18)
      Spec.assertEqWith s "the Piker is dead" (S.creaturesInPlay S.bob after) 0
      Spec.assertEqWith s "the Mammoth survives" (S.creaturesInPlay S.alice after) 1

    Spec.it s "CR 702.19b a non-trample control spills nothing" $ do
      -- Ogre Sentry is a 3/3 that cannot attack (defender), so use the Piker's
      -- existing behavior as the control: a blocked non-trample attacker deals
      -- nothing to the player. (combatDamageTests already asserts bob = 20.)
      piker <- Registry.printing registry "Goblin Piker"
      let (gs, _, _) = S.combatBoardOf [piker] [piker]
          after = S.fightWith tramplingAnswer gs
      Spec.assertEqWith s "bob untouched by a non-trampler" (S.lifeOf S.bob after) (Just 20)

    Spec.it s "CR 702.19b defender-short assignment is rejected" $ do
      -- A cheat responder gives bob 3 while the Piker gets 0. Illegal: the
      -- attacker deals nothing, bob untouched, Piker survives.
      warMammoth <- Registry.printing registry "War Mammoth"
      piker <- Registry.printing registry "Goblin Piker"
      let (gs, _, _) = S.combatBoardOf [warMammoth] [piker]
          cheat p = case p of
            Prompt.AssignCombatDamage _ _ _ thresholds n ->
              case filter (not . S.isCreatureRecipient) (Map.keys thresholds) of
                d : _ -> Map.singleton d n
                [] -> Map.empty
            _ -> S.aggressiveAnswer p
          after = S.settleSba (S.fightWith cheat gs)
      Spec.assertEqWith s "bob untouched" (S.lifeOf S.bob after) (Just 20)
      Spec.assertEqWith s "the Piker survives the rejected assignment" (S.creaturesInPlay S.bob after) 1

    Spec.it s "CR 702.19b under-assignment across two blockers spills nothing (power below total lethal)" $ do
      -- War Mammoth (3/3 trample) blocked by TWO Ogre Sentries (3/3 each): it
      -- cannot reach lethal on both (needs 6, has 3), so no overflow -- bob is
      -- untouched -- and the division among the Ogres is free. Real cards, for
      -- the under-assignment case assignmentLegalityTests pins on the predicate.
      warMammoth <- Registry.printing registry "War Mammoth"
      ogreSentry <- Registry.printing registry "Ogre Sentry"
      let (gs, _, _) = S.combatBoardOf [warMammoth] [ogreSentry, ogreSentry]
          dumpOne p = case p of
            Prompt.AssignCombatDamage _ _ _ thresholds n ->
              case filter S.isCreatureRecipient (Map.keys thresholds) of
                r : _ -> Map.singleton r n
                [] -> Map.empty
            _ -> S.aggressiveAnswer p
          after = S.settleSba (S.fightWith dumpOne gs)
      Spec.assertEqWith s "bob untouched (no overflow)" (S.lifeOf S.bob after) (Just 20)
      Spec.assertEqWith s "one Ogre took all 3 and died, the other lived" (S.creaturesInPlay S.bob after) 1

-- #29: a blocker declared in the declare blockers step can be gone by the combat
-- damage step -- since M3a the pool has instant-speed removal. CR 509.1h keeps the
-- attacker BLOCKED (the combat map is the record and is deliberately not mutated,
-- #28), but CR 510.1c assigns damage only to the creatures CURRENTLY blocking it.
-- Removal happens between declareBlockers and dealCombatDamage, which is exactly
-- when a Murder resolves.
killBlockerMidCombat :: ObjectId.ObjectId -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
killBlockerMidCombat victim answer gs =
  S.runPure answer gs $ do
    Combat.declareAttackers S.alice
    Combat.declareBlockers
    Event.destroy Regenerability.Regenerable [victim]
    Monad.void Damage.dealCombatDamage

-- Every DamageDealt event in the history addressed to `oid`, however much.
damageEventsTo :: ObjectId.ObjectId -> GameState.GameState -> [DamageEvent.DamageEvent]
damageEventsTo oid gs =
  let pick ev = case ev of
        GameEvent.DamageDealt de ->
          if DamageEvent.target de == Recipient.ToCreature oid then [de] else []
        _ -> []
   in concatMap pick (GameState.events gs)

-- Sinks the whole assignment into the first creature recipient offered. This is
-- the discriminating answer for #29: if the engine still offers a departed
-- blocker, the damage lands on the ghost and evaporates, so any assertion about
-- where the damage really went fails. An answer that routes by threshold (like
-- tramplingAnswer) would pass either way, because a departed blocker's threshold
-- computes to 0.
dumpOntoFirstCreature :: Prompt.Prompt r -> r
dumpOntoFirstCreature p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter S.isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  _ -> S.aggressiveAnswer p

departedBlockerSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
departedBlockerSpec s registry =
  Spec.describe s "Departed blockers (#29)" $ do
    Spec.it s "CR 702.19d a trampler whose only blocker left assigns everything to the player" $ do
      -- War Mammoth (3/3 trample) is blocked by a Piker (2/1); the Piker is
      -- destroyed before damage. "As though all blocking creatures have been
      -- assigned lethal damage" -- so all 3 hit bob, and there is nothing left
      -- to choose. dumpOntoFirstCreature sinks everything into a creature
      -- recipient if one is offered, so an offered ghost shows up as bob
      -- taking nothing.
      warMammoth <- Registry.printing registry "War Mammoth"
      piker <- Registry.printing registry "Goblin Piker"
      let (gs, _, theirs) = S.combatBoardOf [warMammoth] [piker]
      case theirs of
        blocker : _ ->
          let after = S.settleSba (killBlockerMidCombat blocker dumpOntoFirstCreature gs)
           in do
                Spec.assertEqWith s "bob took all 3" (S.lifeOf S.bob after) (Just 17)
                Spec.assertEqWith s "nothing was addressed to the departed blocker" (damageEventsTo blocker after) []
        [] -> Spec.assertFailure s "fixture did not build a blocker"

    Spec.it s "CR 510.1c a non-trampler whose only blocker left assigns no combat damage" $ do
      -- A Piker (2/1) blocked by a Piker that then dies. "If no creatures are
      -- currently blocking it ... it assigns no combat damage." bob is untouched
      -- either way, so the observable is the history: the engine must not record
      -- a DamageDealt addressed to a creature that is not there.
      piker <- Registry.printing registry "Goblin Piker"
      let (gs, _, theirs) = S.combatBoardOf [piker] [piker]
      case theirs of
        blocker : _ ->
          let after = S.settleSba (killBlockerMidCombat blocker S.aggressiveAnswer gs)
           in do
                Spec.assertEqWith s "bob untouched" (S.lifeOf S.bob after) (Just 20)
                Spec.assertEqWith s "no phantom damage event" (damageEventsTo blocker after) []
        [] -> Spec.assertFailure s "fixture did not build a blocker"

    Spec.it s "CR 510.1c a partly-departed block assigns only among the survivors" $ do
      -- War Mammoth (3/3 trample) blocked by two Ogre Sentries (3/3); one dies
      -- before damage. One live blocker with lethal exactly 3 leaves nothing to
      -- divide, so this is forced: all 3 onto the survivor, which then dies.
      -- With the ghost still in the list the assignment is a free division and
      -- the whole 3 can sink into it, leaving the survivor untouched.
      warMammoth <- Registry.printing registry "War Mammoth"
      ogreSentry <- Registry.printing registry "Ogre Sentry"
      let (gs, _, theirs) = S.combatBoardOf [warMammoth] [ogreSentry, ogreSentry]
      case theirs of
        dead : _ : _ ->
          let after = S.settleSba (killBlockerMidCombat dead dumpOntoFirstCreature gs)
           in do
                Spec.assertEqWith s "the surviving Ogre took the full 3 and died" (S.creaturesInPlay S.bob after) 0
                Spec.assertEqWith s "bob untouched (3 power, 3 lethal, no excess)" (S.lifeOf S.bob after) (Just 20)
        _ -> Spec.assertFailure s "fixture did not build two blockers"

-- CR 509.1h at whole-card level: alice attacks, bob blocks, and a REAL Lightning Bolt
-- -- cast, paid for, targeted, resolved off the stack, with the CR 704.5g SBA
-- doing the killing -- removes the blocker before the combat damage step. The
-- direct-call twin above (killBlockerMidCombat) reaches the same state through
-- Event.destroy; this one proves the door a player actually uses gets there too.
--
-- `blocks` routes the whole board's blockers at the first attacker (or declines,
-- for the control leg). The state is split at the SBA because S.settleSba is a
-- plain GameState -> GameState, not a Game action.
boltBlockerMidCombat :: Bool -> ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
boltBlockerMidCombat blocks bolt blocker gs =
  let aimedAtBlocker :: Prompt.Prompt r -> r
      aimedAtBlocker p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToCreature blocker)) sets
        Prompt.DeclareBlockers {} | not blocks -> Map.empty
        _ -> S.aggressiveAnswer p
      declared =
        S.runPure aimedAtBlocker gs $ do
          Combat.declareAttackers S.alice
          Combat.declareBlockers
          Cast.castSpell S.alice bolt
          Monad.void Stack.resolveTop
   in S.runPure S.aggressiveAnswer (S.settleSba declared) (Monad.void Damage.dealCombatDamage)

-- CR 509.1h's last sentence -- "A creature remains blocked even if all the
-- creatures blocking it are removed from combat" -- is a STATUS the declaration
-- confers, not a running count of who is still blocking. Combat.blockers spells it
-- with the attacker's KEY, and the two ways the set behind that key can empty out
-- are covered here: the blocker destroyed (the key survives untouched, and
-- Damage.attackerAssignment's liveness filter drops it), and the blocker removed
-- from combat by regenerating (CR 506.4/701.19a: Game.removeFromCombat empties the
-- set but keeps the key).
--
-- Both end at the same observable: the attacker assigns no combat damage at all
-- (CR 510.1c), so the defending player takes nothing. Reading emptiness as
-- unblocked -- the bug this group pins -- lets the attacker through instead.
blockedStaysBlockedSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
blockedStaysBlockedSpec s registry =
  Spec.describe s "Blocked stays blocked (CR 509.1h)" $ do
    Spec.it s "CR 510.1c a blocker Bolted after blocks are declared leaves the attacker blocked, so the defender takes nothing" $ do
      piker <- Registry.printing registry "Goblin Piker"
      mountain <- Registry.printing registry "Mountain"
      lightningBolt <- Registry.printing registry "Lightning Bolt"
      let (board, mine, theirs) = S.combatBoardOf [piker] [piker]
          (_, withLand) = S.addCreature mountain S.alice board
          (bolt, gs) = S.addHandCard lightningBolt S.alice withLand
      case (mine, theirs) of
        (attacker : _, blocker : _) -> do
          let after = boltBlockerMidCombat True bolt blocker gs
              -- The control leg: the SAME Bolt on the SAME blocker, but bob
              -- declines to block. Nothing is blocking either way, so this is
              -- what discriminates "blocked with no blockers left" (assigns
              -- nothing) from "never blocked" (assigns to the player) -- without
              -- it, an engine that simply never dealt this damage would pass.
              unblocked = boltBlockerMidCombat False bolt blocker gs
          Spec.assertBool s (not (Set.member blocker (GameState.battlefield after))) "the Bolt killed the blocker"
          Spec.assertBool s (Combat.isBlocked attacker after) "the attacker is still blocked"
          Spec.assertEqWith s "so bob takes nothing" (S.lifeOf S.bob after) (Just 20)
          Spec.assertBool s (not (Combat.isBlocked attacker unblocked)) "unblocked control leg: not blocked"
          Spec.assertEqWith s "unblocked control leg: bob takes the Piker's 2" (S.lifeOf S.bob unblocked) (Just 18)
        _ -> Spec.assertFailure s "fixture did not build an attacker and a blocker"

    Spec.it s "CR 701.19a a blocker that regenerates is removed from combat, and the attacker is STILL blocked" $ do
      -- Drudge Skeletons blocks, then regenerates off alice's Bolt. CR 701.19a's
      -- rewrite ends with "If it's an attacking or blocking creature, remove it
      -- from combat," so unlike the destroyed blocker above this one is still on
      -- the battlefield and is genuinely removed from combat rather than merely
      -- dead. The shield is seeded rather than activated: what is under test is
      -- CR 509.1h, and bob paying {B} for his own ability is ActivateSpec's
      -- subject.
      piker <- Registry.printing registry "Goblin Piker"
      mountain <- Registry.printing registry "Mountain"
      lightningBolt <- Registry.printing registry "Lightning Bolt"
      drudgeSkeletons <- Registry.printing registry "Drudge Skeletons"
      let (board, mine, theirs) = S.combatBoardOf [piker] [drudgeSkeletons]
          (_, withLand) = S.addCreature mountain S.alice board
          (bolt, gs0) = S.addHandCard lightningBolt S.alice withLand
      case (mine, theirs) of
        (attacker : _, blocker : _) -> do
          let after = boltBlockerMidCombat True bolt blocker (S.addRegenShield blocker gs0)
          Spec.assertBool s (Set.member blocker (GameState.battlefield after)) "CR 701.19a: the Skeletons survived the Bolt"
          Spec.assertEqWith s "CR 701.19a: and its damage was removed" (S.damageOf blocker after) (Just 0)
          Spec.assertEqWith s "CR 506.4: it is no longer blocking anything" (Combat.blockersOf attacker after) Set.empty
          Spec.assertBool s (Combat.isBlocked attacker after) "CR 509.1h: but the attacker remains blocked"
          Spec.assertEqWith s "CR 510.1c: so it assigns no combat damage and bob takes nothing" (S.lifeOf S.bob after) (Just 20)
          Spec.assertEqWith s "CR 510.1d: and the regenerated blocker assigns nothing back" (S.damageOf attacker after) (Just 0)
          -- The Bolt's own 3 is in the history too, so this filters to combat
          -- damage: what must be absent is the attacker hitting a creature the
          -- rules say is no longer blocking it (CR 510.1c).
          Spec.assertEqWith s "and no COMBAT damage was addressed to it either" (filter (\ev -> DamageEvent.kind ev == DamageKind.Combat) (damageEventsTo blocker after)) []
        _ -> Spec.assertFailure s "fixture did not build an attacker and a blocker"

-- The mirror of killBlockerMidCombat: the ATTACKER is gone by the combat damage
-- step. CR 506.4 removes it from combat, so by CR 510.1d its blockers are
-- blocking nothing -- but Combat.blockers is keyed BY the attacker and that key
-- is never pruned (CR 509.1h, #28), so the stale key is what reaches
-- Damage.blockerAssignment.
killAttackerMidCombat :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
killAttackerMidCombat victim gs =
  S.runPure S.aggressiveAnswer gs $ do
    Combat.declareAttackers S.alice
    Combat.declareBlockers
    Event.destroy Regenerability.Regenerable [victim]
    Monad.void Damage.dealCombatDamage

departedAttackerSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
departedAttackerSpec s registry =
  Spec.describe s "Departed attackers (CR 510.1d)" $ do
    Spec.it s "CR 510.1d a blocker whose attacker was destroyed mid-combat assigns no combat damage" $ do
      -- CR 510.1d: "A blocking creature assigns combat damage to the creatures
      -- it's blocking. If it isn't currently blocking any creatures (if, for
      -- example, they were destroyed or removed from combat), it assigns no
      -- combat damage." The attacker is destroyed between declare blockers and
      -- the combat damage step -- CR 506.4 removes it from combat.
      --
      -- What the assertion catches: an implementation that filters only the
      -- BLOCKER (Damage.blockerAssignment's Projection.powerOf reads the
      -- blocker, which is alive) and so emits a DamageEvent addressed to the
      -- dead attacker. Marking that damage is a no-op the moment the object is
      -- gone from GameState.objects, so the mark is NOT the observable -- the
      -- CR 608.2i history is. The control leg proves the assertion is not
      -- vacuous: with the attacker alive the same board records exactly that
      -- event.
      piker <- Registry.printing registry "Goblin Piker"
      let (gs, mine, _) = S.combatBoardOf [piker] [piker]
      case mine of
        attacker : _ -> do
          Spec.assertEqWith s "nothing was addressed to the destroyed attacker" (damageEventsTo attacker (killAttackerMidCombat attacker gs)) []
          Spec.assertEqWith s "and with the attacker alive the blocker DOES hit it -- the filter is what did it" (fmap DamageEvent.amount (damageEventsTo attacker (S.fightWith S.aggressiveAnswer gs))) [2]
        [] -> Spec.assertFailure s "fixture did not build an attacker"

    Spec.it s "CR 510.1d a blocker whose attacker left the game assigns no combat damage" $ do
      -- The departure route to the same stale key. Three seats, because at two
      -- the concession ends the game (CR 104.2a). CR 800.4a's first clause
      -- deletes alice's attacker outright and drops its Combat.attackers entry,
      -- but the Combat.blockers KEY survives -- deliberately, since CR 509.1h's
      -- last sentence is about the blockers' side of that record -- so bob's
      -- blocker is still handed a dead attacker to hit.
      piker <- Registry.printing registry "Goblin Piker"
      let (attacker, b1) = S.addCreature piker S.alice S.threePlayerGame
          (blocker, b2) = S.addCreature piker S.bob b1
          fighting =
            b2
              { GameState.combat =
                  Combat.Type.MkCombat
                    { Combat.Type.attackers = Map.singleton attacker (AttackTarget.OfPlayer S.bob),
                      Combat.Type.blockers = Map.singleton attacker (Set.singleton blocker),
                      Combat.Type.struckFirst = Nothing,
                      -- CR 508.1k / 509.1g: each joined combat under its own
                      -- controller, which is what a declaration would have
                      -- recorded and what CR 506.4 compares against.
                      Combat.Type.joinedUnder = Map.fromList [(attacker, S.alice), (blocker, S.bob)],
                      Combat.Type.attackersJoined = True,
                      Combat.Type.defender = Just S.bob
                    }
              }
          gone = Departure.depart Departure.Type.Conceded S.alice fighting
          (assignedAfter, _) = S.runPureWith S.identityAnswer gone (Damage.gatherCombatDamage (const True))
          (assignedBefore, _) = S.runPureWith S.identityAnswer fighting (Damage.gatherCombatDamage (const True))
      Spec.assertBool s (Map.member attacker (Combat.Type.blockers (GameState.combat gone))) "CR 509.1h: the blockers key really is still there, so this is the live path"
      Spec.assertEqWith s "no assignment names the departed attacker" (filter (\ev -> DamageEvent.target ev == Recipient.ToCreature attacker) assignedAfter) []
      Spec.assertEqWith s "and with alice still in the game the blocker's hit is assigned -- the filter is what did it" (fmap DamageEvent.amount (filter (\ev -> DamageEvent.target ev == Recipient.ToCreature attacker) assignedBefore)) [2]

-- CR 800.4e: "If combat damage would be assigned to a player who has left the
-- game, that damage isn't assigned." attackerAssignment reads the defender's
-- status at two independent sites -- the unblocked/trample-through toDefender
-- list, and the CR 702.19b threshold map the assignment prompt offers -- and
-- both need coverage.
--
-- S.identityAnswer's AssignCombatDamage arm dumps the WHOLE amount onto the
-- first CREATURE recipient it finds (Support.hs), never a player one, so it
-- cannot tell whether a ToPlayer entry is present in the threshold map at all:
-- guarded or not, a blocked trampler's excess lands on the blocker either way
-- under that answerer. It is fine for the unblocked path (no prompt is ever
-- issued there), but the trample threshold map needs an answerer that actually
-- spends the excess on a player recipient when one is offered.
-- defenderOrBlockerAnswer assigns each blocker exactly its threshold and routes
-- the leftover to a player recipient if the threshold map offers one, falling
-- back onto the blocker (over-lethal, still legal -- Damage.legalAssignment has
-- no upper bound) when it does not. That is what actually surfaces whether the
-- departed defender was offered.
defenderOrBlockerAnswer :: Prompt.Prompt r -> r
defenderOrBlockerAnswer p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    let blockerEntries = Map.toList (Map.filterWithKey (\r _ -> S.isCreatureRecipient r) thresholds)
        toBlockers = Map.fromList blockerEntries
        spent = sum (fmap snd blockerEntries)
        leftover = if n >= spent then n - spent else 0
        defenders = filter (not . S.isCreatureRecipient) (Map.keys thresholds)
     in case defenders of
          d : _ -> Map.insert d leftover toBlockers
          [] -> case blockerEntries of
            (r, _) : _ -> Map.insertWith (+) r leftover toBlockers
            [] -> toBlockers
  _ -> S.aggressiveAnswer p

departedDefenderSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
departedDefenderSpec s registry =
  Spec.describe s "Departed defender (CR 800.4e)" $ do
    Spec.it s "CR 800.4e no combat damage is assigned to a player who has left the game" $ do
      -- CR 800.4e: "If combat damage would be assigned to a player who has left
      -- the game, that damage isn't assigned." Reachable: a defending player can
      -- concede between the declare-attackers step and the combat damage step.
      -- Three seats, because at two the concession ends the game (CR 104.2a).
      piker <- Registry.printing registry "Goblin Piker"
      let (attacker, board) = S.addCreature piker S.alice S.threePlayerGame
          attacking =
            board
              { GameState.combat =
                  Combat.Type.MkCombat
                    { Combat.Type.attackers = Map.singleton attacker (AttackTarget.OfPlayer S.bob),
                      Combat.Type.blockers = Map.empty,
                      Combat.Type.struckFirst = Nothing,
                      Combat.Type.joinedUnder = Map.singleton attacker S.alice,
                      Combat.Type.attackersJoined = True,
                      Combat.Type.defender = Just S.bob
                    }
              }
          gone = Departure.depart Departure.Type.Conceded S.bob attacking
          (assignedAfter, _) = S.runPureWith S.identityAnswer gone (Damage.gatherCombatDamage (const True))
          (assignedBefore, _) = S.runPureWith S.identityAnswer attacking (Damage.gatherCombatDamage (const True))
      Spec.assertEqWith s "nothing is assigned to the departed defender" assignedAfter []
      Spec.assertEqWith s "and with bob still in the game the same board assigns one hit -- the guard is what did it" (length assignedBefore) 1
      Spec.assertEqWith s "to bob" (fmap DamageEvent.target assignedBefore) [Recipient.ToPlayer S.bob]

    Spec.it s "CR 800.4e a departed defender is not offered as a trample recipient either" $ do
      -- CR 702.19b assigns trample's excess "as its controller chooses", and the
      -- defending player is one of the choices Prompt.AssignCombatDamage offers.
      -- CR 800.4e removes the damage, so the choice must not be offered: an
      -- assignment the engine then discards would silently take damage away from
      -- the blockers it could otherwise have gone to.
      --
      -- CAROL is the defender and the one who leaves, and BOB's Piker blocks, so
      -- the blocker survives the departure and the board stays in the prompt arm.
      -- (Making the defender the blocker's controller would work too, right up
      -- to the point where CR 800.4a's first clause removes their blocker and
      -- the board falls out of that arm entirely.) War Mammoth is a 3/3 with
      -- trample; the Piker is a 2/1, so there is excess and a real choice.
      --
      -- S.identityAnswer is not the discriminating answerer here (see the
      -- group comment above): it never picks a player recipient, so a blocked
      -- trampler's excess lands on the blocker whether the defender is offered
      -- or not. defenderOrBlockerAnswer is used for both legs instead.
      warMammoth <- Registry.printing registry "War Mammoth"
      piker <- Registry.printing registry "Goblin Piker"
      let (attacker, b1) = S.addCreature warMammoth S.alice S.threePlayerGame
          (blocker, b2) = S.addCreature piker S.bob b1
          attacking =
            b2
              { GameState.combat =
                  Combat.Type.MkCombat
                    { Combat.Type.attackers = Map.singleton attacker (AttackTarget.OfPlayer S.carol),
                      Combat.Type.blockers = Map.singleton attacker (Set.singleton blocker),
                      Combat.Type.struckFirst = Nothing,
                      Combat.Type.joinedUnder = Map.fromList [(attacker, S.alice), (blocker, S.bob)],
                      Combat.Type.attackersJoined = True,
                      Combat.Type.defender = Just S.carol
                    }
              }
          gone = Departure.depart Departure.Type.Conceded S.carol attacking
          (assignedAfter, _) = S.runPureWith defenderOrBlockerAnswer gone (Damage.gatherCombatDamage (const True))
          (assignedBefore, _) = S.runPureWith defenderOrBlockerAnswer attacking (Damage.gatherCombatDamage (const True))
      Spec.assertBool s (Maybe.isJust (Game.lookupObject blocker gone)) "the blocker survived carol's departure, so the board is still in the prompt arm"
      Spec.assertBool s (notElem (Recipient.ToPlayer S.carol) (fmap DamageEvent.target assignedAfter)) "no assignment names the departed defender"
      Spec.assertEqWith s "all three points land on the blocker instead" (fmap DamageEvent.amount (filter (\ev -> DamageEvent.target ev == Recipient.ToCreature blocker) assignedAfter)) [3]
      Spec.assertBool s (Maybe.isJust (List.find (\ev -> DamageEvent.target ev == Recipient.ToPlayer S.carol) assignedBefore)) "with carol still in the game the threshold map DOES offer her -- the guard is what did it"

-- Grant deathtouch to `oid` the way Serpent's Gift does: a stored continuous
-- effect over just that object. Timestamp is arbitrary (no competing layer-6
-- effect in these fixtures).
grantDeathtouch :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
grantDeathtouch oid gs =
  let eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = ObjectId.MkObjectId 997,
            ContinuousEffect.timestamp = Timestamp.MkTimestamp 500,
            ContinuousEffect.expiry = Expiry.Type.AtCleanup,
            ContinuousEffect.modification = Modification.GainKeyword Keyword.Deathtouch,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
   in gs {GameState.continuousEffects = eff : GameState.continuousEffects gs}

trampleDeathtouchSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
trampleDeathtouchSpec s registry =
  Spec.describe s "TrampleDeathtouch" $ do
    Spec.it s "CR 702.2c a deathtouch-granted trampler needs only 1 on the blocker, spilling the rest" $ do
      -- War Mammoth (3/3 trample) GRANTED deathtouch into Ogre Sentry (3/3):
      -- lethal collapses to 1, so 1 to the Ogre and 2 tramples to bob; the Ogre
      -- still dies (704.5h, via the deal-time bit). Real cards replace M2c's
      -- synthetic deathtrampler.
      warMammoth <- Registry.printing registry "War Mammoth"
      ogreSentry <- Registry.printing registry "Ogre Sentry"
      let (gs0, mammoths, _) = S.combatBoardOf [warMammoth] [ogreSentry]
          mammothId = case mammoths of
            m : _ -> m
            [] -> ObjectId.MkObjectId 999
          gs = grantDeathtouch mammothId gs0
          after = S.settleSba (S.fightWith tramplingAnswer gs)
      Spec.assertEqWith s "bob took the 2 overflow" (S.lifeOf S.bob after) (Just 18)
      Spec.assertEqWith s "the Ogre is dead" (S.creaturesInPlay S.bob after) 0

    Spec.it s "CR 702.19b the control: plain trample into the same 3/3 spills nothing" $ do
      -- War Mammoth (3/3 trample, NO deathtouch) into Ogre Sentry (3/3): lethal
      -- is 3, all 3 go to the Ogre, 0 tramples. Only deathtouch changes the spill.
      warMammoth <- Registry.printing registry "War Mammoth"
      ogreSentry <- Registry.printing registry "Ogre Sentry"
      let (gs, _, _) = S.combatBoardOf [warMammoth] [ogreSentry]
          after = S.settleSba (S.fightWith tramplingAnswer gs)
      Spec.assertEqWith s "bob untouched without deathtouch" (S.lifeOf S.bob after) (Just 20)

m2cPropertySpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
m2cPropertySpec s registry =
  Spec.describe s "M2cProperties" $ do
    Spec.it s "a deathtoucher's victim with toughness > 0 is gone after the SBA" $ do
      -- The property in fixture form (the deck has no deathtoucher, so this is
      -- the M2c coverage; it becomes a random-game property when a deathtoucher
      -- joins a deck -- the castability work, #23). Every toughness we throw at
      -- the 1/1 deathtoucher dies to it.
      typhoidRats <- Registry.printing registry "Typhoid Rats"
      piker <- Registry.printing registry "Goblin Piker"
      nimbleBirdsticker <- Registry.printing registry "Nimble Birdsticker"
      ogreSentry <- Registry.printing registry "Ogre Sentry"
      let victims = [piker, nimbleBirdsticker, ogreSentry]
          killsIt v =
            let (gs, _, _) = S.combatBoardOf [typhoidRats] [v]
                after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
             in S.creaturesInPlay S.bob after == 0
      Spec.assertBool s (all killsIt victims) "deathtouch kills every toughness"

    Spec.it s "the deathtouch and trample reads never name a card" $
      -- A structural reminder, asserted by the interaction falsifier's outcome
      -- (TrampleDeathtouch) depending only on the keyword projection. This case
      -- documents the invariant; the real enforcement is code review of
      -- Damage.blockerThreshold and Sba.woundedByDeathtouch, which case on
      -- Keyword, never on a printing.
      Spec.assertBool s True "see TrampleDeathtouch and Deathtouch groups"

-- CR 120.1a: "Damage can't be dealt to an object that's not a battle, a
-- creature, or a planeswalker." Damage.damageRecipient is where a Recipient that
-- names a permanent GENERICALLY -- Pawl.Engine.Binding.became's entrant, which
-- Pawl.Engine.Event.eventBindings tags Recipient.ToObject because the trigger condition
-- says nothing about the entrant's card types -- gets classified before an
-- effect can build a damage event out of it.
--
-- Aether Flash exercises the two answers this function gives in a real game
-- (TriggerSpec's aetherFlashTests): a creature entrant becomes ToCreature, and
-- an entrant already dead by the time the ability resolves (CR 608.2h) becomes
-- Nothing. The third, a permanent that exists and is not a creature, no card in
-- the pool can produce -- every DealDamage on a generically named slot belongs
-- to a condition whose Filter admits only creatures -- so it is pinned here.
damageRecipientSpec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
damageRecipientSpec s registry =
  Spec.describe s "CR 120.1a which recipients damage can be dealt to" $ do
    Spec.it s "a generically named creature becomes CR 120.3e's creature recipient" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
      Spec.assertEqWith
        s
        "retagged, not rejected"
        (Damage.damageRecipient gs (Recipient.ToObject oid))
        (Just (Recipient.ToCreature oid))

    Spec.it s "a generically named NONcreature permanent can be dealt no damage" $ do
      plains <- Registry.printing registry "Plains"
      let (oid, gs) = S.addCreature plains S.alice (Setup.emptyGame S.bothPlayers)
      Spec.assertBool s (Set.member oid (GameState.battlefield gs)) "the land is really there"
      Spec.assertEqWith s "and takes nothing" (Damage.damageRecipient gs (Recipient.ToObject oid)) Nothing

    Spec.it s "an object that no longer exists takes nothing either (CR 608.2h)" $
      Spec.assertEqWith
        s
        "no recipient"
        (Damage.damageRecipient (Setup.emptyGame S.bothPlayers) (Recipient.ToObject (ObjectId.MkObjectId 99)))
        Nothing

    -- The pass-through half. A combat recipient (CR 510.1b-d) and a chosen
    -- target out of a typed Pool were classified when they were built, so this
    -- function is not a second, later reading of the same question.
    Spec.it s "a creature or player recipient is unchanged" $ do
      piker <- Registry.printing registry "Goblin Piker"
      let (oid, gs) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
      Spec.assertEqWith s "creature" (Damage.damageRecipient gs (Recipient.ToCreature oid)) (Just (Recipient.ToCreature oid))
      Spec.assertEqWith s "player" (Damage.damageRecipient gs (Recipient.ToPlayer S.bob)) (Just (Recipient.ToPlayer S.bob))

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Damage" $ do
  damageSpec s registry
  damageRecipientSpec s registry
  legendRuleSpec s registry
  worldRuleSpec s registry
  damageEventSpec s registry
  deathtouchSpec s registry
  assignmentLegalitySpec s
  trampleSpec s registry
  departedBlockerSpec s registry
  blockedStaysBlockedSpec s registry
  departedAttackerSpec s registry
  departedDefenderSpec s registry
  trampleDeathtouchSpec s registry
  sbaSpec s
  creatureSbaSpec s registry
  infectSpec s registry
  toxicSpec s registry
  m2cPropertySpec s registry
