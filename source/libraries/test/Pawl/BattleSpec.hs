{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Battle, rule 310: the defense a battle prints (CR 210.1 /
-- 310.4a), the defense counters CR 310.4b makes it enter with, the protector CR
-- 310.9a has its controller choose as it enters, CR 310.12a's restriction of that
-- choice to an opponent, and CR 310.11's state-based action -- listed as CR 704.5x
-- and CR 704.5y -- repairing the designation once it is illegal.
--
-- And what a protector is FOR, which lives in Pawl.Engine.Combat rather than in
-- Pawl.Engine.Battle but is rule 310 all the same: CR 310.5's attackable battle
-- (Combat.attackableBattles), CR 310.9b including its "notably, a Siege battle can
-- be attacked by its own controller", CR 310.9c's blocking, and CR 310.9d with CR
-- 508.5 (Defender.playerOf) -- including CR 506.4c's attacker left attacking a
-- battle that has gone, or whose protector CR 704.5y moved, whose defending
-- player is the seat recorded as it joined combat.
-- Those are attackSpec below; Pawl.CombatSpec keeps rule 508's own cases.
--
-- Also the pieces rule 310 needed underneath it, exercised here because this is
-- where a card reaches them: Pawl.Types.Defense, CounterKind.Defense,
-- Event.designateProtector, Object.protector and AttackTarget.OfBattle.
--
-- Invasion of Dominaria // Serra Faithkeeper is the battle every case here is
-- built on. {2}{W} Battle -- Siege,
-- defense 5, "When this Siege enters, you gain 4 life and draw a card",
-- transforming into a 4/4 Angel with flying and vigilance.
--
-- It is deliberately not Invasion of Kaladesh. That card's front face is simpler
-- still, but its BACK face is a Legendary Artifact -- Vehicle with a
-- characteristic-defining power and crew, and a card file must carry both faces
-- honestly. Serra Faithkeeper is two printed keywords, so every line of this card
-- is representable and these cases exercise rule 310 rather than the card.
--
-- Goblin Piker and Bog Wraith join it for the combat cases, and are the pool's
-- plainest bodies: a vanilla 2/1 and a 3/3 whose entire text is swampwalk. Neither
-- is a battle, so attackSpec's cases read rule 310 rather than the attacker.
--
-- And how a battle is defeated: CR 310.6 / 120.3h's damage removing defense
-- counters, CR 310.12b's intrinsic Siege ability, and CR 310.7 / 704.5v's and CR
-- 310.8 / 704.5w's state-based actions.
-- Those are damageSpec and defeatSpec below.
module Pawl.BattleSpec where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Battle as Battle
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Sba as Sba
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.AttackerDeclared as AttackerDeclared
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterChange as CounterChange
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Defense as Defense
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.SelfCountersRemoved as SelfCountersRemoved
import qualified Pawl.Types.TeamId as TeamId
import qualified Pawl.Types.Teams as Teams
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Battle" $ do
  entrySpec s registry
  candidateSpec s registry
  repairSpec s registry
  attackSpec s registry
  damageSpec s registry
  defeatSpec s registry

-- CR 310.4b and CR 310.9a both fire as the battle enters, and both are visible
-- from a cast -- which is what makes this the gameplay-level test rather than a
-- projection one.
entrySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
entrySpec s registry = Spec.describe s "Entry" $ do
  Spec.it s "CR 310.4b Invasion of Dominaria enters with five defense counters" $ do
    (after, oid) <- castInvasion s registry
    Spec.assertEqWith s "five defense counters" (S.counterOf CounterKind.Defense oid after) 5
  Spec.it s "CR 210.1 that five is the PRINTED number, and it is projected" $ do
    (after, oid) <- castInvasion s registry
    Spec.assertEqWith
      s
      "the projection carries the printed defense"
      (PC.defense (Projection.project oid after))
      (Just (Defense.MkDefense 5))
  Spec.it s "CR 310.12a its protector is an opponent of its controller" $ do
    (after, oid) <- castInvasion s registry
    -- Two seats leave exactly one legal protector, so this asserts CR 310.12a's
    -- restriction rather than a choice; data/scenarios/battle is where the
    -- choice is made observable.
    Spec.assertEqWith s "bob protects it" (protectorOf oid after) (Just S.bob)
  Spec.it s "CR 616.1e entering a battle orders no replacement effects" $ do
    (after, oid) <- castInvasionRefusingToOrder s registry
    -- Both halves still landed, so this is not passing by never entering.
    Spec.assertEqWith s "five defense counters" (S.counterOf CounterKind.Defense oid after) 5
    Spec.assertEqWith s "and a protector" (protectorOf oid after) (Just S.bob)

-- CR 310.9a's candidate rule at the level Pawl.Engine.Battle states it, which is
-- the arithmetic the entry choice and the CR 704.5x re-choice SHARE -- so a drift
-- between them would show here.
--
-- The projections are the REAL card's, taken off the board a cast produced, so
-- these cases cannot pass against a Siege pawl does not actually build. The
-- no-battle-types half is that same projection with its subtypes stripped, which
-- has no printing to take it from: every battle printed so far has the Siege
-- subtype CR 310.12 describes. data/cards/synthetic-besiege-the-front.json does
-- reach that shape on a real board (Pawl.CombatEffectSpec's BattleGrantRemoval),
-- so the arithmetic here is a sibling of a reachable state rather than of none.
candidateSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
candidateSpec s registry = Spec.describe s "Candidates" $ do
  Spec.it s "CR 310.12a a Siege offers its controller's opponents and not its controller" $ do
    siege <- siegePC s registry
    Spec.assertEqWith
      s
      "bob and carol"
      (Battle.protectorCandidates Teams.none siege S.alice [S.alice, S.bob, S.carol])
      [S.bob, S.carol]
  Spec.it s "CR 310.9a a battle with no battle types offers only its controller" $ do
    siege <- siegePC s registry
    Spec.assertEqWith
      s
      "alice alone"
      (Battle.protectorCandidates Teams.none siege {PC.subtypes = Set.empty} S.alice [S.alice, S.bob, S.carol])
      [S.alice]
  Spec.it s "CR 704.5x a departed player is not a candidate" $ do
    siege <- siegePC s registry
    Spec.assertEqWith
      s
      "carol alone, bob having left"
      (Battle.protectorCandidates Teams.none siege S.alice [S.alice, S.carol])
      [S.carol]
  -- CR 102.3 with CR 310.12a: a teammate is not an opponent, so a Siege cannot be
  -- protected by one. The same board as the first case with one thing changed --
  -- bob and alice are now on a team -- so the case cannot pass for want of a
  -- candidate: carol is still offered.
  Spec.it s "CR 310.12a a Siege does not offer its controller's teammate" $ do
    siege <- siegePC s registry
    Spec.assertEqWith
      s
      "carol alone, bob being alice's teammate"
      (Battle.protectorCandidates (Teams.MkTeams (Map.fromList [(S.alice, TeamId.MkTeamId 0), (S.bob, TeamId.MkTeamId 0), (S.carol, TeamId.MkTeamId 1)])) siege S.alice [S.alice, S.bob, S.carol])
      [S.carol]
  -- CR 310.11's second sentence, listed as CR 704.5x's and CR 704.5y's: the branch
  -- that puts the battle into its owner's graveyard. Held HERE rather than at the
  -- game level because it is
  -- unreachable there: a Siege's candidates are its controller's opponents still
  -- in the game, and a game in which its controller has no opponent left has
  -- already ended under CR 104.2a. Pawl.Engine.Sba routes an empty answer into
  -- the put-into-graveyard batch all the same, and states the argument for both
  -- branches of CR 310.9a.
  Spec.it s "CR 704.5x a Siege whose controller is alone has no candidate" $ do
    siege <- siegePC s registry
    Spec.assertEqWith s "nobody" (Battle.protectorCandidates Teams.none siege S.alice [S.alice]) []
  Spec.it s "CR 704.5y a Siege protected by its own controller needs repair" $ do
    siege <- siegePC s registry
    Spec.assertBool
      s
      (Battle.needsProtector Teams.none siege S.alice [S.alice, S.bob] False (Just S.alice))
      "the controller is not a legal protector of their own Siege"
  Spec.it s "CR 310.11 a legal designation needs no repair" $ do
    siege <- siegePC s registry
    Spec.assertBool
      s
      (not (Battle.needsProtector Teams.none siege S.alice [S.alice, S.bob] False (Just S.bob)))
      "bob is legal and is left alone"

-- CR 704.5x: the designation is repaired by a state-based action once the
-- designated player is no longer in the game.
repairSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
repairSpec s registry = Spec.describe s "Repair" $ do
  Spec.it s "CR 704.5x the repair reports that an action was performed" $ do
    (entered, _) <- castInvasionThreeSeated s registry (protectTo S.carol)
    -- CR 704.3: the check repeats until no state-based action is performed, so a
    -- pass that repaired a designation must SAY it acted. Read off
    -- performStateBasedActions' own answer, which is the value that loop reads;
    -- nothing else about the repair is visible to it.
    let gone = S.departs Departure.Type.Conceded S.carol entered
        (acted, _) = S.runPureWith S.identityAnswer gone Sba.performStateBasedActions
    Spec.assertBool s acted "the pass reports the repair"

-- CR 310.5: battles can be attacked, and everything that follows from WHOM they
-- are attacked through -- CR 310.9b's protector rule, CR 310.9c's blocking, CR
-- 310.9d with CR 508.5's defending player, and CR 704.5x's rider once one of them
-- is under attack.
attackSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
attackSpec s registry = Spec.describe s "Attacking" $ do
  -- The same removal at THREE DEFENDING SEATS, which is where CR 802.2a bites:
  -- both of alice's opponents defend (CR 802.2, the default option) and bob heads
  -- CR 802.4's APNAP order, so "the protector of the battle that creature was
  -- attacking" and "the first defending player" name different seats. CR 508.5's
  -- second sentence is what has to be read off the recorded seat, there being no
  -- live battle left to ask.
  --
  -- The pair differs in one thing -- which of bob and carol holds the Swamp -- and
  -- the two readings answer it the opposite way round, so neither case can pass on
  -- an engine that had merely lost swampwalk.
  Spec.it s "CR 802.2a the departed battle's attacker reads its PROTECTOR, not the first defending player" $ do
    (gs, battle, mine, _, hers) <- battleCombatOf s registry S.carol S.carol ["Bog Wraith"] ["Island"] ["Goblin Piker", "Swamp"]
    (armed, bolts) <- twoBolts s registry (bothDefending gs)
    case (mine, hers, bolts) of
      ([wraith], blocker : _, [one, two]) -> do
        let after = S.runPure (attackTheBattle battle) armed (Combat.declareAttackers S.manaPerformer S.alice)
            burned = castAt battle S.alice two (castAt battle S.alice one after)
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.carol (Map.singleton blocker (Set.singleton wraith)) burned)) "carol protected the Siege, so her Swamp stops the block"
        -- The premises, after the gameplay assertion so neither can absorb a
        -- mutation of it.
        Spec.assertEqWith s "CR 802.2: both opponents defend, bob first" (Combat.Type.defenders (GameState.combat burned)) [S.bob, S.carol]
        Spec.assertBool s (not (Set.member battle (GameState.battlefield burned))) "CR 310.12b: the second Bolt took the last defense counter"
        Spec.assertEqWith
          s
          "CR 506.4c: still attacking the battle it was declared against"
          (Map.lookup wraith (Combat.Type.attackers (GameState.combat burned)))
          (Just (AttackTarget.OfBattle battle))
      _ -> Spec.assertFailure s "fixture should have a Wraith, a blocker and two Bolts"

  -- The same question one step EARLIER: the Siege leaves during CR 508.1i's mana
  -- window, before CR 508.1k makes the Wraith an attacking creature, so no live
  -- battle is left when the declaration records the seat and raises its CR
  -- 508.2b event. Oppressive Rays taxes the Wraith {3} whatever it attacks;
  -- Liquimetal Coating has made the Siege an artifact, and alice pays with
  -- Krark-Clan Ironworks sacrificing it plus her Mountain. CR 608.2h's last known
  -- protector is the defending player (Defender.playerOf's battle arm), as it is
  -- for a battle removed later.
  --
  -- This case is the proof: carol may block only as the Wraith's defending player
  -- (CR 509.1a), and bob, who heads the defending players, holds the Swamp. An
  -- engine answering nobody forbids carol's block, and one answering bob lets his
  -- Swamp forbid it.
  Spec.it s "CR 508.1i / 508.5 a Siege sacrificed to pay the attack tax leaves its protector defending" $ do
    (after, battle, wraith, blocker) <- sacrificedMidToll s registry ["Swamp"] ["Goblin Piker", "Island"]
    Spec.assertBool s (Combat.legalBlockDeclaration S.carol (Map.singleton blocker (Set.singleton wraith)) after) "carol defends against the Wraith, and bob's Swamp is not hers"
    -- The premises, after the gameplay assertion so neither can absorb a
    -- mutation of it.
    Spec.assertEqWith s "CR 508.2b: the declaration names carol as the defending player" (declaredDefender wraith after) [S.carol]
    Spec.assertBool s (not (Set.member battle (GameState.battlefield after))) "CR 701.21a: the Ironworks ate the Siege"
    Spec.assertEqWith
      s
      "CR 508.1k: attacking the battle it was declared against"
      (Map.lookup wraith (Combat.Type.attackers (GameState.combat after)))
      (Just (AttackTarget.OfBattle battle))
  Spec.it s "CR 702.14c and the same toll board with the Swamp moved to carol forbids the block" $ do
    -- The other half, differing in which of bob and carol holds the Swamp:
    -- swampwalk reads carol's lands, so a legal block above was not an engine that
    -- had merely lost swampwalk.
    (after, battle, wraith, blocker) <- sacrificedMidToll s registry ["Island"] ["Goblin Piker", "Swamp"]
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.carol (Map.singleton blocker (Set.singleton wraith)) after)) "carol protected the Siege, so her Swamp stops the block"
    Spec.assertBool s (not (Set.member battle (GameState.battlefield after))) "the Siege is gone here too"

  Spec.it s "CR 508.4 the other road into combat records the same two seats" $ do
    -- CR 506.4's comparand is written by both writers of `attackers`, and a
    -- creature put onto the battlefield attacking never went through CR 508.1b.
    -- Read off the RECORD here; the Ninja pair above is where this road's record
    -- is proved to matter.
    (gs, battle, mine, _, _) <- battleCombatOf s registry S.carol S.carol ["Goblin Piker"] [] []
    case mine of
      [arrival] -> do
        let after = S.runPure (attackTheBattle battle) gs (Combat.putOntoBattlefieldAttacking Combat.Any arrival)
            combat = GameState.combat after
        Spec.assertEqWith s "it is attacking the Siege" (Map.lookup arrival (Combat.Type.attackers combat)) (Just (AttackTarget.OfBattle battle))
        -- The two records hold DIFFERENT seats, which is the whole reason there
        -- are two: CR 310.9d makes the defending player the protector, and rule
        -- 506.4's controller clause asks about alice.
        Spec.assertEqWith s "CR 506.4: the battle's controller" (Map.lookup arrival (Combat.Type.attackedControlledBy combat)) (Just S.alice)
        Spec.assertEqWith s "CR 802.2a: its protector, and not the same player" (Map.lookup arrival (Combat.Type.attackedUnder combat)) (Just S.carol)
      _ -> Spec.assertFailure s "fixture should have exactly one creature"

-- CR 310.6 / CR 120.3h: damage dealt to a battle removes that many defense
-- counters.
damageSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
damageSpec s registry = Spec.describe s "Damage" $ do
  -- CR 510.1b: Archpriest of Shadows ({3}{B}{B} 4/4, "Whenever this creature
  -- deals combat damage to a player or battle, return target creature card from
  -- your graveyard to the battlefield", Oracle text checked against Scryfall
  -- 2026-09-30) attacks the Siege unblocked, so the battle is the only thing it
  -- deals damage to, and its trigger returns the Hill Giant.
  Spec.it s "CR 510.1b combat damage to a battle fires a player-or-battle trigger" $ do
    archpriest <- S.printingOf s registry "Archpriest of Shadows"
    giant <- S.printingOf s registry "Hill Giant"
    (gs0, battle, mine, _, _) <- battleCombat s registry S.carol S.carol [archpriest] [] []
    let (giantId, gs) = S.addGraveyardCard giant S.alice gs0
        returning :: Prompt.Prompt r -> r
        returning p = case p of
          Prompt.ChooseTargets _ _ _ sets -> S.preferring ((== Just giantId) . Recipient.objectOf) sets
          _ -> attackTheBattle battle p
        attacked = S.runPure returning gs (Combat.declareAttackers S.manaPerformer S.alice)
        dealt = S.runPure returning attacked (Monad.void Damage.dealCombatDamage >> Engine.settleForPriority)
        resolved = S.runPure returning dealt Stack.resolveTop
    Spec.assertEqWith s "CR 603.2 the Hill Giant returned to the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Hill Giant")) S.alice resolved) 1
    Spec.assertEqWith s "the Archpriest attacked the battle" (fmap (\a -> Map.lookup a (Combat.Type.attackers (GameState.combat attacked))) mine) [Just (AttackTarget.OfBattle battle)]
    Spec.assertEqWith s "CR 310.6 and dealt it 4 of its 5 defense" (S.counterOf CounterKind.Defense battle dealt) 1
  Spec.it s "CR 506.4 a battle that has left the battlefield is assigned no combat damage" $ do
    -- THE FALSIFIER for combatRecipient answering with the battle unconditionally.
    -- The same declaration with the Siege destroyed inside CR 510.4's window, so CR
    -- 510.1b gives the attacker nothing to assign to and no damage event is built.
    piker <- S.printingOf s registry "Goblin Piker"
    (gs, battle, _, _, _) <- battleCombat s registry S.carol S.carol [piker] [] []
    let attacked = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.manaPerformer S.alice)
        killed = S.runPure S.identityAnswer attacked (Event.destroy Regenerability.Regenerable [battle])
        dealt = S.runPure S.identityAnswer killed (Monad.void Damage.dealCombatDamage)
    Spec.assertEqWith s "no damage was dealt at all" (S.damageEventsOf dealt) []

-- CR 310.12b, CR 310.7 / 704.5v and CR 310.8 / 704.5w: what happens when the last
-- defense counter comes off. The first two rules are only jointly observable --
-- 704.5v alone would send the Siege to a graveyard where 310.12b exiles it -- so
-- every gameplay-level case here asserts the DESTINATION zone rather than merely
-- that the battle left. 704.5w's battle-type split has no printing to reach it --
-- data/cards/synthetic-besiege-the-front.json grants the card type but hands over
-- five defense counters with it -- so the three classifier cases near the end read
-- Battle.defeated directly.
defeatSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
defeatSpec s registry = Spec.describe s "Defeat" $ do
  Spec.it s "CR 310.12b a Siege has the intrinsic defeat ability" $ do
    siege <- siegePC s registry
    Spec.assertEqWith
      s
      "one ability, conditioned on the last defense counter"
      (fmap TriggeredAbility.condition (Battle.triggeredAbilitiesOf siege))
      [TriggerCondition.SelfLastCounterRemoved (SelfCountersRemoved.MkSelfCountersRemoved CounterKind.Defense Zone.Battlefield)]
  Spec.it s "CR 310.12b a battle with no battle types does not" $ do
    -- Rule 310.12b says "Sieges", not "battles", and this is the falsifier for
    -- gating the mint on the card type instead: the same real projection with its
    -- subtypes stripped, which candidateSpec above uses for CR 310.9a's other
    -- branch.
    siege <- siegePC s registry
    Spec.assertEqWith s "no intrinsic ability" (Battle.triggeredAbilitiesOf siege {PC.subtypes = Set.empty}) []
  Spec.it s "CR 704.5v a battle at defense 0 with nothing pending is named by the state-based action" $ do
    -- The clause's own reading, at the level Pawl.Engine.Battle states it.
    -- Unreachable for a SIEGE in a real game, and that is rule 704.5v's design: the
    -- counters hitting 0 fires CR 310.12b, whose exemption holds the battle there
    -- until the ability exiles it. What this pins is that the clause exists, so
    -- that the case below shows the RIDER and not the clause doing the holding.
    (entered, battle) <- castInvasionThreeSeated s registry (protectTo S.carol)
    let drained = drain battle entered
    Spec.assertEqWith s "it is named" (Battle.defeated (Projection.projectAll drained) (Event.triggeredSources drained) drained) [battle]
  Spec.it s "CR 704.3 and the pass that buries it reports that an action was performed" $ do
    -- CR 704.3: the check repeats while a state-based action was performed, so a
    -- pass whose only action is a defeat must SAY it acted -- otherwise
    -- Engine.performSettle stops one pass early and CR 514.3a's cleanup exception
    -- never fires. The same drained board as the case above, whose classifier
    -- assertion is what says the defeat is the only thing this pass does.
    (entered, battle) <- castInvasionThreeSeated s registry (protectTo S.carol)
    let drained = drain battle entered
        (acted, after) = S.runPureWith S.identityAnswer drained Sba.performStateBasedActions
    Spec.assertBool s acted "the pass reports the defeat"
    Spec.assertBool s (not (S.onBattlefield battle after)) "and the battle has left the battlefield"
  Spec.it s "CR 704.5v and is NOT while its defeat ability is still owed a resolution" $ do
    -- THE FALSIFIER for the rider, on the identical board with CR 310.12b's own
    -- event still unscanned. Pawl.Engine.Engine.performSettle runs the state-based
    -- action pass BEFORE placePendingTriggers, so this window is real rather than
    -- hypothetical, and it is the whole reason the Siege above reaches exile.
    (entered, battle) <- castInvasionThreeSeated s registry (protectTo S.carol)
    let removed = removal battle (drain battle entered)
        owing = Event.triggeredSources removed
    Spec.assertBool s (Battle.awaitingAbility owing removed battle) "the ability has triggered"
    Spec.assertEqWith s "so nothing is buried" (Battle.defeated (Projection.projectAll removed) owing removed) []
  Spec.it s "CR 704.5w a NON-Siege battle at defense 0 is buried anyway" $ do
    -- THE PROVING CASE for rule 704.5w, and the case above is its discriminator:
    -- the same fixture, the same drain and the same unscanned event, differing in
    -- the battle types ALONE. Without that pair a fix that dropped the exemption
    -- outright would pass this one.
    --
    -- The projection is the real Siege's with its subtypes stripped, the fixture
    -- candidateSpec and the CR 310.12b case above already build for CR 310.9a's
    -- other branch: rule 310.12 says only that SOME battles are Sieges, so a
    -- battle with no battle types is unprinted rather than rules-forbidden.
    -- Stripping them also takes CR 310.12b's ability away, which is what makes
    -- the board coherent -- 704.5w's world is one where no defeat ability is owed.
    (entered, battle) <- castInvasionThreeSeated s registry (protectTo S.carol)
    let removed = removal battle (drain battle entered)
        owing = Event.triggeredSources removed
        pcs = Map.adjust (\pc -> pc {PC.subtypes = Set.empty}) battle (Projection.projectAll removed)
    Spec.assertBool s (Battle.awaitingAbility owing removed battle) "an ability has still triggered"
    Spec.assertEqWith s "and CR 704.5w exempts nothing" (Battle.defeated pcs owing removed) [battle]

-- Cast the spell in `caster`'s hand at `battle`, then settle: the spell resolves,
-- CR 310.6 takes its counters off, and CR 310.12b's ability -- if the last one came
-- off -- is placed and resolved inside the same loop. Every case above reads the
-- board that loop leaves.
castAt :: ObjectId.ObjectId -> PlayerId.PlayerId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
castAt battle caster spell gs =
  let answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToBattle battle))) sets
        _ -> S.identityAnswer p
      cast = S.runPure answer gs (S.cast caster spell)
   in S.runPure answer cast Engine.priorityLoop

-- Set a battle's defense counters to none without any damage having been dealt, so
-- the two CR 704.5v cases differ in the unscanned event log ALONE. The Siege's
-- enters ability is placed and resolved first: left unscanned, it is an ability
-- the battle is owed, and CR 704.5v holds the battle for it too.
drain :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
drain oid gs =
  let entered = S.runPure S.identityAnswer gs (Engine.placePendingTriggers >> Stack.resolveTop)
      empty obj = obj {Object.counters = Map.insert CounterKind.Defense 0 (Object.counters obj)}
   in entered {GameState.objects = Map.adjust empty oid (GameState.objects entered)}

-- Record CR 310.12b's trigger event on the drained battle, unscanned: the removal
-- of its last two defense counters.
removal :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
removal battle = Event.recordEvent (GameEvent.CountersRemoved (CounterChange.MkCounterChange battle CounterKind.Defense 2 0))

-- Two more Mountains for alice and two Lightning Bolts in her hand, so a case can
-- burn a defense-5 Siege off the battlefield INSIDE the declare attackers step: 3
-- then 3, the second clamped by the floor Pawl.Engine.Damage documents. Two Bolts
-- rather than a Bolt and a Firebolt, since Firebolt is a SORCERY and CR 307.1
-- keeps it out of combat.
twoBolts ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  GameState.GameState ->
  m (GameState.GameState, [ObjectId.ObjectId])
twoBolts s registry gs = do
  mountain <- S.printingOf s registry "Mountain"
  bolt <- S.printingOf s registry "Lightning Bolt"
  let landed = List.foldl' (\g _ -> snd (S.addPermanent mountain S.alice g)) gs [1 :: Int, 2]
      (one, g1) = S.addHandCard bolt S.alice landed
      (two, g2) = S.addHandCard bolt S.alice g1
  pure (g2, [one, two])

-- Answer CR 508.1b's announcement with the given battle whenever it is offered,
-- and everything else aggressively -- so the DeclareAttackers prompt takes every
-- candidate. Naming a target that is not offered is the point on the falsifier
-- boards: announceAttackTarget filters it back to the defending player, which is
-- what makes those cases read the candidate list rather than the answerer.
attackTheBattle :: ObjectId.ObjectId -> Prompt.Prompt r -> r
attackTheBattle battle p = case p of
  Prompt.ChooseAttackTarget _ _ _ options ->
    Maybe.fromMaybe
      (NonEmpty.head options)
      (List.find (== AttackTarget.OfBattle battle) (NonEmpty.toList options))
  _ -> S.aggressiveAnswer p

-- The three-seat Siege board (carol protects, bob and carol both defend) with
-- alice's Bog Wraith under an Oppressive Rays, a Krark-Clan Ironworks, a Liquimetal
-- Coating and one untapped Mountain; bob and carol hold `theirs` and `hers`. The
-- Coating makes the Siege an artifact at beginning of combat, then alice attacks
-- the Siege and pays the Rays' {3} from the Mountain first and the Ironworks
-- second, feeding it the Siege. Gives back the board after the declaration, the
-- departed Siege's id, the Wraith, and carol's first permanent.
sacrificedMidToll ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  [String] ->
  [String] ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
sacrificedMidToll s registry theirs hers = do
  rays <- S.printingOf s registry "Oppressive Rays"
  coating <- S.printingOf s registry "Liquimetal Coating"
  (gs, battle, mine, _, carols) <- battleCombatOf s registry S.carol S.carol ["Bog Wraith", "Krark-Clan Ironworks", "Liquimetal Coating", "Mountain"] theirs hers
  case (mine, carols, Face.activatedAbilities (S.combinedFace coating)) of
    ([wraith, ironworks, coatingId, mountain], blocker : _, coat : _) -> do
      let (aura, withAura) = S.addPermanent rays S.alice gs
          board = S.attach aura wraith (bothDefending withAura)
          atBeginning = board {GameState.phase = Phase.Combat CombatStep.BeginningOfCombat, GameState.priority = Just S.alice}
          coatTheSiege :: Prompt.Prompt r -> r
          coatTheSiege p = case p of
            Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, legal) -> Set.filter ((== Just battle) . Recipient.objectOf) legal) asked
            _ -> S.identityAnswer p
          coated = S.runPure coatTheSiege atBeginning (Activate.activateAbility S.alice coatingId coat >> Stack.resolveTop)
          declaring = coated {GameState.phase = Phase.Combat CombatStep.DeclareAttackers}
          -- CR 605.3a's window pinned by identity: the Mountain while it is
          -- offered, then the Ironworks, whose sacrifice takes the Siege.
          pay :: Prompt.Prompt r -> r
          pay p = case p of
            Prompt.ChooseManaSource _ _ candidates ->
              List.find (== mountain) (NonEmpty.toList candidates) Applicative.<|> List.find (== ironworks) (NonEmpty.toList candidates)
            Prompt.ChooseSacrifices _ _ _ candidates _ _ -> Set.filter (== battle) (Set.fromList candidates)
            _ -> attackTheBattle battle p
          after = S.runPure pay declaring (Combat.declareAttackers S.manaPerformer S.alice)
      Spec.assertBool s (Set.member CardType.Artifact (Projection.cardTypesOf battle declaring)) "CR 205.1b: the Coating made the Siege an artifact"
      pure (after, battle, wraith, blocker)
    _ -> Spec.assertFailure s "fixture should have a Wraith, an Ironworks, a Coating, a Mountain and a blocker"

-- The defending player each CR 508.2b event names for `attacker`.
declaredDefender :: ObjectId.ObjectId -> GameState.GameState -> [PlayerId.PlayerId]
declaredDefender attacker gs =
  let defenderIn event = case event of
        GameEvent.AttackerDeclared declared | AttackerDeclared.attacker declared == attacker -> Just (AttackerDeclared.defender declared)
        _ -> Nothing
   in Maybe.mapMaybe defenderIn (S.eventsOf gs)

-- CR 802.2: both of alice's opponents defending, bob ahead of carol in CR 101.4's
-- APNAP order, on a board battleCombat left with one designated seat.
bothDefending :: GameState.GameState -> GameState.GameState
bothDefending gs = gs {GameState.combat = (GameState.combat gs) {Combat.Type.defenders = [S.bob, S.carol]}}

-- battleCombat by card NAME, for the cases whose printings differ per seat.
battleCombatOf ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  PlayerId.PlayerId ->
  PlayerId.PlayerId ->
  [String] ->
  [String] ->
  [String] ->
  m (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId], [ObjectId.ObjectId], [ObjectId.ObjectId])
battleCombatOf s registry protector defender mine theirs hers = do
  let printings = Monad.mapM (S.printingOf s registry)
  mine2 <- printings mine
  theirs2 <- printings theirs
  hers2 <- printings hers
  battleCombat s registry protector defender mine2 theirs2 hers2

-- alice is active and controls a Siege that `protector` protects (CR 310.9a),
-- plus one Settled permanent per printing in `mine`; bob and carol get one each
-- per printing in `theirs` and `hers`. The board sits in the declare attackers
-- step with `defender` already designated -- stated rather than derived for
-- S.combatBoardOf's reason, since a direct-call test never runs CR 703.4h's
-- turn-based action.
--
-- THREE seats throughout, and that is the whole point of the group: at two seats
-- the Siege's protector, the defending player and "an opponent of the battle's
-- controller" are one person, and every case above is about telling them apart.
-- carol protects, bob is the other opponent, alice controls the battle and
-- attacks it.
battleCombat ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  PlayerId.PlayerId ->
  PlayerId.PlayerId ->
  [Printing.Printing] ->
  [Printing.Printing] ->
  [Printing.Printing] ->
  m (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId], [ObjectId.ObjectId], [ObjectId.ObjectId])
battleCombat s registry protector defender mine theirs hers = do
  (entered, battle) <- castInvasionThreeSeated s registry (protectTo protector)
  let addAll pid ps g =
        List.foldl'
          (\(ids, g1) p -> let (oid, g2) = S.addPermanent p pid g1 in (ids <> [oid], g2))
          ([], g)
          ps
      (ours, gs1) = addAll S.alice mine entered
      (yours, gs2) = addAll S.bob theirs gs1
      (theirsToo, gs3) = addAll S.carol hers gs2
  pure
    ( gs3
        { GameState.activePlayer = S.alice,
          GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
          GameState.combat = (GameState.combat gs3) {Combat.Type.defenders = [defender]}
        },
      battle,
      ours,
      yours,
      theirsToo
    )

-- Cast Invasion of Dominaria on the two-seat board and settle the stack, giving
-- back the state and the battle's id. A Plains sits in the library so the printed
-- trigger's draw has a card to find -- CR 704.5b would otherwise end the game
-- rather than the assertion under test.
castInvasion ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (GameState.GameState, ObjectId.ObjectId)
castInvasion s registry = do
  plains <- S.printingOf s registry "Plains"
  invasion <- S.printingOf s registry "Invasion of Dominaria"
  let stocked = snd (S.addLibraryCard plains S.alice (S.landsInPlay plains 3))
      (gs, spellId) = S.handOne invasion stocked
      cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
      after = S.runPure S.identityAnswer cast Stack.resolveTop
  named s after

-- castInvasion under an answerer that treats a CR 616.1e ordering prompt as a
-- failure.
castInvasionRefusingToOrder ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (GameState.GameState, ObjectId.ObjectId)
castInvasionRefusingToOrder s registry = do
  plains <- S.printingOf s registry "Plains"
  invasion <- S.printingOf s registry "Invasion of Dominaria"
  let stocked = snd (S.addLibraryCard plains S.alice (S.landsInPlay plains 3))
      (gs, spellId) = S.handOne invasion stocked
      cast = S.runPure refusesToOrder gs (S.cast S.alice spellId)
      after = S.runPure refusesToOrder cast Stack.resolveTop
  named s after

-- castInvasion's three-seat twin, under a given answerer. Three seats make this a
-- multiplayer game (CR 800.1), which is what leaves alice's Siege two legal
-- protectors instead of one.
castInvasionThreeSeated ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  (forall r. Prompt.Prompt r -> r) ->
  m (GameState.GameState, ObjectId.ObjectId)
castInvasionThreeSeated s registry answer = do
  plains <- S.printingOf s registry "Plains"
  invasion <- S.printingOf s registry "Invasion of Dominaria"
  let lands = List.foldl' (\g _ -> snd (S.addPermanent plains S.alice g)) S.threePlayerGame [1 :: Int .. 3]
      stocked = snd (S.addLibraryCard plains S.alice lands)
      (spellId, handed) = S.addHandCard invasion S.alice stocked
      ready =
        handed
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      cast = S.runPure answer ready (S.cast S.alice spellId)
      after = S.runPure answer cast Stack.resolveTop
  named s after

named :: (Monad m) => Spec.Spec m n -> GameState.GameState -> m (GameState.GameState, ObjectId.ObjectId)
named s gs = case battleOf gs of
  Nothing -> Spec.assertFailure s "Invasion of Dominaria did not reach the battlefield"
  Just oid -> pure (gs, oid)

-- The battlefield's one battle, by the card type rule 310 keys on.
battleOf :: GameState.GameState -> Maybe ObjectId.ObjectId
battleOf gs =
  let pcs = Projection.projectAll gs
      battles = filter (\oid -> maybe False Battle.isBattle (Map.lookup oid pcs)) (Set.toAscList (GameState.battlefield gs))
   in case battles of
        [oid] -> Just oid
        _ -> Nothing

protectorOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe PlayerId.PlayerId
protectorOf oid gs = Object.protector =<< Map.lookup oid (GameState.objects gs)

-- The real Siege's projection, taken off a board a cast produced.
siegePC :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m PC.ProjectedCharacteristics
siegePC s registry = do
  (after, oid) <- castInvasion s registry
  pure (Projection.project oid after)

-- Name a protector and answer everything else the ordinary way, the shape
-- S.attackTo takes for CR 507.1's defending player.
-- CR 310.9a is not a replacement effect (see Event.designateProtector), so nothing
-- competes with CR 310.4b's counters for a CR 616.1e ordering. An answerer that
-- refuses to order replacements is how that is asserted rather than assumed: when
-- the protector choice WAS an EntryRewrite, both rows landed in
-- ReplacementBucket.Other and entering a battle raised this prompt every time.
refusesToOrder :: Prompt.Prompt r -> r
refusesToOrder p = case p of
  Prompt.ChooseReplacement {} -> error "entering a battle must not ask CR 616.1e to order anything"
  _ -> S.identityAnswer p

protectTo :: PlayerId.PlayerId -> Prompt.Prompt r -> r
protectTo who p = case p of
  Prompt.ChooseProtector {} -> who
  _ -> S.identityAnswer p
