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
-- built on, and is the only battle in `data/cards`. {2}{W} Battle -- Siege,
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
-- is a battle, so every case below reads rule 310 rather than the attacker.
--
-- And how a battle is defeated: CR 310.6 / 120.3h's damage removing defense
-- counters, CR 115.4's "any target" admitting one, CR 310.12b's intrinsic Siege
-- ability, and CR 310.7 / 704.5v's and CR 310.8 / 704.5w's state-based actions.
-- Those are damageSpec and defeatSpec below.
--
-- Two thefts join them for rule 506.4's and rule 704.5y's control changes.
-- Zealous Conscripts ({4}{R} Creature, an entry trigger that gains control of
-- target permanent until end of turn) is what lets a battle's protector take the
-- battle at sorcery speed; Word of Seizing ({3}{R}{R} Instant, split second,
-- untap target permanent and gain control of it until end of turn) is what moves
-- a controller once attackers are declared. Every other GainControl in
-- `data/cards` on 2026-09-02 is sorcery-speed, hangs off a trigger the attack
-- cannot raise, or is filtered to a type a battle does not have -- Ray of Command
-- to a creature, Aladdin to an artifact, Aura Graft to an Aura. Another instant,
-- or an instant-speed activation naming a permanent, would serve here too.
--
-- And CR 509.1a's third subject, "a battle they protect", which is a filter atom
-- (Filter.IsAttackingBattle) rather than a rule of combat: filterSpec below, whose
-- producer is the one SYNTHETIC card this file uses, Synthetic Bulwark Snare. That
-- group's own comment records the Scryfall queries behind calling it synthetic. It
-- also holds CR 506.4's stops-being-a-battle clause, read through that same atom,
-- which Aura Graft and Song of the Dryads reach at instant speed.
--
-- Lightning Bolt ({R} Instant, "deals 3 damage to any target") and Firebolt ({R}
-- Sorcery, "deals 2 damage to any target") are the pool's two plainest CR 115.4
-- spells, and 3 + 2 is exactly the Siege's printed defense of 5. Distinct amounts
-- on purpose: a defense-5 battle taking 5 at once could not tell "removed all the
-- counters" from "removed the right number". attackSpec borrows the Bolt too, cast
-- twice inside the declare attackers step, since Firebolt is a sorcery and CR
-- 307.1 keeps it out of combat.
--
-- And the second half of CR 310.12b's sentence, "then you may cast it transformed
-- without paying its mana cost": CR 608.2g's offered cast, CR 118.9's alternative
-- cost, CR 712.11a / 712.8c's back-face spell and CR 712.13's back-face permanent,
-- all in defeatSpec below.
module Pawl.BattleSpec where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Battle as Battle
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Sba as Sba
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterChange as CounterChange
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Defense as Defense
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TeamId as TeamId
import qualified Pawl.Types.Teams as Teams
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Battle" $ do
  entrySpec s registry
  protectorSpec s registry
  candidateSpec s registry
  repairSpec s registry
  attackSpec s registry
  filterSpec s registry
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
    -- restriction rather than a choice; protectorSpec below is where the choice
    -- is made observable.
    Spec.assertEqWith s "bob protects it" (protectorOf oid after) (Just S.bob)
  Spec.it s "CR 616.1e entering a battle orders no replacement effects" $ do
    (after, oid) <- castInvasionRefusingToOrder s registry
    -- Both halves still landed, so this is not passing by never entering.
    Spec.assertEqWith s "five defense counters" (S.counterOf CounterKind.Defense oid after) 5
    Spec.assertEqWith s "and a protector" (protectorOf oid after) (Just S.bob)
  Spec.it s "the printed enters trigger still fires: gain 4 life and draw a card" $ do
    (after, _) <- castInvasion s registry
    let settled = S.runPure S.identityAnswer after Engine.priorityLoop
    Spec.assertEqWith s "alice gained 4" (S.lifeOf S.alice settled) (Just 24)
    Spec.assertEqWith s "and drew one" (length (Game.zoneMembers Zone.Hand S.alice settled)) 1

-- CR 310.9a's choice, made observable. Three seats, because CR 102.2's two-player
-- game leaves a Siege exactly one legal protector and a one-candidate ask decides
-- nothing.
protectorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
protectorSpec s registry = Spec.describe s "Protector" $ do
  Spec.it s "CR 310.9a the controller chooses which opponent protects it" $ do
    (toBob, oidB) <- castInvasionThreeSeated s registry (protectTo S.bob)
    (toCarol, oidC) <- castInvasionThreeSeated s registry (protectTo S.carol)
    -- Every input the same but the answer to one prompt, the shape M5.6d's
    -- defending-player proof takes: this is what a two-seat board could not
    -- distinguish from an elision.
    Spec.assertEqWith s "bob when bob is named" (protectorOf oidB toBob) (Just S.bob)
    Spec.assertEqWith s "carol when carol is named" (protectorOf oidC toCarol) (Just S.carol)
  Spec.it s "CR 310.12a the controller is never offered, even with three seats" $ do
    -- An interpreter that names the controller anyway is filtered, not obeyed:
    -- Battle.designateProtector falls back to the head of the candidate list.
    (chosen, oid) <- castInvasionThreeSeated s registry (protectTo S.alice)
    Spec.assertBool s (protectorOf oid chosen /= Just S.alice) "alice does not protect her own Siege"
    Spec.assertBool s (protectorOf oid chosen `elem` [Just S.bob, Just S.carol]) "an opponent does"

-- CR 310.9a's candidate rule at the level Pawl.Engine.Battle states it, which is
-- the arithmetic the entry choice and the CR 704.5x re-choice SHARE -- so a drift
-- between them would show here.
--
-- The projections are the REAL card's, taken off the board a cast produced, so
-- these cases cannot pass against a Siege pawl does not actually build. The
-- no-battle-types half is that same projection with its subtypes stripped, which
-- has no printing to take it from: every battle printed so far has the Siege
-- subtype CR 310.12 describes.
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
repairSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
repairSpec s registry = Spec.describe s "Repair" $ do
  Spec.it s "CR 704.5x a battle whose protector leaves the game gets a new one" $ do
    (entered, oid) <- castInvasionThreeSeated s registry (protectTo S.carol)
    Spec.assertEqWith s "carol protects it to begin with" (protectorOf oid entered) (Just S.carol)
    let gone = S.departs Departure.Type.Conceded S.carol entered
        repaired = S.runPure S.identityAnswer gone Sba.checkStateBasedActions
    -- bob is the only opponent left, so the re-choice is elided and its answer is
    -- forced -- which is what makes this assert the SBA firing rather than an
    -- answerer's preference.
    Spec.assertEqWith s "bob protects it now" (protectorOf oid repaired) (Just S.bob)
    Spec.assertBool s (S.onBattlefield oid repaired) "and the battle is still on the battlefield"
  Spec.it s "CR 704.5x a legal protector is not re-chosen" $ do
    (entered, oid) <- castInvasionThreeSeated s registry (protectTo S.carol)
    -- Nobody has left, so CR 704.5x does not apply and the pass must not ask
    -- again. Run under an answerer that would name BOB if it were asked: with all
    -- three seats filled the re-choice would have two candidates and could not be
    -- elided, so the designation still standing at carol is what says the
    -- state-based action declined to fire. Asserting against S.identityAnswer
    -- instead would pass whether or not it fired, since bob leaving leaves carol
    -- the only candidate either way.
    let checked = S.runPure (protectTo S.bob) entered Sba.checkStateBasedActions
    Spec.assertEqWith s "carol still protects it" (protectorOf oid checked) (Just S.carol)
  Spec.it s "CR 704.5x the repair reports that an action was performed" $ do
    (entered, _) <- castInvasionThreeSeated s registry (protectTo S.carol)
    -- CR 704.3: the check repeats until no state-based action is performed, so a
    -- pass that repaired a designation must SAY it acted. Read off
    -- performStateBasedActions' own answer, which is the value that loop reads;
    -- nothing else about the repair is visible to it.
    let gone = S.departs Departure.Type.Conceded S.carol entered
        (acted, _) = S.runPureWith S.identityAnswer gone Sba.performStateBasedActions
    Spec.assertBool s acted "the pass reports the repair"
  Spec.it s "CR 704.5y whole cards: a protector who steals the battle stops being its protector" $ do
    -- Rule 704.5y at gameplay level, which needs a control-change effect that can
    -- name a battle: Zealous Conscripts' GainControl draws from Pool.Permanents,
    -- and Target.permanentRecipients is the whole battlefield.
    --
    -- carol protects alice's Siege and then takes it, so she is a player who
    -- can't be its protector (CR 310.12a wants an opponent of its controller) --
    -- and rule 704.5y, which carries no attacking rider, re-chooses. The board is
    -- three-seated so that the re-choice has two candidates and cannot be elided.
    (board, battle, spell, decoy) <- conscriptedSiege s registry
    let stolen = runConscripts board spell battle
        elsewhere = runConscripts board spell decoy
    Spec.assertEqWith s "CR 704.5y: bob protects it now" (protectorOf battle stolen) (Just S.bob)
    Spec.assertEqWith s "the twin, one target apart: carol still protects the Siege she did not take" (protectorOf battle elsewhere) (Just S.carol)
    -- The two legs differ in the theft and in nothing else.
    Spec.assertEqWith s "carol controls the Siege on the theft leg" (Projection.controllerOf battle stolen) (Just S.carol)
    Spec.assertEqWith s "alice still controls it on the twin" (Projection.controllerOf battle elsewhere) (Just S.alice)
    Spec.assertEqWith s "and the twin's Plains is what carol took instead" (Projection.controllerOf decoy elsewhere) (Just S.carol)
    Spec.assertEqWith s "the Plains is alice's on the theft leg" (Projection.controllerOf decoy stolen) (Just S.alice)
    -- CR 704.5y repairs the designation rather than burying the battle: its own
    -- last clause is reached only where no player can be chosen.
    Spec.assertBool s (S.onBattlefield battle stolen) "the Siege is still on the battlefield"
  Spec.it s "CR 400.7 a battle that leaves the battlefield forgets its protector" $ do
    (entered, oid) <- castInvasionThreeSeated s registry (protectTo S.carol)
    Spec.assertEqWith s "carol protects it while it is on the battlefield" (protectorOf oid entered) (Just S.carol)
    -- CR 400.7 makes the object that reaches the graveyard a new one with no
    -- memory of this existence, so the designation may not ride along; a battle
    -- that returns chooses afresh (CR 310.9a). Asserted over the WHOLE object map
    -- rather than over `oid`, because the move mints a new id -- and the old
    -- incarnation lingering with a stale protector is exactly the failure this
    -- rules out.
    let killed = S.runPure S.identityAnswer entered (Event.destroy Regenerability.Regenerable [oid])
        designations = Maybe.mapMaybe Object.protector (Map.elems (GameState.objects killed))
    Spec.assertEqWith s "nobody protects anything now" designations []

-- CR 310.5: battles can be attacked, and everything that follows from WHOM they
-- are attacked through -- CR 310.9b's protector rule, CR 310.9c's blocking, CR
-- 310.9d with CR 508.5's defending player, and CR 704.5x's rider once one of them
-- is under attack.
attackSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
attackSpec s registry = Spec.describe s "Attacking" $ do
  Spec.it s "CR 310.5 / 310.9b a Siege is offered as an attack target through its PROTECTOR" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    (gs, battle, _, _, _) <- battleCombat s registry S.carol S.carol [piker] [] []
    -- The three seats really are three here, which is what makes the pair of
    -- assertions below distinguishable at all: alice controls the Siege, carol
    -- protects it, bob is neither.
    Spec.assertEqWith s "alice controls the Siege" (Projection.controllerOf battle gs) (Just S.alice)
    Spec.assertEqWith s "carol protects it" (protectorOf battle gs) (Just S.carol)
    -- CR 310.9b's "notably, a Siege battle can be attacked by its own controller":
    -- alice is the attacking player AND the battle's controller, and the battle is
    -- on her list anyway, because CR 310.12a put the protector among her
    -- opponents. Exact rather than a membership test, so a list that grew a
    -- spurious entry fails too.
    Spec.assertEqWith
      s
      "carol herself and the Siege she protects"
      (NonEmpty.toList (Combat.attackTargets S.carol gs))
      [AttackTarget.OfPlayer S.carol, AttackTarget.OfBattle battle]
  Spec.it s "CR 310.9b and NOT through an opponent who merely does not protect it" $ do
    -- THE FALSIFIER for the case above, and the reason it cannot pass vacuously:
    -- the same board read through the other opponent. bob is a legal defending
    -- player (CR 506.2a) with a legal attack available, so this is not "nothing
    -- can be attacked" -- it is the battle alone dropping off the list, which is
    -- CR 310.9b's "any attacking player for whom its protector is a defending
    -- player" and no wider a rule.
    piker <- S.printingOf s registry "Goblin Piker"
    (gs, battle, _, _, _) <- battleCombat s registry S.carol S.bob [piker] [] []
    Spec.assertEqWith s "carol still protects it" (protectorOf battle gs) (Just S.carol)
    Spec.assertEqWith
      s
      "bob alone"
      (NonEmpty.toList (Combat.attackTargets S.bob gs))
      [AttackTarget.OfPlayer S.bob]
  Spec.it s "CR 310.5 / 508.1b a creature is declared as attacking the battle" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    (gs, battle, mine, _, _) <- battleCombat s registry S.carol S.carol [piker] [] []
    case mine of
      [attacker] -> do
        let after = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.manaPerformer S.alice)
        Spec.assertEqWith
          s
          "the record names the battle"
          (Map.lookup attacker (Combat.Type.attackers (GameState.combat after)))
          (Just (AttackTarget.OfBattle battle))
        -- CR 508.1f: the creature really went through the declaration rather than
        -- the record being written past it.
        Spec.assertEqWith s "and it is tapped" (fmap Object.tapped (Game.lookupObject attacker after)) (Just TapState.Tapped)
      _ -> Spec.assertFailure s "fixture should have exactly one attacker"
  Spec.it s "CR 310.9b the identical announcement is refused when the protector is not the defending player" $ do
    -- THE FALSIFIER for the declaration: same board, same answerer, bob defending
    -- instead of carol. announceAttackTarget filters an answer outside CR 508.1b's
    -- list down to the defending player, so a recorded OfPlayer bob is the list
    -- refusing the battle -- not the answerer declining to name it.
    piker <- S.printingOf s registry "Goblin Piker"
    (gs, battle, mine, _, _) <- battleCombat s registry S.carol S.bob [piker] [] []
    case mine of
      [attacker] -> do
        let after = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.manaPerformer S.alice)
        Spec.assertEqWith
          s
          "the defending player instead"
          (Map.lookup attacker (Combat.Type.attackers (GameState.combat after)))
          (Just (AttackTarget.OfPlayer S.bob))
      _ -> Spec.assertFailure s "fixture should have exactly one attacker"

  Spec.it s "CR 310.9d / 508.5 the defending player of a creature attacking a battle is the battle's protector" $ do
    -- Bog Wraith is "Creature -- Wraith 3/3, Swampwalk" and nothing else, so this
    -- reads CR 508.5's defending player and no other text: CR 702.14c's swampwalk
    -- is exactly an ability of an attacking creature that refers to one. carol
    -- protects the Siege and controls the Swamp, so she may not block her own
    -- battle's attacker.
    (gs, battle, mine, _, hers) <- battleCombatOf s registry S.carol S.carol ["Bog Wraith"] [] ["Goblin Piker", "Swamp"]
    case (mine, hers) of
      ([wraith], blocker : _) -> do
        let after = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.manaPerformer S.alice)
        Spec.assertEqWith
          s
          "and it really is attacking the battle"
          (Map.lookup wraith (Combat.Type.attackers (GameState.combat after)))
          (Just (AttackTarget.OfBattle battle))
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.carol (Map.singleton blocker (Set.singleton wraith)) after)) "illegal"
      _ -> Spec.assertFailure s "fixture should have a Wraith and a blocker"
  Spec.it s "CR 702.14c the same attack is blocked normally when the protector's land is an Island" $ do
    -- THE FALSIFIER for the case above: the same board with the wrong land, so a
    -- "cannot block" that had nothing to do with landwalk would fail here too.
    (gs, battle, mine, _, hers) <- battleCombatOf s registry S.carol S.carol ["Bog Wraith"] [] ["Goblin Piker", "Island"]
    case (mine, hers) of
      ([wraith], blocker : _) -> do
        let after = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.manaPerformer S.alice)
        Spec.assertBool s (Combat.legalBlockDeclaration S.carol (Map.singleton blocker (Set.singleton wraith)) after) "legal"
      _ -> Spec.assertFailure s "fixture should have a Wraith and a blocker"
  Spec.it s "CR 310.9d the battle's CONTROLLER's lands are not the ones read" $ do
    -- THE FALSIFIER for reading CR 508.5's defending player off the battle's
    -- controller instead of its protector, which is the one substitution CR 310.9d
    -- exists to make. The Swamp has moved to alice, who controls the Siege and is
    -- attacking it; carol, who protects it, holds an Island. Nothing about the
    -- board changed except which of two distinct players owns the Swamp, and a
    -- controller-reading engine would call this block illegal.
    (gs, battle, mine, _, hers) <- battleCombatOf s registry S.carol S.carol ["Bog Wraith", "Swamp"] [] ["Goblin Piker", "Island"]
    case (mine, hers) of
      (wraith : _, blocker : _) -> do
        let after = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.manaPerformer S.alice)
        Spec.assertBool s (Combat.legalBlockDeclaration S.carol (Map.singleton blocker (Set.singleton wraith)) after) "legal"
      _ -> Spec.assertFailure s "fixture should have a Wraith and a blocker"

  Spec.it s "CR 506.4c / 508.5 the same block stays illegal once the Siege has left the battlefield" $ do
    -- Two Bolts take the Siege's five defense counters off inside the declare
    -- attackers step, CR 310.12b exiles it, and CR 506.4c leaves the Wraith an
    -- attacking creature that is attacking nothing. Its swampwalk still refers to
    -- a defending player, and CR 508.5's second sentence names the protector it
    -- had before it was removed from combat -- carol, the seat with the Swamp,
    -- read off Combat.attackedUnder. Reading the departed battle live finds no
    -- object at all and would call this block legal.
    (gs, battle, mine, _, hers) <- battleCombatOf s registry S.carol S.carol ["Bog Wraith"] [] ["Goblin Piker", "Swamp"]
    (armed, bolts) <- twoBolts s registry gs
    case (mine, hers, bolts) of
      ([wraith], blocker : _, [one, two]) -> do
        let after = S.runPure (attackTheBattle battle) armed (Combat.declareAttackers S.manaPerformer S.alice)
            burned = castAt battle S.alice two (castAt battle S.alice one after)
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.carol (Map.singleton blocker (Set.singleton wraith)) burned)) "carol's Swamp still stops the block"
        Spec.assertBool s (not (Set.member battle (GameState.battlefield burned))) "CR 310.12b: the second Bolt took the last defense counter"
        Spec.assertEqWith
          s
          "CR 506.4c: still attacking the battle it was declared against"
          (Map.lookup wraith (Combat.Type.attackers (GameState.combat burned)))
          (Just (AttackTarget.OfBattle battle))
      _ -> Spec.assertFailure s "fixture should have a Wraith, a blocker and two Bolts"
  Spec.it s "CR 702.14c the same removal with an ISLAND leaves the block legal" $ do
    -- THE FALSIFIER for the case above, and its anti-vacuity control: the same
    -- board differing in carol's one land. Without it, "illegal" above would pass
    -- on an engine that took the exiled battle to mean nothing may block that
    -- attacker at all -- CR 509.1a's "a battle they protect", which pawl checks
    -- through Combat.defenders rather than per pair.
    (gs, battle, mine, _, hers) <- battleCombatOf s registry S.carol S.carol ["Bog Wraith"] [] ["Goblin Piker", "Island"]
    (armed, bolts) <- twoBolts s registry gs
    case (mine, hers, bolts) of
      ([wraith], blocker : _, [one, two]) -> do
        let after = S.runPure (attackTheBattle battle) armed (Combat.declareAttackers S.manaPerformer S.alice)
            burned = castAt battle S.alice two (castAt battle S.alice one after)
        Spec.assertBool s (Combat.legalBlockDeclaration S.carol (Map.singleton blocker (Set.singleton wraith)) burned) "no Swamp on carol's side, so the block is legal"
        Spec.assertBool s (not (Set.member battle (GameState.battlefield burned))) "the Siege is gone here too"
      _ -> Spec.assertFailure s "fixture should have a Wraith, a blocker and two Bolts"
  Spec.it s "CR 508.5 the departed battle's CONTROLLER is not the seat that is read" $ do
    -- THE FALSIFIER for reading the departed battle's CONTROLLER where CR 508.5
    -- names its protector. The Swamp sits with alice, who controlled the Siege
    -- and attacks with the Wraith; carol, who protected it, holds an Island. A
    -- controller-reading fallback would call this block illegal.
    (gs, battle, mine, _, hers) <- battleCombatOf s registry S.carol S.carol ["Bog Wraith", "Swamp"] [] ["Goblin Piker", "Island"]
    (armed, bolts) <- twoBolts s registry gs
    case (mine, hers, bolts) of
      (wraith : _, blocker : _, [one, two]) -> do
        let after = S.runPure (attackTheBattle battle) armed (Combat.declareAttackers S.manaPerformer S.alice)
            burned = castAt battle S.alice two (castAt battle S.alice one after)
        Spec.assertBool s (Combat.legalBlockDeclaration S.carol (Map.singleton blocker (Set.singleton wraith)) burned) "alice's Swamp is not the protector's"
        Spec.assertBool s (not (Set.member battle (GameState.battlefield burned))) "the Siege is gone here too"
      _ -> Spec.assertFailure s "fixture should have a Wraith, a blocker and two Bolts"
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
  Spec.it s "CR 702.14c and the same three-seat board with the lands swapped leaves the block legal" $ do
    -- THE FALSIFIER, differing in one thing: bob, who merely comes first among the
    -- defending players, now holds the Swamp and carol an Island. An engine reading
    -- the head of that list calls this block illegal and the case above legal.
    (gs, battle, mine, _, hers) <- battleCombatOf s registry S.carol S.carol ["Bog Wraith"] ["Swamp"] ["Goblin Piker", "Island"]
    (armed, bolts) <- twoBolts s registry (bothDefending gs)
    case (mine, hers, bolts) of
      ([wraith], blocker : _, [one, two]) -> do
        let after = S.runPure (attackTheBattle battle) armed (Combat.declareAttackers S.manaPerformer S.alice)
            burned = castAt battle S.alice two (castAt battle S.alice one after)
        Spec.assertBool s (Combat.legalBlockDeclaration S.carol (Map.singleton blocker (Set.singleton wraith)) burned) "bob's Swamp is not the protector's, so the block is legal"
        Spec.assertBool s (not (Set.member battle (GameState.battlefield burned))) "the Siege is gone here too"
      _ -> Spec.assertFailure s "fixture should have a Wraith, a blocker and two Bolts"

  Spec.it s "CR 310.9c a creature the protector does not control can't block the battle's attacker" $ do
    -- CR 310.9c: "creatures controlled by other players can't block those
    -- attackers". bob holds the board's only untapped creature besides the
    -- attacker, and bob protects nothing. Nothing forbids it explicitly -- CR
    -- 509.1a already restricts blocking to the defending player, and CR 310.9b
    -- makes the battle attackable only through its protector, so the two rules
    -- meet and bob is never asked.
    piker <- S.printingOf s registry "Goblin Piker"
    (gs, battle, mine, _, _) <- battleCombat s registry S.carol S.carol [piker] [piker] []
    let attacked = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.manaPerformer S.alice)
        blocked = S.runPure S.aggressiveAnswer attacked (Combat.declareBlockers S.manaPerformer)
    -- Asserted, not assumed: without it this case would read the same on a board
    -- where the creature attacked carol herself and no battle was involved at all.
    Spec.assertEqWith
      s
      "the Piker is attacking the battle"
      (Map.elems (Combat.Type.attackers (GameState.combat attacked)))
      (fmap (const (AttackTarget.OfBattle battle)) mine)
    Spec.assertEqWith s "nothing blocks" (Combat.Type.blockers (GameState.combat blocked)) Map.empty
  Spec.it s "CR 310.9c and one the protector does control blocks it" $ do
    -- THE FALSIFIER for the case above: the same Piker moved from bob's side to
    -- carol's, under the same answerer, which blocks with everything it is
    -- offered. Without this the empty map above would pass on a board where
    -- nothing could ever block.
    piker <- S.printingOf s registry "Goblin Piker"
    (gs, battle, mine, _, hers) <- battleCombat s registry S.carol S.carol [piker] [] [piker]
    case (mine, hers) of
      ([attacker], [blocker]) -> do
        let attacked = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.manaPerformer S.alice)
            blocked = S.runPure S.aggressiveAnswer attacked (Combat.declareBlockers S.manaPerformer)
        Spec.assertEqWith
          s
          "the Piker is attacking the battle"
          (Map.lookup attacker (Combat.Type.attackers (GameState.combat attacked)))
          (Just (AttackTarget.OfBattle battle))
        Spec.assertEqWith s "carol blocks" (Combat.blockersOf attacker blocked) (Set.singleton blocker)
      _ -> Spec.assertFailure s "fixture should have one creature a side"

  Spec.it s "CR 704.5x a battle that IS being attacked keeps a protector who has left" $ do
    -- The rider CR 704.5x carries and CR 704.5y does not: "no attacking creatures
    -- are currently attacking that battle". carol concedes with alice's creature
    -- still attacking her Siege, so the designation is illegal -- no player in the
    -- game holds it -- and the state-based action must nonetheless leave it be.
    -- Gatherer states the consequence: the battle "continues to be attacked and
    -- can be dealt combat damage as normal", and a new protector is chosen once
    -- nothing is attacking it.
    piker <- S.printingOf s registry "Goblin Piker"
    (gs, battle, _, _, _) <- battleCombat s registry S.carol S.carol [piker] [] []
    let attacked = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.manaPerformer S.alice)
        gone = S.departs Departure.Type.Conceded S.carol attacked
        checked = S.runPure S.identityAnswer gone Sba.checkStateBasedActions
    Spec.assertBool s (Battle.isBeingAttacked battle gone) "the Siege is under attack"
    Spec.assertBool s (notElem S.carol (Game.stillPlaying gone)) "and carol is gone"
    Spec.assertEqWith s "the illegal designation stands" (protectorOf battle checked) (Just S.carol)
  Spec.it s "CR 704.5x the same concession on the same board DOES repair it when nothing attacks" $ do
    -- THE FALSIFIER for the rider: the identical fixture with the declaration
    -- skipped. bob is the only opponent left, so the re-choice is forced and the
    -- new designation is the state-based action firing rather than an answerer's
    -- preference.
    piker <- S.printingOf s registry "Goblin Piker"
    (gs, battle, _, _, _) <- battleCombat s registry S.carol S.carol [piker] [] []
    let gone = S.departs Departure.Type.Conceded S.carol gs
        checked = S.runPure S.identityAnswer gone Sba.checkStateBasedActions
    Spec.assertBool s (not (Battle.isBeingAttacked battle gone)) "nothing is attacking it"
    Spec.assertEqWith s "bob protects it now" (protectorOf battle checked) (Just S.bob)

  Spec.it s "CR 506.4 whole cards: a Word of Seizing on the attacked Siege stops it being attacked" $ do
    -- The CONTROLLER clause of rule 506.4, which no candidate list can see: bob
    -- steals the Siege mid-combat and carol goes on protecting it (CR 310.9d), so
    -- Combat.attackableBattles still finds it under the same defending player and
    -- only the recorded seat says the controller changed.
    --
    -- bob and not carol, and not alice: alice already controls it, so the spell
    -- would change nothing, and carol taking it would fire CR 704.5y and move the
    -- PROTECTOR -- the clause the candidate list already answers -- leaving the
    -- controller clause unproven.
    (gs, battle, mine, theirs, _) <- battleCombatOf s registry S.carol S.carol ["Goblin Piker"] (replicate 5 "Mountain") []
    (board, spell) <- seizing s registry S.bob gs
    case (mine, theirs) of
      ([attacker], land : _) -> do
        let stolen = runToEndOfCombat (seizeAnswer battle spell battle) board
            elsewhere = runToEndOfCombat (seizeAnswer battle spell land) board
        -- GAMEPLAY FIRST, on the quantity the two readings differ on: the Piker
        -- is a 2/1 and the Siege prints defense 5, so "removed from combat" and
        -- "still attacked" are 5 and 3 (CR 510.1b, CR 310.6).
        Spec.assertEqWith s "CR 510.1b: a Siege removed from combat is assigned no combat damage" (S.counterOf CounterKind.Defense battle stolen) 5
        Spec.assertEqWith s "the twin, one target apart: the Siege bob never took is dealt the Piker's 2" (S.counterOf CounterKind.Defense battle elsewhere) 3
        Spec.assertBool s (Set.member attacker (Combat.Type.attackingNothing (GameState.combat stolen))) "CR 506.4c: the Piker is attacking nothing"
        Spec.assertBool s (not (Set.member attacker (Combat.Type.attackingNothing (GameState.combat elsewhere)))) "and on the twin it is still attacking the Siege"
        -- The two legs differ in the CONTROLLER and in nothing else: carol
        -- protects the battle on both, so the removal cannot be the clause
        -- stillAttackedBattle already answers.
        Spec.assertEqWith s "bob controls the Siege on the theft leg" (Projection.controllerOf battle stolen) (Just S.bob)
        Spec.assertEqWith s "alice still controls it on the twin" (Projection.controllerOf battle elsewhere) (Just S.alice)
        Spec.assertEqWith s "carol protects it on both" (fmap (protectorOf battle) [stolen, elsewhere]) [Just S.carol, Just S.carol]
        -- CR 506.4c: the entry naming the battle stays put, which is what makes
        -- the removal readable at all.
        Spec.assertEqWith
          s
          "the attackers map still names the Siege"
          (Map.lookup attacker (Combat.Type.attackers (GameState.combat stolen)))
          (Just (AttackTarget.OfBattle battle))
        -- Both legs really cast the spell, and each resolved on the permanent it
        -- named: bob's five Mountains are exactly the {3}{R}{R}, so all five are
        -- tapped for it and the target alone is untapped again.
        Spec.assertEqWith s "the twin untapped the Mountain it named" (fmap Object.tapped (Game.lookupObject land elsewhere)) (Just TapState.Untapped)
        Spec.assertEqWith s "where the theft leg left it tapped for the spell" (fmap Object.tapped (Game.lookupObject land stolen)) (Just TapState.Tapped)
      _ -> Spec.assertFailure s "fixture should have one attacker and five Mountains"
    Spec.assertBool s (Projection.controllerOf battle gs == Just S.alice) "alice controlled the Siege before either leg"
  Spec.it s "CR 508.5 whole cards: a protector who steals the attacked Siege keeps the block" $ do
    -- CR 508.5's SECOND sentence with the battle still on the battlefield, which
    -- is the case the last-known designation cannot reach. carol protects alice's
    -- Siege and then takes it, so she is a player who can't be its protector (CR
    -- 310.12a) and rule 704.5y re-chooses bob; CR 506.4 removed the battle from
    -- combat as that happened, and rule 508.5 then names the protector it had
    -- BEFORE the removal -- carol -- where the live designation names bob.
    --
    -- The pair differs in ONE thing: which of bob and carol holds the board's
    -- only Bog Wraith. Both of alice's opponents defend (CR 802.2), so nothing
    -- but the seat rule 508.5 names decides who may block the Piker, and the two
    -- readings answer it the opposite way round.
    (byCarol, battle, piker) <- seizedByProtector s registry S.carol
    (byBob, _, otherPiker) <- seizedByProtector s registry S.bob
    -- GAMEPLAY FIRST, on CR 509.1a's own question: the Piker is a 2/1 and the
    -- Wraith a 3/3, so "may block" and "may not" are a dead attacker and a live
    -- one (CR 510.1d, CR 704.5g).
    Spec.assertBool s (not (S.onBattlefield piker byCarol)) "carol protected the Siege, so her Wraith blocks the Piker and kills it"
    Spec.assertBool s (S.onBattlefield otherPiker byBob) "the twin, one seat apart: bob protects it only after the theft, so his Wraith may not block"
    -- The premises, after the gameplay assertions so neither can absorb a
    -- mutation of them.
    Spec.assertEqWith s "CR 704.5y: bob protects the Siege once carol has taken it" (protectorOf battle byCarol) (Just S.bob)
    Spec.assertEqWith s "CR 506.4: carol controls it, which is what moved the designation" (Projection.controllerOf battle byCarol) (Just S.carol)
    Spec.assertBool s (Set.member piker (Combat.Type.attackingNothing (GameState.combat byCarol))) "CR 506.4c: the Piker is attacking nothing"
    Spec.assertEqWith s "CR 802.2: both opponents defend" (Combat.Type.defenders (GameState.combat byCarol)) [S.bob, S.carol]
    Spec.assertEqWith s "CR 802.2a's third sentence: carol is the recorded seat" (Map.lookup piker (Combat.Type.attackedUnder (GameState.combat byCarol))) (Just S.carol)
  Spec.it s "CR 508.4 the other road into combat records the same two seats" $ do
    -- CR 506.4's comparand is written by both writers of `attackers`, and a
    -- creature put onto the battlefield attacking never went through CR 508.1b.
    -- Read off the RECORD and not off a board: that road's producers are attack
    -- triggers (Hanweir Garrison, Hero of Bladehold), so the creature arrives
    -- while a trigger resolves, and a pure answerer cannot order that against the
    -- instant that would then move the battle (see the vacuity traps in
    -- docs/agents/implementing.md). The pair above is where the record is proved
    -- to matter; this case is what keeps the second writer from going dead.
    (gs, battle, mine, _, _) <- battleCombatOf s registry S.carol S.carol ["Goblin Piker"] [] []
    case mine of
      [arrival] -> do
        let after = S.runPure (attackTheBattle battle) gs (Combat.putOntoBattlefieldAttacking arrival)
            combat = GameState.combat after
        Spec.assertEqWith s "it is attacking the Siege" (Map.lookup arrival (Combat.Type.attackers combat)) (Just (AttackTarget.OfBattle battle))
        -- The two records hold DIFFERENT seats, which is the whole reason there
        -- are two: CR 310.9d makes the defending player the protector, and rule
        -- 506.4's controller clause asks about alice.
        Spec.assertEqWith s "CR 506.4: the battle's controller" (Map.lookup arrival (Combat.Type.attackedControlledBy combat)) (Just S.alice)
        Spec.assertEqWith s "CR 802.2a: its protector, and not the same player" (Map.lookup arrival (Combat.Type.attackedUnder combat)) (Just S.carol)
      _ -> Spec.assertFailure s "fixture should have exactly one creature"

-- CR 509.1a's and CR 802.4a's THIRD subject -- "a battle they protect" -- through
-- the filter atom that asks it, Filter.IsAttackingBattle. Rule 310 rather than
-- rule 508, because CR 310.9d is the whole content of the atom: the seat it
-- compares is the battle's PROTECTOR and not its controller.
--
-- The producer is SYNTHETIC, and no printing is being passed over. Scryfall
-- o:"battle you protect" and o:"battles you protect", both with include:extras,
-- returned nothing on 2026-08-29, and the four cards o:/attacking [a-z ]*battle/
-- finds -- Rampaging Geoderm, Thrashing Frontliner, War Historian and
-- War-Trained Slasher -- all ask about the battle alone with no protector in the
-- sentence, and all as trigger or static conditions rather than as a filter over
-- candidates. A printing writing this subject would refute that.
--
-- Synthetic Bulwark Snare is Soul Snare with CR 509.1a's third subject in place of
-- its second: {W} Enchantment, "{W}, Sacrifice this enchantment: Exile target
-- creature that's attacking you or a battle you protect."
filterSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
filterSpec s registry = Spec.describe s "Filter" $ do
  Spec.it s "CR 310.9d whole card: Synthetic Bulwark Snare reaches a creature attacking a battle you PROTECT" $ do
    -- THREE Snares off ONE board, differing in exactly one thing: which seat fires
    -- one. alice controls the Siege and attacks it with a Goblin Piker (CR
    -- 310.9b's "notably"), carol protects it, bob is neither -- so the protector
    -- and the controller are different players and neither is the other's
    -- fallback. Each seat holds one Plains and one Snare, so the three legs cannot
    -- differ in mana.
    snare <- S.printingOf s registry snareName
    (gs, battle, mine, theirs, hers) <-
      battleCombatOf s registry S.carol S.carol ["Plains", "Goblin Piker", snareName] ["Plains", snareName] ["Plains", snareName]
    case (Face.activatedAbilities (S.combinedFace snare), mine, theirs, hers) of
      ([ability], [alicePlains, piker, aliceSnare], [bobPlains, bobSnare], [carolPlains, carolSnare]) -> do
        let leg holder = runToEndOfCombat (snareAnswer battle holder ability piker) gs
            protected = leg carolSnare
            bystander = leg bobSnare
            controlled = leg aliceSnare
        -- GAMEPLAY FIRST, on the quantity the readings differ on: CR 310.6's
        -- defense counters. The Piker is a 2/1 and the Siege prints defense 5, so
        -- "the attacker was exiled" and "it connected" are 5 and 3.
        Spec.assertEqWith s "CR 310.6: carol's Snare exiled the attacker, so the Siege's defense is untouched" (S.counterOf CounterKind.Defense battle protected) 5
        Spec.assertEqWith s "protector: bob's Snare cannot name it, so the Piker's 2 comes off the Siege" (S.counterOf CounterKind.Defense battle bystander) 3
        Spec.assertEqWith s "CR 310.9d: nor can alice's, though she CONTROLS the Siege" (S.counterOf CounterKind.Defense battle controlled) 3
        Spec.assertBool s (not (S.onBattlefield piker protected)) "the Piker was exiled"
        Spec.assertBool s (S.onBattlefield piker bystander) "protector: on bob's leg it is untouched"
        Spec.assertBool s (S.onBattlefield piker controlled) "controller: on alice's leg too"
        Spec.assertBool s (not (S.onBattlefield carolSnare protected)) "carol's Snare paid its own sacrifice, so the ability really was activated"
        Spec.assertBool s (S.onBattlefield bobSnare bystander) "protector: bob's Snare is unsacrificed, so his was never activated"
        Spec.assertBool s (S.onBattlefield aliceSnare controlled) "controller: alice's is unsacrificed too"
        -- The negative legs failed on the FILTER and not for want of {W}: each
        -- seat's Plains is still untapped, while carol's paid.
        Spec.assertEqWith s "carol tapped her Plains for the activation" (fmap Object.tapped (Game.lookupObject carolPlains protected)) (Just TapState.Tapped)
        Spec.assertEqWith s "bob never spent his" (fmap Object.tapped (Game.lookupObject bobPlains bystander)) (Just TapState.Untapped)
        Spec.assertEqWith s "nor alice hers" (fmap Object.tapped (Game.lookupObject alicePlains controlled)) (Just TapState.Untapped)
        -- Anti-vacuity, read on a leg where nothing was exiled: the Piker IS
        -- attacking, and what it attacks is the Siege rather than a player.
        Spec.assertEqWith
          s
          "CR 508.1b: the Piker really was announced at the Siege"
          (Map.lookup piker (Combat.Type.attackers (GameState.combat bystander)))
          (Just (AttackTarget.OfBattle battle))
        -- The two seats the trio tells apart, and the reason they differ: CR
        -- 310.12a puts a Siege's protector among its controller's opponents.
        Spec.assertEqWith s "CR 310.9a: carol protects the Siege" (protectorOf battle bystander) (Just S.carol)
        Spec.assertEqWith s "CR 310.12a: alice controls it, and is the player attacking it" (Projection.controllerOf battle bystander) (Just S.alice)
      _ -> Spec.assertFailure s "fixture should have one ability, one Piker and three Snares"
  -- CR 506.4's battle clause -- "if it's a battle that's being attacked and stops
  -- being a battle" -- read through the same atom. CR 310.9g is what makes it a
  -- real hole rather than a redundancy: the protector designation SURVIVES the
  -- type change, so Battle.protectorOf goes on naming carol and the atom goes on
  -- answering her unless the card type is asked as well.
  --
  -- The producer is two printings and no new card. Song of the Dryads {2}{G} --
  -- Enchantment - Aura, "Enchant permanent / Enchanted permanent is a colorless
  -- Forest land", is sorcery-speed and so cannot be cast mid-combat; Aura Graft
  -- {1}{U} -- Instant, "Gain control of target Aura that's attached to a
  -- permanent. Attach it to another permanent it can enchant", moves one that is
  -- already down. Both oracle texts checked against Scryfall, 2026-08-29.
  --
  -- A PAIR off ONE board differing in exactly one thing: which permanent alice's
  -- Aura Graft moves the Song onto. Same spell, same two Islands, same answerer.
  -- The move lands AFTER the declaration, which is the point.
  Spec.it s "CR 506.4 a battle that stops being a battle stops being attacked, so the Snare cannot name the attacker" $ do
    snare <- S.printingOf s registry snareName
    (gs0, battle, mine, _, hers) <-
      battleCombatOf s registry S.carol S.carol ["Goblin Piker", "Island", "Island", "Bonesplitter", "Bonesplitter", "Song of the Dryads"] [] ["Plains", snareName]
    graft <- S.printingOf s registry "Aura Graft"
    case (Face.activatedAbilities (S.combinedFace snare), mine, hers) of
      ([ability], [piker, _, _, host, spare, song], [_, carolSnare]) -> do
        let (spell, gs1) = S.addHandCard graft S.alice (S.attachTo song (Recipient.ToObject host) gs0)
            queued =
              gs1
                { GameState.remaining = S.phasesAfterThroughPostcombatMain (Phase.Combat CombatStep.DeclareAttackers)
                }
            declared = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) (attackTheBattle battle) queued
            onto destination =
              S.runPure (graftAnswer song destination) declared $ do
                S.cast S.alice spell
                Stack.resolveTop
                Engine.settleForPriority
            landed = onto battle
            elsewhere = onto spare
            fires = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (snareAnswer battle carolSnare ability piker)
        -- GAMEPLAY FIRST. The Siege is a colorless Forest land, so nothing is
        -- attacking a battle carol protects; an engine reading the surviving
        -- designation anyway hands her Snare the attacker.
        Spec.assertBool s (S.onBattlefield piker (fires landed)) "CR 506.4: the Siege stopped being a battle, so carol's Snare cannot name the Piker"
        Spec.assertBool s (S.onBattlefield carolSnare (fires landed)) "and her Snare is unsacrificed: the ability was never activatable"
        Spec.assertBool s (not (S.onBattlefield piker (fires elsewhere))) "control: with the Song moved to the spare Bonesplitter instead, the same Snare exiles the attacker"
        Spec.assertBool s (not (S.onBattlefield carolSnare (fires elsewhere))) "control: there it paid its own sacrifice, so the activation really happened"
        -- Anti-vacuity, and the isolation. CR 613.1d's layer 4 moved and CR 310.9g's
        -- designation did not, which is the whole reason the type has to be asked.
        Spec.assertEqWith s "CR 613.1d: it is no longer a battle on one leg only" (fmap (Battle.isBattle . Projection.project battle) [landed, elsewhere]) [False, True]
        Spec.assertEqWith s "CR 310.9g: carol protects it on both all the same" (fmap (protectorOf battle) [landed, elsewhere]) [Just S.carol, Just S.carol]
        Spec.assertBool s (Set.member battle (GameState.battlefield landed)) "CR 506.4: nor did it leave the battlefield"
        Spec.assertEqWith
          s
          "CR 506.4c: the Piker is still an attacking creature, its entry still naming the Siege"
          (Map.lookup piker (Combat.Type.attackers (GameState.combat landed)))
          (Just (AttackTarget.OfBattle battle))
      _ -> Spec.assertFailure s "fixture should have one ability, one Piker, two Bonesplitters, a Song and carol's Snare"
  -- The same clause read as the EVENT rule 506.4 makes it: a battle that stops
  -- being a battle stays removed from combat even once it is a battle again.
  -- Pawl.Types.Combat's attackingNothing is the record that remembers it, and
  -- this is the battle half of the pair Pawl.CombatEffectSpec drives for a
  -- planeswalker -- the sibling arm of the same atom, so that neither is fixed
  -- alone.
  --
  -- TWO Aura Grafts, so the Song travels twice and both legs END with the Siege
  -- a battle carol protects on the battlefield: only the route differs. Each
  -- Graft settles before the next (CR 117.5), which is where rule 506.4's
  -- sampler runs.
  Spec.it s "CR 506.4 a battle that stops being a battle and becomes one again stays removed from combat, so the Snare still cannot name the attacker" $ do
    snare <- S.printingOf s registry snareName
    (gs0, battle, mine, _, hers) <-
      battleCombatOf s registry S.carol S.carol ["Goblin Piker", "Island", "Island", "Island", "Island", "Bonesplitter", "Bonesplitter", "Song of the Dryads"] [] ["Plains", snareName]
    graft <- S.printingOf s registry "Aura Graft"
    case (Face.activatedAbilities (S.combinedFace snare), mine, hers) of
      ([ability], [piker, _, _, _, _, host, spare, song], [_, carolSnare]) -> do
        let (first, gs1) = S.addHandCard graft S.alice (S.attachTo song (Recipient.ToObject host) gs0)
            (second, gs2) = S.addHandCard graft S.alice gs1
            queued =
              gs2
                { GameState.remaining = S.phasesAfterThroughPostcombatMain (Phase.Combat CombatStep.DeclareAttackers)
                }
            declared = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) (attackTheBattle battle) queued
            onto spell destination gs =
              S.runPure (graftAnswer song destination) gs $ do
                S.cast S.alice spell
                Stack.resolveTop
                Engine.settleForPriority
            -- Song goes to the Siege and then off it; against the leg where it
            -- never goes near the Siege at all.
            stopped = onto first battle declared
            untouched = onto first spare declared
            thereAndBack = onto second spare stopped
            neverBattle = onto second host untouched
            fires = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (snareAnswer battle carolSnare ability piker)
        -- GAMEPLAY FIRST, and both boards read identically to anything asking
        -- about the board NOW.
        Spec.assertBool s (S.onBattlefield piker (fires thereAndBack)) "CR 506.4: the Siege left combat when it stopped being a battle, and becoming one again does not put it back, so carol's Snare cannot name the Piker"
        Spec.assertBool s (S.onBattlefield carolSnare (fires thereAndBack)) "and her Snare is unsacrificed: the ability was never activatable"
        Spec.assertBool s (not (S.onBattlefield piker (fires neverBattle))) "control: with the Song never on the Siege, the same Snare exiles the attacker"
        Spec.assertBool s (not (S.onBattlefield carolSnare (fires neverBattle))) "control: there it paid its own sacrifice, so the activation really happened"
        -- The pair really is one board differing in one thing.
        Spec.assertEqWith s "CR 613.1d: it stopped being a battle midway on one leg only" (fmap (Battle.isBattle . Projection.project battle) [stopped, untouched]) [False, True]
        Spec.assertEqWith s "and it is a battle again on BOTH, so no live read can tell the legs apart" (fmap (Battle.isBattle . Projection.project battle) [thereAndBack, neverBattle]) [True, True]
        Spec.assertEqWith s "CR 310.9g: carol protects it on both" (fmap (protectorOf battle) [thereAndBack, neverBattle]) [Just S.carol, Just S.carol]
        Spec.assertEqWith s "CR 506.4: nor did it leave the battlefield" (fmap (Set.member battle . GameState.battlefield) [thereAndBack, neverBattle]) [True, True]
        Spec.assertEqWith s "CR 506.4c: the Piker is attacking nothing on one leg only" (fmap (Set.member piker . Combat.Type.attackingNothing . GameState.combat) [thereAndBack, neverBattle]) [True, False]
        Spec.assertEqWith
          s
          "CR 506.4c: and it is still an attacking creature, its entry still naming the Siege"
          (fmap (Map.lookup piker . Combat.Type.attackers . GameState.combat) [thereAndBack, neverBattle])
          [Just (AttackTarget.OfBattle battle), Just (AttackTarget.OfBattle battle)]
        Spec.assertEqWith s "CR 701.3a: the Song ends on a Bonesplitter on both legs" (fmap (Projection.hostOf song) [thereAndBack, neverBattle]) [Just spare, Just host]
      _ -> Spec.assertFailure s "fixture should have one ability, one Piker, four Islands, two Bonesplitters, a Song and carol's Snare"

-- Choose `aura` for Aura Graft's one target slot and `destination` for the host
-- CR 701.3a then moves it to. FILTERED rather than replaced, for snareAnswer's
-- reason; Pawl.CombatEffectSpec holds its own copy for its planeswalker pair.
graftAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
graftAnswer aura destination p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> Set.filter ((== Just aura) . Recipient.objectOf) legal) sets
  Prompt.ChooseAttachment _ _ _ offered -> if List.elem destination (NonEmpty.toList offered) then destination else NonEmpty.head offered
  _ -> S.aggressiveAnswer p

snareName :: String
snareName = "Synthetic Bulwark Snare"

-- Announce the attack at the battle, then fire `snare` at `victim` the first time
-- the ability is offered -- which is once, since the activation sacrifices the
-- enchantment, so a pure answerer needs no counter to stay honest.
--
-- The target set is FILTERED rather than replaced, so a leg whose slot does not
-- admit the victim takes no target at all instead of quietly succeeding on a
-- hand-built recipient -- and on such a leg the activation is never offered at
-- all, CR 602.2b / 601.2c's target choice being part of what makes an ability
-- activatable.
snareAnswer ::
  ObjectId.ObjectId ->
  ObjectId.ObjectId ->
  ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) ->
  ObjectId.ObjectId ->
  Prompt.Prompt r ->
  r
snareAnswer battle snare ability victim p = case p of
  Prompt.ChooseAction _ _ actions
    | elem (A.Activate snare ability) actions -> A.Activate snare ability
    | otherwise -> A.Pass
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, rs) -> Set.filter (== Recipient.ToCreature victim) rs) sets
  _ -> attackTheBattle battle p

-- castInvasionThreeSeated's board handed to CAROL's precombat main phase, with
-- five Mountains and a Zealous Conscripts for her and one extra Plains for alice.
-- Gives back the board, the Siege, the Conscripts and that Plains -- the twin
-- leg's target, and alice's rather than carol's so that both legs really change a
-- permanent's controller.
--
-- Zealous Conscripts, {4}{R} Creature -- Human Warrior 3/3: "Haste. When this
-- creature enters, gain control of target permanent until end of turn. Untap that
-- permanent. It gains haste until end of turn."
conscriptedSiege ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
conscriptedSiege s registry = do
  (entered, battle) <- castInvasionThreeSeated s registry (protectTo S.carol)
  mountain <- S.printingOf s registry "Mountain"
  plains <- S.printingOf s registry "Plains"
  conscripts <- S.printingOf s registry "Zealous Conscripts"
  let lands = List.foldl' (\g _ -> snd (S.addPermanent mountain S.carol g)) entered [1 :: Int .. 5]
      (decoy, withDecoy) = S.addPermanent plains S.alice lands
      (spell, handed) = S.addHandCard conscripts S.carol withDecoy
  pure
    ( handed
        { GameState.activePlayer = S.carol,
          GameState.phase = Phase.PrecombatMain,
          GameState.priority = Just S.carol
        },
      battle,
      spell,
      decoy
    )

-- Cast the Conscripts, aim its entry trigger at `victim`, and settle: the trigger
-- resolves and CR 704.5's pass runs inside the same priority loop.
runConscripts :: GameState.GameState -> ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState
runConscripts gs spell victim =
  let cast = S.runPure (conscriptAnswer spell victim) gs (S.cast S.carol spell)
   in S.runPure (conscriptAnswer spell victim) cast Engine.priorityLoop

-- Cast `spell` and narrow every target slot to `victim`, naming bob for CR
-- 704.5y's re-choice. Two candidates make that a real prompt: alice and bob are
-- both opponents of carol, who controls the battle once the trigger resolves.
conscriptAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
conscriptAnswer spell victim p = case p of
  Prompt.ChooseAction _ _ actions -> case filter (S.isCastOf spell) actions of
    action : _ -> action
    [] -> A.Pass
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> Set.filter ((== Just victim) . Recipient.objectOf) legal) sets
  _ -> protectTo S.bob p

-- Put a Word of Seizing in `who`'s hand, giving back the board and the card.
--
-- Word of Seizing, {3}{R}{R} Instant: "Split second. Untap target permanent and
-- gain control of it until end of turn. It gains haste until end of turn."
-- TARGET PERMANENT at INSTANT speed is what makes it the producer; this module's
-- header records what the rest of the pool offers. Split second is idle on this
-- board -- nobody else wants to respond -- and is on the card because a card file
-- states what is printed.
seizing ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  PlayerId.PlayerId ->
  GameState.GameState ->
  m (GameState.GameState, ObjectId.ObjectId)
seizing s registry who gs = do
  word <- S.printingOf s registry "Word of Seizing"
  let (spell, handed) = S.addHandCard word who gs
  pure (handed, spell)

-- Announce the attack at the battle, and cast `spell` at `victim` the first time
-- its controller has priority. The target set is FILTERED rather than replaced,
-- for snareAnswer's reason.
seizeAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
seizeAnswer battle spell victim p = case p of
  Prompt.ChooseAction _ _ actions -> case filter (S.isCastOf spell) actions of
    action : _ -> action
    [] -> A.Pass
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> Set.filter ((== Just victim) . Recipient.objectOf) legal) sets
  _ -> attackTheBattle battle p

-- battleCombat's board wound forward to the END of combat under `answer`, so a
-- card can act in the declare attackers step's priority round and CR 510 can then
-- deal the damage. battleCombat parks the state in the declare attackers step with
-- no later phases queued, so the walk needs them; the postcombat main phase is
-- there only as a terminator, S.runToStep stopping on the end of combat step
-- before running it (CR 511.3 keeps the creature attacking for the whole of it).
runToEndOfCombat :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToEndOfCombat answer gs =
  S.runToStep
    (Phase.Combat CombatStep.EndOfCombat)
    answer
    gs
      { GameState.remaining = S.phasesAfterThroughPostcombatMain (Phase.Combat CombatStep.DeclareAttackers)
      }

-- carol protects alice's Siege, alice attacks it with a Goblin Piker, and carol
-- takes the battle with a Word of Seizing in the declare attackers step's
-- priority round -- wound to the end of combat, so CR 509 and CR 510 both run.
-- `blockerSeat` holds the board's only Bog Wraith, and both of alice's opponents
-- defend (CR 802.2), so nothing but CR 508.5's seat decides who may block.
seizedByProtector ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  PlayerId.PlayerId ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
seizedByProtector s registry blockerSeat = do
  let wraith who = ["Bog Wraith" | who == blockerSeat]
  (gs, battle, mine, _, _) <-
    battleCombatOf s registry S.carol S.carol ["Goblin Piker"] (wraith S.bob) (replicate 5 "Mountain" <> wraith S.carol)
  (board, spell) <- seizing s registry S.carol (bothDefending gs)
  case mine of
    [attacker] -> pure (runToEndOfCombat (seizeAndProtect battle spell) board, battle, attacker)
    _ -> Spec.assertFailure s "fixture should have exactly one attacker"

-- seizeAnswer aimed at the battle, naming bob for CR 704.5y's re-choice once
-- carol controls it. Two candidates make that a real prompt: alice and bob are
-- both opponents of carol.
seizeAndProtect :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
seizeAndProtect battle spell p = case p of
  Prompt.ChooseProtector {} -> S.bob
  _ -> seizeAnswer battle spell battle p

-- CR 310.6 / CR 120.3h: damage dealt to a battle removes that many defense
-- counters, and CR 115.4 is what lets a damage spell name one.
damageSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
damageSpec s registry = Spec.describe s "Damage" $ do
  Spec.it s "CR 115.4 an any-target spell offers the battle, and only what rule 115.4 names" $ do
    (gs, battle, spells) <- siegeUnderFire s registry ["Lightning Bolt"]
    bolt <- S.printingOf s registry "Lightning Bolt"
    case (spells, S.spellTargetSlot bolt) of
      ([boltId], Just theSlot) ->
        -- EXACT rather than a membership test, and that is what makes this read CR
        -- 115.4 rather than "battles are permanents": alice's Plains and Mountain
        -- are on the same battlefield, and rule 115.4's last sentence keeps them
        -- off the list. Three seats, so all three players are on it.
        Spec.assertEqWith
          s
          "the Siege and the three players"
          (Target.legalRecipients (Just S.alice) boltId theSlot gs)
          ( Set.fromList
              [ Recipient.ToBattle battle,
                Recipient.ToPlayer S.alice,
                Recipient.ToPlayer S.bob,
                Recipient.ToPlayer S.carol
              ]
          )
      _ -> Spec.assertFailure s "fixture should have a Bolt with a target slot"
  Spec.it s "CR 310.6 Lightning Bolt takes three defense counters off it" $ do
    (gs, battle, spells) <- siegeUnderFire s registry ["Lightning Bolt"]
    case spells of
      [boltId] -> do
        let after = castAt battle S.alice boltId gs
        -- Five printed, three dealt, two left -- so this reads the AMOUNT and not
        -- merely "some counters came off".
        Spec.assertEqWith s "two defense counters left" (S.counterOf CounterKind.Defense battle after) 2
        Spec.assertBool s (S.onBattlefield battle after) "and the Siege is still on the battlefield"
      _ -> Spec.assertFailure s "fixture should have a Bolt"
  Spec.it s "CR 310.6 / 510.1b combat damage to a battle removes them too" $ do
    -- The other producer, and the one attacking a battle exists for. Goblin Piker
    -- is a vanilla 2/1, so this reads CR 510.1b's assignment and no card text.
    piker <- S.printingOf s registry "Goblin Piker"
    (gs, battle, mine, _, _) <- battleCombat s registry S.carol S.carol [piker] [] []
    case mine of
      [attacker] -> do
        let attacked = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.manaPerformer S.alice)
            dealt = S.runPure S.identityAnswer attacked (Monad.void Damage.dealCombatDamage)
        Spec.assertEqWith
          s
          "the Piker really is attacking the battle"
          (Map.lookup attacker (Combat.Type.attackers (GameState.combat attacked)))
          (Just (AttackTarget.OfBattle battle))
        Spec.assertEqWith s "three defense counters left" (S.counterOf CounterKind.Defense battle dealt) 3
      _ -> Spec.assertFailure s "fixture should have exactly one attacker"
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
-- that the battle left. 704.5w's battle-type split has no printing to reach it, so
-- the three classifier cases at the end read Battle.defeated directly.
defeatSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
defeatSpec s registry = Spec.describe s "Defeat" $ do
  Spec.it s "CR 310.12b a Siege has the intrinsic defeat ability" $ do
    siege <- siegePC s registry
    Spec.assertEqWith
      s
      "one ability, conditioned on the last defense counter"
      (fmap TriggeredAbility.condition (Battle.triggeredAbilitiesOf siege))
      [TriggerCondition.SelfLastCounterRemoved CounterKind.Defense]
  Spec.it s "CR 310.12b a battle with no battle types does not" $ do
    -- Rule 310.12b says "Sieges", not "battles", and this is the falsifier for
    -- gating the mint on the card type instead: the same real projection with its
    -- subtypes stripped, which candidateSpec above uses for CR 310.9a's other
    -- branch.
    siege <- siegePC s registry
    Spec.assertEqWith s "no intrinsic ability" (Battle.triggeredAbilitiesOf siege {PC.subtypes = Set.empty}) []
  Spec.it s "CR 310.12b / 704.5v three plus two defeats it, and it is EXILED" $ do
    -- THE PROVING CASE. Bolt for 3 then Firebolt for 2 against a printed defense of
    -- 5, so the last counter comes off on the second spell and not the first.
    invasion <- S.printingOf s registry "Invasion of Dominaria"
    (gs, battle, spells) <- siegeUnderFire s registry ["Lightning Bolt", "Firebolt"]
    case spells of
      [boltId, fireboltId] -> do
        let afterBolt = castAt battle S.alice boltId gs
            afterBoth = castAt battle S.alice fireboltId afterBolt
            copiesIn zone = invasionsIn (S.printingName invasion) zone afterBoth
        Spec.assertEqWith s "no defense counters left" (S.counterOf CounterKind.Defense battle afterBoth) 0
        -- EXILE, with the graveyard checked separately: CR 704.5v alone would put
        -- it into its owner's graveyard, and "it left the battlefield" cannot tell
        -- the two rules apart. CR 400.7 mints a fresh id on the move, so the card
        -- is counted by name rather than found at `battle`.
        Spec.assertEqWith s "the Siege is in exile" (copiesIn Zone.Exile) 1
        Spec.assertEqWith s "and in nobody's graveyard" (copiesIn Zone.Graveyard) 0
        Spec.assertBool s (not (S.onBattlefield battle afterBoth)) "and not on the battlefield"
      _ -> Spec.assertFailure s "fixture should have a Bolt and a Firebolt"
  Spec.it s "CR 310.12b / 118.9 / 712.11a she may then cast it TRANSFORMED and FREE" $ do
    -- THE PROVING CASE for the second half of rule 310.12b's sentence, on the
    -- identical board as the exile case above with alice ACCEPTING the offer
    -- rather than declining it.
    --
    -- Free-ness is what makes it happen, not a convenience: every land alice has
    -- is tapped by this point -- three Plains paid for the Invasion and one
    -- Mountain for each of the two burn spells -- so a cast asked for Serra
    -- Faithkeeper's front-face {2}{W} could not be announced at all. CR 118.9's
    -- alternative cost is the only route onto the stack.
    --
    -- Transformed-ness is read off characteristics the FRONT face does not have.
    -- The front face is a Battle -- Siege with defense 5 and no power at all, so
    -- a 4/4 with flying and vigilance cannot be it (CR 712.8c / 712.11a). The
    -- exile count going to 0 is what says the card left rather than being copied.
    invasion <- S.printingOf s registry "Invasion of Dominaria"
    (gs, battle, spells) <- siegeUnderFire s registry ["Lightning Bolt", "Firebolt"]
    case spells of
      [boltId, fireboltId] -> do
        let afterBolt = castAt battle S.alice boltId gs
            afterBoth = castAtWith takesTheCast battle S.alice fireboltId afterBolt
        case angelOn afterBoth of
          Nothing -> Spec.assertFailure s "Serra Faithkeeper should be on the battlefield"
          Just (oid, pc) -> do
            Spec.assertEqWith s "a 4/4" (PC.power pc, PC.toughness pc) (Just 4, Just 4)
            Spec.assertEqWith s "an Angel" (PC.subtypes pc) (Set.singleton Subtype.Angel)
            Spec.assertEqWith
              s
              "with flying and vigilance"
              (Map.keysSet (PC.keywords pc))
              (Set.fromList [Keyword.Flying, Keyword.Vigilance])
            -- CR 110.2b: the permanent's controller is the player who put the
            -- spell onto the stack. The Siege she also controlled makes that a
            -- weak reading on its own, which is why the assertions above carry
            -- the weight.
            Spec.assertEqWith s "under alice's control" (Projection.controllerOf oid afterBoth) (Just S.alice)
            Spec.assertEqWith s "and the card has left exile" (invasionsIn (S.printingName invasion) Zone.Exile afterBoth) 0
      _ -> Spec.assertFailure s "fixture should have a Bolt and a Firebolt"
  Spec.it s "CR 310.12b declining the offer leaves the card in exile" $ do
    -- THE FALSIFIER for casting it unasked. Rule 310.12b says "you MAY cast it",
    -- so the identical board with the offer declined must leave the Angel
    -- unmade -- which is also what the exile case above depends on, since its
    -- answerer declines.
    (gs, battle, spells) <- siegeUnderFire s registry ["Lightning Bolt", "Firebolt"]
    case spells of
      [boltId, fireboltId] -> do
        let afterBolt = castAt battle S.alice boltId gs
            afterBoth = castAt battle S.alice fireboltId afterBolt
        Spec.assertEqWith s "no Angel" (fmap fst (angelOn afterBoth)) Nothing
      _ -> Spec.assertFailure s "fixture should have a Bolt and a Firebolt"
  Spec.it s "CR 310.6 the FIRST of those two spells defeats nothing" $ do
    -- THE FALSIFIER for the case above: the identical board after the Bolt alone.
    -- Without it, an engine that exiled a battle on any damage at all would pass.
    invasion <- S.printingOf s registry "Invasion of Dominaria"
    (gs, battle, spells) <- siegeUnderFire s registry ["Lightning Bolt", "Firebolt"]
    case spells of
      boltId : _ -> do
        let afterBolt = castAt battle S.alice boltId gs
        Spec.assertBool s (S.onBattlefield battle afterBolt) "the Siege is still on the battlefield"
        Spec.assertEqWith s "and nowhere else" (invasionsIn (S.printingName invasion) Zone.Exile afterBolt) 0
      _ -> Spec.assertFailure s "fixture should have a Bolt"
  Spec.it s "CR 704.5v a battle at defense 0 with nothing pending is named by the state-based action" $ do
    -- The clause's own reading, at the level Pawl.Engine.Battle states it.
    -- Unreachable for a SIEGE in a real game, and that is rule 704.5v's design: the
    -- counters hitting 0 fires CR 310.12b, whose exemption holds the battle there
    -- until the ability exiles it. What this pins is that the clause exists, so
    -- that the case below shows the RIDER and not the clause doing the holding.
    (entered, battle) <- castInvasionThreeSeated s registry (protectTo S.carol)
    let drained = drain battle entered
    Spec.assertEqWith s "it is named" (Battle.defeated (Projection.projectAll drained) [] drained) [battle]
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
    let drained = drain battle entered
        removal = [GameEvent.CountersRemoved (CounterChange.MkCounterChange battle CounterKind.Defense 2 0)]
    Spec.assertBool s (Battle.awaitingAbility removal drained battle) "the ability has triggered"
    Spec.assertEqWith s "so nothing is buried" (Battle.defeated (Projection.projectAll drained) removal drained) []
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
    let drained = drain battle entered
        removal = [GameEvent.CountersRemoved (CounterChange.MkCounterChange battle CounterKind.Defense 2 0)]
        pcs = Map.adjust (\pc -> pc {PC.subtypes = Set.empty}) battle (Projection.projectAll drained)
    Spec.assertBool s (Battle.awaitingAbility removal drained battle) "an ability has still triggered"
    Spec.assertEqWith s "and CR 704.5w exempts nothing" (Battle.defeated pcs removal drained) [battle]

-- Cast the spell in `caster`'s hand at `battle`, then settle: the spell resolves,
-- CR 310.6 takes its counters off, and CR 310.12b's ability -- if the last one came
-- off -- is placed and resolved inside the same loop. Every case above reads the
-- board that loop leaves.
castAt :: ObjectId.ObjectId -> PlayerId.PlayerId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
castAt = castAtWith S.identityAnswer

-- castAt with the fallback answerer named, so one case can accept CR 310.12b's
-- offered cast while every other one declines it (Replay.defaultAnswer's arm).
castAtWith ::
  (forall r. Prompt.Prompt r -> r) ->
  ObjectId.ObjectId ->
  PlayerId.PlayerId ->
  ObjectId.ObjectId ->
  GameState.GameState ->
  GameState.GameState
castAtWith fallback battle caster spell gs =
  let answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToBattle battle))) sets
        _ -> fallback p
      cast = S.runPure answer gs (S.cast caster spell)
   in S.runPure answer cast Engine.priorityLoop

-- CR 608.2g: take the cast CR 310.12b offers, and answer everything else as
-- S.identityAnswer does.
takesTheCast :: Prompt.Prompt r -> r
takesTheCast p = case p of
  Prompt.OfferedCast {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- The battlefield permanent whose PROJECTED name is Serra Faithkeeper -- the back
-- face, which is what CR 712.8c gives the resulting spell and CR 712.13 carries
-- onto the battlefield. Read off the projection rather than off the card, since
-- the CARD is named for its front face in every zone (CR 712.8a).
angelOn :: GameState.GameState -> Maybe (ObjectId.ObjectId, PC.ProjectedCharacteristics)
angelOn gs =
  let wanted = CardName.MkCardName (Text.pack "Serra Faithkeeper")
      pcs = Projection.projectAll gs
      hit oid = fmap ((,) oid) (List.find (Set.member wanted . PC.names) (Map.lookup oid pcs))
   in case Maybe.mapMaybe hit (Set.toAscList (GameState.battlefield gs)) of
        [found] -> Just found
        _ -> Nothing

-- Set a battle's defense counters to none without any damage having been dealt, so
-- the two CR 704.5v cases differ in the unscanned event log ALONE.
drain :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
drain oid gs =
  let empty obj = obj {Object.counters = Map.insert CounterKind.Defense 0 (Object.counters obj)}
   in gs {GameState.objects = Map.adjust empty oid (GameState.objects gs)}

-- How many copies of a card sit in `zone`, across every player's share of it.
-- Counted by NAME because CR 400.7 mints a fresh id on every zone change, so the id
-- the battle had on the battlefield names nothing once it has left.
invasionsIn :: CardName.CardName -> Zone.Zone -> GameState.GameState -> Int
invasionsIn wanted zone gs =
  let isCopy oid = fmap S.nameOf (Game.cardOf oid gs) == Just wanted
      members pid = Game.zoneMembers zone pid gs
   in length (concatMap (filter isCopy . members) (Map.keys (GameState.players gs)))

-- Two more Mountains for alice and two Lightning Bolts in her hand, so a case can
-- burn a defense-5 Siege off the battlefield INSIDE the declare attackers step: 3
-- then 3, the second clamped by the floor Pawl.Engine.Damage documents. Not the
-- Bolt/Firebolt pair damageSpec uses for an exact 5 -- Firebolt is a SORCERY, and
-- CR 307.1 keeps it out of combat.
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

-- alice's three-seat board with the Siege on it (carol protects), plus one Mountain
-- and one hand card per named spell -- so each is castable for its {R} without any
-- case above having to say so.
siegeUnderFire ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  [String] ->
  m (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId])
siegeUnderFire s registry spells = do
  (entered, battle) <- castInvasionThreeSeated s registry (protectTo S.carol)
  mountain <- S.printingOf s registry "Mountain"
  printings <- Monad.mapM (S.printingOf s registry) spells
  let withLands = List.foldl' (\g _ -> snd (S.addPermanent mountain S.alice g)) entered printings
      (ids, ready) =
        List.foldl'
          (\(acc, g) p -> let (oid, g2) = S.addHandCard p S.alice g in (acc <> [oid], g2))
          ([], withLands)
          printings
  pure (ready, battle, ids)

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
  mine' <- printings mine
  theirs' <- printings theirs
  hers' <- printings hers
  battleCombat s registry protector defender mine' theirs' hers'

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
