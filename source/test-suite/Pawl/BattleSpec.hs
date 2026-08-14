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
-- 508.5 (Defender.playerOf). Those are attackSpec below; Pawl.CombatSpec
-- keeps rule 508's own cases.
--
-- Also the pieces rule 310 needed underneath it, exercised here because this is
-- where a card reaches them: Pawl.Types.Defense, CounterKind.Defense,
-- EntryRewrite.ChooseProtector, Object.protector and AttackTarget.OfBattle.
--
-- Invasion of Dominaria // Serra Faithkeeper is the whole card pool for this file,
-- and is the only battle in `data/cards`. {2}{W} Battle -- Siege, defense 5, "When
-- this Siege enters, you gain 4 life and draw a card", transforming into a 4/4
-- Angel with flying and vigilance.
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
-- ability, and CR 310.7 / 704.5v's state-based action. Those are damageSpec and
-- defeatSpec below.
--
-- Lightning Bolt ({R} Instant, "deals 3 damage to any target") and Firebolt ({R}
-- Sorcery, "deals 2 damage to any target") are the pool's two plainest CR 115.4
-- spells, and 3 + 2 is exactly the Siege's printed defense of 5. Distinct amounts
-- on purpose: a defense-5 battle taking 5 at once could not tell "removed all the
-- counters" from "removed the right number".
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
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Sba as Sba
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterChange as CounterChange
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Defense as Defense
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
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
-- has no printing to take it from: CR 310.12 makes every battle printed so far a
-- Siege.
candidateSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
candidateSpec s registry = Spec.describe s "Candidates" $ do
  Spec.it s "CR 310.12a a Siege offers its controller's opponents and not its controller" $ do
    siege <- siegePC s registry
    Spec.assertEqWith
      s
      "bob and carol"
      (Battle.protectorCandidates siege S.alice [S.alice, S.bob, S.carol])
      [S.bob, S.carol]
  Spec.it s "CR 310.9a a battle with no battle types offers only its controller" $ do
    siege <- siegePC s registry
    Spec.assertEqWith
      s
      "alice alone"
      (Battle.protectorCandidates siege {PC.subtypes = Set.empty} S.alice [S.alice, S.bob, S.carol])
      [S.alice]
  Spec.it s "CR 704.5x a departed player is not a candidate" $ do
    siege <- siegePC s registry
    Spec.assertEqWith
      s
      "carol alone, bob having left"
      (Battle.protectorCandidates siege S.alice [S.alice, S.carol])
      [S.carol]
  -- CR 310.11's second sentence, listed as CR 704.5x's and CR 704.5y's: the branch
  -- that puts the battle into its owner's graveyard. Held HERE rather than at the
  -- game level because it is
  -- unreachable there: a Siege's candidates are its controller's opponents still
  -- in the game, and a game in which its controller has no opponent left has
  -- already ended under CR 104.2a. Pawl.Engine.Sba routes an empty answer into
  -- the put-into-graveyard batch whether or not a game can reach it (#853).
  Spec.it s "CR 704.5x a Siege whose controller is alone has no candidate" $ do
    siege <- siegePC s registry
    Spec.assertEqWith s "nobody" (Battle.protectorCandidates siege S.alice [S.alice]) []
  Spec.it s "CR 704.5y a Siege protected by its own controller needs repair" $ do
    siege <- siegePC s registry
    Spec.assertBool
      s
      (Battle.needsProtector siege S.alice [S.alice, S.bob] False (Just S.alice))
      "the controller is not a legal protector of their own Siege"
  Spec.it s "CR 310.11 a legal designation needs no repair" $ do
    siege <- siegePC s registry
    Spec.assertBool
      s
      (not (Battle.needsProtector siege S.alice [S.alice, S.bob] False (Just S.bob)))
      "bob is legal and is left alone"

-- CR 704.5x: the designation is repaired by a state-based action once the
-- designated player is no longer in the game.
repairSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
repairSpec s registry = Spec.describe s "Repair" $ do
  Spec.it s "CR 704.5x a battle whose protector leaves the game gets a new one" $ do
    (entered, oid) <- castInvasionThreeSeated s registry (protectTo S.carol)
    Spec.assertEqWith s "carol protects it to begin with" (protectorOf oid entered) (Just S.carol)
    let gone = Departure.depart Departure.Type.Conceded S.carol entered
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
    let gone = Departure.depart Departure.Type.Conceded S.carol entered
        (acted, _) = S.runPureWith S.identityAnswer gone Sba.performStateBasedActions
    Spec.assertBool s acted "the pass reports the repair"
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
        let after = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.alice)
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
        let after = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.alice)
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
        let after = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.alice)
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
        let after = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.alice)
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
        let after = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.alice)
        Spec.assertBool s (Combat.legalBlockDeclaration S.carol (Map.singleton blocker (Set.singleton wraith)) after) "legal"
      _ -> Spec.assertFailure s "fixture should have a Wraith and a blocker"

  Spec.it s "CR 310.9c a creature the protector does not control can't block the battle's attacker" $ do
    -- CR 310.9c: "creatures controlled by other players can't block those
    -- attackers". bob holds the board's only untapped creature besides the
    -- attacker, and bob protects nothing. Nothing forbids it explicitly -- CR
    -- 509.1a already restricts blocking to the defending player, and CR 310.9b
    -- makes the battle attackable only through its protector, so the two rules
    -- meet and bob is never asked.
    piker <- S.printingOf s registry "Goblin Piker"
    (gs, battle, mine, _, _) <- battleCombat s registry S.carol S.carol [piker] [piker] []
    let attacked = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.alice)
        blocked = S.runPure S.aggressiveAnswer attacked Combat.declareBlockers
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
        let attacked = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.alice)
            blocked = S.runPure S.aggressiveAnswer attacked Combat.declareBlockers
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
    let attacked = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.alice)
        gone = Departure.depart Departure.Type.Conceded S.carol attacked
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
    let gone = Departure.depart Departure.Type.Conceded S.carol gs
        checked = S.runPure S.identityAnswer gone Sba.checkStateBasedActions
    Spec.assertBool s (not (Battle.isBeingAttacked battle gone)) "nothing is attacking it"
    Spec.assertEqWith s "bob protects it now" (protectorOf battle checked) (Just S.bob)

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
        let attacked = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.alice)
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
    let attacked = S.runPure (attackTheBattle battle) gs (Combat.declareAttackers S.alice)
        killed = S.runPure S.identityAnswer attacked (Event.destroy Regenerability.Regenerable [battle])
        dealt = S.runPure S.identityAnswer killed (Monad.void Damage.dealCombatDamage)
    Spec.assertEqWith s "no damage was dealt at all" (S.damageEventsOf dealt) []

-- CR 310.12b and CR 310.7 / 704.5v: what happens when the last defense counter
-- comes off. The two rules are only jointly observable -- 704.5v alone would send
-- the Siege to a graveyard where 310.12b exiles it -- so every case here asserts
-- the DESTINATION zone rather than merely that the battle left.
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
  let withLands = List.foldl' (\g _ -> snd (S.addCreature mountain S.alice g)) entered printings
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
          (\(ids, g1) p -> let (oid, g2) = S.addCreature p pid g1 in (ids <> [oid], g2))
          ([], g)
          ps
      (ours, gs1) = addAll S.alice mine entered
      (yours, gs2) = addAll S.bob theirs gs1
      (theirsToo, gs3) = addAll S.carol hers gs2
  pure
    ( gs3
        { GameState.activePlayer = S.alice,
          GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
          GameState.combat = (GameState.combat gs3) {Combat.Type.defender = Just defender}
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
  let lands = List.foldl' (\g _ -> snd (S.addCreature plains S.alice g)) S.threePlayerGame [1 :: Int .. 3]
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
  Nothing -> do
    Spec.assertFailure s "Invasion of Dominaria did not reach the battlefield"
    pure (gs, ObjectId.MkObjectId 0)
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
