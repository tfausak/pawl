{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Combat up to the end of the declare blockers step: what may
-- be declared as an attacker or a blocker, and what the evasion keywords
-- (flying, reach, defender, landwalk, menace, fear) and the requirement and
-- restriction effects do to those declarations. The rest of the step order --
-- damage assignment, removal from combat, and the continuous effects that reach
-- combat -- is Pawl.CombatEffectSpec, which describes under the same name.
-- Also Pawl.Engine.BlockRequirement, whose only consumer is Pawl.Engine.Combat's CR 509.1c
-- check, Pawl.Engine.AttackRequirement, whose only consumer is its CR 508.1d
-- check, Pawl.Engine.CombatRestriction, whose only consumer is that module's CR
-- 508.1c and CR 509.1b checks, and Pawl.Engine.BlockPermission, whose only
-- consumer is its CR 509.1a arity.
module Pawl.CombatSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActiveAttackProhibition as ActiveAttackProhibition
import qualified Pawl.Types.ActiveBlockProhibition as ActiveBlockProhibition
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AttackOption as AttackOption
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.BlocksDeclared as BlocksDeclared
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RestrictedCreatures as RestrictedCreatures
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

combatDamageSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
combatDamageSpec s registry = Spec.describe s "CombatDamage" $ do
  Spec.it s "CR 510.1b an unblocked attacker damages the defending player" $ do
    let board = S.duel S.beginningOfCombat [S.settled "attacker" "Goblin Piker"] []
        script = S.turn 1 [S.on S.declareAttackers S.alice (S.attack [S.aliasRef "attacker"])]
    after <- S.play s registry board script S.combatGame
    -- A Piker is a 2/1, and bob starts at 20.
    Spec.assertEqWith s "bob took 2" (S.lifeOf S.bob after) (Just 18)
  Spec.it s "CR 510.1a Tapestry Warden substitutes toughness only where greater than power" $ do
    warden <- S.printingOf s registry "Tapestry Warden"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [warden, piker] []
        after = S.runCombat S.aggressiveAnswer gs
    -- The 3/4 Warden assigns 4; the 2/1 Piker still assigns 2.
    Spec.assertEqWith s "defender took six" (S.lifeOf S.bob after) (Just 14)
  Spec.it s "CR 613.11 Tapestry Warden compares power and toughness after characteristic effects" $ do
    warden <- S.printingOf s registry "Tapestry Warden"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [warden, piker] []
    case mine of
      [_, pikerId] -> do
        let after = S.runCombat S.aggressiveAnswer (withToughnessBoost pikerId gs)
        -- The final 2/3 Piker joins the Warden in assigning with toughness.
        Spec.assertEqWith s "defender took seven" (S.lifeOf S.bob after) (Just 13)
      _ -> Spec.assertFailure s "fixture should have two attackers"
  Spec.it s "CR 510.1d Tapestry Warden substitutes toughness for a blocker too" $ do
    warden <- S.printingOf s registry "Tapestry Warden"
    let (gs, mine, _) = S.combatBoard warden 1 1
        after = S.fightWith S.aggressiveAnswer gs
    case mine of
      [] -> Spec.assertFailure s "fixture should have an attacker"
      attacker : _ -> Spec.assertEqWith s "attacker took four" (S.damageOf attacker after) (Just 4)
  Spec.it s "CR 702.19b a trampling Tapestry Warden spills the excess over its toughness" $ do
    warden <- S.printingOf s registry "Tapestry Warden"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [warden] [piker]
    case mine of
      [] -> Spec.assertFailure s "fixture should have an attacker"
      attacker : _ -> do
        -- CR 702.19b's excess is excess over what the creature ASSIGNS, so the
        -- 3/4 Warden divides 4: one lethal point onto the 2/1 Piker and three
        -- over. Reading power would leave bob at 18.
        let after = S.settleSba (S.fightWith trampleThresholdAnswer (withTrample attacker gs))
        Spec.assertEqWith s "defender took three over the blocker" (S.lifeOf S.bob after) (Just 17)
        Spec.assertEqWith s "and the blocker took its lethal point" (S.creaturesInPlay S.bob after) 0
  Spec.it s "CR 509 a blocked attacker does not damage the player" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoard piker 1 1
        after = S.fightWith S.aggressiveAnswer gs
    Spec.assertEqWith s "bob untouched" (S.lifeOf S.bob after) (Just 20)
  Spec.it s "CR 510.1c a single blocker takes all the damage, unprompted" $ do
    -- If the engine wrongly prompts here, this interpreter answers with an
    -- empty division, which is illegal (it does not total the attacker's
    -- power), so it is rejected and the blocker takes 0 -- and the assertion
    -- below fails. That is why this proves "unprompted" without an `error`,
    -- which the no-partial-functions rule forbids anyway.
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, theirs) = S.combatBoard piker 1 1
        noAssign :: Prompt.Prompt r -> r
        noAssign p = case p of
          Prompt.AssignCombatDamage {} -> Map.empty
          _ -> S.aggressiveAnswer p
        after = S.fightWith noAssign gs
    case theirs of
      [] -> Spec.assertFailure s "fixture should have a blocker"
      b : _ -> Spec.assertEqWith s "took 2" (S.damageOf b after) (Just 2)
  Spec.it s "CR 510.2 a 2/1 trade kills BOTH creatures" $ do
    -- The simultaneity test. Sequential damage kills only one, because the
    -- blocker would be in the graveyard before it dealt its damage.
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoard piker 1 1
        after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
    Spec.assertEqWith s "alice's is dead" (S.creaturesInPlay S.alice after) 0
    Spec.assertEqWith s "bob's is dead" (S.creaturesInPlay S.bob after) 0
  Spec.it s "CR 510.1c a free division of 2 across two blockers kills both" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, theirs) = S.combatBoard piker 1 2
        split :: Prompt.Prompt r -> r
        split p = case p of
          Prompt.AssignCombatDamage _ _ _ thresholds _ -> Map.fromList (fmap (\r -> (r, 1)) (filter S.isCreatureRecipient (Map.keys thresholds)))
          _ -> S.aggressiveAnswer p
        after = S.settleSba (S.fightWith split gs)
    Spec.assertEqWith s "both blockers dead" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "expected two blockers" (length theirs) 2
  Spec.it s "CR 510.1c the same 2 damage on one blocker kills only it" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoard piker 1 2
        dump :: Prompt.Prompt r -> r
        dump p = case p of
          Prompt.AssignCombatDamage _ _ _ thresholds n ->
            case filter S.isCreatureRecipient (Map.keys thresholds) of
              r : _ -> Map.singleton r n
              [] -> Map.empty
          _ -> S.aggressiveAnswer p
        after = S.settleSba (S.fightWith dump gs)
    Spec.assertEqWith s "one blocker survives" (S.creaturesInPlay S.bob after) 1
  Spec.it s "CR 510.1e an illegal division is rejected and deals nothing" $ do
    -- Not a reachable game state: this is the engine's defense against a
    -- broken interpreter. See the spec, section 3.
    let board =
          S.duel
            S.beginningOfCombat
            [S.settled "attacker" "Goblin Piker"]
            [S.settled "first" "Goblin Piker", S.settled "second" "Goblin Piker"]
        attacker = S.aliasRef "attacker"
        first = S.aliasRef "first"
        second = S.aliasRef "second"
        script =
          S.turn
            1
            [ S.on S.declareAttackers S.alice (S.attack [attacker]),
              S.on S.declareBlockers S.bob (S.block [(first, attacker), (second, attacker)]),
              S.onSource
                S.combatDamage
                S.alice
                attacker
                (S.assignDamage [(S.MkCreatureRecipient first, 99), (S.MkCreatureRecipient second, 99)])
            ]
    fought <- S.play s registry board script S.combatGame
    Spec.assertEqWith s "both blockers survive" (S.creaturesInPlay S.bob (S.settleSba fought)) 2
  Spec.it s "CR 510.1a a legal division deals the damage it names" $ do
    -- The accepting counterpart to the rejection above: the same board, a legal
    -- 1/1 division, and both blockers dead. A rejection outcome alone is green
    -- for any recipient-resolution bug, since a misdirected map is illegal too.
    let board =
          S.duel
            S.beginningOfCombat
            [S.settled "attacker" "Goblin Piker"]
            [S.settled "first" "Goblin Piker", S.settled "second" "Goblin Piker"]
        attacker = S.aliasRef "attacker"
        first = S.aliasRef "first"
        second = S.aliasRef "second"
        script =
          S.turn
            1
            [ S.on S.declareAttackers S.alice (S.attack [attacker]),
              S.on S.declareBlockers S.bob (S.block [(first, attacker), (second, attacker)]),
              S.onSource
                S.combatDamage
                S.alice
                attacker
                (S.assignDamage [(S.MkCreatureRecipient first, 1), (S.MkCreatureRecipient second, 1)])
            ]
    fought <- S.play s registry board script S.combatGame
    -- A Piker is a 2/1, so one point is lethal (CR 510.1c) and both blockers die.
    Spec.assertEqWith s "both blockers died" (S.creaturesInPlay S.bob (S.settleSba fought)) 0
  -- The deterministic successor to the retired "combat happens" property: an
  -- unblocked 2/1 attacker reduces the defender's life by its power.
  Spec.it s "combat deals damage to the defending player" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [piker] []
        after = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith s "defender took two" (S.lifeOf S.bob after) (Just 18)

declaredAttackers :: GameState.GameState -> [ObjectId.ObjectId]
declaredAttackers gs = Map.keys (Combat.Type.attackers (GameState.combat gs))

declareSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
declareSpec s registry = Spec.describe s "Declare" $ do
  Spec.it s "CR 508.1f declaring an attacker taps it" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoard piker 1 1
        after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.manaPerformer S.alice))
    Spec.assertEqWith s "one attacker" (declaredAttackers after) mine
    Spec.assertEqWith s "tapped" (S.tappedCount S.alice after) 1
  Spec.it s "CR 508.1 attackers attack the defending player" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoard piker 1 1
        after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.manaPerformer S.alice))
    case mine of
      [] -> Spec.assertFailure s "fixture should have an attacker"
      oid : _ ->
        Spec.assertEqWith
          s
          "attacking bob"
          (Map.lookup oid (Combat.Type.attackers (GameState.combat after)))
          (Just (AttackTarget.OfPlayer S.bob))
  Spec.it s "an illegal attacker in the answer is dropped" $ do
    -- The interpreter names bob's creature. It is not alice's to attack with.
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, theirs) = S.combatBoard piker 1 1
        liar :: Prompt.Prompt r -> r
        liar p = case p of
          Prompt.DeclareAttackers {} -> theirs
          _ -> S.aggressiveAnswer p
        after = snd (Engine.runGamePure liar gs (Combat.declareAttackers S.manaPerformer S.alice))
    Spec.assertEqWith s "nothing attacks" (declaredAttackers after) []
  Spec.it s "CR 509.1 a blocker is recorded against the attacker it blocks" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = S.combatBoard piker 1 1
        steps = do
          Combat.declareAttackers S.manaPerformer S.alice
          Combat.declareBlockers S.manaPerformer
        after = snd (Engine.runGamePure S.aggressiveAnswer gs steps)
    case mine of
      [] -> Spec.assertFailure s "fixture should have an attacker"
      attacker : _ ->
        Spec.assertEqWith s "blocked by bob's creature" (Combat.blockersOf attacker after) (Set.fromList theirs)
  Spec.it s "an unblocked attacker has no blockers" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoard piker 1 0
        steps = do
          Combat.declareAttackers S.manaPerformer S.alice
          Combat.declareBlockers S.manaPerformer
        after = snd (Engine.runGamePure S.aggressiveAnswer gs steps)
    case mine of
      [] -> Spec.assertFailure s "fixture should have an attacker"
      attacker : _ -> Spec.assertBool s (not (Combat.isBlocked attacker after)) "unblocked"
  Spec.it s "no legal attackers means no prompt and no attacks" $ do
    -- combatBoard 0 1 gives alice nothing. A prompt here would be the engine
    -- asking a question with exactly one answer.
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoard piker 0 1
        after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.manaPerformer S.alice))
    Spec.assertEqWith s "nothing attacks" (declaredAttackers after) []
  -- The end-to-end summoning sickness scenario the spec names: a creature
  -- that just arrived cannot attack, and the SAME creature can once its
  -- controller's untap step has settled it. The halves are tested in Tasks 1
  -- and 4; this proves they compose.
  Spec.it s "CR 302.6 a creature cannot attack the turn it arrives, and can after untapping" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoard piker 1 1
        arrived = justArrived gs
        sameTurn = snd (Engine.runGamePure S.aggressiveAnswer arrived (Combat.declareAttackers S.manaPerformer S.alice))
        nextTurn =
          snd
            . Engine.runGamePure S.aggressiveAnswer arrived
            $ do
              Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap)
              Combat.declareAttackers S.manaPerformer S.alice
    Spec.assertEqWith s "cannot attack the turn it arrives" (declaredAttackers sameTurn) []
    Spec.assertEqWith s "can attack after untapping" (length (declaredAttackers nextTurn)) 1

defenderSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
defenderSpec s registry = Spec.describe s "Defender" $ do
  Spec.it s "CR 702.3b a creature with defender can't attack" $ do
    ogreSentry <- S.printingOf s registry "Ogre Sentry"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [ogreSentry] [piker]
    case mine of
      [] -> Spec.assertFailure s "fixture should have one creature"
      oid : _ -> Spec.assertBool s (not (Combat.canAttack S.alice oid gs)) "can't attack"
  Spec.it s "CR 702.3b a creature with defender is not offered as a legal attacker" $ do
    ogreSentry <- S.printingOf s registry "Ogre Sentry"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [ogreSentry] [piker]
    Spec.assertEqWith s "none" (Combat.legalAttackers S.alice gs) []
  Spec.it s "CR 702.3b defender does not stop it blocking" $ do
    -- 702.3b says "can't attack" and nothing else. A defender that could not
    -- block would be a Wall in the pre-2004 sense, and that is not the rule.
    piker <- S.printingOf s registry "Goblin Piker"
    ogreSentry <- S.printingOf s registry "Ogre Sentry"
    let (gs, _, theirs) = S.combatBoardOf [piker] [ogreSentry]
    case theirs of
      [] -> Spec.assertFailure s "fixture should have one blocker"
      oid : _ -> Spec.assertBool s (Combat.canBlock S.bob oid gs) "may block"
  Spec.it s "a creature without defender is still offered" $ do
    -- The control. If defender were implemented as "nothing may attack", the
    -- test above would pass and this one would fail.
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker] [piker]
    Spec.assertEqWith s "one" (Combat.legalAttackers S.alice gs) mine
  Spec.it s "CR 702.3b a defender is skipped but its neighbor still attacks" $ do
    ogreSentry <- S.printingOf s registry "Ogre Sentry"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [ogreSentry, piker] [piker]
    case mine of
      [_, p] -> Spec.assertEqWith s "only the piker" (Combat.legalAttackers S.alice gs) [p]
      _ -> Spec.assertFailure s "fixture should have two creatures"

-- Answers Prompt.ChooseDefender with a named player and records that it was
-- asked; everything else delegates, so the wildcard keeps this out of the
-- -Werror exhaustiveness net.
choosesDefender :: PlayerId.PlayerId -> Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
choosesDefender who p = case p of
  Prompt.ChooseDefender _ asker _ -> do
    State.modify' (\seen -> seen <> [asker])
    pure who
  _ -> pure (S.identityAnswer p)

-- Sibling of choosesDefender that records the prompt's Decider instead of its
-- PlayerId subject. CR 723.1/723.5: while a player is controlled, their
-- controller makes their choices, and Combat.designateDefenders's
-- `Decide.deciderFor pid gs` is what routes ChooseDefender there. choosesDefender
-- above discards the Decider entirely, so it cannot tell that routing apart from
-- a regression to the raw active-player id; this helper is what makes the
-- Decider observable without touching the six existing cases.
choosesDefenderRecordingDecider :: PlayerId.PlayerId -> Prompt.Prompt r -> State.State [Decider.Decider] r
choosesDefenderRecordingDecider who p = case p of
  Prompt.ChooseDefender decider _ _ -> do
    State.modify' (\seen -> seen <> [decider])
    pure who
  _ -> pure (S.identityAnswer p)

-- Records the PlayerId of every Prompt.DeclareBlockers ask AND the blocker
-- candidates that ask was offered, then blocks the first attacker with
-- everything. Two accumulators threaded as one State pair: the ASK is what
-- CR 509.1 is about (who declares), and the OFFER is what CR 509.1a is about
-- (whose creatures are even eligible). Everything else delegates, so the
-- wildcard keeps this out of the -Werror exhaustiveness net.
recordingBlockers :: Prompt.Prompt r -> State.State ([PlayerId.PlayerId], [ObjectId.ObjectId]) r
recordingBlockers p = case p of
  Prompt.DeclareBlockers _ pid candidates attackers -> do
    State.modify' (\(asks, offers) -> (asks <> [pid], offers <> candidates))
    pure $ case attackers of
      [] -> Map.empty
      a : _ -> Map.fromList (fmap (\b -> (b, Set.singleton a)) candidates)
  _ -> pure (S.identityAnswer p)

-- Run Combat.declareBlockers under recordingBlockers. State.runState (State s a)
-- s0 :: (a, s), so the tuple comes back (final state, accumulators) and this
-- flips it to put the accumulators first.
runRecordingBlockers :: GameState.GameState -> (([PlayerId.PlayerId], [ObjectId.ObjectId]), GameState.GameState)
runRecordingBlockers gs =
  let (after, seen) = State.runState (fmap snd (Engine.runGame recordingBlockers gs (Combat.declareBlockers S.manaPerformer))) ([], [])
   in (seen, after)

-- CR 802: the attack multiple players option, which every game pawl starts uses
-- (Pawl.Types.GameSettings.attackOption). THREE SEATS throughout, since
-- at two the option and CR 506.2's base rule coincide exactly and nothing here
-- can differ.
--
-- One board carries the whole rule, because CR 802's five clauses are five
-- questions about one combat: who defends (802.2), whom each creature attacks
-- (802.3), who is asked to block and about what (802.4, 802.4a/b), and in what
-- order damage is announced (802.5).
--
-- carol's Palace Guard is created BEFORE bob's, so her id is the LOWER one. That
-- is the discriminator for the two ordering assertions: the implementations this
-- replaces walked Combat.blockers in ascending ObjectId order, which on this
-- board answers carol first, where CR 802.4 and CR 802.5 both answer bob.
--
-- Palace Guard (1/4, "can block any number of creatures") and not a plain
-- blocker, for the damage assertion alone: CR 510.1d asks nothing of a creature
-- blocking ONE attacker, so a one-block board raises no AssignCombatDamage prompt
-- to put in an order. Two attackers apiece is what makes each guard's division a
-- real choice.
attackMultiplePlayersSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
attackMultiplePlayersSpec s registry = Spec.describe s "AttackMultiplePlayers" $ do
  Spec.it s "CR 802.2/802.3/802.4/802.5 alice attacks both opponents, each blocks in turn order, and damage is announced in turn order" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    guard <- S.printingOf s registry "Palace Guard"
    let (board, mine, _, hers) = S.threePlayerCombat [piker, piker, piker, piker] [] [guard]
        (bobsGuard, staged) = S.addPermanent guard S.bob board
    case (mine, hers) of
      ([atBob1, atBob2, atCarol1, atCarol2], [carolsGuard]) -> do
        let -- CR 508.1b / CR 802.3: each creature announces whom it attacks, by
            -- id rather than by prompt order, so the declaration is pinned even
            -- if the engine asks in a different sequence.
            aimed oid = if List.elem oid [atBob1, atBob2] then S.bob else S.carol
            declaring :: Prompt.Prompt r -> r
            declaring p = case p of
              Prompt.ChooseAttackTarget _ _ oid options ->
                Maybe.fromMaybe (NonEmpty.head options) (List.find (== AttackTarget.OfPlayer (aimed oid)) (NonEmpty.toList options))
              _ -> S.aggressiveAnswer p
            settled = S.runPure S.identityAnswer staged (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))
            declared = S.runPure declaring settled (Combat.declareAttackers S.manaPerformer S.alice)
        -- CR 802.2: nobody was chosen and BOTH opponents defend, in APNAP order.
        Spec.assertEqWith s "CR 802.2 both opponents are defending players" (Combat.Type.defenders (GameState.combat declared)) [S.bob, S.carol]
        -- CR 802.3: one declaration, two victims.
        Spec.assertEqWith
          s
          "CR 802.3 two creatures attack bob and two attack carol"
          (Map.elems (Combat.Type.attackers (GameState.combat declared)))
          [AttackTarget.OfPlayer S.bob, AttackTarget.OfPlayer S.bob, AttackTarget.OfPlayer S.carol, AttackTarget.OfPlayer S.carol]
        -- CR 802.4 / 802.4a: each defending player is asked, in APNAP order, and
        -- offered only the creatures attacking them. Recorded as (who was asked,
        -- what they were offered) so the two clauses cannot be confused.
        let blocking :: Prompt.Prompt r -> State.State [(PlayerId.PlayerId, [ObjectId.ObjectId])] r
            blocking p = case p of
              Prompt.DeclareBlockers _ pid candidates attackers -> do
                State.modify' (<> [(pid, attackers)])
                pure (Map.fromList (fmap (\b -> (b, Set.fromList attackers)) candidates))
              _ -> pure (S.identityAnswer p)
            (blocked, asked) =
              State.runState
                (fmap snd (Engine.runGame blocking declared (Combat.declareBlockers S.manaPerformer)))
                []
        Spec.assertEqWith
          s
          "CR 802.4 bob declares before carol, whose Guard has the lower id"
          asked
          [(S.bob, [atBob1, atBob2]), (S.carol, [atCarol1, atCarol2])]
        -- CR 802.4a / 802.4b as a LEGALITY question rather than an offer, since
        -- an interpreter can propose a block that was never offered. The pair
        -- differs in one thing -- which attacker carol's Guard is declared
        -- against -- so the negative cannot pass for want of a legal blocker.
        Spec.assertBool
          s
          (not (Combat.legalBlockDeclaration S.carol (Map.singleton carolsGuard (Set.singleton atBob1)) declared))
          "CR 802.4a carol may not block a creature attacking bob"
        Spec.assertBool
          s
          (Combat.legalBlockDeclaration S.carol (Map.singleton carolsGuard (Set.fromList [atCarol1, atCarol2])) declared)
          "CR 802.4b and the same Guard may block both creatures attacking her"
        -- CR 802.5 / CR 703.4k: each player in APNAP order announces how their
        -- creatures assign. alice's four attackers are each blocked by one
        -- creature and so are forced (CR 510.1c), leaving the two Guards as the
        -- only creatures with a division to announce.
        let assigning :: Prompt.Prompt r -> State.State [ObjectId.ObjectId] r
            assigning p = case p of
              Prompt.AssignCombatDamage _ _ source thresholds power -> do
                State.modify' (<> [source])
                pure
                  ( case Map.keys thresholds of
                      recipient : _ -> Map.singleton recipient power
                      [] -> Map.empty
                  )
              _ -> pure (S.identityAnswer p)
            announced =
              State.execState
                (Engine.runGame assigning blocked (Damage.gatherCombatDamage (const True)))
                []
        Spec.assertEqWith
          s
          "CR 802.5 bob's Guard announces before carol's, whose id is lower"
          announced
          [bobsGuard, carolsGuard]
      _ -> Spec.assertFailure s "fixture should give alice four Pikers and carol one Palace Guard"
  Spec.it s "CR 802.4/509.1a a defending player with nothing attacking them is not asked to declare blockers" $ do
    -- Both opponents defend (CR 802.2) and both hold an untapped Palace Guard,
    -- so the only thing that differs between the two legs is WHICH of them the
    -- one Piker was aimed at. attemptBlockDeclaration's own `null candidates`
    -- guard cannot account for either: each player has a creature that could
    -- block.
    piker <- S.printingOf s registry "Goblin Piker"
    guard <- S.printingOf s registry "Palace Guard"
    let (board, _, _, _) = S.threePlayerCombat [piker] [guard] [guard]
        askedWhenAttacking who =
          let settled = S.runPure S.identityAnswer board (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))
              declared = S.runPure (S.attackTo who) settled (Combat.declareAttackers S.manaPerformer S.alice)
           in fst (fst (runRecordingBlockers declared))
    Spec.assertEqWith s "CR 802.4 only bob, the one being attacked, is asked" (askedWhenAttacking S.bob) [S.bob]
    Spec.assertEqWith s "and only carol on the same board with the attack aimed at her" (askedWhenAttacking S.carol) [S.carol]
  Spec.it s "CR 802.4b a requirement on a creature attacking bob does not reach carol's blocker" $ do
    -- CR 802.4b's other half: not which blocks are ALLOWED, but which
    -- requirements CR 509.1c makes carol maximize. Lure ("All creatures able to
    -- block enchanted creature do so") on a creature attacking BOB is the
    -- producer -- carol's Guard is not able to block it at all, so no instance
    -- is minted for her, and her own declaration stays legal.
    --
    -- Its own case rather than another assertion on the board above, because
    -- the Lure changes what BOB may legally declare and that board asserts the
    -- offer he was given.
    piker <- S.printingOf s registry "Goblin Piker"
    guard <- S.printingOf s registry "Palace Guard"
    lure <- S.printingOf s registry "Lure"
    let (board, mine, _, hers) = S.threePlayerCombat [piker, piker, piker, piker] [] [guard]
        (bobsGuard, staged) = S.addPermanent guard S.bob board
    case (mine, hers) of
      ([atBob1, atBob2, atCarol1, atCarol2], [carolsGuard]) -> do
        let aimed oid = if List.elem oid [atBob1, atBob2] then S.bob else S.carol
            declaring :: Prompt.Prompt r -> r
            declaring p = case p of
              Prompt.ChooseAttackTarget _ _ oid options ->
                Maybe.fromMaybe (NonEmpty.head options) (List.find (== AttackTarget.OfPlayer (aimed oid)) (NonEmpty.toList options))
              _ -> S.aggressiveAnswer p
            settled = S.runPure S.identityAnswer staged (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))
            declared = S.runPure declaring settled (Combat.declareAttackers S.manaPerformer S.alice)
            (aura, withAura) = S.addPermanent lure S.alice declared
            lured = S.attach aura atBob1 withAura
        -- THE GAMEPLAY ASSERTION, and first so nothing ahead of it can absorb a
        -- mutation: the Lured creature is attacking bob, so CR 802.4b has carol
        -- judge her blocks as though it were not there.
        Spec.assertBool
          s
          (Combat.legalBlockDeclaration S.carol (Map.singleton carolsGuard (Set.fromList [atCarol1, atCarol2])) lured)
          "CR 802.4b carol's own blocks are legal though a Lured creature goes unblocked"
        -- The control, and the anti-vacuity check: the same Lure DOES bind bob,
        -- whose Guard is able to block it, so a board where the Lure never took
        -- effect fails here rather than passing the assertion above for free.
        Spec.assertBool
          s
          (not (Combat.legalBlockDeclaration S.bob (Map.singleton bobsGuard (Set.singleton atBob2)) lured))
          "CR 509.1c bob, who can block it, may not leave the Lured creature unblocked"
      _ -> Spec.assertFailure s "fixture should give alice four Pikers and carol one Palace Guard"

-- CR 803.1a/803.1b: the attack left and attack right options, which cut CR
-- 506.2a's candidate list down to the ONE seat next to the attacking player.
-- THREE and FOUR seats, since at two the two options and CR 506.2's base rule
-- all name the same one opponent and nothing here could differ.
--
-- Both cases drive the whole declaration rather than reading
-- Combat.attackableOpponents, because the candidate list is not the deliverable:
-- what CR 803.1 restricts is whom a creature ends up attacking.
attackLeftRightSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
attackLeftRightSpec s registry = Spec.describe s "AttackLeftRight" $ do
  Spec.it s "CR 803.1a/803.1b alice attacks the neighbouring seat, and the option says which one" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    -- Seated [alice, bob, carol] with alice attacking, so bob is immediately to
    -- her left and carol immediately to her right.
    --
    -- ONE answerer for both legs, aimed at CAROL: under attack left she is not
    -- a candidate at all, so the interpreter that takes her under attack right
    -- cannot take her here. That is what discriminates against a candidate list
    -- left unrestricted, which would answer carol on both legs.
    let (board, _, _, _) = S.threePlayerCombat [piker] [] []
        declaredWith option =
          let staged = S.attackOption (Just option) board
              settled = S.runPure (S.attackTo S.carol) staged (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))
           in S.runPure (S.attackTo S.carol) settled (Combat.declareAttackers S.manaPerformer S.alice)
        left = declaredWith AttackOption.Leftward
        right = declaredWith AttackOption.Rightward
    Spec.assertEqWith
      s
      "CR 803.1a the creature attacks bob, the seat to alice's left"
      (Map.elems (Combat.Type.attackers (GameState.combat left)))
      [AttackTarget.OfPlayer S.bob]
    Spec.assertEqWith
      s
      "CR 803.1b and attacks carol, the seat to alice's right, on the same board"
      (Map.elems (Combat.Type.attackers (GameState.combat right)))
      [AttackTarget.OfPlayer S.carol]
    -- The designation behind those two, after them so it cannot absorb a
    -- mutation: CR 803.1 leaves one candidate, so CR 507.1's choice is settled
    -- without a prompt.
    Spec.assertEqWith
      s
      "CR 507.1 the left-hand neighbour is the only defending player"
      (Combat.Type.defenders (GameState.combat left))
      [S.bob]
    Spec.assertEqWith
      s
      "CR 507.1 and the right-hand neighbour is, on the other leg"
      (Combat.Type.defenders (GameState.combat right))
      [S.carol]
  Spec.it s "CR 803.1a a player whose nearest opponent to the left is more than one seat away can't attack" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    -- FOUR seats, because three cannot put an opponent two seats away in one
    -- direction while leaving one adjacent in the other.
    --
    -- bob concedes, so the seat immediately to alice's left is empty and carol,
    -- the nearest opponent that way, is two seats off. GameState.turnOrder is
    -- the permanent seating roster, which is what makes that distance real
    -- rather than closing the gap.
    let (_, seated) = S.addPermanent piker S.alice S.fourPlayerGame
        board = seated {GameState.phase = Phase.Combat CombatStep.BeginningOfCombat}
        gone = S.departs Departure.Type.Conceded S.bob board
        declaredWith option gs =
          let staged = S.attackOption (Just option) gs
              settled = S.runPure (S.attackTo S.carol) staged (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))
           in S.runPure (S.attackTo S.carol) settled (Combat.declareAttackers S.manaPerformer S.alice)
    Spec.assertEqWith
      s
      "CR 803.1a nothing attacks when the seat to alice's left is empty"
      (Map.elems (Combat.Type.attackers (GameState.combat (declaredWith AttackOption.Leftward gone))))
      []
    -- The paired board, differing in the option and nothing else: dave is still
    -- seated immediately to alice's right, so the same departure leaves that
    -- direction attackable. Without it the case above passes for want of a
    -- creature or a step.
    Spec.assertEqWith
      s
      "CR 803.1b dave, immediately to alice's right, is attacked on the same departed board"
      (Map.elems (Combat.Type.attackers (GameState.combat (declaredWith AttackOption.Rightward gone))))
      [AttackTarget.OfPlayer S.dave]
    -- And the paired board differing in the departure and nothing else, which
    -- is what makes it the EMPTY SEAT rather than the seat count that stops
    -- the attack.
    Spec.assertEqWith
      s
      "CR 803.1a and bob is attacked while he is still seated there"
      (Map.elems (Combat.Type.attackers (GameState.combat (declaredWith AttackOption.Leftward board))))
      [AttackTarget.OfPlayer S.bob]
    -- CR 803.1a is no attack, not a fallback: carol never becomes a defending
    -- player. After the gameplay assertions, being the same claim upstream.
    Spec.assertEqWith
      s
      "CR 803.1a nobody is designated a defending player"
      (Combat.Type.defenders (GameState.combat (declaredWith AttackOption.Leftward gone)))
      []

-- CR 506.2/506.2a/507.1/703.4h: WHO is being attacked. Distinct from
-- defenderSpec, which is the Defender KEYWORD (CR 702.3b).
defendingPlayerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
defendingPlayerSpec s registry = Spec.describe s "DefendingPlayer" $ do
  Spec.it s "CR 703.4h/507.1 the active player chooses which opponent is the defending player" $ do
    -- THREE seats: the whole point. Discriminating against the behaviour this
    -- phase replaces -- taking the head of the candidate list -- because carol
    -- is not the head. Under head-of-list the answer is ignored and bob
    -- defends, so this exact assertion cannot pass.
    --
    -- State.runState (State s a) s0 :: (a, s): here `a` is the GameState
    -- Engine.runGame returns and `s` is choosesDefender's own accumulator, so
    -- the tuple comes back (after, asked).
    let (after, asked) =
          State.runState
            (fmap snd (Engine.runGame (choosesDefender S.carol) (S.oneDefendingPlayer S.threePlayerGame) (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))))
            []
    Spec.assertEqWith s "carol is the defending player" (Combat.Type.defenders (GameState.combat after)) [S.carol]
    Spec.assertEqWith s "and alice, the active player, is who was asked" asked [S.alice]
  Spec.it s "CR 723.1 a controlled active player's choice of defender routes to their controller" $ do
    -- THREE seats (alice active, bob, carol) with carol controlling alice
    -- (Mindslaver-style: GameState.activeControl = Just (MkDecider carol)),
    -- the established fixture idiom for a controlled player -- the same shape
    -- GameSpec's CR 723.6 concede test (GameSpec.hs:811-858) and DecideSpec's
    -- CR 723.3 case (DecideSpec.hs:19-23) both set up. Three seats, not two, so
    -- attackableOpponents is [bob, carol] -- a REAL choice -- rather than
    -- CR 506.2's one-candidate elision, which would never build a prompt at all
    -- and so could never discriminate the Decider on one.
    --
    -- CR 723.1: "The affected player is controlled during the entire turn"
    -- (the rest of the rule scopes this to the player's own next turn, which
    -- does not change the point here). Combined with CR 723.5 (the controller
    -- makes the controlled player's choices) and CR 723.3 (the controlled
    -- player is still the active player), alice remains the active player
    -- named in the prompt but carol is who must be asked. Combat.designateDefenders
    -- gets this right by computing `Decide.deciderFor pid gs` rather than
    -- defaulting to `Decider.MkDecider pid`.
    --
    -- Discriminates exactly that regression: a `designateDefenders` that used
    -- `Decider.MkDecider pid` (the raw active player, alice) instead of
    -- `Decide.deciderFor pid gs` would record `[Decider.MkDecider S.alice]`
    -- below -- handing alice's own choice back to her, which is the CR 723.1
    -- violation this test exists to catch -- and none of the other six cases in
    -- this group sets activeControl, so none of them would notice.
    let controlled = (S.oneDefendingPlayer S.threePlayerGame) {GameState.activeControl = Just (Decider.MkDecider S.carol)}
        (_, deciders) =
          State.runState
            (fmap snd (Engine.runGame (choosesDefenderRecordingDecider S.bob) controlled (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))))
            []
    Spec.assertEqWith s "carol, alice's controller, is who was asked" deciders [Decider.MkDecider S.carol]
  Spec.it s "CR 506.2 two players: the nonactive player defends and nobody is asked" $ do
    -- The elision, asserted explicitly rather than inferred from the suite
    -- staying green. CR 507.1's condition is a MULTIPLAYER game; CR 506.2's
    -- second sentence settles a two-player game with nothing to ask.
    -- Discriminating twice over: an implementation that prompted anyway would
    -- put alice in `asked`, and one that skipped the prompt AND the write
    -- would leave Nothing, which Task 4 turns into "no attack is possible".
    let (after, asked) =
          State.runState
            (fmap snd (Engine.runGame (choosesDefender S.alice) (Setup.emptyGame S.bothPlayers) (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))))
            []
    Spec.assertEqWith s "bob defends" (Combat.Type.defenders (GameState.combat after)) [S.bob]
    Spec.assertEqWith s "nobody was asked" asked []
  Spec.it s "CR 507.1 a multiplayer game down to one opponent is not asked either" $ do
    -- The case #169 is actually about: CR 703.4h still applies (the game BEGAN
    -- with three players, CR 800.1), and the choice has one candidate.
    -- Discriminating against an elision keyed on the SEAT COUNT rather than on
    -- the candidate count -- that version would prompt here.
    let gone = S.departs Departure.Type.Conceded S.carol S.threePlayerGame
        (after, asked) =
          State.runState
            (fmap snd (Engine.runGame (choosesDefender S.carol) gone (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))))
            []
    Spec.assertEqWith s "bob, the only one left" (Combat.Type.defenders (GameState.combat after)) [S.bob]
    Spec.assertEqWith s "nobody was asked" asked []
  Spec.it s "CR 507.1 with no opponents left the action does not happen at all" $ do
    -- Not reachable in a running game (CR 104.2a ends it), but the branch has
    -- to be total and NonEmpty is why. Discriminating against an
    -- implementation that built the prompt from an empty list.
    let alone = S.departs Departure.Type.Conceded S.carol (S.departs Departure.Type.Conceded S.bob S.threePlayerGame)
        (after, asked) =
          State.runState
            (fmap snd (Engine.runGame (choosesDefender S.bob) alone (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))))
            []
    Spec.assertEqWith s "nobody defends" (Combat.Type.defenders (GameState.combat after)) []
    Spec.assertEqWith s "nobody was asked" asked []
  Spec.it s "CR 800.4h #181 a turn whose active player has left chooses no defending player, diverging from the next-seat reassignment" $ do
    -- CR 800.4j: the turn continues without an active player, so the action
    -- the rules assign to the active player has no subject. CR 800.4j is a
    -- priority rule and licenses no more than that; CR 800.4h would hand the
    -- choice to the next player in turn order rather than drop it, so what is
    -- asserted here is pawl's unobservable divergence from CR 800.4h (#181),
    -- not a rules-required outcome. THREE seats, so that two opponents survive and the choice would
    -- otherwise be a real prompt -- at two seats the elision would hide the
    -- guard entirely.
    --
    -- This drives Engine.runTurnBasedActions, and the guard exists at BOTH
    -- ends of that call (Engine.hs's hasActive and designateDefenders's own test),
    -- so this case passes with either one alone and isolates neither. The
    -- sibling case below is the one that isolates designateDefenders's; the
    -- engine-side copy is redundant on this path and has nothing to isolate.
    let gone = S.departs Departure.Type.Conceded S.alice S.threePlayerGame
        (after, asked) =
          State.runState
            (fmap snd (Engine.runGame (choosesDefender S.carol) gone (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))))
            []
    Spec.assertEqWith s "no defending player" (Combat.Type.defenders (GameState.combat after)) []
    Spec.assertEqWith s "and nobody was asked" asked []
  Spec.it s "CR 800.4j designateDefenders called directly still chooses nobody" $ do
    -- The same rule at the other end of the call, reached WITHOUT
    -- Engine.runTurnBasedActions so that only designateDefenders's own membership
    -- test can be responsible. Discriminating exactly that line: with it gone,
    -- alice -- who has left the game -- is asked, and carol becomes the
    -- defending player on a turn CR 800.4j says has no active player to choose
    -- one. Three seats again, so two candidates survive alice's departure and
    -- the single-candidate elision (#169) cannot be what suppresses the ask.
    let gone = S.departs Departure.Type.Conceded S.alice S.threePlayerGame
        (after, asked) =
          State.runState
            (fmap snd (Engine.runGame (choosesDefender S.carol) gone Combat.designateDefenders))
            []
    Spec.assertEqWith s "no defending player" (Combat.Type.defenders (GameState.combat after)) []
    Spec.assertEqWith s "and nobody was asked" asked []
  Spec.it s "CR 507.1 an answer that is not one of the candidates falls back to the first" $ do
    -- A broken interpreter, not a game state: it names the ACTIVE player.
    -- Discriminating against `defender = Just answer` unchecked, which would
    -- let alice attack herself and, once Task 4 lands, deal combat damage to
    -- the attacking player.
    let (after, _) =
          State.runState
            (fmap snd (Engine.runGame (choosesDefender S.alice) (S.oneDefendingPlayer S.threePlayerGame) (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))))
            []
    Spec.assertEqWith s "the first candidate, never the active player" (Combat.Type.defenders (GameState.combat after)) [S.bob]
  Spec.it s "CR 506.2a the candidates are every other player still in the game" $
    -- Three seats, because two cannot tell "the chosen opponent" from "the
    -- only opponent". Discriminating: an implementation that forgot to drop
    -- the active player would answer [alice, bob, carol].
    Spec.assertEqWith s "bob and carol" (Combat.attackableOpponents S.threePlayerGame) [S.bob, S.carol]
  Spec.it s "CR 102.1 a player who has left the game is not a candidate" $ do
    -- CR 102.1: "A player is one of the people in the game." Four seats so
    -- that TWO candidates survive one departure -- with three seats the
    -- surviving list is a singleton and cannot distinguish "filtered" from
    -- "truncated to one".
    let gone = S.departs Departure.Type.Conceded S.bob S.fourPlayerGame
    Spec.assertEqWith s "bob is dropped, carol and dave remain" (Combat.attackableOpponents gone) [S.carol, S.dave]
    Spec.assertEqWith s "and before he left there were three" (Combat.attackableOpponents S.fourPlayerGame) [S.bob, S.carol, S.dave]
  Spec.it s "CR 101.4 the candidates come back in APNAP order, not seating or player-id order" $ do
    -- Seated carol-alice-bob with alice attacking, so all three readings
    -- disagree: APNAP gives [bob, carol], the raw seating roster gives
    -- [carol, bob], and the players map gives [bob, carol] by id rather than
    -- by seat. Every other fixture in the suite is seated ascending, so this
    -- is the only place they come apart.
    --
    -- APNAP and not the roster, because designateDefenders hands this list
    -- straight to Combat.defenders under CR 802.2 and CR 802.4 and CR 802.5
    -- both read it in that order. Discriminating: the roster reading answers
    -- carol first, and the players-map reading answers bob first for the wrong
    -- reason -- which the case below separates by leaving carol out.
    let rotated = (Setup.emptyGame (S.carol NonEmpty.:| [S.alice, S.bob])) {GameState.activePlayer = S.alice}
    Spec.assertEqWith s "bob, the seat after alice, comes first" (Combat.attackableOpponents rotated) [S.bob, S.carol]
    let rotatedFour = (Setup.emptyGame (S.carol NonEmpty.:| [S.dave, S.alice, S.bob])) {GameState.activePlayer = S.alice}
    Spec.assertEqWith s "and the wrap-around follows the seating, not the ids" (Combat.attackableOpponents rotatedFour) [S.bob, S.carol, S.dave]
  Spec.it s "CR 703.4h no defending player has been chosen before the beginning of combat step" $
    -- Discriminating: a field defaulted to Just <somebody> would let a board
    -- that has never run the turn-based action declare attackers.
    Spec.assertEqWith s "empty combat names nobody" (Combat.Type.defenders (GameState.combat S.threePlayerGame)) []
  Spec.it s "CR 506.2 the designation does not outlive the combat phase" $ do
    -- CR 506.2's sentences are all scoped "During the combat phase", and
    -- CR 703.4h makes the choice per beginning-of-combat step, so a second
    -- combat phase in one turn chooses again. Discriminating: a clearCombat
    -- that reset only attackers and blockers would leave Just carol here, and
    -- the next combat phase would inherit a stale defender.
    let busy = S.threePlayerGame {GameState.combat = (GameState.combat S.threePlayerGame) {Combat.Type.defenders = [S.carol]}}
    Spec.assertEqWith s "cleared at end of combat" (Combat.Type.defenders (GameState.combat (Combat.clearCombat busy))) []
  Spec.it s "CR 508.1 every attacker attacks the CHOSEN defending player" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (board, mine, _, _) = S.threePlayerCombat [piker, piker] [piker] [piker]
        -- carol, deliberately not the first candidate.
        ready =
          board
            { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
              GameState.combat = (GameState.combat board) {Combat.Type.defenders = [S.carol]}
            }
        after = S.runPure S.aggressiveAnswer ready (Combat.declareAttackers S.manaPerformer S.alice)
    Spec.assertEqWith s "both of alice's creatures attack" (length mine) 2
    -- Discriminating: under the head-of-list behaviour this phase replaces,
    -- every value here is OfPlayer bob, because bob is the first candidate.
    Spec.assertEqWith
      s
      "and both attack carol"
      (Combat.Type.attackers (GameState.combat after))
      (Map.fromList (fmap (\oid -> (oid, AttackTarget.OfPlayer S.carol)) mine))
  Spec.it s "CR 508.1 with no defending player chosen, nothing attacks" $ do
    -- Discriminating against a declareAttackers that fell back to computing a
    -- defender when the field is Nothing -- which is the head-of-list
    -- behaviour wearing a different hat. The answerer is maximal (it attacks
    -- with everything offered), so an empty attacker map can only come from
    -- the prompt never being issued.
    piker <- S.printingOf s registry "Goblin Piker"
    let (board, mine, _, _) = S.threePlayerCombat [piker] [piker] [piker]
        ready = board {GameState.phase = Phase.Combat CombatStep.DeclareAttackers}
        after = S.runPure S.aggressiveAnswer ready (Combat.declareAttackers S.manaPerformer S.alice)
    Spec.assertEqWith s "alice really had a legal attacker" (fmap (\oid -> Combat.canAttack S.alice oid ready) mine) [True]
    Spec.assertEqWith s "nobody attacked" (Combat.Type.attackers (GameState.combat after)) Map.empty
    Spec.assertEqWith s "and nothing was tapped" (S.tappedCount S.alice after) 0
  Spec.it s "CR 509.1 only the defending player is asked to declare blockers" $ do
    -- CR 509.1's first sentence names THE defending player, singular. CR 802.4
    -- has several of them declare in APNAP order, which Combat.declareBlockers
    -- does by looping over Defender.defendingPlayers; this board records carol as
    -- the only defender, so that loop has one seat to ask.
    piker <- S.printingOf s registry "Goblin Piker"
    let (board, mine, bobs, carols) = S.threePlayerCombat [piker] [piker] [piker]
        attackMap = case mine of
          oid : _ -> Map.singleton oid (AttackTarget.OfPlayer S.carol)
          [] -> Map.empty
        ready =
          board
            { GameState.phase = Phase.Combat CombatStep.DeclareBlockers,
              GameState.combat =
                (GameState.combat board)
                  { Combat.Type.defenders = [S.carol],
                    Combat.Type.attackers = attackMap
                  }
            }
        ((asked, offeredBlockers), after) = runRecordingBlockers ready
    Spec.assertEqWith s "the fixture gave bob and carol a blocker each" (length bobs, length carols) (1, 1)
    -- Discriminating: the behaviour this phase replaces loops over every
    -- opponent, so `asked` would be [bob, carol].
    Spec.assertEqWith s "only carol was asked" asked [S.carol]
    -- And bob's untapped creature is never offered, per CR 509.1a.
    Spec.assertEqWith s "bob's creature is in no candidate list" (filter (\oid -> elem oid bobs) offeredBlockers) []
    Spec.assertEqWith s "carol's block was recorded" (Map.size (Combat.Type.blockers (GameState.combat after))) 1
  Spec.it s "CR 725.2/802.3 the crown follows whichever defending player was attacked" $ do
    -- CR 725.2's second inherent ability: "Whenever a creature deals combat
    -- damage to the monarch, its controller becomes the monarch." bob is the
    -- monarch. alice attacks with an unblocked 2/1; the two runs differ ONLY
    -- in that creature's announced attack target.
    --
    -- Discriminating: run A is what the deleted head-of-list behaviour did
    -- whatever the answer, so run A alone proves nothing. Run B is
    -- unreachable under it, and the pair is the proof.
    let attacker = S.aliasRef "attacker"
        board =
          S.board
            (S.battlefield S.alice [S.settled "attacker" "Goblin Piker"] NonEmpty.:| [S.playerSetup S.bob, S.playerSetup S.carol])
            S.alice
            S.beginningOfCombat
        script who =
          S.turn
            1
            [ S.on S.declareAttackers S.alice (S.attack [attacker]),
              S.onSource S.declareAttackers S.alice attacker (S.attackPlayer who)
            ]
    built <- S.buildBoardOrFail s registry board
    let crowned = built {S.builtState = S.withMonarch S.bob (S.builtState built)}
    (_, hitBob) <- S.runScriptOrFail s (script S.bob) crowned S.combatGame
    (_, hitCarol) <- S.runScriptOrFail s (script S.carol) crowned S.combatGame
    -- Run A: attacking the monarch takes the crown.
    Spec.assertEqWith s "bob took 2" (S.lifeOf S.bob hitBob) (Just 18)
    Spec.assertEqWith s "carol was untouched" (S.lifeOf S.carol hitBob) (Just 20)
    Spec.assertEqWith s "alice is the monarch" (GameState.monarch hitBob) (Just S.alice)
    -- Run B: attacking the other opponent does not.
    Spec.assertEqWith s "carol took 2" (S.lifeOf S.carol hitCarol) (Just 18)
    Spec.assertEqWith s "bob was untouched" (S.lifeOf S.bob hitCarol) (Just 20)
    Spec.assertEqWith s "bob keeps the crown" (GameState.monarch hitCarol) (Just S.bob)
    -- And neither run ended the game, so both really played a whole combat.
    Spec.assertEqWith s "no result in run A" (GameState.result hitBob) Nothing
    Spec.assertEqWith s "no result in run B" (GameState.result hitCarol) Nothing

tapStateOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)

-- Re-sicken alice's creatures, as though they had just resolved this turn.
justArrived :: GameState.GameState -> GameState.GameState
justArrived gs =
  let sicken o = if Object.owner o == S.alice then o {Object.sickness = Sickness.Sick} else o
   in gs {GameState.objects = fmap sicken (GameState.objects gs)}

hasteSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
hasteSpec s registry = Spec.describe s "Haste" $ do
  Spec.it s "CR 702.10b a creature with haste attacks the turn it arrives" $ do
    goblinChariot <- S.printingOf s registry "Goblin Chariot"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [goblinChariot] [piker]
        after = snd (Engine.runGamePure S.aggressiveAnswer (justArrived gs) (Combat.declareAttackers S.manaPerformer S.alice))
    Spec.assertEqWith s "attacks" (length (declaredAttackers after)) 1
  Spec.it s "CR 302.6 the same creature without haste cannot" $ do
    -- The control. Goblin Chariot and Goblin Piker are both 2/2-ish Goblin
    -- Warriors; the ONLY difference the engine can see is the keyword.
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [piker] [piker]
        after = snd (Engine.runGamePure S.aggressiveAnswer (justArrived gs) (Combat.declareAttackers S.manaPerformer S.alice))
    Spec.assertEqWith s "cannot attack" (declaredAttackers after) []
  Spec.it s "CR 702.10b haste is not needed once the creature has settled" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker] [piker]
        after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.manaPerformer S.alice))
    Spec.assertEqWith s "attacks" (declaredAttackers after) mine
  -- The same contrast one layer up: haste GRANTED by a static ability rather
  -- than printed. Concordant Crossroads says "All creatures have haste", so
  -- the very Piker that could not attack in the control case above now can,
  -- and nothing about the Piker itself changed.
  Spec.it s "CR 702.10b Concordant Crossroads grants haste, so a summoning-sick Piker attacks" $ do
    crossroads <- S.printingOf s registry "Concordant Crossroads"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker] [piker]
        (_, enchanted) = S.addPermanent crossroads S.alice (justArrived gs)
        after = snd (Engine.runGamePure S.aggressiveAnswer enchanted (Combat.declareAttackers S.manaPerformer S.alice))
    Spec.assertEqWith s "attacks anyway" (declaredAttackers after) mine
  -- CR 113.6b, the zone half of the same contrast. Anger's ability states where
  -- it functions -- "as long as this card is in your graveyard and you control a
  -- Mountain, creatures you control have haste" -- so the three boards below
  -- differ in exactly one thing each: the first pair in which zone Anger's card
  -- sits, the second pair in whether alice controls a Mountain. Anger is never a
  -- creature alice can attack with in the graveyard boards, so the Piker is the
  -- only attacker either reading could produce.
  Spec.it s "CR 113.6b Anger in the graveyard grants haste, so a summoning-sick Piker attacks" $ do
    anger <- S.printingOf s registry "Anger"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker] [piker]
        (_, withMountain) = S.addPermanent mountain S.alice (justArrived gs)
        (_, buried) = S.addGraveyardCard anger S.alice withMountain
        after = snd (Engine.runGamePure S.aggressiveAnswer buried (Combat.declareAttackers S.manaPerformer S.alice))
    Spec.assertEqWith s "attacks anyway" (declaredAttackers after) mine
  -- CR 113.6b's "only": on the battlefield the very same printed ability grants
  -- nothing, so the Piker is stuck. Anger itself has printed haste and attacks
  -- from either reading, which is why the assertion names the Piker rather than
  -- counting the declaration.
  Spec.it s "CR 113.6b the same ability on the battlefield grants nothing" $ do
    anger <- S.printingOf s registry "Anger"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker] [piker]
        (_, withMountain) = S.addPermanent mountain S.alice (justArrived gs)
        (_, onBattlefield) = S.addPermanent anger S.alice withMountain
        after = snd (Engine.runGamePure S.aggressiveAnswer onBattlefield (Combat.declareAttackers S.manaPerformer S.alice))
    case mine of
      [pikerId] -> Spec.assertBool s (notElem pikerId (declaredAttackers after)) "the Piker still cannot attack"
      _ -> Spec.assertFailure s "fixture should have one creature"
  -- CR 604.2's clause is still asked, and asked of a source in a GRAVEYARD: drop
  -- the Mountain and the graveyard board above stops granting.
  Spec.it s "CR 604.2 without a Mountain the graveyard ability grants nothing" $ do
    anger <- S.printingOf s registry "Anger"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [piker] [piker]
        (_, buried) = S.addGraveyardCard anger S.alice (justArrived gs)
        after = snd (Engine.runGamePure S.aggressiveAnswer buried (Combat.declareAttackers S.manaPerformer S.alice))
    Spec.assertEqWith s "cannot attack" (declaredAttackers after) []
  Spec.it s "CR 702.10b a hasty creature and a sick one, in the same declaration" $ do
    -- Both sick; only the Chariot may attack. A blanket "sickness ignored"
    -- bug would let both through.
    goblinChariot <- S.printingOf s registry "Goblin Chariot"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [goblinChariot, piker] [piker]
        after = snd (Engine.runGamePure S.aggressiveAnswer (justArrived gs) (Combat.declareAttackers S.manaPerformer S.alice))
    case mine of
      [chariot, _] -> Spec.assertEqWith s "only the chariot" (declaredAttackers after) [chariot]
      _ -> Spec.assertFailure s "fixture should have two creatures"

controlChangeSicknessSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
controlChangeSicknessSpec s registry = Spec.describe s "ControlChangeSickness" $ do
  -- A live steal, with nothing forced: bob's Piker settles under bob at his
  -- untap step, then alice's Control Magic takes it. CR 302.6 asks whether
  -- ALICE has controlled it continuously since HER most recent turn began,
  -- and she has not -- the settle it carries is bob's, not hers.
  Spec.it s "CR 302.6 a creature that just changed control is summoning sick (no haste)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    controlMagic <- S.printingOf s registry "Control Magic"
    let base = Setup.emptyGame S.bothPlayers
        (creature, withCreature) = S.addPermanent piker S.bob base
        settled = S.runPure S.identityAnswer withCreature (Engine.settleAll S.bob)
        (aura, withAura) = S.addPermanent controlMagic S.alice settled
        attached = S.attach aura creature withAura
    Spec.assertEqWith s "alice controls it" (Projection.controllerOf creature attached) (Just S.alice)
    Spec.assertBool s (not (Combat.canAttack S.alice creature attached)) "but it is summoning sick, so it cannot attack this turn"
  -- CR 302.6 asks for control held CONTINUOUSLY. bob's Control Magic takes
  -- alice's settled Piker; alice later removes the Aura and gets the Piker
  -- back (CR 604.2). Control is hers again and was hers when her turn began,
  -- but not for the whole span between, so she still may not attack with it.
  --
  -- Reachable with the pool as it stands: Control Magic is a sorcery-speed
  -- Aura, so bob can only cast it on his own turn, and alice can only answer
  -- it on hers -- after her untap step has already passed.
  Spec.it s "CR 302.6 control that leaves and returns is not continuous" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    controlMagic <- S.printingOf s registry "Control Magic"
    let base = Setup.emptyGame S.bothPlayers
        (creature, withCreature) = S.addPermanent piker S.alice base
        settled = S.runPure S.identityAnswer withCreature (Engine.settleAll S.alice)
        (aura, withAura) = S.addPermanent controlMagic S.bob settled
        -- The steal is observed the next time the board settles -- the CR
        -- 117.5 sweep, which runs wherever the board can change.
        stolen = S.runPure S.identityAnswer (S.attach aura creature withAura) Engine.settleForPriority
        returned = S.runPure S.identityAnswer stolen (Event.changeZone aura Zone.Graveyard)
    Spec.assertEqWith s "bob held it" (Projection.controllerOf creature stolen) (Just S.bob)
    Spec.assertEqWith s "alice has it back" (Projection.controllerOf creature returned) (Just S.alice)
    Spec.assertBool s (not (Combat.canAttack S.alice creature returned)) "but not continuously, so it cannot attack"

-- CR 614.1c's as-enters choice of a player, stamped onto a permanent a fixture
-- placed rather than cast (S.addPermanent runs no entry loop).
chosePlayer :: PlayerId.PlayerId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
chosePlayer pid oid gs = gs {GameState.objects = Map.adjust (\o -> o {Object.chosenPlayer = Just pid}) oid (GameState.objects gs)}

-- Declare attackers with everything, then hand back the state and the ids.
attacking :: [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
attacking mine theirs =
  let (gs, ours, yours) = S.combatBoardOf mine theirs
      after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.manaPerformer S.alice))
   in (after, ours, yours)

-- CR 702.36: grant fear to `oid` with a stored continuous effect. No card in the
-- pool has PRINTED fear (Aphotic Wisps grants it at instant speed, which combat
-- fixtures cannot reach mid-step), so this is the M2c granted-keyword posture.
withFear :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
withFear oid gs =
  let (ts, gs1) = Game.freshTimestamp gs
      eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = oid,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = Expiry.AtCleanup,
            ContinuousEffect.modification = Modification.GainKeyword Keyword.Fear,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}

-- CR 702.19: grant trample to `oid` with a stored continuous effect, withFear's
-- posture. Tapestry Warden does not print trample, and the pool has no printed
-- trampler that also assigns with toughness.
withTrample :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
withTrample oid gs =
  let (ts, gs1) = Game.freshTimestamp gs
      eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = oid,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = Expiry.AtCleanup,
            ContinuousEffect.modification = Modification.GainKeyword Keyword.Trample,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}

-- CR 702.19b: assigns each blocker exactly the threshold the engine offered and
-- every leftover point to the defending player. Reads the amount off the prompt
-- rather than computing it, so a wrong substitution shows up as a wrong life
-- total rather than a rejected assignment.
trampleThresholdAnswer :: Prompt.Prompt r -> r
trampleThresholdAnswer p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    let blockers = Map.filterWithKey (\r _ -> S.isCreatureRecipient r) thresholds
        spent = sum (Map.elems blockers)
        leftover = if n >= spent then n - spent else 0
     in case filter (not . S.isCreatureRecipient) (Map.keys thresholds) of
          d : _ -> Map.insert d leftover blockers
          [] -> blockers
  _ -> S.aggressiveAnswer p

-- A layer-7c effect that turns Goblin Piker's 2/1 into a 2/3, so Tapestry
-- Warden's CR 613.11 rules effect must see the finished characteristics.
withToughnessBoost :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
withToughnessBoost oid gs =
  let (ts, gs1) = Game.freshTimestamp gs
      eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = oid,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = Expiry.AtCleanup,
            ContinuousEffect.modification = Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 0) (Quantity.Literal 2)),
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}

-- CR 702.14c's "the defending player controls at least one land": bob's lands,
-- put onto an already-attacking board.
withLands :: [Printing.Printing] -> GameState.GameState -> GameState.GameState
withLands = withPermanents S.bob

-- Any printings at all onto `who`'s battlefield, on a board that already exists.
-- S.addPermanent is any-printing rather than creature-only, which is how the CR
-- 509.1a Mountain case below reaches a land.
withPermanents :: PlayerId.PlayerId -> [Printing.Printing] -> GameState.GameState -> GameState.GameState
withPermanents who ps gs = List.foldl' (\g p -> snd (S.addPermanent p who g)) gs ps

-- Put `printing` onto bob's battlefield already attached to `host` -- CR 301.5a
-- for an Equipment, CR 303.4b for an Aura. A STATE fixture, as S.attach's own
-- comment says: no equip ability is activated, so the timing of CR 702.6a plays no
-- part in what the CR 509.1a arity below reads.
withAttachment :: Printing.Printing -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
withAttachment printing host gs =
  let (oid, gs1) = S.addPermanent printing S.bob gs
   in S.attach oid host gs1

evasionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
evasionSpec s registry = Spec.describe s "Evasion" $ do
  Spec.it s "CR 702.9b a declaration in which a ground creature blocks a flier is illegal" $ do
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [birdMaiden] [piker]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs)) "illegal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.17b a reach creature may block a flier" $ do
    -- THE FALSIFIER. Fails against any implementation that asks "does the
    -- blocker have flying?"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    nimbleBirdsticker <- S.printingOf s registry "Nimble Birdsticker"
    let (gs, mine, theirs) = attacking [birdMaiden] [nimbleBirdsticker]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs) "legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.9b a flier may block a ground creature" $ do
    -- The asymmetry: 702.9b's second sentence. Fails if flying is implemented
    -- as a symmetric predicate.
    piker <- S.printingOf s registry "Goblin Piker"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    let (gs, mine, theirs) = attacking [piker] [birdMaiden]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs) "legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.9b a flier may block a flier" $ do
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    let (gs, mine, theirs) = attacking [birdMaiden] [birdMaiden]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs) "legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  -- CR 613.1f's layer-6 removal on the case directly above: Sky Tether
  -- ("enchanted creature has defender and loses flying") takes the blocker's
  -- printed flying away, and CR 702.9b then refuses it the flier it could have
  -- blocked a moment ago. The pool's first single-keyword removal -- Humility
  -- takes every ability and a Licid takes back the one it named, and neither can
  -- write this clause.
  --
  -- TWO Bird Maidens on bob's side, differing only in the Aura, so a blanket
  -- "nobody may block a flier" bug cannot pass: the one beside the tethered
  -- creature still blocks.
  Spec.it s "CR 613.1f a flier that loses flying to Sky Tether may no longer block a flier" $ do
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    skyTether <- S.printingOf s registry "Sky Tether"
    let (gs, mine, theirs) = attacking [birdMaiden] [birdMaiden, birdMaiden]
    case (mine, theirs) of
      (a : _, [tethered, other]) -> do
        let board = withAttachment skyTether tethered gs
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton tethered (Set.singleton a)) board)) "the tethered creature may not block the flier"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton other (Set.singleton a)) board) "the one beside it still may"
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying tethered board)) "and it is flying the Aura took, not the declaration that broke"
        Spec.assertBool s (Projection.hasKeyword Keyword.Flying other board) "the other keeps its printed flying"
      _ -> Spec.assertFailure s "fixture should have an attacker and two blockers"
  -- The same Aura's other half, so the card is proved whole rather than in the
  -- clause this unit needed: CR 702.3b's defender stops the host attacking. On
  -- ALICE's creatures, since CR 508.1a asks the active player, and again as a
  -- pair on one board.
  Spec.it s "CR 702.3b Sky Tether's other half gives the host defender" $ do
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    skyTether <- S.printingOf s registry "Sky Tether"
    let (gs, mine, _) = S.combatBoardOf [birdMaiden, birdMaiden] []
    case mine of
      [tethered, other] -> do
        let (aura, withAura) = S.addPermanent skyTether S.alice gs
            board = S.attach aura tethered withAura
        Spec.assertBool s (not (Combat.canAttack S.alice tethered board)) "the tethered creature cannot attack"
        Spec.assertBool s (Combat.canAttack S.alice other board) "the one beside it can"
        Spec.assertBool s (Projection.hasKeyword Keyword.Defender tethered board) "defender is what stops it"
      _ -> Spec.assertFailure s "fixture should have two creatures"
  -- CR 509.1b's second paragraph on a CARD rather than on the rulebook: Questing
  -- Beast's "can't be blocked by creatures with power 2 or less" is the pool's
  -- first card-authored CombatRestriction.CantBeBlockedBy, where CR 701.54c's
  -- Ring-bearer clause (Pawl.Engine.Ring) is minted by the rules.
  --
  -- A PAIR on the same attacker, differing only in the blocker's power, so
  -- neither case can pass because the declaration was illegal for some other
  -- reason: the 2/1 Goblin Piker is barred and the 3/3 War Mammoth is not.
  Spec.it s "CR 509.1b Questing Beast can't be blocked by a creature with power 2 or less" $ do
    questingBeast <- S.printingOf s registry "Questing Beast"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [questingBeast] [piker]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs)) "illegal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 509.1b a creature with power 3 may block Questing Beast" $ do
    questingBeast <- S.printingOf s registry "Questing Beast"
    mammoth <- S.printingOf s registry "War Mammoth"
    let (gs, mine, theirs) = attacking [questingBeast] [mammoth]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs) "legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  -- CR 702.16f: "Attacking creatures with protection can't be blocked by
  -- creatures that have the stated quality." Rule 702.16 stated as CR 509.1b's
  -- pairwise restriction, and the one clause of protection that is already a
  -- printed shape -- Questing Beast's row above with the quality in the blocker
  -- position, minted from the keyword rather than authored on a face.
  --
  -- A PAIR ON ONE BOARD, differing only in the blocker's COLOUR: Cabal Evangel is
  -- a black 2/2 with no abilities at all and Goblin Piker a red 2/1 with none, so
  -- neither declaration can be refused for a reason the rule does not name. An
  -- implementation that stopped every blocker fails the second leg; one that
  -- ignored the quality fails the first.
  --
  -- The Apostle is WHITE, which is what makes the first leg discriminating: an
  -- implementation matching the quality against the ATTACKER rather than the
  -- blocker finds no black on it and admits the Evangel.
  Spec.it s "CR 702.16f Apostle of Purifying Light can't be blocked by a black creature, and can by a red one" $ do
    apostle <- S.printingOf s registry "Apostle of Purifying Light"
    evangel <- S.printingOf s registry "Cabal Evangel"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [apostle] [evangel, piker]
    case (mine, theirs) of
      (a : _, [black, red]) -> do
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton black (Set.singleton a)) gs)) "the black Cabal Evangel may not block it (CR 702.16f)"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton red (Set.singleton a)) gs) "the red Goblin Piker beside it may"
      _ -> Spec.assertFailure s "fixture should have an attacker and two blockers"
  -- CR 702.16k: "Such a permanent ... can't be blocked by creatures that player
  -- controls." True-Name Nemesis, whose quality is Filter.OfChosenPlayer -- the
  -- one quality that asks who an object BELONGS TO rather than what it looks
  -- like, answered off Filter.Context.carrierChosenPlayer, which
  -- Pawl.Engine.CombatRestriction.cantBeBlockedBy fills off the attacker.
  --
  -- A PAIR ON ONE BOARD, differing only in WHOM the Nemesis chose. The blocker is
  -- the same red Goblin Piker bob controls in both rows, so an implementation
  -- that stopped every blocker fails the second leg, and one that read "an
  -- opponent" rather than the chosen seat fails it too: alice is the Nemesis's
  -- own controller, and rule 702.16k names what the CHOSEN player controls.
  --
  -- The choice is stamped rather than cast for, which the case below asserts: the
  -- entry road that writes it is proved by Pawl.DamageSpec's True-Name Nemesis
  -- group and Pawl.ReplacementSpec's Stuffy Doll group, and a combat fixture
  -- cannot reach a cast.
  Spec.it s "CR 702.16k True-Name Nemesis can't be blocked by the chosen player's creature, and can by another's" $ do
    nemesis <- S.printingOf s registry "True-Name Nemesis"
    piker <- S.printingOf s registry "Goblin Piker"
    let (base, mine, theirs) = S.combatBoardOf [nemesis] [piker]
        declared who attacker = snd (Engine.runGamePure S.aggressiveAnswer (chosePlayer who attacker base) (Combat.declareAttackers S.manaPerformer S.alice))
    case (mine, theirs) of
      (a : _, blocker : _) -> do
        let chosenBob = declared S.bob a
            chosenAlice = declared S.alice a
            blocks = Map.singleton blocker (Set.singleton a)
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob blocks chosenBob)) "CR 702.16k bob was chosen, so bob's Goblin Piker may not block it"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob blocks chosenAlice) "and with alice chosen instead the same Piker may"
        Spec.assertEqWith s "CR 614.1c and the two boards really differ in the seat the Nemesis chose" (Game.lookupObject a chosenBob >>= Object.chosenPlayer, Game.lookupObject a chosenAlice >>= Object.chosenPlayer) (Just S.bob, Just S.alice)
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 509.1a a ground creature is still a legal blocker while a flier attacks" $ do
    -- 509.1a is about the blocker ALONE: it can block SOMETHING. This test
    -- fails if evasion is wrongly implemented as a filter on the candidates.
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, theirs) = attacking [birdMaiden] [piker]
    Spec.assertEqWith s "still offered" (Combat.legalBlockers S.bob gs) theirs
  Spec.it s "CR 509.1b an illegal declaration is rejected WHOLE, not repaired" $ do
    -- aggressiveAnswer blocks the first attacker with EVERYTHING, so bob
    -- declares the reach creature (legal) AND the Piker (illegal) on the
    -- flier. Neither may block. A per-pair filter would drop the Piker and
    -- let the Birdsticker's block stand -- which is what M1b does today, and
    -- is unsound: under menace, dropping one blocker from a pair manufactures
    -- an illegal single block.
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    nimbleBirdsticker <- S.printingOf s registry "Nimble Birdsticker"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [birdMaiden] [nimbleBirdsticker, piker]
        steps = do
          Combat.declareAttackers S.manaPerformer S.alice
          Combat.declareBlockers S.manaPerformer
        after = snd (Engine.runGamePure S.aggressiveAnswer gs steps)
    case Map.keys (Combat.Type.attackers (GameState.combat after)) of
      [] -> Spec.assertFailure s "fixture should have an attacker"
      a : _ -> Spec.assertEqWith s "nobody blocks" (Combat.blockersOf a after) Set.empty
  Spec.it s "CR 509.1b a wholly legal declaration is accepted" $ do
    -- The control for the test above: with only the reach creature, the same
    -- interpreter produces a legal declaration and the block stands.
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    nimbleBirdsticker <- S.printingOf s registry "Nimble Birdsticker"
    let (gs, _, theirs) = S.combatBoardOf [birdMaiden] [nimbleBirdsticker]
        steps = do
          Combat.declareAttackers S.manaPerformer S.alice
          Combat.declareBlockers S.manaPerformer
        after = snd (Engine.runGamePure S.aggressiveAnswer gs steps)
    case Map.keys (Combat.Type.attackers (GameState.combat after)) of
      [] -> Spec.assertFailure s "fixture should have an attacker"
      a : _ -> Spec.assertEqWith s "the reach creature blocks" (Combat.blockersOf a after) (Set.fromList theirs)
  Spec.it s "CR 509.1a a Mountain is not a legal blocker, flier or no flier" $ do
    -- The classification, from the other side: `canBlock` asks
    -- is-it-a-creature, never which card it is. M1b (tests cards) "a land may not
    -- attack" but never that a land may not BLOCK, so this closes a real gap
    -- rather than restating one.
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    mountain <- S.printingOf s registry "Mountain"
    let (gs, mine, _) = attacking [birdMaiden] []
        withLand = snd (S.addPermanent mountain S.bob gs)
    case mine of
      [] -> Spec.assertFailure s "fixture should have an attacker"
      _ : _ -> Spec.assertEqWith s "no legal blockers" (Combat.legalBlockers S.bob withLand) []
  Spec.it s "CR 702.9b a flier connects past an untapped ground creature, in a real combat" $ do
    -- The integration case, and it is precise rather than vacuous. WITH
    -- flying: nothing may block, bob takes 1, and both creatures live.
    -- WITHOUT flying: the Piker blocks, bob takes 0, and the two TRADE (Bird
    -- Maiden is 1/2, Piker is 2/1). All three assertions distinguish them.
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [birdMaiden] [piker]
        after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
    Spec.assertEqWith s "bob took 1" (S.lifeOf S.bob after) (Just 19)
    Spec.assertEqWith s "the flier lives" (S.creaturesInPlay S.alice after) 1
    Spec.assertEqWith s "the would-be blocker lives" (S.creaturesInPlay S.bob after) 1
  Spec.it s "CR 702.36b a red creature may not block a creature with fear" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs0, mine, theirs) = attacking [piker] [piker]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) (withFear a gs0))) "illegal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.36b a black creature may block a creature with fear" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let (gs0, mine, theirs) = attacking [piker] [typhoidRats]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) (withFear a gs0)) "legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.36b an ARTIFACT creature may block a creature with fear" $ do
    -- THE FALSIFIER for reading 702.36b as a colour test alone: Darksteel Myr
    -- is a colourless artifact creature and blocks legally.
    piker <- S.printingOf s registry "Goblin Piker"
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let (gs0, mine, theirs) = attacking [piker] [darksteelMyr]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) (withFear a gs0)) "legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.36b a devoid creature with a black mana cost may not block a creature with fear" $ do
    -- THE FALSIFIER for reading the blocker's PRINTED colour: Slaughter
    -- Drone's mana cost is {1}{B}, but CR 702.114a makes it colourless (not
    -- black), so it is not a legal blocker of a fear attacker. Fails against
    -- any implementation that reads the blocker's printed colour rather than
    -- its projected colour.
    piker <- S.printingOf s registry "Goblin Piker"
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let (gs0, mine, theirs) = attacking [piker] [slaughterDrone]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) (withFear a gs0))) "illegal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.36b fear restricts being blocked, never blocking" $ do
    -- The 702.9b asymmetry, restated for fear: a fear creature blocking a
    -- plain attacker is legal.
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs0, mine, theirs) = attacking [piker] [piker]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) (withFear b gs0)) "legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"

  -- CR 702.13's five cases. Unlike fear's above, these run off a PRINTED
  -- keyword: Highborn Ghoul ({B}{B} Creature -- Zombie 2/1, intimidate and
  -- nothing else) is the pool's first card to PRINT a colour-based evasion
  -- ability, so there is no granted-keyword fixture between the card and the
  -- gate, and no other text on the card for a case to pass on.
  Spec.it s "CR 702.13b a green creature may not block a creature with intimidate" $ do
    highbornGhoul <- S.printingOf s registry "Highborn Ghoul"
    prowlingSerpopard <- S.printingOf s registry "Prowling Serpopard"
    let (gs, mine, theirs) = attacking [highbornGhoul] [prowlingSerpopard]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs)) "illegal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.13b a black creature may block a creature with intimidate" $ do
    -- The sibling of the case above, identical but for the blocker's colour:
    -- Typhoid Rats is black, so it shares a colour with the black Ghoul. This
    -- is the clause fear cannot be told apart from, which is why the
    -- colourless-attacker case below has to sit beside it.
    highbornGhoul <- S.printingOf s registry "Highborn Ghoul"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let (gs, mine, theirs) = attacking [highbornGhoul] [typhoidRats]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs) "legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.13b an ARTIFACT creature may block a creature with intimidate" $ do
    -- THE FALSIFIER for reading 702.13b as a colour test alone: Darksteel Myr
    -- is a colourless artifact creature, so it shares no colour with the Ghoul
    -- and blocks on the artifact clause instead.
    highbornGhoul <- S.printingOf s registry "Highborn Ghoul"
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let (gs, mine, theirs) = attacking [highbornGhoul] [darksteelMyr]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs) "legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.13b a COLOURLESS creature with intimidate may be blocked only by artifact creatures" $ do
    -- THE FALSIFIER that separates intimidate from fear, and the only assertion
    -- here a hard-coded Color.Black fails. CR 105.3's "effects may also make a
    -- colored object become colorless" is SetColor with no colours, so the
    -- attacker is the printed Ghoul with its colour taken away at CR 613 layer
    -- 5. CR 105.2c: it now has no colour, so it shares one with nobody -- the
    -- black Typhoid Rats that blocked it legally in the case above may not, and
    -- only the artifact creature may.
    highbornGhoul <- S.printingOf s registry "Highborn Ghoul"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let (gs0, mine, theirs) = attacking [highbornGhoul] [typhoidRats, darksteelMyr]
    case (mine, theirs) of
      (a : _, black : artifact : _) ->
        let gs = S.withEffect a (Modification.SetColor Set.empty) gs0
         in do
              Spec.assertBool s (Set.null (Projection.colorsOf a gs)) "the attacker is colourless"
              Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton black (Set.singleton a)) gs)) "the black creature may not block"
              Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton artifact (Set.singleton a)) gs) "the artifact creature may"
      _ -> Spec.assertFailure s "fixture should have an attacker and two blockers"
  Spec.it s "CR 702.13b intimidate restricts being blocked, never blocking" $ do
    -- The 702.9b asymmetry, restated for intimidate: the Ghoul blocking a red
    -- attacker it shares no colour with is legal.
    piker <- S.printingOf s registry "Goblin Piker"
    highbornGhoul <- S.printingOf s registry "Highborn Ghoul"
    let (gs, mine, theirs) = attacking [piker] [highbornGhoul]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs) "legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"

  -- CR 702.28's four cases, off a PRINTED keyword: Soltari Foot Soldier ({W}
  -- Creature -- Soltari Soldier 1/1, shadow and nothing else) has no other text
  -- for a case to pass on. Goblin Piker is the non-shadow creature throughout, so
  -- the only thing that varies between the cases is which side has shadow.
  Spec.it s "CR 702.28b a creature without shadow may not block a creature with shadow" $ do
    footSoldier <- S.printingOf s registry "Soltari Foot Soldier"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [footSoldier] [piker]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs)) "illegal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.28b a creature WITH shadow may not block a creature without shadow" $ do
    -- THE FALSIFIER, and the case no other evasion ability in the pool has:
    -- 702.28b's second sentence restricts BLOCKING, so the board flying's
    -- asymmetry makes legal (see "a flier may block a ground creature" above) is
    -- illegal here. Fails against any implementation that reads shadow off the
    -- attacker alone.
    piker <- S.printingOf s registry "Goblin Piker"
    footSoldier <- S.printingOf s registry "Soltari Foot Soldier"
    let (gs, mine, theirs) = attacking [piker] [footSoldier]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs)) "illegal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.28b a creature with shadow may block a creature with shadow" $ do
    -- Both halves of 702.28b are satisfied at once, which is what keeps the two
    -- cases above from passing on a gate that simply forbids every block.
    footSoldier <- S.printingOf s registry "Soltari Foot Soldier"
    let (gs, mine, theirs) = attacking [footSoldier] [footSoldier]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs) "legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.28b a shadow creature connects past an untapped ground creature, in a real combat" $ do
    -- The gameplay-level case, flying's above with shadow in place of flying and
    -- precise for its reasons: bob takes 1 rather than 0, and the 1/1 Foot
    -- Soldier survives a 2/1 Piker that never got to block it.
    footSoldier <- S.printingOf s registry "Soltari Foot Soldier"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [footSoldier] [piker]
        after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
    Spec.assertEqWith s "bob took 1" (S.lifeOf S.bob after) (Just 19)
    Spec.assertEqWith s "the shadow creature lives" (S.creaturesInPlay S.alice after) 1
    Spec.assertEqWith s "the would-be blocker lives" (S.creaturesInPlay S.bob after) 1

  -- CR 702.31's three legality cases and its gameplay one, off a PRINTED keyword:
  -- Shu Cavalry ({2}{W} Creature -- Human Soldier 2/2, horsemanship and nothing
  -- else) has no other text for a case to pass on. Goblin Piker is the
  -- non-horsemanship creature throughout.
  Spec.it s "CR 702.31b a creature without horsemanship may not block a creature with horsemanship" $ do
    shuCavalry <- S.printingOf s registry "Shu Cavalry"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [shuCavalry] [piker]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs)) "illegal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.31b a creature WITH horsemanship may block a creature without horsemanship" $ do
    -- THE FALSIFIER, and what separates horsemanship from shadow: 702.31b's second
    -- sentence says a horseman blocks with or without, so the board shadow's
    -- equality makes illegal (see "a creature WITH shadow may not block a creature
    -- without shadow" above) is legal here. Fails against any implementation that
    -- reads the keyword off both creatures.
    piker <- S.printingOf s registry "Goblin Piker"
    shuCavalry <- S.printingOf s registry "Shu Cavalry"
    let (gs, mine, theirs) = attacking [piker] [shuCavalry]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs) "legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.31b a creature with horsemanship may block a creature with horsemanship" $ do
    -- The exception 702.31b states, which keeps the illegal case above from
    -- passing on a gate that simply forbids every block of a horseman.
    shuCavalry <- S.printingOf s registry "Shu Cavalry"
    let (gs, mine, theirs) = attacking [shuCavalry] [shuCavalry]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs) "legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.31b a horseman connects past an untapped ground creature, in a real combat" $ do
    -- The gameplay-level case: bob takes 2, and the 2/2 Cavalry survives a 2/1
    -- Piker that never got to block it.
    shuCavalry <- S.printingOf s registry "Shu Cavalry"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [shuCavalry] [piker]
        after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
    Spec.assertEqWith s "bob took 2" (S.lifeOf S.bob after) (Just 18)
    Spec.assertEqWith s "the horseman lives" (S.creaturesInPlay S.alice after) 1
    Spec.assertEqWith s "the would-be blocker lives" (S.creaturesInPlay S.bob after) 1

  -- CR 702.118b, off a PRINTED keyword: Furtive Homunculus ({1}{U} Creature --
  -- Homunculus 2/1, skulk and nothing else) has no other text for a case to pass
  -- on. Its power 2 is the threshold every case here is measured against, and the
  -- three blockers have powers 3, 2 and 1 -- distinct, and straddling it.
  Spec.it s "CR 702.118b skulk bars a bigger blocker, admits an equal or smaller one" $ do
    -- One tuple, three powers: greater is barred, EQUAL is not (702.118b says
    -- "greater", so the boundary is the case a >= would get wrong), lesser is not.
    homunculus <- S.printingOf s registry "Furtive Homunculus"
    hillGiant <- S.printingOf s registry "Hill Giant"
    piker <- S.printingOf s registry "Goblin Piker"
    elves <- S.printingOf s registry "Llanowar Elves"
    let (gs, mine, theirs) = attacking [homunculus] [hillGiant, piker, elves]
        blocks b a = Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs
    case (mine, theirs) of
      (a : _, [bigger, equal, smaller]) ->
        Spec.assertEqWith
          s
          "3 is barred, 2 and 1 are not"
          (blocks bigger a, blocks equal a, blocks smaller a)
          (False, True, True)
      _ -> Spec.assertFailure s "fixture should have an attacker and three blockers"
  Spec.it s "CR 702.118b a skulker may block an attacker of any power" $ do
    -- THE ASYMMETRY, 702.9b's for skulk: 702.118b restricts being BLOCKED and
    -- says nothing about blocking. The SMALLER attacker is the falsifier -- an
    -- implementation that reads skulk off the blocker bars the 2/1 skulker from
    -- the 1/1 Elves, and a blocker-read cannot be told from an attacker-read on
    -- the 3/3 Giant, where 2 <= 3 either way.
    hillGiant <- S.printingOf s registry "Hill Giant"
    elves <- S.printingOf s registry "Llanowar Elves"
    homunculus <- S.printingOf s registry "Furtive Homunculus"
    let blocks attacker =
          let (gs, mine, theirs) = attacking [attacker] [homunculus]
           in case (mine, theirs) of
                (a : _, b : _) -> Just (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs)
                _ -> Nothing
    Spec.assertEqWith
      s
      "the skulker blocks a 3/3 and a 1/1 alike"
      (blocks hillGiant, blocks elves)
      (Just True, Just True)
  Spec.it s "CR 702.118b the powers compared are the PROJECTED ones" $ do
    -- The same Piker, blocking legally at its printed 2 and illegally at 3 after
    -- a CR 122.1a +1/+1 counter -- so the gate reads CR 613's answer and not the
    -- printed box. One board, one counter apart.
    homunculus <- S.printingOf s registry "Furtive Homunculus"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [homunculus] [piker]
        blocks b a = Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a))
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertEqWith
          s
          "printed 2 blocks, counter-boosted 3 does not"
          (blocks b a gs, blocks b a (S.addCounter CounterKind.PlusOnePlusOne 1 b gs))
          (True, False)
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.118b a skulker connects past a bigger untapped creature, in a real combat" $ do
    -- The gameplay-level case, with its own control: the SAME attacker into a
    -- Piker it does not outclass is blocked, and both 2/1s trade. So the Hill
    -- Giant reading is skulk talking, not a fixture that never blocks.
    homunculus <- S.printingOf s registry "Furtive Homunculus"
    hillGiant <- S.printingOf s registry "Hill Giant"
    piker <- S.printingOf s registry "Goblin Piker"
    let play defender =
          let (gs, _, _) = S.combatBoardOf [homunculus] [defender]
              after = S.settleSba (S.fightWith S.aggressiveAnswer gs)
           in (S.lifeOf S.bob after, S.creaturesInPlay S.alice after, S.creaturesInPlay S.bob after)
    Spec.assertEqWith
      s
      "unblockable past the Giant; traded with the Piker"
      (play hillGiant, play piker)
      ((Just 18, 1, 1), (Just 20, 0, 0))

  Spec.it s "CR 702.14c a swampwalker may not be blocked while the defending player controls a Swamp" $ do
    -- Bog Wraith is "Creature -- Wraith 3/3, Swampwalk" and nothing else, so
    -- this asks about the keyword and no other text.
    bogWraith <- S.printingOf s registry "Bog Wraith"
    piker <- S.printingOf s registry "Goblin Piker"
    swamp <- S.printingOf s registry "Swamp"
    let (gs0, mine, theirs) = attacking [bogWraith] [piker]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) (withLands [swamp] gs0))) "illegal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"

  Spec.it s "CR 702.14c a swampwalker is blocked normally when the defending player's land is an Island" $ do
    -- THE FALSIFIER, and the reason the case above cannot pass vacuously:
    -- the same board with the wrong land. The declaration is legal AND the
    -- block survives a real declare blockers step.
    bogWraith <- S.printingOf s registry "Bog Wraith"
    piker <- S.printingOf s registry "Goblin Piker"
    island <- S.printingOf s registry "Island"
    let (gs0, mine, theirs) = attacking [bogWraith] [piker]
        gs = withLands [island] gs0
    case (mine, theirs) of
      (a : _, b : _) -> do
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs) "legal"
        let after = S.runPure S.aggressiveAnswer gs (Combat.declareBlockers S.manaPerformer)
        Spec.assertEqWith s "the block sticks" (Combat.blockersOf a after) (Set.singleton b)
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"

  Spec.it s "CR 702.14c the land type read is the PROJECTED one, so an Urborg'd Island is a Swamp" $ do
    -- THE FALSIFIER for reading the defending player's lands off their
    -- PRINTED type lines. Urborg, Tomb of Yawgmoth is "Each land is a Swamp
    -- in addition to its other land types" -- a CR 613 layer-4
    -- AddLandSubtype over every land -- so bob's Island is a Swamp and the
    -- Wraith walks on it. Urborg is ALICE'S, so the only land bob controls
    -- printed no Swamp at all.
    bogWraith <- S.printingOf s registry "Bog Wraith"
    piker <- S.printingOf s registry "Goblin Piker"
    island <- S.printingOf s registry "Island"
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    let (gs0, mine, theirs) = attacking [bogWraith] [piker]
        gs = snd (S.addPermanent urborg S.alice (withLands [island] gs0))
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs)) "illegal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"

  -- CR 702.14c's FOURTH clause: "with both the specified type or supertype and
  -- the specified subtype (as in 'snow swampwalk')". Legions of Lim-Dûl is the
  -- printing, and the pair of cases below is what a bare land type could not
  -- distinguish at all -- both lands are Swamps, and only one is snow.
  Spec.it s "CR 702.14c a snow swampwalker walks on a Snow-Covered Swamp" $ do
    legions <- S.printingOf s registry "Legions of Lim-Dûl"
    piker <- S.printingOf s registry "Goblin Piker"
    snowSwamp <- S.printingOf s registry "Snow-Covered Swamp"
    let (gs0, mine, theirs) = attacking [legions] [piker]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) (withLands [snowSwamp] gs0))) "illegal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"

  Spec.it s "CR 702.14c a snow swampwalker does NOT walk on an ordinary Swamp" $ do
    -- THE DISCRIMINATOR for the supertype half. A plain Swamp satisfies the
    -- subtype and not the supertype, so the conjunction must fail -- which is
    -- exactly what the old bare-Subtype payload could not express, since it
    -- would have seen a Swamp and stopped there.
    legions <- S.printingOf s registry "Legions of Lim-Dûl"
    piker <- S.printingOf s registry "Goblin Piker"
    swamp <- S.printingOf s registry "Swamp"
    let (gs0, mine, theirs) = attacking [legions] [piker]
        gs = withLands [swamp] gs0
    case (mine, theirs) of
      (a : _, b : _) -> do
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs) "legal"
        let after = S.runPure S.aggressiveAnswer gs (Combat.declareBlockers S.manaPerformer)
        Spec.assertEqWith s "the block sticks" (Combat.blockersOf a after) (Set.singleton b)
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"

  -- The OTHER conjunct of snow swampwalk. The pair above is satisfied by an
  -- implementation that evaluated HasSupertype Snow alone and dropped
  -- HasSubtype Swamp -- both its lands are Swamps, so the subtype never
  -- discriminates. A Snow-Covered Mountain is snow and not a Swamp, which
  -- closes that half.
  Spec.it s "CR 702.14c a snow swampwalker does NOT walk on a Snow-Covered Mountain" $ do
    legions <- S.printingOf s registry "Legions of Lim-Dûl"
    piker <- S.printingOf s registry "Goblin Piker"
    snowMountain <- S.printingOf s registry "Snow-Covered Mountain"
    let (gs0, mine, theirs) = attacking [legions] [piker]
        gs = withLands [snowMountain] gs0
    case (mine, theirs) of
      (a : _, b : _) -> do
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs) "legal"
        let after = S.runPure S.aggressiveAnswer gs (Combat.declareBlockers S.manaPerformer)
        Spec.assertEqWith s "the block sticks" (Combat.blockersOf a after) (Set.singleton b)
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"

  -- CR 702.14c's THIRD clause: "without the specified type or supertype (as in
  -- 'nonbasic landwalk')". Dryad Sophisticate is the printing, and this is the
  -- clause NO positive subtype test can express -- the criterion is a negation.
  Spec.it s "CR 702.14c a nonbasic landwalker walks on a nonbasic land" $ do
    dryad <- S.printingOf s registry "Dryad Sophisticate"
    piker <- S.printingOf s registry "Goblin Piker"
    -- Ash Barrens is a plain nonbasic land: no Basic supertype, and no text
    -- that touches types. Deliberately NOT Urborg, whose land-type rewriting
    -- would drag a CR 613 layer-4 subtype change into a test about a SUPERTYPE,
    -- where it is inert and only obscures what is being asked.
    ashBarrens <- S.printingOf s registry "Ash Barrens"
    let (gs0, mine, theirs) = attacking [dryad] [piker]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) (withLands [ashBarrens] gs0))) "illegal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"

  Spec.it s "CR 702.14c a nonbasic landwalker is blocked normally when every land is basic" $ do
    -- THE FALSIFIER for the negation, and the case that separates it from every
    -- positive test: a basic Swamp is a land the criterion must REJECT. An
    -- implementation that ignored the Not and matched any land would call this
    -- illegal.
    --
    -- It falsifies the LAND-NESS conjunct too, which is the other half of the
    -- design. landwalkAllowsGiven's candidate set is everything the defender
    -- controls, not only their lands, and bob's blocking Goblin Piker is a
    -- nonbasic permanent -- so a reader that left the CardType.Land test out
    -- would match the Piker and call this illegal as well.
    dryad <- S.printingOf s registry "Dryad Sophisticate"
    piker <- S.printingOf s registry "Goblin Piker"
    swamp <- S.printingOf s registry "Swamp"
    let (gs0, mine, theirs) = attacking [dryad] [piker]
        gs = withLands [swamp] gs0
    case (mine, theirs) of
      (a : _, b : _) -> do
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs) "legal"
        let after = S.runPure S.aggressiveAnswer gs (Combat.declareBlockers S.manaPerformer)
        Spec.assertEqWith s "the block sticks" (Combat.blockersOf a after) (Set.singleton b)
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"

  -- CR 702.14c's SECOND clause: "with the specified type or supertype (as in
  -- 'artifact landwalk')". Vectis Gloves is the only paper source of artifact
  -- landwalk, and it GRANTS the keyword rather than printing it on a creature --
  -- so the criterion arrives through a CR 613.1f layer-6 GainKeyword rather than
  -- off the card, which is the other way to have one (Lord of Atlantis is the
  -- pool's second grant, in TextChangedLandwalk below).
  Spec.it s "CR 702.14c an artifact landwalker granted by Vectis Gloves walks on an artifact land" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    gloves <- S.printingOf s registry "Vectis Gloves"
    seat <- S.printingOf s registry "Seat of the Synod"
    let (gs0, mine, theirs) = attacking [piker] [piker]
    case (mine, theirs) of
      (a : _, b : _) -> do
        let (glovesId, equipped) = S.addPermanent gloves S.alice (withLands [seat] gs0)
            armed = S.attach glovesId a equipped
        -- The premise: the Gloves really grant it, and the Piker prints none.
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) armed)) "illegal while equipped"
        -- THE FALSIFIER, and what makes this a granted-keyword test rather than
        -- a repeat of the printed ones: the SAME board with the Gloves
        -- unattached. A bare Piker has no landwalk, so the block is legal.
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) equipped) "legal once nothing is equipped"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"

  Spec.it s "CR 702.14c an artifact landwalker does NOT walk on a plain land" $ do
    -- The other discriminator: Seat of the Synod is an Artifact Land, and an
    -- ordinary Swamp is not. An implementation that dropped the criterion and
    -- matched any land would call this illegal.
    piker <- S.printingOf s registry "Goblin Piker"
    gloves <- S.printingOf s registry "Vectis Gloves"
    swamp <- S.printingOf s registry "Swamp"
    let (gs0, mine, theirs) = attacking [piker] [piker]
    case (mine, theirs) of
      (a : _, b : _) -> do
        let (glovesId, board) = S.addPermanent gloves S.alice (withLands [swamp] gs0)
            armed = S.attach glovesId a board
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) armed) "legal"
        let after = S.runPure S.aggressiveAnswer armed (Combat.declareBlockers S.manaPerformer)
        Spec.assertEqWith s "the block sticks" (Combat.blockersOf a after) (Set.singleton b)
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"

  Spec.it s "CR 702.14d swampwalk on the BLOCKER cancels nothing" $ do
    -- CR 702.14d's own example, in swamps: the defending player controls the
    -- named land AND a creature with the same landwalk, and still may not
    -- block. Fails against any implementation that compares the attacker's
    -- landwalk with the blocker's -- which is how protection reads, and is
    -- the wrong shape here.
    bogWraith <- S.printingOf s registry "Bog Wraith"
    swamp <- S.printingOf s registry "Swamp"
    let (gs0, mine, theirs) = attacking [bogWraith] [bogWraith]
        gs = withLands [swamp] gs0
    case (mine, theirs) of
      (a : _, b : _) -> do
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs)) "illegal"
        let after = S.runPure S.aggressiveAnswer gs (Combat.declareBlockers S.manaPerformer)
        Spec.assertEqWith s "nobody blocks" (Combat.blockersOf a after) Set.empty
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"

-- CR 612.1's word swap, cast for real: alice pays {U} out of her own Island and
-- resolves a Magical Hack aimed at `target`, replacing `from` with `to`.
--
-- Her OWN Island, deliberately. CR 702.14c reads the DEFENDING player's lands,
-- so mana on alice's side of the board cannot satisfy the landwalk this Hack is
-- about to rewrite, and every land bob controls in these cases is there to be
-- read rather than tapped.
castHackAt :: ObjectId.ObjectId -> ObjectId.ObjectId -> Subtype.Subtype -> Subtype.Subtype -> GameState.GameState -> GameState.GameState
castHackAt hackId target from to gs =
  let answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject target))) sets
        Prompt.ChooseLandTypeSwap {} -> (from, to)
        _ -> S.identityAnswer p
   in S.runPure answer (gs {GameState.priority = Just S.alice}) (do S.cast S.alice hackId; Stack.resolveTop)

-- The board both landwalk text-change groups attack from: alice attacks with
-- everything she has, bob defends with a Goblin Piker and one land of
-- `landName`, and alice holds a Magical Hack plus the Island that pays for it.
-- With `hacked`, she casts it at `hackTarget` (chosen from her permanents by the
-- caller) before attackers are declared.
--
-- Returns the post-declaration state, the attacker of interest and the blocker.
hackedLandwalkBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  [Printing.Printing] ->
  ([ObjectId.ObjectId] -> Maybe ObjectId.ObjectId) ->
  Bool ->
  Subtype.Subtype ->
  Subtype.Subtype ->
  Printing.Printing ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
hackedLandwalkBoard s registry mine hackTarget hacked from to defendersLand = do
  piker <- S.printingOf s registry "Goblin Piker"
  island <- S.printingOf s registry "Island"
  magicalHack <- S.printingOf s registry "Magical Hack"
  let (gs0, ours, theirs) = S.combatBoardOf mine [piker]
      (_, gs1) = S.addPermanent island S.alice gs0
      (_, gs2) = S.addPermanent defendersLand S.bob gs1
      (hackId, gs3) = S.addHandCard magicalHack S.alice gs2
      board = case (hacked, hackTarget ours) of
        (True, Just t) -> castHackAt hackId t from to gs3
        _ -> gs3
      attacked = snd (Engine.runGamePure S.aggressiveAnswer board (Combat.declareAttackers S.manaPerformer S.alice))
  case (ours, theirs) of
    (a : _, b : _) -> pure (attacked, a, b)
    _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"

-- CR 612.1 reaching a keyword's own land type, end to end through the real
-- engine, in both of the two ways a creature can have one.
--
-- CR 702.14a: landwalk "appears within an object's rules
-- text as '[type]walk'". CR 612.1: a text change reaches
-- "any words or symbols printed on that object". So the land type in swampwalk
-- is a word a Magical Hack swaps -- which is the example Magical Hack's own
-- reminder text gives, "you may change 'swampwalk' to 'plainswalk'".
--
-- Each half is a 2x2: hacked or not, crossed with the OLD land and the NEW one
-- on bob's side. The diagonal is what discriminates -- "the Merfolk was blocked"
-- is equally true of a landwalk that never applied for some unrelated reason, so
-- the unhacked pair is asserted alongside as the paired control.
textChangedLandwalkSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
textChangedLandwalkSpec s registry = Spec.describe s "TextChangedLandwalk" $ do
  -- The GRANTED half. Lord of Atlantis {U}{U} Creature -- Merfolk 2/2, "Other
  -- Merfolk get +1/+1 and have islandwalk." (checked against Scryfall,
  -- 2026-08-05) and Tidal Warrior, the pool's other Merfolk, are the whole
  -- board.
  --
  -- Which creature the Hack NAMES is the whole of CR 612.3: "any abilities that
  -- are granted to an object can't be modified by text-changing effects that
  -- affect that object". Naming the Lord rewrites the grant; naming the Warrior
  -- that RECEIVED islandwalk must not, and the case below asserts exactly that.
  --
  -- Two mechanisms give it, and the first is the one that scopes the rewrite:
  -- gatherStatic is called with the SOURCE's own changes, so a Hack on the
  -- Warrior never reaches the Lord's GainKeyword at all. The layer order (the
  -- swap at 3, the grant at 6) is the second and weaker one.
  let -- combatBoardOf returns the ids in printing order, so the Warrior is the
      -- head and the Lord is the second. Which of the two the Hack names is the
      -- parameter, since that is the whole difference between CR 612.1's case
      -- and CR 612.3's.
      theWarrior = Maybe.listToMaybe
      theLord = Maybe.listToMaybe . drop 1
      lordBoardAt hackTarget hacked land = do
        lord <- S.printingOf s registry "Lord of Atlantis"
        tidalWarrior <- S.printingOf s registry "Tidal Warrior"
        landP <- S.printingOf s registry land
        hackedLandwalkBoard s registry [tidalWarrior, lord] hackTarget hacked Subtype.Island Subtype.Swamp landP
      lordBoard = lordBoardAt theLord
  Spec.it s "CR 702.14c an unhacked Lord of Atlantis grants ISLANDwalk" $ do
    -- The premise, and the control the two hacked cases are read against: with
    -- the Lord's text as printed, bob's Island stops the block and his Swamp
    -- does not.
    (onIsland, warrior, blocker) <- lordBoard False "Island"
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton blocker (Set.singleton warrior)) onIsland)) "an Island stops the block"
    -- The Lord's OTHER modification, which the swap must leave alone: a Tidal
    -- Warrior is a printed 1/1, so a 2/2 is the +1/+1 half still applying.
    Spec.assertEqWith s "and the Warrior is a 2/2" (Projection.powerOf warrior onIsland) (Just 2)
    (onSwamp, warrior', blocker') <- lordBoard False "Swamp"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker' (Set.singleton warrior')) onSwamp) "a Swamp does not"
  Spec.it s "CR 612.1 a hacked Lord of Atlantis grants SWAMPwalk instead" $ do
    -- THE CASE. Island -> Swamp on the Lord, and bob's board never moves: the
    -- Island that used to stop the block no longer does, and the Swamp that
    -- used to allow it no longer does either. Both halves fail against a
    -- rewrite that walks past a Modification.GainKeyword.
    (onIsland, warrior, blocker) <- lordBoard True "Island"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker (Set.singleton warrior)) onIsland) "an Island no longer stops the block"
    let after = S.runPure S.aggressiveAnswer onIsland (Combat.declareBlockers S.manaPerformer)
    Spec.assertEqWith s "and the block sticks" (Combat.blockersOf warrior after) (Set.singleton blocker)
    Spec.assertEqWith s "the Warrior is still a 2/2" (Projection.powerOf warrior onIsland) (Just 2)
    (onSwamp, warrior', blocker') <- lordBoard True "Swamp"
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton blocker' (Set.singleton warrior')) onSwamp)) "a Swamp stops it now"
  Spec.it s "CR 612.3 a Hack on the creature that RECEIVED islandwalk moves nothing" $ do
    -- CR 612.3 itself, and the case the two above are read against a THIRD
    -- time: the same Island -> Swamp swap, aimed at the WARRIOR. Its islandwalk
    -- is the Lord's text and not its own, so the swap cannot reach it and the
    -- board answers exactly as the unhacked one does.
    --
    -- The Warrior is a legal and non-vacuous target: its own printed text says
    -- "target land becomes an Island until end of turn", so the Hack really does
    -- have an Island of its own to rewrite there. What it must not rewrite is
    -- the keyword, which arrived from somewhere else.
    (onIsland, warrior, blocker) <- lordBoardAt theWarrior True "Island"
    -- THE ANTI-VACUITY CHECK, and it has to come first: every assertion below
    -- also holds of a Hack that was never cast at all. This one says the swap
    -- really did land, and on the Warrior.
    Spec.assertEqWith s "the Hack resolved onto the Warrior" (Projection.textChangesAffecting warrior onIsland) [(Subtype.Island, Subtype.Swamp)]
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton blocker (Set.singleton warrior)) onIsland)) "an Island still stops the block"
    Spec.assertEqWith s "and the Warrior is still a 2/2" (Projection.powerOf warrior onIsland) (Just 2)
    -- The half that keeps this from passing by the landwalk simply vanishing: a
    -- Warrior that had wrongly picked up swampwalk would make this one illegal.
    (onSwamp, warrior', blocker') <- lordBoardAt theWarrior True "Swamp"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker' (Set.singleton warrior')) onSwamp) "and a Swamp still does not"
    let after = S.runPure S.aggressiveAnswer onSwamp (Combat.declareBlockers S.manaPerformer)
    Spec.assertEqWith s "the block sticks" (Combat.blockersOf warrior' after) (Set.singleton blocker')
  -- The PRINTED half, one carrier over: Bog Wraith ("Creature -- Wraith 3/3,
  -- Swampwalk" and nothing else) has the keyword on its own type line rather
  -- than from a grant, so the swap has to reach the projection's keyword map at
  -- CR 613.1c layer 3 instead of a static ability's modification. Same rule,
  -- different site -- and the pair below is what tells the two apart.
  let wraithBoard hacked land = do
        bogWraith <- S.printingOf s registry "Bog Wraith"
        landP <- S.printingOf s registry land
        hackedLandwalkBoard s registry [bogWraith] Maybe.listToMaybe hacked Subtype.Swamp Subtype.Island landP
  Spec.it s "CR 702.14c an unhacked Bog Wraith walks on SWAMPS" $ do
    (onSwamp, wraith, blocker) <- wraithBoard False "Swamp"
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton blocker (Set.singleton wraith)) onSwamp)) "a Swamp stops the block"
    (onIsland, wraith', blocker') <- wraithBoard False "Island"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker' (Set.singleton wraith')) onIsland) "an Island does not"
  Spec.it s "CR 612.1 a hacked Bog Wraith walks on ISLANDS" $ do
    (onSwamp, wraith, blocker) <- wraithBoard True "Swamp"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker (Set.singleton wraith)) onSwamp) "a Swamp no longer stops the block"
    let after = S.runPure S.aggressiveAnswer onSwamp (Combat.declareBlockers S.manaPerformer)
    Spec.assertEqWith s "and the block sticks" (Combat.blockersOf wraith after) (Set.singleton blocker)
    (onIsland, wraith', blocker') <- wraithBoard True "Island"
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton blocker' (Set.singleton wraith')) onIsland)) "an Island stops it now"
  -- The THIRD carrier, and the one that needed the walk into a defined card's
  -- keywords (see #643): a landwalk printed on a TOKEN, by the spell that mints
  -- it. Goblin Scouts {3}{R}{R} Sorcery, whole text "Create three 1/1 red Goblin
  -- Scout creature tokens with mountainwalk" (checked against Scryfall), hacked
  -- ON THE STACK so CR 612.2a's swap reaches the card the Create defines.
  --
  -- The word is in the KEYWORD alone: Mountain is a land type and the token's
  -- type line spells Goblin Scout, so a rewrite gated on the type line -- which
  -- is how #640 reached these faces -- finds nothing and stops. The token's
  -- subtypes and name are asserted unchanged for exactly that reason.
  let scoutBoard hacked land = do
        landP <- S.printingOf s registry land
        goblinScoutsBoard s registry hacked landP
  Spec.it s "CR 702.14c an unhacked Goblin Scouts mints MOUNTAINwalkers" $ do
    (onMountain, scout, blocker) <- scoutBoard False "Mountain"
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton blocker (Set.singleton scout)) onMountain)) "a Mountain stops the block"
    (onSwamp, scout', blocker') <- scoutBoard False "Swamp"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker' (Set.singleton scout')) onSwamp) "a Swamp does not"
  Spec.it s "CR 612.1 a hacked Goblin Scouts mints SWAMPwalkers instead" $ do
    (onMountain, scout, blocker) <- scoutBoard True "Mountain"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker (Set.singleton scout)) onMountain) "a Mountain no longer stops the block"
    let after = S.runPure S.aggressiveAnswer onMountain (Combat.declareBlockers S.manaPerformer)
    Spec.assertEqWith s "and the block sticks" (Combat.blockersOf scout after) (Set.singleton blocker)
    -- CR 612.2 held to the one word: Magical Hack swaps land types, so the
    -- token's creature types and its derived name are untouched.
    Spec.assertEqWith s "still a Goblin Scout" (Projection.subtypesOf scout onMountain) (Set.fromList [Subtype.Goblin, Subtype.Scout])
    Spec.assertEqWith s "still named Goblin Scout Token" (Projection.namesOf scout onMountain) (Set.singleton (CardName.MkCardName (Text.pack "Goblin Scout Token")))
    (onSwamp, scout', blocker') <- scoutBoard True "Swamp"
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton blocker' (Set.singleton scout')) onSwamp)) "a Swamp stops it now"

-- alice casts Goblin Scouts, optionally has a Magical Hack resolved at the
-- SORCERY ON THE STACK (Mountain -> Swamp), and then the sorcery resolves; the
-- three tokens settle and attack into bob's Goblin Piker and one land of
-- `defendersLand`. Returns the post-declaration state, one Scout and the
-- blocker.
--
-- Five Mountains and an Island on alice's side, and the Island is deliberate for
-- castHackAt's reason: CR 702.14c reads the DEFENDING player's lands, so nothing
-- alice controls can satisfy the landwalk under test.
--
-- Engine.settleAll after the mint, because CR 302.6 would otherwise keep tokens
-- created this turn out of the attack entirely and the fixture would prove
-- nothing.
goblinScoutsBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  Bool ->
  Printing.Printing ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
goblinScoutsBoard s registry hacked defendersLand = do
  piker <- S.printingOf s registry "Goblin Piker"
  mountain <- S.printingOf s registry "Mountain"
  island <- S.printingOf s registry "Island"
  goblinScouts <- S.printingOf s registry "Goblin Scouts"
  magicalHack <- S.printingOf s registry "Magical Hack"
  let (gs0, _, theirs) = S.combatBoardOf [] [piker]
      gs1 = S.landsFor island S.alice 1 (S.landsFor mountain S.alice 5 gs0)
      (_, gs2) = S.addPermanent defendersLand S.bob gs1
      (scoutsId, gs3) = S.addHandCard goblinScouts S.alice gs2
      (hackId, gs4) = S.addHandCard magicalHack S.alice gs3
      onStack = S.runPure S.identityAnswer (gs4 {GameState.priority = Just S.alice}) (S.cast S.alice scoutsId)
      spellId = case GameState.stack onStack of
        top : _ -> top
        [] -> ObjectId.MkObjectId 999
      swapped = if hacked then castHackAt hackId spellId Subtype.Mountain Subtype.Swamp onStack else onStack
      minted = S.runPure S.identityAnswer swapped Stack.resolveTop
      settled = S.runPure S.identityAnswer minted (Engine.settleAll S.alice)
      attacked = S.runPure S.aggressiveAnswer settled (Combat.declareAttackers S.manaPerformer S.alice)
  case (S.tokensOf attacked, theirs) of
    (a : _, b : _) -> pure (attacked, a, b)
    _ -> Spec.assertFailure s "fixture should have minted a Scout and left a blocker"

-- CR 702.111: grant menace to `oid` with a stored continuous effect, withFear's
-- twin. Used only by the CR 509.1b "after a legal block has been declared" case
-- below, which needs menace to ARRIVE mid-combat; every other case here reads
-- Boggart Brute's printed keyword.
withMenace :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
withMenace oid gs =
  let (ts, gs1) = Game.freshTimestamp gs
      eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = oid,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = Expiry.AtCleanup,
            ContinuousEffect.modification = Modification.GainKeyword Keyword.Menace,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}

-- Hammerheim's "{T}: Target creature loses all landwalk abilities until end of
-- turn." (Oracle checked against Scryfall, 2026-09-02) activated at `victim` and
-- resolved. Its FIRST ability is "{T}: Add {R}.", so the removal is the second of
-- the two the projection hands out.
--
-- Projection.abilitiesOf rather than Activate.abilitiesFor: CR 605.3b keeps a
-- mana ability off the activatable list, so the pair here is the printed pair.
removingLandwalk :: (Monad m) => Spec.Spec m n -> ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> m GameState.GameState
removingLandwalk s hammerheimId victim board = case Projection.abilitiesOf hammerheimId board of
  [_, remove] -> pure (S.runPure (namingTarget victim) board (Activate.activateAbility S.alice hammerheimId remove >> Stack.resolveTop))
  abilities -> Spec.assertFailure s ("expected exactly two Hammerheim abilities, got " <> show (length abilities))

-- The board CR 702.14a's family removal is read on. Alice attacks with a Stalker
-- Hag ({B/G}{B/G}{B/G} Creature -- Hag 3/2, "Swampwalk, forestwalk", the pool's
-- only creature printing TWO landwalks) and controls Hammerheim plus a Concordant
-- Crossroads; bob defends with a Goblin Piker, a Swamp AND a Forest.
--
-- BOTH of bob's lands, which is what makes this a FAMILY case rather than a
-- second spelling of Modification.LoseKeyword: with only one of them down, taking
-- one written landwalk away would already free the block, and the board could not
-- tell a removal that reached one instance from one that reached the family.
--
-- The Crossroads is the sibling keyword. "All creatures have haste" is a static
-- ability, so CR 613.7a gives its effect the enchantment's own timestamp and CR
-- 613.7b gives the removal a later one: the grant is in place first, and a CR
-- 613.1f WIPE would take the Hag's haste with the landwalks where a family
-- removal must leave it standing. Haste and not an evasion
-- keyword deliberately: a granted flying would decide the block by itself and
-- mask the case.
--
-- `activated` is the only difference between the two boards.
hammerheimBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
hammerheimBoard s registry activated = do
  stalkerHag <- S.printingOf s registry "Stalker Hag"
  piker <- S.printingOf s registry "Goblin Piker"
  swamp <- S.printingOf s registry "Swamp"
  forest <- S.printingOf s registry "Forest"
  crossroads <- S.printingOf s registry "Concordant Crossroads"
  hammerheim <- S.printingOf s registry "Hammerheim"
  let (gs0, ours, theirs) = attacking [stalkerHag] [piker]
      (_, hasted) = S.addPermanent crossroads S.alice (withLands [swamp, forest] gs0)
      (hammerheimId, placed) = S.addPermanent hammerheim S.alice hasted
  case (ours, theirs) of
    (hag : _, blocker : _) -> do
      board <- if activated then removingLandwalk s hammerheimId hag placed else pure placed
      pure (board, hag, blocker)
    _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"

-- CR 613.1f layer 6 scoped to CR 702.14a's GENERIC TERM: "loses all landwalk
-- abilities" reaches every written [type]walk at once, which no removal naming an
-- instance can do.
landwalkFamilyRemovalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
landwalkFamilyRemovalSpec s registry = Spec.describe s "LandwalkFamilyRemoval" $ do
  Spec.it s "CR 702.14c an unhammered Stalker Hag walks over bob's Swamp and Forest" $ do
    -- The control the case below is read against, and the anti-vacuity check on
    -- it: the same board with the ability never activated, where both printed
    -- landwalks stop the block.
    (board, hag, blocker) <- hammerheimBoard s registry False
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton blocker (Set.singleton hag)) board)) "the block is illegal while both landwalks stand"
    Spec.assertBool s (Projection.hasKeyword (Keyword.Landwalk (Filter.HasSubtype Subtype.Swamp)) hag board) "swampwalk as printed"
    Spec.assertBool s (Projection.hasKeyword (Keyword.Landwalk (Filter.HasSubtype Subtype.Forest)) hag board) "forestwalk as printed"
    Spec.assertBool s (Projection.hasKeyword Keyword.Haste hag board) "and the Crossroads' haste"
  Spec.it s "CR 613.1f Hammerheim takes BOTH landwalks and the Hag can be blocked" $ do
    -- THE CASE. Bob's board never moves; the Hag's does. Both written landwalks
    -- have to go for this declaration to be legal, since either one alone would
    -- still find a land of its own on bob's side.
    (board, hag, blocker) <- hammerheimBoard s registry True
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker (Set.singleton hag)) board) "the block is legal once every landwalk is gone"
    let after = S.runPure S.aggressiveAnswer board (Combat.declareBlockers S.manaPerformer)
    Spec.assertEqWith s "and the block sticks" (Combat.blockersOf hag after) (Set.singleton blocker)
    -- THE FAMILY-NOT-WIPE half: CR 613.1f's removal is scoped to rule 702.14's
    -- abilities, so haste -- a keyword of no family at all -- survives it.
    Spec.assertBool s (Projection.hasKeyword Keyword.Haste hag board) "haste is no landwalk, so it survives"
    Spec.assertBool s (not (Projection.hasKeyword (Keyword.Landwalk (Filter.HasSubtype Subtype.Swamp)) hag board)) "swampwalk is gone"
    Spec.assertBool s (not (Projection.hasKeyword (Keyword.Landwalk (Filter.HasSubtype Subtype.Forest)) hag board)) "forestwalk is gone too"

-- CR 509.1a: the defending player chooses ONE creature for each blocker to
-- block, and an effect can raise that number. Foriysian Brigade {3}{W} 2/4,
-- "This creature can block an additional creature each combat", is the pool's
-- plainest printing -- it says nothing else at all, so these cases read the
-- arity and nothing beside it. High Ground {W} says the same sentence about a
-- whole team.
--
-- Every case here blocks with BOB's creatures, CR 509.1 giving the declaration
-- to the defending player.
blockPermissionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
blockPermissionSpec s registry = Spec.describe s "BlockPermission" $ do
  Spec.it s "CR 509.1a a Brigade blocks two attackers where a Piker blocks one" $ do
    -- The anti-vacuity control is the PIKER on the same board: it is offered as
    -- a blocker, it may block either attacker alone, and the pair is refused --
    -- so the Brigade's extra is the permission talking and not the fixture.
    brigade <- S.printingOf s registry "Foriysian Brigade"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker, piker] [brigade, piker]
    case (mine, theirs) of
      ([first, second], [b, plain]) ->
        Spec.assertEqWith
          s
          "the Brigade takes both, the Piker only one"
          ( Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.fromList [first, second])) gs,
            Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton first)) gs,
            Combat.legalBlockDeclaration S.bob (Map.singleton plain (Set.fromList [first, second])) gs,
            Combat.legalBlockDeclaration S.bob (Map.singleton plain (Set.singleton first)) gs
          )
          (True, True, False, True)
      _ -> Spec.assertFailure s "fixture should have two attackers and two blockers"
  Spec.it s "CR 509.1a High Ground gives the arity to the whole team" $ do
    -- The Affected arm the Brigade cannot exercise: an enchantment naming
    -- creatures its controller controls. alice's Piker is not helped by bob's
    -- High Ground, which is what makes the ControlledBy half of the filter
    -- observable -- the third reading below is alice's own creature.
    highGround <- S.printingOf s registry "High Ground"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker, piker] [piker]
        (control, _, plain) = attacking [piker, piker] [piker]
    case (mine, theirs, plain) of
      ([first, second], [b], [other]) -> do
        let enchanted = snd (S.addPermanent highGround S.bob gs)
        Spec.assertEqWith
          s
          "with the enchantment two, without it one"
          ( Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.fromList [first, second])) enchanted,
            Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.fromList [first, second])) gs,
            Combat.legalBlockDeclaration S.bob (Map.singleton other (Set.fromList [first, second])) control
          )
          (True, False, False)
      _ -> Spec.assertFailure s "fixture should have two attackers and one blocker"
  Spec.it s "CR 509.1a a Palace Guard blocks every attacker, where a Brigade stops at two" $ do
    -- Palace Guard {2}{W} 1/4, "This creature can block any number of creatures",
    -- says nothing else at all. The Brigade beside it is the anti-vacuity control
    -- and the discriminating one: THREE attackers is exactly where an unbounded
    -- arity parts company with the largest one any card in the pool prints.
    palaceGuard <- S.printingOf s registry "Palace Guard"
    brigade <- S.printingOf s registry "Foriysian Brigade"
    highGround <- S.printingOf s registry "High Ground"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker, piker, piker] [palaceGuard, brigade]
    case (mine, theirs) of
      ([first, second, third], [guard, b]) ->
        Spec.assertEqWith
          s
          "the Guard takes all three, the Brigade only two, and a High Ground beside the Guard changes nothing"
          ( Combat.legalBlockDeclaration S.bob (Map.singleton guard (Set.fromList [first, second, third])) gs,
            Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.fromList [first, second, third])) gs,
            Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.fromList [first, second])) gs,
            -- The last reading is the SUM's absorbing case: a second permission
            -- must leave "any number" alone rather than collapse it to a count.
            Combat.legalBlockDeclaration S.bob (Map.singleton guard (Set.fromList [first, second, third])) (snd (S.addPermanent highGround S.bob gs))
          )
          (True, False, True, True)
      _ -> Spec.assertFailure s "fixture should have three attackers and two blockers"
  Spec.it s "CR 509.1h / 509.3e a real declare blockers step puts the Guard on all three attackers" $ do
    -- Not a claim about legalBlockDeclaration alone: the step runs, and CR
    -- 509.3e's count on the one BlocksDeclared event is three.
    palaceGuard <- S.printingOf s registry "Palace Guard"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker, piker, piker] [palaceGuard]
    case (mine, theirs) of
      ([first, second, third], [guard]) -> do
        let after = S.runPure (blockAll [first, second, third]) gs (Combat.declareBlockers S.manaPerformer)
        Spec.assertEqWith
          s
          "one blocker, three blocked attackers"
          ( Combat.blockersOf first after,
            Combat.blockersOf second after,
            Combat.blockersOf third after,
            [n | GameEvent.BlocksDeclared (BlocksDeclared.MkBlocksDeclared b n) <- S.eventsOf after, b == guard]
          )
          (Set.singleton guard, Set.singleton guard, Set.singleton guard, [3])
      _ -> Spec.assertFailure s "fixture should have three attackers and one blocker"
  Spec.it s "CR 509.1c the maximization enumerates an unbounded blocker's whole power set" $ do
    -- The only path that reads choicesUpTo's unbounded case: blockCeiling's
    -- search runs only with a requirement in force (#342). Three Lured attackers
    -- and one blocker, so the maximum is three -- which the Guard can attain and
    -- the Brigade, capped at two, cannot. Blocking two is legal for the Brigade
    -- for exactly that reason, and illegal for the Guard, which is what a search
    -- that stopped at two would get backwards.
    lure <- S.printingOf s registry "Lure"
    palaceGuard <- S.printingOf s registry "Palace Guard"
    brigade <- S.printingOf s registry "Foriysian Brigade"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = luringAll lure [piker, piker, piker] [palaceGuard]
        (control, ours, yours) = luringAll lure [piker, piker, piker] [brigade]
    case (mine, theirs, ours, yours) of
      ([first, second, third], [guard], [a, b, _], [c]) ->
        Spec.assertEqWith
          s
          "three is attainable for the Guard and not for the Brigade"
          ( Combat.legalBlockDeclaration S.bob (Map.singleton guard (Set.fromList [first, second, third])) gs,
            Combat.legalBlockDeclaration S.bob (Map.singleton guard (Set.fromList [first, second])) gs,
            Combat.legalBlockDeclaration S.bob (Map.singleton c (Set.fromList [a, b])) control
          )
          (True, False, True)
      _ -> Spec.assertFailure s "fixture should have three attackers and one blocker"
  Spec.it s "CR 604.2 / 613.1f a conditional ability-remover whose clause is FALSE removes nothing" $ do
    -- Ray of Frost {1}{U} Enchantment -- Aura: "As long as enchanted creature is
    -- red, it loses all abilities." Palace Guard is WHITE, so the clause is false
    -- and its permission stands. Pawl.Engine.BlockPermission reads
    -- Projection.abilityRemoval from OUTSIDE the layer fold, which is the reader
    -- that used to count a false clause as a removal.
    --
    -- Humility beside it is the anti-vacuity control: an UNCONDITIONAL
    -- LoseAllAbilities on the same board and the same blocker does drop the
    -- permission, so a gate that had turned the whole reader off would fail here.
    ray <- S.printingOf s registry "Ray of Frost"
    humility <- S.printingOf s registry "Humility"
    palaceGuard <- S.printingOf s registry "Palace Guard"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker, piker] [palaceGuard]
    case (mine, theirs) of
      ([first, second], [guard]) -> do
        let both = Map.singleton guard (Set.fromList [first, second])
            (rayId, withRay) = S.addPermanent ray S.bob gs
            enchanted = S.attach rayId guard withRay
            humbled = snd (S.addPermanent humility S.bob gs)
        Spec.assertEqWith
          s
          "the Guard blocks both under the Ray, and only one under Humility"
          ( Combat.legalBlockDeclaration S.bob both gs,
            Combat.legalBlockDeclaration S.bob both enchanted,
            Combat.legalBlockDeclaration S.bob both humbled,
            Combat.legalBlockDeclaration S.bob (Map.singleton guard (Set.singleton first)) humbled
          )
          (True, True, False, True)
      _ -> Spec.assertFailure s "fixture should have two attackers and one blocker"
  Spec.it s "CR 604.2 the Entourage's permission holds only while its controller is the monarch" $ do
    -- Entourage of Trest {4}{G} 4/4, "As long as you're the monarch, this
    -- creature can block an additional creature each combat". CR 109.5's "you" is
    -- the Entourage's own controller, so the third reading is the one that makes
    -- the gate observable: the designation sitting on ALICE grants nothing.
    entourage <- S.printingOf s registry "Entourage of Trest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker, piker] [entourage]
    case (mine, theirs) of
      ([first, second], [e]) -> do
        let pair = Map.singleton e (Set.fromList [first, second])
            one = Map.singleton e (Set.singleton first)
        Spec.assertEqWith
          s
          "monarch bob blocks two; no monarch and monarch alice block one"
          ( Combat.legalBlockDeclaration S.bob pair (S.withMonarch S.bob gs),
            Combat.legalBlockDeclaration S.bob pair gs,
            Combat.legalBlockDeclaration S.bob pair (S.withMonarch S.alice gs),
            Combat.legalBlockDeclaration S.bob one gs
          )
          (True, False, False, True)
      _ -> Spec.assertFailure s "fixture should have two attackers and one blocker"
  Spec.it s "CR 301.5a Kemba's Legion's arity is the Equipment attached to IT" $ do
    -- Kemba's Legion {5}{W}{W} 4/6, "This creature can block an additional creature
    -- each combat for each Equipment attached to this creature" -- the pool's one
    -- COUNTED permission. Bonesplitter and Vectis Gloves say nothing about
    -- blocking, so every number below is the Legion's own sentence.
    --
    -- FOUR attackers, which is what separates the three readings a smaller board
    -- collapses: with two Equipment the count is three, so "any number" would take
    -- the fourth and a fixed "an additional" would refuse the third.
    legion <- S.printingOf s registry "Kemba's Legion"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    gloves <- S.printingOf s registry "Vectis Gloves"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker, piker, piker, piker] [legion, piker]
    case (mine, theirs) of
      ([first, second, third, fourth], [kemba, _]) -> do
        let blocks n = Map.singleton kemba (Set.fromList (take n [first, second, third, fourth]))
            legal n = Combat.legalBlockDeclaration S.bob (blocks n)
            one = withAttachment bonesplitter kemba gs
            two = withAttachment gloves kemba one
        Spec.assertEqWith
          s
          "bare one, one Equipment two, two Equipment three -- and never one more"
          ( legal 1 gs,
            legal 2 gs,
            legal 2 one,
            legal 3 one,
            legal 3 two,
            legal 4 two
          )
          (True, False, True, False, True, False)
      _ -> Spec.assertFailure s "fixture should have four attackers and two blockers"
  Spec.it s "CR 301.5a the Legion counts neither another creature's Equipment nor its own Aura" $ do
    -- The pair that makes the two conjuncts of "Equipment attached to this
    -- creature" observable, each board ONE attachment away from the same
    -- one-Equipment board: a second Equipment on the OTHER blocker (the
    -- IsAttachedToSource half) and an Aura on the Legion itself (the HasSubtype
    -- half). Both must leave the arity at two.
    legion <- S.printingOf s registry "Kemba's Legion"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    gloves <- S.printingOf s registry "Vectis Gloves"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker, piker, piker, piker] [legion, piker]
    case (mine, theirs) of
      ([first, second, third, _], [kemba, plain]) -> do
        let blocks n = Map.singleton kemba (Set.fromList (take n [first, second, third]))
            legal n = Combat.legalBlockDeclaration S.bob (blocks n)
            one = withAttachment bonesplitter kemba gs
            elsewhere = withAttachment gloves plain one
            aura = withAttachment unholyStrength kemba one
        Spec.assertEqWith
          s
          "an Equipment on the Piker and an Aura on the Legion both leave the count at two"
          ( legal 2 elsewhere,
            legal 3 elsewhere,
            legal 2 aura,
            legal 3 aura
          )
          (True, False, True, False)
      _ -> Spec.assertFailure s "fixture should have four attackers and two blockers"
  Spec.it s "CR 509.1b the restrictions are checked against EACH attacker blocked" $ do
    -- A Brigade has neither flying nor reach, so CR 702.9b refuses it the Bird
    -- Maiden however many creatures it may block. The pair is the whole point:
    -- an arity check that stopped at the count would admit the flier alongside
    -- the ground attacker.
    brigade <- S.printingOf s registry "Foriysian Brigade"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [birdMaiden, piker] [brigade]
    case (mine, theirs) of
      ([flier, ground], [b]) ->
        Spec.assertEqWith
          s
          "the flier is out either way"
          ( Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.fromList [flier, ground])) gs,
            Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton flier)) gs,
            Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton ground)) gs
          )
          (False, False, True)
      _ -> Spec.assertFailure s "fixture should have two attackers and one blocker"
  Spec.it s "CR 702.111b menace counts CREATURES blocking each attacker, not blocks" $ do
    -- Two menace attackers need two blockers EACH. The Brigade covering both by
    -- itself is one creature apiece and illegal; the same Brigade plus a Piker on
    -- each Brute is two apiece and legal. A count that read the declaration's
    -- blockers rather than the pairs would call the first one legal.
    boggartBrute <- S.printingOf s registry "Boggart Brute"
    brigade <- S.printingOf s registry "Foriysian Brigade"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [boggartBrute, boggartBrute] [brigade, piker, piker]
    case (mine, theirs) of
      ([first, second], [b, x, y]) ->
        Spec.assertEqWith
          s
          "one creature apiece is not two"
          ( Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.fromList [first, second])) gs,
            Combat.legalBlockDeclaration S.bob (Map.fromList [(b, Set.fromList [first, second]), (x, Set.singleton first), (y, Set.singleton second)]) gs
          )
          (False, True)
      _ -> Spec.assertFailure s "fixture should have two attackers and three blockers"
  Spec.it s "CR 509.1h a real declare blockers step blocks both attackers with the one Brigade" $ do
    -- Not a claim about legalBlockDeclaration alone: the step itself runs, and
    -- both attackers come out of it blocked by the same creature.
    brigade <- S.printingOf s registry "Foriysian Brigade"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker, piker] [brigade]
    case (mine, theirs) of
      ([first, second], [b]) -> do
        let after = S.runPure (blockAll [first, second]) gs (Combat.declareBlockers S.manaPerformer)
        Spec.assertEqWith
          s
          "one blocker, two blocked attackers"
          (Combat.blockersOf first after, Combat.blockersOf second after, Combat.isBlocked first after, Combat.isBlocked second after)
          (Set.singleton b, Set.singleton b, True, True)
      _ -> Spec.assertFailure s "fixture should have two attackers and one blocker"
  Spec.it s "CR 510.1d the Brigade divides its 2 among the creatures it blocks" $ do
    -- 2 power over two 2/1 Pikers: one each kills both, all on one kills only
    -- that one. Distinct counts, so the two divisions cannot be confused -- and
    -- an UNDIVIDED reading, 2 to each Piker, kills both on either leg.
    brigade <- S.printingOf s registry "Foriysian Brigade"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker, piker] [brigade]
        dump :: Prompt.Prompt r -> r
        dump p = case p of
          Prompt.AssignCombatDamage _ _ _ thresholds n ->
            case filter S.isCreatureRecipient (Map.keys thresholds) of
              r : _ -> Map.singleton r n
              [] -> Map.empty
          _ -> blockAll mine p
        split :: Prompt.Prompt r -> r
        split p = case p of
          Prompt.AssignCombatDamage _ _ _ thresholds _ -> Map.fromList (fmap (\r -> (r, 1)) (filter S.isCreatureRecipient (Map.keys thresholds)))
          _ -> blockAll mine p
    Spec.assertEqWith
      s
      "all on one kills one; one each kills both"
      ( S.creaturesInPlay S.alice (S.settleSba (S.fightWith dump gs)),
        S.creaturesInPlay S.alice (S.settleSba (S.fightWith split gs))
      )
      (1, 0)
  Spec.it s "CR 510.1d blocking ONE creature is forced, and unprompted" $ do
    -- The same interpreter that would answer an illegal empty division, which
    -- assigns nothing: the Piker takes 2 anyway, so no prompt was raised.
    brigade <- S.printingOf s registry "Foriysian Brigade"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker] [brigade]
        noAssign :: Prompt.Prompt r -> r
        noAssign p = case p of
          Prompt.AssignCombatDamage {} -> Map.empty
          _ -> S.aggressiveAnswer p
    case mine of
      [a] -> Spec.assertEqWith s "the attacker took the whole 2" (S.damageOf a (S.fightWith noAssign gs)) (Just 2)
      _ -> Spec.assertFailure s "fixture should have one attacker"
  Spec.it s "CR 702.22k a banding attacker moves the blocker's division to the ACTIVE player" $ do
    -- Benalish Hero {W} 1/1 has printed banding. With it among the creatures the
    -- Brigade blocks, CR 702.22k hands alice the division that CR 510.1d would
    -- have given bob; with two plain Pikers instead, bob keeps it. Three seats
    -- are not needed here -- the two players are already on opposite sides of
    -- the declaration -- but the control board is, since both readings answer
    -- with a PlayerId either way.
    brigade <- S.printingOf s registry "Foriysian Brigade"
    benalishHero <- S.printingOf s registry "Benalish Hero"
    piker <- S.printingOf s registry "Goblin Piker"
    let (banded, bandedAttackers, _) = S.combatBoardOf [benalishHero, piker] [brigade]
        (plain, plainAttackers, _) = S.combatBoardOf [piker, piker] [brigade]
    Spec.assertEqWith
      s
      "banding inverts the chooser"
      (divisionChooser bandedAttackers banded, divisionChooser plainAttackers plain)
      ([S.alice], [S.bob])

-- `luring`, but a Lure on EVERY attacker: one requirement instance per attacker,
-- which is what makes CR 509.1c's maximum bigger than one blocker's ordinary
-- arity.
luringAll :: Printing.Printing -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
luringAll lure mine theirs =
  let (gs, ours, yours) = attacking mine theirs
      enchant g attacker = let (aura, withAura) = S.addPermanent lure S.alice g in S.attach aura attacker withAura
   in (List.foldl' enchant gs ours, ours, yours)

-- Blocks every attacker in `attackers` with every creature offered, which is
-- what aggressiveAnswer cannot do: it puts them all on the first attacker.
blockAll :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
blockAll attackers p = case p of
  Prompt.DeclareBlockers _ _ mine _ -> Map.fromList (fmap (\b -> (b, Set.fromList attackers)) mine)
  _ -> S.aggressiveAnswer p

-- Whom CR 510.1d's division was asked of, in order. A list rather than a Maybe
-- so a case that raised no prompt at all is told from one that raised it of the
-- wrong player.
divisionChooser :: [ObjectId.ObjectId] -> GameState.GameState -> [PlayerId.PlayerId]
divisionChooser attackers gs =
  let record :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
      record p = case p of
        Prompt.AssignCombatDamage _ pid _ thresholds n -> do
          State.modify' (<> [pid])
          pure $ case filter S.isCreatureRecipient (Map.keys thresholds) of
            r : _ -> Map.singleton r n
            [] -> Map.empty
        _ -> pure (blockAll attackers p)
   in snd
        ( State.runState
            ( fmap snd . Engine.runGame record gs $ do
                Combat.declareAttackers S.manaPerformer S.alice
                Combat.declareBlockers S.manaPerformer
                Damage.dealCombatDamage
            )
            []
        )

-- CR 702.111b, proved by Boggart Brute ("Creature -- Goblin Warrior 3/2,
-- Menace") -- the blocking side's SET-SHAPED combat restriction, and the first
-- evasion ability that is not a question about a (blocker, attacker) pair. Its
-- attacking counterpart is Bonded Construct's "can't attack alone", in
-- Pawl.CombatEffectSpec.
--
-- The whole group turns on the difference between "two or more creatures block
-- it" and "each creature blocking it passes some test". Flying, reach, fear and
-- landwalk are all the second kind, so they are checked in
-- Pawl.Engine.Combat.pairAllowed; menace is the first of the first kind, and is
-- checked in blockDeclarationAllowed, which sees the whole map at once. The
-- zero-blockers case below is what separates 702.111b's "can't be blocked EXCEPT
-- BY two or more" from the naive "at least two creatures must block it".
menaceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
menaceSpec s registry = Spec.describe s "Menace" $ do
  Spec.it s "CR 702.111b a declaration in which ONE creature blocks a menace attacker is illegal" $ do
    boggartBrute <- S.printingOf s registry "Boggart Brute"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [boggartBrute] [piker]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs)) "illegal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.111b a declaration in which TWO creatures block a menace attacker is legal" $ do
    -- THE FALSIFIER for reading 702.111b as "can't be blocked": the same
    -- attacker, blocked by two of the very creature that could not block it
    -- alone. The block also survives a real declare blockers step, so this is
    -- not a claim about legalBlockDeclaration alone.
    boggartBrute <- S.printingOf s registry "Boggart Brute"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [boggartBrute] [piker, piker]
    case (mine, theirs) of
      (a : _, [b, c]) -> do
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.fromList [(b, Set.singleton a), (c, Set.singleton a)]) gs) "legal"
        let after = S.runPure S.aggressiveAnswer gs (Combat.declareBlockers S.manaPerformer)
        Spec.assertEqWith s "both block" (Combat.blockersOf a after) (Set.fromList [b, c])
      _ -> Spec.assertFailure s "fixture should have an attacker and two blockers"
  Spec.it s "CR 702.111b declining to block a menace attacker is legal, on the very board where one blocker is not" $ do
    -- 702.111b says "can't be blocked EXCEPT BY two or more creatures", not
    -- "must be blocked by two or more creatures". A naive "count the blockers
    -- of each attacker and demand two" rejects the empty declaration, which is
    -- always legal under restrictions alone. Both halves are asserted on ONE
    -- board so neither can be satisfied by a different fixture.
    boggartBrute <- S.printingOf s registry "Boggart Brute"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [boggartBrute] [piker]
    case (mine, theirs) of
      (a : _, b : _) -> do
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty gs) "declining is legal"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs)) "one blocker is not"
        -- And the engine reaches that legal answer for itself: S.aggressiveAnswer
        -- blocks with everything, which here is the illegal single block, so
        -- declareBlockers falls back to the forced declaration -- which is the
        -- empty one, not "block with two" and not a repaired partial block.
        let after = S.runPure S.aggressiveAnswer gs (Combat.declareBlockers S.manaPerformer)
        Spec.assertEqWith s "nobody blocks" (Combat.blockersOf a after) Set.empty
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.111b a whole combat: one Piker cannot stop the Brute, two can" $ do
    -- The gameplay-level case, run through S.fightWith rather than asked of
    -- legalBlockDeclaration -- and it is precise rather than vacuous, because
    -- the two legs differ in every observable. WITH one blocker: nobody may
    -- block, bob takes 3, and both creatures live. WITH two: both block,
    -- bob takes 0, and the Brute (3/2) dies to 2+2 while killing the first
    -- Piker (2/1) with its 3.
    boggartBrute <- S.printingOf s registry "Boggart Brute"
    piker <- S.printingOf s registry "Goblin Piker"
    let fight theirs =
          let (gs, _, _) = S.combatBoardOf [boggartBrute] theirs
           in S.settleSba (S.fightWith S.aggressiveAnswer gs)
        one = fight [piker]
        two = fight [piker, piker]
    Spec.assertEqWith s "one blocker: bob takes 3" (S.lifeOf S.bob one) (Just 17)
    Spec.assertEqWith s "one blocker: the Brute lives" (S.creaturesInPlay S.alice one) 1
    Spec.assertEqWith s "one blocker: so does the Piker it could not block with" (S.creaturesInPlay S.bob one) 1
    Spec.assertEqWith s "two blockers: bob takes nothing" (S.lifeOf S.bob two) (Just 20)
    Spec.assertEqWith s "two blockers: the Brute dies to 2+2" (S.creaturesInPlay S.alice two) 0
    Spec.assertEqWith s "two blockers: taking one Piker with it" (S.creaturesInPlay S.bob two) 1
  Spec.it s "CR 702.111b menace constrains the set blocking ITS attacker, not every attacker" $ do
    -- The control that keeps the restriction narrow: a Piker attacking beside
    -- the Brute still takes exactly one blocker. Fails against any
    -- implementation that reads menace off the declaration as a whole rather
    -- than per attacker.
    boggartBrute <- S.printingOf s registry "Boggart Brute"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [boggartBrute, piker] [piker]
    case (mine, theirs) of
      ([brute, plain], b : _) -> do
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton plain)) gs) "one blocker on the plain attacker is legal"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton brute)) gs)) "one blocker on the menace attacker is not"
      _ -> Spec.assertFailure s "fixture should have two attackers and a blocker"
  Spec.it s "CR 509.1b menace and fear are cumulative: two blockers, and both must pass fear" $ do
    -- "Different evasion abilities are cumulative." A Boggart Brute granted
    -- fear needs TWO blockers, and each of them must be an artifact creature
    -- and/or a black creature (CR 702.36b). Typhoid Rats is black, Darksteel
    -- Myr is a colourless artifact, and the Piker is neither.
    --
    -- Fails against an implementation that lets menace REPLACE the pairwise
    -- checks (leg two would pass) or that lets a passing pair excuse the count
    -- (leg three would pass).
    boggartBrute <- S.printingOf s registry "Boggart Brute"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs0, mine, theirs) = attacking [boggartBrute] [typhoidRats, darksteelMyr, piker]
    case (mine, theirs) of
      (a : _, [rats, myr, plain]) -> do
        let gs = withFear a gs0
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.fromList [(rats, Set.singleton a), (myr, Set.singleton a)]) gs) "two fear-legal blockers"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.fromList [(rats, Set.singleton a), (plain, Set.singleton a)]) gs)) "two blockers, one of which fear forbids"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton rats (Set.singleton a)) gs)) "one fear-legal blocker is still one blocker"
      _ -> Spec.assertFailure s "fixture should have an attacker and three blockers"
  Spec.it s "CR 702.111b menace restricts being blocked, never attacking or blocking" $ do
    -- The asymmetry every evasion gate here has (see evasionAllows), stated for
    -- menace on both sides at once: the Brute attacks alone, and the Brute
    -- blocks alone.
    boggartBrute <- S.printingOf s registry "Boggart Brute"
    piker <- S.printingOf s registry "Goblin Piker"
    -- canAttack is asked BEFORE the declaration, since attacking taps the
    -- attacker (CR 508.1f) and a tapped creature fails CR 508.1a for a reason
    -- that has nothing to do with menace.
    let (before, mine, theirs) = S.combatBoardOf [boggartBrute] [boggartBrute]
        gs = snd (Engine.runGamePure S.aggressiveAnswer before (Combat.declareAttackers S.manaPerformer S.alice))
    case (mine, theirs) of
      (a : _, b : _) -> do
        Spec.assertBool s (Combat.canAttack S.alice a before) "a menace creature may attack"
        Spec.assertEqWith s "and it does" (Map.keys (Combat.Type.attackers (GameState.combat gs))) [a]
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs)) "but one creature may not block it"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
    let (gs2, mine2, theirs2) = attacking [piker] [boggartBrute]
    case (mine2, theirs2) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs2) "a menace creature blocking alone is legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 509.1c a Lured menace attacker must be blocked by BOTH creatures or by neither" $ do
    -- CR 509.1c's own worked example, in the pool's cards: "A player controls
    -- one creature that 'blocks if able' and another creature with no
    -- abilities. If a creature with menace attacks that player, the player must
    -- block with both creatures." Lure requires every able creature rather than
    -- one of them, which lands on the same answer.
    --
    -- What this proves is that the two halves of CR 509.1 compose: the
    -- maximization ranges over declarations menace ALREADY allows, so the
    -- single block is not merely worse than the double one, it is not a
    -- candidate at all. `luring` is blockRequirementSpec's helper, below.
    lure <- S.printingOf s registry "Lure"
    boggartBrute <- S.printingOf s registry "Boggart Brute"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = luring lure [boggartBrute] [piker, piker]
    case (mine, theirs) of
      (a : _, [b, c]) -> do
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty gs)) "neither blocking is illegal"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs)) "one blocking is illegal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.fromList [(b, Set.singleton a), (c, Set.singleton a)]) gs) "both blocking is legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and two blockers"
  Spec.it s "CR 509.1c a Lured menace attacker with only ONE creature to block it may go unblocked" $ do
    -- CR 509.1c maximizes over the requirements "that could be obeyed WITHOUT
    -- DISOBEYING ANY RESTRICTIONS", so menace BOUNDS the maximization rather
    -- than competing with it. The lone Piker is able to block (the requirement
    -- instance exists), but no legal declaration has it blocking, so the
    -- maximum is zero and declining attains it.
    --
    -- THE FALSIFIER for computing CR 509.1c's maximum over the pairwise-legal
    -- declarations and only then filtering by the set-shaped restriction: that
    -- order makes the maximum one, and declining illegal, with no legal answer
    -- left for the defending player to give.
    lure <- S.printingOf s registry "Lure"
    boggartBrute <- S.printingOf s registry "Boggart Brute"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = luring lure [boggartBrute] [piker]
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty gs) "no blocks is legal"
  Spec.it s "CR 509.1b gaining menace AFTER a legal block has been declared doesn't affect that block" $ do
    -- "If an attacking creature gains or loses an evasion ability after a legal
    -- block has been declared, it doesn't affect that block." One Piker blocks
    -- one Piker legally; the attacker then gains menace, which would have
    -- forbidden that block had it been there at declaration time. The block
    -- stands.
    --
    -- Both assertions are needed: the keyword one is what stops this passing
    -- vacuously against a board where menace never arrived at all.
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker] [piker]
    case (mine, theirs) of
      (a : _, b : _) -> do
        let blocked = S.runPure S.aggressiveAnswer gs (Combat.declareBlockers S.manaPerformer)
            after = withMenace a blocked
        Spec.assertBool s (Projection.hasKeyword Keyword.Menace a after) "the attacker now has menace"
        Spec.assertEqWith s "and is still blocked by the one creature" (Combat.blockersOf a after) (Set.singleton b)
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"

-- Declare attackers with everything, then put a Lure on the first attacker.
-- Attaching directly is S.attach's state-fixture posture -- Pawl.Engine.Cast can cast
-- the Aura, but a combat fixture cannot reach a sorcery-speed cast mid-step --
-- and the printing is the real Lure, never a synthetic.
luring :: Printing.Printing -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
luring lure mine theirs =
  let (gs, ours, yours) = attacking mine theirs
   in case ours of
        -- Unreachable: every caller passes at least one attacking printing.
        [] -> (gs, ours, yours)
        attacker : _ ->
          let (aura, withAura) = S.addPermanent lure S.alice gs
           in (S.attach aura attacker withAura, ours, yours)

-- CR 604.2's threshold gate, stocked: `n` copies of `printing` in ALICE's
-- graveyard, alice controlling every source these cases attach. CR 109.5 is what
-- makes that the right seat -- "your graveyard" on a static ability is the
-- current controller's.
filling :: Printing.Printing -> Int -> GameState.GameState -> GameState.GameState
filling printing n gs = List.foldl' (\g _ -> snd (S.addGraveyardCard printing S.alice g)) gs [1 .. n]

-- CR 509.1c, proved by Lure ("All creatures able to block enchanted creature do
-- so") -- the pool's first blocking REQUIREMENT, and the first board on which
-- declining to block is not a legal answer.
--
-- Prized Unicorn ("All creatures able to block this creature do so") is the second
-- carrier, and the one CR 604.2's layer-6 strip needs: it is a CREATURE, so
-- Humility's "each creature loses all abilities" reaches its requirement with no
-- animator in between.
blockRequirementSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
blockRequirementSpec s registry = Spec.describe s "BlockRequirements" $ do
  Spec.it s "CR 509.1c declining to block a Lured attacker is illegal" $ do
    -- THE FALSIFIER for a restrictions-only reading of CR 509.1: the empty
    -- declaration disobeys no restriction, which is exactly why 509.1c is a
    -- maximization and not a per-creature check.
    lure <- S.printingOf s registry "Lure"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = luring lure [piker] [piker]
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty gs)) "no blocks is illegal"
  Spec.it s "CR 509.1 the same board WITHOUT the Lure lets the defender decline" $ do
    -- The control for the test above, and the reason it is not vacuous: the
    -- empty declaration is legal here, so the Lure is what changed the answer.
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = attacking [piker] [piker]
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty gs) "no blocks is legal"
  Spec.it s "CR 509.1c blocking the Lured attacker is legal" $ do
    lure <- S.printingOf s registry "Lure"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = luring lure [piker] [piker]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs) "legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 509.1c a creature that CANNOT block the Lured attacker is not required to" $ do
    -- Lure's "able to block" doing its work: the Bird Maiden has flying (CR
    -- 702.9b), so the ground Piker could not block it under any declaration.
    -- No requirement instance exists, the maximum is zero, and declining stays
    -- legal. Fails against an implementation that requires every creature to
    -- block regardless of the restrictions.
    lure <- S.printingOf s registry "Lure"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = luring lure [birdMaiden] [piker]
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty gs) "no blocks is legal"
  Spec.it s "CR 509.1a a TAPPED creature is not able to block, so a Lure does not require it" $ do
    -- The other half of "able": CR 509.1a's chosen creatures "must be
    -- untapped", so a tapped creature is never a candidate and carries no
    -- requirement.
    lure <- S.printingOf s registry "Lure"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, theirs) = luring lure [piker] [piker]
    case theirs of
      b : _ -> Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty (S.tapObject b gs)) "no blocks is legal"
      _ -> Spec.assertFailure s "fixture should have a blocker"
  Spec.it s "CR 509.1c the maximum is over the creatures that CAN block, not all of them" $ do
    -- The maximization biting. A Lured Bird Maiden (flying) is attacking; bob
    -- has a ground Piker, which may not block it, and a Nimble Birdsticker,
    -- which has reach and may. The maximum obtainable without disobeying a
    -- restriction is ONE, and only the Birdsticker's block attains it: the
    -- empty declaration obeys zero and is illegal, and the Piker's block is
    -- illegal under CR 702.9b whatever it would obey.
    lure <- S.printingOf s registry "Lure"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    piker <- S.printingOf s registry "Goblin Piker"
    nimbleBirdsticker <- S.printingOf s registry "Nimble Birdsticker"
    let (gs, mine, theirs) = luring lure [birdMaiden] [piker, nimbleBirdsticker]
    case (mine, theirs) of
      (a : _, [ground, reacher]) -> do
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty gs)) "no blocks is illegal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton reacher (Set.singleton a)) gs) "the reach creature blocking is legal"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton ground (Set.singleton a)) gs)) "the ground creature blocking is illegal"
      _ -> Spec.assertFailure s "fixture should have an attacker and two blockers"
  Spec.it s "CR 509.1c with two able creatures BOTH are required to block" $ do
    -- One Lure over two creatures is TWO requirements, not one -- CR 509.1c
    -- checks "each creature they control". A single block obeys one of two
    -- and is illegal; blocking with both attains the maximum.
    lure <- S.printingOf s registry "Lure"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = luring lure [piker] [piker, piker]
    case (mine, theirs) of
      (a : _, [first, second]) -> do
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton first (Set.singleton a)) gs)) "one blocker is not enough"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.fromList [(first, Set.singleton a), (second, Set.singleton a)]) gs) "both blockers is legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and two blockers"
  Spec.it s "CR 509.1c whole cards: a Lure forces a block through a real declare blockers step" $ do
    -- The gameplay-level case, run through Combat.declareBlockers with an
    -- interpreter that declines to block. Declining is now an illegal answer,
    -- and the maximum leaves exactly one legal declaration -- the rules
    -- forcing it, not the engine choosing.
    --
    -- Precise rather than vacuous, and all three assertions distinguish the
    -- two worlds. WITHOUT the requirement: nobody blocks, bob takes 2 and both
    -- Pikers live. WITH it: the Piker blocks, bob takes nothing, and the two
    -- 2/1s trade.
    lure <- S.printingOf s registry "Lure"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker] [piker]
        declining :: Prompt.Prompt r -> r
        declining p = case p of
          Prompt.DeclareBlockers {} -> Map.empty
          _ -> S.aggressiveAnswer p
        withLure = case mine of
          -- Unreachable: the fixture has one attacking printing.
          [] -> gs
          a : _ -> let (aura, withAura) = S.addPermanent lure S.alice gs in S.attach aura a withAura
        after = S.settleSba (S.fightWith declining withLure)
    Spec.assertEqWith s "bob took nothing" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "alice's attacker is dead" (S.creaturesInPlay S.alice after) 0
    Spec.assertEqWith s "bob's blocker is dead" (S.creaturesInPlay S.bob after) 0
  Spec.it s "CR 509.1c declining to block a Prized Unicorn is illegal" $ do
    -- The pool's second blocking requirement, and the first that names its OWN
    -- SOURCE rather than an attachment: "all creatures able to block THIS
    -- CREATURE do so" is Affected.Matching Filter.IsSource, matched against the
    -- attacker's identity. No Aura and no animator anywhere -- the requirement
    -- rides on a creature card. Fails against an implementation that only ever
    -- resolves an attachment, which is what Lure beside it prints.
    prizedUnicorn <- S.printingOf s registry "Prized Unicorn"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [prizedUnicorn] [piker]
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty gs)) "no blocks is illegal"
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs) "blocking the Unicorn is legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 509.1c a Prized Unicorn does not lure the OTHER attacker alongside it" $ do
    -- IsSource is an identity test, not "every attacker this permanent
    -- controls": with a Piker attacking beside the Unicorn, blocking the Piker
    -- obeys nothing and the maximum is still attained only by blocking the
    -- Unicorn. Fails against an implementation that mints a requirement per
    -- attacker rather than per matching attacker.
    prizedUnicorn <- S.printingOf s registry "Prized Unicorn"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [prizedUnicorn, piker] [piker]
    case (mine, theirs) of
      ([unicorn, other], b : _) -> do
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton unicorn)) gs) "blocking the Unicorn is legal"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton other)) gs)) "blocking the other attacker instead is illegal"
      _ -> Spec.assertFailure s "fixture should have two attackers and a blocker"
  Spec.it s "CR 509.1c a Gaea's Protector is one requirement over three able blockers, not three" $ do
    -- THE DISCRIMINATOR between the two readings of a sentence naming several
    -- creatures. "This creature must be blocked if able" is ONE requirement,
    -- obeyed by any single blocker; Lure's is one PER creature, obeyed only by
    -- all of them. Three able blockers is what tells them apart -- with one
    -- able blocker both readings force the same declaration.
    --
    -- The one-blocker declaration's LEGALITY is the quantity that separates
    -- them: the Lure reading makes it illegal. Declining is illegal under both,
    -- so it proves only that a requirement exists.
    --
    -- The control is the same board with a plain Goblin Piker attacking, which
    -- is what keeps "declining is illegal" from passing for a reason other than
    -- the requirement.
    gaeasProtector <- S.printingOf s registry "Gaea's Protector"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [gaeasProtector] [piker, piker, piker]
        (control, _, _) = attacking [piker] [piker, piker, piker]
    case (mine, theirs) of
      (a : _, [first, second, third]) -> do
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton first (Set.singleton a)) gs) "ONE blocker attains CR 509.1c's maximum"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty gs)) "declining is illegal"
        Spec.assertBool
          s
          (Combat.legalBlockDeclaration S.bob (Map.fromList [(first, Set.singleton a), (second, Set.singleton a), (third, Set.singleton a)]) gs)
          "and blocking with all three is legal too, the maximum being one"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty control) "without the requirement, declining is legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and three blockers"
  Spec.it s "CR 509.1c a Gaea's Protector nobody is able to block requires nothing" $ do
    -- The "if able" of "must be blocked if able", on the clause CR 509.1a
    -- states first: a tapped creature is never a candidate, so the group has no
    -- member and raises the maximum by nothing. A REGRESSION FENCE rather than
    -- a proof -- the mechanism is the `candidates` list every requirement above
    -- is already narrowed by -- and the pair of boards is here because the
    -- group is a new reader of it.
    gaeasProtector <- S.printingOf s registry "Gaea's Protector"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, theirs) = attacking [gaeasProtector] [piker]
    case theirs of
      b : _ -> do
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty gs)) "untapped, declining is illegal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty (S.tapObject b gs)) "tapped, declining is legal"
      _ -> Spec.assertFailure s "fixture should have a blocker"
  Spec.it s "CR 509.1c a Lure and a Gaea's Protector attacking together compose" $ do
    -- The composition board for the two arities, and the reason the group is a
    -- term in ONE maximization rather than a check of its own. Three blockers,
    -- a Lured Piker and the Protector: the Lure is three requirements on its
    -- own pairs and the Protector one over all three blockers, so the maximum
    -- is three and TWO declarations attain it -- all three on the Lured Piker,
    -- or two there and one on the Protector.
    --
    -- A group read as a check ("the Protector must be blocked, full stop")
    -- calls the all-three-on-the-Lure declaration illegal; one read as a
    -- per-pair weight calls the two-and-one declaration illegal. Both are
    -- asserted legal, and the two declarations that obey fewer than three are
    -- asserted illegal.
    lure <- S.printingOf s registry "Lure"
    gaeasProtector <- S.printingOf s registry "Gaea's Protector"
    piker <- S.printingOf s registry "Goblin Piker"
    let (board, mine, theirs) = attacking [gaeasProtector, piker] [piker, piker, piker]
    case (mine, theirs) of
      ([protector, lured], [first, second, third]) -> do
        let (aura, withAura) = S.addPermanent lure S.alice board
            gs = S.attach aura lured withAura
            onLure = Set.singleton lured
            onProtector = Set.singleton protector
        Spec.assertBool
          s
          (Combat.legalBlockDeclaration S.bob (Map.fromList [(first, onLure), (second, onLure), (third, onLure)]) gs)
          "all three on the Lured attacker obeys three"
        Spec.assertBool
          s
          (Combat.legalBlockDeclaration S.bob (Map.fromList [(first, onLure), (second, onLure), (third, onProtector)]) gs)
          "and so does two there and one on the Protector"
        Spec.assertBool
          s
          (not (Combat.legalBlockDeclaration S.bob (Map.fromList [(first, onLure), (second, onProtector), (third, onProtector)]) gs))
          "one on the Lure and two on the Protector obeys two and is illegal"
        Spec.assertBool
          s
          (not (Combat.legalBlockDeclaration S.bob (Map.fromList [(first, onProtector), (second, onProtector), (third, onProtector)]) gs))
          "and all three on the Protector obeys one"
      _ -> Spec.assertFailure s "fixture should have two attackers and three blockers"
  Spec.it s "CR 509.1c whole cards: a Gaea's Protector forces a block through a real declare blockers step" $ do
    -- The gameplay-level case, run through Combat.declareBlockers with an
    -- interpreter that declines to block, and a pair of boards differing in ONE
    -- thing: whether bob's Piker is untapped.
    --
    -- WITH the block forced: bob takes nothing, and the 4/2 and the 2/1 trade.
    -- WITHOUT it (the Piker tapped, so no creature is able): bob takes four and
    -- both creatures live.
    gaeasProtector <- S.printingOf s registry "Gaea's Protector"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, theirs) = attacking [gaeasProtector] [piker]
        declining :: Prompt.Prompt r -> r
        declining p = case p of
          Prompt.DeclareBlockers {} -> Map.empty
          _ -> S.aggressiveAnswer p
    case theirs of
      b : _ -> do
        let after = S.settleSba (S.fightWith declining gs)
            control = S.settleSba (S.fightWith declining (S.tapObject b gs))
        Spec.assertEqWith s "bob took nothing" (S.lifeOf S.bob after) (Just 20)
        Spec.assertEqWith s "the Protector died to the block it forced" (S.creaturesInPlay S.alice after) 0
        Spec.assertEqWith s "and so did the blocker" (S.creaturesInPlay S.bob after) 0
        Spec.assertEqWith s "with nobody able to block, bob took four" (S.lifeOf S.bob control) (Just 16)
        Spec.assertEqWith s "and the Protector lived" (S.creaturesInPlay S.alice control) 1
      _ -> Spec.assertFailure s "fixture should have a blocker"
  Spec.it s "CR 604.2 Humility strips a Prized Unicorn's block requirement, so declining becomes legal" $ do
    -- CR 604.2: a static ability's continuous effect is active only while the
    -- permanent "remains on the battlefield AND HAS THE ABILITY", so Humility's
    -- CR 613.1f layer-6 LoseAllAbilities takes the requirement with it. Both
    -- worlds are asserted on ONE board so the pair cannot drift: without
    -- Humility declining is illegal, with it the empty declaration becomes a
    -- legal answer. Fails against an implementation that reads
    -- Face.blockRequirements off the printed card.
    --
    -- The third assertion is what keeps the second from passing vacuously: the
    -- combat is still live under Humility -- the Unicorn is still attacking and
    -- the (now 1/1) Piker is still able to block it -- so declining became legal
    -- because the requirement went away, not because there was nothing to block.
    prizedUnicorn <- S.printingOf s registry "Prized Unicorn"
    piker <- S.printingOf s registry "Goblin Piker"
    humility <- S.printingOf s registry "Humility"
    let (gs, mine, theirs) = attacking [prizedUnicorn] [piker]
        underHumility = S.withHumility humility gs
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty gs)) "without Humility, no blocks is illegal"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty underHumility) "under Humility, no blocks is legal"
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) underHumility) "and blocking is still legal, so the combat is still live"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 303.4m a Lure that is not attached to anything requires nothing" $ do
    -- CR 303.4m reads the SOURCE's attachment, so an unattached Lure names no
    -- attacker and mints no requirement. The Aura stays ON the battlefield
    -- throughout, so this is not a test that removing it works -- CR 704.5m
    -- would bury it, and no state-based-action pass is run here.
    lure <- S.printingOf s registry "Lure"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = attacking [piker] [piker]
        withAura = snd (S.addPermanent lure S.alice gs)
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty withAura) "no blocks is legal"
  -- CR 509.1c's SUBJECT axis, and the SUBJECTLESS shape: Razorgrass Screen ({1}
  -- Artifact Creature -- Wall 2/1, "Defender. This creature blocks each combat if
  -- able." -- checked against Scryfall, 2026-08-16) prints a requirement on
  -- ITSELF that names no attacker, where every card above prints an attacker and
  -- leaves the subject at "all creatures able to".
  Spec.it s "CR 509.1c a Razorgrass Screen must block, though nothing names an attacker" $ do
    -- The requirement mints one pair per attacker the Screen may block, so
    -- declining obeys zero and is illegal.
    screen <- S.printingOf s registry "Razorgrass Screen"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker] [screen]
    case (mine, theirs) of
      (a : _, [wall]) -> do
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty gs)) "no blocks is illegal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton wall (Set.singleton a)) gs) "the Screen blocking is legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a Screen"
  Spec.it s "CR 509.1 the same board with a plain Wall lets the defender decline" $ do
    -- The control, and the reason the case above is not vacuous: swap the Screen
    -- for a defender creature carrying no requirement and declining is legal
    -- again.
    ogreSentry <- S.printingOf s registry "Ogre Sentry"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = attacking [piker] [ogreSentry]
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty gs) "no blocks is legal"
  Spec.it s "CR 509.1c the SUBJECT axis: only the Screen is required, not everything bob controls" $ do
    -- THE AXIS UNDER TEST. Lure's requirement is over all creatures able; this
    -- one is over ITSELF, so a Piker beside the Screen carries none and cannot
    -- attain the maximum in the Screen's place. Fails against a reader that
    -- ignores the subject field.
    screen <- S.printingOf s registry "Razorgrass Screen"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker] [screen, piker]
    case (mine, theirs) of
      (a : _, [wall, bystander]) -> do
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton wall (Set.singleton a)) gs) "the Screen alone attains the maximum"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton bystander (Set.singleton a)) gs)) "the Piker blocking instead does not"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty gs)) "and declining does not"
      _ -> Spec.assertFailure s "fixture should have a Screen and a bystander"
  Spec.it s "CR 509.1c 'if able': an attacker the Screen cannot block requires nothing" $ do
    -- The `able` prune on the new axis. Bird Maiden has flying (CR 702.9b) and
    -- the Screen has no reach, so no pair is minted, the maximum is zero and
    -- declining is legal.
    screen <- S.printingOf s registry "Razorgrass Screen"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    let (gs, _, _) = attacking [birdMaiden] [screen]
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty gs) "no blocks is legal"
  Spec.it s "CR 509.1a two attackers, one Screen: blocking EITHER attains the maximum" $ do
    -- "each combat", not "each attacker" -- the assertion that pins the absent
    -- attacker axis. Two pairs are minted, but CR 509.1a lets the Screen block
    -- one attacker, so the maximum is one and both single blocks are legal while
    -- declining is not.
    screen <- S.printingOf s registry "Razorgrass Screen"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker, piker] [screen]
    case (mine, theirs) of
      ([first, second], [wall]) -> do
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton wall (Set.singleton first)) gs) "blocking the first is legal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton wall (Set.singleton second)) gs) "so is blocking the second"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty gs)) "declining is not"
      _ -> Spec.assertFailure s "fixture should have two attackers and a Screen"
  Spec.it s "CR 509.1c two requirements on ONE pair count twice" $ do
    -- CR 509.1c counts REQUIREMENTS being obeyed, not the (blocker, attacker)
    -- pairs they name. The board above, plus a Lure on the SECOND attacker:
    --
    --   Screen's "blocks each combat if able"  -> (Screen, first), (Screen, second)
    --   Lure on the second attacker            -> (Screen, second)
    --
    -- so (Screen, second) carries TWO requirements and (Screen, first) one. CR
    -- 509.1a caps the Screen at one attacker, so the maximum obtainable is two
    -- and only blocking the Lured attacker attains it.
    --
    -- The Lure goes on the SECOND attacker deliberately. Ties in
    -- blockCeilingGiven's fold go to the earlier declaration in enumeration
    -- order, so under a pair-counting reading -- where both blocks obey one --
    -- the forced declaration names the FIRST attacker. Putting the Lure last
    -- makes the LAST assertion discriminate too; on the first attacker it would
    -- agree with both readings.
    --
    -- Both boards are built here rather than leaning on the case above, so the
    -- pair cannot drift: `plain` and `lured` differ in the Lure and nothing
    -- else. Blocking the second attacker is legal on BOTH (it obeys the
    -- maximum either way), which is the anti-vacuity leg -- the Screen really
    -- is an able blocker of both attackers, so the illegality below is the
    -- count and not an unrelated CR 509.1b refusal.
    screen <- S.printingOf s registry "Razorgrass Screen"
    piker <- S.printingOf s registry "Goblin Piker"
    lure <- S.printingOf s registry "Lure"
    let (plain, mine, theirs) = attacking [piker, piker] [screen]
    case (mine, theirs) of
      ([first, second], [wall]) -> do
        let (aura, withAura) = S.addPermanent lure S.alice plain
            lured = S.attach aura second withAura
            blocks attacker = Map.singleton wall (Set.singleton attacker)
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (blocks first) plain) "without the Lure, blocking the first attacker is legal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (blocks second) plain) "and so is blocking the second"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (blocks second) lured) "with the Lure, blocking the Lured attacker obeys both requirements"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (blocks first) lured)) "but blocking the plain one obeys only one, so it is illegal"
        Spec.assertEqWith s "and the forced declaration names the Lured attacker" (Combat.forcedBlockDeclaration S.bob lured) (blocks second)
      _ -> Spec.assertFailure s "fixture should have two attackers and a Screen"
  Spec.it s "CR 702.3b the Screen still can't attack" $ do
    -- The card's other line, and the control that keeps the new axis from being
    -- read as a permission to attack.
    screen <- S.printingOf s registry "Razorgrass Screen"
    let (gs, mine, _) = S.combatBoardOf [screen] []
    case mine of
      [wall] -> Spec.assertBool s (not (Combat.canAttack S.alice wall gs)) "defender forbids the attack"
      _ -> Spec.assertFailure s "fixture should have one creature"
  Spec.it s "CR 509.1c whole cards: the Screen blocks a real declare blockers step" $ do
    -- Gameplay level, under an answerer that DECLINES, so the block is CR
    -- 509.1c's degradation to the forced declaration rather than the answerer
    -- choosing it. The control board swaps the Screen for an Ogre Sentry, which
    -- is a defender with no requirement, so the two boards differ only in the
    -- requirement and every assertion below differs between them.
    screen <- S.printingOf s registry "Razorgrass Screen"
    piker <- S.printingOf s registry "Goblin Piker"
    ogreSentry <- S.printingOf s registry "Ogre Sentry"
    let declining :: Prompt.Prompt r -> r
        declining p = case p of
          Prompt.DeclareBlockers {} -> Map.empty
          _ -> S.aggressiveAnswer p
        run theirs = S.settleSba (S.fightWith declining (let (gs, _, _) = S.combatBoardOf [piker] theirs in gs))
        blocked = run [screen]
        unblocked = run [ogreSentry]
    Spec.assertEqWith s "the Screen was forced to block, so bob took nothing" (S.lifeOf S.bob blocked) (Just 20)
    Spec.assertEqWith s "and the 2/1 Piker killed the 2/1 Screen" (S.creaturesInPlay S.bob blocked) 0
    Spec.assertEqWith s "an Ogre Sentry with no requirement declines, so bob takes two" (S.lifeOf S.bob unblocked) (Just 18)
    Spec.assertEqWith s "and it survives, having blocked nothing" (S.creaturesInPlay S.bob unblocked) 1
  -- CR 509.1c's CONDITION -- "or that it must block if some condition is met" --
  -- which every card above leaves absent. Seton's Desire ({2}{G} Enchantment --
  -- Aura, "Enchant creature. Enchanted creature gets +2/+2. Threshold -- As long
  -- as there are seven or more cards in your graveyard, all creatures able to
  -- block enchanted creature do so." -- checked against Scryfall, 2026-09-06) is
  -- Lure's sentence behind CR 604.2's "as long as" clause, and the clause is the
  -- one Otarian Juggernaut prints on the attacking side.
  --
  -- The printings that word the gate on themselves instead -- The Masamune,
  -- Ace's Baseball Bat, Enkira, Hostile Scavenger and Frodo Baggins -- all say
  -- "must be BLOCKED if able", which is the gate beside
  -- Pawl.Types.RequirementArity.AnySubject rather than beside Lure's arity.
  -- Gaea's Protector is what proves that arity; Seton's Desire is what proves
  -- the gate, and no printing in data/cards/ yet states both at once.
  --
  -- The two boards differ in ONE thing, the number of cards in alice's
  -- graveyard, and the threshold falls between them -- the pair
  -- Pawl.CombatCostSpec's conditionalAttackRequirementSpec builds for the same
  -- clause on the attacking side.
  Spec.it s "CR 509.1c a threshold blocking requirement bites only once the gate holds" $ do
    -- THE AXIS UNDER TEST. The second assertion is what keeps the first from
    -- passing vacuously: the Piker really is an able blocker under the
    -- threshold, so declining is legal because the gate is false and not because
    -- there is nothing to block.
    desire <- S.printingOf s registry "Seton's Desire"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = luring desire [piker] [piker]
    case (mine, theirs) of
      (a : _, b : _) -> do
        let under = filling piker 6 gs
            over = filling piker 7 gs
            blocks = Map.singleton b (Set.singleton a)
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty under) "six cards in the graveyard: declining is legal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob blocks under) "and blocking is still legal, so the combat is live"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty over)) "seven cards: the gate holds and declining is illegal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob blocks over) "blocking the enchanted attacker is legal"
        Spec.assertEqWith s "and the ceiling counts the requirement, so the forced declaration is that block" (Combat.forcedBlockDeclaration S.bob over) blocks
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 509.1c the gated requirement still names only the enchanted attacker" $ do
    -- The object axis under the gate: a Piker attacking beside the enchanted one
    -- carries no requirement, so blocking it instead obeys nothing however full
    -- the graveyard is. Fails against a reader that lets a holding gate lure
    -- every attacker.
    desire <- S.printingOf s registry "Seton's Desire"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = luring desire [piker, piker] [piker]
    case (mine, theirs) of
      ([enchanted, other], b : _) -> do
        let over = filling piker 7 gs
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton enchanted)) over) "blocking the enchanted attacker is legal"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton other)) over)) "blocking the other attacker instead is illegal"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty over)) "and declining is illegal"
      _ -> Spec.assertFailure s "fixture should have two attackers and a blocker"
  Spec.it s "CR 509.1c whole cards: the threshold forces a block through a real declare blockers step" $ do
    -- Gameplay level, under an answerer that DECLINES. Both boards carry the
    -- Aura -- so its +2/+2 is on both and the 4/3 attacker is the same creature
    -- either way -- and differ only in the graveyard, and both assertions
    -- differ between them: over the threshold the Piker is forced to block and
    -- dies while bob takes nothing; under it nobody blocks, bob takes 4 and the
    -- blocker lives.
    desire <- S.printingOf s registry "Seton's Desire"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker] [piker]
        declining :: Prompt.Prompt r -> r
        declining p = case p of
          Prompt.DeclareBlockers {} -> Map.empty
          _ -> S.aggressiveAnswer p
        enchanted = case mine of
          -- Unreachable: the fixture has one attacking printing.
          [] -> gs
          a : _ -> let (aura, withAura) = S.addPermanent desire S.alice gs in S.attach aura a withAura
        run n = S.settleSba (S.fightWith declining (filling piker n enchanted))
        blocked = run 7
        unblocked = run 6
    Spec.assertEqWith s "seven cards in the graveyard: the Piker was forced to block, so bob took nothing" (S.lifeOf S.bob blocked) (Just 20)
    Spec.assertEqWith s "and the blocker died to the 4/3" (S.creaturesInPlay S.bob blocked) 0
    Spec.assertEqWith s "six cards: the gate is false and bob takes four" (S.lifeOf S.bob unblocked) (Just 16)
    Spec.assertEqWith s "and the blocker survives, having blocked nothing" (S.creaturesInPlay S.bob unblocked) 1

-- A combat board that has NOT yet declared attackers, with Curse of the Nightly
-- Hunt on the battlefield attached to `who`. The attacking twin of `luring`, and
-- the two differ exactly where the rules do: a blocking requirement is checked
-- after attackers exist, an attacking one before.
--
-- The Curse goes on the ACTIVE player, which is where it bites: "creatures
-- enchanted player controls attack each combat if able" says nothing until the
-- enchanted player has a declare attackers step of their own. Its controller is
-- alice in every case below and never matters -- CR 508.1d asks the active player
-- about their own creatures, not about whose ability is talking.
cursing :: Printing.Printing -> PlayerId.PlayerId -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
cursing curse who mine theirs =
  let (gs, ours, yours) = S.combatBoardOf mine theirs
   in (cursingBoard curse who gs, ours, yours)

-- The same Curse, attached to a board that already exists. What `cursing` is
-- built from, and what a board `cursing` cannot build -- one whose planeswalker
-- needs its loyalty counters placed first -- reaches for instead.
cursingBoard :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
cursingBoard curse who gs =
  let (aura, withAura) = S.addPermanent curse S.alice gs
   in S.attachTo aura (Recipient.ToPlayer who) withAura

-- CR 508.1d, proved by Curse of the Nightly Hunt ("Creatures enchanted player
-- controls attack each combat if able") -- the pool's first attacking REQUIREMENT,
-- and the first board on which declining to attack is not a legal answer.
--
-- The requirement sits ON TOP of CR 508.1a rather than beside it: "if able" is
-- Pawl.Engine.Combat.legalAttackers, so a creature that could not have attacked anyway
-- carries no requirement and cannot make declining illegal. Half the group is
-- that half.
attackRequirementSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
attackRequirementSpec s registry = Spec.describe s "AttackRequirements" $ do
  Spec.it s "CR 508.1d declining to attack under a Curse of the Nightly Hunt is illegal" $ do
    -- THE FALSIFIER for a restrictions-only reading of CR 508.1: the empty
    -- declaration disobeys no restriction, which is exactly why 508.1d is a
    -- maximization and not a per-creature check.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = cursing curse S.alice [piker] []
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] gs)) "no attack is illegal"
  Spec.it s "CR 508.1 the same board WITHOUT the Curse lets the active player decline" $ do
    -- The control for the test above, and the reason it is not vacuous:
    -- attacking is optional by default (CR 508.1a chooses "which creatures,
    -- IF ANY"), so the Curse is what changed the answer.
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [piker] []
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] gs) "no attack is legal"
  Spec.it s "CR 508.1d attacking with the required creature is legal" $ do
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = cursing curse S.alice [piker] []
    case mine of
      a : _ -> Spec.assertBool s (Combat.legalAttackDeclaration S.alice [a] gs) "attacking is legal"
      _ -> Spec.assertFailure s "fixture should have a creature"
  Spec.it s "CR 303.4m the Curse requires the ENCHANTED player's creatures, not the active player's" $ do
    -- Affected.AttachedPlayerControls read for the wrong player is the bug
    -- this catches: with the Curse on bob, alice's creatures are outside its
    -- set entirely and she may still decline. bob's own creatures are not a
    -- second requirement either -- CR 508.1a's candidates are the ACTIVE
    -- player's, so a nonactive player's creature is never "able" to attack.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = cursing curse S.bob [piker] [piker]
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] gs) "no attack is legal"
  Spec.it s "CR 508.1a a TAPPED creature is not able to attack, so the Curse does not require it" $ do
    -- "If able" doing its work, on the clause CR 508.1a states first: the
    -- chosen creatures "must be untapped", so a tapped one is never a
    -- candidate and carries no requirement.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = cursing curse S.alice [piker] []
    case mine of
      a : _ -> Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] (S.tapObject a gs)) "no attack is legal"
      _ -> Spec.assertFailure s "fixture should have a creature"
  Spec.it s "CR 302.6 a summoning sick creature is not able to attack, so the Curse does not require it" $ do
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = cursing curse S.alice [piker] []
    case mine of
      a : _ ->
        let sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) a (GameState.objects gs)}
         in Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] sick) "no attack is legal"
      _ -> Spec.assertFailure s "fixture should have a creature"
  Spec.it s "CR 702.3b a Wall of Stone is not required to attack, but the Piker beside it is" $ do
    -- Defender is the one printed CR 508.1c restriction in the pool, and it
    -- reaches the requirement through the same candidate list. Both creatures
    -- on ONE board, so a blanket "nothing is required" bug cannot pass: the
    -- Piker alone attains the maximum, and the Wall neither adds to it nor is
    -- allowed to attack.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = cursing curse S.alice [wallOfStone, piker] []
    case mine of
      [wall, p] -> do
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] gs)) "no attack is illegal"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [p] gs) "the Piker alone is legal"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [wall, p] gs)) "the Wall may not attack at all"
      _ -> Spec.assertFailure s "fixture should have a Wall and a Piker"
  Spec.it s "CR 508.1d with two able creatures BOTH are required to attack" $ do
    -- One Curse over two creatures is TWO requirements, not one -- CR 508.1d
    -- checks "each creature they control". Attacking with one obeys one of
    -- two and is illegal; attacking with both attains the maximum.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = cursing curse S.alice [piker, piker] []
    case mine of
      [first, second] -> do
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [first] gs)) "one attacker is not enough"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [first, second] gs) "both attackers is legal"
      _ -> Spec.assertFailure s "fixture should have two creatures"
  Spec.it s "CR 303.4m a Curse that is not attached to anything requires nothing" $ do
    -- CR 303.4m reads the SOURCE's attachment, so an unattached Curse names no
    -- player and mints no requirement. The Aura stays ON the battlefield
    -- throughout, so this is not a test that removing it works -- CR 704.5m
    -- would bury it, and no state-based-action pass is run here.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [piker] []
        withAura = snd (S.addPermanent curse S.alice gs)
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] withAura) "no attack is legal"
  Spec.it s "CR 508.1d a Seeker of Slaanesh requires ONE attacker from the opponent whose turn it is" $ do
    -- The attacking twin of the Gaea's Protector case: "each opponent must
    -- attack with at least one creature each combat if able" is ONE requirement
    -- over every creature that opponent controls, obeyed by attacking with any
    -- one of them, where Curse of the Nightly Hunt beside it is one per
    -- creature and obeyed only by attacking with all of them.
    --
    -- THREE SEATS, because "each opponent" collapses onto one in a two-player
    -- game. bob prints the Seeker; alice is the opponent declaring attackers,
    -- and carol is the second opponent, whose Piker matches the Seeker's
    -- subject clause and could attack on HER turn. CR 508.1a's candidates are
    -- the ACTIVE player's, so carol's creature is outside the group and cannot
    -- obey alice's requirement -- which is what the vacuous board below
    -- asserts, carol's Piker being untouched between the two.
    --
    -- TWO creatures for alice, and that is what tells the arities apart: the
    -- per-creature reading makes attacking with exactly one of them illegal,
    -- where this one makes it the maximum. Declining is illegal under both, so
    -- it proves only that a requirement exists.
    --
    -- The pair of boards differs in ONE thing: whether alice's two creatures
    -- are Goblin Pikers or Walls of Stone, whose CR 702.3b defender keeps them
    -- off the candidate list entirely. That is the "if able", and with no
    -- member the group raises CR 508.1d's maximum by nothing.
    seeker <- S.printingOf s registry "Seeker of Slaanesh"
    piker <- S.printingOf s registry "Goblin Piker"
    wallOfStone <- S.printingOf s registry "Wall of Stone"
    let reach = S.runToStep (Phase.Combat CombatStep.DeclareAttackers) S.identityAnswer
        (bound, mine, _, _) = S.threePlayerCombat [piker, piker] [seeker] [piker]
        (vacuous, _, _, _) = S.threePlayerCombat [wallOfStone, wallOfStone] [seeker] [piker]
        boundAt = reach bound
        vacuousAt = reach vacuous
    case mine of
      [first, second] -> do
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [first] boundAt) "ONE attacker attains CR 508.1d's maximum"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] boundAt)) "declining to attack is illegal"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [first, second] boundAt) "and attacking with both is legal too, the maximum being one"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] vacuousAt) "with nothing able to attack, declining is legal again"
        -- The other reader of the ceiling, and the one an interpreter that
        -- repeats a rewound declaration lands on: the witness declaration keeps
        -- the pinned announcement the group's maximum was measured through, so
        -- it names ONE of the two Pikers rather than both or neither.
        let offered = Combat.legalAttackers S.alice boundAt
        Spec.assertEqWith
          s
          "and the forced declaration names one of them"
          (length (Combat.forcedAttackDeclaration (Combat.attackCeiling offered boundAt) offered))
          1
      _ -> Spec.assertFailure s "fixture should have two creatures for alice"
  Spec.it s "CR 508.1d whole cards: a Curse forces an attack through a real declare attackers step" $ do
    -- The gameplay-level case, run through Engine.runStep -- the priority loop
    -- and the CR 703.4i turn-based action, not a direct call -- with an
    -- interpreter that declines to attack. Declining is now an illegal answer,
    -- and the maximum leaves the rules forcing the attack rather than the
    -- engine choosing it.
    --
    -- Precise rather than vacuous, and both worlds are asserted. WITHOUT the
    -- Curse the declining interpreter attacks with nothing and bob stays at
    -- 20; WITH it the Piker attacks, taps, and bob takes 2.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = cursing curse S.alice [piker] []
        (plain, _, _) = S.combatBoardOf [piker] []
        declining :: Prompt.Prompt r -> r
        declining p = case p of
          Prompt.DeclareAttackers {} -> []
          _ -> S.aggressiveAnswer p
        after = S.runCombat declining gs
        control = S.runCombat declining plain
    Spec.assertEqWith s "without the Curse, bob takes nothing" (S.lifeOf S.bob control) (Just 20)
    Spec.assertEqWith s "with it, bob takes two" (S.lifeOf S.bob after) (Just 18)
    case mine of
      a : _ -> do
        Spec.assertEqWith s "and the creature really was declared" (S.attackerDeclarationsOf after) [a]
        -- CR 508.1f: declaring taps it. The forced declaration is a real one,
        -- not a bookkeeping entry.
        Spec.assertEqWith s "and tapped" (tapStateOf a after) (Just TapState.Tapped)
      _ -> Spec.assertFailure s "fixture should have a creature"

-- Put a Pacifism onto the battlefield under alice's control and attach it to
-- `host`. Attaching directly is `luring`'s state-fixture posture, for the same
-- reason -- Pawl.Engine.Cast can cast the Aura, but a combat fixture cannot reach a
-- sorcery-speed cast mid-step -- and the printing is the real Pacifism.
--
-- The Aura's id comes back alongside the board, which `luring` and `cursing` do
-- not need: one case below removes it to watch the restriction lift. alice
-- controls it in every case, even when it sits on bob's blocker, and that never
-- matters -- CR 508.1c and CR 509.1b ask the declaring player about their own
-- creatures, not about whose ability is talking.
pacifying :: Printing.Printing -> ObjectId.ObjectId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
pacifying pacifism host gs =
  let (aura, withAura) = S.addPermanent pacifism S.alice gs
   in (aura, S.attach aura host withAura)

-- CR 508.1c and CR 509.1b, proved by Pacifism ("Enchanted creature can't attack
-- or block") -- the pool's first printed combat RESTRICTION that is not CR
-- 702.3b's defender keyword, and the first card that prints both sides of the
-- pair at once.
--
-- A restriction is not a requirement turned around. CR 508.1d and CR 509.1c
-- maximize over the requirements "that could be obeyed WITHOUT DISOBEYING ANY
-- RESTRICTIONS", so a restriction bounds the maximization rather than competing
-- with it; the two interaction cases below are what state that, and they are the
-- cases a "requirements win" implementation gets wrong.
combatRestrictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
combatRestrictionSpec s registry = Spec.describe s "CombatRestrictions" $ do
  Spec.it s "CR 508.1c an enchanted creature can't attack, and the Piker beside it still can" $ do
    -- Both creatures on ONE board, so a blanket "nothing may attack" bug cannot
    -- pass: the restriction has to be narrow to the enchanted creature.
    pacifism <- S.printingOf s registry "Pacifism"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker, piker] []
    case mine of
      [pacified, other] -> do
        let board = snd (pacifying pacifism pacified gs)
        Spec.assertBool s (not (Combat.canAttack S.alice pacified board)) "the enchanted creature cannot attack"
        Spec.assertBool s (Combat.canAttack S.alice other board) "the one beside it can"
        Spec.assertEqWith s "and only that one is offered" (Combat.legalAttackers S.alice board) [other]
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [pacified] board)) "declaring the enchanted creature is illegal"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [other] board) "declaring the other one is legal"
      _ -> Spec.assertFailure s "fixture should have two creatures"
  Spec.it s "CR 509.1b an enchanted creature can't block either, and the Piker beside it still can" $ do
    -- The other half of Pacifism's one line, on the other side of the combat
    -- phase. CR 702.3b's defender is the contrast: that keyword stops an attack
    -- and says nothing about blocking, so a restriction carrier that reused it
    -- could not print this card.
    pacifism <- S.printingOf s registry "Pacifism"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker] [piker, piker]
    case (mine, theirs) of
      (a : _, [pacified, other]) -> do
        let board = snd (pacifying pacifism pacified gs)
        Spec.assertBool s (not (Combat.canBlock S.bob pacified board)) "the enchanted creature cannot block"
        Spec.assertBool s (Combat.canBlock S.bob other board) "the one beside it can"
        Spec.assertEqWith s "and only that one is offered" (Combat.legalBlockers S.bob board) [other]
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton pacified (Set.singleton a)) board)) "blocking with the enchanted creature is illegal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton other (Set.singleton a)) board) "blocking with the other one is legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and two blockers"
  Spec.it s "CR 604.2 the restriction lifts the moment the Aura leaves the battlefield" $ do
    -- A restriction is gathered LIVE from the battlefield and never captured, the
    -- posture Pawl.Types.BlockRequirement's header argues for a requirement -- so an
    -- Aura leaving lifts it with nothing to unwind. Both worlds on ONE board, so
    -- the pair cannot drift.
    pacifism <- S.printingOf s registry "Pacifism"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker] []
    case mine of
      [creature] -> do
        let (aura, board) = pacifying pacifism creature gs
            gone = S.runPure S.identityAnswer board (Event.changeZone aura Zone.Graveyard)
        Spec.assertBool s (not (Combat.canAttack S.alice creature board)) "under the Aura it cannot attack"
        Spec.assertBool s (Combat.canAttack S.alice creature gone) "with the Aura in the graveyard it can again"
        -- The block half lifts on the same board. Asked of alice, who controls
        -- the creature: CR 509.1a's chosen-from set is a controller question, not
        -- a defending-player one, so canBlock answers it for either seat.
        Spec.assertBool s (not (Combat.canBlock S.alice creature board)) "under the Aura it cannot block"
        Spec.assertBool s (Combat.canBlock S.alice creature gone) "with the Aura in the graveyard it can again"
      _ -> Spec.assertFailure s "fixture should have a creature"
  Spec.it s "CR 508.1d a creature under BOTH a Curse and a Pacifism is not forced to attack" $ do
    -- THE INTERACTION CASE. Curse of the Nightly Hunt requires the creature to
    -- attack and Pacifism says it can't, and CR 508.1d settles it: the maximum is
    -- over the requirements obeyable "without disobeying any restrictions", so a
    -- creature that cannot attack carries no requirement instance and declining
    -- becomes legal again. The third assertion is what discriminates that from
    -- "the requirement was satisfied somehow" -- attacking with the creature is
    -- still illegal, so it left the candidate list rather than obeying anything.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    pacifism <- S.printingOf s registry "Pacifism"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = cursing curse S.alice [piker] []
    case mine of
      [creature] -> do
        let board = snd (pacifying pacifism creature gs)
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] gs)) "without the Pacifism, declining is illegal"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] board) "with it, declining is legal"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [creature] board)) "and attacking with it is illegal, requirement or no requirement"
      _ -> Spec.assertFailure s "fixture should have a creature"
  Spec.it s "CR 508.1d the same Curse still forces the creature the Pacifism does not touch" $ do
    -- The control for the case above, and the reason it is not "requirements
    -- stopped working": one Curse over two creatures is two requirements, the
    -- Pacifism removes exactly one of them, and the maximum drops from two to
    -- one rather than to zero.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    pacifism <- S.printingOf s registry "Pacifism"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = cursing curse S.alice [piker, piker] []
    case mine of
      [pacified, other] -> do
        let board = snd (pacifying pacifism pacified gs)
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] board)) "declining is still illegal"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [other] board) "the unenchanted creature alone attains the maximum"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [pacified, other] board)) "and the enchanted one may not attack even to obey"
      _ -> Spec.assertFailure s "fixture should have two creatures"
  Spec.it s "CR 509.1c a Lure does not require a Pacifism'd creature to block" $ do
    -- The blocking-side twin of the interaction case, on CR 509.1c's identically
    -- worded maximization. Both worlds on ONE board.
    lure <- S.printingOf s registry "Lure"
    pacifism <- S.printingOf s registry "Pacifism"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, theirs) = luring lure [piker] [piker]
    case theirs of
      [blocker] -> do
        let board = snd (pacifying pacifism blocker gs)
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty gs)) "without the Pacifism, declining to block is illegal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty board) "with it, declining is legal"
      _ -> Spec.assertFailure s "fixture should have one blocker"
  Spec.it s "CR 303.4m a Pacifism that is not attached to anything restricts nothing" $ do
    -- CR 303.4m reads the SOURCE's attachment, so an unattached Pacifism names no
    -- creature and restricts none. The Aura stays ON the battlefield throughout,
    -- so this is not a test that removing it works -- CR 704.5m would bury it, and
    -- no state-based-action pass is run here.
    pacifism <- S.printingOf s registry "Pacifism"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker] []
        withAura = snd (S.addPermanent pacifism S.alice gs)
    case mine of
      [creature] -> Spec.assertBool s (Combat.canAttack S.alice creature withAura) "it may still attack"
      _ -> Spec.assertFailure s "fixture should have a creature"
  Spec.it s "CR 508.1c whole cards: a Pacifism'd creature sits out a real declare attackers step" $ do
    -- The gameplay-level case, run through Engine.runStep -- the priority loop and
    -- the CR 703.4i turn-based action, not a direct call -- with the interpreter
    -- that attacks with everything it is offered. Both worlds asserted: without
    -- the Aura both 2/1 Pikers connect for 4, with it only one connects for 2.
    pacifism <- S.printingOf s registry "Pacifism"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker, piker] []
    case mine of
      [pacified, other] -> do
        let board = snd (pacifying pacifism pacified gs)
            after = S.runCombat S.aggressiveAnswer board
            control = S.runCombat S.aggressiveAnswer gs
        Spec.assertEqWith s "without the Aura, bob takes four" (S.lifeOf S.bob control) (Just 16)
        Spec.assertEqWith s "with it, bob takes two" (S.lifeOf S.bob after) (Just 18)
        Spec.assertEqWith s "and only the unenchanted Piker was ever declared" (S.attackerDeclarationsOf after) [other]
      _ -> Spec.assertFailure s "fixture should have two creatures"
  Spec.it s "CR 509.1b whole cards: a Pacifism'd creature sits out a real declare blockers step" $ do
    -- The blocking-side gameplay case. Without the Aura the interpreter blocks and
    -- the two 2/1s trade, so bob takes nothing; with it the block cannot happen and
    -- the attacker connects.
    pacifism <- S.printingOf s registry "Pacifism"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, theirs) = S.combatBoardOf [piker] [piker]
    case theirs of
      [blocker] -> do
        let board = snd (pacifying pacifism blocker gs)
            after = S.settleSba (S.runCombat S.aggressiveAnswer board)
            control = S.settleSba (S.runCombat S.aggressiveAnswer gs)
        Spec.assertEqWith s "without the Aura, the block happens and bob takes nothing" (S.lifeOf S.bob control) (Just 20)
        Spec.assertEqWith s "with it, the attacker connects" (S.lifeOf S.bob after) (Just 18)
        Spec.assertEqWith s "and bob's creature is alive, having blocked nothing" (S.creaturesInPlay S.bob after) 1
      _ -> Spec.assertFailure s "fixture should have one blocker"
  -- CR 509.1b narrowed by a MANA VALUE rather than by an attachment, and pointed
  -- at the OTHER seat: Void Winnower's "your opponents can't block with creatures
  -- with even mana values". The two blockers differ in parity alone -- a Goblin
  -- Piker ({1}{R}, 2) and an Uthden Troll ({1}{R}{R}, 3) -- and the Winnower
  -- controller's own Goblin Piker is the possessive control: same card, same even
  -- mana value, other side of "your opponents", and it may still block.
  Spec.it s "CR 509.1b Void Winnower stops an opponent blocking with an even mana value creature" $ do
    winnower <- S.printingOf s registry "Void Winnower"
    piker <- S.printingOf s registry "Goblin Piker"
    troll <- S.printingOf s registry "Uthden Troll"
    let (gs, mine, theirs) = S.combatBoardOf [piker, winnower] [piker, troll]
    case (mine, theirs) of
      ([alicesPiker, winnowerId], [evenBlocker, oddBlocker]) -> do
        let bare = S.runPure S.identityAnswer gs (Event.changeZone winnowerId Zone.Graveyard)
        Spec.assertBool s (not (Combat.canBlock S.bob evenBlocker gs)) "the mana value 2 creature cannot block"
        Spec.assertBool s (Combat.canBlock S.bob oddBlocker gs) "the mana value 3 creature beside it can"
        Spec.assertBool s (Combat.canBlock S.alice alicesPiker gs) "and the same card under the Winnower's own controller may block"
        Spec.assertBool s (Combat.canBlock S.bob evenBlocker bare) "the pair: with the Winnower gone the even creature may block again"
      _ -> Spec.assertFailure s "fixture should have two creatures a side"

-- CR 601.2c: announce every variable slot at one, then aim each slot at `oid`
-- where it is a legal recipient and take the rest as they come. Reasonable Doubt
-- has two slots and only one of them may be a creature, so the spell slot keeps
-- the one spell on the stack while the creature slot takes the named Piker.
suspecting :: ObjectId.ObjectId -> Prompt.Prompt r -> r
suspecting oid p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> fmap (const 1) offers
  Prompt.ChooseTargets _ _ _ offers -> S.preferring (\r -> Recipient.objectOf r == Just oid) offers
  _ -> S.identityAnswer p

-- bob's suspect, a Goblin Piker beside it and his two Islands, with `ahead`
-- placed under him BEFORE the pair and `behind` after them -- which is how the
-- pair below puts one Humility on either side of the same permanent and changes
-- nothing else. Placement order is timestamp order (Pawl.Support.addPermanent
-- allocates one per object), so the two boards differ in exactly one timestamp
-- comparison.
--
-- Then alice attacks, a Goblin Piker spell of hers goes on the stack to be
-- Reasonable Doubt's counter target, and bob casts the Doubt suspecting the
-- FIRST of the two. Returns the settled board, the suspected permanent, the one
-- beside it, alice's attacker and the spell the Doubt countered.
--
-- `suspected` is the printing the designation lands on: a Goblin Piker for the
-- timestamp pair, a Dryad Arbor for the CR 305.7 case, which needs a permanent a
-- basic-land-type set can reach.
suspectBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  Printing.Printing ->
  [Printing.Printing] ->
  [Printing.Printing] ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
suspectBoard s registry suspected ahead behind = do
  piker <- S.printingOf s registry "Goblin Piker"
  island <- S.printingOf s registry "Island"
  doubt <- S.printingOf s registry "Reasonable Doubt"
  let (gs0, mine, _) = S.combatBoardOf [piker] []
      (suspect, gsA) = S.addPermanent suspected S.bob (withPermanents S.bob ahead gs0)
      (other, gsB) = S.addPermanent piker S.bob gsA
      gs2 = withPermanents S.bob (behind <> [island, island]) gsB
      declared = S.runPure S.aggressiveAnswer gs2 (Combat.declareAttackers S.manaPerformer S.alice)
      (victim, gs3) = S.spellOnStack piker S.alice declared
      (doubtId, gs4) = S.addHandCard doubt S.bob gs3
      resolved = S.runPure (suspecting suspect) gs4 (S.cast S.bob doubtId >> Stack.resolveTop >> Engine.settleForPriority)
      attacker = case mine of
        a : _ -> a
        [] -> S.noSource
  pure (resolved, suspect, other, attacker, victim)

-- CR 701.60b's designation, read off the object -- Nothing for an object that has
-- left, which no assertion below wants to pass for.
suspectedOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe Bool
suspectedOf oid gs = fmap (Set.member Designation.Suspected . Object.designations) (Game.lookupObject oid gs)

-- Convincing Mirage's two prompts: its CR 303.4a enchant slot, forced onto the
-- one land this group cares about, and its CR 614.1c as-enters basic land type.
-- Recipient.ToObject and not ToCreature: the Aura's slot is over lands, which is
-- what Pawl.Support's stillLegalEnchant note warns about.
mirageOn :: ObjectId.ObjectId -> Subtype.Subtype -> Prompt.Prompt r -> r
mirageOn landId subtype p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject landId))) sets
  Prompt.ChooseBasicLandType {} -> subtype
  _ -> S.identityAnswer p

-- CR 701.60c against the two rules that strip abilities: CR 613.1f's layer-6
-- removal, ordered by CR 613.7, and CR 305.7's layer-4 subtype set, which spares
-- an ability the rules granted. Proved by Reasonable Doubt {1}{U} Instant,
-- "Counter target spell unless its controller pays {2}. Suspect up to one target
-- creature", cast under Humility for the first pair and beside a Convincing
-- Mirage for the third case.
--
-- Rule 701.60c states its restriction as quoted text, so what the designation gives
-- a permanent is an ABILITY, and CR 613.1f puts Humility's removal in the same
-- layer as the grant. The grant's timestamp is the suspected permanent's own (see
-- Pawl.Engine.Projection.designationGathered), so ORDER decides: a Humility already
-- on the battlefield when the Piker arrived applies first and the grant lands on
-- top of it, while one that arrived later applies last and takes the ability away.
--
-- The pair of boards differs in that one thing, and the two boards must DISAGREE --
-- a reading with no timestamps in it answers both alike. Rule 701.60c's two halves
-- are then each other's anti-vacuity leg: menace goes through the layer fold and
-- "can't block" through Pawl.Engine.CombatRestriction, so if only one of them moves
-- between the boards, one subsystem read the order and the other did not. The
-- second Piker is the third leg: it entered beside the suspect and was never
-- suspected, so a board on which nothing can block fails at it rather than passing.
suspectedAbilityRemovalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
suspectedAbilityRemovalSpec s registry = Spec.describe s "SuspectedAbilityRemoval" $ do
  Spec.it s "CR 613.7 a Humility older than the suspect leaves rule 701.60c's ability in place" $ do
    humility <- S.printingOf s registry "Humility"
    piker <- S.printingOf s registry "Goblin Piker"
    (gs, suspect, other, attacker, victim) <- suspectBoard s registry piker [humility] []
    Spec.assertEqWith s "the Doubt resolved and suspected the Piker it named, not the one beside it" (suspectedOf suspect gs, suspectedOf other gs) (Just True, Just False)
    -- The Doubt's other clause, so the card is exercised whole rather than only in
    -- the half this pair turns on: alice paid nothing, so her spell was countered.
    -- Her graveyard is what separates that from the spell having RESOLVED, which
    -- would have put a third Piker onto the battlefield instead. CR 701.6a puts the
    -- countered spell into its owner's graveyard as a new incarnation, so the stack
    -- object itself is gone.
    Spec.assertEqWith s "and countered the spell it named" (Maybe.isNothing (Game.lookupObject victim gs), length (Game.zoneMembers Zone.Graveyard S.alice gs)) (True, 1)
    Spec.assertBool s (Projection.hasKeyword Keyword.Menace suspect gs) "CR 701.60c: the later grant survives the earlier removal, so the menace half is there"
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton suspect (Set.singleton attacker)) gs)) "CR 701.60c: and so is the can't-block half"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton other (Set.singleton attacker)) gs) "while the unsuspected Piker beside it blocks"
  Spec.it s "CR 613.7 a Humility younger than the suspect removes it" $ do
    humility <- S.printingOf s registry "Humility"
    piker <- S.printingOf s registry "Goblin Piker"
    (gs, suspect, other, attacker, _) <- suspectBoard s registry piker [] [humility]
    Spec.assertEqWith s "the same designation, on the same Piker: CR 701.60b makes it no ability, so no removal reaches it" (suspectedOf suspect gs, suspectedOf other gs) (Just True, Just False)
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Menace suspect gs)) "CR 613.1f: the later removal wipes the grant, so the menace half is gone"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton suspect (Set.singleton attacker)) gs) "CR 613.1f: and the can't-block half with it"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton other (Set.singleton attacker)) gs) "as the Piker beside it could all along"
  -- CR 305.7's carve-out, which the case above's Humility cannot reach: that rule
  -- strips "all abilities generated from its rules text" and then says outright
  -- that it "doesn't remove any abilities that were granted to the land by other
  -- effects". Rule 701.60c grants this one, so the strip must spare it.
  --
  -- Dryad Arbor is the suspect because it is a PRINTED Land Creature -- no
  -- animation, so there is no second effect to confuse the read, and CR 305.7's
  -- own "doesn't add or remove any card types" keeps it a creature and therefore
  -- a would-be blocker afterwards. Convincing Mirage {1}{U} ("Enchant land / As
  -- this Aura enters, choose a basic land type. / Enchanted land is the chosen
  -- type.") is the setter, CAST rather than attached by hand: the chosen type is
  -- a CR 614.1c as-enters rewrite, and an Aura placed without it would leave
  -- Object.chosenSubtype empty and strip nothing at all.
  Spec.it s "CR 305.7 setting a suspected land's subtype spares the ability rule 701.60c granted" $ do
    dryadArbor <- S.printingOf s registry "Dryad Arbor"
    island <- S.printingOf s registry "Island"
    mirage <- S.printingOf s registry "Convincing Mirage"
    -- Two Islands behind the pair, so bob can pay for the Mirage after the Doubt:
    -- untouched mana is what keeps the negative leg below about the designation.
    (board, arbor, other, attacker, _) <- suspectBoard s registry dryadArbor [] [island, island]
    let (mirageId, withMirage) = S.addHandCard mirage S.bob board
        cast = S.runPure (mirageOn arbor Subtype.Island) withMirage (S.cast S.bob mirageId)
        settled = S.settleSba (S.runPure (mirageOn arbor Subtype.Island) cast Stack.resolveTop)
        -- Dryad Arbor is a LAND, so bob's two casts may have tapped it for mana.
        -- CR 509.1a lets only an untapped creature block, and a tapped one would
        -- make the first assertion pass without the restriction being read at
        -- all. Untapped here rather than by juggling which land pays: nothing in
        -- this case is about mana.
        gs = settled {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Untapped}) arbor (GameState.objects settled)}
    -- THE assertion, gameplay level and first.
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton arbor (Set.singleton attacker)) gs)) "CR 305.7: the rulebook-granted can't-block survives the subtype set"
    -- The third leg the pair above uses: an unsuspected permanent beside it still
    -- blocks, so a board on which nothing can block fails here.
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton other (Set.singleton attacker)) gs) "while the unsuspected Piker beside it blocks"
    -- The other half of rule 701.60c's one sentence, which goes through the layer
    -- fold instead: the two must agree.
    Spec.assertBool s (Projection.hasKeyword Keyword.Menace arbor gs) "CR 701.60c: and the menace half, which no layer-4 strip touches, is there too"
    -- ANTI-VACUITY, and not optional: if the Aura failed to attach or the chosen
    -- type went unset, there are no set-land-subtype effects at all, the gate this
    -- case exists to remove is vacuously satisfied, and the first assertion passes
    -- under the wrong implementation too.
    Spec.assertEqWith s "CR 305.7: the set really happened -- an Island where a Forest was, the Dryad creature type untouched" (Projection.subtypesOf arbor gs) (Set.fromList [Subtype.Island, Subtype.Dryad])
    Spec.assertEqWith s "on the permanent the Doubt suspected, which is still suspected" (suspectedOf arbor gs, suspectedOf other gs) (Just True, Just False)

-- CR 508.1c's and CR 509.1b's SECOND clause -- "or that it can't attack unless
-- some condition is met" -- proved by Blind-Spot Giant ("This creature can't
-- attack or block unless you control another Giant"), the pool's first printed
-- conditional restriction. Pacifism above prints the first clause of the same
-- parenthetical, and the two groups are deliberately separate: what is under test
-- here is only that the gate is read, and read afresh.
--
-- The card is the right prover on three counts. Its condition reads YOUR OWN
-- board rather than the defending player's, which is the other reading and is
-- defendingPlayerRestrictionSpec's below, and it gates on a FACT rather than on a
-- cost, which rides Pawl.Types.AttackCost instead for the reason CR 508.1d's
-- third sentence gives. It prints BOTH arms from one line, as Pacifism does. And "ANOTHER
-- Giant" makes it self-excluding, which the two directions below are about: a
-- lone Blind-Spot Giant does not count itself, while a second one counts the
-- first.
conditionalCombatRestrictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
conditionalCombatRestrictionSpec s registry = Spec.describe s "Conditional CombatRestrictions" $ do
  Spec.it s "CR 508.1c a lone Blind-Spot Giant can't attack: 'another Giant' does not count itself" $ do
    -- Direction one of the self-exclusion. The Goblin Piker beside it is the
    -- control on two axes at once: it is not a Giant, so it does not satisfy the
    -- condition, and it carries no restriction, so a blanket "nothing may attack"
    -- bug cannot pass.
    blindSpotGiant <- S.printingOf s registry "Blind-Spot Giant"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [blindSpotGiant, piker] []
    case mine of
      [giant, other] -> do
        Spec.assertBool s (not (Combat.canAttack S.alice giant gs)) "the Giant cannot attack"
        Spec.assertBool s (Combat.canAttack S.alice other gs) "the Piker beside it can"
        Spec.assertEqWith s "and only the Piker is offered" (Combat.legalAttackers S.alice gs) [other]
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [giant] gs)) "declaring the Giant is illegal"
      _ -> Spec.assertFailure s "fixture should have two creatures"
  Spec.it s "CR 508.1c a Hill Giant beside it meets the condition and the restriction does not apply" $ do
    -- The other side of the same board. Hill Giant is a vanilla Giant, so the
    -- only thing that changed between this case and the one above is whether the
    -- condition holds.
    blindSpotGiant <- S.printingOf s registry "Blind-Spot Giant"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (gs, mine, _) = S.combatBoardOf [blindSpotGiant, hillGiant] []
    case mine of
      [giant, hill] -> do
        Spec.assertBool s (Combat.canAttack S.alice giant gs) "the Blind-Spot Giant may attack"
        Spec.assertEqWith s "and both are offered" (Combat.legalAttackers S.alice gs) [giant, hill]
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [giant, hill] gs) "declaring both is legal"
      _ -> Spec.assertFailure s "fixture should have two creatures"
  Spec.it s "CR 508.1c two Blind-Spot Giants each count as the other's 'another Giant'" $ do
    -- Direction two of the self-exclusion, and the case that discriminates
    -- "another" from "no Blind-Spot Giant counts". The condition is read once per
    -- SOURCE, so `Not IsSource` excludes a different creature for each of them and
    -- both are freed.
    blindSpotGiant <- S.printingOf s registry "Blind-Spot Giant"
    let (gs, mine, _) = S.combatBoardOf [blindSpotGiant, blindSpotGiant] []
        (lone, _, _) = S.combatBoardOf [blindSpotGiant] []
    case (mine, Combat.legalAttackers S.alice lone) of
      ([first, second], []) -> do
        Spec.assertBool s (Combat.canAttack S.alice first gs) "the first may attack"
        Spec.assertBool s (Combat.canAttack S.alice second gs) "so may the second"
        Spec.assertEqWith s "and both are offered" (Combat.legalAttackers S.alice gs) [first, second]
      (_, offered) -> Spec.assertFailure s ("fixture should have two Giants and a restricted lone one, got " <> show offered)
  Spec.it s "CR 508.1c the condition reads YOUR board: an opponent's Giant does not free it" $ do
    -- "you control another Giant" -- CR 109.5's "you" is the ability's
    -- controller, so bob's Hill Giant is not one of yours. Without this the
    -- condition would be a bare subtype count.
    blindSpotGiant <- S.printingOf s registry "Blind-Spot Giant"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (gs, mine, _) = S.combatBoardOf [blindSpotGiant] [hillGiant]
    case mine of
      [giant] -> Spec.assertBool s (not (Combat.canAttack S.alice giant gs)) "bob's Giant does not free alice's"
      _ -> Spec.assertFailure s "fixture should have one creature"
  Spec.it s "CR 509.1b the same condition gates the block half" $ do
    -- CR 509.1b's parenthetical is CR 508.1c's with "block" in place of
    -- "attack", and the card prints both from one line. Both worlds again, so a
    -- block half wired to the attack half's answer cannot pass.
    blindSpotGiant <- S.printingOf s registry "Blind-Spot Giant"
    hillGiant <- S.printingOf s registry "Hill Giant"
    piker <- S.printingOf s registry "Goblin Piker"
    let (alone, _, theirs) = attacking [piker] [blindSpotGiant]
        (freed, _, pair) = attacking [piker] [blindSpotGiant, hillGiant]
    case (theirs, pair) of
      ([lone], [giant, _]) -> do
        Spec.assertBool s (not (Combat.canBlock S.bob lone alone)) "the lone Giant cannot block"
        Spec.assertEqWith s "and is not offered" (Combat.legalBlockers S.bob alone) []
        Spec.assertBool s (Combat.canBlock S.bob giant freed) "with a Hill Giant beside it, it can"
      _ -> Spec.assertFailure s "fixture should have the Giants on bob's side"
  Spec.it s "CR 508.1c the gate is re-read: the Hill Giant leaving re-imposes the restriction" $ do
    -- The gather is LIVE and re-derived on every read, the posture every carrier
    -- of a CR 613.11 effect takes, so a condition that stops holding re-imposes
    -- the restriction with nothing to unwind -- and one that starts holding lifts
    -- it. Both worlds on ONE board, so the pair cannot drift.
    blindSpotGiant <- S.printingOf s registry "Blind-Spot Giant"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (gs, mine, _) = S.combatBoardOf [blindSpotGiant, hillGiant] []
    case mine of
      [giant, hill] -> do
        let gone = S.runPure S.identityAnswer gs (Event.changeZone hill Zone.Graveyard)
        Spec.assertBool s (Combat.canAttack S.alice giant gs) "with the Hill Giant it may attack"
        Spec.assertBool s (not (Combat.canAttack S.alice giant gone)) "with the Hill Giant in the graveyard it may not"
        Spec.assertBool s (not (Combat.canBlock S.alice giant gone)) "nor block"
      _ -> Spec.assertFailure s "fixture should have two creatures"
  Spec.it s "CR 508.1c whole cards: the condition decides a real declare attackers step" $ do
    -- The gameplay-level case, run through the priority loop and CR 703.4i's
    -- turn-based action rather than a direct call. With the Hill Giant both
    -- connect for 4 + 3; without it the Blind-Spot Giant is never declared and
    -- bob takes nothing.
    blindSpotGiant <- S.printingOf s registry "Blind-Spot Giant"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (gs, mine, _) = S.combatBoardOf [blindSpotGiant, hillGiant] []
    case mine of
      [giant, hill] -> do
        let gone = S.runPure S.identityAnswer gs (Event.changeZone hill Zone.Graveyard)
            after = S.runCombat S.aggressiveAnswer gs
            control = S.runCombat S.aggressiveAnswer gone
        Spec.assertEqWith s "with the Hill Giant, bob takes seven" (S.lifeOf S.bob after) (Just 13)
        Spec.assertEqWith s "and both were declared" (S.attackerDeclarationsOf after) [giant, hill]
        Spec.assertEqWith s "without it, bob takes nothing" (S.lifeOf S.bob control) (Just 20)
        Spec.assertEqWith s "and nothing was declared" (S.attackerDeclarationsOf control) []
      _ -> Spec.assertFailure s "fixture should have two creatures"

-- CR 508.1c's gate naming the DEFENDING PLAYER (CR 508.5). Armored Galleon
-- ({4}{U} Creature -- Human Pirate 5/4, "This creature can't attack unless
-- defending player controls an Island." -- checked against Scryfall, 2026-08-16)
-- is the pool's first card whose "unless" clause is about the player being
-- attacked rather than about the source's controller;
-- conditionalCombatRestrictionSpec's Blind-Spot Giant is the other reading.
--
-- Three seats in the last case but two in the rest, and the split is deliberate:
-- a two-player board cannot tell "the defending player" from "an opponent", so
-- the discriminator lives on the three-seat board while the cheaper cases prove
-- the gate is read at all.
defendingPlayerRestrictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
defendingPlayerRestrictionSpec s registry = Spec.describe s "DefendingPlayerCombatRestriction" $ do
  Spec.it s "CR 508.1c the Galleon can't attack a defender with no Island" $ do
    -- The NEGATIVE, paired with the positive below on a board differing only in
    -- who controls the Island. The Piker beside it is the control on the other
    -- axis: it carries no restriction, so a blanket "nothing may attack" bug
    -- cannot pass here.
    galleon <- S.printingOf s registry "Armored Galleon"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [galleon, piker] []
    case mine of
      [ship, other] -> do
        Spec.assertBool s (not (Combat.canAttack S.alice ship gs)) "the Galleon cannot attack"
        Spec.assertBool s (Combat.canAttack S.alice other gs) "the Piker beside it can"
        Spec.assertEqWith s "and only the Piker is offered" (Combat.legalAttackers S.alice gs) [other]
      _ -> Spec.assertFailure s "fixture should have two creatures"
  Spec.it s "CR 508.1c an Island the DEFENDER controls lifts it" $ do
    -- THE POSITIVE. Without it the negative above passes on any board at all --
    -- a combat restriction assertion is false for summoning sickness, for
    -- tapped-ness and for five other conjuncts of canAttackGiven.
    galleon <- S.printingOf s registry "Armored Galleon"
    island <- S.printingOf s registry "Island"
    let (gs, mine, _) = S.combatBoardOf [galleon] []
        defended = withPermanents S.bob [island] gs
    case mine of
      [ship] -> do
        Spec.assertBool s (not (Combat.canAttack S.alice ship gs)) "without an Island it may not"
        Spec.assertBool s (Combat.canAttack S.alice ship defended) "with bob's Island it may"
        Spec.assertEqWith s "and it is offered" (Combat.legalAttackers S.alice defended) [ship]
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [ship] defended) "declaring it is legal"
      _ -> Spec.assertFailure s "fixture should have one creature"
  Spec.it s "CR 508.5 the Island must be the DEFENDING player's, not the attacker's" $ do
    -- Discriminates ControlledByDefendingPlayer from a bare "an Island is on the
    -- battlefield" (Glacial Crasher's actual shape, already in the pool) and from
    -- CR 109.5's "you". Same card count as the positive above.
    galleon <- S.printingOf s registry "Armored Galleon"
    island <- S.printingOf s registry "Island"
    let (gs, mine, _) = S.combatBoardOf [galleon] []
        mineOnly = withPermanents S.alice [island] gs
    case mine of
      [ship] -> Spec.assertBool s (not (Combat.canAttack S.alice ship mineOnly)) "alice's own Island does not free it"
      _ -> Spec.assertFailure s "fixture should have one creature"
  Spec.it s "CR 508.5a three seats: a NON-defending opponent's Island does not free it" $ do
    -- CR 508.5a names ONE defending player, so "an opponent controls an Island"
    -- is the wrong reading, and a two-player board cannot tell the two apart.
    -- threePlayerCombat sits at the beginning of combat with no defender, so the
    -- step is stated here rather than derived: one board, two seats defending in
    -- turn, and carol holds the only Island.
    galleon <- S.printingOf s registry "Armored Galleon"
    island <- S.printingOf s registry "Island"
    let (gs, mine, _, _) = S.threePlayerCombat [galleon] [] [island]
        defendedBy who =
          gs
            { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
              GameState.combat = (GameState.combat gs) {Combat.Type.defenders = [who]}
            }
    case mine of
      [ship] -> do
        Spec.assertBool s (not (Combat.canAttack S.alice ship (defendedBy S.bob))) "carol's Island does not free an attack on bob"
        Spec.assertBool s (Combat.canAttack S.alice ship (defendedBy S.carol)) "but it does free an attack on carol"
      _ -> Spec.assertFailure s "fixture should have one creature"
  Spec.it s "CR 508.1c whole cards: the gate decides a real declare attackers step" $ do
    -- Gameplay level, through CR 703.4i's turn-based action rather than a direct
    -- call. The Galleon is 5/4 and alone on its side, so the life delta is its
    -- own and no other creature's.
    galleon <- S.printingOf s registry "Armored Galleon"
    island <- S.printingOf s registry "Island"
    let (gs, mine, _) = S.combatBoardOf [galleon] []
        defended = withPermanents S.bob [island] gs
        after = S.runCombat S.aggressiveAnswer defended
        control = S.runCombat S.aggressiveAnswer gs
    case mine of
      [ship] -> do
        Spec.assertEqWith s "with the Island, bob takes five" (S.lifeOf S.bob after) (Just 15)
        Spec.assertEqWith s "and the Galleon was declared" (S.attackerDeclarationsOf after) [ship]
        Spec.assertEqWith s "without it, bob takes nothing" (S.lifeOf S.bob control) (Just 20)
        Spec.assertEqWith s "and nothing was declared" (S.attackerDeclarationsOf control) []
      _ -> Spec.assertFailure s "fixture should have one creature"

-- CR 508.1c's PAIRWISE attacking restriction: one naming WHAT the attack is aimed
-- at rather than which creatures may attack. Blazing Archon ({6}{W}{W}{W}
-- Creature -- Archon 5/6, "Flying / Creatures can't attack you." -- checked
-- against Scryfall, 2026-09-01) is the pool's first, and CR 802.3a is the rule
-- that says such a restriction reaches only the creatures attacking that player.
-- Vow of Flight, at the foot of the group, is the pool's second, and names two
-- of CR 506.3's three attackable things where the Archon names one.
--
-- Two seats throughout, deliberately: this is not a multiplayer rule. CR 508.1b
-- makes a planeswalker its controller controls a SECOND announcement at two
-- seats, so "can't attack you" and a blanket "can't attack" already come apart
-- there, and the Jace case below is what tells them apart.
--
-- Every pair of boards differs in exactly one thing -- who controls the Archon,
-- or whether it is on the board at all -- because "the Piker did not attack" is
-- equally true of summoning sickness and of six other conjuncts of
-- canAttackGiven.
aimedAttackRestrictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
aimedAttackRestrictionSpec s registry = Spec.describe s "AimedAttackRestriction" $ do
  Spec.it s "CR 508.1c the Archon's controller can't be attacked, and CR 109.5 fixes who that is" $ do
    archon <- S.printingOf s registry "Blazing Archon"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = S.combatBoardOf [piker] [archon]
    case (mine, theirs) of
      ([pikerId], [archonId]) -> do
        -- The PAIRED positive, on the same permanents: the Archon under alice's
        -- control protects alice, who is not being attacked, so bob is fair game
        -- again. Discriminates PlayerScope.You from EachPlayer, which would bar
        -- the attack on either board.
        let freed = S.giveControl archonId S.alice gs
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [pikerId] gs)) "CR 802.3a: the Piker may not be declared attacking bob"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [pikerId] freed) "and may once the Archon is alice's instead"
        -- CR 508.1a is untouched: the restriction is about the ANNOUNCEMENT, so
        -- the Piker stays a candidate on both boards and it is the declaration
        -- that is refused.
        Spec.assertBool s (Combat.canAttack S.alice pikerId gs) "the Piker is still a CR 508.1a candidate"
        Spec.assertEqWith s "and still offered" (Combat.legalAttackers S.alice gs) [pikerId]
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] gs) "declining stays legal"
      _ -> Spec.assertFailure s "fixture should give alice a Piker and bob an Archon"
  Spec.it s "CR 508.1b a planeswalker that player controls is a different announcement" $ do
    -- THE DISCRIMINATOR against a blanket CantAttack, which would refuse both
    -- announcements below. One board, two declarations of the same creature.
    archon <- S.printingOf s registry "Blazing Archon"
    jace <- S.printingOf s registry "Jace Beleren"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = S.combatBoardOf [piker] [archon, jace]
    case (mine, theirs) of
      ([pikerId], [_, jaceId]) -> do
        let board = S.addCounter CounterKind.Loyalty 3 jaceId gs
        Spec.assertBool s (Combat.legalAttackDeclarationAs S.alice [(pikerId, AttackTarget.OfPlaneswalker jaceId)] board) "CR 508.1b: attacking bob's planeswalker is legal"
        Spec.assertBool s (not (Combat.legalAttackDeclarationAs S.alice [(pikerId, AttackTarget.OfPlayer S.bob)] board)) "attacking bob himself is not"
        -- The fixture pin: without the loyalty the planeswalker is a CR 704.5i
        -- casualty and the case above would be about a board that cannot exist.
        Spec.assertEqWith s "and Jace really is on the board with loyalty" (S.counterOf CounterKind.Loyalty jaceId board) 3
      _ -> Spec.assertFailure s "fixture should give alice a Piker and bob an Archon and a Jace"
  Spec.it s "CR 508.1c whole cards: the declare attackers step aims the Piker at Jace instead" $ do
    -- GAMEPLAY LEVEL, through CR 703.4i's turn-based action. The control is the
    -- same board with the Archon left off, where the announcement the engine
    -- makes is bob's own seat.
    archon <- S.printingOf s registry "Blazing Archon"
    jace <- S.printingOf s registry "Jace Beleren"
    piker <- S.printingOf s registry "Goblin Piker"
    let (guarded, mine, theirs) = S.combatBoardOf [piker] [archon, jace]
        (open, mineOpen, theirsOpen) = S.combatBoardOf [piker] [jace]
    case (mine, theirs, mineOpen, theirsOpen) of
      ([pikerId], [_, jaceId], [openPiker], [openJace]) -> do
        -- Read at the declare blockers step, before a block can absorb the
        -- damage: the Archon is a 5/6 and would eat the Piker it is protecting
        -- bob from.
        let announced g = Map.lookup (fst g) (Combat.Type.attackers (GameState.combat (S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer (snd g))))
        Spec.assertEqWith
          s
          "the Piker was announced at the planeswalker, bob being off limits"
          (announced (pikerId, S.addCounter CounterKind.Loyalty 3 jaceId guarded))
          (Just (AttackTarget.OfPlaneswalker jaceId))
        Spec.assertEqWith
          s
          "and at bob himself on the same board without the Archon"
          (announced (openPiker, S.addCounter CounterKind.Loyalty 3 openJace open))
          (Just (AttackTarget.OfPlayer S.bob))
      _ -> Spec.assertFailure s "fixture should give alice a Piker on each board"
  Spec.it s "CR 508.1d a creature with nobody it may attack is not able, so the Curse excuses it" $ do
    -- CR 508.1d counts the requirements obeyable "without disobeying any
    -- restrictions", so an announcement this restriction forbids is worth
    -- nothing to the maximization. Without that, the Curse would demand an
    -- attack the restriction refuses and no declaration at all would be legal.
    archon <- S.printingOf s registry "Blazing Archon"
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = cursing curse S.alice [piker] [archon]
    case mine of
      [pikerId] -> do
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] gs) "CR 508.1d: declining is legal, the Curse having nothing it can require"
        -- The PIN, and the reason the assertion above is not vacuous: on the
        -- same board without the Archon the Curse does forbid declining, so it
        -- really is in force and really does reach this creature.
        let (uncursed, _, _) = cursing curse S.alice [piker] []
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] uncursed)) "and it does forbid declining with no Archon on the board"
        Spec.assertEqWith s "the Piker is a candidate on both boards, so the Curse reaches it" (Combat.legalAttackers S.alice gs) [pikerId]
      _ -> Spec.assertFailure s "fixture should give alice a Piker"

  Spec.it s "CR 508.1c whole cards: with nothing else to attack, no attack is declared" $ do
    -- The other half of the gameplay reading: bob controls no planeswalker, so
    -- every announcement CR 508.1b offers is forbidden and the creature is not
    -- declared at all. Paired with the same board under alice's control, where
    -- both her creatures connect.
    archon <- S.printingOf s registry "Blazing Archon"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = S.combatBoardOf [piker] [archon]
    case (mine, theirs) of
      ([pikerId], [archonId]) -> do
        -- Blocks DECLINED, because the Archon is a 5/6 that would block the very
        -- Piker it is keeping off bob: with blocks on, "bob takes nothing" is
        -- true whether or not the restriction bit, and the life assertion proves
        -- nothing.
        let unblocking :: Prompt.Prompt r -> r
            unblocking p = case p of
              Prompt.DeclareBlockers {} -> Map.empty
              _ -> S.aggressiveAnswer p
            after = S.runCombat unblocking gs
            control = S.runCombat unblocking (S.giveControl archonId S.alice gs)
        Spec.assertEqWith s "bob takes nothing" (S.lifeOf S.bob after) (Just 20)
        Spec.assertEqWith s "and nothing was declared" (S.attackerDeclarationsOf after) []
        Spec.assertEqWith s "with the Archon protecting alice instead, the Piker's 2 and the Archon's 5 both land" (S.lifeOf S.bob control) (Just 13)
        Spec.assertEqWith s "and both were declared" (S.attackerDeclarationsOf control) [pikerId, archonId]
      _ -> Spec.assertFailure s "fixture should give alice a Piker and bob an Archon"

  -- CR 506.3's OTHER attackable things. Vow of Flight ({2}{U} Enchantment --
  -- Aura, "Enchant creature / Enchanted creature gets +2/+2, has flying, and
  -- can't attack you or planeswalkers you control." -- checked against Scryfall,
  -- 2026-09-01) names two of the three where Blazing Archon names one, so the
  -- pair of cards is what tells the `kinds` field from a hardcoded OfPlayer.
  --
  -- Both boards carry a Jace with loyalty on them, since a planeswalker at zero
  -- is a CR 704.5i casualty and the announcement would be about a board that
  -- cannot exist.
  Spec.it s "CR 506.3 the Vow bars bob's planeswalker too, where the Archon leaves it attackable" $ do
    vow <- S.printingOf s registry "Vow of Flight"
    archon <- S.printingOf s registry "Blazing Archon"
    jace <- S.printingOf s registry "Jace Beleren"
    piker <- S.printingOf s registry "Goblin Piker"
    let (vowed, mine, theirs) = S.combatBoardOf [piker] [vow, jace]
        (guarded, mineGuarded, theirsGuarded) = S.combatBoardOf [piker] [archon, jace]
    case (mine, theirs, mineGuarded, theirsGuarded) of
      ([pikerId], [vowId, jaceId], [otherPiker], [_, otherJace]) -> do
        let board = S.attach vowId pikerId (S.addCounter CounterKind.Loyalty 3 jaceId vowed)
            control = S.addCounter CounterKind.Loyalty 3 otherJace guarded
        Spec.assertBool s (not (Combat.legalAttackDeclarationAs S.alice [(pikerId, AttackTarget.OfPlaneswalker jaceId)] board)) "CR 506.3: bob's planeswalker is off limits under the Vow"
        Spec.assertBool s (Combat.legalAttackDeclarationAs S.alice [(otherPiker, AttackTarget.OfPlaneswalker otherJace)] control) "and attackable under the Archon, whose sentence names only the seat"
        Spec.assertBool s (not (Combat.legalAttackDeclarationAs S.alice [(pikerId, AttackTarget.OfPlayer S.bob)] board)) "CR 508.1b: bob himself is off limits under the Vow"
        Spec.assertBool s (not (Combat.legalAttackDeclarationAs S.alice [(otherPiker, AttackTarget.OfPlayer S.bob)] control)) "and under the Archon, which is the half the two cards share"
        -- The fixture pin: the Aura has to be ON the Piker for its affected set
        -- to reach it, and its +2/+2 is the only thing on either board that says
        -- so.
        Spec.assertEqWith s "the Vow really is attached, so the 2/1 Piker is a 4/3" (S.powerToughnessOf pikerId board) (Just (4, 3))
      _ -> Spec.assertFailure s "fixture should give alice a Piker and bob a Vow and a Jace on one board, an Archon and a Jace on the other"

  Spec.it s "CR 506.3 whole cards: with both announcements barred the Piker is not declared at all" $ do
    -- GAMEPLAY LEVEL, and the control differs in exactly one thing: who controls
    -- the Vow. CR 109.5's "you" then names alice, so the Aura protects the
    -- attacker's own seat and both of bob's announcements open back up.
    vow <- S.printingOf s registry "Vow of Flight"
    jace <- S.printingOf s registry "Jace Beleren"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = S.combatBoardOf [piker] [vow, jace]
    case (mine, theirs) of
      ([pikerId], [vowId, jaceId]) -> do
        let board = S.attach vowId pikerId (S.addCounter CounterKind.Loyalty 3 jaceId gs)
            after = S.runCombat S.aggressiveAnswer board
            control = S.runCombat S.aggressiveAnswer (S.giveControl vowId S.alice board)
        Spec.assertEqWith s "bob takes nothing" (S.lifeOf S.bob after) (Just 20)
        Spec.assertEqWith s "and bob's Jace keeps its loyalty, the other announcement being barred too" (S.counterOf CounterKind.Loyalty jaceId after) 3
        Spec.assertEqWith s "and nothing was declared" (S.attackerDeclarationsOf after) []
        Spec.assertEqWith s "with the Vow protecting alice instead, the enchanted Piker's 4 lands" (S.lifeOf S.bob control) (Just 16)
        Spec.assertEqWith s "and it was declared" (S.attackerDeclarationsOf control) [pikerId]
      _ -> Spec.assertFailure s "fixture should give alice a Piker and bob a Vow and a Jace"

-- CR 802.3a: a restriction that applies to attacking a SPECIFIC PLAYER applies
-- only to the creatures attacking that player. Armored Galleon ({4}{U} Creature
-- -- Human Pirate 5/4, "This creature can't attack unless defending player
-- controls an Island.") is the pool's producer, and CR 508.5 is what makes its
-- gate about the player being attacked rather than about its controller.
--
-- THREE SEATS with BOTH opponents defending throughout (CR 802.2), which is the
-- only board that can tell the rule from pawl's old reading: with one defending
-- player there is one answer and the first seat in turn order is it.
-- defendingPlayerRestrictionSpec above is that board, one defender at a time, and
-- proves the gate is read at all.
--
-- carol holds the only Island in every case, so "the seat that frees the attack"
-- is never the first defending player in turn order -- the seat the old reading
-- took.
perDefenderRestrictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
perDefenderRestrictionSpec s registry = Spec.describe s "PerDefenderAttackRestriction" $ do
  Spec.it s "CR 802.3a the Galleon may attack the defender with an Island and not the one without" $ do
    galleon <- S.printingOf s registry "Armored Galleon"
    island <- S.printingOf s registry "Island"
    let (gs, mine, _, _) = S.threePlayerCombat [galleon] [] [island]
        declaring g =
          g
            { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
              GameState.combat = (GameState.combat g) {Combat.Type.defenders = [S.bob, S.carol]}
            }
        board = declaring gs
        -- The PAIR: the same board with carol's Island taken away, where the
        -- restriction binds at BOTH seats and the Galleon cannot attack at all.
        (bare, bareMine, _, _) = S.threePlayerCombat [galleon] [] []
        noIsland = declaring bare
    case (mine, bareMine) of
      ([ship], [bareShip]) -> do
        Spec.assertBool s (Combat.legalAttackDeclarationAs S.alice [(ship, AttackTarget.OfPlayer S.carol)] board) "CR 802.3a: attacking carol, who controls the Island, is legal"
        Spec.assertBool s (not (Combat.legalAttackDeclarationAs S.alice [(ship, AttackTarget.OfPlayer S.bob)] board)) "and attacking bob, who does not, is not"
        -- CR 508.1a's candidate list, which the old reading took the Galleon off
        -- entirely: the restriction binds at one seat, so the creature is able to
        -- attack and it is the ANNOUNCEMENT that is refused.
        Spec.assertBool s (Combat.canAttack S.alice ship board) "the Galleon is a candidate"
        Spec.assertBool s (not (Combat.canAttack S.alice bareShip noIsland)) "and is not one when NEITHER defender controls an Island"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] board) "declining stays legal"
      _ -> Spec.assertFailure s "fixture should give alice one Galleon on each board"
  Spec.it s "CR 508.5 the defending player of an attack on a planeswalker is its controller" $ do
    -- The gate is read through the ANNOUNCEMENT's defending player, not off the
    -- seat named directly: bob controls the planeswalker and no Island, so
    -- attacking it is refused while attacking carol is not. A reader that
    -- consulted only CR 506.3's player arm would allow the planeswalker.
    galleon <- S.printingOf s registry "Armored Galleon"
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, mine, theirs, _) = S.threePlayerCombat [galleon] [jace] [island]
    case (mine, theirs) of
      ([ship], [jaceId]) -> do
        let board =
              (S.addCounter CounterKind.Loyalty 3 jaceId gs)
                { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
                  GameState.combat = (GameState.combat gs) {Combat.Type.defenders = [S.bob, S.carol]}
                }
        Spec.assertBool s (not (Combat.legalAttackDeclarationAs S.alice [(ship, AttackTarget.OfPlaneswalker jaceId)] board)) "CR 508.5: bob controls the planeswalker and no Island, so his planeswalker is off limits too"
        Spec.assertBool s (Combat.legalAttackDeclarationAs S.alice [(ship, AttackTarget.OfPlayer S.carol)] board) "while carol, who controls the Island, may still be attacked"
        Spec.assertBool s (not (Combat.legalAttackDeclarationAs S.alice [(ship, AttackTarget.OfPlayer S.bob)] board)) "and bob himself may not"
        -- The fixture pin: without loyalty the planeswalker is a CR 704.5i
        -- casualty and the first assertion is about a board that cannot exist.
        Spec.assertEqWith s "Jace is on the board with loyalty" (S.counterOf CounterKind.Loyalty jaceId board) 3
      _ -> Spec.assertFailure s "fixture should give alice a Galleon and bob a Jace"
  Spec.it s "CR 802.3a whole cards: the declare attackers step sends the Galleon at the Island's controller" $ do
    -- GAMEPLAY LEVEL, from the beginning of combat step, so CR 703.4h picks both
    -- defending players (CR 802.2) rather than the fixture stating them. The two
    -- boards differ in exactly one thing: which opponent holds the Island.
    galleon <- S.printingOf s registry "Armored Galleon"
    island <- S.printingOf s registry "Island"
    let (atCarol, _, _, _) = S.threePlayerCombat [galleon] [] [island]
        (atBob, _, _, _) = S.threePlayerCombat [galleon] [island] []
        after = S.runCombat S.aggressiveAnswer atCarol
        control = S.runCombat S.aggressiveAnswer atBob
        -- Read at the declare blockers step, since the end of combat step empties
        -- the record the pins below read.
        atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer atCarol
    Spec.assertEqWith s "carol holds the Island, so carol takes the Galleon's five" (S.lifeOf S.carol after) (Just 15)
    Spec.assertEqWith s "and bob, who does not, takes nothing" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "with the Island moved to bob, bob takes the five" (S.lifeOf S.bob control) (Just 15)
    Spec.assertEqWith s "and carol takes nothing" (S.lifeOf S.carol control) (Just 20)
    Spec.assertEqWith s "the announcement really named carol (CR 508.1b)" (Map.elems (Combat.Type.attackers (GameState.combat atBlockers))) [AttackTarget.OfPlayer S.carol]
    -- The fixture pin: CR 802.2 really did make BOTH opponents defending players,
    -- so each case above is a choice between two seats and not a one-seat combat.
    Spec.assertEqWith s "both opponents defended" (Combat.Type.defenders (GameState.combat atBlockers)) [S.bob, S.carol]

-- CR 612.1 reaching a combat restriction's GATE. Glacial Crasher ({4}{U}{U}
-- Creature -- Elemental 5/5, "Trample. This creature can't attack unless there is
-- a Mountain on the battlefield." -- checked against Scryfall, 2026-08-05) is the
-- pool's first restriction whose CR 508.1c "unless" clause names a basic land
-- type, so it is the first card a Magical Hack can aim at this read-point.
--
-- Why this card and not one of the two dozen "can't attack unless defending player
-- controls an Island" printings, which are the same sentence in a commoner shape:
-- their condition is about the player being attacked, so it would test CR 508.5's
-- read (defendingPlayerRestrictionSpec's Armored Galleon) alongside CR 612.1's
-- swap. Glacial Crasher asks the same question of the WHOLE battlefield, so the
-- swap is the only thing under test. A sweep of the full Oracle corpus
-- (2026-08-05) for a combat requirement or restriction whose clause names a basic
-- land type returns this card, Harbor Serpent's five-Island count -- the same
-- shape with a bigger threshold -- Leviathan's "unless you sacrifice two
-- Islands", which is a COST and rides Pawl.Types.AttackCost rather than a gate,
-- Kraken of the Straits's pairwise "can't block this creature", which
-- Pawl.Types.CombatRestriction's header argues is not representable, that blocked
-- defending-player family, and landwalk reminder text.
--
-- Landwalk is deliberately not this group: textChangedLandwalkSpec above is the
-- swap reaching a KEYWORD's own land type, which is read out of the
-- Keyword.Landwalk constructor and never through a Condition.
--
-- Two directions, because "the Crasher did not attack" is equally true of a
-- restriction that was never lifted for some unrelated reason. The hack that
-- FREES it and the hack that BINDS it are asserted against the same unhacked
-- controls, and both fail against a reader that passes the printed gate through.
textChangedCombatRestrictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
textChangedCombatRestrictionSpec s registry = Spec.describe s "TextChangedCombatRestriction" $ do
  -- alice attacks with a lone Glacial Crasher and holds a Magical Hack plus the
  -- Island that pays for it; bob defends with nothing and, with `withMountain`, a
  -- Mountain. The Island is alice's own, so it is on the battlefield to be
  -- COUNTED as well as tapped -- which is the point in the freeing direction,
  -- where the swap makes the gate read Islands. bob gets no creature, so the
  -- gameplay case below turns on the declaration alone and never on a block.
  let crasherBoard withMountain hacked from to = do
        crasher <- S.printingOf s registry "Glacial Crasher"
        island <- S.printingOf s registry "Island"
        mountain <- S.printingOf s registry "Mountain"
        magicalHack <- S.printingOf s registry "Magical Hack"
        let (gs0, ours, _) = S.combatBoardOf [crasher] []
            (_, gs1) = S.addPermanent island S.alice gs0
            gs2 = if withMountain then snd (S.addPermanent mountain S.bob gs1) else gs1
            (hackId, gs3) = S.addHandCard magicalHack S.alice gs2
        case ours of
          [crasherId] -> pure (if hacked then castHackAt hackId crasherId from to gs3 else gs3, crasherId)
          _ -> Spec.assertFailure s "fixture should have the Crasher"
  Spec.it s "CR 508.1c the printed gate reads Mountains" $ do
    -- The premise the two hacked cases are read against. Nothing here involves a
    -- text change: the restriction lifts exactly when a Mountain is on the
    -- battlefield, and bob's Mountain counts, the clause naming no controller.
    (without, crasher) <- crasherBoard False False Subtype.Mountain Subtype.Island
    (with, crasher') <- crasherBoard True False Subtype.Mountain Subtype.Island
    Spec.assertBool s (not (Combat.canAttack S.alice crasher without)) "with no Mountain the Crasher cannot attack"
    Spec.assertEqWith s "and is not offered" (Combat.legalAttackers S.alice without) []
    Spec.assertBool s (Combat.canAttack S.alice crasher' with) "with bob's Mountain it can"
  Spec.it s "CR 612.1 a hacked Crasher reads ISLANDS and alice's own Island frees it" $ do
    -- THE FREEING DIRECTION. Mountain -> Island on the Crasher, and the board
    -- never moves: there is still no Mountain anywhere, and the Island that could
    -- not satisfy the printed gate satisfies the rewritten one. This fails
    -- against a reader that hands Condition.holds the printed condition.
    (gs, crasher) <- crasherBoard False True Subtype.Mountain Subtype.Island
    -- The anti-vacuity check, first: every assertion below also holds of a Hack
    -- that was never cast, so this one says the swap really landed on the Crasher.
    Spec.assertEqWith s "the Hack resolved onto the Crasher" (Projection.textChangesAffecting crasher gs) [(Subtype.Mountain, Subtype.Island)]
    Spec.assertBool s (Combat.canAttack S.alice crasher gs) "the hacked Crasher may attack"
    Spec.assertEqWith s "and is offered" (Combat.legalAttackers S.alice gs) [crasher]
  Spec.it s "CR 612.1 a hack to a type nobody controls BINDS a Crasher the board had freed" $ do
    -- THE BINDING DIRECTION, and the half that keeps the case above from passing
    -- by the gate simply going unread. bob's Mountain is on the battlefield and
    -- lifts the printed restriction; Mountain -> Forest rewrites the gate to a
    -- type no player controls, so the same board forbids the attack.
    (gs, crasher) <- crasherBoard True True Subtype.Mountain Subtype.Forest
    Spec.assertEqWith s "the Hack resolved onto the Crasher" (Projection.textChangesAffecting crasher gs) [(Subtype.Mountain, Subtype.Forest)]
    Spec.assertBool s (not (Combat.canAttack S.alice crasher gs)) "the hacked Crasher may not attack"
    Spec.assertEqWith s "and is not offered" (Combat.legalAttackers S.alice gs) []
  Spec.it s "CR 612.1 whole cards: the rewritten gate decides a real declare attackers step" $ do
    -- The gameplay-level case, run through CR 703.4i's turn-based action and the
    -- priority loop rather than a direct call. The Crasher is a 5/5, so bob's life
    -- total is what the two worlds differ in.
    (freed, _) <- crasherBoard False True Subtype.Mountain Subtype.Island
    (bound, _) <- crasherBoard False False Subtype.Mountain Subtype.Island
    let after = S.runCombat S.aggressiveAnswer freed
        control = S.runCombat S.aggressiveAnswer bound
    Spec.assertEqWith s "hacked, the Crasher connects for five" (S.lifeOf S.bob after) (Just 15)
    Spec.assertEqWith s "unhacked, it is never declared" (S.attackerDeclarationsOf control) []
    Spec.assertEqWith s "and bob takes nothing" (S.lifeOf S.bob control) (Just 20)

-- castHackAt's twin for a board where alice controls more than one land, and the
-- one thing it does differently is NAME THE LAND THAT PAYS. The Swamp the Bell has
-- animated is itself a mana source, so the shared caster's interpreter -- which
-- takes the head of Pawl.Engine.Cost.chooseSource's candidates -- taps the very
-- creature the cases below are about, and CR 508.1a's "must be untapped" then
-- refuses the attack for a reason that has nothing to do with a text change. That
-- is not hypothetical: it is how this group's first red run lied about which
-- assertions were failing.
castHackPaying :: ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId -> Subtype.Subtype -> Subtype.Subtype -> GameState.GameState -> GameState.GameState
castHackPaying island hackId target from to gs =
  let answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject target))) sets
        Prompt.ChooseLandTypeSwap {} -> (from, to)
        Prompt.ChooseManaSource {} -> Just island
        _ -> S.identityAnswer p
   in S.runPure answer (gs {GameState.priority = Just S.alice}) (do S.cast S.alice hackId; Stack.resolveTop)

-- CR 612.1 reaching the AFFECTED SET of a combat requirement or restriction --
-- the other half of the sentence textChangedCombatRestrictionSpec above proves
-- for the gate. Three readers share the shape and none of them is the layer fold:
-- Pawl.Engine.CombatRestriction.restricted, Pawl.Engine.AttackRequirement.instances
-- and Pawl.Engine.BlockRequirement.instances all take an Affected printed on a
-- permanent and ask Projection.affects about it, so a text change on the source
-- must reach the subtype word inside it (CR 613.11 puts all three after the
-- layers, which is why the layer fold's own rewrite does not cover them).
--
-- SYNTHETIC CARDS, and why. A sweep of the full Oracle bulk corpus (2026-08-06,
-- 38623 oracle entries) for a line carrying combat-requirement or
-- combat-restriction vocabulary alongside a land-type word returns 218 cards, and
-- every one of them puts the land type somewhere OTHER than the affected set:
-- landwalk reminder text, which is a keyword's own word and is covered by
-- textChangedLandwalkSpec above (#523); "can't attack unless defending player
-- controls an Island", which is the gate rather than the affected set (Armored
-- Galleon, defendingPlayerRestrictionSpec); Leviathan's "unless you
-- sacrifice two Islands", which is a cost; and Kraken of the Straits, where the
-- type sits inside a count in a pairwise clause Pawl.Types.CombatRestriction's
-- header argues is not representable. No printing in Magic names a basic land
-- type as the SUBJECT of one of these effects, so the two cards below are
-- written. Nothing in the CR forbids them: CR 508.1c and CR 509.1c describe the
-- effects in terms of the creatures they name, and CR 305.7's basic land types
-- are ordinary subtypes an Affected may match on -- Kormus Bell prints exactly
-- that affected set for a static ability.
--
--   Synthetic Wetland Embargo {2}{W} Enchantment
--     "Swamps can't attack. Islands can't block."
--   Synthetic Wetland Frenzy {2}{R} Enchantment
--     "Swamps attack each combat if able.
--      All creatures able to block Islands do so."
--
-- Each card names TWO DIFFERENT land types on purpose, so one printing gives both
-- directions of the discriminator. The Swamp half is read against animated Swamps
-- and is FREED by a hack; the Island half is read against the same animated
-- Swamps and is BOUND by one. A reader that dropped the restriction or the
-- requirement whenever any text change was present would pass the freeing half
-- and fail the binding half.
--
-- Kormus Bell ("All Swamps are 1/1 black creatures that are still lands") is what
-- makes a land a combat participant at all, and Magical Hack is the text changer.
-- Both are real printings already in the pool.
textChangedCombatAffectedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
textChangedCombatAffectedSpec s registry = Spec.describe s "TextChangedCombatAffected" $ do
  -- alice controls a Kormus Bell, the Swamp it animates into a 1/1, the card under
  -- test, and the Island that pays for the Magical Hack in her hand; `theirs` is
  -- bob's side. With `hacked`, the Hack is cast at the card under test before
  -- anything is declared.
  --
  -- Her OWN Island, and it is never animated: the Bell names Swamps, so the land
  -- that pays for the swap is not also a creature that could attack or block and
  -- confuse a count.
  let board name theirs hacked from to = do
        bell <- S.printingOf s registry "Kormus Bell"
        swamp <- S.printingOf s registry "Swamp"
        island <- S.printingOf s registry "Island"
        magicalHack <- S.printingOf s registry "Magical Hack"
        printing <- S.printingOf s registry name
        let (gs0, ours, yours) = S.combatBoardOf [bell, swamp] theirs
            (islandId, gs1) = S.addPermanent island S.alice gs0
            (sourceId, gs2) = S.addPermanent printing S.alice gs1
            (hackId, gs3) = S.addHandCard magicalHack S.alice gs2
        case ours of
          [_, swampId] -> pure (if hacked then castHackPaying islandId hackId sourceId from to gs3 else gs3, swampId, sourceId, yours)
          _ -> Spec.assertFailure s "fixture should have the Bell and the Swamp"
      embargo = board "Synthetic Wetland Embargo"
      frenzy = board "Synthetic Wetland Frenzy"
  Spec.it s "CR 508.1c the printed Embargo stops the Bell's animated Swamp attacking" $ do
    -- The premise, and the anti-vacuity check for every negative below: the
    -- animated Swamp really is an attack candidate, so "it did not attack" is the
    -- restriction talking rather than a land that was never a creature. Both
    -- worlds on one board -- with the Embargo and without it.
    (gs, swampId, _, _) <- embargo [] False Subtype.Swamp Subtype.Forest
    bell <- S.printingOf s registry "Kormus Bell"
    swamp <- S.printingOf s registry "Swamp"
    let (bare, ours, _) = S.combatBoardOf [bell, swamp] []
    Spec.assertBool s (not (Combat.canAttack S.alice swampId gs)) "under the Embargo the animated Swamp cannot attack"
    Spec.assertEqWith s "and nothing is offered" (Combat.legalAttackers S.alice gs) []
    Spec.assertEqWith s "without it the same Swamp is offered" (Combat.legalAttackers S.alice bare) (drop 1 ours)
  Spec.it s "CR 612.1 a hacked Embargo reads FORESTS and the animated Swamp may attack" $ do
    -- THE FREEING DIRECTION, over the affected set rather than the gate. The board
    -- never moves: the Swamp is still a Swamp and still animated, and only the
    -- Embargo's own printed word changed. Fails against a reader that hands
    -- Projection.affects the printed Affected.
    (gs, swampId, sourceId, _) <- embargo [] True Subtype.Swamp Subtype.Forest
    Spec.assertEqWith s "the Hack resolved onto the Embargo" (Projection.textChangesAffecting sourceId gs) [(Subtype.Swamp, Subtype.Forest)]
    Spec.assertBool s (Combat.canAttack S.alice swampId gs) "the animated Swamp may attack"
    Spec.assertEqWith s "and is offered" (Combat.legalAttackers S.alice gs) [swampId]
  Spec.it s "CR 509.1b a hacked Embargo's block half BINDS a Swamp the printed one left alone" $ do
    -- THE BINDING DIRECTION. The Embargo prints "Islands can't block", and the
    -- only creature on bob's side is a Swamp the Bell animated, so the printed
    -- clause leaves it free; Island -> Swamp rewrites the clause onto it. This is
    -- the half a reader that simply dropped every restriction in the presence of a
    -- text change would fail.
    swamp <- S.printingOf s registry "Swamp"
    (printed, _, _, theirs) <- embargo [swamp] False Subtype.Island Subtype.Swamp
    (hacked, _, sourceId, theirs') <- embargo [swamp] True Subtype.Island Subtype.Swamp
    case (theirs, theirs') of
      ([blocker], [blocker']) -> do
        Spec.assertEqWith s "the Hack resolved onto the Embargo" (Projection.textChangesAffecting sourceId hacked) [(Subtype.Island, Subtype.Swamp)]
        Spec.assertBool s (Combat.canBlock S.bob blocker printed) "under the printed Embargo bob's animated Swamp can block"
        Spec.assertBool s (not (Combat.canBlock S.bob blocker' hacked)) "under the hacked one it cannot"
      _ -> Spec.assertFailure s "fixture should have one blocker"
  Spec.it s "CR 508.1d the printed Frenzy requires the animated Swamp to attack" $ do
    -- The requirement twin of the premise above, and the same anti-vacuity shape:
    -- declining is illegal WITH the Frenzy and legal without it, so the
    -- maximization really is counting an instance minted off the affected set.
    (gs, _, _, _) <- frenzy [] False Subtype.Swamp Subtype.Forest
    bell <- S.printingOf s registry "Kormus Bell"
    swamp <- S.printingOf s registry "Swamp"
    let (bare, _, _) = S.combatBoardOf [bell, swamp] []
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] gs)) "under the Frenzy declining is illegal"
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] bare) "without it declining is legal"
  Spec.it s "CR 612.1 a hacked Frenzy reads FORESTS and the animated Swamp is required no more" $ do
    -- THE FREEING DIRECTION for Pawl.Engine.AttackRequirement.instances. The
    -- positive control rides along: the Swamp may still attack, so the requirement
    -- lifted rather than the creature dropping off CR 508.1a's candidate list.
    (gs, swampId, sourceId, _) <- frenzy [] True Subtype.Swamp Subtype.Forest
    Spec.assertEqWith s "the Hack resolved onto the Frenzy" (Projection.textChangesAffecting sourceId gs) [(Subtype.Swamp, Subtype.Forest)]
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] gs) "declining is legal"
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [swampId] gs) "and attacking is still legal"
  Spec.it s "CR 509.1c a hacked Frenzy's block half BINDS bob's Piker to the animated Swamp" $ do
    -- THE BINDING DIRECTION for Pawl.Engine.BlockRequirement.instances. The Frenzy
    -- prints "All creatures able to block Islands do so" and the lone attacker is
    -- an animated Swamp, so the printed clause mints no instance; Island -> Swamp
    -- makes it mint one, and declining to block stops being a legal answer.
    piker <- S.printingOf s registry "Goblin Piker"
    (printed0, _, _, _) <- frenzy [piker] False Subtype.Island Subtype.Swamp
    (hacked0, swampId, sourceId, theirs) <- frenzy [piker] True Subtype.Island Subtype.Swamp
    let declare g = snd (Engine.runGamePure S.aggressiveAnswer g (Combat.declareAttackers S.manaPerformer S.alice))
        printed = declare printed0
        hacked = declare hacked0
    case theirs of
      [blocker] -> do
        Spec.assertEqWith s "the Hack resolved onto the Frenzy" (Projection.textChangesAffecting sourceId hacked) [(Subtype.Island, Subtype.Swamp)]
        Spec.assertEqWith s "the animated Swamp is the attacker in both worlds" (S.attackerDeclarationsOf hacked) [swampId]
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty printed) "under the printed Frenzy declining to block is legal"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty hacked)) "under the hacked one it is illegal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker (Set.singleton swampId)) hacked) "and blocking is the legal answer"
      _ -> Spec.assertFailure s "fixture should have one blocker"
  Spec.it s "CR 612.1 whole cards: the rewritten affected sets decide a real combat phase" $ do
    -- The gameplay-level case, run through CR 703.4i's turn-based action and the
    -- priority loop rather than a direct call, with an interpreter that would
    -- rather not act. Under the printed Frenzy the animated 1/1 Swamp is
    -- forced to attack an undefended bob for one; under the hacked one nothing is
    -- required and nothing is declared.
    (printed, _, _, _) <- frenzy [] False Subtype.Swamp Subtype.Forest
    (hacked, _, _, _) <- frenzy [] True Subtype.Swamp Subtype.Forest
    let declining :: Prompt.Prompt r -> r
        declining p = case p of
          Prompt.DeclareAttackers {} -> []
          _ -> S.aggressiveAnswer p
        after = S.runCombat declining printed
        control = S.runCombat declining hacked
    Spec.assertEqWith s "printed, the forced Swamp connects for one" (S.lifeOf S.bob after) (Just 19)
    Spec.assertEqWith s "hacked, nothing is declared" (S.attackerDeclarationsOf control) []
    Spec.assertEqWith s "and bob takes nothing" (S.lifeOf S.bob control) (Just 20)

-- CR 122.1b / 702.147a, through the card that puts the counter: Rot-Curse
-- Rakshasa {1}{B} Creature -- Demon 5/5, "Trample", "Decayed" and "Renew --
-- {X}{B}{B}, Exile this card from your graveyard: Put a decayed counter on each
-- of X target creatures. Activate only as a sorcery" (Tarkir: Dragonstorm,
-- checked against Scryfall 2026-08-29; data/cards/rot-curse-rakshasa.json).
--
-- alice has eight Swamps, one Goblin Piker to attack with and the Rakshasa in
-- her graveyard; bob has THREE Pikers, one more than the largest X that resolves
-- below, so the announcement chooses which creatures it lands on rather than
-- taking every candidate the board offers.
--
-- Eight Swamps and not four: the third case announces X as five to watch the
-- activation reverse for want of TARGETS, and on a board that could not pay
-- {5}{B}{B} it would reverse for want of MANA and prove nothing.
--
-- Two boards differing in the announced X alone, because only the pair says the
-- count came from CR 601.2b's announcement rather than from a number written on
-- the card: at X=2 two of bob's Pikers stop blocking and at X=1 only one does.
-- The untouched Piker is the third leg -- it stands on both boards and blocks on
-- both, so a board on which nothing may block fails here rather than passing.
renewBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
renewBoard rakshasa swamp piker =
  let (gyId, withCard) = S.addGraveyardCard rakshasa S.alice (S.landsInPlay swamp 8)
      (attacker, withAttacker) = S.addPermanent piker S.alice withCard
      (theirs, board) =
        List.foldl'
          (\(ids, g) _ -> let (oid, g1) = S.addPermanent piker S.bob g in (ids <> [oid], g1))
          ([], withAttacker)
          [1 .. (3 :: Int)]
   in ( gyId,
        attacker,
        theirs,
        board
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      )

-- Announces X and answers CR 601.2c's targets out of `oids`, by FILTERING the
-- offered set: the pool decides which flavour of Recipient a candidate arrives
-- as, and a hand-built one of another flavour is dropped by CR 608.2b's re-read
-- (Pawl.ActivateSpec's answerXTargeting says the same).
--
-- As many of them as the slot was OFFERED, rather than all of them: an offer of
-- the wrong size then lands counters on the wrong creatures instead of failing
-- Target.selectionLegal and reversing the activation whole, which is what leaves
-- the announced count itself observable on the board.
renewing :: Natural.Natural -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
renewing x oids p = case p of
  Prompt.ChooseX {} -> x
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(count, legal) -> Set.take (Natural.toIntSaturating count) (Set.filter (maybe False (`elem` oids) . Recipient.objectOf) legal)) sets
  _ -> S.identityAnswer p

-- S.combatBoardOf's own board shape, taken over a board a test built for itself:
-- alice active in her declare-attackers step, with bob the defending player by CR
-- 506.2's second sentence. Stated rather than derived, for that fixture's reason
-- -- a direct-call test never runs the turn-based action that would fill it in.
declaringAttackers :: GameState.GameState -> GameState.GameState
declaringAttackers gs =
  S.runPure
    S.aggressiveAnswer
    gs
      { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
        GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.bob]}
      }
    (Combat.declareAttackers S.manaPerformer S.alice)

-- CR 122.1b: a keyword counter grants its keyword, so a creature carrying a
-- decayed counter has rule 702.147a's "This creature can't block" -- and the
-- short-circuit Pawl.Engine.CombatRestriction.inForce takes before it looks for a
-- minting keyword has to see the counter, no permanent on the board printing or
-- granting one.
keywordCounterRestrictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
keywordCounterRestrictionSpec s registry = Spec.describe s "KeywordCounterRestriction" $ do
  Spec.it s "CR 122.1b two decayed counters, announced as X, stop both creatures blocking" $ do
    rakshasa <- S.printingOf s registry "Rot-Curse Rakshasa"
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gyId, attacker, theirs, gs) = renewBoard rakshasa swamp piker
    case (Activate.abilitiesFor gyId gs, theirs) of
      ([ability], [first_, second, spared]) -> do
        let resolved = S.runPure (renewing 2 [first_, second]) gs (Activate.activateAbility S.alice gyId ability >> Stack.resolveTop)
            board = declaringAttackers resolved
            decayed = CounterKind.Keyword Keyword.Decayed
        -- CR 509.1b, the behaviour this case exists for: the counter alone forbids
        -- the block, and the Piker beside it is what says the answer is about
        -- those two creatures rather than about the board.
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton first_ (Set.singleton attacker)) board)) "blocking with the first counter-bearer is illegal"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton second (Set.singleton attacker)) board)) "and so is blocking with the second"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton spared (Set.singleton attacker)) board) "the Piker that got no counter still blocks"
        Spec.assertEqWith s "and it is the only blocker offered" (Combat.legalBlockers S.bob board) [spared]
        -- CR 601.2c: the announcement put the counters on exactly the two
        -- creatures it named, which is what makes the block answers above about
        -- the counter rather than about bob's board.
        Spec.assertEqWith
          s
          "two of the three Pikers carry one decayed counter each"
          (fmap (\oid -> S.counterOf decayed oid resolved) theirs)
          [1, 1, 0]
        -- The rest of the activation: CR 602.2b routes it through CR 601.2b-i, so
        -- the {X}{B}{B} was paid and CR 406.2's exile paid the rest of the cost.
        -- The Rakshasa is in exile rather than the graveyard it was activated
        -- from.
        Spec.assertEqWith s "the Rakshasa exiled itself to pay for it" (Game.zoneMembers Zone.Graveyard S.alice resolved, length (Game.zoneMembers Zone.Exile S.alice resolved)) ([], 1)
      (abilities, _) -> Spec.assertEqWith s "exactly one ability to activate, on three Pikers" (length abilities) 1
  -- The pair's other half: the same board, the same card, X announced as one. The
  -- count follows the announcement, so the second Piker keeps blocking.
  Spec.it s "CR 601.2c announcing X as one leaves the second creature blocking" $ do
    rakshasa <- S.printingOf s registry "Rot-Curse Rakshasa"
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gyId, attacker, theirs, gs) = renewBoard rakshasa swamp piker
    case (Activate.abilitiesFor gyId gs, theirs) of
      ([ability], [first_, second, spared]) -> do
        let resolved = S.runPure (renewing 1 [first_]) gs (Activate.activateAbility S.alice gyId ability >> Stack.resolveTop)
            board = declaringAttackers resolved
            decayed = CounterKind.Keyword Keyword.Decayed
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton first_ (Set.singleton attacker)) board)) "the one creature it named cannot block"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton second (Set.singleton attacker)) board) "the one X=2 would have named still can"
        Spec.assertEqWith s "and both untouched Pikers are offered" (Combat.legalBlockers S.bob board) [second, spared]
        Spec.assertEqWith
          s
          "only the named Piker carries a decayed counter"
          (fmap (\oid -> S.counterOf decayed oid resolved) theirs)
          [1, 0, 0]
      (abilities, _) -> Spec.assertEqWith s "exactly one ability to activate, on three Pikers" (length abilities) 1
  -- CR 601.2c against CR 601.2b's freedom: X is announced without reference to the
  -- board, so an X larger than the creatures available is announceable and then
  -- unfillable -- whereupon CR 601.2e, which CR 602.2b routes an activation
  -- through, returns the game to before the activation was proposed.
  -- Reject-not-repair: the counters do not land on the four creatures there ARE.
  Spec.it s "CR 601.2c announcing more X than there are creatures reverses the whole activation" $ do
    rakshasa <- S.printingOf s registry "Rot-Curse Rakshasa"
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gyId, attacker, theirs, gs) = renewBoard rakshasa swamp piker
    case (Activate.abilitiesFor gyId gs, theirs) of
      ([ability], [first_, _, _]) -> do
        -- FIVE, against the four creatures the board holds -- bob's three and
        -- alice's attacker, since "X target creatures" names no controller -- and
        -- answered with every one of them, so the announcement fails on the number
        -- alone rather than on an answer that left a legal creature out.
        let resolved = S.runPure (renewing 5 (attacker : theirs)) gs (Activate.activateAbility S.alice gyId ability >> Stack.resolveTop)
            board = declaringAttackers resolved
            decayed = CounterKind.Keyword Keyword.Decayed
        Spec.assertEqWith
          s
          "no creature carries a decayed counter"
          (fmap (\oid -> S.counterOf decayed oid resolved) (attacker : theirs))
          [0, 0, 0, 0]
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton first_ (Set.singleton attacker)) board) "so the first Piker still blocks"
        Spec.assertEqWith s "and all three are offered" (Combat.legalBlockers S.bob board) theirs
        -- CR 601.2e's reversal is of the WHOLE activation, so the cost is unpaid
        -- too: the Rakshasa is in the graveyard it would have exiled itself from.
        Spec.assertEqWith s "the Rakshasa never left the graveyard" (Game.zoneMembers Zone.Graveyard S.alice resolved) [gyId]
      (abilities, _) -> Spec.assertEqWith s "exactly one ability to activate, on three Pikers" (length abilities) 1

-- CR 509.1b / 611.1 / 613.11: a restriction a RESOLUTION hands a creature for a
-- duration, which neither the group above's counter nor the printed rows further
-- up can state -- the counter's restriction is indefinite, and a printed row is
-- gathered live off a source standing on the battlefield. Zirda, the Dawnwaker's
-- "{1}, {T}: Target creature can't block this turn" (checked against Scryfall) is
-- the pool's printing; the row lands in GameState.blockProhibitions and
-- Pawl.Engine.CombatRestriction.blockProhibited is what reads it.
--
-- THE PAIR that makes these cases discriminating: the SAME activation is made on
-- both boards, for the same {1} and the same tap, and the two differ only in
-- which creature the one target slot named -- one of bob's twins, or alice's own
-- attacker, whose block the restriction has nothing to reach. So a green control
-- leg cannot come from an unpaid cost, an untapped Zirda, or a board on which
-- nobody could have blocked.
storedBlockRestrictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
storedBlockRestrictionSpec s registry = Spec.describe s "StoredBlockRestriction" $ do
  Spec.it s "CR 509.1b the creature Zirda named cannot block, and its twin still does" $ do
    (attacker, victim, twin, resolved) <- zirdaResolved s registry (\_ v _ -> v)
    let declaring = declaringAttackers resolved
        after = S.runPure S.aggressiveAnswer declaring (Combat.declareBlockers S.manaPerformer)
    Spec.assertEqWith s "only the twin ended up blocking" (Combat.blockersOf attacker after) (Set.singleton twin)
    Spec.assertBool s (not (Combat.canBlock S.bob victim declaring)) "and the named creature is off CR 509.1a's candidate list"
    Spec.assertEqWith s "one restriction was stored, over the creature named" (fmap ActiveBlockProhibition.object (GameState.blockProhibitions resolved)) [victim]
  -- The pair's other half: the same activation, aimed at alice's attacker.
  Spec.it s "CR 509.1b aimed elsewhere, both of bob's twins block" $ do
    (attacker, victim, twin, resolved) <- zirdaResolved s registry (\a _ _ -> a)
    let after = S.runPure S.aggressiveAnswer (declaringAttackers resolved) (Combat.declareBlockers S.manaPerformer)
    Spec.assertEqWith s "both twins blocked" (Combat.blockersOf attacker after) (Set.fromList [victim, twin])
    Spec.assertEqWith s "and the restriction was stored all the same, over the attacker" (fmap ActiveBlockProhibition.object (GameState.blockProhibitions resolved)) [attacker]
  -- CR 514.2 / 611.2a: "this turn" arms Expiry.AtCleanup, so the cleanup sweep
  -- drops the row and the creature blocks again. Through the sweep directly,
  -- which is the narrowest path that shows it.
  Spec.it s "CR 514.2 the restriction ends at cleanup" $ do
    (_, victim, _, resolved) <- zirdaResolved s registry (\_ v _ -> v)
    let swept = Expiry.dropAtCleanup resolved
    Spec.assertBool s (not (Combat.canBlock S.bob victim resolved)) "restricted on the turn it resolved"
    Spec.assertBool s (Combat.canBlock S.bob victim swept) "and blocking again once the turn's cleanup has run"
    Spec.assertEqWith s "with nothing left stored" (GameState.blockProhibitions swept) []
  -- CR 509.1c is CR 508.1d's textual mirror -- both count the requirements that
  -- could be obeyed "without disobeying any restrictions" -- and this side
  -- reaches it by the same route storedAttackRestrictionSpec's last case proves
  -- one rule over: the prohibition takes the Screen off
  -- Pawl.Engine.Combat.legalBlockersGiven, which is the candidate list
  -- Pawl.Engine.BlockRequirement.instances mints against, so the maximum drops to
  -- zero and declining becomes legal. The control is the SAME Zirda activation
  -- aimed at alice's attacker, where the Screen must still block.
  Spec.it s "CR 509.1c a required blocker the restriction covers may decline after all" $ do
    (restrained, control, wall, attacker) <- zirdaScreenBoards s registry
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty restrained) "the Screen may decline once it can't block"
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty control)) "while the control's Screen may not"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton wall (Set.singleton attacker)) control) "which blocking the lone attacker satisfies"
    Spec.assertEqWith s "nothing is offered on the restricted board" (Combat.legalBlockers S.bob restrained) []
    Spec.assertEqWith s "and the Screen is offered on the control" (Combat.legalBlockers S.bob control) [wall]
    Spec.assertEqWith s "with the same lone attacker declared on each" (fmap declaredAttackers [restrained, control]) [[attacker], [attacker]]

-- Alice's Zirda, activated once and resolved, over a board of her own attacker
-- and two of bob's identical blockers. `pick` chooses which of the three the one
-- target slot names, and is the only thing the boards above differ in.
zirdaResolved ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  (ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId) ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
zirdaResolved s registry pick = do
  zirda <- S.printingOf s registry "Zirda, the Dawnwaker"
  mountain <- S.printingOf s registry "Mountain"
  piker <- S.printingOf s registry "Goblin Piker"
  let (zirdaId, withZirda) = S.addPermanent zirda S.alice (S.landsInPlay mountain 2)
      (attacker, withAttacker) = S.addPermanent piker S.alice withZirda
      (victim, withVictim) = S.addPermanent piker S.bob withAttacker
      (twin, placed) = S.addPermanent piker S.bob withVictim
      board =
        placed
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
      abilities = Activate.abilitiesFor zirdaId board
      resolved = activatingZirda zirdaId (pick attacker victim twin) board
  Spec.assertEqWith s "Zirda states exactly one activated ability" (length abilities) 1
  pure (attacker, victim, twin, resolved)

-- The CR 509.1c board: alice's Zirda and one Goblin Piker against bob's lone
-- Razorgrass Screen ({1} Artifact Creature -- Wall 2/1, "Defender. This creature
-- blocks each combat if able." -- checked against Scryfall, 2026-08-30), with the
-- same Zirda activation aimed at the Screen (the first state) and at alice's
-- attacker (the second). Zirda pays {T}, so it is tapped and attacks on neither,
-- and attackers are declared after the activation, leaving both boards facing one
-- Piker.
zirdaScreenBoards ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (GameState.GameState, GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
zirdaScreenBoards s registry = do
  zirda <- S.printingOf s registry "Zirda, the Dawnwaker"
  mountain <- S.printingOf s registry "Mountain"
  piker <- S.printingOf s registry "Goblin Piker"
  screen <- S.printingOf s registry "Razorgrass Screen"
  let (zirdaId, withZirda) = S.addPermanent zirda S.alice (S.landsInPlay mountain 2)
      (attacker, withAttacker) = S.addPermanent piker S.alice withZirda
      (wall, placed) = S.addPermanent screen S.bob withAttacker
      board = mainPhaseFor placed
      abilities = Activate.abilitiesFor zirdaId board
      run named = declaringAttackers (activatingZirda zirdaId named board)
  Spec.assertEqWith s "Zirda states exactly one activated ability" (length abilities) 1
  pure (run wall, run attacker, wall, attacker)

-- Zirda's one ability, activated and resolved with its target slot aimed at
-- `named` -- activatingNetter's twin, and the only thing the boards either Zirda
-- fixture builds differ in.
activatingZirda :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
activatingZirda zirdaId named board = case Activate.abilitiesFor zirdaId board of
  [ability] -> S.runPure (namingTarget named) board (Activate.activateAbility S.alice zirdaId ability >> Stack.resolveTop)
  _ -> board

-- Aim Zirda's one target slot at this permanent, PINNED by filtering the offered
-- set rather than built from the id: CR 115.1's pool of creatures offers
-- Recipient.ToCreature, and a hand-built Recipient.ToObject of the same permanent
-- is a different recipient that CR 608.2b's re-read drops silently. Filtering also
-- stops the answerer repairing a mutation by finding whatever is still legal.
namingTarget :: ObjectId.ObjectId -> Prompt.Prompt r -> r
namingTarget oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, offered) -> Set.filter ((== Just oid) . Recipient.objectOf) offered) sets
  _ -> S.identityAnswer p

-- CR 508.1c / 611.1 / 613.11: the group above's twin one rule over -- a stored
-- ATTACKING restriction, which no printed row can state for a creature whose
-- source has left. Netter en-Dal's "{W}, {T}, Discard a card: Target creature
-- can't attack this turn" (checked against Scryfall) is the pool's printing; the
-- row lands in GameState.attackProhibitions and
-- Pawl.Engine.CombatRestriction.attackProhibited is what reads it.
--
-- THE PAIR: the same activation is made on every board here, for the same {W},
-- the same tap and the same discard, and the boards differ only in which creature
-- the one target slot named -- one of alice's two identical Pikers, or bob's,
-- whose attack the restriction has nothing to reach. A green control leg
-- therefore cannot come from an unpaid cost, an empty hand, or a board on which
-- nobody could have attacked.
storedAttackRestrictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
storedAttackRestrictionSpec s registry = Spec.describe s "StoredAttackRestriction" $ do
  Spec.it s "CR 508.1c the creature Netter en-Dal named cannot attack, and its twin still does" $ do
    (victim, twin, _, resolved) <- netterResolved s registry (\v _ _ -> v)
    let after = declaringAttackers resolved
    Spec.assertEqWith s "only the twin ended up attacking" (declaredAttackers after) [twin]
    Spec.assertBool s (not (Combat.canAttack S.alice victim resolved)) "and the named creature is off CR 508.1a's candidate list"
    Spec.assertEqWith s "one restriction was stored, over the creature named" (fmap ActiveAttackProhibition.affected (GameState.attackProhibitions resolved)) [RestrictedCreatures.Named victim]
  -- The pair's other half: the same activation, aimed at bob's Piker.
  Spec.it s "CR 508.1c aimed elsewhere, both of alice's twins attack" $ do
    (victim, twin, elsewhere, resolved) <- netterResolved s registry (\_ _ e -> e)
    let after = declaringAttackers resolved
    Spec.assertEqWith s "both twins attacked" (declaredAttackers after) [victim, twin]
    Spec.assertEqWith s "and the restriction was stored all the same, over bob's Piker" (fmap ActiveAttackProhibition.affected (GameState.attackProhibitions resolved)) [RestrictedCreatures.Named elsewhere]
  -- CR 514.2 / 611.2a: "this turn" arms Expiry.AtCleanup, so the cleanup sweep
  -- drops the row and the creature attacks again. Through the sweep directly,
  -- which is the narrowest path that shows it.
  Spec.it s "CR 514.2 the restriction ends at cleanup" $ do
    (victim, _, _, resolved) <- netterResolved s registry (\v _ _ -> v)
    let swept = Expiry.dropAtCleanup resolved
    Spec.assertBool s (not (Combat.canAttack S.alice victim resolved)) "restricted on the turn it resolved"
    Spec.assertBool s (Combat.canAttack S.alice victim swept) "and attacking again once the turn's cleanup has run"
    Spec.assertEqWith s "with nothing left stored" (GameState.attackProhibitions swept) []
  -- CR 508.1d counts requirements obeyed "without disobeying any restrictions",
  -- so a stored restriction does not deadlock a Curse of the Nightly Hunt -- it
  -- takes the creature off Pawl.Engine.Combat.legalAttackers, which is the
  -- candidate list Pawl.Engine.AttackRequirement.instances mints against, and the
  -- maximum drops to zero. The control is the SAME Curse and the SAME activation
  -- aimed at bob's Piker, where declining stays illegal. CR 509.1c is this
  -- sentence's mirror, not its opposite: storedBlockRestrictionSpec's last case
  -- is the same shape one rule over.
  Spec.it s "CR 508.1d a required creature the restriction covers may decline after all" $ do
    -- CR 506.2's defending player has to be stated, because CR 508.1d mints an
    -- instance per (creature, announcement) pair and a board with no defender
    -- offers no announcement -- on which declining is trivially legal and the
    -- control leg cannot fail.
    (restrained, control, piker1) <- cursedNetterBoards s registry
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] restrained) "the required Piker may decline once it can't attack"
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] control)) "while the control's required Piker may not"
    Spec.assertEqWith s "nothing is offered on the restricted board" (Combat.legalAttackers S.alice restrained) []
    Spec.assertEqWith s "and the Piker is offered on the control" (Combat.legalAttackers S.alice control) [piker1]

-- Alice's Netter en-Dal, activated once and resolved, over a board of her two
-- identical Pikers and one of bob's. `pick` chooses which of the three the one
-- target slot names, and is the only thing the boards above differ in. The single
-- card in alice's hand is the discard the cost takes, and the one Plains is its
-- {W}.
netterResolved ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  (ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId) ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
netterResolved s registry pick = do
  netter <- S.printingOf s registry "Netter en-Dal"
  plains <- S.printingOf s registry "Plains"
  piker <- S.printingOf s registry "Goblin Piker"
  let (netterId, withNetter) = S.addPermanent netter S.alice (S.landsInPlay plains 1)
      (_, withCard) = S.addHandCard plains S.alice withNetter
      (victim, withVictim) = S.addPermanent piker S.alice withCard
      (twin, withTwin) = S.addPermanent piker S.alice withVictim
      (elsewhere, placed) = S.addPermanent piker S.bob withTwin
      board = mainPhaseFor placed
      abilities = Activate.abilitiesFor netterId board
      resolved = activatingNetter netterId (pick victim twin elsewhere) board
  Spec.assertEqWith s "Netter en-Dal states exactly one activated ability" (length abilities) 1
  pure (victim, twin, elsewhere, resolved)

-- The CR 508.1d board: a Curse of the Nightly Hunt on alice over one Piker of
-- hers, with the same Netter en-Dal activation aimed at that Piker (the first
-- state) and at bob's (the second). Netter itself pays {T}, so it is tapped and
-- required of nothing.
cursedNetterBoards ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (GameState.GameState, GameState.GameState, ObjectId.ObjectId)
cursedNetterBoards s registry = do
  netter <- S.printingOf s registry "Netter en-Dal"
  plains <- S.printingOf s registry "Plains"
  piker <- S.printingOf s registry "Goblin Piker"
  curse <- S.printingOf s registry "Curse of the Nightly Hunt"
  let (netterId, withNetter) = S.addPermanent netter S.alice (S.landsInPlay plains 1)
      (_, withCard) = S.addHandCard plains S.alice withNetter
      (piker1, withPiker) = S.addPermanent piker S.alice withCard
      (elsewhere, withBob) = S.addPermanent piker S.bob withPiker
      (aura, withAura) = S.addPermanent curse S.alice withBob
      board = mainPhaseFor (S.attachTo aura (Recipient.ToPlayer S.alice) withAura)
      abilities = Activate.abilitiesFor netterId board
      run named = activatingNetter netterId named board
  Spec.assertEqWith s "Netter en-Dal states exactly one activated ability" (length abilities) 1
  pure (facingBob (run piker1), facingBob (run elsewhere), piker1)

-- CR 506.2: alice in her declare-attackers step with bob the defending player,
-- stated rather than derived for `declaringAttackers`' reason -- a direct call to
-- Pawl.Engine.Combat.legalAttackDeclaration never runs the turn-based action that
-- would fill it in.
facingBob :: GameState.GameState -> GameState.GameState
facingBob gs =
  gs
    { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
      GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.bob]}
    }

-- Alice active with priority in her precombat main phase, which is when the
-- activation above is made.
-- CR 611.2c's third sentence on the stored carrier: a resolving "creatures can't
-- attack you" modifies no characteristic and no controller, so it modifies the
-- rules and reaches creatures that were not on the battlefield when it began --
-- a CLASS, where Netter en-Dal's row above froze one id. Chronomantic Escape
-- ({4}{W}{W} Sorcery, "Until your next turn, creatures can't attack you. Exile
-- Chronomantic Escape with three time counters on it. / Suspend 3--{2}{W}" --
-- checked against Scryfall, 2026-09-02) is the pool's producer, and the row it
-- stores is read at Pawl.Engine.CombatRestriction.cantAttackPlayer beside the
-- printed carrier's.
--
-- Not implemented, and STRICTER than printed in both places: the suspend keyword
-- and the self-exile with time counters (#3340), so pawl's card is cast for its
-- mana cost alone and goes to the graveyard as CR 608.2n says, never returning.
-- Neither reaches the restriction this group is about.
--
-- THREE SEATS with both opponents defending (CR 802.2), the only board on which
-- "can't attack you" and a blanket "can't attack" come apart for a creature with
-- no planeswalker to aim at, and the pair: the same board with the Escape left
-- in alice's hand.
storedClassAttackRestrictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
storedClassAttackRestrictionSpec s registry = Spec.describe s "StoredClassAttackRestriction" $ do
  Spec.it s "CR 611.2c whole cards: a creature that entered after the Escape resolved still can't attack alice" $ do
    -- GAMEPLAY LEVEL, from bob's beginning of combat step so CR 703.4h picks
    -- both defending players (CR 802.2) and CR 703.4i declares, under an answerer
    -- that aims at alice wherever she is offered. The Pikers are 2/1s and nobody
    -- has a blocker, so each life total is one creature's doing.
    (early, late, restricted, _, control) <- escapeBoards s registry
    let after = S.runCombat (S.attackTo S.alice) (bobsCombat restricted)
        opened = S.runCombat (S.attackTo S.alice) (bobsCombat control)
    Spec.assertEqWith s "alice takes nothing, the late Piker included" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "and carol takes both Pikers' 4" (S.lifeOf S.carol after) (Just 16)
    Spec.assertEqWith s "both were declared, since carol is still attackable" (S.attackerDeclarationsOf after) [early, late]
    Spec.assertEqWith s "with the Escape uncast, alice takes both" (S.lifeOf S.alice opened) (Just 16)
    Spec.assertEqWith s "and carol nothing" (S.lifeOf S.carol opened) (Just 20)
  Spec.it s "CR 802.3a the announcement, not the creature, is what the row refuses" $ do
    (early, late, restricted, lateControl, control) <- escapeBoards s registry
    let declaring = bobDeclaring restricted
        open = bobDeclaring control
    Spec.assertBool s (not (Combat.legalAttackDeclarationAs S.bob [(late, AttackTarget.OfPlayer S.alice)] declaring)) "CR 611.2c: the Piker that entered after resolution may not be announced at alice"
    Spec.assertBool s (Combat.legalAttackDeclarationAs S.bob [(late, AttackTarget.OfPlayer S.carol)] declaring) "and may at carol, whom the Escape names nowhere"
    Spec.assertBool s (not (Combat.legalAttackDeclarationAs S.bob [(early, AttackTarget.OfPlayer S.alice)] declaring)) "the Piker that was there all along may not either"
    Spec.assertBool s (Combat.legalAttackDeclarationAs S.bob [(lateControl, AttackTarget.OfPlayer S.alice)] open) "and on the pair, alice is fair game"
    -- CR 508.1a is untouched: the row is about the ANNOUNCEMENT, so both Pikers
    -- stay candidates and it is the declaration that is refused.
    Spec.assertEqWith s "both Pikers are still offered" (Combat.legalAttackers S.bob declaring) [early, late]
    Spec.assertEqWith s "one row was stored, a class aimed at alice's seat" (fmap ActiveAttackProhibition.affected (GameState.attackProhibitions declaring)) [RestrictedCreatures.Matching (Filter.HasCardType CardType.Creature)]
  -- CR 611.2a: "until your next turn" is Expiry.AtTurnOf alice, so the row
  -- survives alice's own cleanup, bob's whole turn and carol's, and is gone as
  -- alice's begins. Through Engine.handoffTurn, the road
  -- Pawl.Engine.Expiry.dropAtTurnOf fires on, with CR 514.2's sweep run ahead of
  -- each handoff -- which is what tells this duration from "this turn": under
  -- Expiry.AtCleanup the row is gone before bob's turn ever begins.
  Spec.it s "CR 611.2a the restriction outlasts every other seat's turn and ends as alice's begins" $ do
    (_, late, restricted, _, _) <- escapeBoards s registry
    let carolsTurn = handoff restricted
        alicesTurn = handoff carolsTurn
        bobsSecondTurn = handoff alicesTurn
    Spec.assertEqWith s "carol is active" (GameState.activePlayer carolsTurn) S.carol
    Spec.assertBool s (not (Combat.legalAttackDeclarationAs S.bob [(late, AttackTarget.OfPlayer S.alice)] (bobDeclaring carolsTurn))) "still in force through carol's turn"
    Spec.assertEqWith s "alice is active" (GameState.activePlayer alicesTurn) S.alice
    Spec.assertEqWith s "and nothing is stored once her turn has begun" (GameState.attackProhibitions alicesTurn) []
    Spec.assertEqWith s "bob is active again" (GameState.activePlayer bobsSecondTurn) S.bob
    Spec.assertBool s (Combat.legalAttackDeclarationAs S.bob [(late, AttackTarget.OfPlayer S.alice)] (bobDeclaring bobsSecondTurn)) "and his Piker may attack alice on his next turn"

-- Alice's Chronomantic Escape, cast from her hand off six Plains in her
-- precombat main phase and resolved, then the turn handed to bob. Bob's `early`
-- Piker was on the battlefield as the Escape resolved; his `late` one is placed
-- AFTER the handoff, so it was nowhere when the effect began -- the fixture
-- settles it (S.addPermanent writes Sickness.Settled), standing in for haste. The
-- pair is the same board, in the same order, with the Escape left in alice's
-- hand -- its late Piker under its own id, since casting spends fresh ones (CR
-- 400.7).
escapeBoards ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId, GameState.GameState)
escapeBoards s registry = do
  escape <- S.printingOf s registry "Chronomantic Escape"
  plains <- S.printingOf s registry "Plains"
  piker <- S.printingOf s registry "Goblin Piker"
  let (early, withEarly) = S.addPermanent piker S.bob (S.landsFor plains S.alice 6 S.threePlayerGame)
      (escapeId, withCard) = S.addHandCard escape S.alice withEarly
      board = mainPhaseFor withCard
      resolved = S.runPure S.identityAnswer board (S.cast S.alice escapeId >> Stack.resolveTop)
      (late, restricted) = S.addPermanent piker S.bob (handoff resolved)
      (lateControl, control) = S.addPermanent piker S.bob (handoff board)
  Spec.assertEqWith s "the Escape resolved and stored one row" (length (GameState.attackProhibitions resolved)) 1
  Spec.assertEqWith s "and left the pair nothing" (GameState.attackProhibitions control) []
  pure (early, late, restricted, lateControl, control)

-- Bob's turn at its beginning of combat step, with the combat phase's steps
-- ahead of it: `handoff` leaves the board at his untap step, and S.runCombat
-- runs only while inside the combat phase. S.threePlayerCombat's shape.
bobsCombat :: GameState.GameState -> GameState.GameState
bobsCombat gs =
  gs
    { GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
      GameState.combat = Combat.emptyCombat,
      GameState.remaining = S.phasesAfterThroughPostcombatMain (Phase.Combat CombatStep.BeginningOfCombat)
    }

-- CR 506.2 / 802.2: bob in his declare attackers step with both opponents
-- defending, stated rather than derived for `declaringAttackers`' reason.
bobDeclaring :: GameState.GameState -> GameState.GameState
bobDeclaring gs =
  gs
    { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
      GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.alice, S.carol]}
    }

-- CR 514.2 then CR 500.7: this turn's cleanup sweep, then the next seat's turn
-- begins. Engine.handoffTurn is the seat walk alone, so the sweep is stated --
-- without it a "this turn" row would survive into bob's turn and every case
-- above would be green under Expiry.AtCleanup. Pawl.ExpirySpec's helper of the
-- same name with the sweep folded in, duplicated rather than hoisted.
handoff :: GameState.GameState -> GameState.GameState
handoff gs = S.runPure S.identityAnswer (Expiry.dropAtCleanup gs) Engine.handoffTurn

mainPhaseFor :: GameState.GameState -> GameState.GameState
mainPhaseFor gs =
  gs
    { GameState.activePlayer = S.alice,
      GameState.phase = Phase.PrecombatMain,
      GameState.priority = Just S.alice
    }

-- Netter en-Dal's one ability, activated and resolved with its target slot aimed
-- at `named`.
activatingNetter :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
activatingNetter netterId named board = case Activate.abilitiesFor netterId board of
  [ability] -> S.runPure (namingTarget named) board (Activate.activateAbility S.alice netterId ability >> Stack.resolveTop)
  _ -> board

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Combat" $ do
  declareSpec s registry
  combatDamageSpec s registry
  defenderSpec s registry
  defendingPlayerSpec s registry
  attackMultiplePlayersSpec s registry
  attackLeftRightSpec s registry
  hasteSpec s registry
  evasionSpec s registry
  textChangedLandwalkSpec s registry
  landwalkFamilyRemovalSpec s registry
  menaceSpec s registry
  blockPermissionSpec s registry
  blockRequirementSpec s registry
  attackRequirementSpec s registry
  combatRestrictionSpec s registry
  keywordCounterRestrictionSpec s registry
  storedBlockRestrictionSpec s registry
  storedAttackRestrictionSpec s registry
  storedClassAttackRestrictionSpec s registry
  suspectedAbilityRemovalSpec s registry
  conditionalCombatRestrictionSpec s registry
  defendingPlayerRestrictionSpec s registry
  aimedAttackRestrictionSpec s registry
  perDefenderRestrictionSpec s registry
  textChangedCombatRestrictionSpec s registry
  textChangedCombatAffectedSpec s registry
  controlChangeSicknessSpec s registry
