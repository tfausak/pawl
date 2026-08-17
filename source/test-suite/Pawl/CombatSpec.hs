{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Combat: attack/block legality, combat damage, and the combat
-- keywords (flying, reach, defender, vigilance, haste, first/double strike).
-- Also Pawl.Engine.BlockRequirement, whose only consumer is Pawl.Engine.Combat's CR 509.1c
-- check, Pawl.Engine.AttackRequirement, whose only consumer is its CR 508.1d
-- check, Pawl.Engine.CombatRestriction, whose only consumer is that module's CR
-- 508.1c and CR 509.1b checks, Pawl.Engine.AttackCost, whose only consumer is
-- its CR 508.1d cost clause and CR 508.1h total, and Pawl.Engine.BlockPermission,
-- whose only consumer is its CR 509.1a arity.
module Pawl.CombatSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.BlocksDeclared as BlocksDeclared
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Zone as Zone

combatDamageSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
combatDamageSpec s registry = Spec.describe s "CombatDamage" $ do
  Spec.it s "CR 510.1b an unblocked attacker damages the defending player" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoard piker 1 0
        after = S.fightWith S.aggressiveAnswer gs
    -- A Piker is a 2/1, and bob starts at 20.
    Spec.assertEqWith s "bob took 2" (S.lifeOf S.bob after) (Just 18)
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
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoard piker 1 2
        cheat :: Prompt.Prompt r -> r
        cheat p = case p of
          Prompt.AssignCombatDamage _ _ _ thresholds _ -> Map.fromList (fmap (\r -> (r, 99)) (filter S.isCreatureRecipient (Map.keys thresholds)))
          _ -> S.aggressiveAnswer p
        after = S.settleSba (S.fightWith cheat gs)
    Spec.assertEqWith s "both blockers survive" (S.creaturesInPlay S.bob after) 2
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
        after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
    Spec.assertEqWith s "one attacker" (declaredAttackers after) mine
    Spec.assertEqWith s "tapped" (S.tappedCount S.alice after) 1
  Spec.it s "CR 508.1 attackers attack the defending player" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoard piker 1 1
        after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
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
        after = snd (Engine.runGamePure liar gs (Combat.declareAttackers S.alice))
    Spec.assertEqWith s "nothing attacks" (declaredAttackers after) []
  Spec.it s "CR 509.1 a blocker is recorded against the attacker it blocks" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = S.combatBoard piker 1 1
        steps = do
          Combat.declareAttackers S.alice
          Combat.declareBlockers
        after = snd (Engine.runGamePure S.aggressiveAnswer gs steps)
    case mine of
      [] -> Spec.assertFailure s "fixture should have an attacker"
      attacker : _ ->
        Spec.assertEqWith s "blocked by bob's creature" (Combat.blockersOf attacker after) (Set.fromList theirs)
  Spec.it s "an unblocked attacker has no blockers" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoard piker 1 0
        steps = do
          Combat.declareAttackers S.alice
          Combat.declareBlockers
        after = snd (Engine.runGamePure S.aggressiveAnswer gs steps)
    case mine of
      [] -> Spec.assertFailure s "fixture should have an attacker"
      attacker : _ -> Spec.assertBool s (not (Combat.isBlocked attacker after)) "unblocked"
  Spec.it s "no legal attackers means no prompt and no attacks" $ do
    -- combatBoard 0 1 gives alice nothing. A prompt here would be the engine
    -- asking a question with exactly one answer.
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoard piker 0 1
        after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
    Spec.assertEqWith s "nothing attacks" (declaredAttackers after) []
  -- The end-to-end summoning sickness scenario the spec names: a creature
  -- that just arrived cannot attack, and the SAME creature can once its
  -- controller's untap step has settled it. The halves are tested in Tasks 1
  -- and 4; this proves they compose.
  Spec.it s "CR 302.6 a creature cannot attack the turn it arrives, and can after untapping" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoard piker 1 1
        arrived = justArrived gs
        sameTurn = snd (Engine.runGamePure S.aggressiveAnswer arrived (Combat.declareAttackers S.alice))
        nextTurn =
          snd
            . Engine.runGamePure S.aggressiveAnswer arrived
            $ do
              Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap)
              Combat.declareAttackers S.alice
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
-- controller makes their choices, and Combat.chooseDefender's
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
  let (after, seen) = State.runState (fmap snd (Engine.runGame recordingBlockers gs Combat.declareBlockers)) ([], [])
   in (seen, after)

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
            (fmap snd (Engine.runGame (choosesDefender S.carol) S.threePlayerGame (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))))
            []
    Spec.assertEqWith s "carol is the defending player" (Combat.Type.defender (GameState.combat after)) (Just S.carol)
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
    -- named in the prompt but carol is who must be asked. Combat.chooseDefender
    -- gets this right by computing `Decide.deciderFor pid gs` rather than
    -- defaulting to `Decider.MkDecider pid`.
    --
    -- Discriminates exactly that regression: a `chooseDefender` that used
    -- `Decider.MkDecider pid` (the raw active player, alice) instead of
    -- `Decide.deciderFor pid gs` would record `[Decider.MkDecider S.alice]`
    -- below -- handing alice's own choice back to her, which is the CR 723.1
    -- violation this test exists to catch -- and none of the other six cases in
    -- this group sets activeControl, so none of them would notice.
    let controlled = S.threePlayerGame {GameState.activeControl = Just (Decider.MkDecider S.carol)}
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
    Spec.assertEqWith s "bob defends" (Combat.Type.defender (GameState.combat after)) (Just S.bob)
    Spec.assertEqWith s "nobody was asked" asked []
  Spec.it s "CR 507.1 a multiplayer game down to one opponent is not asked either" $ do
    -- The case #169 is actually about: CR 703.4h still applies (the game BEGAN
    -- with three players, CR 800.1), and the choice has one candidate.
    -- Discriminating against an elision keyed on the SEAT COUNT rather than on
    -- the candidate count -- that version would prompt here.
    let gone = Departure.depart Departure.Type.Conceded S.carol S.threePlayerGame
        (after, asked) =
          State.runState
            (fmap snd (Engine.runGame (choosesDefender S.carol) gone (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))))
            []
    Spec.assertEqWith s "bob, the only one left" (Combat.Type.defender (GameState.combat after)) (Just S.bob)
    Spec.assertEqWith s "nobody was asked" asked []
  Spec.it s "CR 507.1 with no opponents left the action does not happen at all" $ do
    -- Not reachable in a running game (CR 104.2a ends it), but the branch has
    -- to be total and NonEmpty is why. Discriminating against an
    -- implementation that built the prompt from an empty list.
    let alone = Departure.depart Departure.Type.Conceded S.carol (Departure.depart Departure.Type.Conceded S.bob S.threePlayerGame)
        (after, asked) =
          State.runState
            (fmap snd (Engine.runGame (choosesDefender S.bob) alone (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))))
            []
    Spec.assertEqWith s "nobody defends" (Combat.Type.defender (GameState.combat after)) Nothing
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
    -- ends of that call (Engine.hs's hasActive and chooseDefender's own test),
    -- so this case passes with either one alone and isolates neither. The
    -- sibling case below is the one that isolates chooseDefender's; the
    -- engine-side copy is redundant on this path and has nothing to isolate.
    let gone = Departure.depart Departure.Type.Conceded S.alice S.threePlayerGame
        (after, asked) =
          State.runState
            (fmap snd (Engine.runGame (choosesDefender S.carol) gone (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))))
            []
    Spec.assertEqWith s "no defending player" (Combat.Type.defender (GameState.combat after)) Nothing
    Spec.assertEqWith s "and nobody was asked" asked []
  Spec.it s "CR 800.4j chooseDefender called directly still chooses nobody" $ do
    -- The same rule at the other end of the call, reached WITHOUT
    -- Engine.runTurnBasedActions so that only chooseDefender's own membership
    -- test can be responsible. Discriminating exactly that line: with it gone,
    -- alice -- who has left the game -- is asked, and carol becomes the
    -- defending player on a turn CR 800.4j says has no active player to choose
    -- one. Three seats again, so two candidates survive alice's departure and
    -- the single-candidate elision (#169) cannot be what suppresses the ask.
    let gone = Departure.depart Departure.Type.Conceded S.alice S.threePlayerGame
        (after, asked) =
          State.runState
            (fmap snd (Engine.runGame (choosesDefender S.carol) gone Combat.chooseDefender))
            []
    Spec.assertEqWith s "no defending player" (Combat.Type.defender (GameState.combat after)) Nothing
    Spec.assertEqWith s "and nobody was asked" asked []
  Spec.it s "CR 507.1 an answer that is not one of the candidates falls back to the first" $ do
    -- A broken interpreter, not a game state: it names the ACTIVE player.
    -- Discriminating against `defender = Just answer` unchecked, which would
    -- let alice attack herself and, once Task 4 lands, deal combat damage to
    -- the attacking player.
    let (after, _) =
          State.runState
            (fmap snd (Engine.runGame (choosesDefender S.alice) S.threePlayerGame (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))))
            []
    Spec.assertEqWith s "the first candidate, never the active player" (Combat.Type.defender (GameState.combat after)) (Just S.bob)
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
    let gone = Departure.depart Departure.Type.Conceded S.bob S.fourPlayerGame
    Spec.assertEqWith s "bob is dropped, carol and dave remain" (Combat.attackableOpponents gone) [S.carol, S.dave]
    Spec.assertEqWith s "and before he left there were three" (Combat.attackableOpponents S.fourPlayerGame) [S.bob, S.carol, S.dave]
  Spec.it s "CR 506.2a the candidates come back in SEATING order, not player-id order" $ do
    -- The discriminator between Game.stillPlayingInOrder and
    -- Game.stillPlaying: seated carol-alice-bob with alice attacking,
    -- seating order gives [carol, bob] and the players map gives [bob, carol].
    -- Every other fixture in the suite is seated ascending, so this is the
    -- only place the two readings disagree.
    let rotated = (Setup.emptyGame (S.carol NonEmpty.:| [S.alice, S.bob])) {GameState.activePlayer = S.alice}
    Spec.assertEqWith s "carol's seat comes first" (Combat.attackableOpponents rotated) [S.carol, S.bob]
  Spec.it s "CR 703.4h no defending player has been chosen before the beginning of combat step" $
    -- Discriminating: a field defaulted to Just <somebody> would let a board
    -- that has never run the turn-based action declare attackers.
    Spec.assertEqWith s "empty combat names nobody" (Combat.Type.defender (GameState.combat S.threePlayerGame)) Nothing
  Spec.it s "CR 506.2 the designation does not outlive the combat phase" $ do
    -- CR 506.2's sentences are all scoped "During the combat phase", and
    -- CR 703.4h makes the choice per beginning-of-combat step, so a second
    -- combat phase in one turn chooses again. Discriminating: a clearCombat
    -- that reset only attackers and blockers would leave Just carol here, and
    -- the next combat phase would inherit a stale defender.
    let busy = S.threePlayerGame {GameState.combat = (GameState.combat S.threePlayerGame) {Combat.Type.defender = Just S.carol}}
    Spec.assertEqWith s "cleared at end of combat" (Combat.Type.defender (GameState.combat (Combat.clearCombat busy))) Nothing
  Spec.it s "CR 508.1 every attacker attacks the CHOSEN defending player" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (board, mine, _, _) = S.threePlayerCombat [piker, piker] [piker] [piker]
        -- carol, deliberately not the first candidate.
        ready =
          board
            { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
              GameState.combat = (GameState.combat board) {Combat.Type.defender = Just S.carol}
            }
        after = S.runPure S.aggressiveAnswer ready (Combat.declareAttackers S.alice)
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
        after = S.runPure S.aggressiveAnswer ready (Combat.declareAttackers S.alice)
    Spec.assertEqWith s "alice really had a legal attacker" (fmap (\oid -> Combat.canAttack S.alice oid ready) mine) [True]
    Spec.assertEqWith s "nobody attacked" (Combat.Type.attackers (GameState.combat after)) Map.empty
    Spec.assertEqWith s "and nothing was tapped" (S.tappedCount S.alice after) 0
  Spec.it s "CR 509.1 only the defending player is asked to declare blockers" $ do
    -- CR 509.1's first sentence names THE defending player, singular. CR 802.4
    -- is the rule that has several of them declare in APNAP order, and it needs
    -- an option pawl cannot express (#175).
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
                  { Combat.Type.defender = Just S.carol,
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
  Spec.it s "CR 725.2/507.1 the crown follows whichever opponent was chosen as the defending player" $ do
    -- CR 725.2's second inherent ability: "Whenever a creature deals combat
    -- damage to the monarch, its controller becomes the monarch." bob is the
    -- monarch. alice attacks with an unblocked 2/1; the two runs differ ONLY
    -- in the answer to Prompt.ChooseDefender.
    --
    -- Discriminating: run A is what the deleted head-of-list behaviour did
    -- whatever the answer, so run A alone proves nothing. Run B is
    -- unreachable under it, and the pair is the proof.
    piker <- S.printingOf s registry "Goblin Piker"
    let (board, _, _, _) = S.threePlayerCombat [piker] [] []
        crowned = S.withMonarch S.bob board
        hitBob = S.runCombat (S.attackTo S.bob) crowned
        hitCarol = S.runCombat (S.attackTo S.carol) crowned
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
        after = snd (Engine.runGamePure S.aggressiveAnswer (justArrived gs) (Combat.declareAttackers S.alice))
    Spec.assertEqWith s "attacks" (length (declaredAttackers after)) 1
  Spec.it s "CR 302.6 the same creature without haste cannot" $ do
    -- The control. Goblin Chariot and Goblin Piker are both 2/2-ish Goblin
    -- Warriors; the ONLY difference the engine can see is the keyword.
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [piker] [piker]
        after = snd (Engine.runGamePure S.aggressiveAnswer (justArrived gs) (Combat.declareAttackers S.alice))
    Spec.assertEqWith s "cannot attack" (declaredAttackers after) []
  Spec.it s "CR 702.10b haste is not needed once the creature has settled" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker] [piker]
        after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
    Spec.assertEqWith s "attacks" (declaredAttackers after) mine
  -- The same contrast one layer up: haste GRANTED by a static ability rather
  -- than printed. Concordant Crossroads says "All creatures have haste", so
  -- the very Piker that could not attack in the control case above now can,
  -- and nothing about the Piker itself changed.
  Spec.it s "CR 702.10b Concordant Crossroads grants haste, so a summoning-sick Piker attacks" $ do
    crossroads <- S.printingOf s registry "Concordant Crossroads"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker] [piker]
        (_, enchanted) = S.addCreature crossroads S.alice (justArrived gs)
        after = snd (Engine.runGamePure S.aggressiveAnswer enchanted (Combat.declareAttackers S.alice))
    Spec.assertEqWith s "attacks anyway" (declaredAttackers after) mine
  Spec.it s "CR 702.10b a hasty creature and a sick one, in the same declaration" $ do
    -- Both sick; only the Chariot may attack. A blanket "sickness ignored"
    -- bug would let both through.
    goblinChariot <- S.printingOf s registry "Goblin Chariot"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [goblinChariot, piker] [piker]
        after = snd (Engine.runGamePure S.aggressiveAnswer (justArrived gs) (Combat.declareAttackers S.alice))
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
        (creature, withCreature) = S.addCreature piker S.bob base
        settled = S.runPure S.identityAnswer withCreature (Engine.settleAll S.bob)
        (aura, withAura) = S.addCreature controlMagic S.alice settled
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
        (creature, withCreature) = S.addCreature piker S.alice base
        settled = S.runPure S.identityAnswer withCreature (Engine.settleAll S.alice)
        (aura, withAura) = S.addCreature controlMagic S.bob settled
        -- The steal is observed the next time the board settles -- the CR
        -- 117.5 sweep, which runs wherever the board can change.
        stolen = S.runPure S.identityAnswer (S.attach aura creature withAura) Engine.settleForPriority
        returned = S.runPure S.identityAnswer stolen (Event.changeZone aura Zone.Graveyard)
    Spec.assertEqWith s "bob held it" (Projection.controllerOf creature stolen) (Just S.bob)
    Spec.assertEqWith s "alice has it back" (Projection.controllerOf creature returned) (Just S.alice)
    Spec.assertBool s (not (Combat.canAttack S.alice creature returned)) "but not continuously, so it cannot attack"

-- Declare attackers with everything, then hand back the state and the ids.
attacking :: [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
attacking mine theirs =
  let (gs, ours, yours) = S.combatBoardOf mine theirs
      after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
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

-- CR 702.14c's "the defending player controls at least one land": bob's lands,
-- put onto an already-attacking board.
withLands :: [Printing.Printing] -> GameState.GameState -> GameState.GameState
withLands = withPermanents S.bob

-- Any printings at all onto `who`'s battlefield, on a board that already exists.
-- S.addCreature is any-printing rather than creature-only, which is how the CR
-- 509.1a Mountain case below reaches a land and landSubtypeStripSpec an
-- enchantment.
withPermanents :: PlayerId.PlayerId -> [Printing.Printing] -> GameState.GameState -> GameState.GameState
withPermanents who ps gs = List.foldl' (\g p -> snd (S.addCreature p who g)) gs ps

-- Put `printing` onto bob's battlefield already attached to `host` -- CR 301.5a
-- for an Equipment, CR 303.4b for an Aura. A STATE fixture, as S.attach's own
-- comment says: no equip ability is activated, so the timing of CR 702.6a plays no
-- part in what the CR 509.1a arity below reads.
withAttachment :: Printing.Printing -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
withAttachment printing host gs =
  let (oid, gs1) = S.addCreature printing S.bob gs
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
          Combat.declareAttackers S.alice
          Combat.declareBlockers
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
          Combat.declareAttackers S.alice
          Combat.declareBlockers
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
        withLand = snd (S.addCreature mountain S.bob gs)
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
        let after = S.runPure S.aggressiveAnswer gs Combat.declareBlockers
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
        gs = snd (S.addCreature urborg S.alice (withLands [island] gs0))
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
        let after = S.runPure S.aggressiveAnswer gs Combat.declareBlockers
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
        let after = S.runPure S.aggressiveAnswer gs Combat.declareBlockers
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
        let after = S.runPure S.aggressiveAnswer gs Combat.declareBlockers
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
        let (glovesId, equipped) = S.addCreature gloves S.alice (withLands [seat] gs0)
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
        let (glovesId, board) = S.addCreature gloves S.alice (withLands [swamp] gs0)
            armed = S.attach glovesId a board
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) armed) "legal"
        let after = S.runPure S.aggressiveAnswer armed Combat.declareBlockers
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
        let after = S.runPure S.aggressiveAnswer gs Combat.declareBlockers
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
      (_, gs1) = S.addCreature island S.alice gs0
      (_, gs2) = S.addCreature defendersLand S.bob gs1
      (hackId, gs3) = S.addHandCard magicalHack S.alice gs2
      board = case (hacked, hackTarget ours) of
        (True, Just t) -> castHackAt hackId t from to gs3
        _ -> gs3
      attacked = snd (Engine.runGamePure S.aggressiveAnswer board (Combat.declareAttackers S.alice))
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
    let after = S.runPure S.aggressiveAnswer onIsland Combat.declareBlockers
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
    let after = S.runPure S.aggressiveAnswer onSwamp Combat.declareBlockers
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
    let after = S.runPure S.aggressiveAnswer onSwamp Combat.declareBlockers
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
    let after = S.runPure S.aggressiveAnswer onMountain Combat.declareBlockers
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
      (_, gs2) = S.addCreature defendersLand S.bob gs1
      (scoutsId, gs3) = S.addHandCard goblinScouts S.alice gs2
      (hackId, gs4) = S.addHandCard magicalHack S.alice gs3
      onStack = S.runPure S.identityAnswer (gs4 {GameState.priority = Just S.alice}) (S.cast S.alice scoutsId)
      spellId = case GameState.stack onStack of
        top : _ -> top
        [] -> ObjectId.MkObjectId 999
      swapped = if hacked then castHackAt hackId spellId Subtype.Mountain Subtype.Swamp onStack else onStack
      minted = S.runPure S.identityAnswer swapped Stack.resolveTop
      settled = S.runPure S.identityAnswer minted (Engine.settleAll S.alice)
      attacked = S.runPure S.aggressiveAnswer settled (Combat.declareAttackers S.alice)
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
        let enchanted = snd (S.addCreature highGround S.bob gs)
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
            Combat.legalBlockDeclaration S.bob (Map.singleton guard (Set.fromList [first, second, third])) (snd (S.addCreature highGround S.bob gs))
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
        let after = S.runPure (blockAll [first, second, third]) gs Combat.declareBlockers
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
        let after = S.runPure (blockAll [first, second]) gs Combat.declareBlockers
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
      enchant g attacker = let (aura, withAura) = S.addCreature lure S.alice g in S.attach aura attacker withAura
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
                Combat.declareAttackers S.alice
                Combat.declareBlockers
                Damage.dealCombatDamage
            )
            []
        )

-- CR 702.111b, proved by Boggart Brute ("Creature -- Goblin Warrior 3/2,
-- Menace") -- the blocking side's SET-SHAPED combat restriction, and the first
-- evasion ability that is not a question about a (blocker, attacker) pair. Its
-- attacking counterpart is Bonded Construct's "can't attack alone"
-- (attacksAloneSpec below).
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
        let after = S.runPure S.aggressiveAnswer gs Combat.declareBlockers
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
        let after = S.runPure S.aggressiveAnswer gs Combat.declareBlockers
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
        gs = snd (Engine.runGamePure S.aggressiveAnswer before (Combat.declareAttackers S.alice))
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
        let blocked = S.runPure S.aggressiveAnswer gs Combat.declareBlockers
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
          let (aura, withAura) = S.addCreature lure S.alice gs
           in (S.attach aura attacker withAura, ours, yours)

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
          a : _ -> let (aura, withAura) = S.addCreature lure S.alice gs in S.attach aura a withAura
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
    -- resolves Affected.Attached.
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
        withAura = snd (S.addCreature lure S.alice gs)
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
    -- makes the third assertion discriminate; on the first attacker it would
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
        let (aura, withAura) = S.addCreature lure S.alice plain
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
-- built from, and what a board it cannot build -- `jaceBoard`'s, which needs its
-- planeswalker's loyalty counters placed first -- reaches for instead.
cursingBoard :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
cursingBoard curse who gs =
  let (aura, withAura) = S.addCreature curse S.alice gs
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
        withAura = snd (S.addCreature curse S.alice gs)
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] withAura) "no attack is legal"
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
  let (aura, withAura) = S.addCreature pacifism S.alice gs
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
        withAura = snd (S.addCreature pacifism S.alice gs)
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

-- bob's two Pikers and his two Islands, with `ahead` placed under him BEFORE the
-- Pikers and `behind` after them -- which is how the pair below puts one Humility on
-- either side of the same permanent and changes nothing else. Placement order is
-- timestamp order (Pawl.Support.addCreature allocates one per object), so the two
-- boards differ in exactly one timestamp comparison.
--
-- Then alice attacks, a Goblin Piker spell of hers goes on the stack to be
-- Reasonable Doubt's counter target, and bob casts the Doubt suspecting his FIRST
-- Piker. Returns the settled board, the suspected Piker, the one beside it,
-- alice's attacker and the spell the Doubt countered.
suspectBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  [Printing.Printing] ->
  [Printing.Printing] ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
suspectBoard s registry ahead behind = do
  piker <- S.printingOf s registry "Goblin Piker"
  island <- S.printingOf s registry "Island"
  doubt <- S.printingOf s registry "Reasonable Doubt"
  let (gs0, mine, _) = S.combatBoardOf [piker] []
      (suspect, gsA) = S.addCreature piker S.bob (withPermanents S.bob ahead gs0)
      (other, gsB) = S.addCreature piker S.bob gsA
      gs2 = withPermanents S.bob (behind <> [island, island]) gsB
      declared = S.runPure S.aggressiveAnswer gs2 (Combat.declareAttackers S.alice)
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

-- CR 701.60c against CR 613.1f, ordered by CR 613.7 -- proved by Reasonable Doubt
-- {1}{U} Instant, "Counter target spell unless its controller pays {2}. Suspect up
-- to one target creature", cast under Humility.
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
    (gs, suspect, other, attacker, victim) <- suspectBoard s registry [humility] []
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
    (gs, suspect, other, attacker, _) <- suspectBoard s registry [] [humility]
    Spec.assertEqWith s "the same designation, on the same Piker: CR 701.60b makes it no ability, so no removal reaches it" (suspectedOf suspect gs, suspectedOf other gs) (Just True, Just False)
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Menace suspect gs)) "CR 613.1f: the later removal wipes the grant, so the menace half is gone"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton suspect (Set.singleton attacker)) gs) "CR 613.1f: and the can't-block half with it"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton other (Set.singleton attacker)) gs) "as the Piker beside it could all along"

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
              GameState.combat = (GameState.combat gs) {Combat.Type.defender = Just who}
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
            (_, gs1) = S.addCreature island S.alice gs0
            gs2 = if withMountain then snd (S.addCreature mountain S.bob gs1) else gs1
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
            (islandId, gs1) = S.addCreature island S.alice gs0
            (sourceId, gs2) = S.addCreature printing S.alice gs1
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
    let declare g = snd (Engine.runGamePure S.aggressiveAnswer g (Combat.declareAttackers S.alice))
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

-- CR 508.1c read through CR 506.5, proved by Bonded Construct ("{1} Artifact
-- Creature -- Construct 2/1, This creature can't attack alone") -- the attacking
-- side's SET-SHAPED restriction, and menaceSpec's twin across the combat phase.
--
-- What makes it a different KIND of restriction from Pacifism's, and not merely a
-- narrower one: there is no answer to "may this creature attack?" at all. The
-- Construct may attack in some declarations and not in others, so it stays on CR
-- 508.1a's candidate list and the illegality is a property of the declaration.
-- The first assertion of every case below is that it is still offered, because
-- "the attack was refused" is also what a bug that drops it from the candidate
-- list produces -- and that bug would pass every negative assertion here.
--
-- The requirement cases are the subtle half. CR 508.1d's maximum is "the maximum
-- possible number of requirements that could be obeyed WITHOUT DISOBEYING ANY
-- RESTRICTIONS", and before this card no attacking restriction could take a
-- required creature's declaration away, so the maximum was always every
-- requirement at once. Under a Curse of the Nightly Hunt a lone Construct is
-- required to attack and forbidden to attack alone, and the maximum is zero.
attacksAloneSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
attacksAloneSpec s registry = Spec.describe s "AttacksAlone" $ do
  Spec.it s "CR 506.5 a lone Bonded Construct is a legal CANDIDATE that may not be the whole declaration" $ do
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    let (gs, mine, _) = S.combatBoardOf [bondedConstruct] []
    case mine of
      [construct] -> do
        Spec.assertBool s (Combat.canAttack S.alice construct gs) "the Construct can attack"
        Spec.assertEqWith s "and is offered" (Combat.legalAttackers S.alice gs) [construct]
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [construct] gs)) "but attacking alone is illegal"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] gs) "and declining stays legal"
      _ -> Spec.assertFailure s "fixture should have one creature"
  Spec.it s "CR 506.5 a Goblin Piker beside it makes the same Construct's attack legal" $ do
    -- The permitted case on the SAME board as the refused one, which is what
    -- separates this restriction from summoning sickness, a tap, or a defender:
    -- none of those changes its answer when a second creature is declared.
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [bondedConstruct, piker] []
    case mine of
      [construct, other] -> do
        Spec.assertEqWith s "both are offered" (Combat.legalAttackers S.alice gs) [construct, other]
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [construct] gs)) "the Construct alone is still illegal"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [construct, other] gs) "the two together are legal"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [other] gs) "and the Piker alone is legal, so nothing blanket-refused"
      _ -> Spec.assertFailure s "fixture should have two creatures"
  Spec.it s "CR 508.1c two Bonded Constructs may attack together, which is that rule's own Example" $ do
    -- Verbatim: "A player controls two creatures, each with a restriction that
    -- states 'This creature can't attack alone.' It's legal to declare both as
    -- attackers." The reading it falsifies is "each restricted creature needs an
    -- UNRESTRICTED companion", which passes every other case in this group.
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    let (gs, mine, _) = S.combatBoardOf [bondedConstruct, bondedConstruct] []
    case mine of
      [first, second] -> do
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [first, second] gs) "declaring both is legal"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [first] gs)) "either one alone is illegal"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [second] gs)) "in both directions"
      _ -> Spec.assertFailure s "fixture should have two Constructs"
  Spec.it s "CR 508.1d a required creature that can't attack alone makes the maximum ZERO" $ do
    -- The board CR 508.1d's closed form got wrong: the Curse requires the
    -- Construct to attack if able, the Construct may not attack alone, and there
    -- is nobody to attack with -- so no legal declaration obeys the requirement
    -- and declining attains the maximum. A ceiling that assumed "every required
    -- creature at once is legal" answers this by forcing an illegal attack.
    --
    -- The lone Piker under the same Curse is the control, and it is the case that
    -- makes this one non-vacuous: there declining IS illegal, so the Curse is
    -- live and it is the Construct's restriction that moved the maximum.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = cursing curse S.alice [bondedConstruct] []
        (control, _, _) = cursing curse S.alice [piker] []
    case mine of
      [construct] -> do
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] control)) "a required Piker may not decline"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] gs) "but a required Construct with no company may"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [construct] gs)) "and attacking alone stays illegal, requirement or no requirement"
      _ -> Spec.assertFailure s "fixture should have one creature"
  Spec.it s "CR 508.1d with company the maximum is BOTH, and the Piker alone no longer attains it" $ do
    -- The other side of the same interaction. One Curse over two able creatures
    -- is two requirements, both obeyable at once because attacking together is
    -- legal -- so the restriction bounds the maximum here without zeroing it, and
    -- the declaration that obeys only the Piker's is now illegal.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = cursing curse S.alice [bondedConstruct, piker] []
    case mine of
      [construct, other] -> do
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] gs)) "declining is illegal"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [other] gs)) "the Piker alone obeys one of two"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [construct] gs)) "the Construct alone is illegal twice over, on the restriction and on the count"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [construct, other] gs) "only both together is legal"
      _ -> Spec.assertFailure s "fixture should have two creatures"
  Spec.it s "CR 508.1d a Ghostly Prison excuses the whole maximum even where attacking together is legal" $ do
    -- The cost clause reaching the ENUMERATION, which is the one path of
    -- attackCeiling the cases above leave untested: the restriction is in force,
    -- so the closed form is not taken, and the search still has to be over the
    -- creatures that attack FREELY. With the Prison out and no Forests, neither
    -- creature does, so the maximum is zero even though attacking together would
    -- obey both requirements and disobey nothing.
    --
    -- An enumeration that ranged over every candidate would answer two here and
    -- make declining illegal, which is CR 508.1d's third sentence exactly
    -- backwards: a player is never required to pay a cost to attack.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    prison <- S.printingOf s registry "Ghostly Prison"
    forest <- S.printingOf s registry "Forest"
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    piker <- S.printingOf s registry "Goblin Piker"
    let (board, mine, _) = imprisoning prison forest S.bob [bondedConstruct, piker] 0
        taxed = cursingBoard curse S.alice board
        (plain, _, _) = cursing curse S.alice [bondedConstruct, piker] []
    case mine of
      [construct, other] -> do
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] plain)) "without the Prison, declining is illegal"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] taxed) "with it, declining is legal"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [construct, other] taxed) "and attacking together anyway is still legal"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [construct] taxed)) "while the Construct alone stays illegal, cost or no cost"
      _ -> Spec.assertFailure s "fixture should have two creatures"
  Spec.it s "CR 506.5 whole cards: a lone Construct sits out a real declare attackers step" $ do
    -- The gameplay-level case, through the priority loop and CR 703.4i's
    -- turn-based action rather than a direct call, with the interpreter that
    -- attacks with everything it is offered.
    --
    -- THREE boards, because two would not be enough. The Construct with a Piker
    -- connects for 4; the Construct alone is refused and bob takes nothing; the
    -- PIKER alone connects for 2, which is what rules out "a lone attacker never
    -- gets through" as the explanation of the middle board.
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    piker <- S.printingOf s registry "Goblin Piker"
    let (pair, mine, _) = S.combatBoardOf [bondedConstruct, piker] []
        (lone, _, _) = S.combatBoardOf [bondedConstruct] []
        (lonePiker, _, _) = S.combatBoardOf [piker] []
        after = S.runCombat S.aggressiveAnswer pair
        refused = S.runCombat S.aggressiveAnswer lone
        control = S.runCombat S.aggressiveAnswer lonePiker
    Spec.assertEqWith s "with company, bob takes four" (S.lifeOf S.bob after) (Just 16)
    Spec.assertEqWith s "and both were declared" (S.attackerDeclarationsOf after) mine
    Spec.assertEqWith s "alone, bob takes nothing" (S.lifeOf S.bob refused) (Just 20)
    Spec.assertEqWith s "and nothing was declared" (S.attackerDeclarationsOf refused) []
    Spec.assertEqWith s "while a lone PIKER connects for two" (S.lifeOf S.bob control) (Just 18)

-- CR 508.1c and CR 509.1b, proved by Silent Arbiter ("{4} Artifact Creature --
-- Construct 1/5, No more than one creature can attack each combat. No more than
-- one creature can block each combat.") -- the restriction that forbids a
-- declaration for its SIZE, and the third shape of combat restriction after
-- Pacifism's per-creature one and Bonded Construct's set-shaped one.
--
-- What makes it a different kind again from Bonded Construct's: that one NAMES
-- creatures and asks what the declaration holds, so a board of two of them
-- allows a two-creature attack (CR 508.1c's Example). This one names no creature
-- at all, so no Affected could carry it and no candidate list can hide it -- and
-- it is not scoped to its controller either, which the cases below prove by
-- putting the Arbiter on the DEFENDING player's battlefield and holding the
-- attacking player to one attacker.
--
-- The first assertion of every case is that the creatures are still OFFERED, on
-- attacksAloneSpec's terms: "the attack was refused" is also what a bug that
-- subtracted them from CR 508.1a's or CR 509.1a's candidate list produces, and
-- that bug would pass every negative assertion here.
--
-- The empty declaration is asserted LEGAL wherever no requirement is in force,
-- because "no more than one" is a ceiling and not a quota: a reading of the bound
-- as "exactly one" passes every other assertion in the first case.
boundedDeclarationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
boundedDeclarationSpec s registry = Spec.describe s "BoundedDeclaration" $ do
  Spec.it s "CR 508.1c a Silent Arbiter allows EITHER attacker but not both" $ do
    -- Both single-creature declarations are legal on the SAME board the
    -- two-creature one is refused on, which is what separates a size bound from
    -- summoning sickness, a tap, a defender, or the Arbiter simply not being a
    -- candidate: none of those changes its answer when a creature is dropped
    -- from the declaration.
    silentArbiter <- S.printingOf s registry "Silent Arbiter"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [silentArbiter, piker] []
        (control, theirs, _) = S.combatBoardOf [piker, piker] []
    case (mine, theirs) of
      ([arbiter, other], [first, second]) -> do
        Spec.assertEqWith s "both are offered" (Combat.legalAttackers S.alice gs) [arbiter, other]
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [arbiter] gs) "the Arbiter alone is legal"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [other] gs) "the Piker alone is legal"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [arbiter, other] gs)) "the two together are not"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] gs) "and declining stays legal, so the bound is a ceiling and not a quota"
        -- The control that makes the refusal the BOUND talking: two Goblin
        -- Pikers and no Arbiter attack together happily.
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [first, second] control) "two creatures attack together without the Arbiter"
      _ -> Spec.assertFailure s "fixture should have two creatures a side"
  Spec.it s "CR 508.1c the bound is GLOBAL: bob's Arbiter holds ALICE to one attacker" $ do
    -- Silent Arbiter's sentence says "no more than one creature", not "no more
    -- than one creature you control". A reader that scoped the bound to its
    -- source's controller passes every other attacking case in this group, since
    -- the Arbiter sits on alice's side in all of them.
    silentArbiter <- S.printingOf s registry "Silent Arbiter"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker, piker] [silentArbiter]
        (control, _, _) = S.combatBoardOf [piker, piker] [piker]
    case mine of
      [first, second] -> do
        Spec.assertEqWith s "both of alice's are offered" (Combat.legalAttackers S.alice gs) [first, second]
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [first] gs) "one of them may attack"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [first, second] gs)) "but not both"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [first, second] control) "and with a plain Piker there instead, both may"
      _ -> Spec.assertFailure s "fixture should have two attackers"
  Spec.it s "CR 509.1b the same Arbiter holds bob to one BLOCKER" $ do
    -- The blocking half of the same card, and the same anti-vacuity shape: both
    -- of bob's Pikers are offered, either may block alone, and the pair is
    -- refused. Alice attacks with one creature because the Arbiter's first
    -- sentence already holds her to one -- which is the attacking half's global
    -- reach, observed again.
    silentArbiter <- S.printingOf s registry "Silent Arbiter"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker] [silentArbiter, piker, piker]
        (control, plain, others) = attacking [piker] [piker, piker]
    case (mine, theirs, plain, others) of
      ([a], [arbiter, first, second], [b], [x, y]) -> do
        Spec.assertEqWith s "all three of bob's are offered" (Combat.legalBlockers S.bob gs) [arbiter, first, second]
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton first (Set.singleton a)) gs) "one blocker is legal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton second (Set.singleton a)) gs) "either one of them"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.fromList [(first, Set.singleton a), (second, Set.singleton a)]) gs)) "two are not"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty gs) "and declining stays legal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.fromList [(x, Set.singleton b), (y, Set.singleton b)]) control) "two Pikers double block without the Arbiter"
      _ -> Spec.assertFailure s "fixture should have one attacker and bob's blockers"
  Spec.it s "CR 508.1d's own Example: the required creature attacks, and nothing else does" $ do
    -- Verbatim: "A player controls two creatures: one that 'attacks if able' and
    -- one with no abilities. An effect states 'No more than one creature can
    -- attack each turn.' The only legal attack is for just the creature that
    -- 'attacks if able' to attack. It's illegal to attack with the other
    -- creature, attack with both, or attack with neither."
    --
    -- Built as: a Kormus Bell animating alice's Swamp into a 1/1, a Synthetic
    -- Wetland Frenzy requiring Swamps to attack, and a Goblin Piker that is not a
    -- Swamp -- so exactly ONE requirement instance is minted, which is what the
    -- Example needs and what a Curse of the Nightly Hunt could not give. The
    -- Arbiter is bob's, so "an effect states" is the global sentence the Example
    -- describes rather than something alice's own board says.
    --
    -- The control on the same board WITHOUT the Arbiter is what proves the Frenzy
    -- is live independently of the bound: there attacking with both is legal and
    -- declining is not.
    silentArbiter <- S.printingOf s registry "Silent Arbiter"
    (gs, swampId, pikerId) <- exampleBoard s registry [silentArbiter]
    (control, controlSwamp, controlPiker) <- exampleBoard s registry []
    Spec.assertEqWith s "both of alice's creatures are offered" (Combat.legalAttackers S.alice gs) [swampId, pikerId]
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [swampId] gs) "the required Swamp alone is the only legal attack"
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [pikerId] gs)) "attacking with the other creature is illegal"
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [swampId, pikerId] gs)) "with both is illegal"
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] gs)) "with neither is illegal"
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [controlSwamp, controlPiker] control) "without the Arbiter both may attack"
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] control)) "and the Frenzy still forbids declining, so it is live on its own"
  Spec.it s "CR 508.1d two required creatures under a bound of one: the maximum is ONE, not two" $ do
    -- The board attackCeiling's CLOSED FORM gets wrong, and the reason its guard
    -- has to test the answer rather than the restriction. With a Curse of the
    -- Nightly Hunt over two Goblin Pikers the instance set is both of them, and
    -- the closed form hands back both -- a declaration the bound forbids, which
    -- no player could attain, so every declaration would be illegal at once.
    -- Enumerating instead finds a maximum of one, and either Piker attains it.
    --
    -- The case above does NOT prove this: there the closed form's answer is the
    -- single required Swamp, which is within a bound of one, so the shortcut is
    -- still exact. Two required creatures is the smallest board where the two
    -- readings disagree.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    silentArbiter <- S.printingOf s registry "Silent Arbiter"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = cursing curse S.alice [piker, piker] [silentArbiter]
        (control, _, _) = cursing curse S.alice [piker, piker] []
    case mine of
      [first, second] -> do
        Spec.assertEqWith s "both are still offered" (Combat.legalAttackers S.alice gs) [first, second]
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [first] gs) "attacking with one attains the maximum"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [second] gs) "and so does the other one"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [first, second] gs)) "both together is over the bound"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] gs)) "and declining obeys neither requirement"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [first] control)) "without the Arbiter one Piker no longer attains it"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [first, second] control) "and both together do"
      _ -> Spec.assertFailure s "fixture should have two creatures"
  Spec.it s "CR 509.1c a Lure under a bound of one: the maximum is ONE blocker" $ do
    -- The blocking twin of the case above, over blockCeiling's fold. Lure makes
    -- every creature able to block the enchanted attacker do so, which is all
    -- three of bob's; the bound allows one. So declining becomes illegal, exactly
    -- one blocker is legal, and two remain forbidden -- the requirement and the
    -- restriction each moving one of the three answers.
    lure <- S.printingOf s registry "Lure"
    silentArbiter <- S.printingOf s registry "Silent Arbiter"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = luring lure [piker] [silentArbiter, piker, piker]
        (control, plain, others) = luring lure [piker] [piker, piker]
    case (mine, theirs, plain, others) of
      ([a], [_, first, second], [b], [x, y]) -> do
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty gs)) "declining is illegal under the Lure"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton first (Set.singleton a)) gs) "one blocker attains the maximum"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.fromList [(first, Set.singleton a), (second, Set.singleton a)]) gs)) "two are over the bound"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton x (Set.singleton b)) control)) "without the Arbiter one blocker no longer attains it"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.fromList [(x, Set.singleton b), (y, Set.singleton b)]) control) "and both blocking does"
      _ -> Spec.assertFailure s "fixture should have one attacker and bob's blockers"
  Spec.it s "CR 508.1c whole cards: an over-large attack is refused in a real declare attackers step" $ do
    -- The gameplay-level attacking case, through the priority loop and CR 703.4i's
    -- turn-based action, with the interpreter that attacks with everything it is
    -- offered.
    --
    -- THREE boards, on attacksAloneSpec's terms, and no two share an observable:
    -- the Arbiter beside a Piker is refused outright and bob takes nothing; a lone
    -- Piker connects for two, which rules out "a lone attacker never gets
    -- through"; the Arbiter attacking by ITSELF connects for one, which rules out
    -- "the Arbiter is never a legal attacker".
    silentArbiter <- S.printingOf s registry "Silent Arbiter"
    piker <- S.printingOf s registry "Goblin Piker"
    let (pair, _, _) = S.combatBoardOf [silentArbiter, piker] []
        (lonePiker, _, _) = S.combatBoardOf [piker] []
        (loneArbiter, _, _) = S.combatBoardOf [silentArbiter] []
        refused = S.runCombat S.aggressiveAnswer pair
        connects = S.runCombat S.aggressiveAnswer lonePiker
        alone = S.runCombat S.aggressiveAnswer loneArbiter
    Spec.assertEqWith s "two offered, none declared" (S.attackerDeclarationsOf refused) []
    Spec.assertEqWith s "so bob takes nothing" (S.lifeOf S.bob refused) (Just 20)
    Spec.assertEqWith s "a lone Piker connects for two" (S.lifeOf S.bob connects) (Just 18)
    Spec.assertEqWith s "and the Arbiter by itself connects for one" (S.lifeOf S.bob alone) (Just 19)
  Spec.it s "CR 509.1b whole cards: an over-large block is refused in a real declare blockers step" $ do
    -- The gameplay-level blocking case, run through Combat.declareBlockers as
    -- menaceSpec runs its own. S.aggressiveAnswer blocks with everything, which
    -- on the first board is two creatures and therefore illegal, so
    -- declareBlockers falls back to the forced declaration -- the empty one, not
    -- a block repaired down to one creature.
    --
    -- THREE boards again: two candidate blockers under the Arbiter (nobody
    -- blocks), ONE candidate blocker under the same Arbiter (it blocks, so the
    -- Arbiter does not forbid blocking as such), and two candidates with no
    -- Arbiter (both block, so two blockers are not refused as such).
    silentArbiter <- S.printingOf s registry "Silent Arbiter"
    piker <- S.printingOf s registry "Goblin Piker"
    let (crowded, mine, theirs) = attacking [piker] [silentArbiter, piker]
        (single, mineToo, theirsToo) = attacking [piker] [silentArbiter]
        (plain, mineThree, theirsThree) = attacking [piker] [piker, piker]
    case (mine, theirs, mineToo, theirsToo, mineThree, theirsThree) of
      ([a], [arbiter, other], [b], [loneArbiter], [c], [x, y]) -> do
        Spec.assertEqWith s "both of bob's are offered" (Combat.legalBlockers S.bob crowded) [arbiter, other]
        let refused = S.runPure S.aggressiveAnswer crowded Combat.declareBlockers
            blocked = S.runPure S.aggressiveAnswer single Combat.declareBlockers
            doubled = S.runPure S.aggressiveAnswer plain Combat.declareBlockers
        Spec.assertEqWith s "nobody blocks" (Combat.blockersOf a refused) Set.empty
        Spec.assertEqWith s "the lone Arbiter does block" (Combat.blockersOf b blocked) (Set.singleton loneArbiter)
        Spec.assertEqWith s "and without one, two Pikers do" (Combat.blockersOf c doubled) (Set.fromList [x, y])
      _ -> Spec.assertFailure s "fixture should have one attacker on each board"

-- CR 508.1d's Example board: alice controls a Kormus Bell animating her Swamp
-- into a 1/1, a Synthetic Wetland Frenzy requiring Swamps to attack, and a Goblin
-- Piker that is not a Swamp -- so the Swamp is the Example's creature that
-- "attacks if able" and the Piker is its creature with no abilities. `theirs` is
-- bob's side, which is where the Example's "an effect states" goes.
--
-- Named rather than inlined because both the Example and its no-Arbiter control
-- need it, and the two must differ in nothing else.
exampleBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  [Printing.Printing] ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
exampleBoard s registry theirs = do
  bell <- S.printingOf s registry "Kormus Bell"
  swamp <- S.printingOf s registry "Swamp"
  piker <- S.printingOf s registry "Goblin Piker"
  frenzy <- S.printingOf s registry "Synthetic Wetland Frenzy"
  let (gs0, ours, _) = S.combatBoardOf [bell, swamp, piker] theirs
      gs1 = snd (S.addCreature frenzy S.alice gs0)
  case ours of
    [_, swampId, pikerId] -> pure (gs1, swampId, pikerId)
    _ -> Spec.assertFailure s "fixture should have the Bell, the Swamp and the Piker"

vigilanceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
vigilanceSpec s registry = Spec.describe s "Vigilance" $ do
  Spec.it s "CR 702.20b attacking doesn't tap a creature with vigilance, but does tap its neighbor" $ do
    -- Both creatures in ONE declaration, so a blanket "nothing taps" bug
    -- cannot pass: the Piker must still tap.
    windseekerCentaur <- S.printingOf s registry "Windseeker Centaur"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [windseekerCentaur, piker] [piker]
        after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
    case mine of
      [centaur, p] -> do
        Spec.assertEqWith s "both attacking" (length (declaredAttackers after)) 2
        Spec.assertEqWith s "the centaur is untapped" (tapStateOf centaur after) (Just TapState.Untapped)
        Spec.assertEqWith s "the piker is tapped" (tapStateOf p after) (Just TapState.Tapped)
      _ -> Spec.assertFailure s "fixture should have two attackers"
  Spec.it s "CR 702.20b vigilance still attacks" $ do
    -- Vigilance is not a legality question: the creature is declared as an
    -- attacker exactly as normal. It simply skips CR 508.1f's tap.
    windseekerCentaur <- S.printingOf s registry "Windseeker Centaur"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [windseekerCentaur] [piker]
        after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
    Spec.assertEqWith s "attacking" (declaredAttackers after) mine
  Spec.it s "CR 702.20b an untapped vigilant attacker can still be blocked" $ do
    -- It is attacking, so it is in the Combat record, tapped or not.
    windseekerCentaur <- S.printingOf s registry "Windseeker Centaur"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = S.combatBoardOf [windseekerCentaur] [piker]
        steps = do
          Combat.declareAttackers S.alice
          Combat.declareBlockers
        after = snd (Engine.runGamePure S.aggressiveAnswer gs steps)
    case mine of
      [] -> Spec.assertFailure s "fixture should have an attacker"
      attacker : _ -> Spec.assertEqWith s "blocked" (Combat.blockersOf attacker after) (Set.fromList theirs)

combatLegalitySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
combatLegalitySpec s registry = Spec.describe s "CombatLegality" $ do
  Spec.it s "a Settled untapped creature may attack" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoard piker 1 0
    case mine of
      [] -> Spec.assertFailure s "fixture should have an attacker"
      oid : _ -> Spec.assertBool s (Combat.canAttack S.alice oid gs) "may attack"
  Spec.it s "CR 302.6 a summoning sick creature may not attack" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoard piker 1 0
    case mine of
      [] -> Spec.assertFailure s "fixture should have an attacker"
      oid : _ ->
        let sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}
         in Spec.assertBool s (not (Combat.canAttack S.alice oid sick)) "may not attack"
  Spec.it s "CR 508.1a a tapped creature may not attack" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoard piker 1 0
    case mine of
      [] -> Spec.assertFailure s "fixture should have an attacker"
      oid : _ ->
        let tapped = gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}
         in Spec.assertBool s (not (Combat.canAttack S.alice oid tapped)) "may not attack"
  Spec.it s "a land may not attack" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs = (S.landsInPlay mountain 1) {GameState.activePlayer = S.alice}
    case Game.zoneMembers Zone.Battlefield S.alice gs of
      [] -> Spec.assertFailure s "fixture should have one Mountain"
      oid : _ -> Spec.assertBool s (not (Combat.canAttack S.alice oid gs)) "may not attack"
  Spec.it s "you may not attack with a creature you do not control" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, theirs) = S.combatBoard piker 1 1
    case theirs of
      [] -> Spec.assertFailure s "fixture should have a blocker"
      oid : _ -> Spec.assertBool s (not (Combat.canAttack S.alice oid gs)) "not alice's"
  -- CR 302.6 restricts attacking and tap abilities. It says NOTHING about
  -- blocking, and getting this wrong is the classic beginner bug.
  Spec.it s "CR 302.6 a summoning sick creature MAY block" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, theirs) = S.combatBoard piker 1 1
    case theirs of
      [] -> Spec.assertFailure s "fixture should have a blocker"
      oid : _ ->
        let sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}
         in Spec.assertBool s (Combat.canBlock S.bob oid sick) "may block"
  Spec.it s "CR 509.1a a tapped creature may not block" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, theirs) = S.combatBoard piker 1 1
    case theirs of
      [] -> Spec.assertFailure s "fixture should have a blocker"
      oid : _ ->
        let tapped = gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}
         in Spec.assertBool s (not (Combat.canBlock S.bob oid tapped)) "may not block"
  Spec.it s "legalAttackers lists exactly the active player's creatures" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoard piker 2 3
    Spec.assertEqWith s "two" (Combat.legalAttackers S.alice gs) mine
  Spec.it s "CR 508.1a a player can attack with a creature they control but do not own" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        gs0 = S.giveControl oid S.alice base
    Spec.assertBool s (elem oid (Combat.legalAttackers S.alice gs0)) "alice may attack with it"
    Spec.assertBool s (notElem oid (Combat.legalAttackers S.bob gs0)) "bob may not (not the controller, not active)"
  Spec.it s "combat starts empty and clears" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoard piker 1 0
        busy = case mine of
          [] -> gs
          oid : _ ->
            gs
              { GameState.combat =
                  Combat.Type.MkCombat
                    { Combat.Type.attackers = Map.singleton oid (AttackTarget.OfPlayer S.bob),
                      Combat.Type.blockers = Map.empty,
                      Combat.Type.struckFirst = Nothing,
                      Combat.Type.joinedUnder = Map.singleton oid S.alice,
                      Combat.Type.attacked = Set.singleton (AttackTarget.OfPlayer S.bob),
                      Combat.Type.declaredAttacked = Set.singleton (AttackTarget.OfPlayer S.bob),
                      Combat.Type.blockersDeclared = True,
                      Combat.Type.defender = Just S.bob
                    }
              }
    Spec.assertEqWith s "starts empty" (Combat.Type.attackers (GameState.combat gs)) Map.empty
    Spec.assertEqWith s "clears" (Combat.Type.attackers (GameState.combat (Combat.clearCombat busy))) Map.empty
    -- CR 506.7c: the CR 511.3 reset re-arms CR 506.7b's boundary, so a CR 500.8
    -- second combat phase gets its own window rather than inheriting this one's.
    Spec.assertBool s (Combat.afterBlockersDeclared busy) "CR 506.7b's boundary is up while combat is live"
    Spec.assertBool s (not (Combat.afterBlockersDeclared (Combat.clearCombat busy))) "and down again once combat clears"

keywordSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
keywordSpec s registry = Spec.describe s "Keyword" $ do
  let gs0 = Setup.emptyGame S.bothPlayers
      -- Each M2a printing carries exactly its one keyword and no other.
      carriesOnly (name, keyword) =
        Spec.it s (name <> " carries exactly " <> show keyword) $ do
          printing <- S.printingOf s registry name
          let (oid, gs) = S.addCreature printing S.alice gs0
          Spec.assertEqWith s "keywords" (Projection.keywordsOf oid gs) (Map.singleton keyword 1)
          Spec.assertBool s (Projection.hasKeyword keyword oid gs) "hasKeyword"
  mapM_ carriesOnly S.m2aKeywords
  Spec.it s "a Piker has no keywords" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addCreature piker S.alice gs0
    Spec.assertEqWith s "none" (Projection.keywordsOf oid gs) Map.empty
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying oid gs)) "no flying"
  Spec.it s "a Mountain has no keywords" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.landsInPlay mountain 1
    case Game.zoneMembers Zone.Battlefield S.alice gs of
      [] -> Spec.assertFailure s "fixture should have one Mountain"
      oid : _ -> Spec.assertEqWith s "none" (Projection.keywordsOf oid gs) Map.empty
  Spec.it s "an unknown id has no keywords" $
    Spec.assertEqWith s "none" (Projection.keywordsOf (ObjectId.MkObjectId 999) gs0) Map.empty
  -- Flying is on Bird Maiden and NOT on Nimble Birdsticker. If this
  -- passes while the reach case above also passes, the two keywords
  -- are genuinely distinct rather than one flag.
  Spec.it s "reach is not flying" $ do
    nimbleBirdsticker <- S.printingOf s registry "Nimble Birdsticker"
    let (oid, gs) = S.addCreature nimbleBirdsticker S.alice gs0
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying oid gs)) "no flying"

-- Run whole steps until the first-strike combat damage step has been dealt
-- (struckFirst is set) or combat ends, so a test can observe the board BETWEEN
-- the two combat damage steps.
runToFirstStrikeDone :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToFirstStrikeDone answer gs0 =
  let go n g =
        if n <= (0 :: Int)
          || Maybe.isJust (Combat.Type.struckFirst (GameState.combat g))
          || not (S.inCombatPhase (GameState.phase g))
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 24 gs0

firstStrikeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
firstStrikeSpec s registry = Spec.describe s "FirstStrike" $ do
  Spec.it s "CR 702.7b a first striker kills a vanilla blocker and lives" $ do
    -- The tiger (2/1 first strike) kills the Piker (2/1) in the first-strike
    -- step; the SBA between steps buries it before it can deal, so the tiger
    -- survives at zero damage.
    sabretoothTiger <- S.printingOf s registry "Sabretooth Tiger"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [sabretoothTiger] [piker]
        after = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith s "the blocker is dead" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "the first striker lives" (S.creaturesInPlay S.alice after) 1
  Spec.it s "CR 510.2 the control: two vanilla 2/1s trade" $ do
    -- With a Piker in the tiger's place there is one combat damage step and
    -- both die. So first strike is the sole cause above.
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [piker] [piker]
        after = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith s "alice's is dead" (S.creaturesInPlay S.alice after) 0
    Spec.assertEqWith s "bob's is dead" (S.creaturesInPlay S.bob after) 0
  Spec.it s "CR 702.4b a double striker deals twice to an unblocked player" $ do
    -- The raptor (2/1 double strike) deals 2 in each step: bob loses 4.
    ridgetopRaptor <- S.printingOf s registry "Ridgetop Raptor"
    let (gs, _, _) = S.combatBoardOf [ridgetopRaptor] []
        after = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith s "bob took 4" (S.lifeOf S.bob after) (Just 16)
  Spec.it s "CR 702.7b the control: a first striker deals once to a player" $ do
    sabretoothTiger <- S.printingOf s registry "Sabretooth Tiger"
    let (gs, _, _) = S.combatBoardOf [sabretoothTiger] []
        after = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith s "bob took 2" (S.lifeOf S.bob after) (Just 18)
  Spec.it s "CR 510.1b the control: a vanilla creature deals once to a player" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [piker] []
        after = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith s "bob took 2" (S.lifeOf S.bob after) (Just 18)
  Spec.it s "CR 510.4 double strike kills a 3/3 across two steps; first strike does not" $ do
    -- The raptor deals 2 + 2 = 4 to the Ogre (3/3), killing it. A first
    -- striker deals 2 once, and the Ogre lives.
    ridgetopRaptor <- S.printingOf s registry "Ridgetop Raptor"
    sabretoothTiger <- S.printingOf s registry "Sabretooth Tiger"
    ogreSentry <- S.printingOf s registry "Ogre Sentry"
    let raptorVs = S.combatBoardOf [ridgetopRaptor] [ogreSentry]
        tigerVs = S.combatBoardOf [sabretoothTiger] [ogreSentry]
        afterRaptor = S.runCombat S.aggressiveAnswer (frst raptorVs)
        afterTiger = S.runCombat S.aggressiveAnswer (frst tigerVs)
    Spec.assertEqWith s "double strike kills the Ogre" (S.creaturesInPlay S.bob afterRaptor) 0
    Spec.assertEqWith s "first strike leaves the Ogre" (S.creaturesInPlay S.bob afterTiger) 1
  Spec.it s "CR 510.4 a striker killed in the first step does not deal in the second" $ do
    -- Raptor (double strike) and tiger (first strike) each block-kill the
    -- other in the first step. Neither is "remaining" for the second step, so
    -- no second-wave damage; both are simply dead.
    ridgetopRaptor <- S.printingOf s registry "Ridgetop Raptor"
    sabretoothTiger <- S.printingOf s registry "Sabretooth Tiger"
    let (gs, _, _) = S.combatBoardOf [ridgetopRaptor] [sabretoothTiger]
        after = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith s "attacker dead" (S.creaturesInPlay S.alice after) 0
    Spec.assertEqWith s "blocker dead" (S.creaturesInPlay S.bob after) 0
  Spec.it s "CR 510.4 the mixed board: first strike once, vanilla once, double strike twice" $ do
    -- Tiger (first strike), raptor (double strike) and Piker (vanilla) all
    -- attack unblocked. First-strike step: tiger 2 + raptor 2 = 4. Second
    -- step: raptor 2 + Piker 2 = 4. bob: 20 - 8 = 12. The naive "strikers in
    -- step one, everyone else in step two" drops the raptor's second hit and
    -- lands bob at 14.
    sabretoothTiger <- S.printingOf s registry "Sabretooth Tiger"
    ridgetopRaptor <- S.printingOf s registry "Ridgetop Raptor"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [sabretoothTiger, ridgetopRaptor, piker] []
        mid = runToFirstStrikeDone S.aggressiveAnswer gs
        after = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith s "after the first-strike step, bob took 4" (S.lifeOf S.bob mid) (Just 16)
    Spec.assertEqWith s "after both steps, bob took 8" (S.lifeOf S.bob after) (Just 12)

-- Attacks and blocks with everything, and casts whenever a cast is legal --
-- aggressiveAnswer's combat decisions with castAnswer's priority decision. The
-- end-of-combat group needs both: the attack has to happen for there to be an
-- attacking creature, and the spell has to be cast for the attacking-ness to be
-- observable.
attackAndCast :: Prompt.Prompt r -> r
attackAndCast p = case p of
  Prompt.ChooseAction {} -> S.castAnswer p
  _ -> S.aggressiveAnswer p

-- alice attacks with one Piker while holding a Kill Shot and exactly the three
-- Plains that pay for it; bob has nothing, so the attack is unblocked. Sits at
-- the declare attackers step like every combatBoardOf board, so the ENGINE
-- declares the attack and carries it forward -- the combat record this group
-- observes is never hand-written. S.addCreature is what puts the Plains out: it
-- is the "any printing, on the battlefield, untapped and Settled" helper its
-- haddock says it is, and lands need exactly that.
killShotBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> GameState.GameState
killShotBoard plains piker killShot =
  let (gs0, _, _) = S.combatBoardOf [piker] []
      addLands n g = if n <= (0 :: Int) then g else addLands (n - 1) (snd (S.addCreature plains S.alice g))
      (withCard, _) = S.handOne killShot (addLands 3 gs0)
   in -- handOne parks its state in a precombat main phase; this board is mid-combat.
      withCard {GameState.phase = GameState.phase gs0, GameState.priority = GameState.priority gs0}

-- Run whole steps until the end of combat step is the current phase, WITHOUT
-- running it, so a test can play that one step itself under a different
-- answerer.
runToEndOfCombat :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToEndOfCombat = S.runToStep (Phase.Combat CombatStep.EndOfCombat)

-- CR 511.3: creatures are removed from combat as the end of combat step ENDS, so
-- they are still attacking for the whole of that step -- including its priority
-- round (CR 511.1), where the active player may cast an instant. Kill Shot
-- ("Destroy target attacking creature") is what makes the window observable: it
-- has a legal target during the end of combat step and none after it.
endOfCombatSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
endOfCombatSpec s registry = Spec.describe s "EndOfCombat" $ do
  Spec.it s "CR 511.3 whole card: Kill Shot destroys an attacker during the end of combat step" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    killShot <- S.printingOf s registry "Kill Shot"
    let atEnd = runToEndOfCombat S.aggressiveAnswer (killShotBoard plains piker killShot)
        after = snd (Engine.runGamePure attackAndCast atEnd Engine.runStep)
    Spec.assertEqWith s "the step under test is the end of combat step" (GameState.phase atEnd) (Phase.Combat CombatStep.EndOfCombat)
    Spec.assertBool s (not (Map.null (Combat.Type.attackers (GameState.combat atEnd)))) "the Piker is still attacking as the step begins"
    Spec.assertEqWith s "the attacker was destroyed" (S.creaturesInPlay S.alice after) 0
  Spec.it s "CR 511.3 the removal still happens, one step later: combat is empty once the step ends" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    killShot <- S.printingOf s registry "Kill Shot"
    let atEnd = runToEndOfCombat S.aggressiveAnswer (killShotBoard plains piker killShot)
        after = snd (Engine.runGamePure S.aggressiveAnswer atEnd Engine.runStep)
    Spec.assertEqWith s "the combat phase is over" (GameState.phase after) Phase.PostcombatMain
    Spec.assertEqWith s "no attackers" (Combat.Type.attackers (GameState.combat after)) Map.empty
    -- CR 506.2's designation is scoped to the combat phase, and clearCombat
    -- resets it alongside the attackers.
    Spec.assertEqWith s "no defending player" (Combat.Type.defender (GameState.combat after)) Nothing
  Spec.it s "CR 511.3 the twin: the same Kill Shot has no target in the postcombat main phase" $ do
    -- The discriminator for the case above. If IsAttacking simply read True
    -- for every creature, or if combat were never cleared at all, this would
    -- kill the Piker too.
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    killShot <- S.printingOf s registry "Kill Shot"
    let atEnd = runToEndOfCombat S.aggressiveAnswer (killShotBoard plains piker killShot)
        postcombat = snd (Engine.runGamePure S.aggressiveAnswer atEnd Engine.runStep)
        after = snd (Engine.runGamePure attackAndCast postcombat Engine.runStep)
    Spec.assertEqWith s "the step under test is the postcombat main phase" (GameState.phase postcombat) Phase.PostcombatMain
    Spec.assertEqWith s "the Piker survives" (S.creaturesInPlay S.alice after) 1

-- The state out of a combatBoardOf triple.
frst :: (a, b, c) -> a
frst (a, _, _) = a

m2bExitSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
m2bExitSpec s registry = Spec.describe s "M2bExit" $ do
  Spec.it s "the milestone: first strike breaks the trade, double strike doubles the hit, no attacker no damage" $ do
    sabretoothTiger <- S.printingOf s registry "Sabretooth Tiger"
    piker <- S.printingOf s registry "Goblin Piker"
    ridgetopRaptor <- S.printingOf s registry "Ridgetop Raptor"
    let trade = S.runCombat S.aggressiveAnswer (frst (S.combatBoardOf [sabretoothTiger] [piker]))
        doubled = S.runCombat S.aggressiveAnswer (frst (S.combatBoardOf [ridgetopRaptor] []))
        quiet = S.runCombat S.aggressiveAnswer (frst (S.combatBoardOf [] []))
    Spec.assertEqWith s "first striker lives" (S.creaturesInPlay S.alice trade) 1
    Spec.assertEqWith s "its would-be killer is dead" (S.creaturesInPlay S.bob trade) 0
    Spec.assertEqWith s "double striker deals 4" (S.lifeOf S.bob doubled) (Just 16)
    Spec.assertEqWith s "an attacker-less turn deals nothing" (S.lifeOf S.bob quiet) (Just 20)

-- alice is mid-combat with three Pikers; bob holds a Ray of Command and exactly
-- the four Islands that pay for it, and controls nothing else. The board sits at
-- the declare attackers step like every combatBoardOf board, so the ENGINE
-- declares the attack and carries it forward: no test here writes the combat
-- record. S.addCreature is what puts the Islands out -- the "any printing, on the
-- battlefield, untapped and Settled" helper its haddock says it is.
rayBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, [ObjectId.ObjectId])
rayBoard island piker ray =
  let (gs0, mine, _) = S.combatBoardOf [piker, piker, piker] []
      addLands n g = if n <= (0 :: Int) then g else addLands (n - 1) (snd (S.addCreature island S.bob g))
   in (snd (S.addHandCard ray S.bob (addLands 4 gs0)), mine)

-- Attack with everything except `homebody`, never block, cast whenever a cast is
-- offered, and aim every target at `victim`.
--
-- Blocks are DECLINED rather than routed, and that is what keeps the two legs
-- comparable: a stolen attacker arrives untapped (Ray of Command untaps it) and
-- hasty under its new controller, so an aggressive blocker answer would have bob
-- block with the very creature the case is about and hide the damage question
-- behind CR 509.1's routing.
steal :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
steal homebody victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victim))) sets
  Prompt.ChooseAction {} -> S.castAnswer p
  Prompt.DeclareAttackers _ _ ids -> filter (/= homebody) ids
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

-- Block with everything, cast whenever a cast is offered, aim every target at
-- `victim`. The blocker-side twin of `steal`.
snatch :: ObjectId.ObjectId -> Prompt.Prompt r -> r
snatch victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victim))) sets
  Prompt.ChooseAction {} -> S.castAnswer p
  _ -> S.aggressiveAnswer p

-- CR 506.4: "A permanent is removed from combat if it leaves the battlefield, if
-- its controller changes, ..." -- and a creature so removed "stops being an
-- attacking, blocking, blocked, and/or unblocked creature".
--
-- Ray of Command is the pool's producer for the control-change clause: {3}{U},
-- INSTANT, "Untap target creature an opponent controls and gain control of it
-- until end of turn. That creature gains haste until end of turn." Act of Treason
-- has the same three effects and cannot reach this window at all, because it is a
-- sorcery -- which is why this clause was worked card-driven rather than built
-- speculatively. Ray of Command's third sentence, the delayed trigger that taps the
-- creature when its controller loses it, does not reach these legs: every one of
-- them stops at the end of combat step, well before the CR 514.2 sweep that ends
-- the control effect. Pawl.TriggerSpec's "RayOfCommand" group is what proves it.
--
-- Every leg runs whole steps through Engine.runStep, so the combat record under
-- test is the engine's own and the removal is observed where a player would see
-- it: at the CR 117.5 settle that follows the spell resolving. Each leg stops at
-- the end of combat step, where CR 511.3 says the record still reads live.
controlChangeRemovalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
controlChangeRemovalSpec s registry = Spec.describe s "ControlChangeRemoval" $ do
  Spec.it s "CR 506.4 whole card: Ray of Command on an attacker removes THAT attacker from combat, and it deals no combat damage" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    rayOfCommand <- S.printingOf s registry "Ray of Command"
    killShot <- S.printingOf s registry "Kill Shot"
    case (rayBoard island piker rayOfCommand, S.spellTargetSlot killShot) of
      ((gs, [stolen, other, homebody]), Just attackingSlot) -> do
        let atEnd = runToEndOfCombat (steal homebody stolen) gs
            attackers = Combat.Type.attackers (GameState.combat atEnd)
            legal = Target.legalRecipients Nothing S.noSource attackingSlot atEnd
        Spec.assertEqWith s "the leg really reached the end of combat step, where the record still reads live (CR 511.3)" (GameState.phase atEnd) (Phase.Combat CombatStep.EndOfCombat)
        Spec.assertEqWith s "bob really did gain control of it" (Projection.controllerOf stolen atEnd) (Just S.bob)
        Spec.assertBool s (Map.notMember stolen attackers) "CR 506.4: so it is no longer an attacking creature"
        Spec.assertBool s (Map.member other attackers) "the attacker bob left alone is untouched"
        -- The discriminating assertion: the unfixed engine keeps the stolen
        -- Piker in the record and deals its 2 alongside the other's.
        Spec.assertEqWith s "CR 510.1: bob takes only the surviving attacker's 2" (S.lifeOf S.bob atEnd) (Just 18)
        -- CR 508.1k through the door a card actually uses: Kill Shot's own
        -- committed target slot is Pool.Creatures narrowed by IsAttacking.
        Spec.assertBool s (not (Set.member (Recipient.ToCreature stolen) legal)) "Filter.IsAttacking no longer finds the stolen creature"
        Spec.assertBool s (Set.member (Recipient.ToCreature other) legal) "and still finds the one that is attacking"
      _ -> Spec.assertFailure s "fixture should have three Pikers and Kill Shot a 'target' slot"
  Spec.it s "CR 506.4 the twin: the same Ray of Command on a creature that is not in combat leaves combat intact" $ do
    -- The control leg, and the reason the case above is not passing for a
    -- trivial reason. The SAME card resolves, the SAME settle runs, and
    -- control really does change -- just not for a combatant. A sampler that
    -- cleared combat whenever it saw a control change, or whenever anything
    -- resolved, would take the attackers out here too.
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    rayOfCommand <- S.printingOf s registry "Ray of Command"
    case rayBoard island piker rayOfCommand of
      (gs, [one, two, homebody]) -> do
        let atEnd = runToEndOfCombat (steal homebody homebody) gs
            attackers = Combat.Type.attackers (GameState.combat atEnd)
        Spec.assertEqWith s "bob gained control of the creature that stayed home" (Projection.controllerOf homebody atEnd) (Just S.bob)
        Spec.assertBool s (Map.member one attackers && Map.member two attackers) "both attackers are still attacking"
        Spec.assertEqWith s "so bob takes both hits" (S.lifeOf S.bob atEnd) (Just 16)
      _ -> Spec.assertFailure s "fixture should have three Pikers"
  Spec.it s "CR 506.4 a stolen BLOCKER is removed from combat, and CR 509.1h leaves the attacker blocked" $ do
    -- The blocker side of the same clause, and the interaction the
    -- Combat.blockers shape exists for: Game.removeFromCombat drops the
    -- blocker from the SET while the attacker's KEY survives, so the attacker
    -- stays blocked and (CR 510.1c) assigns no combat damage at all.
    --
    -- The theft has to land after blocks are declared, so the declare
    -- attackers step is played under an answerer that does not cast and only
    -- the declare blockers step onwards sees `snatch`.
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    rayOfCommand <- S.printingOf s registry "Ray of Command"
    let (gs0, mine, theirs) = S.combatBoardOf [piker] [piker]
        addLands n g = if n <= (0 :: Int) then g else addLands (n - 1) (snd (S.addCreature island S.alice g))
        gs = snd (S.addHandCard rayOfCommand S.alice (addLands 4 gs0))
    case (mine, theirs) of
      (attacker : _, blocker : _) -> do
        let atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
            atEnd = runToEndOfCombat (snatch blocker) atBlockers
            -- The control leg: the same board and the same blocks, with alice
            -- never casting. Two 2/1 Pikers then trade and both die.
            traded = runToEndOfCombat S.aggressiveAnswer atBlockers
        Spec.assertEqWith s "the leg hands over at the declare blockers step, so `snatch` is what declares the blocks and then casts" (GameState.phase atBlockers) (Phase.Combat CombatStep.DeclareBlockers)
        -- The discriminating assertion, and first because it is the one the
        -- unfixed engine fails: with the blocker still in the record the two
        -- Pikers trade, and the ids below stop resolving at all.
        Spec.assertBool s (S.onBattlefield attacker atEnd && S.onBattlefield blocker atEnd) "CR 510.1c: neither creature was dealt combat damage"
        Spec.assertEqWith s "alice really did gain control of the blocker" (Projection.controllerOf blocker atEnd) (Just S.alice)
        Spec.assertEqWith s "CR 506.4: it is blocking nothing" (Combat.blockersOf attacker atEnd) Set.empty
        Spec.assertBool s (Combat.isBlocked attacker atEnd) "CR 509.1h: but the attacker remains blocked"
        Spec.assertEqWith s "and bob takes nothing" (S.lifeOf S.bob atEnd) (Just 20)
        Spec.assertBool s (not (S.onBattlefield attacker traded) && not (S.onBattlefield blocker traded)) "control leg: with no theft the two Pikers trade and both die"
      _ -> Spec.assertFailure s "fixture did not build an attacker and a blocker"

-- Labyrinth of Skophos' SECOND activated ability -- "{4}, {T}: Remove target
-- attacking or blocking creature from combat" -- read off the JSON-loaded
-- printing rather than hand-built, so every leg below exercises the codec's
-- parse of the committed card data (S.spellTargetSlot's posture, for an
-- activated ability rather than a spell). The first is the land's "{T}: Add
-- {C}".
removalAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card)
removalAbility printing = case Face.activatedAbilities (S.combinedFace printing) of
  [_, ability] -> Just ability
  _ -> Nothing

-- That ability's "target" slot: CR 601.2c's narrowing, reached for an
-- activated ability through CR 602.2b, which for this card is Pool.Creatures
-- under `Or [IsAttacking, IsBlocking]`.
removalTargetSlot :: ActivatedAbility.ActivatedAbility Card.Type.Card -> Maybe TargetSlot.TargetSlot
removalTargetSlot ability =
  Map.lookup
    (SlotName.MkSlotName (Text.pack "target"))
    (Modal.allTargetSlots (ActivatedAbility.modal ability))

-- alice is mid-combat with one creature per printing in `mine`; bob defends with
-- one per printing in `theirs`. `who` also controls a Labyrinth of Skophos and
-- the four lands that pay its {4}. S.addCreature is what puts all five out --
-- the "any printing, on the battlefield, untapped and Settled" helper its
-- haddock says it is, which is what a land needs.
skophosBoard ::
  Printing.Printing ->
  Printing.Printing ->
  PlayerId.PlayerId ->
  [Printing.Printing] ->
  [Printing.Printing] ->
  (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId], ObjectId.ObjectId)
skophosBoard labyrinth land who mine theirs =
  let (gs0, ours, yours) = S.combatBoardOf mine theirs
      addLands n g = if n <= (0 :: Int) then g else addLands (n - 1) (snd (S.addCreature land who g))
      (mazeId, gs1) = S.addCreature labyrinth who (addLands 4 gs0)
   in (gs1, ours, yours, mazeId)

-- Fire the Labyrinth's removal ability once, aim it at `victim`, and pay the {4}
-- with anything BUT the Labyrinth itself: CR 601.2g pays an activation's mana
-- before its components (Pawl.Engine.Activate), so tapping the land for its own {C}
-- would leave the {T} unpayable and revert the whole activation. Choosing around
-- that is the player's job, not the engine's.
--
-- Every other prompt falls through to S.aggressiveAnswer, so attacks and blocks
-- still happen.
--
-- STATEFUL, and it has to be, for GameSpec's illegalActivationAnswer reason: an
-- answerer that names the same Activate at every ask never lets the priority
-- loop terminate once the activation stops SUCCEEDING. A rejected one is a
-- no-op (CR 601.2c: an answer outside the legal target set reverts the whole
-- activation), so the cost goes unpaid, the land stays untapped, and the same
-- action is offered again forever. That is unreachable while the engine is
-- right, and this was written pure first: breaking Filter.IsBlocking on purpose
-- hung the suite instead of failing it, which is not a test.
mazeAnswer ::
  ObjectId.ObjectId ->
  ActivatedAbility.ActivatedAbility Card.Type.Card ->
  ObjectId.ObjectId ->
  Prompt.Prompt r ->
  State.State Bool r
mazeAnswer mazeId ability victim p = case p of
  Prompt.ChooseAction _ _ actions -> do
    tried <- State.get
    if tried || notElem (A.Activate mazeId ability) actions
      then pure A.Pass
      else do
        State.put True
        pure (A.Activate mazeId ability)
  Prompt.ChooseTargets _ _ _ sets -> pure (fmap (const (Set.singleton (Recipient.ToCreature victim))) sets)
  Prompt.ChooseManaSource _ _ candidates ->
    pure (Just (Maybe.fromMaybe (NonEmpty.head candidates) (List.find (/= mazeId) (NonEmpty.toList candidates))))
  _ -> pure (S.aggressiveAnswer p)

-- runToEndOfCombat's stateful twin, for the answerer above: the same bounded
-- walk of whole steps, threading the "have I activated yet" flag across them.
runToEndOfCombatWith ::
  (forall r. Prompt.Prompt r -> State.State Bool r) ->
  GameState.GameState ->
  GameState.GameState
runToEndOfCombatWith answer gs0 =
  let go n g s =
        if n <= (0 :: Int)
          || GameState.phase g == Phase.Combat CombatStep.EndOfCombat
          || not (S.inCombatPhase (GameState.phase g))
          then g
          else
            let ((_, g1), s1) = State.runState (Engine.runGame answer g Engine.runStep) s
             in go (n - 1) g1 s1
   in go 8 gs0 False

-- Attack with everything except `homebody`, and otherwise behave aggressively --
-- so the board carries an attacking creature, a blocking creature and a creature
-- that is neither, which is what the target filter has to tell apart.
stayHomeAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> r
stayHomeAnswer homebody p = case p of
  Prompt.DeclareAttackers _ _ ids -> filter (/= homebody) ids
  _ -> S.aggressiveAnswer p

-- CR 506.4: "A permanent is removed from combat if ... an effect specifically
-- removes it from combat." The rule's one clause a card ASKS for, rather than a
-- condition the engine has to notice -- and Labyrinth of Skophos is the pool's
-- producer: "{T}: Add {C}. / {4}, {T}: Remove target attacking or blocking
-- creature from combat." (Land, Murders at Karlov Manor Commander; oracle text
-- checked against Scryfall.)
--
-- Every leg runs whole steps through Engine.runStep, so the combat record under
-- test is the engine's own: the fixture declares nothing by hand. The two damage
-- legs stop at the end of combat step, where CR 511.3 says the record still
-- reads live; the filter leg stops one step earlier, before anything dies.
--
-- Removal is removal only. Nothing here puts a creature back into combat, which
-- is what the rules say too -- the glossary's "removed from combat" entry has
-- the permanent take "no further involvement in that combat phase".
effectRemovalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
effectRemovalSpec s registry = Spec.describe s "EffectRemoval" $ do
  Spec.it s "CR 506.4 whole card: Labyrinth of Skophos removes target ATTACKING creature, and it deals no combat damage" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    labyrinth <- S.printingOf s registry "Labyrinth of Skophos"
    case (removalAbility labyrinth, skophosBoard labyrinth island S.bob [piker] []) of
      (Just ability, (gs, [attacker], _, mazeId)) -> do
        let atEnd = runToEndOfCombatWith (mazeAnswer mazeId ability attacker) gs
            quiet = runToEndOfCombat S.aggressiveAnswer gs
            legal = fmap (\theSlot -> Target.legalRecipients Nothing S.noSource theSlot atEnd) (removalTargetSlot ability)
        Spec.assertEqWith s "the leg really reached the end of combat step, where the record still reads live (CR 511.3)" (GameState.phase atEnd) (Phase.Combat CombatStep.EndOfCombat)
        Spec.assertEqWith s "the ability really was activated: its {T} component was paid" (tapStateOf mazeId atEnd) (Just TapState.Tapped)
        Spec.assertBool s (Map.notMember attacker (Combat.Type.attackers (GameState.combat atEnd))) "CR 506.4: the Piker stopped being an attacking creature"
        -- The discriminating assertion: with the removal missing, the Piker
        -- stays in the record and deals its 2.
        Spec.assertEqWith s "CR 510.1: so bob takes nothing" (S.lifeOf S.bob atEnd) (Just 20)
        Spec.assertEqWith s "and the card's own target filter no longer finds it" (fmap (Set.member (Recipient.ToCreature attacker)) legal) (Just False)
        Spec.assertBool s (Map.member attacker (Combat.Type.attackers (GameState.combat quiet))) "control leg: unactivated, the Piker is still attacking"
        Spec.assertEqWith s "and bob takes its 2" (S.lifeOf S.bob quiet) (Just 18)
      _ -> Spec.assertFailure s "fixture should give bob a Labyrinth with two abilities and alice one Piker"
  Spec.it s "CR 509.1h a removed BLOCKER leaves the attacker blocked, so nothing is dealt combat damage" $ do
    -- The blocker side of the same clause, and the interaction
    -- Game.removeFromCombat's two-way edit of Combat.blockers exists for: the
    -- blocker leaves the SET while the attacker's KEY survives, so the
    -- attacker stays blocked and (CR 510.1c) assigns no combat damage at all.
    --
    -- alice holds the Labyrinth and aims it at her opponent's blocker, so the
    -- removal has to land after blocks are declared: the declare attackers
    -- step is played under an answerer that never activates, and only the
    -- declare blockers step onwards sees `mazeAnswer`.
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    labyrinth <- S.printingOf s registry "Labyrinth of Skophos"
    case (removalAbility labyrinth, skophosBoard labyrinth island S.alice [piker] [piker]) of
      (Just ability, (gs, [attacker], [blocker], mazeId)) -> do
        let atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
            atEnd = runToEndOfCombatWith (mazeAnswer mazeId ability blocker) atBlockers
            -- The control leg: the same board and the same blocks, with the
            -- ability never activated. Two 2/1 Pikers then trade.
            traded = runToEndOfCombat S.aggressiveAnswer atBlockers
        Spec.assertEqWith s "the leg hands over at the declare blockers step, so the blocks are declared before the activation" (GameState.phase atBlockers) (Phase.Combat CombatStep.DeclareBlockers)
        Spec.assertEqWith s "the ability really was activated" (tapStateOf mazeId atEnd) (Just TapState.Tapped)
        Spec.assertBool s (S.onBattlefield attacker atEnd && S.onBattlefield blocker atEnd) "CR 510.1c: neither creature was dealt combat damage"
        Spec.assertEqWith s "CR 506.4: the removed creature is blocking nothing" (Combat.blockersOf attacker atEnd) Set.empty
        Spec.assertBool s (Combat.isBlocked attacker atEnd) "CR 509.1h: but the attacker remains blocked"
        Spec.assertEqWith s "so bob takes nothing either" (S.lifeOf S.bob atEnd) (Just 20)
        Spec.assertBool s (not (S.onBattlefield attacker traded) && not (S.onBattlefield blocker traded)) "control leg: unactivated, the two Pikers trade and both die"
      _ -> Spec.assertFailure s "fixture should give alice a Labyrinth and an attacker, and bob a blocker"
  Spec.it s "CR 601.2c the card's filter admits the attacker and the blocker and rejects the creature that stayed home" $ do
    -- Or [IsAttacking, IsBlocking], and both halves are load-bearing: with
    -- IsAttacking alone the blocker would be rejected, and with no filter at
    -- all the homebody would be admitted.
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    labyrinth <- S.printingOf s registry "Labyrinth of Skophos"
    case (removalAbility labyrinth, skophosBoard labyrinth island S.alice [piker, piker] [piker]) of
      (Just ability, (gs, [attacker, homebody], [blocker], _)) -> do
        -- The combat damage step is the vantage point: blockers have been
        -- declared and nothing has died yet.
        let atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) (stayHomeAnswer homebody) gs
            legal = fmap (\theSlot -> Target.legalRecipients Nothing S.noSource theSlot atDamage) (removalTargetSlot ability)
            admits oid = fmap (Set.member (Recipient.ToCreature oid)) legal
        Spec.assertEqWith s "the fixture reached the combat damage step with blocks declared" (GameState.phase atDamage) (Phase.Combat CombatStep.CombatDamage)
        Spec.assertBool s (Set.member blocker (Combat.blockersOf attacker atDamage)) "the blocker really is blocking the attacker"
        Spec.assertEqWith s "IsAttacking admits the attacker" (admits attacker) (Just True)
        Spec.assertEqWith s "IsBlocking admits the blocker" (admits blocker) (Just True)
        Spec.assertEqWith s "and the creature in neither role is rejected" (admits homebody) (Just False)
      _ -> Spec.assertFailure s "fixture should give alice two Pikers and a Labyrinth, and bob a blocker"

-- alice is mid-combat with Opalescence, Living Plane and a Goblin Piker, plus one
-- Forest that Living Plane has made a 1/1 creature; bob defends with nothing but
-- the two Swamps that pay for the Doom Blade in his hand. The board sits at the
-- declare attackers step like every combatBoardOf board, so the ENGINE declares
-- the attack: no test here writes the combat record.
--
-- Returns alice's three combatBoardOf permanents in printing order alongside the
-- Forest, which is added separately because it is not one of them.
unmakeBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, [ObjectId.ObjectId], ObjectId.ObjectId)
unmakeBoard opalescence livingPlane piker forest swamp doomBlade =
  let (gs0, mine, _) = S.combatBoardOf [opalescence, livingPlane, piker] []
      (land, gs1) = S.addCreature forest S.alice gs0
      addSwamps n g = if n <= (0 :: Int) then g else addSwamps (n - 1) (snd (S.addCreature swamp S.bob g))
   in (snd (S.addHandCard doomBlade S.bob (addSwamps 2 gs1)), mine, land)

-- alice attacks with `land` alone, nobody blocks, and whoever is offered a cast
-- takes it and aims every target at `victim`. The shape of `steal` above, with a
-- reason of its own for declining blocks: bob's own lands are 1/1 creatures while
-- Living Plane lives, so an aggressive blocker answer would put them in front of
-- the attacker and hide the question this asks behind CR 509.1's routing.
unmake :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
unmake land victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victim))) sets
  Prompt.ChooseAction {} -> S.castAnswer p
  Prompt.DeclareAttackers _ _ ids -> filter (== land) ids
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

-- alice attacks with one Goblin Piker and holds the Doom Blade and the two
-- Swamps that pay for it; bob defends with Opalescence, Living Plane and the
-- Forest that Living Plane has made a 1/1 creature. The mirror of unmakeBoard,
-- with the animator on the DEFENDING side so the creature that stops being one is
-- a blocker.
unblockBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId], ObjectId.ObjectId)
unblockBoard opalescence livingPlane piker forest swamp doomBlade =
  let (gs0, mine, theirs) = S.combatBoardOf [piker] [opalescence, livingPlane]
      (land, gs1) = S.addCreature forest S.bob gs0
      addSwamps n g = if n <= (0 :: Int) then g else addSwamps (n - 1) (snd (S.addCreature swamp S.alice g))
   in (snd (S.addHandCard doomBlade S.alice (addSwamps 2 gs1)), mine, theirs, land)

-- Attack with `attacker` alone and cast nothing. The declare attackers step of
-- the blocker leg is played under this, so alice's Swamps -- 1/1 creatures while
-- Living Plane lives, and therefore legal attackers -- stay untapped to pay for
-- the Doom Blade she casts a step later.
attackOnly :: ObjectId.ObjectId -> Prompt.Prompt r -> r
attackOnly attacker p = case p of
  Prompt.DeclareAttackers _ _ ids -> filter (== attacker) ids
  _ -> S.aggressiveAnswer p

-- Block the first attacker with `blocker` alone and cast nothing: the control leg
-- of the blocker case, where Living Plane is left alone and the block resolves
-- into an ordinary trade.
blockOnly :: ObjectId.ObjectId -> Prompt.Prompt r -> r
blockOnly blocker p = case p of
  Prompt.DeclareBlockers _ _ _ attackers -> case attackers of
    a : _ -> Map.singleton blocker (Set.singleton a)
    [] -> Map.empty
  _ -> S.aggressiveAnswer p

-- Block the first attacker with `blocker` alone, cast whenever a cast is offered,
-- and aim every target at `victim`. Blocking with everything instead would put
-- Living Plane -- a 4/4 creature while Opalescence is out -- in front of the
-- attacker too, and killing it would then be a blocker LEAVING THE BATTLEFIELD,
-- which is a different clause of CR 506.4.
unblock :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
unblock blocker victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victim))) sets
  Prompt.ChooseAction {} -> S.castAnswer p
  Prompt.DeclareBlockers _ _ _ attackers -> case attackers of
    a : _ -> Map.singleton blocker (Set.singleton a)
    [] -> Map.empty
  _ -> S.aggressiveAnswer p

-- CR 506.4: "A permanent is removed from combat if ... it's an attacking or
-- blocking creature that ... stops being a creature."
--
-- The clause has no one-card producer, and does not need one: the rule asks about
-- the permanent's creature-ness, not about how it was lost. Three pool cards make
-- it happen through the layer system alone, and every oracle text below was
-- checked against Scryfall.
--
--   * Living Plane ({2}{G}{G} World Enchantment, "All lands are 1/1 creatures
--     that are still lands") is what makes a Forest able to attack or block.
--   * Opalescence ({2}{W}{W} Enchantment, "Each other non-Aura enchantment is a
--     creature in addition to its other types and has base power and base
--     toughness each equal to its mana value") is what puts Living Plane itself
--     within reach of a creature-removal spell. Without it nothing in the pool
--     can touch an enchantment at instant speed, which is why this clause waited
--     on a producer rather than being built speculatively.
--   * Doom Blade ({1}{B} Instant, "Destroy target nonblack creature") kills the
--     green Living Plane in the priority round after the declaration.
--
-- CR 611.3b is what makes that enough: a static ability's continuous effect
-- "applies at all times that the permanent generating it is on the battlefield",
-- so Living Plane leaving takes the animation with it and the Forest stops being
-- a creature WITHOUT leaving the battlefield. That is what makes this the types
-- clause rather than CR 506.4's leaves-the-battlefield one, and each leg asserts
-- the Forest is still on the battlefield to pin it.
--
-- Every leg runs whole steps through Engine.runStep and stops at the end of
-- combat step, where CR 511.3 says the record still reads live.
typeChangeRemovalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
typeChangeRemovalSpec s registry = Spec.describe s "TypeChangeRemoval" $ do
  Spec.it s "CR 506.4 whole cards: an attacking Forest that stops being a creature is removed from combat" $ do
    forest <- S.printingOf s registry "Forest"
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    opalescence <- S.printingOf s registry "Opalescence"
    livingPlane <- S.printingOf s registry "Living Plane"
    doomBlade <- S.printingOf s registry "Doom Blade"
    case unmakeBoard opalescence livingPlane piker forest swamp doomBlade of
      (gs, [_, plane, _], land) -> do
        let atEnd = runToEndOfCombat (unmake land plane) gs
            attackers = Combat.Type.attackers (GameState.combat atEnd)
        Spec.assertEqWith s "the leg really reached the end of combat step, where the record still reads live (CR 511.3)" (GameState.phase atEnd) (Phase.Combat CombatStep.EndOfCombat)
        Spec.assertBool s (elem land (S.attackerDeclarationsOf atEnd)) "the Forest really was attacking: the declaration happened while it was a creature"
        Spec.assertBool s (not (S.onBattlefield plane atEnd)) "the Doom Blade really did kill Living Plane"
        Spec.assertBool s (not (Projection.isCreatureOf land atEnd)) "CR 611.3b: so the Forest stopped being a creature"
        Spec.assertBool s (S.onBattlefield land atEnd) "and is still on the battlefield, so this is the types clause and not the leaves-the-battlefield one"
        -- The discriminating assertion: the unfixed engine leaves the Forest
        -- in the record as an attacking creature, which CR 506.3 says a
        -- noncreature permanent cannot be.
        Spec.assertBool s (Map.notMember land attackers) "CR 506.4: it is no longer an attacking creature"
        Spec.assertEqWith s "CR 510.1: and bob takes nothing" (S.lifeOf S.bob atEnd) (Just 20)
      _ -> Spec.assertFailure s "fixture should give alice Opalescence, Living Plane and a Piker"
  Spec.it s "CR 506.4 the twin: the same Doom Blade on a creature that is not the animator leaves combat intact" $ do
    -- The control leg, and the reason the case above is not passing for a
    -- trivial reason. The SAME card resolves, the SAME settle runs, and a
    -- creature really does die -- just not the one the Forest's creature-ness
    -- hangs on. A sampler that cleared combat whenever anything died, or
    -- whenever anything resolved, would take the Forest out here too.
    forest <- S.printingOf s registry "Forest"
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    opalescence <- S.printingOf s registry "Opalescence"
    livingPlane <- S.printingOf s registry "Living Plane"
    doomBlade <- S.printingOf s registry "Doom Blade"
    case unmakeBoard opalescence livingPlane piker forest swamp doomBlade of
      (gs, [_, plane, homebody], land) -> do
        let atEnd = runToEndOfCombat (unmake land homebody) gs
            attackers = Combat.Type.attackers (GameState.combat atEnd)
        Spec.assertBool s (not (S.onBattlefield homebody atEnd)) "the Piker that stayed home died instead"
        Spec.assertBool s (S.onBattlefield plane atEnd) "Living Plane survives"
        Spec.assertBool s (Projection.isCreatureOf land atEnd) "so the Forest is still a creature"
        Spec.assertBool s (Map.member land attackers) "and still attacking"
        Spec.assertEqWith s "so bob takes its 1" (S.lifeOf S.bob atEnd) (Just 19)
      _ -> Spec.assertFailure s "fixture should give alice Opalescence, Living Plane and a Piker"
  Spec.it s "CR 509.1h a BLOCKER that stops being a creature leaves the attacker blocked, so nothing is dealt combat damage" $ do
    -- The blocker side of the same clause, through the same performer: the
    -- Forest leaves the SET while the attacker's KEY survives, so the Piker
    -- stays blocked and CR 510.1c gives it nobody to assign damage to.
    --
    -- This is the leg where the removal is observable as DAMAGE. An attacker
    -- that stops being a creature loses its power along with its card type, so
    -- Damage.attackerAssignment's Projection.powerOf already declines to
    -- assign anything for it; a stale BLOCKER is screened only for liveness
    -- (Damage's onBattlefield filter), so the unfixed engine marks the
    -- attacker's 2 on a land that is no longer a creature at all.
    --
    -- The kill has to land after blocks are declared, so the declare attackers
    -- step is played under an answerer that never casts and only the declare
    -- blockers step onwards sees `unblock`.
    forest <- S.printingOf s registry "Forest"
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    opalescence <- S.printingOf s registry "Opalescence"
    livingPlane <- S.printingOf s registry "Living Plane"
    doomBlade <- S.printingOf s registry "Doom Blade"
    case unblockBoard opalescence livingPlane piker forest swamp doomBlade of
      (gs, [attacker], [_, plane], land) -> do
        let atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) (attackOnly attacker) gs
            atEnd = runToEndOfCombat (unblock land plane) atBlockers
            -- The control leg: the same board and the same block, with alice
            -- never casting. The 2/1 Piker and the 1/1 Forest then trade.
            traded = runToEndOfCombat (blockOnly land) atBlockers
        Spec.assertEqWith s "the leg hands over at the declare blockers step, so the block is declared before the kill" (GameState.phase atBlockers) (Phase.Combat CombatStep.DeclareBlockers)
        Spec.assertBool s (not (S.onBattlefield plane atEnd)) "the Doom Blade really did kill Living Plane"
        Spec.assertBool s (not (Projection.isCreatureOf land atEnd)) "CR 611.3b: so the Forest stopped being a creature"
        Spec.assertBool s (S.onBattlefield land atEnd) "and is still on the battlefield"
        -- The discriminating assertion: the unfixed engine leaves the Forest
        -- in the blocker set and marks the Piker's 2 on it.
        Spec.assertEqWith s "CR 510.1c: nothing was dealt combat damage" (S.damageOf land atEnd) (Just 0)
        Spec.assertEqWith s "CR 506.4: the Forest is blocking nothing" (Combat.blockersOf attacker atEnd) Set.empty
        Spec.assertBool s (Combat.isBlocked attacker atEnd) "CR 509.1h: but the attacker remains blocked"
        Spec.assertEqWith s "so bob takes nothing either" (S.lifeOf S.bob atEnd) (Just 20)
        Spec.assertBool s (not (S.onBattlefield attacker traded) && not (S.onBattlefield land traded)) "control leg: with Living Plane left alone the Piker and the Forest trade"
      _ -> Spec.assertFailure s "fixture should give alice a Piker and bob Opalescence, Living Plane and a Forest"

-- CR 508.4: "If a creature is put onto the battlefield attacking, its controller
-- chooses which defending player ... it's attacking ... Such creatures are
-- 'attacking' but, for the purposes of trigger events and effects, they never
-- 'attacked'."
--
-- Hanweir Garrison is the pool's only source of one: "Whenever this creature
-- attacks, create two 1/1 red Human creature tokens that are tapped and
-- attacking."
putOntoBattlefieldAttackingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
putOntoBattlefieldAttackingSpec s registry = Spec.describe s "PutOntoBattlefieldAttacking" $ do
  Spec.it s "CR 508.4 whole card: Hanweir Garrison's two Humans enter tapped and attacking" $ do
    garrison <- S.printingOf s registry "Hanweir Garrison"
    let (gs, mine, _) = S.combatBoardOf [garrison] []
        -- The vantage point is the declare blockers step: the trigger fired
        -- at the declaration (CR 508.2b) and resolved in the declare
        -- attackers step's priority round, and CR 511.3 has not yet cleared
        -- the record.
        atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
        tokens = S.tokensOf atBlockers
        attackers = Combat.Type.attackers (GameState.combat atBlockers)
        sicknessOf oid = fmap Object.sickness (Game.lookupObject oid atBlockers)
    Spec.assertEqWith s "the fixture reached the declare blockers step" (GameState.phase atBlockers) (Phase.Combat CombatStep.DeclareBlockers)
    Spec.assertEqWith s "the trigger fired once: two tokens" (length tokens) 2
    mapM_ (\oid -> Spec.assertEqWith s "tapped" (tapStateOf oid atBlockers) (Just TapState.Tapped)) tokens
    mapM_ (\oid -> Spec.assertEqWith s "attacking bob" (Map.lookup oid attackers) (Just (AttackTarget.OfPlayer S.bob))) tokens
    -- CR 302.6 restricts a creature from ATTACKING, and CR 508.4c exempts a
    -- creature put onto the battlefield attacking from the restrictions that
    -- apply to the declaration of attackers -- so a token that has been
    -- controlled for no time at all is attacking anyway.
    mapM_ (\oid -> Spec.assertEqWith s "still summoning sick" (sicknessOf oid) (Just Sickness.Sick)) tokens
    case mine of
      [garrisonId] -> Spec.assertEqWith s "and the Garrison itself is attacking" (Map.lookup garrisonId attackers) (Just (AttackTarget.OfPlayer S.bob))
      _ -> Spec.assertFailure s "fixture should have one Hanweir Garrison"
  Spec.it s "CR 508.3a the tokens are attacking, and the attack trigger fired only for the Garrison" $ do
    -- THE discriminating case, and the one a naive implementation gets
    -- wrong: CR 508.3a's "such abilities won't trigger if a creature is put
    -- onto the battlefield attacking", and CR 508.4's "such creatures are
    -- 'attacking' but ... they never 'attacked'". An engine that put the
    -- tokens into combat by routing them through the declaration would
    -- record them here, and every "whenever a creature attacks" ability
    -- would then fire for the tokens as well.
    --
    -- Two Garrisons, so the assertion is a LIST and not a singleton: a
    -- declaration really does record one entry per creature, which is what
    -- makes the tokens' absence a fact about the tokens rather than about
    -- the shape of the log.
    garrison <- S.printingOf s registry "Hanweir Garrison"
    let (gs, mine, _) = S.combatBoardOf [garrison, garrison] []
        atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
        tokens = S.tokensOf atBlockers
        attackers = Combat.Type.attackers (GameState.combat atBlockers)
    Spec.assertEqWith s "each Garrison's trigger fired once: four tokens" (length tokens) 4
    Spec.assertEqWith s "all six creatures are attacking" (Map.size attackers) 6
    Spec.assertEqWith s "but only the two Garrisons were DECLARED" (S.attackerDeclarationsOf atBlockers) mine
    mapM_ (\oid -> Spec.assertBool s (notElem oid (S.attackerDeclarationsOf atBlockers)) "no token was declared") tokens
  Spec.it s "CR 510.1b the tokens deal combat damage like any attacker" $ do
    garrison <- S.printingOf s registry "Hanweir Garrison"
    let (gs, _, _) = S.combatBoardOf [garrison] []
        after = S.runCombat S.aggressiveAnswer gs
    -- The 2/3 Garrison plus two 1/1 tokens, all unblocked, against bob's 20.
    Spec.assertEqWith s "bob takes 2 + 1 + 1" (S.lifeOf S.bob after) (Just 16)

-- CR 306.6 / CR 508.1b: attacking a planeswalker, through the one planeswalker in
-- the pool.
--
-- Jace Beleren is the whole board on bob's side: {1}{U}{U} Legendary
-- Planeswalker -- Jace, with printed loyalty 3, which is what makes every
-- assertion here arithmetic rather than a threshold nobody can miss -- a 2/1
-- Goblin Piker takes two of the three (CR 306.8), and two of them take all three
-- and reach CR 704.5i.
--
-- PlaneswalkerSpec covers the card itself, including CR 306.5b's entry
-- replacement; the counters here are placed as a state fixture, because a
-- combat board cannot reach the sorcery-speed cast that would place them.
jaceBoard :: Printing.Printing -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], ObjectId.ObjectId)
jaceBoard jace mine =
  let (gs, ours, theirs) = S.combatBoardOf mine [jace]
   in case theirs of
        [jaceId] -> (S.addCounter CounterKind.Loyalty 3 jaceId gs, ours, jaceId)
        -- Unreachable (combatBoardOf returns one id per printing), and total
        -- rather than an `error`: S.noSource names no object, so a fixture that
        -- somehow got here fails the first assertion instead of the suite.
        _ -> (gs, ours, S.noSource)

isPlaneswalkerTarget :: AttackTarget.AttackTarget -> Bool
isPlaneswalkerTarget target = case target of
  AttackTarget.OfPlaneswalker _ -> True
  AttackTarget.OfPlayer _ -> False
  AttackTarget.OfBattle _ -> False

-- Announce every attack at the first planeswalker offered, and answer everything
-- else aggressively. The counterpart of S.aggressiveAnswer, which takes the head
-- of the same list and so always attacks the defending player: the pair is what
-- makes CR 508.1b's announcement a REAL choice here rather than a prompt whose
-- answer the engine could have supplied itself.
--
-- Falls back to the head when no planeswalker is offered, which keeps it total
-- and makes it the same interpreter as S.aggressiveAnswer on a board without one.
attackThePlaneswalker :: Prompt.Prompt r -> r
attackThePlaneswalker p = case p of
  Prompt.ChooseAttackTarget _ _ _ options -> case filter isPlaneswalkerTarget (NonEmpty.toList options) of
    target : _ -> target
    [] -> NonEmpty.head options
  _ -> S.aggressiveAnswer p

-- Record every CR 508.1b announcement the engine asks for -- the creature and the
-- options it was offered -- and answer it with the planeswalker. The prompt is
-- elided at one candidate, so an empty log is the assertion that nothing was
-- asked.
announcementLog :: Prompt.Prompt r -> State.State [(ObjectId.ObjectId, [AttackTarget.AttackTarget])] r
announcementLog p = case p of
  Prompt.ChooseAttackTarget _ _ oid options -> do
    State.modify' (\seen -> seen <> [(oid, NonEmpty.toList options)])
    pure (attackThePlaneswalker p)
  _ -> pure (attackThePlaneswalker p)

-- Declare attackers under the recording interpreter, keeping the log.
announcementsFor :: GameState.GameState -> [(ObjectId.ObjectId, [AttackTarget.AttackTarget])]
announcementsFor gs = State.execState (Engine.runGame announcementLog gs (Combat.declareAttackers S.alice)) []

planeswalkerAttackSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
planeswalkerAttackSpec s registry = Spec.describe s "AttackingAPlaneswalker" $ do
  Spec.it s "CR 508.1b a creature is declared attacking the planeswalker, not its controller" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, mine, jaceId) = jaceBoard jace [piker]
        atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) attackThePlaneswalker gs
    case mine of
      [attacker] ->
        Spec.assertEqWith
          s
          "the record names the planeswalker (CR 508.1b)"
          (Map.lookup attacker (Combat.Type.attackers (GameState.combat atBlockers)))
          (Just (AttackTarget.OfPlaneswalker jaceId))
      _ -> Spec.assertFailure s "fixture should have one attacker"
  Spec.it s "CR 306.8 whole cards: a 2/1 attacking Jace takes two loyalty counters and bob takes nothing" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, _, jaceId) = jaceBoard jace [piker]
        after = S.runCombat attackThePlaneswalker gs
    Spec.assertEqWith s "CR 306.8: 3 - 2" (S.counterOf CounterKind.Loyalty jaceId after) 1
    Spec.assertEqWith s "CR 510.1b: the damage did not reach its controller" (S.lifeOf S.bob after) (Just 20)
    Spec.assertBool s (Set.member jaceId (GameState.battlefield after)) "CR 704.5i does not apply at loyalty 1"
  -- The pair that makes the announcement a choice: ONE board, two interpreters,
  -- two different games. An engine that answered CR 508.1b for the player could
  -- not produce both lines.
  Spec.it s "CR 508.1b both answers are reachable: the same board, attacked the other way" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, _, jaceId) = jaceBoard jace [piker]
        atJace = S.runCombat attackThePlaneswalker gs
        atBob = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith s "attacking Jace: bob is untouched" (S.lifeOf S.bob atJace) (Just 20)
    Spec.assertEqWith s "attacking Jace: two counters gone" (S.counterOf CounterKind.Loyalty jaceId atJace) 1
    Spec.assertEqWith s "attacking bob: he takes two" (S.lifeOf S.bob atBob) (Just 18)
    Spec.assertEqWith s "attacking bob: Jace keeps all three" (S.counterOf CounterKind.Loyalty jaceId atBob) 3
  Spec.it s "CR 704.5i two attackers take all three loyalty counters and Jace is buried" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, _, jaceId) = jaceBoard jace [piker, piker]
        after = S.runCombat attackThePlaneswalker gs
    Spec.assertEqWith s "loyalty 0 (Natural, not wrapped past zero)" (S.counterOf CounterKind.Loyalty jaceId after) 0
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield after))) "CR 704.5i: off the battlefield"
    Spec.assertEqWith s "CR 704.5i: in its owner's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "and none of the 4 damage splashed onto bob" (S.lifeOf S.bob after) (Just 20)
  -- CR 508.1b's announcement is asked PER CREATURE, and the answers are
  -- independent: two Pikers, one at Jace and one at bob.
  Spec.it s "CR 508.1b the announcement is per creature, and the two may differ" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, mine, jaceId) = jaceBoard jace [piker, piker]
        splitting :: Prompt.Prompt r -> r
        splitting p = case p of
          -- The first Piker (the lower id) is sent at Jace and the second at bob.
          Prompt.ChooseAttackTarget _ _ oid options ->
            if Just oid == Maybe.listToMaybe mine
              then attackThePlaneswalker p
              else NonEmpty.head options
          _ -> S.aggressiveAnswer p
        after = S.runCombat splitting gs
    Spec.assertEqWith s "one Piker's 2 went to Jace" (S.counterOf CounterKind.Loyalty jaceId after) 1
    Spec.assertEqWith s "and the other's 2 went to bob" (S.lifeOf S.bob after) (Just 18)
  Spec.it s "CR 508.1b the prompt is asked once per attacker, over the defending player and their planeswalker" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, mine, jaceId) = jaceBoard jace [piker, piker]
    Spec.assertEqWith
      s
      "two attackers, two announcements, each offering both targets"
      (announcementsFor gs)
      (fmap (\oid -> (oid, [AttackTarget.OfPlayer S.bob, AttackTarget.OfPlaneswalker jaceId])) mine)
  -- The regression guard, and the elision: CR 508.1b calls for no announcement
  -- when the defending player controls no planeswalker, so the engine must not
  -- ask -- and the board must play exactly as it did before the prompt existed.
  Spec.it s "CR 508.1b with no planeswalker the announcement is not asked at all" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [piker, piker] []
    Spec.assertEqWith s "nothing was asked" (announcementsFor gs) []
    Spec.assertEqWith s "and the two Pikers still connect for 4" (S.lifeOf S.bob (S.runCombat attackThePlaneswalker gs)) (Just 16)
  -- CR 506.4 / CR 506.4c / CR 510.1b, at gameplay level and without an
  -- instant: two first strikers kill Jace in the FIRST combat damage step
  -- (CR 510.4), and the Piker attacking the same planeswalker then has nothing
  -- to assign in the second -- "If it isn't currently attacking anything (if,
  -- for example, it was attacking a planeswalker that has left the
  -- battlefield), it assigns no combat damage."
  --
  -- The control is the same board attacked the other way: 2 + 2 + 2 is bob at
  -- 14, so the missing 2 here is the rule and not a board that never dealt it.
  Spec.it s "CR 510.1b whole cards: a planeswalker killed by first strike leaves its attacker assigning nothing" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    tiger <- S.printingOf s registry "Sabretooth Tiger"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, mine, jaceId) = jaceBoard jace [tiger, tiger, piker]
        atFirstStrike = S.runToStep (Phase.Combat CombatStep.CombatDamage) attackThePlaneswalker gs
        atSecond = snd (Engine.runGamePure attackThePlaneswalker atFirstStrike Engine.runStep)
        after = S.runCombat attackThePlaneswalker gs
        control = S.runCombat S.aggressiveAnswer gs
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield atSecond))) "the two 2/1 first strikers buried Jace (CR 704.5i)"
    case reverse mine of
      thePiker : _ -> do
        -- CR 506.4c: the Piker is still an attacking creature, though it is
        -- attacking nothing. Removing it from combat instead is the bug this
        -- pins.
        Spec.assertBool
          s
          (Map.member thePiker (Combat.Type.attackers (GameState.combat atSecond)))
          "CR 506.4c: the Piker remains an attacking creature"
        -- "It assigns no combat damage" is a claim about ASSIGNMENT, so it is
        -- asserted on the CR 608.2i damage log and not only on bob's life total:
        -- the planeswalker's id still names an object in the graveyard, so an
        -- engine that skipped CR 506.4 would deal the Piker's 2 to a permanent
        -- that is not there and leave every life total looking right.
        Spec.assertEqWith
          s
          "the Piker assigned no combat damage (CR 510.1b)"
          (filter (\ev -> DamageEvent.source ev == thePiker) (S.damageEventsOf after))
          []
      _ -> Spec.assertFailure s "fixture should have three attackers"
    Spec.assertEqWith s "so bob is untouched" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "the same board attacked at bob is 2 + 2 + 2" (S.lifeOf S.bob control) (Just 14)

-- Run whole combat steps under a MONADIC interpreter, so an assignment prompt can
-- be recorded as well as answered. S.runCombat's interpreter is pure and cannot
-- report what it was offered, and what CR 702.19c is about is the shape of the
-- offer.
runCombatLogging ::
  (forall r. Prompt.Prompt r -> State.State [Map.Map Recipient.Recipient Natural] r) ->
  GameState.GameState ->
  (GameState.GameState, [Map.Map Recipient.Recipient Natural])
runCombatLogging answer gs0 =
  let go n g =
        if n <= (0 :: Int) || Maybe.isJust (GameState.result g) || not (S.inCombatPhase (GameState.phase g))
          then pure g
          else do
            (_, next) <- Engine.runGame answer g Engine.runStep
            go (n - 1) next
   in State.runState (go 24 gs0) []

-- Record every CR 702.19b/702.19c threshold map the engine offers, and answer it
-- with `answer` -- a fixed division the test picked, which is what makes the
-- assignment a CHOICE the interpreter made rather than one the engine computed.
assignmentLog ::
  Map.Map Recipient.Recipient Natural ->
  Prompt.Prompt r ->
  State.State [Map.Map Recipient.Recipient Natural] r
assignmentLog answer p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds _ -> do
    State.modify' (\seen -> seen <> [thresholds])
    pure answer
  _ -> pure (attackThePlaneswalker p)

-- assignmentLog with one pinned division PER ASSIGNING CREATURE, which is what a
-- board with two of them needs: a division picked by searching the offer for a
-- legal one would find another after the engine's check moved, and the case would
-- stay green while proving nothing. An unlisted creature is answered with the
-- empty division, which never totals its power and so assigns nothing.
pinnedAssignments ::
  (forall a. Prompt.Prompt a -> a) ->
  [(ObjectId.ObjectId, Map.Map Recipient.Recipient Natural)] ->
  Prompt.Prompt r ->
  State.State [Map.Map Recipient.Recipient Natural] r
pinnedAssignments base answers p = case p of
  Prompt.AssignCombatDamage _ _ source thresholds _ -> do
    State.modify' (\seen -> seen <> [thresholds])
    pure (Maybe.fromMaybe Map.empty (List.lookup source answers))
  _ -> pure (base p)

-- CR 702.19c / CR 702.19e / CR 702.19f: trample over planeswalkers, through
-- Thrasta, Tempest's Roar -- the only card that prints it.
--
-- A 7/7 into a 3-loyalty Jace Beleren, so the three numbers the rule turns on --
-- power, loyalty, and the 4 that spills past it -- are all distinct and no two
-- readings of CR 702.19c land on the same board.
--
-- "That planeswalker's controller" and "the defending player" are one seat here.
-- That is not the two-player collapse that hides a bug: CR 702.19c names the
-- planeswalker's controller precisely because the attack was declared against
-- that player's planeswalker (CR 508.1b), so the two are the same player on every
-- board pawl can build.
--
-- Thrasta's cost reduction is implemented and dormant here: nothing is cast on
-- these boards, so CR 601.2f is never reached. Pawl.CostSpec is where it is
-- proved.
--
-- Its hexproof clause is dormant for a different reason:
-- S.combatBoardOf puts Thrasta onto the battlefield without a zone change, so
-- Quantity.EnteredThisTurn reads 0 and the CR 604.2 gate is shut. Pawl.ConditionSpec
-- is where the clause is proved.
trampleOverPlaneswalkersSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
trampleOverPlaneswalkersSpec s registry = Spec.describe s "TrampleOverPlaneswalkers" $ do
  Spec.it s "CR 702.19c an unblocked 7/7 pays Jace's 3 loyalty and sends the other 4 at bob" $ do
    thrasta <- S.printingOf s registry "Thrasta, Tempest's Roar"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, _, jaceId) = jaceBoard jace [thrasta]
        answer = Map.fromList [(Recipient.ToPlaneswalker jaceId, 3), (Recipient.ToPlayer S.bob, 4)]
        (after, offered) = runCombatLogging (assignmentLog answer) gs
    Spec.assertEqWith
      s
      "CR 702.19c: the planeswalker at its LOYALTY, its controller behind it at 0"
      offered
      [Map.fromList [(Recipient.ToPlaneswalker jaceId, 3), (Recipient.ToPlayer S.bob, 0)]]
    Spec.assertEqWith s "bob took the 4 past Jace" (S.lifeOf S.bob after) (Just 16)
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield after))) "CR 704.5i: Jace took all 3 and is buried"
  -- The pair that makes CR 702.19c's "may be assigned as the attacking creature's
  -- controller chooses" a choice: ONE board, two interpreters, two games. An
  -- engine that computed "the excess goes to the player" passes the case above
  -- and fails this one.
  Spec.it s "CR 702.19c the whole 7 may stay on Jace instead" $ do
    thrasta <- S.printingOf s registry "Thrasta, Tempest's Roar"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, _, jaceId) = jaceBoard jace [thrasta]
        answer = Map.singleton (Recipient.ToPlaneswalker jaceId) 7
        (after, offered) = runCombatLogging (assignmentLog answer) gs
    Spec.assertEqWith s "the same offer was made" (fmap Map.keys offered) [[Recipient.ToPlaneswalker jaceId, Recipient.ToPlayer S.bob]]
    Spec.assertEqWith s "bob is untouched" (S.lifeOf S.bob after) (Just 20)
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield after))) "Jace dies either way"
  -- CR 702.19f, the negative control: a plain trampler attacking a planeswalker
  -- can assign the defending player nothing, "even if ... the damage the attacking
  -- creature could assign is greater than the planeswalker's loyalty".
  --
  -- Panglacial Wurm and not War Mammoth, and that is the whole point of the case:
  -- a 3/3 into 3 loyalty is forced whether or not the keyword is there, so it
  -- could not tell the two apart. The Wurm is 9/5 with plain trample, so 6 would
  -- spill past Jace if CR 702.19f were not enforced.
  Spec.it s "CR 702.19f plain trample offers the defending player nothing" $ do
    wurm <- S.printingOf s registry "Panglacial Wurm"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, _, jaceId) = jaceBoard jace [wurm]
        answer = Map.fromList [(Recipient.ToPlaneswalker jaceId, 3), (Recipient.ToPlayer S.bob, 6)]
        (after, offered) = runCombatLogging (assignmentLog answer) gs
    Spec.assertEqWith s "no division was ever asked for, so no map held the player" offered []
    Spec.assertEqWith s "bob is untouched" (S.lifeOf S.bob after) (Just 20)
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield after))) "all 9 went to Jace (CR 704.5i)"
  -- CR 702.19c's LAST sentence: "when checking for assigned damage equal to a
  -- planeswalker's loyalty, take into account damage from other creatures that's
  -- being assigned during the same combat damage step". A 2/1 Goblin Piker
  -- attacking Jace beside Thrasta covers 2 of the 3 loyalty, so Thrasta owes it 1
  -- and 6 reaches bob -- where a threshold read per attacker makes Thrasta owe the
  -- whole 3 and rejects this division outright.
  --
  -- The pair below is ONE difference: whether the Piker is announced attacking
  -- Jace or attacking bob. Same cards, same seats, same pinned division for
  -- Thrasta -- and the same offer, asserted in both, so what moved is the CHECK
  -- and not what Thrasta was asked.
  --
  -- Every number distinct: 7 power over 3 loyalty, split 1 + 6, with the Piker's 2
  -- the only way the loyalty is covered. No two readings of the rule agree here --
  -- per attacker, Thrasta assigns nothing at all.
  Spec.it s "CR 702.19c another attacker's damage pays down the loyalty Thrasta must cover" $ do
    thrasta <- S.printingOf s registry "Thrasta, Tempest's Roar"
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, mine, jaceId) = jaceBoard jace [piker, thrasta]
        thrastaId = case mine of [_, t] -> t; _ -> S.noSource
        pikerId = case mine of [p, _] -> p; _ -> S.noSource
        answer = Map.fromList [(Recipient.ToPlaneswalker jaceId, 1), (Recipient.ToPlayer S.bob, 6)]
        offer = [Map.fromList [(Recipient.ToPlaneswalker jaceId, 3), (Recipient.ToPlayer S.bob, 0)]]
        -- One offer either way: the Piker's own assignment is forced (CR 510.1b,
        -- one recipient), so the only division asked for is Thrasta's.
        --
        -- CR 508.1b: the defending player heads the options (Combat.attackTargets
        -- orders them), so this announces the Piker at bob and Thrasta at Jace.
        pikerAtBob :: Prompt.Prompt a -> a
        pikerAtBob p = case p of
          Prompt.ChooseAttackTarget _ _ oid options | oid == pikerId -> NonEmpty.head options
          _ -> attackThePlaneswalker p
        (shared, sharedOffer) = runCombatLogging (pinnedAssignments attackThePlaneswalker [(thrastaId, answer)]) gs
        (alone, aloneOffer) = runCombatLogging (pinnedAssignments pikerAtBob [(thrastaId, answer)]) gs
    Spec.assertEqWith s "CR 702.19c: Jace is offered at his LOYALTY either way" sharedOffer offer
    Spec.assertEqWith s "and the same offer when the Piker is elsewhere" aloneOffer offer
    Spec.assertEqWith s "the Piker's 2 plus Thrasta's 1 is Jace's whole loyalty, so 6 reaches bob" (S.lifeOf S.bob shared) (Just 14)
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield shared))) "CR 704.5i: Jace took 3 between them"
    -- The Piker at bob instead: nothing else is assigning to Jace, so Thrasta's 1
    -- leaves him short and the division is rejected -- Thrasta assigns nothing and
    -- only the Piker's 2 lands.
    Spec.assertEqWith s "with the Piker at bob, only its own 2 reaches him" (S.lifeOf S.bob alone) (Just 18)
    Spec.assertBool s (Set.member jaceId (GameState.battlefield alone)) "and Jace is untouched"
  -- CR 702.2c is about a CREATURE: "any nonzero amount of combat damage assigned
  -- to a creature by a source with deathtouch". A planeswalker's bar is CR
  -- 702.19c's count of loyalty counters, which deathtouch says nothing about, so
  -- Typhoid Rats' 1 in the Piker's seat pays 1 of Jace's 3 and no more -- leaving
  -- Thrasta's 1 + 6 short, and rejected.
  --
  -- The Rats stand where the 2/1 Piker stood in the case above, so the board is
  -- that one with a smaller, deathtouch attacker: an engine that read CR 702.2c on
  -- every recipient rather than on creatures alone lets the whole 6 through here.
  -- The Piker's 2 covered the loyalty between them and this 1 leaves it one short,
  -- so the two cases land on different boards for the reason the rule gives.
  Spec.it s "CR 702.2c does not clear a planeswalker's loyalty bar" $ do
    thrasta <- S.printingOf s registry "Thrasta, Tempest's Roar"
    rats <- S.printingOf s registry "Typhoid Rats"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, mine, jaceId) = jaceBoard jace [rats, thrasta]
        thrastaId = case mine of [_, t] -> t; _ -> S.noSource
        answer = Map.fromList [(Recipient.ToPlaneswalker jaceId, 1), (Recipient.ToPlayer S.bob, 6)]
        (after, offered) = runCombatLogging (pinnedAssignments attackThePlaneswalker [(thrastaId, answer)]) gs
    Spec.assertEqWith s "Jace is offered at his loyalty, as ever" offered [Map.fromList [(Recipient.ToPlaneswalker jaceId, 3), (Recipient.ToPlayer S.bob, 0)]]
    Spec.assertEqWith s "the Rats' deathtouch 1 counts as 1, so Thrasta's division is rejected" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "CR 306.8: only the Rats' 1 came off Jace" (S.counterOf CounterKind.Loyalty jaceId after) 2
  -- CR 702.19e, the exception to CR 506.4c: two 2/1 first strikers bury Jace in the
  -- FIRST combat damage step (CR 510.4), and Thrasta -- still recorded as attacking
  -- it -- assigns to the defending player in the second. The control is the same
  -- board with War Mammoth in Thrasta's seat, where CR 506.4c stands and the
  -- attacker assigns nothing (the existing CR 510.1b case above is that rule).
  Spec.it s "CR 702.19e whole cards: a planeswalker killed by first strike does not stop the trampler" $ do
    thrasta <- S.printingOf s registry "Thrasta, Tempest's Roar"
    warMammoth <- S.printingOf s registry "War Mammoth"
    tiger <- S.printingOf s registry "Sabretooth Tiger"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, _, jaceId) = jaceBoard jace [tiger, tiger, thrasta]
        (control, _, _) = jaceBoard jace [tiger, tiger, warMammoth]
        after = S.runCombat attackThePlaneswalker gs
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield after))) "the two first strikers buried Jace"
    Spec.assertEqWith s "CR 702.19e: Thrasta's 7 reached bob anyway" (S.lifeOf S.bob after) (Just 13)
    Spec.assertEqWith
      s
      "CR 506.4c / CR 510.1b: a plain trampler in the same seat assigns nothing"
      (S.lifeOf S.bob (S.runCombat attackThePlaneswalker control))
      (Just 20)

-- CR 702.19b's last sentence, the twin of CR 702.19c's above: "when checking for
-- assigned lethal damage, take into account damage already marked on the creature
-- and damage from other creatures that's being assigned during the same combat
-- damage step". The second half needs ONE creature blocking TWO attackers, which
-- is Palace Guard's "can block any number of creatures" (CR 509.1a, through
-- Pawl.Engine.BlockPermission).
--
-- Two cases, for the rule's two consumers: the CHECK on a division (below) and
-- the elision that decides whether a division is asked for at all (after it).
blockingAll :: [ObjectId.ObjectId] -> Prompt.Prompt a -> a
blockingAll attackers p = case p of
  Prompt.DeclareBlockers _ _ blockers _ -> Map.fromList (fmap (\b -> (b, Set.fromList attackers)) blockers)
  _ -> S.aggressiveAnswer p

sharedBlockerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
sharedBlockerSpec s registry = Spec.describe s "SharedBlocker" $ do
  -- Panglacial Wurm (9/5 trample) and Thrasta (7/7 trample) into the 1/4 Guard, so
  -- both are past its bar and both are asked to divide -- a creature whose power
  -- the bar absorbs is forced instead, which is the case after this one. Between
  -- them they owe the Guard 4 once, and the division here pays it 1 + 3: read per
  -- attacker, both are short and BOTH assign nothing, so no two readings of the
  -- rule land on the same board.
  Spec.it s "CR 702.19b two tramplers owe one shared blocker a single lethal bar" $ do
    wurm <- S.printingOf s registry "Panglacial Wurm"
    thrasta <- S.printingOf s registry "Thrasta, Tempest's Roar"
    guard <- S.printingOf s registry "Palace Guard"
    let (gs, mine, theirs) = S.combatBoardOf [wurm, thrasta] [guard]
        (wurmId, thrastaId) = case mine of [w, t] -> (w, t); _ -> (S.noSource, S.noSource)
        guardId = case theirs of [g] -> g; _ -> S.noSource
        -- 1 + 3 onto the Guard is its whole toughness between them, and each
        -- trampler spills the rest.
        answers =
          [ (wurmId, Map.fromList [(Recipient.ToCreature guardId, 1), (Recipient.ToPlayer S.bob, 8)]),
            (thrastaId, Map.fromList [(Recipient.ToCreature guardId, 3), (Recipient.ToPlayer S.bob, 4)]),
            -- CR 510.1d: the Guard divides its own 1 power among the creatures it
            -- blocks. Pinned onto the Wurm so the board says which.
            (guardId, Map.singleton (Recipient.ToCreature wurmId) 1)
          ]
        (both, bothOffered) = runCombatLogging (pinnedAssignments (blockingAll [wurmId, thrastaId]) answers) gs
        (one, oneOffered) = runCombatLogging (pinnedAssignments (blockingAll [wurmId]) answers) gs
    Spec.assertEqWith
      s
      "CR 702.19b: each trampler is offered the Guard's WHOLE bar, and the defending player behind it"
      bothOffered
      [ Map.fromList [(Recipient.ToCreature guardId, 4), (Recipient.ToPlayer S.bob, 0)],
        Map.fromList [(Recipient.ToCreature guardId, 4), (Recipient.ToPlayer S.bob, 0)],
        Map.fromList [(Recipient.ToCreature wurmId, 0), (Recipient.ToCreature thrastaId, 0)]
      ]
    Spec.assertEqWith s "8 + 4 spilled past the Guard" (S.lifeOf S.bob both) (Just 8)
    Spec.assertBool s (not (Set.member guardId (GameState.battlefield both))) "CR 704.5g: the Guard took its 4"
    -- The same board with the Guard declared against the Wurm alone: nothing else
    -- is assigning to it, so the Wurm's 1 leaves it short and that division is
    -- rejected. Thrasta is unblocked and its 7 is forced (CR 510.1b).
    Spec.assertEqWith s "only the Wurm is asked once it is blocked alone" oneOffered [Map.fromList [(Recipient.ToCreature guardId, 4), (Recipient.ToPlayer S.bob, 0)]]
    Spec.assertEqWith s "so bob takes Thrasta's 7 and nothing of the Wurm's" (S.lifeOf S.bob one) (Just 13)
    Spec.assertBool s (Set.member guardId (GameState.battlefield one)) "and the Guard is untouched"
  -- The same rule reaching the PROMPT rather than the check. Rhox Maulers is a 4/4
  -- trampler into a 1/4 Guard: its whole power is the Guard's bar, so on its own
  -- there is nothing to ask and Damage.attackerAssignment forces all 4 onto the
  -- Guard. Beside the Wurm there IS something to ask -- the Wurm can pay part of
  -- that bar -- and the division below spends 1 on the Guard and 3 on bob.
  --
  -- The pair is one difference again: whether the Guard is declared against the
  -- Wurm as well. With the Maulers blocked ALONE nothing else can pay the bar, the
  -- rules leave nothing to ask, and no division is offered at all -- so an engine
  -- that kept the elision unconditionally passes the negative and fails this
  -- positive.
  Spec.it s "CR 702.19b a trampler its blocker's bar absorbs is still asked once another attacker shares that blocker" $ do
    maulers <- S.printingOf s registry "Rhox Maulers"
    wurm <- S.printingOf s registry "Panglacial Wurm"
    guard <- S.printingOf s registry "Palace Guard"
    let (gs, mine, theirs) = S.combatBoardOf [maulers, wurm] [guard]
        (maulersId, wurmId) = case mine of [m, w] -> (m, w); _ -> (S.noSource, S.noSource)
        guardId = case theirs of [g] -> g; _ -> S.noSource
        -- 1 + 4 is past the Guard's bar of 4 on purpose: "at least" (CR 702.19b),
        -- so the two boards below cannot land on the same life total by paying it
        -- exactly.
        answers =
          [ (maulersId, Map.fromList [(Recipient.ToCreature guardId, 1), (Recipient.ToPlayer S.bob, 3)]),
            (wurmId, Map.fromList [(Recipient.ToCreature guardId, 4), (Recipient.ToPlayer S.bob, 5)]),
            (guardId, Map.singleton (Recipient.ToCreature wurmId) 1)
          ]
        (both, bothOffered) = runCombatLogging (pinnedAssignments (blockingAll [maulersId, wurmId]) answers) gs
        (alone, aloneOffered) = runCombatLogging (pinnedAssignments (blockingAll [maulersId]) answers) gs
    Spec.assertEqWith
      s
      "the Maulers are asked to divide, and offered the same bar the Wurm is"
      (fmap Map.keys bothOffered)
      [ [Recipient.ToCreature guardId, Recipient.ToPlayer S.bob],
        [Recipient.ToCreature guardId, Recipient.ToPlayer S.bob],
        [Recipient.ToCreature maulersId, Recipient.ToCreature wurmId]
      ]
    Spec.assertEqWith s "3 of the Maulers' 4 and 5 of the Wurm's 9 spill past the Guard" (S.lifeOf S.bob both) (Just 12)
    -- Blocked alone, the Maulers have nowhere their 4 could go but the Guard, and
    -- the unblocked Wurm has nothing to divide either (CR 510.1b).
    Spec.assertEqWith s "blocked alone, no division is asked for at all" aloneOffered []
    Spec.assertEqWith s "so bob takes the Wurm's whole 9 and none of the Maulers' 4" (S.lifeOf S.bob alone) (Just 11)
  -- CR 702.2c inside CR 702.19b's last sentence: the OTHER creature's damage is
  -- deathtouch damage, so it counts toward the shared Guard's bar as LETHAL and
  -- not as its face value of 1. Typhoid Rats (1/1 deathtouch) and Panglacial Wurm
  -- (9/5 trample) into the 1/4 Guard: the Rats' 1 is all the bar the Wurm has to
  -- wait on, so the Wurm's whole 9 may spill past.
  --
  -- The pair is ONE difference -- whether the first attacker has deathtouch --
  -- with Llanowar Elves as the 1/1 that does not (its mana ability is out of
  -- reach: an attacking creature is tapped). Same seats, same blocks, the same
  -- pinned division for the Wurm, and the same offer asserted on both, so what
  -- moves is the CHECK.
  --
  -- The two readings differ by exactly the Guard's remaining toughness: 9 through
  -- against 6, since without deathtouch the Wurm owes the Guard 4 - 1 = 3 first.
  -- The third board below spends that 3 to show it, so the negative's 20 is not
  -- the only thing separating them and no board is a coincidence of the others.
  Spec.it s "CR 702.2c another creature's deathtouch damage is lethal on the shared blocker" $ do
    rats <- S.printingOf s registry "Typhoid Rats"
    elves <- S.printingOf s registry "Llanowar Elves"
    wurm <- S.printingOf s registry "Panglacial Wurm"
    guard <- S.printingOf s registry "Palace Guard"
    let board first =
          let (gs, mine, theirs) = S.combatBoardOf [first, wurm] [guard]
              (firstId, wurmId) = case mine of [f, w] -> (f, w); _ -> (S.noSource, S.noSource)
              guardId = case theirs of [g] -> g; _ -> S.noSource
           in (gs, firstId, wurmId, guardId)
        (deadly, ratsId, deadlyWurm, deadlyGuard) = board rats
        (plain, elvesId, plainWurm, plainGuard) = board elves
        -- Nothing at all on the Guard from the Wurm: with the bar met by the
        -- Rats' deathtouch there is no floor left to pay, which is the whole
        -- difference between the readings.
        spillItAll wurmId guardId =
          [ (wurmId, Map.fromList [(Recipient.ToCreature guardId, 0), (Recipient.ToPlayer S.bob, 9)]),
            -- CR 510.1d: the Guard's own 1 power, pinned onto the Wurm, which
            -- survives it either way -- so the Guard is the only creature whose
            -- fate the boards can disagree about.
            (guardId, Map.singleton (Recipient.ToCreature wurmId) 1)
          ]
        -- The same board, paying the bar down by the numbers instead: 1 + 3 is the
        -- Guard's whole 4 and 6 is what is left to spill.
        payTheBar =
          [ (plainWurm, Map.fromList [(Recipient.ToCreature plainGuard, 3), (Recipient.ToPlayer S.bob, 6)]),
            (plainGuard, Map.singleton (Recipient.ToCreature plainWurm) 1)
          ]
        -- Both divisions the step asks for, in the order it asks them: the Wurm
        -- over the Guard and bob (CR 702.19b), then the Guard's own 1 over the two
        -- creatures it blocks (CR 510.1d).
        offers firstId wurmId guardId =
          [ Map.fromList [(Recipient.ToCreature guardId, 4), (Recipient.ToPlayer S.bob, 0)],
            Map.fromList [(Recipient.ToCreature firstId, 0), (Recipient.ToCreature wurmId, 0)]
          ]
        (withDeathtouch, deadlyOffered) =
          runCombatLogging (pinnedAssignments (blockingAll [ratsId, deadlyWurm]) (spillItAll deadlyWurm deadlyGuard)) deadly
        (without, plainOffered) =
          runCombatLogging (pinnedAssignments (blockingAll [elvesId, plainWurm]) (spillItAll plainWurm plainGuard)) plain
        (paid, _) = runCombatLogging (pinnedAssignments (blockingAll [elvesId, plainWurm]) payTheBar) plain
    -- The OFFER is unchanged: a threshold is the blocker's own toughness-minus-
    -- marked bar (Damage.blockerThreshold), and CR 702.2c reaches the CHECK, which
    -- is not settled until the whole step is announced.
    Spec.assertEqWith
      s
      "the Wurm is offered the Guard's whole bar of 4"
      deadlyOffered
      (offers ratsId deadlyWurm deadlyGuard)
    Spec.assertEqWith
      s
      "and the same offer without deathtouch"
      plainOffered
      (offers elvesId plainWurm plainGuard)
    Spec.assertEqWith s "CR 702.2c: the Rats' 1 is lethal, so all 9 reach bob" (S.lifeOf S.bob withDeathtouch) (Just 11)
    Spec.assertBool s (not (Set.member deadlyGuard (GameState.battlefield withDeathtouch))) "CR 704.5h: the Guard took deathtouch damage"
    Spec.assertBool s (Set.member ratsId (GameState.battlefield withDeathtouch)) "the Rats took none of the Guard's damage and live"
    -- Without deathtouch that 1 is a plain 1, the Guard is 3 short, and
    -- the Wurm's division is rejected outright -- it assigns nothing at all.
    Spec.assertEqWith s "1 of plain damage leaves the bar unmet, so the Wurm assigns nothing" (S.lifeOf S.bob without) (Just 20)
    Spec.assertBool s (Set.member plainGuard (GameState.battlefield without)) "and the Guard survives on 1 damage"
    -- The same board paying that 3: the most that can reach bob without deathtouch.
    Spec.assertEqWith s "paying the bar by the numbers costs the Wurm exactly 3" (S.lifeOf S.bob paid) (Just 14)
    Spec.assertBool s (not (Set.member plainGuard (GameState.battlefield paid))) "CR 704.5g: 1 + 3 is the Guard's whole toughness"

-- Aim a spell's every target slot at one object, whatever Recipient arm names it.
-- The filter rather than a built Recipient, so the answer is drawn from what the
-- engine offered.
aimedAtObject :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimedAtObject oid p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    fmap (\(_, candidates) -> Set.filter (\r -> Recipient.objectOf r == Just oid) candidates) sets
  _ -> S.identityAnswer p

-- THREE seats and a stolen planeswalker: alice attacks with Bog Wraith, bob is
-- the defending player and controls carol's Jace Beleren through a Confiscate,
-- and each of the two holds one land. With `bolted`, alice burns Jace off the
-- battlefield after the declaration, which is CR 506.4's "leaves the
-- battlefield" -- so the Wraith is attacking nothing and CR 508.5's second
-- sentence is what names its defending player.
--
-- Three seats and Confiscate together are what make the readings of that sentence
-- distinguishable. Jace's OWNER is carol and its CONTROLLER is bob, so the
-- last-known defending player (bob) and the seat any object-reading answer lands on
-- (carol, CR 108.3's owner, since the buried planeswalker leaves nothing to read a
-- controller off) hold different lands. On a board without the Aura the two
-- coincide and only liveness is proved.
stolenJaceLandwalkBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  Bool ->
  String ->
  String ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
stolenJaceLandwalkBoard s registry bolted defendersLand ownersLand = do
  bogWraith <- S.printingOf s registry "Bog Wraith"
  piker <- S.printingOf s registry "Goblin Piker"
  mountain <- S.printingOf s registry "Mountain"
  confiscate <- S.printingOf s registry "Confiscate"
  jace <- S.printingOf s registry "Jace Beleren"
  bolt <- S.printingOf s registry "Lightning Bolt"
  bobs <- S.printingOf s registry defendersLand
  carols <- S.printingOf s registry ownersLand
  let (gs0, ours, yours, hers) = S.threePlayerCombat [bogWraith, mountain] [piker, bobs] [jace, carols]
  case (ours, yours, hers) of
    (wraith : _, blocker : _, jaceId : _) -> do
      let (confiscateId, gs1) = S.addCreature confiscate S.bob gs0
          (boltId, gs2) = S.addHandCard bolt S.alice gs1
          gs3 = S.addCounter CounterKind.Loyalty 3 jaceId (S.attachTo confiscateId (Recipient.ToObject jaceId) gs2)
          board =
            gs3
              { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
                GameState.priority = Just S.alice,
                -- CR 506.2a / CR 507.1's choice, stated rather than run: bob
                -- defends, which is what puts carol's Jace among the attackable
                -- planeswalkers (CR 306.6 reads the CONTROLLER).
                GameState.combat = (GameState.combat gs3) {Combat.Type.defender = Just S.bob}
              }
          declared = snd (Engine.runGamePure attackThePlaneswalker board (Combat.declareAttackers S.alice))
          burned = S.runPure (aimedAtObject jaceId) declared (do S.cast S.alice boltId; Stack.resolveTop)
      pure (if bolted then S.settleSba burned else declared, wraith, blocker, jaceId)
    _ -> Spec.assertFailure s "fixture should have a Wraith, a blocker and a Jace"

-- CR 508.5's second sentence: once a creature is no longer attacking anything,
-- the defending player its abilities refer to is the controller of the
-- planeswalker it WAS attacking before that planeswalker was removed from combat
-- -- last known information. CR 702.19e is what settles that such a creature
-- still HAS a defending player at all: it assigns its damage to one.
--
-- Bog Wraith is "Creature -- Wraith 3/3, Swampwalk" and nothing else, so CR
-- 702.14c is exactly an ability of an attacking creature that refers to a
-- defending player and no other text is in play. Each pair of cases differs in one
-- thing -- which of the two seats holds the Swamp -- and the removed pair differs
-- from the still-attacked pair in one more, whether the Bolt was cast, so no case
-- can pass because of the board rather than the rule.
lastKnownDefendingPlayerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lastKnownDefendingPlayerSpec s registry = Spec.describe s "LastKnownDefendingPlayer" $ do
  let blocks blocker wraith = Combat.legalBlockDeclaration S.bob (Map.singleton blocker (Set.singleton wraith))
  Spec.it s "CR 702.14c the premise: the stolen planeswalker's CONTROLLER is the defending player while it is attacked" $ do
    (gs, wraith, blocker, jaceId) <- stolenJaceLandwalkBoard s registry False "Swamp" "Island"
    Spec.assertBool s (S.onBattlefield jaceId gs) "Jace is still on the battlefield"
    Spec.assertEqWith s "and bob controls him through the Confiscate" (Projection.controllerOf jaceId gs) (Just S.bob)
    Spec.assertEqWith
      s
      "the Wraith really is attacking him"
      (Map.lookup wraith (Combat.Type.attackers (GameState.combat gs)))
      (Just (AttackTarget.OfPlaneswalker jaceId))
    Spec.assertBool s (not (blocks blocker wraith gs)) "bob's Swamp stops the block"
  Spec.it s "CR 508.5 the same block stays illegal once the planeswalker has left combat" $ do
    -- THE CASE. Jace is gone, so the Wraith attacks nothing (CR 506.4c) and its
    -- swampwalk reads the player it was attacking through -- bob, who holds the
    -- Swamp. Reading the planeswalker itself finds no object at all once the CR
    -- 704.5i burial has run, so a live read answers no defending player and calls
    -- this block legal; reading the owner answers carol, whose land is an Island,
    -- and calls it legal too.
    (gs, wraith, blocker, jaceId) <- stolenJaceLandwalkBoard s registry True "Swamp" "Island"
    Spec.assertBool s (not (S.onBattlefield jaceId gs)) "CR 704.5i: the Bolt's 3 took all of Jace's loyalty"
    Spec.assertBool s (Map.member wraith (Combat.Type.attackers (GameState.combat gs))) "CR 506.4c: still an attacking creature"
    Spec.assertBool s (not (blocks blocker wraith gs)) "bob's Swamp still stops the block"
  Spec.it s "CR 508.5 the gone planeswalker's OWNER is not the seat that is read" $ do
    -- THE FALSIFIER, and the same board with the two lands swapped: carol owns
    -- the Jace and holds the Swamp, bob defends and holds the Island. An engine
    -- that reads the buried planeswalker's owner calls this block illegal.
    (gs, wraith, blocker, jaceId) <- stolenJaceLandwalkBoard s registry True "Island" "Swamp"
    Spec.assertBool s (not (S.onBattlefield jaceId gs)) "Jace is gone here too"
    Spec.assertBool s (blocks blocker wraith gs) "no Swamp on bob's side, so the block is legal"
  Spec.it s "CR 508.5 nor is it the seat that is read while the planeswalker is still attacked" $ do
    -- The pair above with Jace ALIVE, which is what makes the two falsifiers a
    -- reading of CR 508.5 rather than of "the planeswalker is gone": carol owns
    -- him and holds the Swamp, bob controls him and holds the Island, and CR
    -- 508.5's first sentence names the CONTROLLER. An engine reading the owner
    -- calls this block illegal, and calls the premise case legal.
    (gs, wraith, blocker, jaceId) <- stolenJaceLandwalkBoard s registry False "Island" "Swamp"
    Spec.assertBool s (S.onBattlefield jaceId gs) "Jace is still on the battlefield"
    Spec.assertBool s (blocks blocker wraith gs) "the owner's Swamp is not bob's, so the block is legal"

-- CR 508.4 / CR 508.3a / CR 508.8, through the one card in the pool that puts a
-- creature onto the battlefield attacking WITHOUT anything having been declared.
--
-- Meandering Towershell {3}{G}{G} -- Creature -- Turtle 5/9: "Islandwalk.
-- Whenever this creature attacks, exile it. Return it to the battlefield under
-- your control tapped and attacking at the beginning of the declare attackers
-- step on your next turn."
--
-- Hanweir Garrison, the group above, cannot reach either of the two rules these
-- cases are about. Its tokens arrive only because the Garrison itself was
-- declared, so CR 508.8's second clause is never in question there; and a token
-- can never fire a GARRISON's own attack trigger, so CR 508.3a's "including its
-- own triggered ability" has no falsifier there either. The Towershell is both:
-- it returns on a turn its controller declares nothing, and the ability that
-- must not fire is its own.
towershellSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
towershellSpec s registry = Spec.describe s "MeanderingTowershell" $ do
  let boardWith theirs = do
        towershell <- S.printingOf s registry "Meandering Towershell"
        island <- S.printingOf s registry "Island"
        pure (towershellBoard towershell island theirs)
      boardOf = boardWith []
      towershellName = CardName.MkCardName $ Text.pack "Meandering Towershell"
  Spec.it s "CR 508.3a whole card: attacking exiles it, so CR 506.4 leaves it dealing no damage" $ do
    (gs, ours) <- boardOf
    let atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
    Spec.assertEqWith s "it was declared as an attacker" (S.attackerDeclarationsOf atBlockers) [ours]
    Spec.assertEqWith s "and its own trigger exiled it" (S.countOnBattlefieldByName towershellName S.alice atBlockers) 0
    Spec.assertEqWith s "it is the one card in exile" (Set.size (GameState.exile atBlockers)) 1
    -- CR 508.8's FIRST clause is historical (CR 508.1k), so the two steps stay
    -- even though the attacker is gone -- the same fact TurnSpec's Ray of
    -- Command case pins, reached here by the card exiling itself.
    Spec.assertEqWith s "the declare blockers step was reached anyway" (GameState.phase atBlockers) (Phase.Combat CombatStep.DeclareBlockers)
    Spec.assertEqWith s "and one delayed ability is waiting" (length (GameState.delayedTriggers atBlockers)) 1
    -- CR 506.4: the exiled Towershell left the battlefield, so it is no longer a
    -- live combat participant and deals no combat damage. The stale entry stays
    -- in the record on purpose (see Pawl.Engine.Projection's filterReads); every
    -- combat-damage read filters it out by zone instead (Damage.onBattlefield).
    let afterDamage = runToTurnStep 1 Phase.PostcombatMain S.aggressiveAnswer gs
    Spec.assertEqWith s "a 5/9 that left combat deals nobody 5" (S.lifeOf S.bob afterDamage) (Just 20)
  -- CR 110.2's "under your control", the clause the card prints and the engine
  -- used to drop. It differs from the owner's control only when the player who
  -- attacked with the Towershell does not own it, which the card's own ruling
  -- calls out: "If you attack with a Meandering Towershell that you don't own,
  -- you'll control it when it returns."
  --
  -- bob OWNS it; alice steals it and attacks. The steal is Expiry.AtCleanup, so
  -- it is long gone by the return turn -- and it never applied to the returning
  -- incarnation anyway, since CR 400.7 mints a fresh id. So alice controlling
  -- what comes back can only be CR 110.2a's entry controller.
  Spec.it s "CR 110.2 a Towershell its attacker does not own returns under the ATTACKER's control" $ do
    towershell <- S.printingOf s registry "Meandering Towershell"
    island <- S.printingOf s registry "Island"
    let (base, _, theirs) = S.combatBoardOf [] [towershell]
        stock pid g = List.foldl' (\h _ -> snd (S.addLibraryCard island pid h)) g [1 :: Int .. 6]
        stocked = stock S.bob (stock S.alice base)
    case theirs of
      [] -> Spec.assertFailure s "fixture should have given bob a Towershell"
      oid : _ -> do
        let stolen = S.giveControl oid S.alice stocked
            atReturn = runToTurnStep 3 (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer stolen
            isTowershell g o = case Game.cardOf o g of
              Nothing -> False
              Just card -> S.nameOf card == towershellName
            towershells g = filter (isTowershell g) (Set.toList (GameState.battlefield g))
        -- The premise: bob owns it and alice is the one attacking with it.
        Spec.assertEqWith s "bob owns it" (fmap Object.owner (Game.lookupObject oid stolen)) (Just S.bob)
        Spec.assertEqWith s "alice controls it as it attacks" (Projection.controllerOf oid stolen) (Just S.alice)
        Spec.assertEqWith s "the return turn is alice's" (GameState.activePlayer atReturn) S.alice
        case towershells atReturn of
          [back] -> do
            -- CR 400.7: a different id from the one that attacked, so nothing
            -- from before the exile carries over on its own.
            Spec.assertBool s (back /= oid) "a fresh incarnation returned"
            Spec.assertEqWith s "still owned by bob" (fmap Object.owner (Game.lookupObject back atReturn)) (Just S.bob)
            Spec.assertEqWith s "but controlled by alice, who attacked with it" (Projection.controllerOf back atReturn) (Just S.alice)
            -- And CR 506.3b's consequence: a permanent put onto the battlefield
            -- attacking must be the ACTIVE player's, so getting the control
            -- wrong would also have left it not attacking at all.
            Spec.assertBool s (Map.member back (Combat.Type.attackers (GameState.combat atReturn))) "and it is attacking"
            -- Entering under someone's control is BASE state (CR 110.2), not a
            -- continuous effect, so there is no duration for a cleanup step to
            -- run out. Read a turn later, which is what separates it from the
            -- AtCleanup the test fixture's own steal uses: were this carried by
            -- any turn-scoped effect the Towershell would revert to bob here,
            -- and every assertion above would still have passed.
            let laterTurn = runToTurnStep 4 Phase.PostcombatMain S.aggressiveAnswer atReturn
            Spec.assertEqWith s "and alice still controls it a turn later" (Projection.controllerOf back laterTurn) (Just S.alice)
          other -> Spec.assertFailure s ("expected one returned Towershell, got " <> show (length other))

  Spec.it s "CR 508.8 whole card: it returns attacking with NOTHING declared, and the two steps stay" $ do
    -- The reason this card was worth adding: the rule's SECOND clause standing
    -- alone, at gameplay level. alice declares no attacker on the return
    -- turn -- she has none to declare -- and the declare blockers step happens
    -- regardless, because a creature was put onto the battlefield attacking.
    --
    -- Reaching the declare blockers step at all IS the assertion: had the
    -- Towershell not joined combat, Combat.skipEmptyCombat would have dropped
    -- that step and the run would have sailed past it.
    (gs, _) <- boardOf
    let atReturn = runToTurnStep 3 (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
        attackers = Combat.Type.attackers (GameState.combat atReturn)
    Spec.assertEqWith s "the return turn is alice's" (GameState.activePlayer atReturn) S.alice
    Spec.assertEqWith s "and the declare blockers step was NOT skipped" (GameState.phase atReturn) (Phase.Combat CombatStep.DeclareBlockers)
    Spec.assertEqWith s "no creature was declared as an attacker" (S.attackerDeclarationsOf atReturn) []
    Spec.assertEqWith s "the Towershell is back on the battlefield" (S.countOnBattlefieldByName towershellName S.alice atReturn) 1
    case Map.toList attackers of
      [(returned, target)] -> do
        Spec.assertEqWith s "attacking bob (CR 508.4)" target (AttackTarget.OfPlayer S.bob)
        Spec.assertEqWith s "and it entered tapped (CR 110.5b)" (tapStateOf returned atReturn) (Just TapState.Tapped)
        Spec.assertBool s (S.onBattlefield returned atReturn) "the attacker is the returned permanent"
      other -> Spec.assertFailure s ("exactly one attacking creature expected, got " <> show (length other))
  Spec.it s "CR 508.3a on the return its OWN attack trigger does not fire" $ do
    -- The discriminating case. Its ruling: "If Meandering Towershell enters the
    -- battlefield attacking, it wasn't declared as an attacking creature that
    -- turn. Abilities that trigger when a creature attacks, INCLUDING ITS OWN
    -- TRIGGERED ABILITY, won't trigger." An engine that routed the return
    -- through the declaration would exile it again on the spot and arm a second
    -- delayed ability, so both halves are asserted.
    (gs, _) <- boardOf
    let atReturn = runToTurnStep 3 (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
    Spec.assertEqWith s "it is still on the battlefield, not exiled again" (S.countOnBattlefieldByName towershellName S.alice atReturn) 1
    Spec.assertEqWith s "the delayed store is empty: nothing armed a second return" (length (GameState.delayedTriggers atReturn)) 0
    Spec.assertEqWith s "and no declaration was recorded for it" (S.attackerDeclarationsOf atReturn) []
  Spec.it s "CR 508.8 the combat damage step is not skipped either: bob takes 5" $ do
    -- The other half of that clause, and the end-to-end statement of it. The
    -- declare blockers step being reached says the schedule kept it; this says
    -- the combat damage step ran and the creature that never attacked dealt its
    -- damage anyway (CR 508.4: such creatures ARE attacking).
    (gs, _) <- boardOf
    let afterCombat = runToTurnStep 3 Phase.PostcombatMain S.aggressiveAnswer gs
    Spec.assertEqWith s "a 5/9 connected" (S.lifeOf S.bob afterCombat) (Just 15)
  -- CR 508.4's CHOICE, which this card is the pool's only producer of: the
  -- Towershell returns attacking on a turn nothing is declared, and its
  -- controller says what it is attacking as it enters. Its own ruling is the
  -- one being obeyed -- "you choose which opponent or opposing planeswalker
  -- it's attacking. It doesn't have to attack the same opponent ... that it was
  -- when it was exiled."
  --
  -- Both answers are asserted on ONE board, which is what makes this a choice
  -- and not a default: aimed at Jace, its 5 damage buries a 3-loyalty
  -- planeswalker (CR 306.8, CR 704.5i) and bob keeps his 20; aimed at bob, he
  -- takes 5 and Jace keeps all three counters.
  Spec.it s "CR 508.4 whole card: the returned Towershell chooses the planeswalker" $ do
    towershell <- S.printingOf s registry "Meandering Towershell"
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    let (base, _) = towershellBoard towershell island [jace]
        jaceId = case filter (\oid -> Projection.isPlaneswalkerOf oid base) (Set.toList (GameState.battlefield base)) of
          oid : _ -> oid
          [] -> S.noSource
        gs = S.addCounter CounterKind.Loyalty 3 jaceId base
        atReturn = runToTurnStep 3 (Phase.Combat CombatStep.DeclareBlockers) attackThePlaneswalker gs
        after = runToTurnStep 3 Phase.PostcombatMain attackThePlaneswalker gs
        control = runToTurnStep 3 Phase.PostcombatMain S.aggressiveAnswer gs
    Spec.assertEqWith
      s
      "it entered attacking the planeswalker (CR 508.4)"
      (Map.elems (Combat.Type.attackers (GameState.combat atReturn)))
      [AttackTarget.OfPlaneswalker jaceId]
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield after))) "a 5/9 buried a 3-loyalty Jace"
    Spec.assertEqWith s "and bob took none of it" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "aimed at bob instead, he takes 5" (S.lifeOf S.bob control) (Just 15)
    Spec.assertEqWith s "and Jace keeps all three counters" (S.counterOf CounterKind.Loyalty jaceId control) 3
  Spec.it s "CR 702.14c whole card: islandwalk keeps the returned Towershell unblockable" $ do
    -- The pool's first ISLANDwalk (Bog Wraith, #500's card, prints swampwalk),
    -- and the only window in which this card's own evasion can be read: on the
    -- turn it is declared it exiles itself before blockers are declared, so the
    -- return turn is where the keyword does its work.
    --
    -- bob controls an Island and a Wall of Stone, and blocks with everything he
    -- can -- so a Towershell without islandwalk would be blocked here and deal
    -- bob nothing.
    --
    -- A WALL and not a Goblin Piker, because bob's own turn falls between the
    -- two combats: CR 702.3b keeps a creature with defender out of the
    -- declaration, so the Wall is still untapped when the Towershell comes back,
    -- where a Piker would have attacked on turn 2 and be tapped (CR 509.1a) --
    -- unable to block for a reason that has nothing to do with evasion.
    wall <- S.printingOf s registry "Wall of Stone"
    island <- S.printingOf s registry "Island"
    (gs, _) <- boardWith [island, wall]
    let afterCombat = runToTurnStep 3 Phase.PostcombatMain S.aggressiveAnswer gs
    Spec.assertEqWith s "the Wall could not block it (CR 702.14c)" (S.lifeOf S.bob afterCombat) (Just 15)

-- alice at her declare attackers step with one Meandering Towershell and bob
-- defending, both players holding a small library so the draw steps of the turns
-- these tests run through do not empty one (CR 104.3c).
--
-- The library cards are Islands, which is deliberate rather than filler: an
-- Island is the only land in the pool the Towershell's own islandwalk (CR
-- 702.14) reads, and a library is not the battlefield, so CR 702.14c's "the
-- defending player controls at least one land with the specified land type"
-- cannot see one there. A case that wants the evasion says so by putting an
-- Island in `theirs`, which is bob's BATTLEFIELD.
towershellBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> (GameState.GameState, ObjectId.ObjectId)
towershellBoard towershell island theirs =
  let (base, ours, _) = S.combatBoardOf [towershell] theirs
      stock pid g = List.foldl' (\h _ -> snd (S.addLibraryCard island pid h)) g [1 :: Int .. 6]
      gs = stock S.bob (stock S.alice base)
   in case ours of
        [oid] -> (gs, oid)
        -- Unreachable (combatBoardOf returns one id per printing), and total
        -- rather than an `error`: S.noSource names no object, so a fixture that
        -- somehow got here fails the first assertion instead of the whole suite.
        _ -> (gs, S.noSource)

-- runToStep's multi-turn twin: run whole steps until the board is at `phase` on
-- turn `turn`, WITHOUT running that step. Bounded so a bug cannot loop forever,
-- and it stops on a finished game so an empty library ends the run rather than
-- spinning.
runToTurnStep :: Natural -> Phase.Phase -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToTurnStep turn phase answer gs0 =
  let go n g =
        if n <= (0 :: Int)
          || Maybe.isJust (GameState.result g)
          || (GameState.turnNumber g == turn && GameState.phase g == phase)
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 64 gs0

-- A declare-attackers board with a real taxing permanent -- Ghostly Prison, or
-- Sphere of Safety -- under `who`'s control and `lands` untapped Forests under
-- alice's. `cursing`'s twin on the cost side of CR 508.1d: alice is active with
-- one creature per printing in `mine`, and the taxing permanent's controller is
-- the only thing that decides whether her attacks are taxed at all.
--
-- The Forests are real Forests, so CR 305.6's intrinsic ability is what pays. A
-- fixture that seeded a mana pool instead would prove nothing about CR 508.1i's
-- window -- the whole of what that rule gives the player is the chance to make
-- the mana -- and would not survive the step boundary in the gameplay-level case
-- (CR 500.5). Their ids come back so a test can read the payment off the board.
imprisoning :: Printing.Printing -> Printing.Printing -> PlayerId.PlayerId -> [Printing.Printing] -> Int -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
imprisoning prison forest who mine lands =
  let (gs, ours, _) = S.combatBoardOf mine []
      (forests, board) = addForests forest lands (snd (S.addCreature prison who gs))
   in (board, ours, forests)

-- `n` untapped Forests under alice's control, ids first. Not S.landsInPlay, which
-- builds a whole fresh game: these go onto a board that already exists.
addForests :: Printing.Printing -> Int -> GameState.GameState -> ([ObjectId.ObjectId], GameState.GameState)
addForests forest n gs =
  let add (ids, g) _ = let (oid, g1) = S.addCreature forest S.alice g in (ids <> [oid], g1)
   in List.foldl' add ([], gs) [1 .. n]

-- Are all of these permanents tapped? What a test asks of the Forests to see CR
-- 508.1j's payment: it spends exactly what tapping them produced, so the pool is
-- empty again afterwards and the tapped lands are the payment's only trace.
allTapped :: [ObjectId.ObjectId] -> GameState.GameState -> Bool
allTapped oids gs = all (\oid -> tapStateOf oid gs == Just TapState.Tapped) oids

-- The complement, and NOT `not . allTapped`: a payment that tapped one Forest of
-- two would satisfy that, and what these cases assert is that nothing was spent.
allUntapped :: [ObjectId.ObjectId] -> GameState.GameState -> Bool
allUntapped oids gs = all (\oid -> tapStateOf oid gs == Just TapState.Untapped) oids

-- CR 508.1d's cost clause and CR 508.1h-508.1j, proved by Ghostly Prison
-- ("Creatures can't attack you unless their controller pays {2} for each creature
-- they control that's attacking you") -- the pool's first cost to attack, and the
-- first board on which a legal declaration can leave the active player unable to
-- comply with CR 508.1 -- and by Sphere of Safety, which is the same sentence
-- widened to the planeswalkers its controller controls and with a {X} that counts
-- the board where the Prison has a constant.
--
-- Every case here is arithmetic rather than a threshold: a Forest makes one mana,
-- so "how many Forests were tapped" reads the total cost off the board directly.
attackCostSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
attackCostSpec s registry = Spec.describe s "AttackCosts" $ do
  Spec.it s "CR 508.1h/508.1j attacking under a Ghostly Prison costs {2}, and the mana is paid" $ do
    prison <- S.printingOf s registry "Ghostly Prison"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, forests) = imprisoning prison forest S.bob [piker] 2
        after = S.runPure S.aggressiveAnswer gs (Combat.declareAttackers S.alice)
    Spec.assertEqWith s "the Piker really was declared" (S.attackerDeclarationsOf after) mine
    Spec.assertBool s (allTapped forests after) "CR 508.1j: both Forests paid for it"
  Spec.it s "CR 508.1 the same board WITHOUT the Prison pays nothing" $ do
    -- The control for the test above, and the reason it is not vacuous: attacking
    -- is free by default (CR 508.1f: "tapping a creature when it's declared as an
    -- attacker isn't a cost"), so the Prison is what tapped the Forests.
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker] []
        (forests, board) = addForests forest 2 gs
        after = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.alice)
    Spec.assertEqWith s "the Piker still attacks" (S.attackerDeclarationsOf after) mine
    Spec.assertBool s (allUntapped forests after) "and no Forest was tapped"
  Spec.it s "CR 508.1h the total scales with the declaration: two attackers owe {4}" $ do
    -- CR 508.1h totals the WHOLE declaration, which is what makes Ghostly Prison's
    -- "for each creature they control that's attacking you" a multiplication.
    -- Ghostly Prison's own Two-Headed Giant ruling states the same arithmetic from
    -- the other end: "you still only have to pay once per creature."
    prison <- S.printingOf s registry "Ghostly Prison"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, forests) = imprisoning prison forest S.bob [piker, piker] 4
        after = S.runPure S.aggressiveAnswer gs (Combat.declareAttackers S.alice)
    Spec.assertEqWith s "both Pikers were declared" (S.attackerDeclarationsOf after) mine
    Spec.assertBool s (allTapped forests after) "CR 508.1h: all four Forests went"
  Spec.it s "CR 508.1j partial payments are not allowed: three Forests do not buy two attacks" $ do
    -- The same board one Forest short. CR 508.1's preamble -- "the declaration is
    -- illegal; the game returns to the moment before the declaration" -- so it is
    -- not that one Piker attacks and the other does not: NEITHER does, and the
    -- three Forests that could have paid for one are untapped again.
    prison <- S.printingOf s registry "Ghostly Prison"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, forests) = imprisoning prison forest S.bob [piker, piker] 3
        after = S.runPure S.aggressiveAnswer gs (Combat.declareAttackers S.alice)
    Spec.assertEqWith s "nothing was declared" (S.attackerDeclarationsOf after) []
    Spec.assertEqWith s "and nothing is attacking" (Combat.Type.attackers (GameState.combat after)) Map.empty
    Spec.assertBool s (allUntapped forests after) "the Forests are untapped again"
    Spec.assertBool s (allUntapped mine after) "CR 508.1f's tapping was undone too"
  Spec.it s "CR 109.5 a Ghostly Prison its own controller is attacking WITH taxes nothing" $ do
    -- The direction, which is the whole of the "you": alice controls the Prison
    -- and attacks bob, so nothing is attacking alice and no cost is owed. An
    -- engine that taxed every attack while any Prison was on the battlefield
    -- would tap her Forests here.
    prison <- S.printingOf s registry "Ghostly Prison"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, forests) = imprisoning prison forest S.alice [piker] 2
        after = S.runPure S.aggressiveAnswer gs (Combat.declareAttackers S.alice)
    Spec.assertEqWith s "the Piker attacks" (S.attackerDeclarationsOf after) mine
    Spec.assertBool s (allUntapped forests after) "and paid nothing"
  Spec.it s "Ghostly Prison's ruling: a creature that can't attack you can still attack a planeswalker you control" $ do
    -- "Unless some effect explicitly says otherwise, a creature that can't attack
    -- you can still attack a planeswalker you control" (Ghostly Prison, 2014-02-01).
    --
    -- ONE board, two interpreters: attacking Jace is free and attacking bob costs
    -- {2}. An engine that read the DEFENDING PLAYER rather than what each creature
    -- was announced as attacking could not produce both lines.
    prison <- S.printingOf s registry "Ghostly Prison"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, mine, jaceId) = jaceBoard jace [piker]
        withPrison = snd (S.addCreature prison S.bob gs)
        (forests, board) = addForests forest 2 withPrison
        atJace = S.runPure attackThePlaneswalker board (Combat.declareAttackers S.alice)
        atBob = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.alice)
    Spec.assertEqWith
      s
      "attacking Jace: the record names the planeswalker"
      (Map.elems (Combat.Type.attackers (GameState.combat atJace)))
      [AttackTarget.OfPlaneswalker jaceId]
    Spec.assertBool s (allUntapped forests atJace) "attacking Jace: nothing was paid"
    Spec.assertEqWith s "attacking bob: the Piker was declared" (S.attackerDeclarationsOf atBob) mine
    Spec.assertBool s (allTapped forests atBob) "attacking bob: the {2} was paid"
  Spec.it s "CR 306.6 Sphere of Safety taxes the attack on a planeswalker that Ghostly Prison lets through" $ do
    -- The contrast with the case directly above, on the same board shape: Sphere
    -- of Safety prints "you OR PLANESWALKERS YOU CONTROL", which is the "unless
    -- some effect explicitly says otherwise" that Ghostly Prison's own ruling
    -- leaves room for. bob controls one enchantment (the Sphere), so X = 1 and
    -- attacking Jace costs {1}.
    --
    -- The discriminating half is the POSITIVE one -- the Forest went -- because a
    -- creature refused an attack for an unrelated reason looks exactly like one
    -- refused by this gate. The record naming the planeswalker is asserted
    -- alongside it so that a Forest tapped for an attack on bob cannot pass.
    sphere <- S.printingOf s registry "Sphere of Safety"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, _, jaceId) = jaceBoard jace [piker]
        withSphere = snd (S.addCreature sphere S.bob gs)
        (forests, board) = addForests forest 1 withSphere
        atJace = S.runPure attackThePlaneswalker board (Combat.declareAttackers S.alice)
    Spec.assertEqWith
      s
      "the record names the planeswalker"
      (Map.elems (Combat.Type.attackers (GameState.combat atJace)))
      [AttackTarget.OfPlaneswalker jaceId]
    Spec.assertBool s (allTapped forests atJace) "and the {X} was paid for it"
  Spec.it s "CR 508.1h Sphere of Safety's share is the enchantment count, not a constant" $ do
    -- ONE card, two boards differing by exactly one inert enchantment. Megrim is
    -- a bare {2}{B} Enchantment whose only ability triggers on a discard, so it
    -- changes the count and nothing else about the combat.
    --
    -- No constant can produce both lines: with the Sphere alone X = 1 and one
    -- Forest is the whole toll, and with Megrim beside it X = 2 and the same
    -- single Piker owes two. An engine that had kept a literal share would fail
    -- one line or the other whatever literal it picked.
    sphere <- S.printingOf s registry "Sphere of Safety"
    megrim <- S.printingOf s registry "Megrim"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (one, mine1, f1) = imprisoning sphere forest S.bob [piker] 1
        after1 = S.runPure S.aggressiveAnswer one (Combat.declareAttackers S.alice)
        (two0, mine2, f2) = imprisoning sphere forest S.bob [piker] 2
        two = snd (S.addCreature megrim S.bob two0)
        after2 = S.runPure S.aggressiveAnswer two (Combat.declareAttackers S.alice)
    Spec.assertEqWith s "X = 1: the Piker was declared" (S.attackerDeclarationsOf after1) mine1
    Spec.assertBool s (allTapped f1 after1) "X = 1: the one Forest paid"
    Spec.assertEqWith s "X = 2: the same Piker was declared" (S.attackerDeclarationsOf after2) mine2
    Spec.assertBool s (allTapped f2 after2) "X = 2: both Forests paid"
  Spec.it s "CR 508.1d a Curse of the Nightly Hunt does not force an attack a Ghostly Prison taxes" $ do
    -- THE COST CLAUSE: "if a creature can't attack unless a player pays a cost,
    -- that player is not required to pay that cost, even if attacking with that
    -- creature would increase the number of requirements being obeyed."
    --
    -- Both worlds on one line each. Without the Prison the Curse makes declining
    -- illegal (that is AttackRequirements' first case); with it, declining is
    -- legal again, and the requirement has not gone anywhere -- attacking with the
    -- Piker is still a legal declaration, it is just no longer a forced one.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    prison <- S.printingOf s registry "Ghostly Prison"
    piker <- S.printingOf s registry "Goblin Piker"
    let (cursed, mine, _) = cursing curse S.alice [piker] []
        taxed = snd (S.addCreature prison S.bob cursed)
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] cursed)) "without the Prison, declining is illegal"
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] taxed) "CR 508.1d: with the Prison, declining is legal"
    case mine of
      a : _ -> Spec.assertBool s (Combat.legalAttackDeclaration S.alice [a] taxed) "and attacking anyway is still legal"
      _ -> Spec.assertFailure s "fixture should have a creature"
  Spec.it s "CR 508.1d the cost clause excuses a requirement only when EVERY attack costs" $ do
    -- ANY free attack, not ALL attacks free. A creature that could attack Jace for
    -- nothing is not one that "can't attack unless a player pays a cost", so the
    -- Curse still forces it onto the battlefield's other side -- and it is the
    -- player's own CR 508.1b announcement, never the engine's, that then decides
    -- whether they end up paying Ghostly Prison.
    --
    -- The two boards differ ONLY by the planeswalker, which is what makes this the
    -- test for `attacksFreely`'s quantifier: with no planeswalker every target is
    -- taxed and the answers coincide.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    prison <- S.printingOf s registry "Ghostly Prison"
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (withJace, _, _) = jaceBoard jace [piker]
        (plain, _, _) = S.combatBoardOf [piker] []
        taxedWithJace = snd (S.addCreature prison S.bob (cursingBoard curse S.alice withJace))
        taxedPlain = snd (S.addCreature prison S.bob (cursingBoard curse S.alice plain))
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] taxedWithJace)) "the free attack on Jace keeps the requirement"
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] taxedPlain) "with no planeswalker every attack costs, so declining is legal"
  Spec.it s "CR 508.1d whole cards: the Curse forces the attack, and the Prison unforces it" $ do
    -- The gameplay-level case, run through Engine.runStep -- the priority loop and
    -- the CR 703.4i turn-based action, not a direct call -- with an interpreter
    -- that declines to attack. It has the mana to pay twice over, so the Forests
    -- are the discriminator rather than the affordability: WITH the Prison the
    -- engine must not reach for them.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    prison <- S.printingOf s registry "Ghostly Prison"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (cursed, _, _) = cursing curse S.alice [piker] []
        (forests, forced) = addForests forest 2 cursed
        taxed = snd (S.addCreature prison S.bob forced)
        declining :: Prompt.Prompt r -> r
        declining p = case p of
          Prompt.DeclareAttackers {} -> []
          _ -> S.aggressiveAnswer p
        forcedRun = S.runCombat declining forced
        taxedRun = S.runCombat declining taxed
    Spec.assertEqWith s "without the Prison the Curse forces the attack and bob takes two" (S.lifeOf S.bob forcedRun) (Just 18)
    Spec.assertBool s (allUntapped forests forcedRun) "and it cost nothing"
    Spec.assertEqWith s "CR 508.1d: with the Prison, declining stands and bob takes nothing" (S.lifeOf S.bob taxedRun) (Just 20)
    Spec.assertEqWith s "nothing attacked" (S.attackerDeclarationsOf taxedRun) []
    Spec.assertBool s (allUntapped forests taxedRun) "and no mana was spent"

-- CR 305.7 as the FIVE readers in this module read it, which is one shared gate:
-- Pawl.Engine.Projection.liveAfterLayers. Ashaya, Soul of the Wild makes its
-- controller's nontoken creatures Forest LANDS at layer 4, and Blood Moon then
-- depends on that (CR 613.8a) and SETS every nonbasic land's subtype to Mountain,
-- which by CR 305.7 takes the animated permanent's rules text -- its combat
-- sentence included.
--
-- What each case here discriminates is the gate's READING, not the strip: the gate
-- has to judge Blood Moon's "nonbasic land" against the FINISHED projection, which
-- CR 613.10 and CR 613.11 permit because every reader in this module runs after
-- the layers. Judged against BASE characteristics the animated creature is no land
-- at all and keeps its sentence, and that is the only difference the boards below
-- turn on.
--
-- FOUR boards a case, differing in nothing but which of the two permanents is
-- present, and each of the three controls is load-bearing. Ashaya alone ADDS a
-- land type, and CR 305.7's last sentence keeps the rules text of a land that
-- gains types in addition to its own; Blood Moon alone names no creature. Only
-- the conjunction strips.
--
-- ONE CASE PER READER, named in the case's own comment, because each reader keeps
-- its own copy of the `null setEffs || ...` guard: one case for the gate would
-- leave a change to any single copy regressing silently.
-- Pawl.Engine.PlayerEffect's share is pinned in Pawl.PlayerEffectSpec and
-- Pawl.Engine.SacrificeRestriction's in Pawl.SacrificeRestrictionSpec.
--
-- A BOARD THAT CANNOT DISCRIMINATE, recorded because it looks like the obvious
-- one: Glacial Crasher ("this creature can't attack unless you control a
-- Mountain") is no witness for the restriction reader, since Blood Moon makes
-- every nonbasic land a Mountain and the gate is satisfied whether or not the
-- Crasher's own sentence survived.
landSubtypeStripSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
landSubtypeStripSpec s registry = Spec.describe s "LandSubtypeStrip" $ do
  Spec.it s "CR 305.7 an animated Palace Guard set to Mountain blocks only one attacker" $ do
    -- Pawl.Engine.BlockPermission.additionalBlocks. Palace Guard says nothing but
    -- "this creature can block any number of creatures", so THREE attackers are
    -- what separate its unbounded arity from the one CR 509.1a gives every
    -- creature. Ashaya goes under BOB, whose blocker it has to animate; Blood Moon
    -- goes there too and its controller never matters, since its sentence names
    -- lands globally rather than "lands you control".
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    palaceGuard <- S.printingOf s registry "Palace Guard"
    piker <- S.printingOf s registry "Goblin Piker"
    let (base, mine, theirs) = attacking [piker, piker, piker] [palaceGuard]
        with extras = withPermanents S.bob extras base
    case (mine, theirs) of
      ([first, second, third], [guard]) -> do
        let three = Combat.legalBlockDeclaration S.bob (Map.singleton guard (Set.fromList [first, second, third]))
            stripped = with [ashaya, bloodMoon]
        Spec.assertEqWith
          s
          "all three until Ashaya and Blood Moon are both on the battlefield, and then not three"
          (three base, three (with [ashaya]), three (with [bloodMoon]), three stripped)
          (True, True, True, False)
        -- The anti-vacuity leg: the Guard lost its arity, not its ability to
        -- block. Without this, a strip that made it no legal blocker at all --
        -- or a fixture that stopped offering it -- would pass the line above.
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton guard (Set.singleton first)) stripped) "and one attacker is still legal"
      _ -> Spec.assertFailure s "fixture should have three attackers and a Palace Guard"
  Spec.it s "CR 305.7 an animated Prized Unicorn set to Mountain no longer forces a block" $ do
    -- Pawl.Engine.BlockRequirement.instances. blockRequirementSpec's Humility case
    -- with CR 613.1f's layer-6 removal swapped for CR 305.7's layer-4 route:
    -- Humility reaches the Unicorn's own ability directly, where this reaches it
    -- only through the animation, which is exactly the difference between reading
    -- base characteristics and reading the projection.
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    prizedUnicorn <- S.printingOf s registry "Prized Unicorn"
    piker <- S.printingOf s registry "Goblin Piker"
    let (base, mine, theirs) = attacking [prizedUnicorn] [piker]
        with extras = withPermanents S.alice extras base
    case (mine, theirs) of
      ([unicorn], [blocker]) -> do
        let declining = Combat.legalBlockDeclaration S.bob Map.empty
            stripped = with [ashaya, bloodMoon]
        Spec.assertEqWith
          s
          "declining stays illegal until Ashaya and Blood Moon are both on the battlefield"
          (declining base, declining (with [ashaya]), declining (with [bloodMoon]), declining stripped)
          (False, False, False, True)
        -- The same anti-vacuity leg blockRequirementSpec's Humility case carries:
        -- the combat is still live under the strip, so declining became legal
        -- because the requirement went away rather than because there was nothing
        -- to block.
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker (Set.singleton unicorn)) stripped) "and blocking the Unicorn is still legal"
      _ -> Spec.assertFailure s "fixture should have a Unicorn and a blocker"
  Spec.it s "CR 305.7 an animated Bonded Construct set to Mountain may attack alone" $ do
    -- Pawl.Engine.CombatRestriction.inForce, whose `keepsAbilities` holds the gate
    -- for the rows `restricted` then selects from. The Construct is an ARTIFACT
    -- creature and Ashaya animates creatures, so the animation reaches it; a Silent
    -- Arbiter would do as well for the strip and worse for the reading, its
    -- sentence naming no creature to be judged.
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    let (base, mine, _) = S.combatBoardOf [bondedConstruct] []
        with extras = withPermanents S.alice extras base
    case mine of
      [construct] -> do
        let alone = Combat.legalAttackDeclaration S.alice [construct]
            stripped = with [ashaya, bloodMoon]
        Spec.assertEqWith
          s
          "attacking alone stays illegal until Ashaya and Blood Moon are both on the battlefield"
          (alone base, alone (with [ashaya]), alone (with [bloodMoon]), alone stripped)
          (False, False, False, True)
        -- Anchors, so a failure above says which half moved. Both readings of the
        -- gate agree about these -- they are the layer fold's answer, not the
        -- gate's -- so they cannot make the line above pass.
        Spec.assertBool s (Set.member Subtype.Mountain (Projection.subtypesOf construct stripped)) "the Construct is a Mountain"
        Spec.assertBool s (Projection.isCreatureOf construct stripped) "and still a creature (CR 305.7: setting a subtype removes no card type)"
      _ -> Spec.assertFailure s "fixture should have one Construct"
  Spec.it s "CR 305.7 animated Berserkers of Blood Ridge set to Mountain need not attack" $ do
    -- Pawl.Engine.AttackRequirement.instances. Berserkers of Blood Ridge {4}{R}
    -- 4/4 says nothing but "this creature attacks each combat if able", and it is
    -- the pool's only attacking requirement printed on a CREATURE: Curse of the
    -- Nightly Hunt is an Aura, and Ashaya animates nontoken creatures, so the
    -- Curse can never reach this gate.
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
    let (base, mine, _) = S.combatBoardOf [berserkers] []
        with extras = withPermanents S.alice extras base
    case mine of
      [required] -> do
        let declining = Combat.legalAttackDeclaration S.alice []
            stripped = with [ashaya, bloodMoon]
        Spec.assertEqWith
          s
          "declining stays illegal until Ashaya and Blood Moon are both on the battlefield"
          (declining base, declining (with [ashaya]), declining (with [bloodMoon]), declining stripped)
          (False, False, False, True)
        -- The anti-vacuity leg: attacking is still legal under the strip, so
        -- declining became legal because the requirement went away rather than
        -- because a Mountain creature-land may no longer attack.
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [required] stripped) "and attacking with them is still legal"
      _ -> Spec.assertFailure s "fixture should have one Berserkers"
  Spec.it s "CR 305.7 whole cards: the strip unforces the Berserkers' attack in a real declare attackers step" $ do
    -- The gameplay-level case for the same reader, through Engine.runStep -- the
    -- priority loop and CR 703.4i's turn-based action -- with an interpreter that
    -- declines to attack. Both worlds are read off bob's life: unstripped the
    -- rules force the 4/4 through, stripped the declination stands.
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
    let (base, mine, _) = S.combatBoardOf [berserkers] []
        stripped = withPermanents S.alice [ashaya, bloodMoon] base
        declining :: Prompt.Prompt r -> r
        declining p = case p of
          Prompt.DeclareAttackers {} -> []
          _ -> S.aggressiveAnswer p
        forced = S.runCombat declining base
        after = S.runCombat declining stripped
    Spec.assertEqWith s "unstripped, the requirement forces the attack and bob takes four" (S.lifeOf S.bob forced) (Just 16)
    Spec.assertEqWith s "and they really were declared" (S.attackerDeclarationsOf forced) mine
    Spec.assertEqWith s "stripped, bob takes nothing" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "and nothing was declared" (S.attackerDeclarationsOf after) []
  Spec.it s "CR 305.7 an animated Windborn Muse set to Mountain taxes nothing" $ do
    -- Pawl.Engine.AttackCost.costsOn. Windborn Muse {3}{W} 2/3 prints Ghostly
    -- Prison's sentence on a CREATURE, which is what lets Ashaya animate it -- the
    -- Prison itself is an enchantment and can never reach this gate. Both go under
    -- BOB, since by CR 109.5 the cost's "you" is the Muse's own controller and only
    -- an attack on that player is taxed. alice's Forests are BASIC, so Blood Moon
    -- leaves the payment's source alone and the tax is the only thing that moves.
    --
    -- The tax is read off the board rather than asserted as a number: a Forest
    -- makes one mana and the tax is {2}, so "were the Forests tapped" is CR
    -- 508.1j's payment exactly.
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    windbornMuse <- S.printingOf s registry "Windborn Muse"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (board, mine, _) = S.combatBoardOf [piker] []
        (forests, base) = addForests forest 2 (snd (S.addCreature windbornMuse S.bob board))
        with extras = withPermanents S.bob extras base
        declared b = S.runPure S.aggressiveAnswer b (Combat.declareAttackers S.alice)
        paid b = allTapped forests (declared b)
        stripped = with [ashaya, bloodMoon]
    Spec.assertEqWith
      s
      "the {2} is paid until Ashaya and Blood Moon are both on the battlefield"
      (paid base, paid (with [ashaya]), paid (with [bloodMoon]), paid stripped)
      (True, True, True, False)
    -- Two anti-vacuity legs, and the first is the one that matters: nothing was
    -- paid because nothing was owed, not because the declaration was refused for
    -- want of mana (CR 508.1's preamble undoes an unpayable declaration whole).
    Spec.assertEqWith s "the Piker attacked anyway" (S.attackerDeclarationsOf (declared stripped)) mine
    Spec.assertBool s (allUntapped forests (declared stripped)) "and not one Forest went"

-- alice attacks with one creature per printing in `mine`; bob defends with a
-- Goblin Piker, holds Curtain of Light and the two Plains that pay its {1}{W},
-- and has one card left in his library so the spell's draw is not a CR 104.3c
-- loss. Returns the attackers, the blocker and the spell.
curtainBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  [Printing.Printing] ->
  (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId], ObjectId.ObjectId)
curtainBoard plains piker curtain mine =
  let (gs0, ours, yours) = S.combatBoardOf mine [piker]
      paid = snd (S.addCreature plains S.bob (snd (S.addCreature plains S.bob gs0)))
      (curtainId, withCard) = S.addHandCard curtain S.bob paid
      stocked = snd (S.addLibraryCard plains S.bob withCard)
   in (stocked, ours, yours, curtainId)

-- Decline every block, cast whatever is castable, and aim every target at
-- `victim`. The cast is bob's: Curtain of Light is the only card in his hand.
--
-- Blocks are DECLINED so that the only route into CR 509.1h's status is the
-- spell -- an aggressive block would confer it by declaration and hide the case.
castCurtain :: ObjectId.ObjectId -> Prompt.Prompt r -> r
castCurtain victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victim))) sets
  Prompt.ChooseAction {} -> S.castAnswer p
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

-- castCurtain's paired control: the same declined blocks, and no cast. The ONE
-- difference between the two answerers is whether the spell is cast.
declineBlocks :: Prompt.Prompt r -> r
declineBlocks p = case p of
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

-- CR 509.1i's blocker-side event, which CR 509.3d's "becomes blocked by a
-- creature" reads: recorded by the declaration and by nothing else.
blockerWasDeclared :: GameEvent.GameEvent -> Bool
blockerWasDeclared e = case e of
  GameEvent.BlockerDeclared _ -> True
  _ -> False

-- CR 509.1h's escape clause: "an effect says that it becomes blocked". Curtain
-- of Light is the pool's producer -- {1}{W} INSTANT, "Cast this spell only
-- during combat after blockers are declared. Target unblocked attacking creature
-- becomes blocked. Draw a card."
--
-- Its window is CastingRestriction.AfterBlockersDeclared, read through
-- Combat.afterBlockersDeclared; castingWindowSpec below is where CR 506.7b's
-- boundary is proved step by step.
--
-- Sacred Prey ("Whenever this creature becomes blocked, you gain 1 life") is the
-- observer for CR 509.3c, which says a "becomes blocked" ability triggers on the
-- effect exactly as it does on the declaration. Its 1 life and the Prey's 1
-- power are the two numbers every leg is read off, and they move different
-- players' totals.
--
-- THREE readings of the board are told apart, because two of them agree about
-- bob's life total: became blocked by the effect (blocked, nothing blocking it,
-- both creatures alive), blocked by the declaration (blocked, the Piker in the
-- set, both creatures dead), and never blocked (unblocked, bob down 1).
becomesBlockedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
becomesBlockedSpec s registry = Spec.describe s "BecomesBlocked" $ do
  Spec.it s "CR 509.1h whole card: Curtain of Light blocks an unblocked attacker, with nothing blocking it" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    prey <- S.printingOf s registry "Sacred Prey"
    curtain <- S.printingOf s registry "Curtain of Light"
    case curtainBoard plains piker curtain [prey] of
      (gs, [attacker], [blocker], _) -> do
        let atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
            cast = runToEndOfCombat (castCurtain attacker) atBlockers
            -- The control: the same board, the same declined blocks, nothing
            -- cast. The Prey is unblocked and bob takes its 1.
            uncast = runToEndOfCombat declineBlocks atBlockers
            -- The other reading: blocked by the DECLARATION rather than by the
            -- effect. Same board again, and the only change is that bob blocks.
            declared = runToEndOfCombat S.aggressiveAnswer atBlockers
        Spec.assertEqWith s "the leg hands over at the declare blockers step, so the spell is cast after the declaration" (GameState.phase atBlockers) (Phase.Combat CombatStep.DeclareBlockers)
        -- The discriminating assertions: blocked, and blocked by NOTHING.
        Spec.assertBool s (Combat.isBlocked attacker cast) "CR 509.1h: the effect made it a blocked creature"
        Spec.assertEqWith s "and no creature is blocking it" (Combat.blockersOf attacker cast) Set.empty
        Spec.assertEqWith s "CR 510.1c: so it assigns no combat damage and bob takes nothing" (S.lifeOf S.bob cast) (Just 20)
        Spec.assertEqWith s "CR 509.3c: the Prey's becomes-blocked trigger fired" (S.lifeOf S.alice cast) (Just 21)
        Spec.assertBool s (S.onBattlefield attacker cast && S.onBattlefield blocker cast) "nothing was dealt damage either way"
        Spec.assertEqWith s "and bob drew the card the spell says to draw" (length (Game.zoneMembers Zone.Library S.bob cast)) 0
        -- CR 509.3d: "it won't trigger if the creature becomes blocked by an
        -- effect rather than a creature". The event that condition reads is the
        -- one the declaration leg below does record, so this pair is what says
        -- the effect records the attacking side and only that.
        Spec.assertBool s (not (any (blockerWasDeclared . snd) (GameState.events cast))) "no blocker was declared for it"
        -- Never blocked: the trigger is silent and the Prey connects.
        Spec.assertBool s (not (Combat.isBlocked attacker uncast)) "control: with no spell the attacker is unblocked"
        Spec.assertEqWith s "control: so bob takes the Prey's 1" (S.lifeOf S.bob uncast) (Just 19)
        Spec.assertEqWith s "control: and alice gains nothing" (S.lifeOf S.alice uncast) (Just 20)
        Spec.assertEqWith s "control: bob's library is untouched" (length (Game.zoneMembers Zone.Library S.bob uncast)) 1
        -- Blocked by the declaration: the same status from the other writer,
        -- and the board tells them apart by the set and by who is left alive.
        Spec.assertBool s (Combat.isBlocked attacker declared) "declaration leg: blocked as well"
        Spec.assertEqWith s "declaration leg: but the Piker is what is blocking it" (Combat.blockersOf attacker declared) (Set.singleton blocker)
        Spec.assertEqWith s "declaration leg: the same trigger fires" (S.lifeOf S.alice declared) (Just 21)
        Spec.assertBool s (not (S.onBattlefield attacker declared) && not (S.onBattlefield blocker declared)) "declaration leg: and the two creatures trade"
        Spec.assertBool s (any (blockerWasDeclared . snd) (GameState.events declared)) "declaration leg: and CR 509.3d's event IS recorded there"
      _ -> Spec.assertFailure s "fixture should have one attacker and one blocker"
  Spec.it s "CR 509.1h a creature already blocked is not a legal target" $ do
    -- CR 509.1h again, read through the card's own committed target slot:
    -- Pool.Creatures under `And [IsAttacking, Not IsBlocked]`. The pair differs
    -- in exactly one thing -- both attackers are alice's, both are attacking,
    -- and bob's one Piker blocks the first of them.
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    prey <- S.printingOf s registry "Sacred Prey"
    curtain <- S.printingOf s registry "Curtain of Light"
    case (curtainBoard plains piker curtain [prey, prey], S.spellTargetSlot curtain) of
      ((gs, [first, second], _, _), Just slot) -> do
        let atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) S.aggressiveAnswer gs
            legal = Target.legalRecipients Nothing S.noSource slot atDamage
        Spec.assertBool s (Combat.isBlocked first atDamage) "the declaration blocked the first attacker"
        Spec.assertBool s (not (Combat.isBlocked second atDamage)) "and left the second unblocked"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature first) legal)) "Not IsBlocked refuses the blocked attacker"
        Spec.assertBool s (Set.member (Recipient.ToCreature second) legal) "and admits the unblocked one"
      _ -> Spec.assertFailure s "fixture should have two attackers and Curtain of Light a 'target' slot"

-- CR 506.7b's window, "only during combat after blockers are declared", proved
-- step by step on ONE board.
--
-- Every leg comes from the same curtainBoard: the same two seats, the same two
-- Plains paying the same {1}{W}, the same empty stack, and the same unblocked
-- attacking Prey standing as CR 601.2c's legal target. So no leg can refuse the
-- cast for want of mana, of a target, or of a timing window -- the only thing
-- that moves is where in the combat phase the board sits, which is exactly what
-- CR 506.7b is about.
--
-- The pair that carries the rule is `beforeDeclaration` against `declared`:
-- identical states but for CR 509.1's turn-based action having run. Everything
-- after that pair moves GameState.phase on the DECLARED board, which is how the
-- combat damage and end of combat steps -- the two the old transcription's
-- declare-blockers-step window left out -- get read.
--
-- What is NOT provable here is CR 506.7f, and by construction rather than by
-- omission: the pool's only route to a skipped declare blockers step is CR
-- 508.8's empty attack, which leaves no attacking creature, and with no
-- attacking creature Curtain of Light has no legal target -- so such a board
-- refuses the cast whatever the gate answers. The gate implements CR 506.7f
-- all the same, since it reads a record only the step itself writes.
castingWindowSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
castingWindowSpec s registry = Spec.describe s "CastingWindow" $ do
  Spec.it s "CR 506.7b the window opens at the declaration and runs to the end of the combat phase" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    prey <- S.printingOf s registry "Sacred Prey"
    curtain <- S.printingOf s registry "Curtain of Light"
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (gs0, _, _, curtainId) = curtainBoard plains piker curtain [prey]
        (boltId, gs) = S.addHandCard bolt S.bob (snd (S.addCreature mountain S.bob gs0))
        -- The declare blockers step reached but not yet run: attackers are
        -- declared, CR 509.1's turn-based action is not.
        beforeDeclaration = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
        -- That one action, and nothing else. Blocks are DECLINED so the Prey
        -- stays an unblocked attacking creature and every later leg keeps the
        -- same legal target.
        declared = S.runPure declineBlocks beforeDeclaration Combat.declareBlockers
        inStep step = declared {GameState.phase = Phase.Combat step}
        -- The other side of CR 506.7b, on the board that has NOT declared: a
        -- reachable priority window one step earlier.
        atAttackers = beforeDeclaration {GameState.phase = Phase.Combat CombatStep.DeclareAttackers}
    -- Anti-vacuity: bob's unrestricted instant is castable on every one of these
    -- boards, so a leg that refuses the Curtain is refusing the Curtain.
    Spec.assertBool s (all (S.castable S.bob boltId) [beforeDeclaration, declared, inStep CombatStep.CombatDamage, inStep CombatStep.EndOfCombat, atAttackers]) "bob can pay for and legally cast an unrestricted instant on every leg"
    Spec.assertBool s (not (Combat.afterBlockersDeclared beforeDeclaration)) "the declaration has not happened yet"
    Spec.assertBool s (Combat.afterBlockersDeclared declared) "and it has after CR 509.1's turn-based action"
    -- Before the point CR 506.7b names.
    Spec.assertBool s (not (S.castable S.bob curtainId atAttackers)) "refused in the declare attackers step"
    Spec.assertBool s (not (S.castable S.bob curtainId beforeDeclaration)) "refused in the declare blockers step before blockers are declared"
    -- After it. The first was already reachable under the declare-blockers-step
    -- window this replaced; the last two are what that window lost.
    Spec.assertBool s (S.castable S.bob curtainId declared) "castable once blockers are declared"
    Spec.assertBool s (S.castable S.bob curtainId (inStep CombatStep.CombatDamage)) "castable in the combat damage step"
    Spec.assertBool s (S.castable S.bob curtainId (inStep CombatStep.EndOfCombat)) "castable in the end of combat step"

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Combat" $ do
  combatLegalitySpec s registry
  landSubtypeStripSpec s registry
  declareSpec s registry
  combatDamageSpec s registry
  keywordSpec s registry
  firstStrikeSpec s registry
  endOfCombatSpec s registry
  m2bExitSpec s registry
  defenderSpec s registry
  defendingPlayerSpec s registry
  vigilanceSpec s registry
  hasteSpec s registry
  evasionSpec s registry
  textChangedLandwalkSpec s registry
  menaceSpec s registry
  blockPermissionSpec s registry
  blockRequirementSpec s registry
  attackRequirementSpec s registry
  combatRestrictionSpec s registry
  suspectedAbilityRemovalSpec s registry
  conditionalCombatRestrictionSpec s registry
  defendingPlayerRestrictionSpec s registry
  textChangedCombatRestrictionSpec s registry
  textChangedCombatAffectedSpec s registry
  attacksAloneSpec s registry
  boundedDeclarationSpec s registry
  controlChangeSicknessSpec s registry
  controlChangeRemovalSpec s registry
  becomesBlockedSpec s registry
  castingWindowSpec s registry
  typeChangeRemovalSpec s registry
  effectRemovalSpec s registry
  putOntoBattlefieldAttackingSpec s registry
  towershellSpec s registry
  planeswalkerAttackSpec s registry
  trampleOverPlaneswalkersSpec s registry
  sharedBlockerSpec s registry
  lastKnownDefendingPlayerSpec s registry
  attackCostSpec s registry
