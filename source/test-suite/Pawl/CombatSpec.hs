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
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.BlocksDeclared as BlocksDeclared
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Expiry as Expiry
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
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
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
-- 509.1a Mountain case below reaches a land.
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
            (rayId, withRay) = S.addCreature ray S.bob gs
            enchanted = S.attach rayId guard withRay
            humbled = snd (S.addCreature humility S.bob gs)
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
-- built from, and what a board `cursing` cannot build -- one whose planeswalker
-- needs its loyalty counters placed first -- reaches for instead.
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

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Combat" $ do
  declareSpec s registry
  combatDamageSpec s registry
  defenderSpec s registry
  defendingPlayerSpec s registry
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
  controlChangeSicknessSpec s registry
