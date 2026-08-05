{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Combat: attack/block legality, combat damage, and the combat
-- keywords (flying, reach, defender, vigilance, haste, first/double strike).
-- Also Pawl.Engine.BlockRequirement, whose only consumer is Pawl.Engine.Combat's CR 509.1c
-- check, Pawl.Engine.CombatRestriction, whose only consumer is that module's CR
-- 508.1c and CR 509.1b checks, and Pawl.Engine.AttackCost, whose only consumer is
-- its CR 508.1d cost clause and CR 508.1h total.
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
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Face as Face
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
import qualified Pawl.Types.TargetSpec as TargetSpec
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
      a : _ -> Map.fromList (fmap (\b -> (b, a)) candidates)
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
-- put onto an already-attacking board. S.addCreature is any-printing rather than
-- creature-only, which is how the CR 509.1a Mountain case below reaches a land
-- too.
withLands :: [Printing.Printing] -> GameState.GameState -> GameState.GameState
withLands lands gs = List.foldl' (\g p -> snd (S.addCreature p S.bob g)) gs lands

evasionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
evasionSpec s registry = Spec.describe s "Evasion" $ do
  Spec.it s "CR 702.9b a declaration in which a ground creature blocks a flier is illegal" $ do
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [birdMaiden] [piker]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs)) "illegal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.17b a reach creature may block a flier" $ do
    -- THE FALSIFIER. Fails against any implementation that asks "does the
    -- blocker have flying?"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    nimbleBirdsticker <- S.printingOf s registry "Nimble Birdsticker"
    let (gs, mine, theirs) = attacking [birdMaiden] [nimbleBirdsticker]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs) "legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.9b a flier may block a ground creature" $ do
    -- The asymmetry: 702.9b's second sentence. Fails if flying is implemented
    -- as a symmetric predicate.
    piker <- S.printingOf s registry "Goblin Piker"
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    let (gs, mine, theirs) = attacking [piker] [birdMaiden]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs) "legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.9b a flier may block a flier" $ do
    birdMaiden <- S.printingOf s registry "Bird Maiden"
    let (gs, mine, theirs) = attacking [birdMaiden] [birdMaiden]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs) "legal"
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
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) (withFear a gs0))) "illegal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.36b a black creature may block a creature with fear" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let (gs0, mine, theirs) = attacking [piker] [typhoidRats]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b a) (withFear a gs0)) "legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.36b an ARTIFACT creature may block a creature with fear" $ do
    -- THE FALSIFIER for reading 702.36b as a colour test alone: Darksteel Myr
    -- is a colourless artifact creature and blocks legally.
    piker <- S.printingOf s registry "Goblin Piker"
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let (gs0, mine, theirs) = attacking [piker] [darksteelMyr]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b a) (withFear a gs0)) "legal"
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
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) (withFear a gs0))) "illegal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 702.36b fear restricts being blocked, never blocking" $ do
    -- The 702.9b asymmetry, restated for fear: a fear creature blocking a
    -- plain attacker is legal.
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs0, mine, theirs) = attacking [piker] [piker]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b a) (withFear b gs0)) "legal"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"

  Spec.it s "CR 702.14c a swampwalker may not be blocked while the defending player controls a Swamp" $ do
    -- Bog Wraith is "Creature -- Wraith 3/3, Swampwalk" and nothing else, so
    -- this asks about the keyword and no other text.
    bogWraith <- S.printingOf s registry "Bog Wraith"
    piker <- S.printingOf s registry "Goblin Piker"
    swamp <- S.printingOf s registry "Swamp"
    let (gs0, mine, theirs) = attacking [bogWraith] [piker]
    case (mine, theirs) of
      (a : _, b : _) ->
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) (withLands [swamp] gs0))) "illegal"
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
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs) "legal"
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
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs)) "illegal"
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
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) (withLands [snowSwamp] gs0))) "illegal"
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
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs) "legal"
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
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs) "legal"
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
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) (withLands [ashBarrens] gs0))) "illegal"
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
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs) "legal"
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
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) armed)) "illegal while equipped"
        -- THE FALSIFIER, and what makes this a granted-keyword test rather than
        -- a repeat of the printed ones: the SAME board with the Gloves
        -- unattached. A bare Piker has no landwalk, so the block is legal.
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b a) equipped) "legal once nothing is equipped"
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
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b a) armed) "legal"
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
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs)) "illegal"
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
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToObject target)) sets
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
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton blocker warrior) onIsland)) "an Island stops the block"
    -- The Lord's OTHER modification, which the swap must leave alone: a Tidal
    -- Warrior is a printed 1/1, so a 2/2 is the +1/+1 half still applying.
    Spec.assertEqWith s "and the Warrior is a 2/2" (Projection.powerOf warrior onIsland) (Just 2)
    (onSwamp, warrior', blocker') <- lordBoard False "Swamp"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker' warrior') onSwamp) "a Swamp does not"
  Spec.it s "CR 612.1 a hacked Lord of Atlantis grants SWAMPwalk instead" $ do
    -- THE CASE. Island -> Swamp on the Lord, and bob's board never moves: the
    -- Island that used to stop the block no longer does, and the Swamp that
    -- used to allow it no longer does either. Both halves fail against a
    -- rewrite that walks past a Modification.GainKeyword.
    (onIsland, warrior, blocker) <- lordBoard True "Island"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker warrior) onIsland) "an Island no longer stops the block"
    let after = S.runPure S.aggressiveAnswer onIsland Combat.declareBlockers
    Spec.assertEqWith s "and the block sticks" (Combat.blockersOf warrior after) (Set.singleton blocker)
    Spec.assertEqWith s "the Warrior is still a 2/2" (Projection.powerOf warrior onIsland) (Just 2)
    (onSwamp, warrior', blocker') <- lordBoard True "Swamp"
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton blocker' warrior') onSwamp)) "a Swamp stops it now"
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
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton blocker warrior) onIsland)) "an Island still stops the block"
    Spec.assertEqWith s "and the Warrior is still a 2/2" (Projection.powerOf warrior onIsland) (Just 2)
    -- The half that keeps this from passing by the landwalk simply vanishing: a
    -- Warrior that had wrongly picked up swampwalk would make this one illegal.
    (onSwamp, warrior', blocker') <- lordBoardAt theWarrior True "Swamp"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker' warrior') onSwamp) "and a Swamp still does not"
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
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton blocker wraith) onSwamp)) "a Swamp stops the block"
    (onIsland, wraith', blocker') <- wraithBoard False "Island"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker' wraith') onIsland) "an Island does not"
  Spec.it s "CR 612.1 a hacked Bog Wraith walks on ISLANDS" $ do
    (onSwamp, wraith, blocker) <- wraithBoard True "Swamp"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker wraith) onSwamp) "a Swamp no longer stops the block"
    let after = S.runPure S.aggressiveAnswer onSwamp Combat.declareBlockers
    Spec.assertEqWith s "and the block sticks" (Combat.blockersOf wraith after) (Set.singleton blocker)
    (onIsland, wraith', blocker') <- wraithBoard True "Island"
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton blocker' wraith') onIsland)) "an Island stops it now"

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
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs)) "illegal"
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
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.fromList [(b, a), (c, a)]) gs) "legal"
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
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs)) "one blocker is not"
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
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b plain) gs) "one blocker on the plain attacker is legal"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b brute) gs)) "one blocker on the menace attacker is not"
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
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.fromList [(rats, a), (myr, a)]) gs) "two fear-legal blockers"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.fromList [(rats, a), (plain, a)]) gs)) "two blockers, one of which fear forbids"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton rats a) gs)) "one fear-legal blocker is still one blocker"
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
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs)) "but one creature may not block it"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
    let (gs2, mine2, theirs2) = attacking [piker] [boggartBrute]
    case (mine2, theirs2) of
      (a : _, b : _) ->
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs2) "a menace creature blocking alone is legal"
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
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs)) "one blocking is illegal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.fromList [(b, a), (c, a)]) gs) "both blocking is legal"
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
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs) "legal"
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
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton reacher a) gs) "the reach creature blocking is legal"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton ground a) gs)) "the ground creature blocking is illegal"
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
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton first a) gs)) "one blocker is not enough"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.fromList [(first, a), (second, a)]) gs) "both blockers is legal"
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
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b a) gs) "blocking the Unicorn is legal"
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
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b unicorn) gs) "blocking the Unicorn is legal"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b other) gs)) "blocking the other attacker instead is illegal"
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
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton b a) underHumility) "and blocking is still legal, so the combat is still live"
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
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton pacified a) board)) "blocking with the enchanted creature is illegal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton other a) board) "blocking with the other one is legal"
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

-- CR 508.1c's and CR 509.1b's SECOND clause -- "or that it can't attack unless
-- some condition is met" -- proved by Blind-Spot Giant ("This creature can't
-- attack or block unless you control another Giant"), the pool's first printed
-- conditional restriction. Pacifism above prints the first clause of the same
-- parenthetical, and the two groups are deliberately separate: what is under test
-- here is only that the gate is read, and read afresh.
--
-- The card is the right prover on three counts. Its condition reads YOUR OWN
-- board rather than the defending player's, which is the one thing a condition
-- still cannot name (#620), and it gates on a FACT rather than on a cost, which
-- rides Pawl.Types.AttackCost instead for the reason CR 508.1d's third sentence
-- gives. It prints BOTH arms from one line, as Pacifism does. And "ANOTHER
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
                      Combat.Type.defender = Just S.bob
                    }
              }
    Spec.assertEqWith s "starts empty" (Combat.Type.attackers (GameState.combat gs)) Map.empty
    Spec.assertEqWith s "clears" (Combat.Type.attackers (GameState.combat (Combat.clearCombat busy))) Map.empty

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

-- Run whole steps until `step` is the current phase, WITHOUT running it, so a
-- test can play that one step itself under a different answerer. Bounded so a
-- bug cannot loop forever. Stops early if combat is left, so a caller that names
-- a step this combat never reaches gets the state at the exit rather than a
-- hang.
runToStep :: Phase.Phase -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToStep step answer gs0 =
  let go n g =
        if n <= (0 :: Int)
          || GameState.phase g == step
          || not (S.inCombatPhase (GameState.phase g))
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 8 gs0

-- Run whole steps until the end of combat step is the current phase, WITHOUT
-- running it, so a test can play that one step itself under a different
-- answerer.
runToEndOfCombat :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToEndOfCombat = runToStep (Phase.Combat CombatStep.EndOfCombat)

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
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToCreature victim)) sets
  Prompt.ChooseAction {} -> S.castAnswer p
  Prompt.DeclareAttackers _ _ ids -> filter (/= homebody) ids
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

-- Block with everything, cast whenever a cast is offered, aim every target at
-- `victim`. The blocker-side twin of `steal`.
snatch :: ObjectId.ObjectId -> Prompt.Prompt r -> r
snatch victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToCreature victim)) sets
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
-- speculatively. Ray of Command's remaining sentence, the delayed trigger that
-- taps the creature when its controller loses it, is not implemented (#287).
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
    case (rayBoard island piker rayOfCommand, S.spellTargetSpec killShot) of
      ((gs, [stolen, other, homebody]), Just attackingSpec) -> do
        let atEnd = runToEndOfCombat (steal homebody stolen) gs
            attackers = Combat.Type.attackers (GameState.combat atEnd)
            legal = Target.legalRecipients Nothing S.noSource attackingSpec atEnd
        Spec.assertEqWith s "the leg really reached the end of combat step, where the record still reads live (CR 511.3)" (GameState.phase atEnd) (Phase.Combat CombatStep.EndOfCombat)
        Spec.assertEqWith s "bob really did gain control of it" (Projection.controllerOf stolen atEnd) (Just S.bob)
        Spec.assertBool s (Map.notMember stolen attackers) "CR 506.4: so it is no longer an attacking creature"
        Spec.assertBool s (Map.member other attackers) "the attacker bob left alone is untouched"
        -- The discriminating assertion: the unfixed engine keeps the stolen
        -- Piker in the record and deals its 2 alongside the other's.
        Spec.assertEqWith s "CR 510.1: bob takes only the surviving attacker's 2" (S.lifeOf S.bob atEnd) (Just 18)
        -- CR 508.1k through the door a card actually uses: Kill Shot's own
        -- committed target spec is Pool.Creatures narrowed by IsAttacking.
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
        let atBlockers = runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
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
-- parse of the committed card data (S.spellTargetSpec's posture, for an
-- activated ability rather than a spell). The first is the land's "{T}: Add
-- {C}".
removalAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card)
removalAbility printing = case Face.activatedAbilities (S.combinedFace printing) of
  [_, ability] -> Just ability
  _ -> Nothing

-- That ability's "target" slot spec: CR 601.2c's narrowing, reached for an
-- activated ability through CR 602.2b, which for this card is Pool.Creatures
-- under `Or [IsAttacking, IsBlocking]`.
removalSpec :: ActivatedAbility.ActivatedAbility Card.Type.Card -> Maybe TargetSpec.TargetSpec
removalSpec ability =
  Map.lookup
    (SlotName.MkSlotName (Text.pack "target"))
    (Modal.allTargetSpecs (ActivatedAbility.modal ability))

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
  Prompt.ChooseTargets _ _ _ sets -> pure (fmap (const (Recipient.ToCreature victim)) sets)
  Prompt.ChooseManaSource _ _ candidates ->
    pure (Maybe.fromMaybe (NonEmpty.head candidates) (List.find (/= mazeId) (NonEmpty.toList candidates)))
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
            legal = fmap (\theSpec -> Target.legalRecipients Nothing S.noSource theSpec atEnd) (removalSpec ability)
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
        let atBlockers = runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
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
        let atDamage = runToStep (Phase.Combat CombatStep.CombatDamage) (stayHomeAnswer homebody) gs
            legal = fmap (\theSpec -> Target.legalRecipients Nothing S.noSource theSpec atDamage) (removalSpec ability)
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
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToCreature victim)) sets
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
    a : _ -> Map.singleton blocker a
    [] -> Map.empty
  _ -> S.aggressiveAnswer p

-- Block the first attacker with `blocker` alone, cast whenever a cast is offered,
-- and aim every target at `victim`. Blocking with everything instead would put
-- Living Plane -- a 4/4 creature while Opalescence is out -- in front of the
-- attacker too, and killing it would then be a blocker LEAVING THE BATTLEFIELD,
-- which is a different clause of CR 506.4.
unblock :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
unblock blocker victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToCreature victim)) sets
  Prompt.ChooseAction {} -> S.castAnswer p
  Prompt.DeclareBlockers _ _ _ attackers -> case attackers of
    a : _ -> Map.singleton blocker a
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
        let atBlockers = runToStep (Phase.Combat CombatStep.DeclareBlockers) (attackOnly attacker) gs
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
        atBlockers = runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
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
        atBlockers = runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
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
        atBlockers = runToStep (Phase.Combat CombatStep.DeclareBlockers) attackThePlaneswalker gs
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
        atFirstStrike = runToStep (Phase.Combat CombatStep.CombatDamage) attackThePlaneswalker gs
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
    let atBlockers = runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
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

-- A declare-attackers board with a real Ghostly Prison under `who`'s control and
-- `lands` untapped Forests under alice's. `cursing`'s twin on the cost side of CR
-- 508.1d: alice is active with one creature per printing in `mine`, and the
-- Prison's controller is the only thing that decides whether her attacks are
-- taxed at all.
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
-- comply with CR 508.1.
--
-- Every case here is arithmetic rather than a threshold: the tax is {2} a
-- creature and a Forest makes one mana, so "how many Forests were tapped" reads
-- the total cost off the board directly.
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

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Combat" $ do
  combatLegalitySpec s registry
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
  blockRequirementSpec s registry
  attackRequirementSpec s registry
  combatRestrictionSpec s registry
  conditionalCombatRestrictionSpec s registry
  attacksAloneSpec s registry
  controlChangeSicknessSpec s registry
  controlChangeRemovalSpec s registry
  typeChangeRemovalSpec s registry
  effectRemovalSpec s registry
  putOntoBattlefieldAttackingSpec s registry
  towershellSpec s registry
  planeswalkerAttackSpec s registry
  attackCostSpec s registry
