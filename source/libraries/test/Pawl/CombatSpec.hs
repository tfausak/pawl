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
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Activatable as Activatable
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
import qualified Pawl.Scenario as Scenario
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActiveAttackProhibition as ActiveAttackProhibition
import qualified Pawl.Types.ActiveAttackRequirement as ActiveAttackRequirement
import qualified Pawl.Types.ActiveBlockProhibition as ActiveBlockProhibition
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AfterObjectTurn as AfterObjectTurn
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.BlocksDeclared as BlocksDeclared
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Choices as Choices
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KickerDecision as KickerDecision
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RestrictedCreatures as RestrictedCreatures
import qualified Pawl.Types.Seat as Seat
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Staged as Staged
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

declaredAttackers :: GameState.GameState -> [ObjectId.ObjectId]
declaredAttackers gs = Map.keys (Combat.Type.attackers (GameState.combat gs))

declareSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
declareSpec s registry = Spec.describe s "Declare" $ do
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
-- Decider observable without touching choosesDefender's cases.
choosesDefenderRecordingDecider :: PlayerId.PlayerId -> Prompt.Prompt r -> State.State [Decider.Decider] r
choosesDefenderRecordingDecider who p = case p of
  Prompt.ChooseDefender decider _ _ -> do
    State.modify' (\seen -> seen <> [decider])
    pure who
  _ -> pure (S.identityAnswer p)

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

-- CR 506.2/506.2a/507.1/703.4h: WHO is being attacked. Distinct from
-- defenderSpec, which is the Defender KEYWORD (CR 702.3b).
defendingPlayerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
defendingPlayerSpec s registry = Spec.describe s "DefendingPlayer" $ do
  Spec.it s "CR 723.1 a controlled active player's choice of defender routes to their controller" $ do
    -- THREE seats (alice active, bob, carol) with carol controlling alice
    -- (Mindslaver-style: GameState.control names carol as alice's decider),
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
    -- violation this test exists to catch -- and no other case in this group
    -- sets GameState.control, so none of them would notice.
    let controlled = (S.oneDefendingPlayer S.threePlayerGame) {GameState.control = S.turnControl S.carol S.alice}
        (_, deciders) =
          State.runState
            (fmap snd (Engine.runGame (choosesDefenderRecordingDecider S.bob) controlled (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))))
            []
    Spec.assertEqWith s "carol, alice's controller, is who was asked" deciders [Decider.MkDecider S.carol]
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
  Spec.it s "CR 800.4h designateDefenders called directly reassigns the same way" $ do
    -- CR 800.4h reached WITHOUT Engine.runTurnBasedActions, so that only
    -- designateDefenders's own Game.ruleChooser call can be responsible. The
    -- turn-based road is data/scenarios/departed-active-player-choice-passes-on.json.
    let gone = S.departs Departure.Type.Conceded S.alice (S.oneDefendingPlayer S.threePlayerGame)
        (after, asked) =
          State.runState
            (fmap snd (Engine.runGame (choosesDefender S.carol) gone Combat.designateDefenders))
            []
    Spec.assertEqWith s "bob, the next player in turn order, is who was asked" asked [S.bob]
    Spec.assertEqWith s "and carol, his answer, is the defending player" (Combat.Type.defenders (GameState.combat after)) [S.carol]
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
  Spec.it s "CR 506.2 the designation does not outlive the combat phase" $ do
    -- CR 506.2's sentences are all scoped "During the combat phase", and
    -- CR 703.4h makes the choice per beginning-of-combat step, so a second
    -- combat phase in one turn chooses again. Discriminating: a clearCombat
    -- that reset only attackers and blockers would leave Just carol here, and
    -- the next combat phase would inherit a stale defender.
    let busy = S.threePlayerGame {GameState.combat = (GameState.combat S.threePlayerGame) {Combat.Type.defenders = [S.carol]}}
    Spec.assertEqWith s "cleared at end of combat" (Combat.Type.defenders (GameState.combat (Combat.clearCombat busy))) []
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

-- Re-sicken alice's creatures, as though they had just resolved this turn.
justArrived :: GameState.GameState -> GameState.GameState
justArrived gs =
  let sicken o = if Object.owner o == S.alice then o {Object.sickness = Sickness.Sick} else o
   in gs {GameState.objects = fmap sicken (GameState.objects gs)}

hasteSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
hasteSpec s registry = Spec.describe s "Haste" $ do
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

-- Any printings at all onto `who`'s battlefield, on a board that already exists.
-- S.addPermanent is any-printing rather than creature-only, which is how the CR
-- 509.1a Mountain case below reaches a land.
withPermanents :: PlayerId.PlayerId -> [Printing.Printing] -> GameState.GameState -> GameState.GameState
withPermanents who ps gs = List.foldl' (\g p -> snd (S.addPermanent p who g)) gs ps

-- Prison Barricade {1}{W} Creature -- Wall 1/3, whole text: "Defender / Kicker
-- {1}{W} / If this creature was kicked, it enters with a +1/+1 counter on it and
-- with 'This creature can attack as though it didn't have defender.'" (Oracle
-- checked on Scryfall). Cast off four Plains with `kicks` as the kicker answer,
-- then settled (CR 302.6, a fixture precondition both boards share) and run
-- through a combat aimed at bob. Returns the board after combat and the Wall.
barricadeCombat :: Printing.Printing -> Printing.Printing -> Natural.Natural -> (GameState.GameState, Maybe ObjectId.ObjectId)
barricadeCombat plains barricade kicks =
  let (board, cardId) = S.handOne barricade (S.landsInPlay plains 4)
      cast = S.runPure (kickWith kicks) board (S.cast S.alice cardId >> Stack.resolveTop >> Engine.settleForPriority)
      wall = List.find (\o -> Projection.hasName (CardName.MkCardName (Text.pack "Prison Barricade")) o cast) (Set.toList (GameState.battlefield cast))
      settle o = o {Object.sickness = Sickness.Settled S.alice}
      ready =
        cast
          { GameState.objects = maybe id (Map.adjust settle) wall (GameState.objects cast),
            GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
            GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.bob]},
            GameState.remaining = S.phasesAfter (Phase.Combat CombatStep.DeclareAttackers)
          }
   in (S.runCombat (S.attackTo S.bob) ready, wall)

-- CR 702.33a's answer, and S.identityAnswer's to everything else.
kickWith :: Natural.Natural -> Prompt.Prompt r -> r
kickWith kicks p = case p of
  Prompt.ChooseKicker {} -> KickerDecision.MkKickerDecision kicks
  _ -> S.identityAnswer p

evasionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
evasionSpec s registry = Spec.describe s "Evasion" $ do
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
  -- CR 702.3b lifted: the pair of boards differs only in the kicker answer, so
  -- the kicked entry's quoted ability is what lets the Wall attack. Declared
  -- through the turn-based action, not asked of Combat.canAttack.
  Spec.it s "CR 702.3b a kicked Prison Barricade attacks as though it didn't have defender" $ do
    plains <- S.printingOf s registry "Plains"
    barricade <- S.printingOf s registry "Prison Barricade"
    case (barricadeCombat plains barricade 1, barricadeCombat plains barricade 0) of
      ((kicked, Just wall), (unkicked, Just _)) -> do
        Spec.assertEqWith s "CR 702.3b kicked, the Wall is declared as an attacker" (S.attackerDeclarationsOf kicked) [wall]
        Spec.assertBool s (Projection.hasKeyword Keyword.Defender wall kicked) "and it still has defender"
        Spec.assertEqWith s "CR 702.3b unkicked, defender keeps it home" (S.attackerDeclarationsOf unkicked) []
      _ -> Spec.assertFailure s "the Barricade should be on the battlefield after each cast"
  -- CR 509.1b's "unless" gate read against CR 205.3m: Graxiplon "can't be blocked
  -- unless defending player controls three or more creatures that share a
  -- creature type". Every board declares the same Hill Giant of bob's blocking;
  -- they differ only in bob's third creature, and the last in alice's.
  Spec.it s "CR 205.3m Graxiplon can be blocked only when bob controls three creatures sharing a creature type" $ do
    graxiplon <- S.printingOf s registry "Graxiplon"
    giant <- S.printingOf s registry "Hill Giant"
    piker <- S.printingOf s registry "Goblin Piker"
    changeling <- S.printingOf s registry "Woodland Changeling"
    let blockable mine third = case attacking (graxiplon : mine) [giant, giant, third] of
          (gs, a : _, b : _) -> Just (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs)
          _ -> Nothing
    Spec.assertEqWith s "three Giants: the block is legal" (blockable [] giant) (Just True)
    Spec.assertEqWith s "two Giants and a Goblin share no type three ways: illegal" (blockable [] piker) (Just False)
    Spec.assertEqWith s "CR 702.73a two Giants and a changeling: legal" (blockable [] changeling) (Just True)
    Spec.assertEqWith s "alice's Goblin is not the defending player's: still illegal" (blockable [piker] piker) (Just False)
    -- Forest is a land type, not a creature type: a Dryad Arbor beside two
    -- Forests made creatures (layer 4, a stored continuous effect) holds Forest
    -- three ways and a creature type only once.
    arbor <- S.printingOf s registry "Dryad Arbor"
    forest <- S.printingOf s registry "Forest"
    let animate oid gs =
          let (ts, gs1) = Game.freshTimestamp gs
              eff = ContinuousEffect.MkContinuousEffect oid ts Expiry.AtCleanup (Modification.AddCardType CardType.Creature) (Affected.TheseObjects (Set.singleton oid))
           in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}
    case attacking [graxiplon] [arbor, forest, forest] of
      (gs, a : _, [b, f1, f2]) ->
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) (animate f2 (animate f1 gs)))) "CR 205.3m three creatures sharing only Forest: illegal"
      _ -> Spec.assertFailure s "fixture should give bob a Dryad Arbor and two Forests"
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

  Spec.it s "CR 702.13b a COLOURLESS creature with intimidate may be blocked only by artifact creatures" $ do
    -- THE FALSIFIER that separates intimidate from fear, and the only assertion
    -- here a hard-coded Color.Black fails. CR 105.3's "effects may also make a
    -- colored object become colorless" is SetColor with no colours, so the
    -- attacker is the printed Ghoul with its colour taken away at CR 613 layer
    -- 5. CR 105.2c: it now has no colour, so it shares one with nobody -- the
    -- black Typhoid Rats that blocks the printed Ghoul legally
    -- (cr-702-13b-a-black-creature-may-block-a-creature-with.json) may not, and
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
  let -- combatBoardOf returns the ids in printing order, so the Lord is the
      -- second.
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
    (onSwamp, warrior2, blocker2) <- lordBoard False "Swamp"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker2 (Set.singleton warrior2)) onSwamp) "a Swamp does not"
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
    (onIsland, wraith2, blocker2) <- wraithBoard False "Island"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker2 (Set.singleton wraith2)) onIsland) "an Island does not"

-- CR 702.111: grant menace to `oid` with a stored continuous effect. Used only
-- by the CR 509.1b "after a legal block has been declared" case below, which
-- needs menace to ARRIVE mid-combat; every other case here reads Boggart
-- Brute's printed keyword.
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
            Maybe.mapMaybe
              ( \event -> case event of
                  GameEvent.BlocksDeclared (BlocksDeclared.MkBlocksDeclared b n) | b == guard -> Just n
                  _ -> Nothing
              )
              (S.eventsOf after)
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
  Spec.it s "CR 509.1c two requirements on ONE pair count twice" $ do
    -- CR 509.1c counts REQUIREMENTS being obeyed, not the (blocker, attacker)
    -- pairs they name. The board of
    -- cr-509-1a-two-attackers-one-screen-blocking-either-attains.json, plus a
    -- Lure on the SECOND attacker:
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

-- CR 509.1b's pairwise restriction written from the BLOCKER's side
-- (CombatRestriction.CantBlockCreatures). Every board attacks with a 2/1 Goblin
-- Piker AND a 1/1 Llanowar Elves, so each barred pair has a legal twin beside it
-- and a blocker that could block nothing fails the second leg.
cantBlockCreaturesSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
cantBlockCreaturesSpec s registry = Spec.describe s "CantBlockCreatures" $ do
  let blocks gs b a = Combat.legalBlockDeclaration S.bob (Map.singleton b (Set.singleton a)) gs
  -- Wan Shi Tong, All-Knowing's Spirit tokens, "This token can't block or be
  -- blocked by non-Spirit creatures": the card's own library trigger makes them,
  -- and each half is proved against a Spirit (Dutiful Knowledge Seeker) and a
  -- non-Spirit (Goblin Piker) on the other side.
  let spiritsFor gs0 who = do
        island <- S.printingOf s registry "Island"
        let (cardId, gs1) = S.addHandCard island who gs0
            moved = S.runPure S.identityAnswer gs1 (Event.changeZone cardId Zone.Library)
            settle g =
              let g' = S.runPure S.identityAnswer g Engine.settleForPriority
               in if null (GameState.stack g') then g' else settle (S.runPure S.identityAnswer g' Stack.resolveTop)
        pure (settle moved)
  Spec.it s "CR 509.1b Wan Shi Tong's Spirit tokens can't block a non-Spirit, and can block a Spirit" $ do
    wan <- S.printingOf s registry "Wan Shi Tong, All-Knowing"
    seeker <- S.printingOf s registry "Dutiful Knowledge Seeker"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs0, mine, _) = S.combatBoardOf [piker, seeker] [wan]
    made <- spiritsFor gs0 S.bob
    let gs = snd (Engine.runGamePure S.aggressiveAnswer made (Combat.declareAttackers S.manaPerformer S.alice))
    case (mine, S.tokensOf gs) of
      ([pikerId, seekerId], [a, b]) -> do
        Spec.assertBool s (not (blocks gs a pikerId)) "a Spirit token may not block the non-Spirit Piker"
        Spec.assertBool s (blocks gs a seekerId) "it may block the Spirit Seeker"
        Spec.assertEqWith s "both tokens are bob's" (fmap (`Projection.controllerOf` gs) [a, b]) [Just S.bob, Just S.bob]
      (_, other) -> Spec.assertFailure s ("expected two Spirit tokens, got " <> show (length other))

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

-- CR 508.1c's PAIRWISE attacking restriction: one naming WHAT the attack is aimed
-- at rather than which creatures may attack. Blazing Archon ({6}{W}{W}{W}
-- Creature -- Archon 5/6, "Flying / Creatures can't attack you." -- checked
-- against Scryfall, 2026-09-01) is one, and CR 802.3a is the rule
-- that says such a restriction reaches only the creatures attacking that player.
-- Vow of Flight, near the foot of the group, names two of CR 506.3's three
-- attackable things where the Archon names one; Teferi's Moat, last, reads its
-- own chosen colour.
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

  -- CR 607.2d: Teferi's Moat ("As this enchantment enters, choose a color. /
  -- Creatures of the chosen color without flying can't attack you.", Scryfall
  -- 2026-10-02) reads its own CR 105.2 choice in the restriction's affected set.
  -- The boards differ only in the colour chosen; the red Bird Maiden flies.
  Spec.it s "CR 607.2d Teferi's Moat bars only nonflying creatures of the colour it chose" $ do
    moat <- S.printingOf s registry "Teferi's Moat"
    piker <- S.printingOf s registry "Goblin Piker"
    maiden <- S.printingOf s registry "Bird Maiden"
    let (gs, mine, theirs) = S.combatBoardOf [piker, maiden] [moat]
    case (mine, theirs) of
      ([pikerId, maidenId], [moatId]) -> do
        let choosing color = gs {GameState.objects = Map.adjust (\o -> o {Object.chosenColors = Set.singleton color}) moatId (GameState.objects gs)}
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [pikerId] (choosing Color.Red))) "red chosen: the red Piker may not attack bob"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [pikerId] (choosing Color.Green)) "green chosen: the red Piker may"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [maidenId] (choosing Color.Red)) "red chosen: the red Bird Maiden flies, so it may"
      _ -> Spec.assertFailure s "fixture should give alice a Piker and a Bird Maiden and bob a Moat"

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

-- CR 508.5a / 802.3a: a restriction judged against the WHOLE declaration --
-- "can't attack alone", and an unscoped "no more than N creatures can attack" --
-- whose gate names the defending player. The restriction applies to attacking
-- creatures, so the gate is read per creature, at the seat THAT creature is
-- announced against. Scryfall o:"attack alone unless" and o:"can attack each
-- combat unless", 2026-10-08, find only Pipsqueak, Rebel Strongarm, gated on
-- itself, so the producers are Synthetic Tidal Palisade ({3} Artifact,
-- "No more than one creature can attack each combat unless defending player
-- controls an Island.") and Synthetic Tidal Sentry ({1} Artifact Creature --
-- Construct 2/1, "This creature can't attack alone unless defending player
-- controls an Island.").
--
-- bob, the FIRST defending player in turn order, holds the only Island, so the
-- old reading -- the gate judged at that one seat -- lifted both restrictions for
-- every announcement.
perDefenderWholeRestrictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
perDefenderWholeRestrictionSpec s registry = Spec.describe s "PerDefenderWholeRestriction" $ do
  let declaring g =
        g
          { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
            GameState.combat = (GameState.combat g) {Combat.Type.defenders = [S.bob, S.carol]}
          }
  Spec.it s "CR 508.5a the bound counts only the creatures attacking a defender without an Island" $ do
    palisade <- S.printingOf s registry "Synthetic Tidal Palisade"
    bears <- S.printingOf s registry "Grizzly Bears"
    island <- S.printingOf s registry "Island"
    let (gs, mine, _, _) = S.threePlayerCombat [palisade, bears, bears] [island] []
        board = declaring gs
    case mine of
      [_, first, second] -> do
        Spec.assertBool s (not (Combat.legalAttackDeclarationAs S.alice [(first, AttackTarget.OfPlayer S.carol), (second, AttackTarget.OfPlayer S.carol)] board)) "two attacking carol, who controls no Island, is over the bound"
        Spec.assertBool s (Combat.legalAttackDeclarationAs S.alice [(first, AttackTarget.OfPlayer S.bob), (second, AttackTarget.OfPlayer S.bob)] board) "two attacking bob, who controls the Island, is not bound at all"
        Spec.assertBool s (Combat.legalAttackDeclarationAs S.alice [(first, AttackTarget.OfPlayer S.bob), (second, AttackTarget.OfPlayer S.carol)] board) "one at each seat counts one against the bound"
      _ -> Spec.assertFailure s "fixture should give alice a Palisade and two Bears"
  Spec.it s "CR 508.5a can't attack alone binds only a creature announced at a defender without an Island" $ do
    sentry <- S.printingOf s registry "Synthetic Tidal Sentry"
    bears <- S.printingOf s registry "Grizzly Bears"
    island <- S.printingOf s registry "Island"
    let (gs, mine, _, _) = S.threePlayerCombat [sentry, bears] [island] []
        board = declaring gs
    case mine of
      [sentryId, bearsId] -> do
        Spec.assertBool s (not (Combat.legalAttackDeclarationAs S.alice [(sentryId, AttackTarget.OfPlayer S.carol)] board)) "the Sentry alone may not attack carol, who controls no Island"
        Spec.assertBool s (Combat.legalAttackDeclarationAs S.alice [(sentryId, AttackTarget.OfPlayer S.bob)] board) "and alone may attack bob, who does"
        Spec.assertBool s (Combat.legalAttackDeclarationAs S.alice [(sentryId, AttackTarget.OfPlayer S.carol), (bearsId, AttackTarget.OfPlayer S.carol)] board) "with a companion it may attack carol"
      _ -> Spec.assertFailure s "fixture should give alice a Sentry and a Bears"
  Spec.it s "CR 508.1d the maximum sends both Berserkers at the defender the bound does not count" $ do
    -- Berserkers of Blood Ridge attacks each combat if able. Both may attack,
    -- at bob, so the maximum is two and one Berserker alone falls short of it --
    -- which a search treating the gated bound as a bound on every seat would
    -- call the maximum.
    palisade <- S.printingOf s registry "Synthetic Tidal Palisade"
    berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
    island <- S.printingOf s registry "Island"
    let (gs, mine, _, _) = S.threePlayerCombat [palisade, berserkers, berserkers] [island] []
        board = declaring gs
    case mine of
      [_, first, second] -> do
        Spec.assertBool s (not (Combat.legalAttackDeclarationAs S.alice [(first, AttackTarget.OfPlayer S.bob)] board)) "one Berserker alone obeys fewer requirements than the maximum"
        Spec.assertBool s (Combat.legalAttackDeclarationAs S.alice [(first, AttackTarget.OfPlayer S.bob), (second, AttackTarget.OfPlayer S.carol)] board) "both attacking, one at each seat, obeys it"
      _ -> Spec.assertFailure s "fixture should give alice a Palisade and two Berserkers"

-- CR 508.5 / 805.10e: a pairwise blocking gate naming the defending player is
-- read at the player the ATTACKER is attacking, not at the first defending player
-- and not at the blocker's controller. Graxiplon ("can't be blocked unless
-- defending player controls three or more creatures that share a creature type")
-- is the producer; every board gives the attacked player three Hill Giants and
-- the other defender at most one.
defendingPlayerOfBlockGateSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
defendingPlayerOfBlockGateSpec s registry = Spec.describe s "DefendingPlayerOfBlockGate" $ do
  -- Free for all, three seats: Graxiplon and a Goblin Piker attack carol, a
  -- second Piker attacks bob (the first defending player, with no creature).
  -- carol's Giant blocks the Piker, and General Jarkeld's switch
  -- (Combat.switchBlockers, which its effect performs) moves it onto Graxiplon.
  Spec.it s "CR 508.5 a switched blocker reads Graxiplon's gate at carol, whom it attacks" $ do
    graxiplon <- S.printingOf s registry "Graxiplon"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    let (gs, mine, _, hers) = S.threePlayerCombat [graxiplon, piker, piker] [] [giant, giant, giant]
    case (mine, hers) of
      ([grax, atCarol, atBob], blocker : _) -> do
        let board =
              gs
                { GameState.phase = Phase.Combat CombatStep.DeclareBlockers,
                  GameState.combat =
                    (GameState.combat gs)
                      { Combat.Type.defenders = [S.bob, S.carol],
                        Combat.Type.attackers = Map.fromList [(grax, AttackTarget.OfPlayer S.carol), (atCarol, AttackTarget.OfPlayer S.carol), (atBob, AttackTarget.OfPlayer S.bob)],
                        Combat.Type.blockers = Map.singleton atCarol (Set.singleton blocker)
                      }
                }
            switched = Combat.switchBlockers atCarol grax board
        Spec.assertEqWith s "CR 508.5: the Giant now blocks Graxiplon" (Combat.blockersOf grax switched) (Set.singleton blocker)
        Spec.assertEqWith s "and no longer the Piker" (Combat.blockersOf atCarol switched) Set.empty
      _ -> Spec.assertFailure s "fixture should give alice three attackers and carol three Giants"
  -- Two-Headed Giant's combat (CR 810.7, 805.10d): alice's Graxiplon attacks
  -- dave, and his teammate carol -- first in APNAP order, holding one Giant --
  -- blocks it. The gate is dave's, so with his three Giants the block is legal;
  -- the pair differs only in dave's third Giant.
  Spec.it s "CR 805.10e a teammate's block reads Graxiplon's gate at dave, whom it attacks" $ do
    graxiplon <- S.printingOf s registry "Graxiplon"
    giant <- S.printingOf s registry "Hill Giant"
    let twoHeaded = S.inTeams [[S.alice, S.bob], [S.carol, S.dave]] S.fourPlayerGame
        shared = twoHeaded {GameState.settings = (GameState.settings twoHeaded) {GameSettings.sharedTeamTurns = True}}
        board daveGiants =
          let (grax, g1) = S.addPermanent graxiplon S.alice shared
              (blocker, g2) = S.addPermanent giant S.carol g1
              g3 = withPermanents S.dave (replicate daveGiants giant) g2
           in ( grax,
                blocker,
                g3
                  { GameState.activePlayer = S.alice,
                    GameState.phase = Phase.Combat CombatStep.DeclareBlockers,
                    GameState.combat =
                      (GameState.combat g3)
                        { Combat.Type.defenders = [S.carol, S.dave],
                          Combat.Type.attackers = Map.singleton grax (AttackTarget.OfPlayer S.dave)
                        }
                  }
              )
        legal daveGiants =
          let (grax, blocker, gs) = board daveGiants
           in Combat.legalBlockDeclaration S.carol (Map.singleton blocker (Set.singleton grax)) gs
    Spec.assertBool s (legal 3) "CR 805.10e: dave controls three Giants, so carol's Giant may block"
    Spec.assertBool s (not (legal 2)) "and with two it may not"

-- CR 508.1d under two CROSSING gated bounds (CR 508.5a / 802.3a). Synthetic Tidal
-- Palisade counts the attackers at a seat without an Island, Synthetic Ridge
-- Palisade ("No more than one creature can attack each combat unless defending
-- player controls a Mountain.") those at a seat without a Mountain. bob holds an
-- Island, carol a Mountain, dave neither, so Tidal counts {carol, dave} and Ridge
-- {bob, dave}: overlapping, neither inside the other.
--
-- Alluring Siren's resolved "attacks you if able" is stamped onto the store, one
-- per Siren: carol's names alice's first Piker, dave's her second. Obeying both
-- puts two announcements in Tidal's count, so the maximum is ONE. A search that
-- nests one bound inside the other overstates it as two, which no declaration
-- attains, and its witness then lets declining through.
crossingAttackBoundsSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
crossingAttackBoundsSpec s registry = Spec.describe s "CrossingAttackBounds" $ do
  Spec.it s "CR 508.1d one requirement is the maximum when the two bounds cross at dave" $ do
    tidal <- S.printingOf s registry "Synthetic Tidal Palisade"
    ridge <- S.printingOf s registry "Synthetic Ridge Palisade"
    piker <- S.printingOf s registry "Goblin Piker"
    siren <- S.printingOf s registry "Alluring Siren"
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    let g0 = withPermanents S.alice [tidal, ridge] S.fourPlayerGame
        (toCarol, g1) = S.addPermanent piker S.alice g0
        (toDave, g2) = S.addPermanent piker S.alice g1
        (carolSiren, g3) = S.addPermanent siren S.carol (withPermanents S.carol [mountain] (withPermanents S.bob [island] g2))
        (daveSiren, g4) = S.addPermanent siren S.dave g3
        lure sirenId pid lured g =
          let (ts, g') = Game.freshTimestamp g
              stored = ActiveAttackRequirement.MkActiveAttackRequirement sirenId pid ts Expiry.AtCleanup (RestrictedCreatures.Named lured) (AttackTarget.OfPlayer pid)
           in g' {GameState.attackRequirements = stored : GameState.attackRequirements g'}
        g5 = lure daveSiren S.dave toDave (lure carolSiren S.carol toCarol g4)
        board =
          g5
            { GameState.activePlayer = S.alice,
              GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
              GameState.combat = (GameState.combat g5) {Combat.Type.defenders = [S.bob, S.carol, S.dave]}
            }
    Spec.assertBool s (Combat.legalAttackDeclarationAs S.alice [(toDave, AttackTarget.OfPlayer S.dave)] board) "CR 508.1d: obeying dave's Siren alone obeys the maximum"
    Spec.assertBool s (not (Combat.legalAttackDeclarationAs S.alice [(toDave, AttackTarget.OfPlayer S.dave), (toCarol, AttackTarget.OfPlayer S.carol)] board)) "CR 508.1c: obeying both puts two under Tidal's bound"
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] board)) "and declining obeys fewer than the maximum"

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
textChangedCombatRestrictionSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
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
    (with, crasher2) <- crasherBoard True False Subtype.Mountain Subtype.Island
    Spec.assertBool s (not (Combat.canAttack S.alice crasher without)) "with no Mountain the Crasher cannot attack"
    Spec.assertEqWith s "and is not offered" (Combat.legalAttackers S.alice without) []
    Spec.assertBool s (Combat.canAttack S.alice crasher2 with) "with bob's Mountain it can"

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
-- as, and a hand-built one of another flavour is dropped by CR 608.2b's re-read.
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
    case (Activatable.abilitiesFor gyId gs, theirs) of
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
    case (Activatable.abilitiesFor gyId gs, theirs) of
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
    case (Activatable.abilitiesFor gyId gs, theirs) of
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
      abilities = Activatable.abilitiesFor zirdaId board
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
      abilities = Activatable.abilitiesFor zirdaId board
      run named = declaringAttackers (activatingZirda zirdaId named board)
  Spec.assertEqWith s "Zirda states exactly one activated ability" (length abilities) 1
  pure (run wall, run attacker, wall, attacker)

-- Zirda's one ability, activated and resolved with its target slot aimed at
-- `named` -- activatingNetter's twin, and the only thing the boards either Zirda
-- fixture builds differ in.
activatingZirda :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
activatingZirda zirdaId named board = case Activatable.abilitiesFor zirdaId board of
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
      abilities = Activatable.abilitiesFor netterId board
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
      abilities = Activatable.abilitiesFor netterId board
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
-- The card's own "exile Chronomantic Escape with three time counters on it" is CR
-- 201.5's self reference and resolves as printed; Pawl.ResolveSpec's "CR 201.5 a
-- resolving spell naming itself" is what proves it. Its suspend ability is
-- printed and works; neither clause reaches the restriction this group is about,
-- so the boards below read the row and nothing else.
--
-- THREE SEATS with both opponents defending (CR 802.2), the only board on which
-- "can't attack you" and a blanket "can't attack" come apart for a creature with
-- no planeswalker to aim at, and the pair: the same board with the Escape left
-- in alice's hand.
storedClassAttackRestrictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
storedClassAttackRestrictionSpec s registry = Spec.describe s "StoredClassAttackRestriction" $ do
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

-- CR 611.2a: a duration that names a WINDOW rather than a deadline -- Wall of
-- Dust's "Whenever this creature blocks a creature, that creature can't attack
-- during its controller's next turn" (Oracle checked against Scryfall
-- 2026-09-21), the pool's only printing of such a duration. The row lands in
-- GameState.attackProhibitions under Expiry.DuringTurnOfControllerOf, and
-- Pawl.Engine.CombatRestriction.liveAttackProhibitions is the gate that keeps it
-- inert until a later turn whose active player controls the creature.
--
-- "Its controller" is read when that turn comes, not when the Wall blocked
-- (Gideon, Battle-Forged's 2015-06-22 ruling: a creature that changes
-- controllers before its chance to attack is bound during its new controller's
-- next turn). So Act of Treason on bob's turn 2 makes that turn the window:
-- the stolen, hasty Giant has its chance to attack there. Act of Treason is
-- the whole reason bob has three Mountains.
--
-- THE PAIR: the same attacker, the same declaration and the same block on every
-- board here, differing only in WHICH wall blocked -- Wall of Dust, or Wall of
-- Stone, a vanilla 0/8 defender whose block stores nothing. A green leg
-- therefore cannot come from summoning sickness, from a missing haste or from a
-- creature that was never offered.
windowAttackRestrictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
windowAttackRestrictionSpec s registry = Spec.describe s "WindowAttackRestriction" $ do
  Spec.it s "CR 611.2a the window follows the Giant to bob, so it cannot attack on his turn" $ do
    (giant, treason, blocked) <- wallBoard s registry "Wall of Dust"
    (openGiant, openTreason, control) <- wallBoard s registry "Wall of Stone"
    let bobsTurn = stealing treason giant (handoffUntapping blocked)
        openTurn = stealing openTreason openGiant (handoffUntapping control)
    Spec.assertEqWith s "the block stored one row, over the creature the Wall blocked" (fmap ActiveAttackProhibition.affected (GameState.attackProhibitions blocked)) [RestrictedCreatures.Named giant]
    Spec.assertEqWith s "and the pair stored none" (GameState.attackProhibitions control) []
    Spec.assertEqWith s "the row is still stored on bob's turn" (length (GameState.attackProhibitions bobsTurn)) 1
    Spec.assertEqWith s "bob controls the Giant and is active" (Projection.controllerOf giant bobsTurn, GameState.activePlayer bobsTurn) (Just S.bob, S.bob)
    Spec.assertBool s (not (Combat.legalAttackDeclarationAs S.bob [(giant, AttackTarget.OfPlayer S.alice)] (declaringAt S.alice bobsTurn))) "CR 611.2a: bob now controls the Giant on his turn, so the Giant may not attack"
    Spec.assertBool s (Combat.legalAttackDeclarationAs S.bob [(openGiant, AttackTarget.OfPlayer S.alice)] (declaringAt S.alice openTurn)) "as the pair's Giant does"
    -- Ordered LAST so a card that printed the end-only duration reddens the
    -- declaration above rather than being absorbed here: the seat is left open
    -- on the blocked creature (CR 108.4 / 110.2, read when a later turn comes)
    -- and 1 is the turn the block happened on.
    Spec.assertEqWith s "under a window naming the Giant's controller's turn after turn 1" (fmap ActiveAttackProhibition.expiry (GameState.attackProhibitions blocked)) [Expiry.DuringTurnOfControllerOf (AfterObjectTurn.MkAfterObjectTurn giant 1)]
  Spec.it s "CR 611.2a and cannot attack once that turn has begun" $ do
    (giant, _, blocked) <- wallBoard s registry "Wall of Dust"
    (openGiant, _, control) <- wallBoard s registry "Wall of Stone"
    let alicesTurn = handoffUntapping (handoffUntapping blocked)
        openAlices = handoffUntapping (handoffUntapping control)
    Spec.assertEqWith s "alice is active on turn 3" (GameState.activePlayer alicesTurn, GameState.turnNumber alicesTurn) (S.alice, 3)
    Spec.assertBool s (not (Combat.legalAttackDeclarationAs S.alice [(giant, AttackTarget.OfPlayer S.bob)] (declaringAt S.bob alicesTurn))) "the window is open, so the Giant may not attack"
    Spec.assertBool s (not (Combat.canAttack S.alice giant (declaringAt S.bob alicesTurn))) "and it is off CR 508.1a's candidate list"
    Spec.assertBool s (Combat.legalAttackDeclarationAs S.alice [(openGiant, AttackTarget.OfPlayer S.bob)] (declaringAt S.bob openAlices)) "the pair: with nothing stored the same Giant attacks"

-- Turn 1 played through its whole combat phase: alice attacks with a Hill Giant
-- and bob blocks with `blocker`, whose only job on the Wall of Stone leg is to
-- be a legal block. The Giant is a 3/3 so it survives Wall of Dust's 1, and the
-- Wall is a 1/4 so it survives the Giant's 3 -- both boards end combat with
-- every permanent still on the battlefield.
--
-- bob's three Mountains and his Act of Treason are for `stealing` below; they
-- are on both legs so the two boards differ in one card.
wallBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> m (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
wallBoard s registry blocker = do
  let bobsSide =
        (S.battlefield S.bob (S.settled "blocker" blocker : replicate 3 (S.ready (S.permanent "Mountain"))))
          { Seat.hand = Seq.singleton (S.aliased "treason" (S.cardSetup "Act of Treason"))
          }
      setup =
        S.board
          (S.battlefield S.alice [S.settled "giant" "Hill Giant"] NonEmpty.:| [bobsSide])
          S.alice
          S.beginningOfCombat
      script =
        S.turn
          1
          [ S.on S.declareAttackers S.alice (S.attack [S.aliasRef "giant"]),
            S.on S.declareBlockers S.bob (S.block [(S.aliasRef "blocker", S.aliasRef "giant")])
          ]
  built <- S.buildBoardOrFail s registry setup
  (_, after) <- S.runScriptOrFail s script built S.combatGame
  case (aliasOf "giant" built, aliasOf "treason" built) of
    (Just giant, Just treason) -> pure (giant, treason, after)
    _ -> Spec.assertFailure s "the board omitted an alias"

aliasOf :: String -> Staged.Staged -> Maybe ObjectId.ObjectId
aliasOf name built = Map.lookup (Label.MkLabel (Text.pack name)) (Staged.objects built)

-- bob's Act of Treason, cast from his hand in his own precombat main phase and
-- resolved with its one target slot aimed at the Giant: he gains control of it
-- until end of turn, untaps it and gives it haste, which is what lets it attack
-- on a turn that is not its owner's.
stealing :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
stealing treason giant gs =
  let board = gs {GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.bob}
   in S.runPure (namingTarget giant) board (S.cast S.bob treason >> Stack.resolveTop)

-- CR 514.2, then the seat walk, then CR 502.3: this turn's cleanup sweep, the
-- next seat's turn, and that turn's untap step. The untap is what `handoff` above
-- leaves owed and every case here needs: a creature that attacked on turn 1 is
-- tapped (CR 508.1f), and a tapped creature is off CR 508.1a's candidate list
-- for a reason that has nothing to do with the row under test.
handoffUntapping :: GameState.GameState -> GameState.GameState
handoffUntapping gs =
  S.runPure
    S.identityAnswer
    (Expiry.dropAtCleanup gs)
    (Engine.handoffTurn >> Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))

-- The active player's declare attackers step with `defending` as the one
-- defending player, stated rather than derived for `declaringAttackers`' reason.
declaringAt :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
declaringAt defending gs =
  gs
    { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
      GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [defending]}
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
activatingNetter netterId named board = case Activatable.abilitiesFor netterId board of
  [ability] -> S.runPure (namingTarget named) board (Activate.activateAbility S.alice netterId ability >> Stack.resolveTop)
  _ -> board

-- CR 508.1a's "they can't also be battles", the one clause of that rule no
-- printing could reach: nothing makes a permanent a creature and a battle at
-- once. Scryfall o:/battle in addition/ and o:/becomes a battle/ were both empty
-- and t:battle t:creature returned only Sieges matching on their creature BACK
-- face (2026-09-08), so data/cards/synthetic-besiege-the-front.json is the
-- producer: "Until end of turn, target creature is a battle in addition to its
-- other types. Put five defense counters on it."
--
-- Two Goblin Pikers, one of them made a battle, on one board: the pair differs in
-- the battle type and in nothing else, so "the Piker did not attack" cannot be
-- summoning sickness or any other conjunct of canAttackGiven. The five defense
-- counters keep CR 704.5w from burying the grantee before the declaration is
-- asked.
creatureBattleDeclarationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
creatureBattleDeclarationSpec s registry = Spec.describe s "CreatureBattleDeclaration" $ do
  Spec.it s "CR 508.1a a creature that is also a battle is not offered as an attacker" $ do
    let mine =
          (S.battlefield S.alice [S.settled "grantee" "Goblin Piker", S.settled "other" "Goblin Piker", S.settled "first" "Island", S.settled "second" "Island", S.settled "third" "Island"])
            { Seat.hand = Seq.singleton (S.aliased "spell" (S.cardSetup "Synthetic Besiege the Front"))
            }
        setup = S.board (mine NonEmpty.:| [S.playerSetup S.bob]) S.alice S.beginningOfCombat
        choices =
          Choices.none
            { Choices.targets = Just [S.aliasRef "grantee"],
              Choices.manaSources = Seq.fromList [Just (S.aliasRef "first"), Just (S.aliasRef "second"), Just (S.aliasRef "third")]
            }
        script = S.turn 1 [S.on S.beginningOfCombat S.alice (S.castAction (S.aliasRef "spell") choices)]
    granted <- S.play s registry setup script S.priorityGame
    let pikers = Scenario.namedObjects (CardName.MkCardName (Text.pack "Goblin Piker")) granted
        (battles, plain) = List.partition (\oid -> Projection.isBattleOf oid granted) pikers
        fought = S.runCombat S.aggressiveAnswer granted
    -- Gameplay level, and first. The life delta is NOT the discriminator here and
    -- cannot be: an offer that still held the creature-battle would have it
    -- declared, and CR 506.4's becomes-a-battle clause below would then pull it
    -- straight back out, leaving bob at 18 either way. What survives the
    -- declaration is CR 508.1f's tap, which is not undone by the removal.
    Spec.assertEqWith s "CR 508.1f the creature-battle was never tapped to attack, and CR 510.1b bob takes only the plain Piker's two" (fmap (\oid -> fmap Object.tapped (Game.lookupObject oid fought)) battles, S.lifeOf S.bob fought) ([Just TapState.Untapped], Just 18)
    Spec.assertEqWith s "CR 508.1a the offer holds the Piker that is not a battle, and only it" (Set.fromList (Combat.legalAttackers S.alice granted)) (Set.fromList plain)
    Spec.assertEqWith s "and the spell really left the other one a creature that is a battle" (length pikers, fmap (\oid -> Projection.isCreatureOf oid granted) battles) (2, [True])
  Spec.it s "CR 509.1a a creature that is also a battle is not offered as a blocker either" $ do
    -- The blocking twin, on bob's side of the same card. Here the OFFER is the
    -- whole observable and the assertion comes first: a creature-battle that was
    -- offered and declared as a blocker is pulled straight back out by CR 506.4's
    -- becomes-a-battle clause, so it deals and is dealt nothing either way and no
    -- damage reading can tell the two boards apart. alice's 3/4 Tapestry Warden
    -- taking one 2/1 Piker's two and living is the control on the other axis.
    let theirs =
          (S.battlefield S.bob [S.settled "grantee" "Goblin Piker", S.settled "other" "Goblin Piker", S.settled "first" "Island", S.settled "second" "Island", S.settled "third" "Island"])
            { Seat.hand = Seq.singleton (S.aliased "spell" (S.cardSetup "Synthetic Besiege the Front"))
            }
        setup = S.board (S.battlefield S.alice [S.settled "warden" "Tapestry Warden"] NonEmpty.:| [theirs]) S.alice S.beginningOfCombat
        choices =
          Choices.none
            { Choices.targets = Just [S.aliasRef "grantee"],
              Choices.manaSources = Seq.fromList [Just (S.aliasRef "first"), Just (S.aliasRef "second"), Just (S.aliasRef "third")]
            }
        script = S.turn 1 [S.on S.beginningOfCombat S.bob (S.castAction (S.aliasRef "spell") choices)]
    granted <- S.play s registry setup script S.priorityGame
    let pikers = Scenario.namedObjects (CardName.MkCardName (Text.pack "Goblin Piker")) granted
        (battles, plain) = List.partition (\oid -> Projection.isBattleOf oid granted) pikers
        fought = S.settleSba (S.runCombat S.aggressiveAnswer granted)
        wardens = Scenario.namedObjects (CardName.MkCardName (Text.pack "Tapestry Warden")) granted
    Spec.assertEqWith s "CR 509.1a the offer holds the Piker that is not a battle, and only it" (Set.fromList (Combat.legalBlockers S.bob granted)) (Set.fromList plain)
    Spec.assertEqWith s "CR 510.1c and one Piker really blocked, so the Warden took two and lived" (fmap (\oid -> (S.onBattlefield oid fought, S.damageOf oid fought)) wardens) [(True, Just 2)]
    Spec.assertEqWith s "and the spell really left the other one a creature that is a battle" (length pikers, fmap (\oid -> Projection.isCreatureOf oid granted) battles) (2, [True])
  -- CR 506.3f, the put-onto-the-battlefield roads, which no printing reaches
  -- for the reason above: data/cards/synthetic-siege-muster.json is the
  -- producer. Its first two modes
  -- make the same tapped and attacking Soldier and differ only in its battle
  -- type. alice declares nothing, so CR 508.8's second clause alone keeps the
  -- declare blockers step: the plain Soldier keeps it and the creature-battle
  -- must not. The skip is the only observable, since CR 506.4 would pull an
  -- attacking creature-battle back out before it dealt any damage.
  Spec.it s "CR 506.3f a creature-battle put onto the battlefield attacking never attacks, so CR 508.8 skips blocks and damage" $ do
    let muster mode = do
          let mine =
                (S.battlefield S.alice [S.settled "first" "Plains", S.settled "second" "Plains"])
                  { Seat.hand = Seq.singleton (S.aliased "spell" (S.cardSetup "Synthetic Siege Muster"))
                  }
              setup = S.board (mine NonEmpty.:| [S.playerSetup S.bob]) S.alice S.declareAttackers
              choices =
                Choices.none
                  { Choices.modes = Just (Seq.singleton (ModeIndex.MkModeIndex mode)),
                    Choices.manaSources = Seq.fromList [Just (S.aliasRef "first"), Just (S.aliasRef "second")]
                  }
              script = S.turn 1 [S.on S.declareAttackers S.alice (S.castAction (S.aliasRef "spell") choices)]
          S.play s registry setup script Engine.runStep
    plain <- muster 0
    battle <- muster 1
    let soldiers = Scenario.namedObjects (CardName.MkCardName (Text.pack "Soldier Token"))
    Spec.assertEqWith s "CR 508.8 the creature-battle attacked nothing, so the step after declare attackers is end of combat" (GameState.phase battle) S.endOfCombat
    Spec.assertEqWith s "CR 508.8 while the plain Soldier keeps the declare blockers step" (GameState.phase plain) S.declareBlockers
    Spec.assertEqWith s "and each mode really made one tapped Soldier, the second a creature that is a battle" (fmap (\gs -> fmap (\oid -> (fmap Object.tapped (Game.lookupObject oid gs), Projection.isCreatureOf oid gs, Projection.isBattleOf oid gs)) (soldiers gs)) [plain, battle]) [[(Just TapState.Tapped, True, False)], [(Just TapState.Tapped, True, True)]]

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Combat" $ do
  declareSpec s registry
  creatureBattleDeclarationSpec s registry
  defenderSpec s registry
  defendingPlayerSpec s registry
  attackMultiplePlayersSpec s registry
  hasteSpec s registry
  evasionSpec s registry
  textChangedLandwalkSpec s registry
  menaceSpec s registry
  blockPermissionSpec s registry
  blockRequirementSpec s registry
  attackRequirementSpec s registry
  combatRestrictionSpec s registry
  cantBlockCreaturesSpec s registry
  keywordCounterRestrictionSpec s registry
  storedBlockRestrictionSpec s registry
  storedAttackRestrictionSpec s registry
  storedClassAttackRestrictionSpec s registry
  windowAttackRestrictionSpec s registry
  conditionalCombatRestrictionSpec s registry
  defendingPlayerRestrictionSpec s registry
  aimedAttackRestrictionSpec s registry
  perDefenderRestrictionSpec s registry
  perDefenderWholeRestrictionSpec s registry
  defendingPlayerOfBlockGateSpec s registry
  crossingAttackBoundsSpec s registry
  textChangedCombatRestrictionSpec s registry
  textChangedCombatAffectedSpec s registry
  controlChangeSicknessSpec s registry
